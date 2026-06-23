#' Outcome-blind exposure overlap diagnostics
#'
#' `margot_exposure_overlap()` estimates or accepts propensity scores using only
#' the exposure and baseline covariates. It is intended for registration-stage
#' support checks before any outcome model, causal forest, LMTP estimator, or
#' causal contrast is fitted.
#'
#' @param data A data frame containing the exposure and, when `covariates` is a
#'   character vector, the baseline covariates.
#' @param exposure Character string naming a binary exposure coded 0/1.
#' @param covariates Character vector of baseline covariate names, or a numeric
#'   matrix/data frame with one row per observation in `data`.
#' @param weights Optional character string naming design weights in `data`, or
#'   a numeric vector with one value per observation.
#' @param method Character. One of `"supplied"`, `"logistic"`, or
#'   `"probability_forest"`.
#' @param bounds Numeric length-2 vector giving the lower and upper propensity
#'   score bounds used for overlap and trimming summaries.
#' @param plot Logical; if `TRUE`, return a propensity-score histogram.
#' @param propensity Optional numeric vector of propensity scores. Required for
#'   `method = "supplied"`.
#' @param grf_defaults Optional list passed to `grf::probability_forest()` when
#'   `method = "probability_forest"`.
#' @param seed Integer seed used by the probability forest path.
#' @param verbose Logical; emit progress messages.
#' @param ... Reserved. Outcome-like arguments supplied here trigger an error.
#'
#' @return A list with support summaries, balance summaries, optional plot, and
#'   enough metadata to document the outcome-blind exposure diagnostic.
#' @export
margot_exposure_overlap <- function(data,
                                    exposure,
                                    covariates,
                                    weights = NULL,
                                    method = c("supplied", "logistic", "probability_forest"),
                                    bounds = c(0.05, 0.95),
                                    plot = TRUE,
                                    propensity = NULL,
                                    grf_defaults = list(),
                                    seed = 12345,
                                    verbose = TRUE,
                                    ...) {
  reserved <- list(...)
  reserved_names <- names(reserved)
  if (!is.null(reserved_names)) {
    outcome_like <- grep("(^|_)(outcome|outcomes|outcome_var|outcome_vars|Y|y)($|_)", reserved_names, value = TRUE)
    if (length(outcome_like) > 0L) {
      stop("Outcome variables are not allowed in `margot_exposure_overlap()`.", call. = FALSE)
    }
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(exposure) || length(exposure) != 1L || !exposure %in% names(data)) {
    stop("`exposure` must name one column in `data`.", call. = FALSE)
  }
  method <- match.arg(method)
  bounds <- as.numeric(bounds)
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[1] <= 0 || bounds[2] >= 1 || bounds[1] >= bounds[2]) {
    stop("`bounds` must be a numeric length-2 vector inside (0, 1).", call. = FALSE)
  }

  x <- .margot_exposure_covariates(data, covariates, drop_reference = FALSE)
  x_model <- if (identical(method, "logistic")) {
    .margot_exposure_covariates(data, covariates, drop_reference = TRUE)
  } else {
    x
  }
  w <- .margot_exposure_binary(data[[exposure]], exposure)
  sw <- .margot_exposure_weights(data, weights)
  ps_input <- .margot_exposure_propensity(propensity, nrow(data), method)

  keep <- stats::complete.cases(x) & !is.na(w)
  if (!is.null(sw)) keep <- keep & !is.na(sw)
  if (!is.null(ps_input)) keep <- keep & !is.na(ps_input)
  if (!any(keep)) {
    stop("No complete rows are available for exposure-overlap diagnostics.", call. = FALSE)
  }

  x_complete <- x[keep, , drop = FALSE]
  x_model_complete <- x_model[keep, , drop = FALSE]
  w_complete <- w[keep]
  sw_complete <- if (!is.null(sw)) sw[keep] else NULL
  ps_complete <- if (!is.null(ps_input)) ps_input[keep] else NULL

  if (!all(w_complete %in% c(0L, 1L))) {
    stop("`exposure` must be binary after complete-case filtering.", call. = FALSE)
  }
  if (length(unique(w_complete)) < 2L) {
    stop("`exposure` must contain both 0 and 1 among complete rows.", call. = FALSE)
  }

  if (identical(method, "logistic")) {
    if (verbose) cli::cli_alert_info("estimating outcome-blind logistic exposure model")
    ps_complete <- .margot_exposure_logistic(x_model_complete, w_complete, sw_complete)
  } else if (identical(method, "probability_forest")) {
    if (verbose) cli::cli_alert_info("estimating outcome-blind probability forest for exposure support")
    ps_complete <- .margot_exposure_probability_forest(
      x = x_complete,
      w = w_complete,
      weights = sw_complete,
      grf_defaults = grf_defaults,
      seed = seed
    )
  }
  ps_complete <- pmin(pmax(as.numeric(ps_complete), 1e-6), 1 - 1e-6)

  overlap_summary <- calculate_overlap_statistics(
    W = w_complete,
    W_hat = ps_complete,
    exposure_name = exposure,
    bounds = bounds
  )
  trim <- calculate_trimming_summary(ps_complete, bounds = bounds)
  trimming_summary <- data.frame(
    n_total = trim$n_total,
    n_trimmed_lower = trim$n_trimmed_lower,
    n_trimmed_upper = trim$n_trimmed_upper,
    n_trimmed_total = trim$n_trimmed_total,
    pct_trimmed = trim$pct_trimmed,
    bounds_lower = bounds[1],
    bounds_upper = bounds[2],
    stringsAsFactors = FALSE
  )
  prevalence_summary <- .margot_exposure_prevalence(w_complete, sw_complete)
  ess_summary <- .margot_exposure_ess(ps_complete, sw_complete, bounds)
  balance_summary <- .margot_exposure_balance(x_complete, w_complete, sw_complete)

  propensity_plot <- NULL
  if (isTRUE(plot)) {
    propensity_plot <- create_propensity_plot(
      W = w_complete,
      W_hat = ps_complete,
      exposure_name = exposure,
      theme = "classic",
      bounds = bounds
    )
  }

  out <- list(
    overlap_summary = overlap_summary,
    trimming_summary = trimming_summary,
    prevalence_summary = prevalence_summary,
    effective_sample_size = ess_summary,
    balance_summary = balance_summary,
    propensity_plot = propensity_plot,
    propensity = ps_complete,
    exposure = w_complete,
    complete_rows = which(keep),
    dropped_rows = which(!keep),
    metadata = list(
      exposure = exposure,
      method = method,
      bounds = bounds,
      n_input = nrow(data),
      n_complete = sum(keep),
      n_dropped = sum(!keep),
      has_weights = !is.null(sw_complete),
      outcome_blind = TRUE
    )
  )
  class(out) <- c("margot_exposure_overlap", "list")
  out
}

# prepare baseline covariates for outcome-blind support diagnostics.
.margot_exposure_covariates <- function(data, covariates, drop_reference = FALSE) {
  if (is.character(covariates)) {
    if (!length(covariates)) {
      stop("`covariates` must contain at least one baseline covariate.", call. = FALSE)
    }
    missing_covariates <- setdiff(covariates, names(data))
    if (length(missing_covariates) > 0L) {
      stop("Missing covariates: ", paste(missing_covariates, collapse = ", "), call. = FALSE)
    }
    x <- data[, covariates, drop = FALSE]
  } else if (is.matrix(covariates) || is.data.frame(covariates)) {
    x <- as.data.frame(covariates)
    if (nrow(x) != nrow(data)) {
      stop("`covariates` must have one row per observation in `data`.", call. = FALSE)
    }
  } else {
    stop("`covariates` must be a character vector, matrix, or data frame.", call. = FALSE)
  }
  .margot_encode_exposure_covariates(x, drop_reference = drop_reference)
}

# coerce the exposure to a strict binary 0/1 vector.
.margot_exposure_binary <- function(x, exposure) {
  if (is.logical(x)) {
    out <- as.integer(x)
  } else if (is.numeric(x) || is.integer(x)) {
    observed_raw <- stats::na.omit(unique(x))
    if (!all(observed_raw %in% c(0, 1))) {
      stop("`exposure` must be coded 0/1.", call. = FALSE)
    }
    out <- as.integer(x)
  } else if (is.factor(x) || is.character(x)) {
    x_chr <- as.character(x)
    if (!all(stats::na.omit(unique(x_chr)) %in% c("0", "1"))) {
      stop("`exposure` must be coded 0/1; `", exposure, "` is not coercible to binary.", call. = FALSE)
    }
    out <- as.integer(x_chr)
  } else {
    stop("`exposure` must be binary numeric, logical, factor, or character.", call. = FALSE)
  }
  out[is.na(x)] <- NA_integer_
  observed <- stats::na.omit(unique(out))
  if (!all(observed %in% c(0L, 1L))) {
    stop("`exposure` must be coded 0/1.", call. = FALSE)
  }
  out
}

# encode baseline covariates while preserving missingness for complete-case checks.
.margot_encode_exposure_covariates <- function(x, drop_reference = FALSE) {
  encoded <- list()
  for (var in names(x)) {
    value <- x[[var]]
    if (is.numeric(value) || is.integer(value)) {
      encoded[[var]] <- as.numeric(value)
    } else if (is.logical(value)) {
      encoded[[var]] <- as.integer(value)
      encoded[[var]][is.na(value)] <- NA_integer_
    } else if (is.factor(value) || is.character(value)) {
      value_chr <- as.character(value)
      levels <- sort(unique(value_chr[!is.na(value_chr)]))
      if (length(levels) == 0L) {
        encoded[[var]] <- rep(NA_real_, length(value_chr))
      } else {
        if (isTRUE(drop_reference) && length(levels) > 1L) {
          levels <- levels[-1L]
        }
        for (level in levels) {
          dummy_name <- paste(var, make.names(level), sep = "_")
          dummy <- as.integer(value_chr == level)
          dummy[is.na(value_chr)] <- NA_integer_
          encoded[[dummy_name]] <- dummy
        }
      }
    } else {
      stop("Unsupported covariate type for `", var, "`.", call. = FALSE)
    }
  }
  out <- as.data.frame(encoded, check.names = FALSE)
  names(out) <- make.unique(names(out))
  out
}

# prepare optional design weights for support summaries.
.margot_exposure_weights <- function(data, weights) {
  if (is.null(weights)) {
    return(NULL)
  }
  if (is.character(weights) && length(weights) == 1L) {
    if (!weights %in% names(data)) {
      stop("`weights` must name one column in `data`.", call. = FALSE)
    }
    out <- data[[weights]]
  } else if (is.numeric(weights) && length(weights) == nrow(data)) {
    out <- weights
  } else {
    stop("`weights` must be NULL, a column name, or a numeric vector with one value per row.", call. = FALSE)
  }
  out <- as.numeric(out)
  if (any(out < 0, na.rm = TRUE)) {
    stop("`weights` must be non-negative.", call. = FALSE)
  }
  out
}

# validate supplied propensities when the caller does not want a model fit.
.margot_exposure_propensity <- function(propensity, n, method) {
  if (identical(method, "supplied")) {
    if (is.null(propensity)) {
      stop("`propensity` is required when `method = \"supplied\"`.", call. = FALSE)
    }
  } else if (!is.null(propensity)) {
    stop("`propensity` can only be supplied when `method = \"supplied\"`.", call. = FALSE)
  }
  if (is.null(propensity)) {
    return(NULL)
  }
  if (!is.numeric(propensity) || length(propensity) != n) {
    stop("`propensity` must be a numeric vector with one value per row in `data`.", call. = FALSE)
  }
  if (any(propensity < 0 | propensity > 1, na.rm = TRUE)) {
    stop("`propensity` values must be in [0, 1].", call. = FALSE)
  }
  propensity
}

# estimate propensity scores using a baseline-covariate logistic model.
.margot_exposure_logistic <- function(x, w, weights) {
  model_data <- data.frame(.exposure = w, x, check.names = FALSE)
  fit <- stats::glm(
    .exposure ~ .,
    data = model_data,
    family = stats::binomial(),
    weights = weights
  )
  stats::predict(fit, type = "response")
}

# estimate propensity scores using grf probability forest without outcomes.
.margot_exposure_probability_forest <- function(x, w, weights, grf_defaults, seed) {
  if (!requireNamespace("grf", quietly = TRUE)) {
    stop("Package `grf` is required for `method = \"probability_forest\"`.", call. = FALSE)
  }
  forest_args <- utils::modifyList(
    list(
      X = as.matrix(x),
      Y = factor(w, levels = c(0L, 1L)),
      sample.weights = weights,
      seed = seed
    ),
    grf_defaults
  )
  forest <- do.call(grf::probability_forest, forest_args)
  pred <- stats::predict(forest)$predictions
  if (is.matrix(pred)) {
    if ("1" %in% colnames(pred)) {
      return(pred[, "1"])
    }
    return(pred[, ncol(pred)])
  }
  as.numeric(pred)
}

# summarise observed exposure prevalence with and without design weights.
.margot_exposure_prevalence <- function(w, weights) {
  weighted <- if (is.null(weights)) {
    NA_real_
  } else {
    stats::weighted.mean(w == 1L, weights, na.rm = TRUE)
  }
  data.frame(
    n = length(w),
    n_treated = sum(w == 1L),
    n_control = sum(w == 0L),
    prevalence_unweighted = mean(w == 1L),
    prevalence_weighted = weighted,
    stringsAsFactors = FALSE
  )
}

# compute total and retained effective sample size under support bounds.
.margot_exposure_ess <- function(propensity, weights, bounds) {
  keep <- propensity >= bounds[1] & propensity <= bounds[2]
  w_all <- if (is.null(weights)) rep(1, length(propensity)) else weights
  w_keep <- w_all[keep]
  data.frame(
    bounds_lower = bounds[1],
    bounds_upper = bounds[2],
    n_in_bounds = sum(keep),
    n_out_of_bounds = sum(!keep),
    effective_n_all = sum(w_all)^2 / sum(w_all^2),
    effective_n_in_bounds = if (length(w_keep) == 0L || sum(w_keep^2) == 0) NA_real_ else sum(w_keep)^2 / sum(w_keep^2),
    stringsAsFactors = FALSE
  )
}

# calculate weighted and unweighted baseline differences by exposure group.
.margot_exposure_balance <- function(x, w, weights) {
  rows <- lapply(names(x), function(var) {
    value <- x[[var]]
    treated <- w == 1L
    control <- w == 0L
    mean_treated <- mean(value[treated], na.rm = TRUE)
    mean_control <- mean(value[control], na.rm = TRUE)
    pooled_sd <- sqrt((stats::var(value[treated], na.rm = TRUE) + stats::var(value[control], na.rm = TRUE)) / 2)
    std_diff <- if (!is.finite(pooled_sd) || pooled_sd == 0) NA_real_ else (mean_treated - mean_control) / pooled_sd

    weighted_treated <- NA_real_
    weighted_control <- NA_real_
    weighted_std_diff <- NA_real_
    if (!is.null(weights)) {
      weighted_treated <- stats::weighted.mean(value[treated], weights[treated], na.rm = TRUE)
      weighted_control <- stats::weighted.mean(value[control], weights[control], na.rm = TRUE)
      weighted_std_diff <- if (!is.finite(pooled_sd) || pooled_sd == 0) NA_real_ else (weighted_treated - weighted_control) / pooled_sd
    }

    data.frame(
      variable = var,
      mean_treated_unweighted = mean_treated,
      mean_control_unweighted = mean_control,
      std_diff_unweighted = std_diff,
      mean_treated_weighted = weighted_treated,
      mean_control_weighted = weighted_control,
      std_diff_weighted = weighted_std_diff,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
