# purpose: coerce a report input to a two-dimensional matrix. inputs: an object,
# its argument name, and an optional row count; output: a matrix.
.margot_evidence_matrix <- function(x, name, n = NULL, numeric = FALSE) {
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1L)
  } else {
    x <- as.matrix(x)
  }
  if (!is.null(n) && nrow(x) != n) {
    stop("`", name, "` must have ", n, " rows.", call. = FALSE)
  }
  if (numeric) {
    if (!is.numeric(x)) {
      stop("`", name, "` must be numeric.", call. = FALSE)
    }
    storage.mode(x) <- "double"
  }
  x
}

# purpose: validate one non-negative report weight vector. inputs: weights,
# expected length, and argument name; output: a numeric vector.
.margot_evidence_weights <- function(x, n, name, default = 1) {
  if (is.null(x)) {
    return(rep(default, n))
  }
  x <- as.numeric(x)
  if (length(x) != n) {
    stop("`", name, "` must have length ", n, ".", call. = FALSE)
  }
  if (any(x < 0, na.rm = TRUE)) {
    stop("`", name, "` must be non-negative.", call. = FALSE)
  }
  x
}

# purpose: compute a weighted mean without silently using invalid weights.
# inputs: values and weights; output: one numeric value.
.margot_evidence_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(keep) || sum(w[keep]) <= 0) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# purpose: compute a population-form weighted standard deviation. inputs:
# values and weights; output: one numeric value.
.margot_evidence_weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(keep) || sum(w[keep]) <= 0) return(NA_real_)
  centre <- .margot_evidence_weighted_mean(x[keep], w[keep])
  sqrt(sum(w[keep] * (x[keep] - centre)^2) / sum(w[keep]))
}

# purpose: compute a weighted proportion for one logical indicator. inputs:
# indicator and weights; output: one numeric proportion.
.margot_evidence_weighted_prop <- function(x, w) {
  keep <- !is.na(x) & is.finite(w) & w >= 0
  if (!any(keep) || sum(w[keep]) <= 0) return(NA_real_)
  sum(as.numeric(x[keep]) * w[keep]) / sum(w[keep])
}

# purpose: describe one weight vector with zero weights retained. inputs: weights,
# a stage label, and top-share fractions; output: summary and concentration tables.
.margot_evidence_weight_summary <- function(weights,
                                            stage,
                                            top_shares = c(0.01, 0.05, 0.10)) {
  weights <- as.numeric(weights)
  top_shares <- sort(unique(as.numeric(top_shares)))
  if (length(top_shares) == 0L || any(!is.finite(top_shares)) ||
      any(top_shares <= 0 | top_shares > 1)) {
    stop("`top_shares` must contain proportions in (0, 1].", call. = FALSE)
  }
  finite <- is.finite(weights)
  non_negative <- finite & weights >= 0
  valid <- weights[non_negative]
  total <- sum(valid)
  kish <- if (length(valid) > 0L && total > 0 && sum(valid^2) > 0) {
    total^2 / sum(valid^2)
  } else {
    NA_real_
  }
  quantile_value <- function(probability) {
    if (!length(valid)) return(NA_real_)
    as.numeric(stats::quantile(valid, probability, names = FALSE, na.rm = TRUE))
  }
  summary <- data.frame(
    stage = stage,
    n = length(weights),
    n_missing = sum(is.na(weights)),
    n_non_finite = sum(!is.na(weights) & !finite),
    n_zero = sum(non_negative & weights == 0),
    n_positive = sum(non_negative & weights > 0),
    min = quantile_value(0),
    p01 = quantile_value(0.01),
    p05 = quantile_value(0.05),
    median = quantile_value(0.50),
    mean = if (length(valid)) mean(valid) else NA_real_,
    p95 = quantile_value(0.95),
    p99 = quantile_value(0.99),
    max = quantile_value(1),
    sum_weights = if (length(valid)) total else NA_real_,
    kish_effective_n = kish,
    kish_fraction_of_all_rows = kish / length(weights),
    stringsAsFactors = FALSE
  )
  ordered <- sort(valid, decreasing = TRUE)
  concentration <- do.call(rbind, lapply(top_shares, function(share) {
    top_n <- if (length(ordered)) max(1L, ceiling(share * length(weights))) else 0L
    top_sum <- if (top_n > 0L) sum(utils::head(ordered, top_n)) else 0
    data.frame(
      stage = stage,
      top_fraction = share,
      top_n = top_n,
      weight_share = if (total > 0) top_sum / total else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  list(summary = summary, concentration = concentration)
}

# purpose: summarise a numeric vector with fixed quantiles. inputs: values;
# output: one data-frame row.
.margot_evidence_numeric_summary <- function(x) {
  x <- as.numeric(x)
  finite <- x[is.finite(x)]
  q <- function(probability) {
    if (!length(finite)) return(NA_real_)
    as.numeric(stats::quantile(finite, probability, names = FALSE, na.rm = TRUE))
  }
  data.frame(
    n = length(x),
    n_missing = sum(is.na(x)),
    n_non_finite = sum(!is.na(x) & !is.finite(x)),
    n_zero = sum(finite == 0),
    min = q(0),
    p01 = q(0.01),
    p05 = q(0.05),
    median = q(0.50),
    mean = if (length(finite)) mean(finite) else NA_real_,
    p95 = q(0.95),
    p99 = q(0.99),
    max = q(1),
    stringsAsFactors = FALSE
  )
}

#' Report descriptive censoring evidence for an LMTP analysis
#'
#' `margot_lmtp_censoring_report()` reports observed retention and, when
#' supplied, fitted continued-observation probabilities and censoring factors.
#' The function returns aggregate evidence only. It neither classifies censoring
#' support nor returns a route action.
#'
#' @param observed A logical or `0`/`1` matrix with one participant per row and
#'   one censoring transition per column. A vector represents one transition.
#' @param baseline_weights Optional non-negative baseline design weights, one per
#'   participant. Equal weights are used when this argument is `NULL`.
#' @param fitted_probabilities Optional matrix of fitted probabilities of
#'   continued observation, with the same dimensions as `observed`.
#' @param censoring_factors Optional matrix of separately identified censoring
#'   density-ratio factors, with the same dimensions as `observed`.
#' @param joint_ratios Optional matrix of joint exposure-and-censoring density
#'   ratios, with the same dimensions as `observed`.
#' @param wave_labels Optional transition labels. Column names from `observed`
#'   are used when available.
#' @param policy_id Optional policy identifier recorded as provenance.
#' @param learner_specification Optional aggregate description of the registered
#'   censoring learners.
#' @param out_of_fold_performance Optional aggregate out-of-fold performance
#'   record for the censoring learners.
#' @param na_is_unobserved Logical; whether an `NA` observation indicator denotes
#'   loss to follow-up. The default is `TRUE`.
#'
#' @return An object of class `margot_lmtp_censoring_report` containing retention,
#'   probability, factor, zero-cause, learner, and provenance records. The
#'   computed tables contain no participant-level rows; supplied learner records
#'   must likewise be aggregate.
#' @export
margot_lmtp_censoring_report <- function(observed,
                                         baseline_weights = NULL,
                                         fitted_probabilities = NULL,
                                         censoring_factors = NULL,
                                         joint_ratios = NULL,
                                         wave_labels = NULL,
                                         policy_id = NULL,
                                         learner_specification = NULL,
                                         out_of_fold_performance = NULL,
                                         na_is_unobserved = TRUE) {
  observed_raw <- .margot_evidence_matrix(observed, "observed")
  if (!is.logical(observed_raw) && !is.numeric(observed_raw)) {
    stop("`observed` must be logical or numeric.", call. = FALSE)
  }
  n <- nrow(observed_raw)
  tau <- ncol(observed_raw)
  observed_numeric <- suppressWarnings(matrix(
    as.numeric(observed_raw), nrow = n, ncol = tau,
    dimnames = dimnames(observed_raw)
  ))
  if (any(!is.na(observed_numeric) & !observed_numeric %in% c(0, 1))) {
    stop("`observed` must contain only logical, 0, 1, or `NA` values.", call. = FALSE)
  }
  observed_logical <- observed_numeric == 1
  missing_observed <- is.na(observed_logical)
  if (isTRUE(na_is_unobserved)) observed_logical[missing_observed] <- FALSE

  labels <- wave_labels %||% colnames(observed_raw)
  if (is.null(labels)) labels <- paste0("Wave ", seq_len(tau))
  if (length(labels) != tau || anyNA(labels) || anyDuplicated(labels)) {
    stop("`wave_labels` must uniquely label every censoring transition.", call. = FALSE)
  }
  weights <- .margot_evidence_weights(baseline_weights, n, "baseline_weights")

  probabilities <- NULL
  if (!is.null(fitted_probabilities)) {
    probabilities <- .margot_evidence_matrix(
      fitted_probabilities, "fitted_probabilities", n = n, numeric = TRUE
    )
    if (!identical(dim(probabilities), dim(observed_raw))) {
      stop("`fitted_probabilities` must have the same dimensions as `observed`.", call. = FALSE)
    }
    if (any(probabilities < 0 | probabilities > 1, na.rm = TRUE)) {
      stop("`fitted_probabilities` must lie between 0 and 1.", call. = FALSE)
    }
  }

  factors <- NULL
  if (!is.null(censoring_factors)) {
    factors <- .margot_evidence_matrix(
      censoring_factors, "censoring_factors", n = n, numeric = TRUE
    )
    if (!identical(dim(factors), dim(observed_raw))) {
      stop("`censoring_factors` must have the same dimensions as `observed`.", call. = FALSE)
    }
    if (any(factors < 0, na.rm = TRUE)) {
      stop("`censoring_factors` must be non-negative.", call. = FALSE)
    }
  }

  joint <- NULL
  if (!is.null(joint_ratios)) {
    joint <- .margot_evidence_matrix(
      joint_ratios, "joint_ratios", n = n, numeric = TRUE
    )
    if (!identical(dim(joint), dim(observed_raw))) {
      stop("`joint_ratios` must have the same dimensions as `observed`.", call. = FALSE)
    }
    if (any(joint < 0, na.rm = TRUE)) {
      stop("`joint_ratios` must be non-negative.", call. = FALSE)
    }
  }

  retention <- do.call(rbind, lapply(seq_len(tau), function(j) {
    obs <- observed_logical[, j]
    data.frame(
      policy_id = policy_id %||% NA_character_,
      wave = as.character(labels[[j]]),
      n_rows = n,
      n_indicator_missing = sum(missing_observed[, j]),
      n_observed = sum(obs, na.rm = TRUE),
      pct_observed_unweighted = 100 * mean(obs, na.rm = TRUE),
      pct_observed_weighted = 100 * .margot_evidence_weighted_prop(obs, weights),
      stringsAsFactors = FALSE
    )
  }))

  probability_summary <- if (is.null(probabilities)) NULL else do.call(
    rbind,
    lapply(seq_len(tau), function(j) {
      cbind(
        data.frame(
          policy_id = policy_id %||% NA_character_,
          wave = as.character(labels[[j]]),
          stringsAsFactors = FALSE
        ),
        .margot_evidence_numeric_summary(probabilities[, j])
      )
    })
  )

  factor_summary <- NULL
  joint_summary <- NULL
  zero_causes <- NULL
  if (!is.null(factors)) {
    factor_summary <- do.call(rbind, lapply(seq_len(tau), function(j) {
      cbind(
        data.frame(
          policy_id = policy_id %||% NA_character_,
          wave = as.character(labels[[j]]),
          stringsAsFactors = FALSE
        ),
        .margot_evidence_numeric_summary(factors[, j])
      )
    }))
    zero_causes <- do.call(rbind, lapply(seq_len(tau), function(j) {
      zero <- is.finite(factors[, j]) & factors[, j] == 0
      data.frame(
        policy_id = policy_id %||% NA_character_,
        wave = as.character(labels[[j]]),
        component = "censoring_factor",
        cause = c(
          "zero_factor_after_non_observation",
          "zero_factor_while_observed",
          "factor_missing_or_non_finite"
        ),
        n = c(
          sum(zero & !observed_logical[, j]),
          sum(zero & observed_logical[, j]),
          sum(!is.finite(factors[, j]))
        ),
        stringsAsFactors = FALSE
      )
    }))
  }
  if (!is.null(joint)) {
    joint_summary <- do.call(rbind, lapply(seq_len(tau), function(j) {
      cbind(
        data.frame(
          policy_id = policy_id %||% NA_character_,
          wave = as.character(labels[[j]]),
          stringsAsFactors = FALSE
        ),
        .margot_evidence_numeric_summary(joint[, j])
      )
    }))
    joint_zero_causes <- do.call(rbind, lapply(seq_len(tau), function(j) {
      joint_zero <- is.finite(joint[, j]) & joint[, j] == 0
      factor_zero <- if (is.null(factors)) {
        rep(NA, n)
      } else {
        is.finite(factors[, j]) & factors[, j] == 0
      }
      data.frame(
        policy_id = policy_id %||% NA_character_,
        wave = as.character(labels[[j]]),
        component = "joint_ratio",
        cause = c(
          "zero_joint_ratio_with_zero_censoring_factor",
          "zero_joint_ratio_with_positive_censoring_factor",
          "zero_joint_ratio_with_missing_or_non_finite_censoring_factor",
          "zero_joint_ratio_without_component_factor",
          "joint_ratio_missing_or_non_finite"
        ),
        n = c(
          if (is.null(factors)) 0L else sum(joint_zero & factor_zero),
          if (is.null(factors)) 0L else sum(
            joint_zero & is.finite(factors[, j]) & factors[, j] > 0
          ),
          if (is.null(factors)) 0L else sum(joint_zero & !is.finite(factors[, j])),
          if (is.null(factors)) sum(joint_zero) else 0L,
          sum(!is.finite(joint[, j]))
        ),
        stringsAsFactors = FALSE
      )
    }))
    zero_causes <- if (is.null(zero_causes)) {
      joint_zero_causes
    } else {
      rbind(zero_causes, joint_zero_causes)
    }
  }

  structure(
    list(
      retention = retention,
      fitted_probability_summary = probability_summary,
      censoring_factor_summary = factor_summary,
      joint_ratio_summary = joint_summary,
      exact_zero_causes = zero_causes,
      learner_specification = learner_specification,
      out_of_fold_performance = out_of_fold_performance,
      metadata = list(
        policy_id = policy_id,
        wave_labels = as.character(labels),
        na_is_unobserved = isTRUE(na_is_unobserved),
        censoring_factors_supplied = !is.null(factors),
        joint_ratios_supplied = !is.null(joint),
        fitted_probabilities_supplied = !is.null(probabilities),
        decision_role = "descriptive_nonbinding"
      )
    ),
    class = c("margot_lmtp_censoring_report", "list")
  )
}

# purpose: produce source, projected-source, and target summaries for one
# numeric variable. inputs: values and weights; output: one balance row.
.margot_projection_numeric_balance <- function(variable,
                                               source,
                                               target,
                                               projection_weights,
                                               target_weights) {
  source_unweighted <- rep(1, length(source))
  source_mean <- .margot_evidence_weighted_mean(source, source_unweighted)
  projected_mean <- .margot_evidence_weighted_mean(source, projection_weights)
  target_mean <- .margot_evidence_weighted_mean(target, target_weights)
  source_sd <- .margot_evidence_weighted_sd(source, source_unweighted)
  projected_sd <- .margot_evidence_weighted_sd(source, projection_weights)
  target_sd <- .margot_evidence_weighted_sd(target, target_weights)
  before_scale <- sqrt((source_sd^2 + target_sd^2) / 2)
  after_scale <- sqrt((projected_sd^2 + target_sd^2) / 2)
  data.frame(
    variable = variable,
    variable_type = "numeric",
    level = NA_character_,
    source_n_observed = sum(is.finite(source)),
    target_n_observed = sum(is.finite(target)),
    source_value = source_mean,
    projected_source_value = projected_mean,
    target_value = target_mean,
    difference_before_projection = source_mean - target_mean,
    difference_after_projection = projected_mean - target_mean,
    standardised_difference_before = if (is.finite(before_scale) && before_scale > 0) {
      (source_mean - target_mean) / before_scale
    } else {
      NA_real_
    },
    standardised_difference_after = if (is.finite(after_scale) && after_scale > 0) {
      (projected_mean - target_mean) / after_scale
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

# purpose: produce source, projected-source, and target proportions for one
# categorical variable. inputs: values and weights; output: balance rows.
.margot_projection_categorical_balance <- function(variable,
                                                   source,
                                                   target,
                                                   projection_weights,
                                                   target_weights) {
  source_chr <- ifelse(is.na(source), "<missing>", as.character(source))
  target_chr <- ifelse(is.na(target), "<missing>", as.character(target))
  levels <- sort(unique(c(source_chr, target_chr)))
  do.call(rbind, lapply(levels, function(level) {
    source_indicator <- source_chr == level
    target_indicator <- target_chr == level
    source_value <- mean(source_indicator)
    projected_value <- .margot_evidence_weighted_prop(source_indicator, projection_weights)
    target_value <- .margot_evidence_weighted_prop(target_indicator, target_weights)
    data.frame(
      variable = variable,
      variable_type = "categorical",
      level = level,
      source_n_observed = sum(source_chr != "<missing>"),
      target_n_observed = sum(target_chr != "<missing>"),
      source_value = source_value,
      projected_source_value = projected_value,
      target_value = target_value,
      difference_before_projection = source_value - target_value,
      difference_after_projection = projected_value - target_value,
      standardised_difference_before = NA_real_,
      standardised_difference_after = NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

# purpose: create stable stratum labels including missing values. inputs: a data
# frame and variable names; output: one label per row.
.margot_projection_strata <- function(data, variables) {
  values <- lapply(data[, variables, drop = FALSE], function(x) {
    ifelse(is.na(x), "<missing>", as.character(x))
  })
  do.call(paste, c(values, sep = " | "))
}

#' Report target-population projection evidence
#'
#' `margot_target_projection_report()` compares the realised source sample with
#' the registered target population before and after projection weighting. It
#' reports aggregate balance, projection-weight concentration, and source
#' representation across registered strata. The function returns evidence and
#' provenance without an accept-or-reject classification.
#'
#' @param source A source-sample data frame.
#' @param target A target-population data frame or aggregate target microdata.
#' @param variables Character vector naming the registered projection variables
#'   present in both data frames.
#' @param projection_weights Optional non-negative source-sample projection
#'   weights. Equal weights represent an unweighted projection.
#' @param target_weights Optional non-negative target-population weights. Equal
#'   weights are used when this argument is `NULL`.
#' @param strata Optional subset of `variables` defining the registered overlap
#'   strata. The report describes target strata that lack source representation.
#' @param harmonisation Optional aggregate record of the source-to-target
#'   harmonisation rules.
#' @param projection_model Optional aggregate description of the projection
#'   model or supplied-weight construction.
#' @param uncertainty Optional aggregate description of projection-weight or
#'   target-margin uncertainty.
#' @param top_shares Fractions used for projection-weight concentration summaries.
#'
#' @return An object of class `margot_target_projection_report` containing a
#'   variable-level balance table, stratum representation table, projection-weight
#'   summary, concentration table, and provenance. The return value contains no
#'   participant-level rows.
#' @export
margot_target_projection_report <- function(source,
                                            target,
                                            variables,
                                            projection_weights = NULL,
                                            target_weights = NULL,
                                            strata = NULL,
                                            harmonisation = NULL,
                                            projection_model = NULL,
                                            uncertainty = NULL,
                                            top_shares = c(0.01, 0.05, 0.10)) {
  if (!is.data.frame(source) || !is.data.frame(target)) {
    stop("`source` and `target` must be data frames.", call. = FALSE)
  }
  variables <- unique(as.character(variables))
  if (!length(variables) || anyNA(variables) || any(!nzchar(variables))) {
    stop("`variables` must name at least one projection variable.", call. = FALSE)
  }
  missing_source <- setdiff(variables, names(source))
  missing_target <- setdiff(variables, names(target))
  if (length(missing_source) || length(missing_target)) {
    stop(
      "Projection variables are missing: source [",
      paste(missing_source, collapse = ", "), "]; target [",
      paste(missing_target, collapse = ", "), "].",
      call. = FALSE
    )
  }
  source_weights <- .margot_evidence_weights(
    projection_weights, nrow(source), "projection_weights"
  )
  target_weights <- .margot_evidence_weights(
    target_weights, nrow(target), "target_weights"
  )

  balance <- do.call(rbind, lapply(variables, function(variable) {
    source_value <- source[[variable]]
    target_value <- target[[variable]]
    source_numeric <- is.numeric(source_value)
    target_numeric <- is.numeric(target_value)
    if (!identical(source_numeric, target_numeric)) {
      stop("Projection variable `", variable, "` has incompatible source and target types.", call. = FALSE)
    }
    if (source_numeric) {
      .margot_projection_numeric_balance(
        variable,
        as.numeric(source_value),
        as.numeric(target_value),
        source_weights,
        target_weights
      )
    } else {
      .margot_projection_categorical_balance(
        variable,
        source_value,
        target_value,
        source_weights,
        target_weights
      )
    }
  }))
  rownames(balance) <- NULL

  strata_table <- NULL
  if (!is.null(strata)) {
    strata <- unique(as.character(strata))
    if (!length(strata) || any(!strata %in% variables)) {
      stop("`strata` must name one or more registered projection variables.", call. = FALSE)
    }
    source_labels <- .margot_projection_strata(source, strata)
    target_labels <- .margot_projection_strata(target, strata)
    labels <- sort(unique(c(source_labels, target_labels)))
    source_total <- sum(source_weights[is.finite(source_weights)], na.rm = TRUE)
    target_total <- sum(target_weights[is.finite(target_weights)], na.rm = TRUE)
    strata_table <- do.call(rbind, lapply(labels, function(label) {
      source_rows <- source_labels == label
      target_rows <- target_labels == label
      source_mass <- sum(source_weights[source_rows], na.rm = TRUE)
      target_mass <- sum(target_weights[target_rows], na.rm = TRUE)
      data.frame(
        stratum = label,
        source_n = sum(source_rows),
        target_n = sum(target_rows),
        projected_source_weight_share = if (source_total > 0) source_mass / source_total else NA_real_,
        target_weight_share = if (target_total > 0) target_mass / target_total else NA_real_,
        represented_in_source = sum(source_rows) > 0 && source_mass > 0,
        stringsAsFactors = FALSE
      )
    }))
    rownames(strata_table) <- NULL
  }

  weight_report <- .margot_evidence_weight_summary(
    source_weights,
    stage = "projection_weight",
    top_shares = top_shares
  )
  structure(
    list(
      balance = balance,
      stratum_representation = strata_table,
      weight_summary = weight_report$summary,
      weight_concentration = weight_report$concentration,
      harmonisation = harmonisation,
      projection_model = projection_model,
      uncertainty = uncertainty,
      metadata = list(
        variables = variables,
        strata = strata,
        source_n = nrow(source),
        target_n = nrow(target),
        decision_role = "descriptive_nonbinding"
      )
    ),
    class = c("margot_target_projection_report", "list")
  )
}

# purpose: construct cumulative products while retaining zeros and propagating
# missing or non-finite values. inputs: ratio matrix; output: cumulative matrix.
.margot_cumulative_ratios <- function(ratios) {
  out <- matrix(NA_real_, nrow(ratios), ncol(ratios), dimnames = dimnames(ratios))
  if (!ncol(ratios)) return(out)
  out[, 1L] <- ratios[, 1L]
  if (ncol(ratios) > 1L) {
    for (j in 2:ncol(ratios)) {
      previous <- out[, j - 1L]
      current <- ratios[, j]
      out[, j] <- previous * current
      out[is.finite(previous) & previous == 0, j] <- 0
    }
  }
  out
}

# purpose: tabulate the first structural cause of zero or missing full weights.
# inputs: baseline weights, joint ratios, and wave labels; output: aggregate rows.
.margot_analysis_weight_causes <- function(baseline_weights, ratios, labels, stage) {
  causes <- character(length(baseline_weights))
  causes[is.na(baseline_weights)] <- "baseline_weight_missing"
  causes[!is.na(baseline_weights) & !is.finite(baseline_weights)] <- "baseline_weight_non_finite"
  causes[is.finite(baseline_weights) & baseline_weights == 0] <- "baseline_weight_zero"
  unresolved <- !nzchar(causes)
  for (j in seq_len(ncol(ratios))) {
    ratio <- ratios[, j]
    missing <- unresolved & is.na(ratio)
    non_finite <- unresolved & !is.na(ratio) & !is.finite(ratio)
    zero <- unresolved & is.finite(ratio) & ratio == 0
    causes[missing] <- paste0("joint_ratio_missing_at_", labels[[j]])
    causes[non_finite] <- paste0("joint_ratio_non_finite_at_", labels[[j]])
    causes[zero] <- paste0("joint_ratio_zero_at_", labels[[j]])
    unresolved <- !nzchar(causes)
  }
  causes[unresolved] <- "positive_finite_full_weight"
  counts <- as.data.frame(table(causes), stringsAsFactors = FALSE)
  names(counts) <- c("cause", "n")
  counts$stage <- stage
  counts[, c("stage", "cause", "n")]
}

#' Report full LMTP analysis-weight concentration
#'
#' `margot_lmtp_analysis_weight_report()` multiplies the baseline design weight
#' by the cumulative joint exposure-and-censoring ratio at each longitudinal
#' node. Zero weights remain in every distribution and Kish effective-sample-size
#' denominator. The function reports aggregate concentration without attaching a
#' positivity or estimator-stability judgement.
#'
#' @param baseline_weights Non-negative baseline design weights, one per participant.
#' @param joint_ratios A non-negative matrix of per-node joint exposure-and-censoring
#'   ratios, with one participant per row and one longitudinal node per column.
#' @param regularised_joint_ratios Optional matrix after the registered numerical
#'   regularisation, with the same dimensions as `joint_ratios`.
#' @param wave_labels Optional longitudinal-node labels. Column names from
#'   `joint_ratios` are used when available.
#' @param policy_id Optional policy identifier recorded as provenance.
#' @param top_shares Fractions used for concentration summaries.
#' @param regularisation Optional aggregate description of the registered
#'   numerical regularisation.
#'
#' @return An object of class `margot_lmtp_analysis_weight_report` containing
#'   per-wave summaries, top-weight shares, aggregate zero and missingness causes,
#'   regularisation comparisons, and provenance. The return value contains no
#'   participant-level weights.
#' @export
margot_lmtp_analysis_weight_report <- function(baseline_weights,
                                               joint_ratios,
                                               regularised_joint_ratios = NULL,
                                               wave_labels = NULL,
                                               policy_id = NULL,
                                               top_shares = c(0.01, 0.05, 0.10),
                                               regularisation = NULL) {
  ratios <- .margot_evidence_matrix(joint_ratios, "joint_ratios", numeric = TRUE)
  n <- nrow(ratios)
  tau <- ncol(ratios)
  if (!n || !tau) stop("`joint_ratios` must be a non-empty matrix.", call. = FALSE)
  if (any(ratios < 0, na.rm = TRUE)) {
    stop("`joint_ratios` must be non-negative.", call. = FALSE)
  }
  baseline_weights <- .margot_evidence_weights(
    baseline_weights, n, "baseline_weights"
  )
  labels <- wave_labels %||% colnames(ratios)
  if (is.null(labels)) labels <- paste0("Wave ", seq_len(tau))
  if (length(labels) != tau || anyNA(labels) || anyDuplicated(labels)) {
    stop("`wave_labels` must uniquely label every joint-ratio column.", call. = FALSE)
  }

  regularised <- NULL
  if (!is.null(regularised_joint_ratios)) {
    regularised <- .margot_evidence_matrix(
      regularised_joint_ratios,
      "regularised_joint_ratios",
      n = n,
      numeric = TRUE
    )
    if (!identical(dim(regularised), dim(ratios))) {
      stop("`regularised_joint_ratios` must have the same dimensions as `joint_ratios`.", call. = FALSE)
    }
    if (any(regularised < 0, na.rm = TRUE)) {
      stop("`regularised_joint_ratios` must be non-negative.", call. = FALSE)
    }
  }

  describe_stage <- function(stage_ratios, stage) {
    cumulative <- .margot_cumulative_ratios(stage_ratios)
    summaries <- vector("list", tau)
    concentration <- vector("list", tau)
    for (j in seq_len(tau)) {
      full_weights <- baseline_weights * cumulative[, j]
      report <- .margot_evidence_weight_summary(full_weights, stage, top_shares)
      report$summary$policy_id <- policy_id %||% NA_character_
      report$summary$wave <- as.character(labels[[j]])
      report$concentration$policy_id <- policy_id %||% NA_character_
      report$concentration$wave <- as.character(labels[[j]])
      summaries[[j]] <- report$summary[, c(
        "policy_id", "wave", setdiff(names(report$summary), c("policy_id", "wave"))
      )]
      concentration[[j]] <- report$concentration[, c(
        "policy_id", "wave", setdiff(names(report$concentration), c("policy_id", "wave"))
      )]
    }
    list(
      summary = do.call(rbind, summaries),
      concentration = do.call(rbind, concentration),
      causes = .margot_analysis_weight_causes(
        baseline_weights, stage_ratios, as.character(labels), stage
      ),
      final_weights = baseline_weights * cumulative[, tau]
    )
  }

  raw_report <- describe_stage(ratios, "raw")
  stages <- list(raw_report)
  if (!is.null(regularised)) {
    stages[[2L]] <- describe_stage(regularised, "regularised")
  }
  summary <- do.call(rbind, lapply(stages, `[[`, "summary"))
  concentration <- do.call(rbind, lapply(stages, `[[`, "concentration"))
  causes <- do.call(rbind, lapply(stages, `[[`, "causes"))
  rownames(summary) <- rownames(concentration) <- rownames(causes) <- NULL

  regularisation_comparison <- NULL
  if (length(stages) == 2L) {
    raw <- stages[[1L]]$final_weights
    reg <- stages[[2L]]$final_weights
    raw_total <- sum(raw[is.finite(raw)], na.rm = TRUE)
    reg_total <- sum(reg[is.finite(reg)], na.rm = TRUE)
    regularisation_comparison <- data.frame(
      policy_id = policy_id %||% NA_character_,
      raw_final_weight_sum = raw_total,
      regularised_final_weight_sum = reg_total,
      difference = reg_total - raw_total,
      relative_difference = if (raw_total != 0) (reg_total - raw_total) / raw_total else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      weight_summary = summary,
      weight_concentration = concentration,
      exact_zero_and_missing_causes = causes,
      regularisation_comparison = regularisation_comparison,
      regularisation = regularisation,
      metadata = list(
        policy_id = policy_id,
        wave_labels = as.character(labels),
        regularised_ratios_supplied = !is.null(regularised),
        zero_weights_included = TRUE,
        decision_role = "descriptive_nonbinding"
      )
    ),
    class = c("margot_lmtp_analysis_weight_report", "list")
  )
}

#' Assemble the mandatory nonbinding LMTP evidence reports
#'
#' `margot_lmtp_evidence_report()` combines the censoring, target-population
#' projection, and full-analysis-weight reports under stable names. A structurally
#' unavailable component requires an explicit reason. The manifest records
#' availability without assigning a scientific or routing verdict.
#'
#' @param censoring_report A `margot_lmtp_censoring_report` object or `NULL`.
#' @param projection_report A `margot_target_projection_report` object or `NULL`.
#' @param analysis_weight_reports One `margot_lmtp_analysis_weight_report` object,
#'   a named list of such objects, or `NULL`.
#' @param missing_reasons Named character vector giving the structural reason for
#'   each missing component. Permitted names are `censoring_report`,
#'   `projection_report`, and `analysis_weight_reports`.
#'
#' @return An object of class `margot_lmtp_evidence_report` containing the three
#'   report families and an availability manifest.
#' @export
margot_lmtp_evidence_report <- function(censoring_report = NULL,
                                        projection_report = NULL,
                                        analysis_weight_reports = NULL,
                                        missing_reasons = character()) {
  if (!is.character(missing_reasons) ||
      (length(missing_reasons) && is.null(names(missing_reasons)))) {
    stop("`missing_reasons` must be a named character vector.", call. = FALSE)
  }
  if (length(missing_reasons) &&
      (anyNA(names(missing_reasons)) || any(!nzchar(names(missing_reasons))) ||
       anyDuplicated(names(missing_reasons)) || anyNA(missing_reasons) ||
       any(!nzchar(trimws(missing_reasons))))) {
    stop(
      "`missing_reasons` must uniquely name a non-empty structural reason.",
      call. = FALSE
    )
  }
  allowed <- c("censoring_report", "projection_report", "analysis_weight_reports")
  if (any(!names(missing_reasons) %in% allowed)) {
    stop("`missing_reasons` contains an unknown component name.", call. = FALSE)
  }
  validate_one <- function(x, class_name, name) {
    if (!is.null(x) && !inherits(x, class_name)) {
      stop("`", name, "` must inherit from `", class_name, "`.", call. = FALSE)
    }
  }
  validate_one(censoring_report, "margot_lmtp_censoring_report", "censoring_report")
  validate_one(projection_report, "margot_target_projection_report", "projection_report")
  if (!is.null(analysis_weight_reports)) {
    if (inherits(analysis_weight_reports, "margot_lmtp_analysis_weight_report")) {
      analysis_weight_reports <- list(analysis_weight_reports)
    }
    if (!is.list(analysis_weight_reports) || !length(analysis_weight_reports) ||
        !all(vapply(
          analysis_weight_reports,
          inherits,
          logical(1),
          what = "margot_lmtp_analysis_weight_report"
        ))) {
      stop(
        "`analysis_weight_reports` must contain only `margot_lmtp_analysis_weight_report` objects.",
        call. = FALSE
      )
    }
  }
  components <- list(
    censoring_report = censoring_report,
    projection_report = projection_report,
    analysis_weight_reports = analysis_weight_reports
  )
  available <- !vapply(components, is.null, logical(1))
  missing_names <- names(available)[!available]
  missing_without_reason <- setdiff(missing_names, names(missing_reasons))
  if (length(missing_without_reason)) {
    stop(
      "Every missing report requires a structural reason: ",
      paste(missing_without_reason, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  reasons_for_available <- intersect(names(available)[available], names(missing_reasons))
  if (length(reasons_for_available)) {
    stop(
      "A supplied structural reason names an available report: ",
      paste(reasons_for_available, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  manifest <- data.frame(
    object = names(components),
    available = unname(available),
    structural_reason = vapply(names(components), function(name) {
      if (available[[name]]) NA_character_ else unname(missing_reasons[[name]])
    }, character(1)),
    decision_role = "descriptive_nonbinding",
    stringsAsFactors = FALSE
  )
  structure(
    c(components, list(manifest = manifest)),
    class = c("margot_lmtp_evidence_report", "list")
  )
}
