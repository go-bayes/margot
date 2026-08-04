#' Best linear projection of conditional average treatment effects
#'
#' Computes `grf::best_linear_projection()` for every causal forest retained by
#' [margot_causal_forest()], projecting each forest's estimated conditional
#' average treatment effects onto the **full** covariate matrix the forest was
#' fitted on. This is the registered reporting surface: it replaces the internal
#' `blp_top` field, which projected onto a top-15 variable-importance screen and
#' is retired from reporting.
#'
#' Results are reported as estimates with 95% confidence intervals. No
#' significance stars, p-values, or multiplicity corrections are produced, by
#' design. Studies fitted with `use_train_test_split = FALSE` project on the
#' same sample used for the average treatment effect.
#'
#' Every projection is isolated: an outcome whose projection fails contributes a
#' single structured failure row and never aborts the batch.
#'
#' @param models A list returned by [margot_causal_forest()] fitted with
#'   `save_models = TRUE`. The covariate matrix is taken from the same object
#'   when `save_data = TRUE`, otherwise from each forest's stored design matrix.
#' @param covariates Optional numeric matrix of covariates to project onto.
#'   Defaults to `NULL`, which uses the matrix the forests were fitted on. When
#'   supplied, its dimensions and column names must match that matrix.
#' @param target_sample Character; passed to `grf::best_linear_projection()` as
#'   `target.sample`. One of `"all"` (default) or `"overlap"`.
#' @param model_names Optional character vector of outcome names (with or
#'   without the `model_` prefix) restricting which forests are projected.
#'   Defaults to `NULL`, meaning every retained forest.
#' @param ... Further arguments passed to `grf::best_linear_projection()`.
#'
#' @return A data frame of class `margot_blp` with one row per outcome and
#'   coefficient, carrying the columns `outcome`, `term`, `estimate`,
#'   `std_error`, `conf_low`, `conf_high`, `target_sample`, `n`, `ess`,
#'   `matrix_fingerprint`, and `status`. Confidence intervals are 95% normal
#'   approximations from the coefficient table returned by `grf`. `ess` is the
#'   Kish effective sample size of the forest's sample weights, or `NA` when the
#'   forest carries no weights. `status` is `"ok"` or `"failed: <message>"`.
#'
#' @seealso [margot_table_blp()], [margot_plot_blp()]
#'
#' @examples
#' \dontrun{
#' cf <- margot_causal_forest(
#'   data = df, outcome_vars = outcomes, covariates = X, W = W,
#'   weights = w, save_models = TRUE, save_data = TRUE
#' )
#' blp <- margot_blp(cf)
#' margot_table_blp(blp)
#' margot_plot_blp(blp)
#' }
#'
#' @importFrom grf best_linear_projection
#' @importFrom stats qnorm
#' @export
margot_blp <- function(models,
                       covariates = NULL,
                       target_sample = c("all", "overlap"),
                       model_names = NULL,
                       ...) {
  # purpose: per-outcome best linear projection onto the full fitted covariate
  # set. inputs: a margot_causal_forest result (save_models = TRUE) and an
  # optional covariate matrix. output: a tidy margot_blp data frame.
  target_sample <- match.arg(target_sample)

  if (!is.list(models)) {
    stop("`models` must be a list returned by margot_causal_forest().", call. = FALSE)
  }
  if (!"full_models" %in% names(models) || length(models$full_models) == 0) {
    stop(
      "no fitted forests found in `models`. ",
      "re-run margot_causal_forest() with save_models = TRUE.",
      call. = FALSE
    )
  }

  forests <- models$full_models
  available <- names(forests)
  if (!is.null(model_names)) {
    wanted <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
    missing_models <- setdiff(wanted, available)
    if (length(missing_models) > 0) {
      stop(
        "no fitted forest for: ",
        paste(sub("^model_", "", missing_models), collapse = ", "),
        call. = FALSE
      )
    }
    available <- wanted
  }

  x_matrix <- .margot_blp_covariates(models, forests[available], covariates)
  fingerprint <- .margot_blp_fingerprint(x_matrix)

  rows <- lapply(available, function(mn) {
    forest <- forests[[mn]]
    outcome <- sub("^model_", "", mn)
    ess <- .margot_blp_ess(forest$sample.weights)

    projection <- tryCatch(
      grf::best_linear_projection(
        forest,
        A = x_matrix,
        target.sample = target_sample,
        ...
      ),
      error = function(e) e
    )

    if (inherits(projection, "error")) {
      return(data.frame(
        outcome = outcome,
        term = NA_character_,
        estimate = NA_real_,
        std_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        target_sample = target_sample,
        n = NA_integer_,
        ess = ess,
        matrix_fingerprint = fingerprint,
        status = paste0("failed: ", conditionMessage(projection)),
        stringsAsFactors = FALSE
      ))
    }

    coefs <- as.matrix(projection)
    estimate <- as.numeric(coefs[, 1])
    std_error <- as.numeric(coefs[, 2])
    # 95% normal-approximation interval; grf reports HC3 robust standard errors
    z <- stats::qnorm(0.975)
    # nobs is attached by lmtest::coeftest; fall back to the projection matrix
    n_used <- attr(projection, "nobs")
    if (is.null(n_used)) n_used <- nrow(x_matrix)

    data.frame(
      outcome = outcome,
      term = rownames(coefs),
      estimate = estimate,
      std_error = std_error,
      conf_low = estimate - z * std_error,
      conf_high = estimate + z * std_error,
      target_sample = target_sample,
      n = as.integer(n_used),
      ess = ess,
      matrix_fingerprint = fingerprint,
      status = "ok",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "relativity_note") <- .margot_blp_relativity_note()
  class(out) <- c("margot_blp", "data.frame")
  out
}

#' Reporting table of best linear projection coefficients
#'
#' Formats a [margot_blp()] result as a reporting table of estimates with 95%
#' confidence intervals. No significance stars and no p-values are produced, by
#' design.
#'
#' Every rendered table must carry the relativity sentence attached as the
#' `caption` attribute of the returned object. It states that the coefficients
#' are descriptive projections interpretable only relative to the other
#' covariates in the registered set.
#'
#' @param blp A `margot_blp` data frame returned by [margot_blp()].
#' @param outcomes Optional character vector of outcomes to retain, in the order
#'   given. Defaults to `NULL`, meaning every outcome.
#' @param terms Optional character vector of coefficient terms to retain, in the
#'   order given. Defaults to `NULL`, meaning every term.
#' @param digits Integer; decimal places for the reported estimate and interval.
#'   Default 2.
#'
#' @return A data frame of class `margot_blp_table` with columns `outcome`,
#'   `term`, `estimate`, `conf_low`, `conf_high`, `estimate_ci`, and `status`,
#'   carrying the mandatory relativity sentence as its `caption` attribute.
#'
#' @seealso [margot_blp()], [margot_plot_blp()]
#'
#' @examples
#' \dontrun{
#' blp <- margot_blp(cf)
#' tbl <- margot_table_blp(blp, digits = 2)
#' attr(tbl, "caption")
#' }
#'
#' @export
margot_table_blp <- function(blp, outcomes = NULL, terms = NULL, digits = 2) {
  # purpose: format a margot_blp result for reporting. inputs: the tidy frame
  # plus optional outcome/term filters. output: a data frame carrying an
  # "estimate [low, high]" column and the mandatory relativity caption.
  blp <- .margot_blp_filter(blp, outcomes, terms)

  fmt <- function(x) formatC(x, format = "f", digits = digits)
  estimate_ci <- ifelse(
    is.na(blp$estimate),
    NA_character_,
    paste0(fmt(blp$estimate), " [", fmt(blp$conf_low), ", ", fmt(blp$conf_high), "]")
  )

  out <- data.frame(
    outcome = blp$outcome,
    term = blp$term,
    estimate = round(blp$estimate, digits),
    conf_low = round(blp$conf_low, digits),
    conf_high = round(blp$conf_high, digits),
    estimate_ci = estimate_ci,
    status = blp$status,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL

  attr(out, "caption") <- .margot_blp_relativity_note()
  class(out) <- c("margot_blp_table", "data.frame")
  out
}

#' Plot best linear projection coefficients
#'
#' Draws a forest-style plot of [margot_blp()] coefficients with 95% confidence
#' intervals and a zero reference line, one facet per outcome. Nothing is
#' coloured or annotated by significance, by design.
#'
#' @param blp A `margot_blp` data frame returned by [margot_blp()].
#' @param outcomes Optional character vector of outcomes to retain, in the order
#'   given. Defaults to `NULL`, meaning every outcome.
#' @param terms Optional character vector of coefficient terms to retain, in the
#'   order given. Defaults to `NULL`, meaning every term.
#' @param title Optional plot title. Defaults to `NULL`.
#' @param caption Plot caption. Defaults to the mandatory relativity sentence.
#'
#' @return A `ggplot` object.
#'
#' @seealso [margot_blp()], [margot_table_blp()]
#'
#' @examples
#' \dontrun{
#' blp <- margot_blp(cf)
#' margot_plot_blp(blp)
#' }
#'
#' @importFrom rlang .data
#' @export
margot_plot_blp <- function(blp,
                            outcomes = NULL,
                            terms = NULL,
                            title = NULL,
                            caption = .margot_blp_relativity_note()) {
  # purpose: forest-style coefficient plot of a margot_blp result. inputs: the
  # tidy frame plus optional outcome/term filters. output: a ggplot object.
  blp <- .margot_blp_filter(blp, outcomes, terms)

  drawable <- blp[!is.na(blp$estimate), , drop = FALSE]
  if (nrow(drawable) == 0) {
    stop("no projections available to plot; every retained row failed.", call. = FALSE)
  }
  n_dropped <- nrow(blp) - nrow(drawable)
  if (n_dropped > 0) {
    cli::cli_alert_warning("dropping {n_dropped} row{?s} without an estimate from the plot")
  }

  # terms plot bottom-to-top in ggplot, so reverse the level order to read down
  term_levels <- rev(unique(drawable$term))
  drawable$term <- factor(drawable$term, levels = term_levels)
  drawable$outcome <- factor(drawable$outcome, levels = unique(drawable$outcome))

  # the relativity sentence is long; wrap it so the caption stays on the canvas
  if (!is.null(caption)) caption <- paste(strwrap(caption, width = 100), collapse = "\n")

  ggplot2::ggplot(
    drawable,
    ggplot2::aes(x = .data$estimate, y = .data$term)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      linewidth = 0.4, size = 0.3
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$outcome), scales = "free_x") +
    ggplot2::labs(
      title = title,
      x = "Coefficient (95% CI)",
      y = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.7))
    )
}

# --- internal helpers -------------------------------------------------------

# purpose: the registered relativity sentence every rendered table and plot
# must carry. output: a single character string.
.margot_blp_relativity_note <- function() {
  paste(
    "Coefficients are descriptive projections of the forest's estimated conditional effects",
    "onto the registered baseline covariate set; each coefficient is interpretable only",
    "relative to the other covariates in the set and is not a causal effect of the covariate."
  )
}

# purpose: resolve the covariate matrix to project onto and assert it is the
# single matrix every forest was fitted on. inputs: the result object, the
# retained forests, an optional user matrix. output: a numeric matrix.
.margot_blp_covariates <- function(models, forests, covariates) {
  fitted_matrices <- lapply(forests, function(f) f$X.orig)
  if (any(vapply(fitted_matrices, is.null, logical(1)))) {
    stop("at least one forest does not carry its design matrix (`X.orig`).", call. = FALSE)
  }

  # the batch fitter passes one matrix to every forest, so identity across
  # outcomes is guaranteed by construction; assert it rather than trust it
  reference <- fitted_matrices[[1]]
  for (i in seq_along(fitted_matrices)) {
    if (!identical(fitted_matrices[[i]], reference)) {
      stop(
        "the covariate matrix differs across outcomes (first mismatch: ",
        sub("^model_", "", names(forests)[i]),
        "); a single projection matrix is required.",
        call. = FALSE
      )
    }
  }

  x_matrix <- covariates
  if (is.null(x_matrix)) x_matrix <- models$covariates
  if (is.null(x_matrix)) x_matrix <- reference
  x_matrix <- as.matrix(x_matrix)

  if (!identical(dim(x_matrix), dim(reference))) {
    stop(
      "the supplied covariate matrix has dimensions ",
      paste(dim(x_matrix), collapse = " x "),
      " but the forests were fitted on ",
      paste(dim(reference), collapse = " x "), ".",
      call. = FALSE
    )
  }
  if (!identical(colnames(x_matrix), colnames(reference))) {
    stop("the supplied covariate matrix has different column names to the fitted matrix.", call. = FALSE)
  }
  x_matrix
}

# purpose: short reproducible fingerprint of the projection matrix from its
# dimensions and column names. inputs: a matrix. output: a 12-character string.
.margot_blp_fingerprint <- function(x) {
  substr(rlang::hash(list(dim(x), dimnames(x))), 1, 12)
}

# purpose: Kish effective sample size of a weight vector. inputs: sample
# weights or NULL. output: a numeric, NA when no weights are carried.
.margot_blp_ess <- function(weights) {
  if (is.null(weights) || length(weights) == 0 || all(is.na(weights))) {
    return(NA_real_)
  }
  w <- as.numeric(weights)
  sum(w)^2 / sum(w^2)
}

# purpose: validate a margot_blp object and apply outcome/term filters in the
# order requested. inputs: the object plus optional filters. output: a plain
# data frame.
.margot_blp_filter <- function(blp, outcomes, terms) {
  if (!inherits(blp, "margot_blp")) {
    stop("`blp` must be a `margot_blp` object returned by margot_blp().", call. = FALSE)
  }
  blp <- as.data.frame(blp, stringsAsFactors = FALSE)

  if (!is.null(outcomes)) {
    missing_outcomes <- setdiff(outcomes, blp$outcome)
    if (length(missing_outcomes) > 0) {
      stop("no projection rows for outcome: ", paste(missing_outcomes, collapse = ", "), call. = FALSE)
    }
    blp <- blp[order(match(blp$outcome, outcomes), na.last = NA), , drop = FALSE]
  }
  if (!is.null(terms)) {
    missing_terms <- setdiff(terms, blp$term)
    if (length(missing_terms) > 0) {
      stop("no projection rows for term: ", paste(missing_terms, collapse = ", "), call. = FALSE)
    }
    blp <- blp[blp$term %in% terms, , drop = FALSE]
    blp <- blp[order(match(blp$outcome, unique(blp$outcome)), match(blp$term, terms)), , drop = FALSE]
  }
  if (nrow(blp) == 0) {
    stop("no rows remain after filtering.", call. = FALSE)
  }
  rownames(blp) <- NULL
  blp
}
