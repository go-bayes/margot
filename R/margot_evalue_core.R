# Convert an OLS estimate to the approximate risk-ratio scale.
.margot_ols_to_rr <- function(est, sd, delta = 1) {
  if (!is.numeric(est) || length(est) < 1L || anyNA(est)) {
    stop("`est` must contain non-missing numeric values.", call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || !is.finite(sd) || sd <= 0) {
    stop("`sd` must be one positive finite numeric value.", call. = FALSE)
  }
  if (!is.numeric(delta) || length(delta) != 1L || is.na(delta) || !is.finite(delta)) {
    stop("`delta` must be one finite numeric value.", call. = FALSE)
  }

  exp(0.91 * est * abs(delta) / sd)
}

# Compute the null E-value for one risk ratio.
.margot_evalue_threshold <- function(risk_ratio) {
  if (!is.numeric(risk_ratio) || length(risk_ratio) != 1L || is.na(risk_ratio)) {
    stop("`risk_ratio` must be one non-missing numeric value.", call. = FALSE)
  }
  if (risk_ratio < 0) {
    stop("`risk_ratio` cannot be negative.", call. = FALSE)
  }

  ratio_away_from_null <- if (risk_ratio <= 1) 1 / risk_ratio else risk_ratio
  ratio_away_from_null + sqrt(ratio_away_from_null * (ratio_away_from_null - 1))
}

# Compute point and confidence-bound E-values for one risk-ratio estimate.
.margot_evalues_rr <- function(est, lo = NA_real_, hi = NA_real_) {
  inputs <- list(est = est, lo = lo, hi = hi)
  valid_scalar <- vapply(
    inputs,
    function(x) is.numeric(x) && length(x) == 1L && !is.nan(x),
    logical(1)
  )
  if (!all(valid_scalar)) {
    stop("`est`, `lo`, and `hi` must each be one numeric value; interval limits may be `NA`.", call. = FALSE)
  }
  if (is.na(est)) {
    stop("`est` cannot be missing.", call. = FALSE)
  }
  if (est < 0 || (!is.na(lo) && lo < 0) || (!is.na(hi) && hi < 0)) {
    stop("Risk ratios and their confidence limits cannot be negative.", call. = FALSE)
  }
  if (!is.na(lo) && !is.na(hi) && lo > hi) {
    stop("`lo` must be less than or equal to `hi`.", call. = FALSE)
  }
  if ((!is.na(lo) && est < lo) || (!is.na(hi) && est > hi)) {
    stop("`est` must lie within the confidence interval.", call. = FALSE)
  }

  point <- .margot_evalue_threshold(est)
  bound <- 1

  if (est > 1 && !is.na(lo)) {
    bound <- if (lo < 1) 1 else .margot_evalue_threshold(lo)
  } else if (est < 1 && !is.na(hi)) {
    bound <- if (hi > 1) 1 else .margot_evalue_threshold(hi)
  }

  c(E_Value = point, E_Val_bound = bound)
}

# Compute point and confidence-bound E-values for one OLS estimate.
.margot_evalues_ols <- function(est, se = NA_real_, sd, delta = 1) {
  if (!is.numeric(se) || length(se) != 1L || is.nan(se) || (!is.na(se) && (!is.finite(se) || se < 0))) {
    stop("`se` must be one non-negative finite numeric value or `NA`.", call. = FALSE)
  }

  rr_point <- .margot_ols_to_rr(est, sd = sd, delta = delta)
  if (is.na(se)) {
    return(.margot_evalues_rr(rr_point))
  }

  standardised_estimate <- est * abs(delta) / sd
  standardised_se <- se * abs(delta) / sd
  rr_lo <- exp(0.91 * standardised_estimate - 1.78 * standardised_se)
  rr_hi <- exp(0.91 * standardised_estimate + 1.78 * standardised_se)

  .margot_evalues_rr(rr_point, lo = rr_lo, hi = rr_hi)
}

# Compute E-values from unrounded model-summary rows.
.margot_compute_evalues <- function(tab, scale, delta, sd) {
  if (!is.data.frame(tab) || nrow(tab) < 1L) {
    stop("`tab` must be a data frame with at least one row.", call. = FALSE)
  }

  rows <- if (scale == "RD") {
    required <- c("E[Y(1)]-E[Y(0)]", "standard_error")
    if (!all(required %in% names(tab))) {
      stop("Difference-scale E-values require estimate and `standard_error` columns.", call. = FALSE)
    }
    Map(
      function(est, se) .margot_evalues_ols(est, se = se, sd = sd, delta = delta),
      tab[[required[1]]],
      tab[[required[2]]]
    )
  } else {
    required <- c("E[Y(1)]/E[Y(0)]", "2.5 %", "97.5 %")
    if (!all(required %in% names(tab))) {
      stop("Risk-ratio E-values require estimate and confidence-limit columns.", call. = FALSE)
    }
    Map(
      function(est, lo, hi) .margot_evalues_rr(est, lo = lo, hi = hi),
      tab[[required[1]]],
      tab[[required[2]]],
      tab[[required[3]]]
    )
  }

  values <- do.call(rbind, rows)
  tibble::as_tibble(values)
}
