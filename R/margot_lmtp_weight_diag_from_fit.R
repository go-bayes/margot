#' Density-ratio diagnostics from an LMTP fit
#'
#' Constructs per-wave density-ratio summaries and cumulative effective sample
#' sizes (ESS) directly from an LMTP model object. For incremental propensity
#' score interventions (IPSI) with \eqn{\delta > 1}, zeros in the density ratios
#' arise solely from dropout, so the helper reconstructs the observation mask via
#' `density_ratios > 0`, computes per-wave quantiles/tail probabilities, applies
#' optional right-tail winsorisation, and reports ESS for the cumulative weight
#' trajectories (raw and trimmed).
#'
#' @param fit Either a single LMTP model (anything exposing `$density_ratios`) or
#'   a full `margot_lmtp()` result.
#' @param outcome Optional outcome name (required when `fit` is a full
#'   `margot_lmtp()` run).
#' @param shift Optional shift name (clean suffix or full). Required when `fit`
#'   is a full `margot_lmtp()` run with multiple shifts.
#' @param trim_right Numeric in `(0, 1]`; optional right-tail winsorisation level
#'   applied per wave before forming cumulative products (default `0.999`).
#' @param thresholds Numeric vector of ratio thresholds used when computing tail
#'   mass (`Pr(r_t > a)`).
#'
#' @return A list with `wave_table` (per-wave diagnostics), `mask_from_fit`
#'   (logical observation mask), `density_ratios`, and cumulative weights
#'   (`w_cum_raw`, `w_cum_trim`).
#' @examples
#' dr <- matrix(c(2, 0, 1,
#'                1.5, 0.5, 2), nrow = 2, byrow = TRUE)
#' fit <- list(density_ratios = dr)
#' diagnostics <- margot_lmtp_weight_diag_from_fit(fit)
#' diagnostics$wave_table
#' @export
margot_lmtp_weight_diag_from_fit <- function(fit,
                                             outcome = NULL,
                                             shift = NULL,
                                             trim_right = 0.999,
                                             thresholds = c(5, 10, 25, 50, 100)) {
  resolve_model <- function(obj, outcome, shift) {
    if (!is.null(obj$models)) {
      if (is.null(outcome)) stop("Provide `outcome` when passing a margot_lmtp() run.")
      if (!outcome %in% names(obj$models)) stop("Outcome not found in fit$models: ", outcome)
      shift_models <- obj$models[[outcome]]
      if (!length(shift_models)) stop("No shift models stored for outcome ", outcome)
      clean <- function(nm) {
        pref <- paste0(outcome, "_")
        if (startsWith(nm, pref)) substring(nm, nchar(pref) + 1L) else nm
      }
      shift_names <- names(shift_models)
      clean_names <- vapply(shift_names, clean, character(1))
      idx <- NULL
      if (!is.null(shift)) {
        idx <- which(shift_names == shift | clean_names == shift)
        if (!length(idx)) stop("Shift not found for outcome ", outcome, ": ", shift)
      } else {
        if (length(shift_models) == 1L) idx <- 1L else stop("Multiple shifts available; supply `shift`.")
      }
      mod <- shift_models[[idx[1]]]
    } else {
      mod <- obj
    }
    if (is.environment(mod)) mod <- as.list.environment(mod)
    if (inherits(mod, "lmtp")) mod <- as.list(mod)
    mod
  }

  fit_obj <- resolve_model(fit, outcome, shift)
  if (is.null(fit_obj$density_ratios)) stop("Selected model does not expose $density_ratios.")
  r_raw <- fit_obj$density_ratios
  if (inherits(r_raw, "Matrix")) r_raw <- as.matrix(r_raw)
  if (!is.matrix(r_raw)) r_raw <- as.matrix(r_raw)
  storage.mode(r_raw) <- "double"
  n <- nrow(r_raw); tau <- ncol(r_raw)
  if (!n || !tau) stop("density_ratios must be a non-empty matrix.")

  mask <- r_raw > 0

  r_trim <- matrix(NA_real_, n, tau)
  for (t in seq_len(tau)) {
    xt <- ifelse(mask[, t], r_raw[, t], NA_real_)
    if (trim_right > 0 && trim_right < 1 && sum(is.finite(xt)) > 0) {
      q <- stats::quantile(xt, probs = trim_right, na.rm = TRUE, names = FALSE)
      r_trim[, t] <- ifelse(mask[, t], pmin(r_raw[, t], q), NA_real_)
    } else {
      r_trim[, t] <- xt
    }
  }

  cumulate <- function(mat) {
    out <- matrix(NA_real_, nrow(mat), ncol(mat))
    if (!ncol(mat)) return(out)
    out[, 1] <- mat[, 1]
    if (ncol(mat) >= 2) {
      for (t in 2:ncol(mat)) {
        prev <- out[, t - 1]
        curr <- mat[, t]
        out[, t] <- ifelse(is.finite(prev) & is.finite(curr), prev * curr, NA_real_)
      }
    }
    out
  }
  w_raw <- cumulate(ifelse(mask, r_raw, NA_real_))
  w_trim <- cumulate(r_trim)

  ess <- function(w) {
    ok <- is.finite(w) & (w > 0)
    if (!any(ok)) return(NA_real_)
    ww <- w[ok]
    (sum(ww))^2 / sum(ww^2)
  }

  q_probs <- c(0.5, 0.9, 0.95, 0.99, 0.995, 0.999)
  wave_rows <- lapply(seq_len(tau), function(t) {
    xt <- ifelse(mask[, t], r_raw[, t], NA_real_)
    xt_tr <- r_trim[, t]
    wt <- w_raw[, t]
    wt_tr <- w_trim[, t]
    fracs <- vapply(thresholds, function(a) mean(xt > a, na.rm = TRUE), numeric(1))
    data.frame(
      wave = t,
      n_obs = sum(mask[, t], na.rm = TRUE),
      prop_censored = mean(!mask[, t], na.rm = TRUE),
      r_q50  = stats::quantile(xt,    probs = q_probs[1], na.rm = TRUE, names = FALSE),
      r_q90  = stats::quantile(xt,    probs = q_probs[2], na.rm = TRUE, names = FALSE),
      r_q95  = stats::quantile(xt,    probs = q_probs[3], na.rm = TRUE, names = FALSE),
      r_q99  = stats::quantile(xt,    probs = q_probs[4], na.rm = TRUE, names = FALSE),
      r_q999 = stats::quantile(xt,    probs = q_probs[6], na.rm = TRUE, names = FALSE),
      rtrim_q99  = stats::quantile(xt_tr, probs = q_probs[4], na.rm = TRUE, names = FALSE),
      rtrim_q999 = stats::quantile(xt_tr, probs = q_probs[6], na.rm = TRUE, names = FALSE),
      frac_gt_5   = fracs[match(5,   thresholds, nomatch = NA)],
      frac_gt_10  = fracs[match(10,  thresholds, nomatch = NA)],
      frac_gt_25  = fracs[match(25,  thresholds, nomatch = NA)],
      frac_gt_50  = fracs[match(50,  thresholds, nomatch = NA)],
      frac_gt_100 = fracs[match(100, thresholds, nomatch = NA)],
      ess_cum_raw  = ess(wt),
      ess_cum_trim = ess(wt_tr)
    )
  })
  wave_table <- do.call(rbind, wave_rows)

  list(
    wave_table = wave_table,
    mask_from_fit = mask,
    density_ratios = r_raw,
    w_cum_raw = w_raw,
    w_cum_trim = w_trim
  )
}
