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
#' @param outcome Optional outcome name. When `fit` is a full `margot_lmtp()`
#'   run and `outcome = NULL`, the first stored outcome is used.
#' @param shift Optional shift name (clean suffix or full). Required when `fit`
#'   is a full `margot_lmtp()` run with multiple shifts. Ignored when `shifts`
#'   is supplied.
#' @param shifts Optional character vector of shifts to summarise in a single
#'   call. When supplied, diagnostics are returned in the same order. When
#'   `NULL`, all available shifts for the selected outcome are returned.
#' @param trim_right Numeric in `(0, 1]`; optional right-tail winsorisation level
#'   applied per wave before forming cumulative products (default `0.999`).
#' @param thresholds Numeric vector of ratio thresholds used when computing tail
#'   mass (`Pr(r_t > a)`).
#' @param label_mapping Optional named list for relabelling wave indices (e.g.,
#'   via `label_mapping$wave_labels` or `label_mapping$wave_1 = "Baseline"`).
#'   Mirrors the conventions used by `margot_interpret_lmtp_positivity()`.
#'
#' @return For a single shift, a list with `wave_table` (per-wave diagnostics),
#'   `mask_from_fit` (logical observation mask), `density_ratios`, cumulative
#'   weights (`w_cum_raw`, `w_cum_trim`), and shift metadata. When multiple
#'   shifts are requested, a named list of such objects (one per shift) is
#'   returned.
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
                                             shifts = NULL,
                                             trim_right = 0.999,
                                             thresholds = c(5, 10, 25, 50, 100),
                                             label_mapping = NULL) {
  if (!is.null(shifts)) stopifnot(is.character(shifts))
  if (!is.null(shift)) {
    stopifnot(is.character(shift))
    if (is.null(shifts)) {
      shifts <- shift
    }
  }
  selection <- margot_resolve_positivity_selection(
    x = fit,
    outcome = outcome,
    shifts = shifts,
    caller = "margot_lmtp_weight_diag_from_fit"
  )
  outcome <- selection$outcome
  shift_models <- selection$outcome_models
  shift_df <- selection$shift_df
  if (!nrow(shift_df)) {
    stop("No shift models stored for outcome ", outcome)
  }

  map_wave_label <- function(idx) {
    key_num <- as.character(idx)
    key_full <- paste0("wave_", key_num)
    pull_label <- function(container, key) {
      if (is.null(container) || is.null(key)) return(NULL)
      val <- container[[key]]
      if (is.null(val) && !is.null(names(container)) && any(names(container) == key)) {
        val <- container[names(container) == key][[1]]
      }
      val
    }
    if (!is.null(label_mapping) && is.list(label_mapping)) {
      labs <- label_mapping$wave_labels %||% NULL
      val <- pull_label(labs, key_num)
      if (is.null(val)) val <- pull_label(labs, key_full)
      if (is.null(val)) val <- label_mapping[[key_full]]
      if (is.null(val)) val <- label_mapping[[key_num]]
      if (!is.null(val) && !is.na(val)) return(as.character(val)[1])
    }
    paste0("Wave ", key_num)
  }

  tidy_model <- function(mod) {
    if (is.environment(mod)) mod <- as.list.environment(mod)
    if (inherits(mod, "lmtp")) mod <- as.list(mod)
    mod
  }

  compute_diag <- function(mod, shift_full, shift_clean) {
    fit_obj <- tidy_model(mod)
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
      mask_col <- mask[, t]
      w_col <- as.numeric(r_raw[, t])
      mask_finite <- is.finite(w_col)
      n_total <- sum(mask_finite)
      w_pos_vec <- w_col[mask_col & mask_finite]
      n_pos <- length(w_pos_vec)
      ess_pos <- if (n_pos > 0) ess(w_pos_vec) else NA_real_
      ess_pos_frac <- if (n_pos > 0 && is.finite(ess_pos)) ess_pos / n_pos else NA_real_
      ess_pos_frac_pt <- if (n_total > 0 && is.finite(ess_pos)) ess_pos / n_total else NA_real_
      fracs <- vapply(thresholds, function(a) mean(xt > a, na.rm = TRUE), numeric(1))
      data.frame(
        wave = t,
        n_obs = sum(mask_col, na.rm = TRUE),
        prop_censored = mean(!mask_col, na.rm = TRUE),
        ess_pos = ess_pos,
        ess_pos_frac = ess_pos_frac,
        ess_pos_frac_pt = ess_pos_frac_pt,
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
    wave_table$wave_label <- vapply(wave_table$wave, map_wave_label, character(1))
    col_order <- c("wave", "wave_label", setdiff(names(wave_table), c("wave", "wave_label")))
    wave_table <- wave_table[, col_order]

    list(
      wave_table = wave_table,
      mask_from_fit = mask,
      density_ratios = r_raw,
      w_cum_raw = w_raw,
      w_cum_trim = w_trim,
      shift_full = shift_full,
      shift_clean = shift_clean
    )
  }

  selected <- lapply(seq_len(nrow(shift_df)), function(i) {
    shift_full <- shift_df$shift_full[[i]]
    list(
      shift_full = shift_full,
      shift_clean = shift_df$shift_clean[[i]],
      model = shift_models[[shift_full]]
    )
  })

  diagnostics <- lapply(selected, function(info) {
    compute_diag(info$model, info$shift_full, info$shift_clean)
  })

  if (length(diagnostics) == 1L) {
    return(diagnostics[[1]])
  }

  names_vec <- vapply(seq_along(selected), function(i) {
    nm <- selected[[i]]$shift_clean
    if (is.null(nm) || !nzchar(nm)) nm <- selected[[i]]$shift_full
    if (is.null(nm) || !nzchar(nm)) nm <- paste0("shift_", i)
    nm
  }, character(1))
  names(diagnostics) <- make.unique(names_vec)
  diagnostics
}
