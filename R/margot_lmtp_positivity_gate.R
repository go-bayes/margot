#' Registered trim-ladder positivity gate for LMTP fits
#'
#' Applies a pre-registered positivity decision rule to one or more LMTP
#' fits across a descending ladder of trim rungs (default 0.99, 0.98,
#' 0.96), returning a mechanical pass/fail per policy and rung plus the
#' selected rung. The gate exists so that trim selection can be cited in a
#' registration and reproduced from code rather than exercised as analyst
#' judgement.
#'
#' Three criteria must hold at a rung for a policy to pass:
#' \enumerate{
#'   \item \strong{Effective sample size (variance side).} The Kish ESS of
#'     the rung-winsorised cumulative weights (`ess_cum_trim` from
#'     [margot_lmtp_weight_diag_from_fit()]) must be at least `ess_floor`
#'     of the uncensored observations at every wave. Winsorising harder
#'     homogenises weights, so this criterion can be rescued by descending
#'     the ladder.
#'   \item \strong{Trimmed mass (bias side).} Winsorising at the rung must
#'     remove no more than `trim_mass_budget` of total cumulative ratio
#'     mass at the final wave (`trim_mass_share`). Capping harder always
#'     removes more mass, so descending the ladder can only worsen this
#'     criterion: it is the pre-registered budget on how much distortion a
#'     study will buy for stability.
#'   \item \strong{Product support band.} The fraction of uncensored
#'     cumulative ratio products outside the support band (default
#'     \eqn{[0.1, 10]}) must not reach "Limited" status under the shared
#'     support-status rule (`Adequate` at 5 percent outside or less,
#'     `Caution` to 20 percent, `Limited` beyond). This criterion is
#'     computed on the raw ratios and does not vary with the rung.
#' }
#'
#' The two rung-dependent criteria move in opposite directions, which
#' gives the ladder its logic: descend while the ESS criterion fails and
#' the mass budget still holds; once the budget is breached (or the rungs
#' are exhausted), the policy fails the gate and the registered
#' contingency applies.
#'
#' @param fit A `margot_lmtp()` result, a single LMTP model exposing
#'   `$density_ratios`, or a list of such models (the same inputs as
#'   [margot_lmtp_weight_diag_from_fit()]).
#' @param outcome Optional outcome name when `fit` is a full run.
#' @param shifts Optional character vector of shifts to gate; defaults to
#'   all shifts stored for the outcome.
#' @param rungs Numeric vector of trim rungs, evaluated in order; the
#'   first passing rung is selected (default `c(0.99, 0.98, 0.96)`).
#' @param ess_floor Minimum acceptable ESS fraction of uncensored
#'   observations at every wave (default 0.5, matching
#'   [margot_lmtp_positivity()]'s `ess_warn`).
#' @param trim_mass_budget Maximum acceptable share of total cumulative
#'   ratio mass removed by winsorising at the rung (default 0.05).
#' @param test_thresholds Optional list of product-band thresholds
#'   (defaults: `prod_log10 = -1`, `prod_frac_ok = 0.05`,
#'   `prod_frac_warn = 0.20`), matching `margot_ipsi_summary()`.
#' @param label_mapping Optional label mapping forwarded to
#'   [margot_lmtp_weight_diag_from_fit()].
#' @param verbose If TRUE, prints one verdict line per policy.
#'
#' @return A list with:
#'   \item{by_rung}{data.frame with one row per outcome/shift/rung:
#'     `min_ess_frac`, `trim_mass_share`, `prod_frac_outside`,
#'     `support_status`, the three criterion flags, and `pass`.}
#'   \item{selection}{data.frame with one row per outcome/shift:
#'     `selected_rung` (NA when no rung passes) and a `verdict` string.}
#'   \item{criteria}{list recording the thresholds the gate applied, for
#'     citation in registration documents.}
#'
#' @examples
#' set.seed(2026)
#' tame <- matrix(stats::runif(300, 0.8, 1.25), ncol = 3)
#' fit <- list(density_ratios = tame)
#' gate <- margot_lmtp_positivity_gate(fit, verbose = FALSE)
#' gate$selection
#'
#' @seealso [margot_lmtp_weight_diag_from_fit()], [margot_lmtp_positivity()]
#' @export
margot_lmtp_positivity_gate <- function(fit,
                                        outcome = NULL,
                                        shifts = NULL,
                                        rungs = c(0.99, 0.98, 0.96),
                                        ess_floor = 0.5,
                                        trim_mass_budget = 0.05,
                                        test_thresholds = NULL,
                                        label_mapping = NULL,
                                        verbose = TRUE) {
  stopifnot(is.numeric(rungs), length(rungs) >= 1L, all(rungs > 0), all(rungs < 1))
  stopifnot(is.numeric(ess_floor), ess_floor > 0, ess_floor <= 1)
  stopifnot(is.numeric(trim_mass_budget), trim_mass_budget >= 0, trim_mass_budget <= 1)

  selection <- margot_resolve_positivity_selection(
    x = fit,
    outcome = outcome,
    shifts = shifts,
    caller = "margot_lmtp_positivity_gate"
  )
  outcome_name <- selection$outcome
  shift_models <- selection$outcome_models
  shift_df <- selection$shift_df
  if (!nrow(shift_df)) {
    stop("No shift models stored for outcome ", outcome_name)
  }

  thresholds <- margot_positivity_thresholds(test_thresholds)

  # ratios matrix from a stored model, tolerant of lmtp objects and environments
  extract_ratios <- function(mod) {
    if (is.environment(mod)) mod <- as.list.environment(mod)
    if (inherits(mod, "lmtp")) mod <- as.list(mod)
    dr <- mod$density_ratios
    if (is.null(dr)) stop("Selected model does not expose $density_ratios.")
    dr <- as.matrix(dr)
    storage.mode(dr) <- "double"
    dr
  }

  by_rung_rows <- list()
  selection_rows <- list()

  for (i in seq_len(nrow(shift_df))) {
    shift_full <- shift_df$shift_full[[i]]
    shift_clean <- shift_df$shift_clean[[i]]
    model <- shift_models[[shift_full]]
    dr <- extract_ratios(model)

    # rung-independent product-band criterion on the raw ratios
    product_metrics <- margot_positivity_product_metrics(
      dr, cols = seq_len(ncol(dr)), thresholds = thresholds
    )
    support_status <- margot_positivity_support_status(
      product_metrics$prod_frac_outside, thresholds
    )
    band_ok <- !identical(support_status, "Limited")

    selected_rung <- NA_real_
    for (rung in rungs) {
      diag <- margot_lmtp_weight_diag_from_fit(
        model,
        trim_right = rung,
        label_mapping = label_mapping
      )
      wave_table <- diag$wave_table

      ess_frac <- ifelse(wave_table$n_obs > 0,
                         wave_table$ess_cum_trim / wave_table$n_obs,
                         NA_real_)
      min_ess_frac <- if (all(is.na(ess_frac))) NA_real_ else min(ess_frac, na.rm = TRUE)
      ess_ok <- is.finite(min_ess_frac) && min_ess_frac >= ess_floor

      # final-wave share of cumulative ratio mass removed by the cap
      tau <- ncol(diag$w_cum_raw)
      raw_mass <- sum(diag$w_cum_raw[, tau], na.rm = TRUE)
      trim_mass <- sum(diag$w_cum_trim[, tau], na.rm = TRUE)
      trim_mass_share <- if (raw_mass > 0) 1 - trim_mass / raw_mass else NA_real_
      mass_ok <- is.finite(trim_mass_share) && trim_mass_share <= trim_mass_budget

      pass <- ess_ok && mass_ok && band_ok
      by_rung_rows[[length(by_rung_rows) + 1L]] <- data.frame(
        outcome = outcome_name,
        shift = shift_clean,
        rung = rung,
        min_ess_frac = min_ess_frac,
        trim_mass_share = trim_mass_share,
        prod_frac_outside = product_metrics$prod_frac_outside,
        support_status = support_status,
        ess_ok = ess_ok,
        mass_ok = mass_ok,
        band_ok = band_ok,
        pass = pass,
        stringsAsFactors = FALSE
      )
      if (pass && is.na(selected_rung)) {
        selected_rung <- rung
      }
    }

    verdict <- if (is.na(selected_rung)) {
      "positivity gate failed at all rungs: apply the registered contingency"
    } else {
      sprintf("estimate with lmtp_control(.trim = %.2f)", selected_rung)
    }
    selection_rows[[length(selection_rows) + 1L]] <- data.frame(
      outcome = outcome_name,
      shift = shift_clean,
      selected_rung = selected_rung,
      support_status = support_status,
      verdict = verdict,
      stringsAsFactors = FALSE
    )
    if (isTRUE(verbose)) {
      cli::cli_inform("{outcome_name} / {shift_clean}: {verdict}")
    }
  }

  list(
    by_rung = do.call(rbind, by_rung_rows),
    selection = do.call(rbind, selection_rows),
    criteria = list(
      rungs = rungs,
      ess_floor = ess_floor,
      trim_mass_budget = trim_mass_budget,
      band = margot_positivity_band_strings(thresholds)$interval_label,
      band_fail_status = "Limited"
    )
  )
}
