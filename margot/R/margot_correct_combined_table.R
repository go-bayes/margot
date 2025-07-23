#' @keywords internal
process_evalue <- function(tab, scale, delta, sd) {

  ev <- if (scale == "RD") {
    EValue::evalues.OLS(tab$`E[Y(1)]-E[Y(0)]`,
                        se    = tab$standard_error,
                        sd    = sd,
                        delta = delta,
                        true  = 0)
  } else {
    EValue::evalues.RR(tab$`E[Y(1)]/E[Y(0)]`,
                       lo   = tab$`2.5 %`,
                       hi   = tab$`97.5 %`,
                       true = 1)
  }

  ev_df <- as.data.frame(ev)[2, c("point","lower","upper"), drop = FALSE]

  tibble::tibble(
    E_Value     = ev_df$point,
    E_Val_bound = dplyr::coalesce(ev_df$lower, ev_df$upper, 1)
  )
}



#' Correct a "combined table" for multiplicity **and** recompute *E*-values
#'
#' @description
#' `margot_correct_combined_table()` takes the **combined_table** produced by the
#' various *margot* models (or by your own code) and
#' \enumerate{
#'   \item widens the confidence interval according to the chosen
#'         family–wise-error correction, **and**
#'   \item recalculates *E*-values (and their lower bounds) so they match the
#'         new interval.
#' }
#' By default it implements the single–step **Bonferroni** correction at
#' \eqn{\alpha = 0.05} as advocated by VanderWeele & Mathur (2019).
#'
#' @param combined_table A data frame with *at least* the columns
#'   \itemize{
#'     \item `E[Y(1)]-E[Y(0)]` **or** `E[Y(1)]/E[Y(0)]`
#'     \item `2.5 %`, `97.5 %`   (unadjusted CI limits)
#'   }
#'   Extra columns (e.g. the original *E*-values) are carried through.
#' @param adjust Multiplicity method: `"bonferroni"` (default), `"holm"`, or `"BH"`.
#'   Bonferroni and Holm provide strong FWER control; BH provides FDR control.
#' @param alpha  Family-wise error-rate (for bonferroni/holm) or false discovery
#'   rate (for BH) to control. Default `0.05`.
#' @param scale  Scale to use when recomputing the *E*-value.
#'   `"RD"` (risk difference / ATE, **default**) or `"RR"` (risk ratio).
#' @param delta,sd Arguments passed to [EValue::evalues.OLS()] when
#'   `scale = "RD"`.  Ignored for `"RR"`.
#'
#' @return A data frame with the same rows (and order) as `combined_table`, but
#'   with
#'   \itemize{
#'     \item updated `2.5 %` and `97.5 %` columns, and
#'     \item freshly computed `E_Value` and `E_Val_bound`.
#'   }
#'
#' @section How the correction is applied:
#' Let \eqn{m} be the number of rows (tests).
#' \itemize{
#'   \item **Bonferroni** uses
#'     \deqn{ z^* = \Phi^{-1}\!\bigl(1-\alpha/(2m)\bigr) }
#'     and rescales the original half-width.
#'   \item **Holm** first step-down adjusts the (two-sided) *p*-value for each
#'     test, then back-calculates a *symmetric* CI whose coverage matches the
#'     adjusted *p*.  Point estimates **never** change.
#'   \item **BH** (Benjamini-Hochberg) applies FDR correction to *p*-values,
#'     then back-calculates symmetric CIs. Controls false discovery rate rather
#'     than family-wise error rate.
#' }
#'
#' @references
#' VanderWeele TJ, Mathur MB (2019).
#' *Some desirable properties of the Bonferroni correction:
#' Is the Bonferroni correction really so bad?*
#' **Am J Epidemiol** 188(3): 617–618.
#'
#' @importFrom stats qnorm pnorm p.adjust
#' @importFrom dplyr mutate across any_of bind_cols
#' @importFrom purrr pmap_dfr
#' @importFrom EValue evalues.OLS evalues.RR
margot_correct_combined_table <- function(combined_table,
                                          adjust = c("bonferroni", "holm", "BH"),
                                          alpha  = 0.05,
                                          scale  = c("RD", "RR"),
                                          delta  = 1,
                                          sd     = 1) {

  adjust <- match.arg(adjust)
  scale  <- match.arg(scale)

  ## ---- 0 • sanity checks ----------------------------------------------------
  if      ("E[Y(1)]-E[Y(0)]" %in% names(combined_table)) {
    est_col <- "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(combined_table)) {
    est_col <- "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Couldn't find a point-estimate column in `combined_table`.")
  }

  if (!all(c("2.5 %", "97.5 %") %in% names(combined_table)))
    stop("`combined_table` must contain '2.5 %' and '97.5 %' columns.")

  m      <- nrow(combined_table)          # number of tests
  z_orig <- stats::qnorm(0.975)           # 1.96

  tbl <- combined_table

  ## ---- 1  adjust the CI ----------------------------------------------------
  if (adjust == "bonferroni") {

    z_star <- stats::qnorm(1 - alpha / (2 * m))

    # original half-width so we don't need the raw SE
    half_w <- (tbl$`97.5 %` - tbl$`2.5 %`) / 2

    tbl <- tbl |>
      dplyr::mutate(
        `2.5 %`  = !!rlang::sym(est_col) - (half_w * z_star / z_orig),
        `97.5 %` = !!rlang::sym(est_col) + (half_w * z_star / z_orig)
      )

  } else if (adjust == "holm") {   # -------- Holm ------------------------------

    ## back-calculate SE from the *original* CI
    se <- (tbl$`97.5 %` - tbl[[est_col]]) / z_orig
    z  <- tbl[[est_col]] / se
    p  <- 2 * (1 - stats::pnorm(abs(z)))           # two-sided
    p_adj <- stats::p.adjust(p, method = "holm")

    z_star <- stats::qnorm(1 - p_adj / 2)

    tbl <- tbl |>
      dplyr::mutate(
        `2.5 %`  = !!rlang::sym(est_col) - z_star * se,
        `97.5 %` = !!rlang::sym(est_col) + z_star * se
      )

  } else {                         # -------- BH (Benjamini-Hochberg) -----------

    ## back-calculate SE from the *original* CI
    se <- (tbl$`97.5 %` - tbl[[est_col]]) / z_orig
    z  <- tbl[[est_col]] / se
    p  <- 2 * (1 - stats::pnorm(abs(z)))           # two-sided
    p_adj <- stats::p.adjust(p, method = "BH")     # benjamini-hochberg FDR

    z_star <- stats::qnorm(1 - p_adj / 2)

    tbl <- tbl |>
      dplyr::mutate(
        `2.5 %`  = !!rlang::sym(est_col) - z_star * se,
        `97.5 %` = !!rlang::sym(est_col) + z_star * se
      )
  }

  ## ---- 2  recompute E-values ----------------------------------------------
  new_EV <- purrr::pmap_dfr(
    list(est = tbl[[est_col]],
         lo  = tbl$`2.5 %`,
         hi  = tbl$`97.5 %`,
         se0 = (tbl$`97.5 %` - tbl[[est_col]]) / stats::qnorm(0.975)),
    \(est, lo, hi, se0) {
      tmp <- tibble::tibble(
        `E[Y(1)]-E[Y(0)]` = est,
        `E[Y(1)]/E[Y(0)]` = NA_real_,   # ignored for RD
        `2.5 %`           = lo,
        `97.5 %`          = hi,
        standard_error    = se0
      )
      process_evalue(tmp, scale, delta, sd)
    }
  )

  ## ---- 3 bind & round -----------------------------------------------------
  tbl |>
    dplyr::select(-dplyr::any_of(c("E_Value", "E_Val_bound"))) |>
    dplyr::bind_cols(new_EV) |>
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
}
