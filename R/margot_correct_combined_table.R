#' Correct a "combined table" for multiplicity **and** recompute *E*-values
#'
#' @description
#' `margot_correct_combined_table()` takes the **combined_table** produced by the
#' various *margot* models (or by your own code) and
#' \enumerate{
#'   \item applies the chosen confidence-interval adjustment, **and**
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
#' @param adjust Multiplicity method: `"bonferroni"` (default), `"holm"`, `"BH"`, or `"none"`. Bonferroni and Holm provide strong FWER control; BH provides FDR control. `"none"` retains the supplied confidence limits.
#' @param alpha  Family-wise error-rate (for bonferroni/holm) or false discovery
#'   rate (for BH) to control. Default `0.05`.
#' @param m Positive whole number giving the total number of tests in the Bonferroni family. It must be at least the number of table rows. When `NULL`, Margot uses the number of rows. Holm and BH continue to use the rows supplied in `combined_table` as their adjustment family.
#' @param scale Scale used to recompute the *E*-value. `"RD"` is the legacy label for the standardised-continuous-outcome approximation from an outcome-mean difference or ATE; `"RR"` treats the estimate as a risk ratio.
#' @param delta Exposure contrast represented by an outcome-mean difference, used only when `scale = "RD"`.
#' @param sd Outcome standard deviation used to standardise an outcome-mean difference, used only when `scale = "RD"`.
#'
#' @return A data frame with the same rows (and order) as `combined_table`, but
#'   with
#'   \itemize{
#'     \item updated `2.5 %` and `97.5 %` columns, and
#'     \item freshly computed `E_Value` and `E_Val_bound`.
#'   }
#'   Numeric columns retain their computational precision. Apply display
#'   rounding only when formatting the returned table for presentation.
#'
#' @section E-value calculation:
#' For a risk ratio \eqn{r}, let \eqn{r^* = r} when \eqn{r \ge 1} and
#' \eqn{r^* = 1/r} otherwise. Margot computes the null E-value as
#' \deqn{r^* + \sqrt{r^*(r^*-1)}.}
#' The confidence-bound E-value uses the confidence limit closest to 1 and
#' equals 1 when the interval includes 1.
#'
#' For an outcome-mean difference \eqn{b}, exposure contrast \eqn{\delta}, and
#' outcome standard deviation \eqn{s}, Margot first forms the standardised
#' difference \eqn{d = b|\delta|/s}. It approximates the risk ratio as
#' \eqn{\exp(0.91d)} and the risk-ratio confidence limits as
#' \eqn{\exp(0.91d \pm 1.78\,\mathrm{SE}(d))}, then applies the same null
#' E-value equation. This calculation treats \eqn{s} as known.
#'
#' @section How the correction is applied:
#' For Bonferroni, let \eqn{m} be the total number of tests in the multiplicity family.
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
#' VanderWeele TJ, Ding P (2017). Sensitivity analysis in observational research: introducing the E-value. *Annals of Internal Medicine* 167(4): 268–274. \doi{10.7326/M16-2607}.
#'
#' Chinn S (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. *Statistics in Medicine* 19(22): 3127–3131.
#'
#' VanderWeele TJ (2017). On a square-root transformation of the odds ratio for a common outcome. *Epidemiology* 28(6): e58.
#'
#' @importFrom stats qnorm pnorm p.adjust
#' @importFrom dplyr mutate across any_of bind_cols
#' @importFrom purrr pmap_dfr
margot_correct_combined_table <- function(combined_table,
                                          adjust = c("bonferroni", "holm", "BH", "none"),
                                          alpha = 0.05,
                                          scale = c("RD", "RR"),
                                          delta = 1,
                                          sd = 1,
                                          m = NULL) {
  adjust <- match.arg(adjust)
  scale <- match.arg(scale)

  ## ---- 0 • sanity checks ----------------------------------------------------
  if (scale == "RD") {
    rd_cols <- c("ATE", "ATT", "ATC", "ATO", "E[Y(1)]-E[Y(0)]")
    est_col <- rd_cols[rd_cols %in% names(combined_table)][1]
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(combined_table)) {
    est_col <- "E[Y(1)]/E[Y(0)]"
  } else {
    est_col <- NULL
  }

  if (is.null(est_col) || is.na(est_col)) {
    stop("Couldn't find a point-estimate column in `combined_table`.")
  }

  if (!all(c("2.5 %", "97.5 %") %in% names(combined_table))) {
    stop("`combined_table` must contain '2.5 %' and '97.5 %' columns.")
  }

  n_tests <- nrow(combined_table)
  multiplicity <- .margot_resolve_multiplicity(m, n_tests)
  m <- multiplicity$realised
  z_orig <- stats::qnorm(0.975) # 1.96

  tbl <- combined_table
  confidence_level <- rep(1 - alpha, n_tests)

  # Keep original numeric columns as provided; avoid coercion to prevent
  # introducing accidental NAs in downstream interpretation/tables.

  ## ---- 1  adjust the CI ----------------------------------------------------
  if (adjust == "bonferroni") {
    z_star <- stats::qnorm(1 - alpha / (2 * m))
    confidence_level <- rep(1 - (alpha / m), n_tests)

    if (scale == "RR") {
      # Adjust on the log scale, then exponentiate back to preserve positivity
      eps <- .Machine$double.eps
      est_rr   <- pmax(tbl[[est_col]], eps)
      lo_rr    <- pmax(tbl$`2.5 %`, eps)
      hi_rr    <- pmax(tbl$`97.5 %`, eps)

      est_log  <- log(est_rr)
      hi_log   <- log(hi_rr)
      # infer log-SE from original 97.5% bound
      se_log   <- (hi_log - est_log) / z_orig
      new_lo   <- exp(est_log - z_star * se_log)
      new_hi   <- exp(est_log + z_star * se_log)

      tbl$`2.5 %`  <- new_lo
      tbl$`97.5 %` <- new_hi
    } else {
      # RD: rescale original half-width (symmetric Wald on difference scale)
      half_w <- (tbl$`97.5 %` - tbl$`2.5 %`) / 2
      tbl <- tbl |>
        dplyr::mutate(
          `2.5 %`  = !!rlang::sym(est_col) - (half_w * z_star / z_orig),
          `97.5 %` = !!rlang::sym(est_col) + (half_w * z_star / z_orig)
        )
    }
  } else if (adjust == "holm") { # -------- Holm ------------------------------

    ## back-calculate SE from the *original* CI
    se <- (tbl$`97.5 %` - tbl[[est_col]]) / z_orig
    z <- tbl[[est_col]] / se
    p <- 2 * (1 - stats::pnorm(abs(z))) # two-sided
    p_adj <- stats::p.adjust(p, method = "holm")
    confidence_level <- 1 - p_adj

    z_star <- stats::qnorm(1 - p_adj / 2)

    tbl <- tbl |>
      dplyr::mutate(
        `2.5 %`  = !!rlang::sym(est_col) - z_star * se,
        `97.5 %` = !!rlang::sym(est_col) + z_star * se
      )
  } else if (adjust == "BH") { # -------- BH (Benjamini-Hochberg) -----------

    ## back-calculate SE from the *original* CI
    se <- (tbl$`97.5 %` - tbl[[est_col]]) / z_orig
    z <- tbl[[est_col]] / se
    p <- 2 * (1 - stats::pnorm(abs(z))) # two-sided
    p_adj <- stats::p.adjust(p, method = "BH") # benjamini-hochberg FDR
    confidence_level <- 1 - p_adj

    z_star <- stats::qnorm(1 - p_adj / 2)

    tbl <- tbl |>
      dplyr::mutate(
        `2.5 %`  = !!rlang::sym(est_col) - z_star * se,
        `97.5 %` = !!rlang::sym(est_col) + z_star * se
      )
  } else {
    # Retain the supplied confidence limits when no adjustment is requested.
  }

  ## ---- 2  recompute E-values ----------------------------------------------
  new_EV <- purrr::pmap_dfr(
    list(
      est = tbl[[est_col]],
      lo = tbl$`2.5 %`,
      hi = tbl$`97.5 %`,
      se0 = (tbl$`97.5 %` - tbl[[est_col]]) / stats::qnorm(0.975)
    ),
    \(est, lo, hi, se0) {
      values <- if (scale == "RD") {
        .margot_evalues_ols(est, se = se0, sd = sd, delta = delta)
      } else {
        .margot_evalues_rr(est, lo = lo, hi = hi)
      }
      tibble::as_tibble_row(values)
    }
  )

  ## ---- 3 bind exact numeric results ---------------------------------------
  out <- tbl |>
    dplyr::select(-dplyr::any_of(c("E_Value", "E_Val_bound", "confidence_level"))) |>
    dplyr::bind_cols(new_EV) |>
    dplyr::mutate(confidence_level = confidence_level)

  attr(out, "confidence_level") <- confidence_level
  attr(out, "multiplicity") <- multiplicity
  out
}
