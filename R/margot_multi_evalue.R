#' Multi-bias E-value table (v1: unmeasured confounding)
#'
#' Create a table-first sensitivity summary that reports baseline E-values
#' (after optional Bonferroni CI adjustment) and, when specified, computes
#' multi-bias E-values using the EValue package (confounding, selection,
#' misclassification). For RD/continuous outcomes, estimates are mapped to the
#' RR scale via the OLS-to-RR approximation (delta/sd) before multi-bias
#' evaluation.
#'
#' @param results A data frame containing at least one of `E[Y(1)]-E[Y(0)]` (RD)
#'   or `E[Y(1)]/E[Y(0)]` (RR), plus columns `2.5 %` and `97.5 %`. Typically a
#'   combined table row or small set of rows from a margot workflow.
#' @param scale Character. "RD" (difference) or "RR" (ratio). If omitted, the
#'   function attempts to infer from the presence of `E[Y(1)]-E[Y(0)]` vs
#'   `E[Y(1)]/E[Y(0)]`.
#' @param intervention_type Character. One of "exposure_shift" or "ipsi".
#'   Used only for interpretation notes.
#' @param delta_exposure Numeric. Exposure contrast size for OLS-type E-values
#'   (RD). Defaults to 1. For IPSI, interpret as one policy contrast (α1 vs α0).
#' @param sd_outcome Numeric. Outcome standard deviation used for OLS-type
#'   E-values (RD). Defaults to 1 (standardized outcomes).
#' @param biases Either an EValue bias object created with
#'   `EValue::multi_bias()` (preferred), a single EValue `bias` object (e.g.,
#'   `EValue::confounding()`), or a character vector of bias names to build
#'   with sensible defaults: accepted tokens are `"confounding"`,
#'   `"selection"`, `"misclassification_outcome"`, and
#'   `"misclassification_exposure"`.
#' @param apply_bonferroni_first Logical; if TRUE (default), widen CIs using a
#'   Bonferroni correction at FWER `alpha` before computing E-values.
#' @param alpha Numeric FWER level for Bonferroni. Default 0.05.
#' @param m Optional integer for multiplicity (number of tests). If NULL,
#'   inferred from `nrow(results)`.
#' @param notes Logical; if TRUE include a LaTeX-ready interpretation note in
#'   the output list.
#'
#' @return A list with elements:
#'   - `table`: data frame with adjusted CIs (if requested), baseline E-values
#'     (`E_Value`, `E_Val_bound`), and audit columns `alpha_fwer`, `m`,
#'     `scale`, `intervention_type`, `delta_exposure`, `sd_outcome`, and
#'     `bias_order`. For convenience, mirrored columns `E_value_point` and
#'     `E_value_bound` are also included.
#'   - `notes`: character vector (length 1) with an interpretation message when
#'     `notes = TRUE`.
#'
#' @export
#' @importFrom stats qnorm
margot_multi_evalue <- function(
  results,
  scale = c("RD", "RR"),
  intervention_type = c("exposure_shift", "ipsi"),
  delta_exposure = 1,
  sd_outcome = 1,
  biases = NULL,
  apply_bonferroni_first = TRUE,
  alpha = 0.05,
  m = NULL,
  notes = TRUE,
  include_baseline = TRUE,
  rename_multi = c("none", "friendly"),
  bound_params = NULL
) {
  # ---- input coercion ------------------------------------------------------
  stopifnot(is.data.frame(results) || inherits(results, "tbl"))
  intervention_type <- match.arg(intervention_type)
  rename_multi <- match.arg(rename_multi)

  # infer scale if not explicitly set
  has_rd <- "E[Y(1)]-E[Y(0)]" %in% names(results)
  has_rr <- "E[Y(1)]/E[Y(0)]" %in% names(results)
  if (missing(scale)) {
    scale <- if (has_rr && !has_rd) "RR" else "RD"
  } else {
    scale <- match.arg(scale)
  }

  # basic column sanity
  needed <- c("2.5 %", "97.5 %")
  if (!any(has_rd, has_rr) || !all(needed %in% names(results))) {
    stop("`results` must include either 'E[Y(1)]-E[Y(0)]' (RD) or 'E[Y(1)]/E[Y(0)]' (RR) and '2.5 %', '97.5 %' columns.")
  }

  # multiplicity m
  if (is.null(m)) m <- nrow(results)

  # ---- CI adjustment + baseline E-values -----------------------------------
  adj_method <- if (isTRUE(apply_bonferroni_first)) "bonferroni" else "none"
  out <- margot_correct_combined_table(
    combined_table = results,
    adjust = adj_method,
    alpha = alpha,
    scale = scale,
    delta = delta_exposure,
    sd = sd_outcome
  )

  # audit columns
  out$alpha_fwer <- if (adj_method == "bonferroni") alpha else NA_real_
  out$m <- m
  out$scale <- scale
  out$intervention_type <- intervention_type
  out$delta_exposure <- delta_exposure
  out$sd_outcome <- sd_outcome

  # mirror names to a consistent lower-case style for convenience
  out$E_value_point <- out$E_Value
  out$E_value_bound <- out$E_Val_bound

  # ---- multi-bias integration via EValue (optional) ------------------------
  # Accept either: an EValue "bias"/"multi_bias" object, or a character vector
  # of bias names we can map to EValue constructors with defaults.
  bias_order <- NA_character_
  if (!is.null(biases)) {
    mbias <- NULL
    # if user supplied an EValue bias object, use it
    if (inherits(biases, c("bias", "multi_bias"))) {
      mbias <- biases
      # generate an order string from names if possible
      if (inherits(biases, "multi_bias")) {
        suppressWarnings({
          # attempt to extract bias names from attr parameters table
          params <- attr(biases, "parameters")
          if (is.data.frame(params) && "bias" %in% names(params)) {
            bias_order <- paste(unique(params$bias), collapse = " -> ")
          } else {
            bias_order <- "multi_bias"
          }
        })
      } else {
        bias_order <- names(biases)
      }
    } else if (is.character(biases)) {
      # build a basic multi_bias object from recognized names
      # supported tokens: "confounding", "selection", "misclassification_outcome",
      # "misclassification_exposure"
      parts <- unique(biases)
      built <- list()
      for (p in parts) {
        if (identical(p, "confounding")) built <- append(built, list(EValue::confounding()))
        else if (identical(p, "selection")) built <- append(built, list(EValue::selection("general")))
        else if (identical(p, "misclassification_outcome")) built <- append(built, list(EValue::misclassification("outcome")))
        else if (identical(p, "misclassification_exposure")) built <- append(built, list(EValue::misclassification("exposure", rare_outcome = TRUE, rare_exposure = TRUE)))
        else warning("Ignoring unsupported bias label: ", p)
      }
      if (length(built) > 0) {
        mbias <- do.call(EValue::multi_bias, built)
        bias_order <- paste(parts, collapse = " -> ")
      }
    }

    # compute multi-bias E-values if we have a bias object and the measure
    # is supported by EValue::multi_evalues.* (RR/OR/HR). For RD, map to RR.
    if (!is.null(mbias)) {
      # helper: coalesce bound from lower/upper to the side away from the null
      coalesce_bound <- function(mat) {
        df <- as.data.frame(mat)
        # Expect row 2 is "Multi-bias E-values" with columns point/lower/upper
        if (nrow(df) >= 2 && all(c("point", "lower", "upper") %in% names(df))) {
          list(point = df$point[2], bound = dplyr::coalesce(df$lower[2], df$upper[2], 1))
        } else {
          list(point = NA_real_, bound = NA_real_)
        }
      }

      if (scale == "RR") {
        rr_point <- out$`E[Y(1)]/E[Y(0)]`
        rr_lo    <- out$`2.5 %`
        rr_hi    <- out$`97.5 %`
        mb <- EValue::multi_evalues.RR(mbias, est = rr_point, lo = rr_lo, hi = rr_hi, true = 1)
        cb <- coalesce_bound(mb)
        out$multi_E_value_point <- cb$point
        out$multi_E_value_bound <- cb$bound
      } else {
        # RD: transform to RR using OLS -> MD -> RR mapping
        rd_point <- out$`E[Y(1)]-E[Y(0)]`
        rd_lo    <- out$`2.5 %`
        rd_hi    <- out$`97.5 %`
        # wrap as OLS estimates with sd attribute, then map to RR with delta
        rr_point <- as.numeric(EValue::toRR.OLS(EValue::OLS(rd_point, sd = sd_outcome), delta = delta_exposure))
        rr_lo    <- as.numeric(EValue::toRR.OLS(EValue::OLS(rd_lo, sd = sd_outcome),    delta = delta_exposure))
        rr_hi    <- as.numeric(EValue::toRR.OLS(EValue::OLS(rd_hi, sd = sd_outcome),    delta = delta_exposure))
        mb <- EValue::multi_evalues.RR(mbias, est = rr_point, lo = rr_lo, hi = rr_hi, true = 1)
        cb <- coalesce_bound(mb)
        out$multi_E_value_point <- cb$point
        out$multi_E_value_bound <- cb$bound
      }

      # optional multi-bias bound (requires user-supplied parameters)
      if (!is.null(bound_params) && (scale == "RR" || scale == "RD")) {
        rr_point_for_bound <- if (scale == "RR") rr_point else rr_point
        # compute bound multiplier using EValue::multi_bound with user params
        # bound arguments are taken from bound_params list
        try({
          bmul <- do.call(EValue::multi_bound, c(list(biases = mbias), bound_params))
          out$MB_Bound <- as.numeric(bmul)
          out$RR_min_true <- as.numeric(rr_point_for_bound / bmul)
        }, silent = TRUE)
      }
    }
  }
  out$bias_order <- bias_order

  # include or drop baseline single-bias E-values
  if (!isTRUE(include_baseline)) {
    out$E_value_point <- NULL
    out$E_value_bound <- NULL
    out$E_Value <- NULL
    out$E_Val_bound <- NULL
  }

  # user-friendly column names for multi-bias outputs
  if (rename_multi == "friendly") {
    if ("multi_E_value_point" %in% names(out)) names(out)[names(out) == "multi_E_value_point"] <- "Multi E-Value"
    if ("multi_E_value_bound" %in% names(out)) names(out)[names(out) == "multi_E_value_bound"] <- "Multi E-Value bound"
  }

  # ---- interpretation note --------------------------------------------------
  note_msg <- NULL
  if (isTRUE(notes)) {
    if (identical(intervention_type, "ipsi")) {
      note_msg <- paste0(
        "Interpretation is per the stated policy contrast; estimates reflect $\\mathbf{E}[Y(1)] - \\mathbf{E}[Y(0)]$ on standardized outcome units. ",
        "For IPSI (risk-scale) interventions, the E-value pertains to the contrast between policies ($\\alpha_1$ vs $\\alpha_0$)."
      )
    } else {
      note_msg <- paste0(
        "Interpretation is per the stated policy contrast; estimates reflect $\\mathbf{E}[Y(1)] - \\mathbf{E}[Y(0)]$ on standardized outcome units. ",
        "For continuous outcomes, E-values use an OLS-to-RR approximation with exposure contrast $\\delta = ",
        delta_exposure, "$ and outcome SD = ", sd_outcome, "."
      )
    }
  }

  res <- list(table = out, notes = note_msg)
  class(res) <- c("margot_multi_evalue", class(res))
  res
}
