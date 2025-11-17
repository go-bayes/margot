#' Assemble a full LMTP positivity report (table, diagnostics, text, plot, methods)
#'
#' Bundles the per-shift diagnostics, compact summary table, interpretive text,
#' overlap plot, and a ready-to-drop-in methods paragraph into a single object.
#' Designed to streamline Quarto sections where you want consistent reporting
#' across multiple shift functions.
#'
#' @param x Result of `margot_lmtp()` (with `$models`) or another object
#'   accepted by the underlying helpers.
#' @param outcome Character outcome name.
#' @param shifts Optional character vector of shifts to include (full or cleaned
#'   names). If `NULL`, all available shifts are used.
#' @param label_mapping Optional label map passed through to downstream helpers.
#' @param waves Optional integer vector selecting waves.
#' @param remove_waves Optional integer vector of waves to drop after subsetting.
#' @param test_thresholds Named list of thresholds passed to
#'   `margot_positivity_summary()` and `margot_interpret_lmtp_positivity()`.
#' @param include_policy_rates Logical; whether to compute policy-rate columns
#'   in the summary table and narrative.
#' @param effect_table Optional effect table merged into the summary.
#' @param digits Integer rounding applied to numeric outputs.
#' @param trim_right Numeric in `(0, 1]`; right-tail winsorisation level for
#'   diagnostics supplied to `margot_lmtp_weight_diag_from_fit()`.
#' @param thresholds Numeric vector of ratio thresholds forwarded to
#'   `margot_lmtp_weight_diag_from_fit()`.
#' @param summary_compact Logical; whether to request the compact summary table.
#' @param include_plot Logical; if `TRUE`, returns a `ggplot2`/patchwork object
#'   from `margot_plot_lmtp_overlap_grid()`.
#' @param plot_args Optional named list overriding defaults passed to
#'   `margot_plot_lmtp_overlap_grid()`.
#' @param interpret_args Optional named list overriding defaults passed to
#'   `margot_interpret_lmtp_positivity()` (e.g., `include_tests = FALSE`).
#'
#' @return A named list with elements:
#'   - `summary_table`: tibble/data.frame from `margot_positivity_summary()`.
#'   - `diagnostics`: list returned by `margot_lmtp_weight_diag_from_fit()`.
#'   - `overlap_plot`: plot object (or `NULL` when `include_plot = FALSE`).
#'   - `narrative`: structured list from `margot_interpret_lmtp_positivity(return = "list")`.
#'   - `method_statement`: single character string describing the analytic approach.
#'   - `metadata`: list of context (outcome, shifts, waves, thresholds).
#' @export
margot_positivity_report <- function(x,
                                     outcome,
                                     shifts = NULL,
                                     label_mapping = NULL,
                                     waves = NULL,
                                     remove_waves = NULL,
                                     test_thresholds = list(prod_log10 = -1,
                                                            prod_frac_warn = 0.10,
                                                            near_zero_median = 1e-3,
                                                            near_zero_cv = 0.05),
                                     include_policy_rates = TRUE,
                                     effect_table = NULL,
                                     digits = 3,
                                     trim_right = 0.999,
                                     thresholds = c(5, 10, 25, 50, 100),
                                     summary_compact = TRUE,
                                     include_plot = TRUE,
                                     plot_args = list(),
                                     interpret_args = list()) {
  stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))

  summary_tbl <- margot_positivity_summary(
    x = x,
    outcome = outcome,
    shifts = shifts,
    waves = waves,
    test_thresholds = test_thresholds,
    include_policy_rates = include_policy_rates,
    effect_table = effect_table,
    digits = digits,
    compact = summary_compact,
    include_explanation = FALSE
  )

  diagnostics <- margot_lmtp_weight_diag_from_fit(
    fit = x,
    outcome = outcome,
    shifts = shifts,
    trim_right = trim_right,
    thresholds = thresholds,
    label_mapping = label_mapping
  )

  overlap_plot <- NULL
  if (isTRUE(include_plot)) {
    plot_defaults <- list(
      x = x,
      outcome = outcome,
      shifts = shifts,
      label_mapping = label_mapping
    )
    plot_call <- utils::modifyList(plot_defaults, plot_args)
    overlap_plot <- do.call(margot_plot_lmtp_overlap_grid, plot_call)
  }

  interpret_defaults <- list(
    x = x,
    outcome = outcome,
    shifts = shifts,
    waves = waves,
    remove_waves = remove_waves,
    label_mapping = label_mapping,
    include_policy_rates = include_policy_rates,
    test_thresholds = test_thresholds,
    digits = digits,
    return = "list"
  )
  interpret_call <- utils::modifyList(interpret_defaults, interpret_args)
  narrative <- do.call(margot_interpret_lmtp_positivity, interpret_call)
  if (!is.list(narrative)) {
    narrative <- list(text = narrative)
  }

  method_statement <- build_positivity_method_statement(include_policy_rates)
  relationship_summary <- build_shift_relationship_summary(diagnostics, label_mapping)

  list(
    summary_table = summary_tbl,
    diagnostics = diagnostics,
    overlap_plot = overlap_plot,
    narrative = narrative,
    method_statement = method_statement,
    relationship_summary = relationship_summary,
    metadata = list(
      outcome = outcome,
      shifts = if (is.null(shifts)) names(diagnostics) else shifts,
      waves = waves,
      remove_waves = remove_waves,
      test_thresholds = test_thresholds,
      include_policy_rates = include_policy_rates,
      trim_right = trim_right,
      thresholds = thresholds
    )
  )
}

build_positivity_method_statement <- function(include_policy_rates = TRUE) {
  paragraphs <- c(
    "Density ratios act as weights in the LMTP estimator to rebalance the observed data so it mimics the intervention of interest; large or near-zero values indicate practical positivity strain.",
    "We assess positivity on uncensored rows by examining the distribution of ratios across waves and by monitoring the fraction of person-time with tiny products of ratios, which signal unstable estimators.",
    "Effective sample size (ESS) summarises how much information the weighted data retain: ESS = (sum w)^2 / sum w^2, so higher variance reduces precision; we report ESS relative to both uncensored rows and total person-time.",
    "Zeros in density ratios primarily reflect censoring, not treatment-positivity violations, because censoring removes follow-up treatment data; we therefore separate censoring rates from the uncensored ratio diagnostics.",
    "All estimands reweight to the baseline cohort via inverse probability of censoring, so even the null policy features non-trivial weights."
  )
  if (isTRUE(include_policy_rates)) {
    paragraphs <- c(
      "Policy rates report Pr(A_t = 1) under each policy by reweighting observed exposures with the density ratios; when exposures are not binary we threshold them before computing the weighted average.",
      paragraphs
    )
  }
  out <- paste(paragraphs, collapse = " ")
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

pretty_shift_label <- function(shift_name, label_mapping = NULL) {
  lbl <- shift_name %||% ""
  if (length(lbl) == 0 || is.na(lbl)) lbl <- ""
  if (exists("transform_label", mode = "function")) {
    lbl <- tryCatch(
      transform_label(
        label = shift_name,
        label_mapping = label_mapping,
        options = list(
          remove_tx_prefix = TRUE,
          remove_z_suffix = TRUE,
          remove_underscores = TRUE,
          use_title_case = TRUE,
          quiet = TRUE
        )
      ),
      error = function(e) shift_name
    )
  }
  lbl <- as.character(lbl)
  if (length(lbl) == 0 || all(!nzchar(lbl)) || all(is.na(lbl))) {
    lbl <- shift_name %||% ""
  }
  gsub("_", " ", lbl)
}

describe_shift <- function(shift_info, label_mapping = NULL) {
  sc <- shift_info$shift_clean %||% shift_info$shift_full
  sc_lower <- tolower(sc %||% "")
  label <- pretty_shift_label(sc, label_mapping)
  extra <- ""
  if (sc_lower %in% c("null", "identity")) {
    extra <- " (baseline/observed policy)"
  } else if (grepl("zero", sc_lower, fixed = TRUE)) {
    extra <- " (sets exposure to zero each wave)"
  } else if (grepl("weekly", sc_lower, fixed = TRUE)) {
    extra <- " (encourages weekly exposure)"
  } else if (grepl("monthly", sc_lower, fixed = TRUE)) {
    extra <- " (targets monthly exposure)"
  } else if (grepl("^ipsi", sc_lower)) {
    raw <- gsub("[^0-9\\.]", "", sub("^ipsi_?", "", sc_lower))
    delta <- suppressWarnings(as.numeric(raw))
    extra <- if (is.finite(delta)) {
      paste0(" (IPSI with \u03b4 = ", delta, ")")
    } else {
      " (incremental propensity shift)"
    }
  }
  paste0(label, extra)
}

build_shift_relationship_summary <- function(diagnostics, label_mapping = NULL) {
  if (!is.list(diagnostics) || !length(diagnostics)) return(NULL)
  if (!is.null(diagnostics$wave_table)) {
    diagnostics <- list(diagnostics)
  }
  labels <- vapply(diagnostics, describe_shift, character(1), label_mapping = label_mapping)
  labels <- labels[nzchar(labels)]
  if (!length(labels)) return(NULL)
  ordered <- paste(labels, collapse = " \u2192 ")
  baseline_note <- if (any(grepl("(baseline", labels, fixed = TRUE))) {
    " Identity serves as the baseline for contrasts."
  } else ""
  paste0("Policies examined (ordered as supplied): ", ordered, ".", baseline_note)
}
# This file adds high-level wrappers; provide a local `%||%` helper
`%||%` <- function(x, y) if (!is.null(x)) x else y
#' Assemble a full LMTP positivity report (table, diagnostics, text, plot, methods)
