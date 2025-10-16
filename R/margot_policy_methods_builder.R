#' Build Methods Text From Workflow Context
#'
#' Constructs detailed, parameter-aware methods text suitable for scientific
#' reporting and preregistration, based on the actual objects and settings used
#' in `margot_policy_workflow()`. Returns long, short, and prereg variants.
#'
#' @param stability A `margot_stability_policy_tree` object.
#' @param best The result of `margot_policy_summary_compare_depths()`.
#' @param summary The result of `margot_policy_summary_report()`.
#' @param context List of key settings used by the workflow: `se_method`, `R`,
#'   `audience`, `min_gain_for_depth_switch`, `prefer_stability`,
#'   `dominance_threshold`, `strict_branch`, `restricted_scope_1`,
#'   `restricted_scope_2`, `show_neutral`, `expand_acronyms`.
#' @param citations Logical; include inline citations (default TRUE).
#' @return A named list with `long`, `short`, and `prereg` character strings.
#' @keywords internal
margot_build_method_explanation <- function(stability, best, summary, context, citations = TRUE) {
  md <- stability$metadata %||% list()
  vary_type <- md$vary_type %||% NULL
  n_iter <- md$n_iterations %||% NULL
  train_props <- md$train_proportions %||% md$train_proportion %||% NULL
  tree_method <- md$tree_method %||% NULL
  metaseed <- md$metaseed %||% NULL

  cite <- function(txt) if (isTRUE(citations)) paste0(" (", txt, ")") else ""
  train_txt <- if (!is.null(train_props)) paste0("a held-out test fold (train proportion ", paste0(train_props, collapse = ","), ")") else "a held-out test fold"
  method_engine <- if (!is.null(tree_method)) tree_method else "policytree"
  vt <- if (!is.null(vary_type)) switch(vary_type,
                                       split_only = "random train/test splits",
                                       bootstrap = "bootstrap resampling",
                                       both = "bootstrap resampling and train/test splits",
                                       vary_type) else NULL
  nit <- if (!is.null(n_iter)) paste0(n_iter, " iterations of ") else ""
  seed_txt <- if (!is.null(metaseed)) paste0(" (metaseed ", metaseed, ")") else ""

  # Pull recommendation counts if available
  dec_counts <- list(full = NA_integer_, restricted = NA_integer_, caution = NA_integer_, do_not_deploy = NA_integer_)
  if (!is.null(summary$recommendations_by_model)) {
    decs <- vapply(summary$recommendations_by_model, function(x) x$decision %||% NA_character_, character(1))
    dec_counts$full <- sum(decs == "deploy_full", na.rm = TRUE)
    dec_counts$restricted <- sum(decs == "deploy_restricted", na.rm = TRUE)
    dec_counts$caution <- sum(decs == "caution", na.rm = TRUE)
    dec_counts$do_not_deploy <- sum(decs == "do_not_deploy", na.rm = TRUE)
  }

  # Core statements
  s1 <- paste0(
    "We estimated individualized treatment effects using doubly robust (DR) scores from causal forests and learned depth-1 and depth-2 policy trees to target treatment",
    cite("Athey & Wager, 2021; grf; policytree"), ". Policy trees were fit with ", method_engine,
    ", and evaluated on ", train_txt, " to avoid adaptive overfitting (honest evaluation)."
  )

  s2 <- paste0(
    "Policy value was summarized as the expected gain relative to universal control (policy − control-all) and relative to universal treatment (policy − treat-all), with 95% confidence intervals computed via ",
    if (identical(context$se_method, "bootstrap")) paste0("bootstrap (R=", context$R, ")") else "plug-in standard errors", ".",
    cite("Ehrlich et al., 2024; Athey & Wager, 2021")
  )

  s3 <- paste0(
    "Depth selection applied a parsimony rule: switch to depth-2 only when the policy-value gain exceeded ",
    format(context$min_gain_for_depth_switch, trim = TRUE), ".",
    if (isTRUE(context$prefer_stability)) " We preferred stability by raising this threshold for deployment-focused summaries." else ""
  )

  s4 <- paste0(
    "Restricted-policy recommendations treated only within dominant favorable splits, using dominance threshold ",
    format(context$dominance_threshold, trim = TRUE),
    if (isTRUE(context$strict_branch)) "; dominant split required uplift CI to exclude zero" else "; dominant split could include zero", "."
  )

  s5 <- if (!is.null(vt)) paste0(
    "To assess robustness, a stability analysis used ", nit, vt, seed_txt,
    ", summarizing variable inclusion frequencies and threshold variability",
    cite("Meinshausen & Bühlmann, 2010"), "."
  ) else ""

  s6 <- paste0(
    "We report effect sizes and uncertainty without dichotomous rules, and surface large-but-uncertain ‘signals worth monitoring’ ranked by magnitude relative to uncertainty (SNR-style), optionally adjusted for coverage and stability",
    cite("Efron, 2010; Tusher et al., 2001; Smyth, 2004; Gelman & Stern, 2006"), "."
  )

  s7 <- paste0(
    "Presentation settings: audience=", context$audience, "; show_neutral=", if (is.null(context$show_neutral)) "default" else as.character(context$show_neutral),
    "; expand_acronyms=", as.character(isTRUE(context$expand_acronyms)), "."
  )

  # Signal score methods (if enabled)
  s8 <- ""
  if (!is.null(context$signal_score) && !identical(context$signal_score, "none")) {
    sc <- context$signal_score
    base <- switch(sc,
      pv_snr = "Signal score (PV‑SNR) ranks |PV(control‑all)| relative to its 95% CI half‑width (|estimate| / ((CI_hi − CI_lo)/2)), then multiplies by coverage^(1/2) and by a stability weight (0.5 + 0.5 × consensus strength).",
      uplift_snr = "Signal score (uplift‑SNR) ranks |uplift among treated| relative to its 95% CI half‑width, then multiplies by coverage^(1/2) and by (0.5 + 0.5 × consensus strength).",
      hybrid = "Signal score (hybrid) is 0.7 × PV‑SNR + 0.3 × uplift‑SNR, weighting policy value more heavily while still surfacing large uplift signals.",
      "Signal score ranks magnitude relative to uncertainty and applies coverage and stability weights."
    )
    refs <- cite("Efron, 2010; Tusher et al., 2001; Smyth, 2004; Gelman & Stern, 2006; Meinshausen & Bühlmann, 2010")
    s8 <- paste0(base, " Shown as a descriptive ranking (no gating); top ", context$signals_k %||% 3, " are reported in text.", refs)
  }

  # Optional boilerplate overrides via JSON slots (glue templates)
  bp_ctx <- c(context,
              list(method_engine = method_engine, train_txt = train_txt,
                   vary_type = vt, n_iterations = n_iter, metaseed = metaseed,
                   dominance_threshold = context$dominance_threshold,
                   strict_branch = context$strict_branch))
  long_bp <- margot_get_boilerplate("methods_long", context = bp_ctx)
  short_bp <- margot_get_boilerplate("methods_short", context = bp_ctx)
  prereg_bp <- margot_get_boilerplate("methods_prereg", context = bp_ctx)

  # Short (single paragraph)
  short <- if (!is.null(short_bp) && nzchar(short_bp)) short_bp else paste(s1, s2, s3, s4, s5, s6, s8)

  # Long (multi-paragraph)
  long <- if (!is.null(long_bp) && nzchar(long_bp)) long_bp else paste(s1, s2, s3, s4, s5, s6, s7, s8, sep = "\n\n")

  # Preregistration-style enumerated steps
  prereg <- if (!is.null(prereg_bp) && nzchar(prereg_bp)) prereg_bp else paste0(
    "1) Estimate DR scores via causal forests and fit depth-1 and depth-2 policy trees (engine: ", method_engine, ").\n",
    "2) Evaluate on ", train_txt, " (honest evaluation).\n",
    "3) Compute policy value contrasts (policy − control-all; policy − treat-all) with ",
    if (identical(context$se_method, "bootstrap")) paste0("bootstrap SEs, R=", context$R) else "plug-in SEs", ", CI=95%.\n",
    "4) Select depth using a parsimony rule: switch to depth-2 if gain > ", format(context$min_gain_for_depth_switch, trim = TRUE),
    if (isTRUE(context$prefer_stability)) " (stability preference applied)." else ".", "\n",
    "5) Generate restricted-policy recommendations when a dominant favorable split exists (dominance threshold ", format(context$dominance_threshold, trim = TRUE),
    if (isTRUE(context$strict_branch)) ", uplift CI excludes zero)." else ", uplift CI may include zero).", "\n",
    if (!is.null(vt)) paste0("6) Stability analysis: ", nit, vt, seed_txt, ".\n") else "",
    "7) Reporting: no dichotomous thresholds; surface large-but-uncertain signals using magnitude/uncertainty rankings (", context$signal_score %||% "none", ").\n",
    "8) Presentation: audience=", context$audience, "; show_neutral=", if (is.null(context$show_neutral)) "default" else as.character(context$show_neutral),
    "; expand_acronyms=", as.character(isTRUE(context$expand_acronyms)), ".\n"
  )

  list(long = long, short = short, prereg = prereg)
}
