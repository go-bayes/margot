#' End-to-end Policy Workflow (depth selection -> report -> interpretation)
#'
#' Runs depth comparison, produces a mixed-depth policy summary report, and (optionally)
#' generates per-model interpretations. Designed as a one-call wrapper for
#' policymaker-facing reporting, with safe defaults that prefer simple trees unless
#' depth-2 gains are material.
#'
#' @param stability A `margot_stability_policy_tree` object produced by
#'   [margot_policy_tree_stability()].
#' @param original_df Optional original-scale data (for scale annotations).
#' @param label_mapping Optional named list mapping outcome and variable names.
#' @param se_method Character; "plugin" (default) or "bootstrap".
#' @param R Integer; bootstrap reps when `se_method = "bootstrap"` (default 499).
#' @param dominance_threshold Numeric in (0,1); required dominance share for a restricted
#'   policy recommendation (default 0.8 for policy mode).
#' @param strict_branch Logical; require positive uplift CI for dominant branch before
#'   recommending restricted deployment (default TRUE).
#' @param min_gain_for_depth_switch Numeric; minimum PV gain required to switch from depth-1
#'   to depth-2 (default 0.005 on standardized outcomes).
#' @param include_interpretation Logical; also run [margot_interpret_policy_batch()] with the
#'   selected depths (default TRUE).
#' @param interpret_models Character; controls which models receive full interpretations when
#'   `include_interpretation = TRUE`. Options are:
#'   - `"wins"` (default): only models with statistically reliable policy value gains (95% CI > 0)
#'   - `"wins_borderline"`: models in Wins plus Borderline categories (useful for exploratory work)
#'   - `"recommended"`: all models flagged for deployment (current behaviour prior to this parameter)
#'   - A character vector of specific model names (e.g., `c("model_t2_neuroticism_z_r", "model_t2_agreeableness_z")`)
#' @param plot_models Character; controls which models receive policy tree plots. Options are:
#'   - `"none"` (default): do not generate plots
#'   - `"same"`: use the same models as `interpret_models`
#'   - `"wins"`: only models with statistically reliable policy value gains (95% CI > 0)
#'   - `"wins_borderline"`: models in Wins plus Borderline categories
#'   - `"recommended"`: all models flagged for deployment
#'   - A character vector of specific model names
#' @param plot_output_objects Character vector; which plot types to generate when `plot_models != "none"`.
#'   Options: `"policy_tree"`, `"decision_tree"`, `"combined_plot"`, `"qini_plot"`, `"diff_gain_summaries"`.
#'   Default: `"combined_plot"`.
#' @param policy_tree_args List; additional arguments passed to [margot_plot_policy_tree()]. Default: `list()`.
#' @param decision_tree_args List; additional arguments passed to [margot_plot_decision_tree()]. Default: `list()`.
#' @param qini_args List; additional arguments passed to [margot_plot_qini()]. Default: `list()`.
#' @param order_models Character; controls the order in which models appear in interpretations and plots.
#'   - `"alphabetical"` (default): sort models alphabetically by their label
#'   - `"by_effect"`: order by policy value (strongest effects first, as in previous versions)
#' @param audience Character; one of "policy" or "research" (default "policy").
#' @param use_coherent_in_interpret Logical; if TRUE, reuse PV rows from the coherent summary in
#'   interpretations instead of recomputing (default TRUE).
#' @param brief_include_group Logical; if TRUE, `policy_brief_df` includes the Group
#'   column (Wins/Neutral/Caution). If FALSE, returns a simplified brief without Group.
#'   Default FALSE for policy audiences.
#' @param expand_acronyms Logical; if TRUE, expand common acronyms in labels (e.g.,
#'   RWA -> Right-Wing Authoritarianism (RWA)). You may also supply custom expansions via
#'   `options(margot.boilerplate.acronyms = list(SES = "Socioeconomic Status"))`.
#' @param show_neutral Logical or NULL; controls inclusion of the Neutral group. Default NULL
#'   shows Neutral for research audiences and hides it for policy audiences. Set TRUE/FALSE
#'   to override explicitly.
#' @param prefer_stability Logical; if TRUE, raise the parsimony threshold for switching to
#'   depth-2 (min_gain_for_depth_switch >= 0.01) to prefer depth-1 unless depth-2 gains are
#'   clearly larger.
#' @param signal_score Character; one of "none", "pv_snr", "uplift_snr", or "hybrid". When not
#'   "none", the summary text includes a "Signals Worth Monitoring" section that ranks Neutral
#'   models by the selected score (magnitude relative to uncertainty, optionally weighted by
#'   coverage and stability). See the package NEWS for details.
#' @param signals_k Integer; number of top signals to display when `signal_score != "none"`
#' @param ... Additional pass-through args to [margot_policy_summary_report()], e.g.,
#'   `split_compact`, `split_top_only`, etc.
#'
#' @return A list with components from depth selection (best), summary (summary), and
#'   optional interpretations (interpret). Includes `policy_brief_df` shortcut drawn from
#'   `summary$group_table_df`. Also returns `method_explanation` (long/short/prereg),
#'   `depth_comparison_report` (a list with `text` and `data` showing both depth-1 and
#'   depth-2 policy values for all outcomes with selection rationale),
#'   `summary$signals_df` when `signal_score != "none"`, and top-level `report`,
#'   `report_prose`, and `report_detail` for convenient access to narrative summaries
#'   (bullets, prose, or detailed per-model interpretations respectively).
#'   When `plot_models != "none"`, also returns `plots` (output from [margot_policy()]).
#'
#' @examples
#' \dontrun{
#' # minimal call - interpretations for wins only, no plots (defaults)
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping
#' )
#'
#' # access key outputs
#' wf$policy_brief_df    # summary table
#' wf$report_detail      # detailed interpretations
#' cat(wf$report_prose)
#'
#' # exploratory analysis - include borderline models in interpretations
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping,
#'   interpret_models = "wins_borderline",
#'   show_neutral = TRUE,
#'   signal_score = "pv_snr"
#' )
#'
#' # generate plots for the same models as interpretations
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping,
#'   interpret_models = "wins_borderline",
#'   plot_models = "same",
#'   plot_output_objects = "combined_plot"
#' )
#'
#' # access plots
#' wf$plots$model_t2_neuroticism_z_r$combined_plot
#'
#' # generate multiple plot types
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping,
#'   interpret_models = "wins",
#'   plot_models = "wins",
#'   plot_output_objects = c("combined_plot", "qini_plot")
#' )
#'
#' # interpret and plot specific models only
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping,
#'   interpret_models = c("model_t2_neuroticism_z_r", "model_t2_conscientiousness_z"),
#'   plot_models = c("model_t2_neuroticism_z_r")
#' )
#'
#' # full analysis with all options
#' wf <- margot_policy_workflow(
#'   policy_tree_result_stability,
#'   original_df = original_df,
#'   label_mapping = label_mapping,
#'   se_method = "plugin",
#'   interpret_models = "wins_borderline",
#'   plot_models = "same",
#'   plot_output_objects = "combined_plot",
#'   audience = "policy",
#'   prefer_stability = TRUE,
#'   show_neutral = TRUE,
#'   signal_score = "pv_snr",
#'   signals_k = 3
#' )
#' }
#'
#' @export
margot_policy_workflow <- function(stability,
                                   original_df = NULL,
                                   label_mapping = NULL,
                                   se_method = c("plugin", "bootstrap"),
                                   R = 499L,
                                   dominance_threshold = 0.8,
                                   strict_branch = TRUE,
                                   min_gain_for_depth_switch = 0.005,
                                   include_interpretation = TRUE,
                                   interpret_models = c("wins", "wins_borderline", "recommended"),
                                   plot_models = c("none", "same", "wins", "wins_borderline", "recommended"),
                                   plot_output_objects = "combined_plot",
                                   policy_tree_args = list(),
                                   decision_tree_args = list(),
                                   qini_args = list(),
                                   order_models = c("alphabetical", "by_effect"),
                                   audience = c("policy", "research"),
                                   use_coherent_in_interpret = TRUE,
                                   brief_include_group = FALSE,
                                   expand_acronyms = FALSE,
                                   show_neutral = NULL,
                                   prefer_stability = FALSE,
                                   signal_score = c("none", "pv_snr", "uplift_snr", "hybrid"),
                                   signals_k = 3,
                                   ...) {
  se_method <- match.arg(se_method)
  audience <- match.arg(audience)
  signal_score <- match.arg(signal_score)
  order_models <- match.arg(order_models)

  # interpret_models can be a preset keyword or a character vector of model names
  if (length(interpret_models) == 1 && interpret_models %in% c("wins", "wins_borderline", "recommended")) {
    interpret_models_preset <- interpret_models
  } else if (is.character(interpret_models) && length(interpret_models) >= 1) {
    # user supplied specific model names
    interpret_models_preset <- "custom"
    interpret_models_custom <- interpret_models
  } else {
    interpret_models_preset <- "wins"
  }

  # plot_models can be a preset keyword or a character vector of model names
  if (length(plot_models) == 1 && plot_models %in% c("none", "same", "wins", "wins_borderline", "recommended")) {
    plot_models_preset <- plot_models
  } else if (is.character(plot_models) && length(plot_models) >= 1) {
    # user supplied specific model names
    plot_models_preset <- "custom"
    plot_models_custom <- plot_models
  } else {
    plot_models_preset <- "none"
  }

  dots <- list(...)

  model_names_override <- NULL
  if ("model_names" %in% names(dots)) {
    model_names_override <- dots[["model_names"]]
    dots[["model_names"]] <- NULL
  }

  split_compact <- TRUE
  if ("split_compact" %in% names(dots)) {
    split_compact <- dots[["split_compact"]]
    dots[["split_compact"]] <- NULL
  }
  split_drop_zero <- TRUE
  if ("split_drop_zero" %in% names(dots)) {
    split_drop_zero <- dots[["split_drop_zero"]]
    dots[["split_drop_zero"]] <- NULL
  }
  split_top_only <- TRUE
  if ("split_top_only" %in% names(dots)) {
    split_top_only <- dots[["split_top_only"]]
    dots[["split_top_only"]] <- NULL
  }

  # Configure acronym expansion scope
  old_opt <- options(margot.expand_acronyms = isTRUE(expand_acronyms))
  on.exit(options(old_opt), add = TRUE)

  # Merge default labels (from boilerplate options) with user-provided mapping (user wins)
  bp_labels <- getOption("margot.boilerplate.labels", NULL)
  if (!is.null(bp_labels)) {
    if (is.null(label_mapping)) label_mapping <- list()
    label_mapping <- utils::modifyList(bp_labels, label_mapping)
  }

  # 1) Depth comparison with parsimony threshold
  # Prefer stability: increase the gain required to switch to depth-2
  eff_min_gain <- if (isTRUE(prefer_stability)) max(min_gain_for_depth_switch, 0.01) else min_gain_for_depth_switch

  best <- margot_policy_summary_compare_depths(
    stability,
    original_df = original_df,
    se_method = ifelse(se_method == "bootstrap", "bootstrap", "plugin"),
    R = R,
    seed = 42L,
    label_mapping = label_mapping,
    auto_recommend = TRUE,
    dominance_threshold = dominance_threshold,
    strict_branch = strict_branch,
    model_names = model_names_override,
    split_compact = split_compact,
    split_drop_zero = split_drop_zero,
    split_top_only = split_top_only,
    verbose = TRUE,
    min_gain_for_depth_switch = eff_min_gain
  )

  # Build depth comparison report (always shows both depths for transparency)
  depth_comparison_report <- .build_depth_comparison_report(
    best$depth_summary_df,
    eff_min_gain,
    label_mapping
  )

  # 2) Mixed-depth summary using chosen depths
  summary <- do.call(
    margot_policy_summary_report,
    c(
      list(
        object = stability,
        model_names = model_names_override,
        depths_by_model = best$depth_map,
        auto_recommend = TRUE,
        split_compact = split_compact,
        split_drop_zero = split_drop_zero,
        split_top_only = split_top_only,
        se_method = se_method,
        label_mapping = label_mapping,
        original_df = original_df,
        dominance_threshold = dominance_threshold,
        strict_branch = strict_branch,
        audience = audience,
        show_neutral = show_neutral,
        signal_score = signal_score,
        signals_k = signals_k,
        return_unit_masks = TRUE
      ),
      dots
    )
  )

  # 3) Optional interpretations on selected depths
 # determine which models to interpret based on interpret_models parameter
  interpret_model_ids <- if (interpret_models_preset == "wins") {
    summary$wins_model_ids
  } else if (interpret_models_preset == "wins_borderline") {
    unique(c(summary$wins_model_ids, summary$borderline_model_ids))
  } else if (interpret_models_preset == "recommended") {
    summary$recommended_model_ids
  } else if (interpret_models_preset == "custom") {
    # validate that custom model names exist
    valid_models <- intersect(interpret_models_custom, names(best$depth_map))
    if (length(valid_models) < length(interpret_models_custom)) {
      missing <- setdiff(interpret_models_custom, valid_models)
      cli::cli_warn("Some interpret_models not found and will be skipped: {.val {missing}}")
    }
    valid_models
  } else {
    summary$wins_model_ids
  }

  # helper function to sort model IDs alphabetically by their labels
  sort_models_alphabetically <- function(model_ids, label_mapping) {
    if (length(model_ids) <= 1) return(model_ids)
    # get labels for each model
    labels <- vapply(model_ids, function(mn) {
      tryCatch(
        .apply_label_stability(gsub("^model_", "", mn), label_mapping),
        error = function(e) gsub("^model_", "", mn)
      )
    }, character(1))
    # strip leading punctuation/parentheses for sorting
    # e.g., "(reduced) Neuroticism" sorts under "N" not "("
    sort_keys <- gsub("^[^A-Za-z]+", "", tolower(labels))
    model_ids[order(sort_keys)]
  }

  # apply ordering to interpret_model_ids
  if (order_models == "alphabetical" && length(interpret_model_ids) > 0) {
    interpret_model_ids <- sort_models_alphabetically(interpret_model_ids, label_mapping)
  }

  interpret <- NULL
  if (isTRUE(include_interpretation) && length(interpret_model_ids) > 0) {
    pv_source <- if (isTRUE(use_coherent_in_interpret)) "use_coherent" else "compute"
    interpret <- margot_interpret_policy_batch(
      stability,
      model_names = interpret_model_ids,
      depths_by_model = best$depth_map,
      report_policy_value = if (pv_source == "compute") "both" else "none",
      policy_value_R = R,
      policy_value_seed = 42L,
      policy_value_ci_level = 0.95,
      label_mapping = label_mapping,
      original_df = original_df,
      output_format = "prose",
      return_as_list = TRUE,
      policy_value_source = pv_source,
      coherent_values = summary$coherent_policy_values
    )
  } else if (isTRUE(include_interpretation) && length(interpret_model_ids) == 0) {
    cli::cli_inform("No models matched interpret_models = {.val {interpret_models_preset}}; skipping interpretations.")
  }

  # 4) Optional plots via margot_policy()
  # determine which models to plot based on plot_models parameter
  plot_model_ids <- if (plot_models_preset == "none") {
    character(0)
  } else if (plot_models_preset == "same") {
    # use the same models as interpret_model_ids
    interpret_model_ids
  } else if (plot_models_preset == "wins") {
    summary$wins_model_ids
  } else if (plot_models_preset == "wins_borderline") {
    unique(c(summary$wins_model_ids, summary$borderline_model_ids))
  } else if (plot_models_preset == "recommended") {
    summary$recommended_model_ids
  } else if (plot_models_preset == "custom") {
    # validate that custom model names exist
    valid_plot_models <- intersect(plot_models_custom, names(best$depth_map))
    if (length(valid_plot_models) < length(plot_models_custom)) {
      missing <- setdiff(plot_models_custom, valid_plot_models)
      cli::cli_warn("Some plot_models not found and will be skipped: {.val {missing}}")
    }
    valid_plot_models
  } else {
    character(0)
  }

  # apply ordering to plot_model_ids (unless "same", which inherits from interpret_model_ids)
  if (order_models == "alphabetical" && length(plot_model_ids) > 0 && plot_models_preset != "same") {
    plot_model_ids <- sort_models_alphabetically(plot_model_ids, label_mapping)
  }

  plots <- NULL
  if (length(plot_model_ids) > 0) {
    cli::cli_inform("Generating plots for {length(plot_model_ids)} model(s)...")
    plots <- tryCatch(
      margot_policy(
        result_outcomes = stability,
        model_names = plot_model_ids,
        depths_by_model = best$depth_map,
        original_df = original_df,
        label_mapping = label_mapping,
        output_objects = plot_output_objects,
        policy_tree_args = policy_tree_args,
        decision_tree_args = decision_tree_args,
        qini_args = qini_args
      ),
      error = function(e) {
        cli::cli_warn("Failed to generate plots: {e$message}")
        NULL
      }
    )
  }

  # 5) Methods explanation (long/short/prereg), built from actual settings
  method_context <- list(
    se_method = se_method,
    R = R,
    audience = audience,
    min_gain_for_depth_switch = eff_min_gain,
    prefer_stability = isTRUE(prefer_stability),
    dominance_threshold = dominance_threshold,
    strict_branch = strict_branch,
    restricted_scope_1 = "branch",
    restricted_scope_2 = "leaf",
    show_neutral = show_neutral,
    expand_acronyms = isTRUE(expand_acronyms),
    signal_score = dots[["signal_score"]] %||% signal_score,
    signals_k = dots[["signals_k"]] %||% signals_k
  )
  method_explanation <- tryCatch(
    margot_build_method_explanation(stability, best, summary, method_context, citations = TRUE),
    error = function(e) list(long = "", short = "", prereg = "")
  )

  # Build policy brief table: either grouped (with Group column) or simplified
  brief_full <- summary$group_table_df
  if (isTRUE(brief_include_group)) {
    brief_out <- brief_full
  } else {
    if (is.null(brief_full) || !nrow(brief_full)) {
      brief_out <- brief_full
    } else {
      cols <- c("Outcome", "Depth", "Policy Value (95% CI)", "Uplift in Treated (95% CI)", "Coverage (%)")
      have <- intersect(cols, names(brief_full))
      brief_out <- brief_full[, c(if (all(cols %in% names(brief_full))) cols else have), drop = FALSE]
    }
  }

  # apply alphabetical ordering to policy_brief_df if requested
 if (order_models == "alphabetical" && !is.null(brief_out) && nrow(brief_out) > 0 && "Outcome" %in% names(brief_out)) {
    # strip leading punctuation for sorting (same logic as sort_models_alphabetically)
    sort_keys <- gsub("^[^A-Za-z]+", "", tolower(brief_out$Outcome))
    brief_out <- brief_out[order(sort_keys), , drop = FALSE]
    rownames(brief_out) <- NULL
  }

  masks <- summary$unit_masks
  not_excluded_ids_by_model <- if (is.null(masks) || !length(masks)) {
    list()
  } else {
    lapply(masks, function(x) x$not_excluded_ids)
  }

  not_excluded_ids_df <- if (is.null(masks) || !length(masks)) {
    data.frame()
  } else {
    nm <- names(masks)
    dfs <- lapply(nm, function(mn) {
      ids <- masks[[mn]]$not_excluded_ids
      if (is.null(ids) || !length(ids)) return(NULL)
      data.frame(
        model = mn,
        outcome = tryCatch(.apply_label_stability(gsub("^model_", "", mn), label_mapping), error = function(e) gsub("^model_", "", mn)),
        depth = if (!is.null(best$depth_map) && mn %in% names(best$depth_map)) best$depth_map[[mn]] else NA_integer_,
        eval_id = ids,
        stringsAsFactors = FALSE
      )
    })
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs)) do.call(rbind, dfs) else data.frame()
  }

  wins_neutral_keep <- unique(c(summary$wins_model_ids, summary$neutral_model_ids))
  wins_neutral_ids_by_model <- if (is.null(masks) || !length(masks) || !length(wins_neutral_keep)) {
    list()
  } else {
    sel <- intersect(names(masks), wins_neutral_keep)
    if (!length(sel)) list() else lapply(masks[sel], function(x) x$not_excluded_ids)
  }

  wins_neutral_ids_df <- if (!length(wins_neutral_ids_by_model)) {
    data.frame()
  } else {
    dfs <- lapply(names(wins_neutral_ids_by_model), function(mn) {
      ids <- wins_neutral_ids_by_model[[mn]]
      if (is.null(ids) || !length(ids)) return(NULL)
      data.frame(
        model = mn,
        outcome = tryCatch(.apply_label_stability(gsub("^model_", "", mn), label_mapping), error = function(e) gsub("^model_", "", mn)),
        depth = if (!is.null(best$depth_map) && mn %in% names(best$depth_map)) best$depth_map[[mn]] else NA_integer_,
        eval_id = ids,
        stringsAsFactors = FALSE
      )
    })
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs)) do.call(rbind, dfs) else data.frame()
  }

  models_wins_or_has_ids <- unique(c(names(wins_neutral_ids_by_model), summary$wins_model_ids))
  models_wins_or_has_ids_labels <- if (!length(models_wins_or_has_ids)) character(0) else vapply(
    models_wins_or_has_ids,
    function(mn) tryCatch(.apply_label_stability(gsub("^model_", "", mn), label_mapping), error = function(e) gsub("^model_", "", mn)),
    character(1)
  )

  list(
    best = best,
    summary = summary,
    interpret = interpret,
    plots = plots,
    method_explanation = method_explanation,
    depth_comparison_report = depth_comparison_report,
    policy_brief_df = brief_out,
    report = summary$report,
    report_prose = summary$report_prose,
    report_detail = if (!is.null(interpret)) interpret$report_detail else NULL,
    not_excluded_ids_by_model = not_excluded_ids_by_model,
    not_excluded_ids_df = not_excluded_ids_df,
    wins_neutral_ids_by_model = wins_neutral_ids_by_model,
    wins_neutral_ids_df = wins_neutral_ids_df,
    models_wins_or_has_ids = models_wins_or_has_ids,
    models_wins_or_has_ids_labels = models_wins_or_has_ids_labels
  )
}

#' Build Depth Comparison Report
#'
#' Internal helper to generate a transparent depth comparison report showing
#' both depth-1 and depth-2 policy values for all outcomes.
#'
#' @param depth_summary_df Data frame from margot_policy_summary_compare_depths
#' @param threshold The min_gain_for_depth_switch threshold used
#' @param label_mapping Optional label mapping
#' @return A list with text (formatted report) and data (summary data frame)
#' @keywords internal
.build_depth_comparison_report <- function(depth_summary_df, threshold, label_mapping = NULL) {
  if (is.null(depth_summary_df) || !nrow(depth_summary_df)) {
    return(list(
      text = "No depth comparison data available.",
      data = data.frame()
    ))
  }

  # Build per-outcome comparison lines
  lines <- character()
  lines <- c(lines, "## Depth Comparison Report", "")
  lines <- c(lines, sprintf("Parsimony threshold: %.3f (%.1f%% policy value improvement required for depth-2)",
                            threshold, threshold * 100))
  lines <- c(lines, "")
  lines <- c(lines, "| Outcome | Depth-1 PV | Depth-2 PV | Gain (delta) | Selected | Rationale |")
  lines <- c(lines, "|---------|------------|------------|----------|----------|-----------|")


  for (i in seq_len(nrow(depth_summary_df))) {
    row <- depth_summary_df[i, ]
    outcome_label <- row$outcome_label
    pv1 <- row$pv_depth1
    pv2 <- row$pv_depth2
    selected <- row$depth_selected

    # Calculate gain
    delta <- if (!is.na(pv1) && !is.na(pv2)) pv2 - pv1 else NA_real_

    # Determine rationale
    if (is.na(delta)) {
      rationale <- "Missing data"
    } else if (delta > threshold) {
      rationale <- sprintf("Gain (%.3f) > threshold", delta)
    } else if (delta > 0) {
      rationale <- sprintf("Gain (%.3f) below threshold; prefer simpler tree", delta)
    } else {
      rationale <- "Depth-1 performs better"
    }

    # Format values
    pv1_str <- if (!is.na(pv1)) sprintf("%.3f", pv1) else "—"
    pv2_str <- if (!is.na(pv2)) sprintf("%.3f", pv2) else "—"
    delta_str <- if (!is.na(delta)) sprintf("%.4f", delta) else "—"
    selected_str <- if (!is.na(selected)) sprintf("Depth-%d", selected) else "—"

    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s |",
                              outcome_label, pv1_str, pv2_str, delta_str, selected_str, rationale))
  }

  lines <- c(lines, "")

  # Add summary counts
  n_d1 <- sum(depth_summary_df$depth_selected == 1, na.rm = TRUE)
  n_d2 <- sum(depth_summary_df$depth_selected == 2, na.rm = TRUE)
  lines <- c(lines, sprintf("**Summary**: %d outcome(s) use depth-1, %d outcome(s) use depth-2.", n_d1, n_d2))

  # Add interpretation note
  lines <- c(lines, "")
  lines <- c(lines, "*Note: Depth-1 trees (single split) are preferred when the policy value gain from")

  lines <- c(lines, "adding complexity is below the threshold. This favours interpretable, stable policies.*")

  list(
    text = paste(lines, collapse = "\n"),
    data = depth_summary_df[, c("outcome_label", "pv_depth1", "pv_depth2", "depth_selected"), drop = FALSE]
  )
}
