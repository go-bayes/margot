#' End-to-end Policy Workflow (depth selection → report → interpretation)
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
#' @param audience Character; one of "policy" or "research" (default "policy").
#' @param use_coherent_in_interpret Logical; if TRUE, reuse PV rows from the coherent summary in
#'   interpretations instead of recomputing (default TRUE).
#' @param brief_include_group Logical; if TRUE, `policy_brief_df` includes the Group
#'   column (Wins/Neutral/Caution). If FALSE, returns a simplified brief without Group.
#'   Default FALSE for policy audiences.
#' @param ... Additional pass-through args to [margot_policy_summary_report()], e.g.,
#'   `split_compact`, `split_top_only`, etc.
#'
#' @return A list with components from depth selection (best), summary (summary), and
#'   optional interpretations (interpret). Includes `policy_brief_df` shortcut drawn from
#'   `summary$group_table_df`.
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
                                   audience = c("policy", "research"),
                                   use_coherent_in_interpret = TRUE,
                                   brief_include_group = FALSE,
                                   ...) {
  se_method <- match.arg(se_method)
  audience <- match.arg(audience)

  # 1) Depth comparison with parsimony threshold
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
    split_compact = TRUE,
    split_drop_zero = TRUE,
    split_top_only = TRUE,
    verbose = TRUE,
    min_gain_for_depth_switch = min_gain_for_depth_switch
  )

  # 2) Mixed-depth summary using chosen depths
  summary <- margot_policy_summary_report(
    stability,
    depths_by_model = best$depth_map,
    auto_recommend = TRUE,
    split_compact = TRUE,
    split_drop_zero = TRUE,
    split_top_only = TRUE,
    se_method = se_method,
    label_mapping = label_mapping,
    original_df = original_df,
    dominance_threshold = dominance_threshold,
    strict_branch = strict_branch,
    audience = audience,
    return_unit_masks = TRUE,
    ...
  )

  # 3) Optional interpretations on selected depths
  interpret <- NULL
  if (isTRUE(include_interpretation)) {
    pv_source <- if (isTRUE(use_coherent_in_interpret)) "use_coherent" else "compute"
    interpret <- margot_interpret_policy_batch(
      stability,
      model_names = summary$recommended_model_ids,
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
  }

  # Build policy brief table: either grouped (with Group column) or simplified
  brief_full <- summary$group_table_df
  if (isTRUE(brief_include_group)) {
    brief_out <- brief_full
  } else {
    if (is.null(brief_full) || !nrow(brief_full)) {
      brief_out <- brief_full
    } else {
      cols <- c("Outcome", "Depth", "Effect Size (95% CI)", "Effect in Treated (95% CI)", "Coverage (%)")
      have <- intersect(cols, names(brief_full))
      brief_out <- brief_full[, c(if (all(cols %in% names(brief_full))) cols else have), drop = FALSE]
    }
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
    policy_brief_df = brief_out,
    not_excluded_ids_by_model = not_excluded_ids_by_model,
    not_excluded_ids_df = not_excluded_ids_df,
    wins_neutral_ids_by_model = wins_neutral_ids_by_model,
    wins_neutral_ids_df = wins_neutral_ids_df,
    models_wins_or_has_ids = models_wins_or_has_ids,
    models_wins_or_has_ids_labels = models_wins_or_has_ids_labels
  )
}
