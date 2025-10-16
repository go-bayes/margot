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
#' @param expand_acronyms Logical; if TRUE, expand common acronyms in labels (e.g.,
#'   RWA → Right-Wing Authoritarianism (RWA)). You may also supply custom expansions via
#'   `options(margot.boilerplate.acronyms = list(SES = "Socioeconomic Status"))`.
#' @param show_neutral Logical or NULL; controls inclusion of the Neutral group. Default NULL
#'   shows Neutral for research audiences and hides it for policy audiences. Set TRUE/FALSE
#'   to override explicitly.
#' @param prefer_stability Logical; if TRUE, raise the parsimony threshold for switching to
#'   depth-2 (min_gain_for_depth_switch ≥ 0.01) to prefer depth-1 unless depth-2 gains are
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
#'   `summary$group_table_df`. Also returns `method_explanation` (long/short/prereg) and
#'   `summary$signals_df` when `signal_score != "none"`.
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
                                   expand_acronyms = FALSE,
                                   show_neutral = NULL,
                                   prefer_stability = FALSE,
                                   signal_score = c("none", "pv_snr", "uplift_snr", "hybrid"),
                                   signals_k = 3,
                                   ...) {
  se_method <- match.arg(se_method)
  audience <- match.arg(audience)
  signal_score <- match.arg(signal_score)

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
    split_compact = TRUE,
    split_drop_zero = TRUE,
    split_top_only = TRUE,
    verbose = TRUE,
    min_gain_for_depth_switch = eff_min_gain
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
    show_neutral = show_neutral,
    signal_score = signal_score,
    signals_k = signals_k,
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

  # 4) Methods explanation (long/short/prereg), built from actual settings
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
    signal_score = list(...)[["signal_score"]] %||% tryCatch(match.arg(signal_score), error = function(e) NULL),
    signals_k = list(...)[["signals_k"]] %||% tryCatch(signals_k, error = function(e) NULL)
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
    method_explanation = method_explanation,
    policy_brief_df = brief_out,
    not_excluded_ids_by_model = not_excluded_ids_by_model,
    not_excluded_ids_df = not_excluded_ids_df,
    wins_neutral_ids_by_model = wins_neutral_ids_by_model,
    wins_neutral_ids_df = wins_neutral_ids_df,
    models_wins_or_has_ids = models_wins_or_has_ids,
    models_wins_or_has_ids_labels = models_wins_or_has_ids_labels
  )
}
