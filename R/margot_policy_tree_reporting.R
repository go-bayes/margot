#' Plot a policy-tree branching decision tree
#'
#' @description
#' Thin, explicit wrapper around \code{\link{margot_plot_decision_tree}()}.
#' Use this helper when the intended artefact is the branching assignment rule.
#'
#' @param result_object A list returned by \code{margot_causal_forest()} or a
#'   compatible policy-tree workflow object.
#' @param model_name Character scalar naming the model to plot, with or without
#'   the \code{model_} prefix.
#' @param ... Arguments passed to
#'   \code{\link{margot_plot_decision_tree}()}.
#'
#' @return A ggplot object.
#' @export
margot_plot_policy_decision_tree <- function(result_object, model_name, ...) {
  # keep the explicit policy-tree name while preserving the stable plotter.
  model_name <- .margot_leaf_resolve_model_name(result_object, model_name)
  margot_plot_decision_tree(result_object, model_name = model_name, ...)
}

#' Plot policy-tree projections of evaluation points
#'
#' @description
#' Thin, explicit wrapper around \code{\link{margot_plot_policy_tree}()}.
#' Use this helper when the intended artefact is the point/projection plot that shows
#' observations relative to the selected policy-tree splits.
#'
#' @param result_object A list returned by \code{margot_causal_forest()} or a
#'   compatible policy-tree workflow object.
#' @param model_name Character scalar naming the model to plot, with or without
#'   the \code{model_} prefix.
#' @param ... Arguments passed to \code{\link{margot_plot_policy_tree}()}.
#'
#' @return A ggplot or patchwork plot object.
#' @export
margot_plot_policy_projection <- function(result_object, model_name, ...) {
  # give the projection plot an unambiguous public name.
  model_name <- .margot_leaf_resolve_model_name(result_object, model_name)
  margot_plot_policy_tree(result_object, model_name = model_name, ...)
}

#' Plot policy-tree decision and projection panels
#'
#' @description
#' Builds the standard two-panel policy-tree display: the branching decision
#' tree in panel A, with the projection plot of evaluation points below in
#' panel B.
#'
#' @param result_object A list returned by \code{margot_causal_forest()} or a
#'   compatible policy-tree workflow object.
#' @param model_name Character scalar naming the model to plot.
#' @param max_depth Integer, usually \code{1} or \code{2}.
#' @param original_df Optional data frame with original-scale variables.
#' @param label_mapping Optional named list used for display labels.
#' @param show_leaf_metrics Logical. If \code{TRUE}, the decision tree panel
#'   includes leaf \code{T-C} contrasts and sample shares.
#' @param leaf_metrics Optional output from
#'   \code{\link{margot_policy_leaf_summary}()} or
#'   \code{\link{margot_table_policy_tree}()}.
#' @param layout List passed to \code{\link{margot_plot_policy_combo}()} for
#'   panel heights.
#' @param annotation List passed to \code{\link{margot_plot_policy_combo}()}
#'   for tags.
#' @param projection_args Optional list of arguments passed to
#'   \code{\link{margot_plot_policy_projection}()}.
#' @param decision_tree_args Optional list of arguments passed to
#'   \code{\link{margot_plot_policy_decision_tree}()}.
#'
#' @return A list with \code{decision_tree}, \code{projection}, and
#'   \code{combined_plot}.
#' @export
margot_plot_policy_tree_panels <- function(result_object,
                                           model_name,
                                           max_depth = 2L,
                                           original_df = NULL,
                                           label_mapping = NULL,
                                           show_leaf_metrics = TRUE,
                                           leaf_metrics = NULL,
                                           layout = list(heights = c(1, 2)),
                                           annotation = list(tag_levels = "A"),
                                           projection_args = list(),
                                           decision_tree_args = list()) {
  # assemble the two policy-tree plot types in the standard reading order.
  model_name <- .margot_leaf_resolve_model_name(result_object, model_name)
  decision_defaults <- list(
    show_leaf_metrics = show_leaf_metrics,
    leaf_metrics = leaf_metrics
  )
  decision_tree_args <- .margot_policy_reporting_args(
    defaults = decision_defaults,
    overrides = decision_tree_args
  )

  combo <- margot_plot_policy_combo(
    result_object = result_object,
    model_name = model_name,
    max_depth = max_depth,
    label_mapping = label_mapping,
    original_df = original_df,
    layout = layout,
    annotation = annotation,
    generate_policy_tree = TRUE,
    generate_decision_tree = TRUE,
    policy_tree_args = projection_args,
    decision_tree_args = decision_tree_args
  )

  out <- list(
    decision_tree = combo$decision_tree,
    projection = combo$policy_tree,
    combined_plot = combo$combined_plot
  )
  class(out) <- c("margot_policy_tree_panels", "list")
  out
}

#' Create a policy-tree leaf reporting table
#'
#' @description
#' Returns a tidy leaf table using the package reporting convention: the
#' selected action is shown separately from the signed treatment-control
#' contrast. Positive \code{T-C} values favour treatment and negative values
#' favour control.
#'
#' @details
#' Let \eqn{\Gamma_{ja}} denote the action score for observation \eqn{j} under
#' action \eqn{a}, let \eqn{L} denote a policy-tree leaf, and let
#' \eqn{E_L} denote the evaluation observations routed to that leaf. For binary
#' actions \eqn{C} and \eqn{T}, the reported evaluation-sample contrast is
#' \deqn{
#'   \Delta_L =
#'   \frac{\sum_{j \in E_L} w_j\{\Gamma_{jT} - \Gamma_{jC}\}}
#'        {\sum_{j \in E_L} w_j}.
#' }
#' The selected action is reported separately. For the fitted tree, it is the
#' action stored in the terminal node, learned on the training observations
#' \eqn{S_L} routed to that node:
#' \deqn{
#'   \pi(L) = \arg\max_{a \in \{C,T\}}
#'   \frac{\sum_{j \in S_L} w_j \Gamma_{ja}}{\sum_{j \in S_L} w_j}.
#' }
#' Held-out CV tables therefore report actions learned on training folds and
#' signed contrasts computed on held-out evaluation rows; they do not reselect
#' actions from held-out means.
#' Between-leaf differences in \eqn{\Delta_L} describe variation in the
#' magnitude of the score contrast; they are not the decision rule used by the
#' policy tree.
#'
#' @param object A \code{margot_causal_forest()}-style object, or a
#'   \code{margot_policy_tree_cv} object.
#' @param model_name Optional model name, with or without the \code{model_}
#'   prefix. Required for display-tree tables.
#' @param depth Integer tree depth. If \code{NULL} for a CV object, the selected
#'   depth map is used when available.
#' @param weights Optional evaluation weights for display-tree tables.
#' @param digits Integer; rounding used in formatted columns.
#' @param ci_level Confidence level for display-tree score intervals.
#' @param label_mapping Optional named list used for display labels.
#' @param source Character. \code{"auto"} chooses \code{"heldout_cv"} for
#'   \code{margot_policy_tree_cv} objects and \code{"display_tree"} otherwise.
#' @param include_selected_action_difference Logical. Include the selected
#'   action minus alternative action score contrast. Defaults to \code{FALSE}.
#' @param include_value_contribution Logical. Include baseline-dependent value
#'   contribution columns. Defaults to \code{FALSE}.
#' @param baseline Character. Baseline for optional value contribution columns.
#'
#' @return A tibble with one row per reported leaf or held-out leaf-action
#'   summary.
#' @export
margot_table_policy_tree <- function(object,
                                     model_name = NULL,
                                     depth = NULL,
                                     weights = NULL,
                                     digits = 3L,
                                     ci_level = 0.95,
                                     label_mapping = NULL,
                                     source = c("auto", "display_tree", "heldout_cv"),
                                     include_selected_action_difference = FALSE,
                                     include_value_contribution = FALSE,
                                     baseline = c("control_all", "treat_all")) {
  # build a stable machine-readable leaf table for manuscripts and reports.
  source <- match.arg(source)
  baseline <- match.arg(baseline)
  if (identical(source, "auto")) {
    source <- if (inherits(object, "margot_policy_tree_cv")) "heldout_cv" else "display_tree"
  }

  if (identical(source, "heldout_cv")) {
    out <- .margot_table_policy_tree_cv(
      object = object,
      model_name = model_name,
      depth = depth,
      digits = digits,
      include_selected_action_difference = include_selected_action_difference,
      include_value_contribution = include_value_contribution,
      baseline = baseline
    )
  } else {
    out <- .margot_table_policy_tree_display(
      object = object,
      model_name = model_name,
      depth = depth %||% 1L,
      weights = weights,
      digits = digits,
      ci_level = ci_level,
      label_mapping = label_mapping,
      include_selected_action_difference = include_selected_action_difference,
      include_value_contribution = include_value_contribution,
      baseline = baseline
    )
  }

  attr(out, "source") <- source
  out
}

#' Generate standard policy-tree reporting text
#'
#' @description
#' Produces cautious stock text for policy-tree reports. The text describes the
#' reporting convention without making substantive claims about moderators.
#'
#' @details
#' Policy-tree reporting separates the selected action learned by the fitted
#' tree from the signed treatment-control score contrast computed on evaluation
#' rows. Let \eqn{\Gamma_{ja}} denote the action score for observation \eqn{j}
#' under action \eqn{a}, let \eqn{L} denote a policy-tree leaf, and let
#' \eqn{E_L} denote the evaluation observations routed to that leaf. For binary
#' actions \eqn{C} and \eqn{T}, the reported contrast is
#' \deqn{
#'   \Delta_L =
#'   \frac{\sum_{j \in E_L} w_j\{\Gamma_{jT} - \Gamma_{jC}\}}
#'        {\sum_{j \in E_L} w_j}.
#' }
#' The stored selected action is learned on training observations \eqn{S_L}:
#' \deqn{
#'   \pi(L) = \arg\max_{a \in \{C,T\}}
#'   \frac{\sum_{j \in S_L} w_j \Gamma_{ja}}{\sum_{j \in S_L} w_j}.
#' }
#' If every displayed leaf has the same selected action, the tree describes
#' variation in score-contrast magnitude rather than a selective rule that
#' changes actions across leaves.
#'
#' @param source Character. Reporting source to describe.
#' @param include_ci Logical. Include the interval-interpretation sentence.
#' @param include_plot_convention Logical. Include the two-panel plot sentence.
#' @param collapse Logical. If \code{TRUE}, return one character string;
#'   otherwise return a character vector of sentences.
#'
#' @return A character string or character vector.
#' @export
margot_text_policy_tree <- function(source = c("generic", "heldout_cv", "display_tree"),
                                    include_ci = TRUE,
                                    include_plot_convention = TRUE,
                                    collapse = TRUE) {
  # return reusable text that avoids over-interpreting selected leaves.
  source <- match.arg(source)
  sentences <- c(
    "Policy-tree leaves are reported with signed treatment-control action-score contrasts (`T-C`). Positive values favour treatment and negative values favour control.",
    "Selected actions are the actions stored by the learned tree; evaluation summaries do not reselect actions from evaluation-row means."
  )

  if (identical(source, "heldout_cv")) {
    sentences <- c(
      sentences,
      "For cross-validated reports, policy trees are learned on training folds and evaluated on held-out observations."
    )
  } else if (identical(source, "display_tree")) {
    sentences <- c(
      sentences,
      "For display-tree reports, the table describes the fitted display tree and should be treated as a descriptive summary."
    )
  }

  if (isTRUE(include_ci)) {
    sentences <- c(
      sentences,
      "Intervals summarise score variation after tree selection and should not be interpreted as formal post-selection subgroup tests."
    )
  }

  if (isTRUE(include_plot_convention)) {
    sentences <- c(
      sentences,
      "When both policy-tree plots are shown, panel A presents the branching decision rule and panel B presents the projection of evaluation points relative to the selected splits."
    )
  }

  if (isTRUE(collapse)) paste(sentences, collapse = " ") else sentences
}

#' Assemble policy-tree plots, table, and standard text
#'
#' @description
#' Convenience wrapper for the standard policy-tree reporting artefacts. The
#' returned components are ordinary objects that can be edited, replaced, or
#' omitted in manuscript workflows.
#'
#' @details
#' This helper reports display-tree artefacts and can also attach held-out
#' policy-value summaries when supplied a \code{margot_policy_tree_cv} object.
#' Leaf tables use the signed evaluation-sample \code{T-C} contrast
#' \deqn{
#'   \Delta_L =
#'   \frac{\sum_{j \in E_L} w_j\{\Gamma_{jT} - \Gamma_{jC}\}}
#'        {\sum_{j \in E_L} w_j}
#' }
#' where \eqn{\Gamma_{ja}} is the action score for observation \eqn{j} under
#' action \eqn{a} and \eqn{E_L} are evaluation observations in leaf \eqn{L}.
#' The selected action is reported separately as the fitted tree's stored
#' action, learned on training observations \eqn{S_L}:
#' \deqn{
#'   \pi(L) = \arg\max_{a \in \{C,T\}}
#'   \frac{\sum_{j \in S_L} w_j \Gamma_{ja}}{\sum_{j \in S_L} w_j}.
#' }
#' Tree-level value summaries compare the learned rule with all-control,
#' all-treatment, and best-constant baselines. Between-leaf differences in
#' \eqn{\Delta_L} describe variation in magnitude, not the policy-tree decision
#' rule.
#'
#' @param result_object A \code{margot_causal_forest()}-style object.
#' @param model_name Character scalar naming the model to report.
#' @param policy_cv Optional \code{margot_policy_tree_cv} object with held-out
#'   policy-tree diagnostics.
#' @param depth Optional integer tree depth. If \code{NULL} and \code{policy_cv}
#'   is supplied, the selected depth from \code{policy_cv$depth_map} is used
#'   when available; otherwise depth one is used.
#' @param original_df Optional data frame with original-scale variables.
#' @param weights Optional evaluation weights.
#' @param digits Integer; rounding used in formatted table columns.
#' @param ci_level Confidence level for leaf score intervals.
#' @param label_mapping Optional named list used for display labels.
#' @param include_plots Logical. Include plot components.
#' @param include_table Logical. Include the leaf table.
#' @param include_text Logical. Include standard interpretation text.
#' @param include_policy_value Logical. Include held-out value summaries when
#'   \code{policy_cv} is supplied.
#' @param layout List passed to
#'   \code{\link{margot_plot_policy_tree_panels}()}.
#' @param annotation List passed to
#'   \code{\link{margot_plot_policy_tree_panels}()}.
#' @param projection_args Optional list of arguments for the projection plot.
#' @param decision_tree_args Optional list of arguments for the decision tree.
#'
#' @return A list with \code{table}, \code{text}, \code{plots}, and
#'   \code{metadata}.
#' @export
margot_report_policy_tree <- function(result_object,
                                      model_name,
                                      policy_cv = NULL,
                                      depth = NULL,
                                      original_df = NULL,
                                      weights = NULL,
                                      digits = 3L,
                                      ci_level = 0.95,
                                      label_mapping = NULL,
                                      include_plots = TRUE,
                                      include_table = TRUE,
                                      include_text = TRUE,
                                      include_policy_value = !is.null(policy_cv),
                                      layout = list(heights = c(1, 2)),
                                      annotation = list(tag_levels = "A"),
                                      projection_args = list(),
                                      decision_tree_args = list()) {
  # assemble policy-tree artefacts while keeping each component inspectable.
  model_resolved <- .margot_leaf_resolve_model_name(result_object, model_name)
  report_depth <- .margot_policy_report_depth(policy_cv, model_resolved, depth)
  display_metrics <- if (isTRUE(include_table) || isTRUE(include_plots)) {
    tryCatch(
      margot_policy_leaf_summary(
        object = result_object,
        model_name = model_resolved,
        depth = report_depth,
        weights = weights,
        digits = digits,
        ci_level = ci_level,
        label_mapping = label_mapping
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  table <- if (isTRUE(include_table)) {
    margot_table_policy_tree(
      object = result_object,
      model_name = model_resolved,
      depth = report_depth,
      weights = weights,
      digits = digits,
      ci_level = ci_level,
      label_mapping = label_mapping,
      source = "display_tree"
    )
  } else {
    NULL
  }

  plot_decision_args <- .margot_policy_reporting_args(
    defaults = list(leaf_metrics = display_metrics, show_leaf_metrics = !is.null(display_metrics)),
    overrides = decision_tree_args
  )
  plots <- if (isTRUE(include_plots)) {
    margot_plot_policy_tree_panels(
      result_object = result_object,
      model_name = model_resolved,
      max_depth = report_depth,
      original_df = original_df,
      label_mapping = label_mapping,
      leaf_metrics = display_metrics,
      layout = layout,
      annotation = annotation,
      projection_args = projection_args,
      decision_tree_args = plot_decision_args
    )
  } else {
    NULL
  }

  text <- if (isTRUE(include_text)) {
    margot_text_policy_tree(source = "display_tree")
  } else {
    NULL
  }
  policy_value <- if (isTRUE(include_policy_value) && !is.null(policy_cv)) {
    margot_table_policy_value(policy_cv, model_name = model_resolved, depth = report_depth, digits = digits)
  } else {
    NULL
  }
  heldout_table <- if (!is.null(policy_cv)) {
    margot_table_policy_tree(policy_cv, model_name = model_resolved, depth = report_depth, source = "heldout_cv")
  } else {
    NULL
  }

  out <- list(
    table = table,
    heldout_table = heldout_table,
    policy_value = policy_value,
    text = text,
    plots = plots,
    metadata = list(
      model = model_resolved,
      depth = as.integer(report_depth),
      contrast = "treatment_minus_control",
      ci_level = ci_level
    )
  )
  class(out) <- c("margot_policy_tree_report", "list")
  out
}

#' @keywords internal
.margot_policy_report_depth <- function(policy_cv, model_name, depth = NULL) {
  # resolve report depth from the held-out CV depth map when available.
  if (!is.null(depth)) return(as.integer(depth[[1]]))
  if (inherits(policy_cv, "margot_policy_tree_cv") &&
      !is.null(policy_cv$depth_map) &&
      length(policy_cv$depth_map)) {
    candidates <- unique(c(model_name, gsub("^model_", "", model_name)))
    for (candidate in candidates) {
      selected <- policy_cv$depth_map[[candidate]]
      if (!is.null(selected) && length(selected)) return(as.integer(selected[[1]]))
    }
  }
  1L
}

#' Create a held-out policy-value reporting table
#'
#' @description
#' Summarises cross-validated policy-tree values against all-control,
#' all-treatment, and best-constant baselines.
#'
#' @param object A \code{margot_policy_tree_cv} object.
#' @param model_name Optional model name, with or without the \code{model_}
#'   prefix.
#' @param depth Optional tree depth. If \code{NULL}, all depths are returned.
#' @param digits Integer; rounding used in formatted columns.
#'
#' @return A tibble with held-out policy-value summaries.
#' @export
margot_table_policy_value <- function(object,
                                      model_name = NULL,
                                      depth = NULL,
                                      digits = 3L) {
  # report tree-level value against universal action baselines.
  if (!inherits(object, "margot_policy_tree_cv")) {
    stop("object must be a margot_policy_tree_cv object", call. = FALSE)
  }
  df <- object$value_summary
  if (is.null(df) || !nrow(df)) return(tibble::tibble())
  if (!is.null(model_name)) {
    candidates <- unique(c(as.character(model_name), paste0("model_", as.character(model_name))))
    df <- df[df$model %in% candidates, , drop = FALSE]
  }
  if (!is.null(depth)) df <- df[as.integer(df$depth) == as.integer(depth), , drop = FALSE]
  if (!nrow(df)) return(tibble::tibble())

  out <- tibble::tibble(
    model = df$model,
    outcome = df$outcome,
    outcome_label = df$outcome_label,
    depth = as.integer(df$depth),
    n_folds = df$n_folds,
    n_eval = df$n_eval,
    treatment_coverage = df$coverage_mean,
    value_policy = df$value_policy_mean,
    value_control_all = df$value_control_all_mean,
    value_treat_all = df$value_treat_all_mean,
    value_best_constant = df$value_best_constant_mean,
    best_constant_action = df$best_constant_action,
    gain_vs_control_all = df$gain_vs_control_mean,
    gain_vs_treat_all = df$gain_vs_treat_mean,
    gain_vs_best_constant = df$gain_vs_best_constant_mean,
    n_selected_actions = df$n_selected_actions_max,
    uniform_selected_action = df$uniform_selected_action_all,
    value_policy_label = .margot_policy_format_signed(df$value_policy_mean, digits),
    gain_vs_best_constant_label = .margot_policy_format_signed(df$gain_vs_best_constant_mean, digits)
  )
  attr(out, "source") <- "heldout_cv"
  out
}

#' @keywords internal
.margot_table_policy_tree_display <- function(object,
                                              model_name,
                                              depth,
                                              weights = NULL,
                                              digits = 3L,
                                              ci_level = 0.95,
                                              label_mapping = NULL,
                                              include_selected_action_difference = FALSE,
                                              include_value_contribution = FALSE,
                                              baseline = c("control_all", "treat_all")) {
  # turn fitted display-tree leaf summaries into a reporting table.
  baseline <- match.arg(baseline)
  if (is.null(model_name)) {
    stop("model_name is required for display-tree policy tables", call. = FALSE)
  }
  out <- margot_policy_leaf_summary(
    object = object,
    model_name = model_name,
    depth = depth,
    weights = weights,
    digits = digits,
    ci_level = ci_level,
    label_mapping = label_mapping
  )
  resolved_model <- attr(out, "model", exact = TRUE) %||% model_name
  out$model <- resolved_model
  out$outcome <- gsub("^model_", "", out$model)
  out$outcome_label <- .policy_cv_label(out$outcome, label_mapping)
  out$depth <- depth
  out$source <- "display_tree"
  out <- .margot_public_policy_leaf_table(
    out,
    source = "display_tree",
    digits = digits,
    interval_low = out$treatment_control_ci_low,
    interval_high = out$treatment_control_ci_high,
    include_selected_action_difference = include_selected_action_difference,
    include_value_contribution = include_value_contribution,
    baseline = baseline
  )
  attr(out, "model") <- resolved_model
  attr(out, "depth") <- depth
  attr(out, "ci_level") <- ci_level
  out
}

#' @keywords internal
.margot_public_policy_leaf_table <- function(df,
                                             source,
                                             digits = 3L,
                                             interval_low,
                                             interval_high,
                                             include_selected_action_difference = FALSE,
                                             include_value_contribution = FALSE,
                                             baseline = c("control_all", "treat_all")) {
  # expose the stable reporting schema and hide raw diagnostics by default.
  baseline <- match.arg(baseline)
  direction <- ifelse(
    is.finite(df$treatment_control_contrast) & df$treatment_control_contrast > 0,
    "favours treatment",
    ifelse(
      is.finite(df$treatment_control_contrast) & df$treatment_control_contrast < 0,
      "favours control",
      "no directional contrast"
    )
  )
  selected_action <- if ("action_label" %in% names(df)) df$action_label else df$action
  n <- if ("n" %in% names(df)) df$n else df$n_eval_leaf
  model <- if ("model" %in% names(df)) df$model else NA_character_
  outcome <- if ("outcome" %in% names(df)) df$outcome else NA_character_
  outcome_label <- if ("outcome_label" %in% names(df)) df$outcome_label else NA_character_
  action_counts <- .margot_policy_group_action_counts(
    source = source,
    model = model,
    depth = df$depth,
    selected_action = selected_action
  )
  public <- tibble::tibble(
    source = source,
    model = model,
    outcome_label = outcome_label,
    depth = as.integer(df$depth),
    node_id = as.integer(df$node_id),
    selected_action = selected_action,
    tc_score_contrast = df$treatment_control_contrast,
    score_interval = .margot_policy_format_ci(
      low = interval_low,
      high = interval_high,
      digits = digits
    ),
    sample_percent = .margot_policy_format_percent(df$sample_share),
    direction = direction,
    outcome = outcome,
    n = n,
    sample_share = df$sample_share,
    tc_score_contrast_label = .margot_policy_format_signed(df$treatment_control_contrast, digits),
    n_selected_actions = action_counts,
    uniform_selected_action = action_counts <= 1L,
    label = paste(
      selected_action,
      paste0("T-C: ", .margot_policy_format_signed(df$treatment_control_contrast, digits)),
      paste0("share: ", .margot_policy_format_percent(df$sample_share)),
      sep = "\n"
    )
  )
  if (isTRUE(include_selected_action_difference)) {
    public$selected_action_minus_alternative_score <- df$estimated_advantage
  }
  if (isTRUE(include_value_contribution)) {
    public$value_contribution <- if (identical(baseline, "control_all")) {
      df$value_contribution_vs_control
    } else {
      df$value_contribution_vs_treatment
    }
    public$value_contribution_baseline <- baseline
  }
  public
}

#' @keywords internal
.margot_policy_group_action_counts <- function(source, model, depth, selected_action) {
  # count selected actions within each reported source/model/depth group.
  if (!length(selected_action)) return(integer())
  group_id <- interaction(
    rep(source, length(selected_action)),
    model,
    as.integer(depth),
    drop = TRUE
  )
  out <- integer(length(selected_action))
  for (idx in split(seq_along(selected_action), group_id)) {
    out[idx] <- length(unique(stats::na.omit(selected_action[idx])))
  }
  out
}

#' @keywords internal
.margot_table_policy_tree_cv <- function(object,
                                         model_name = NULL,
                                         depth = NULL,
                                         digits = 3L,
                                         include_selected_action_difference = FALSE,
                                         include_value_contribution = FALSE,
                                         baseline = c("control_all", "treat_all")) {
  # standardise held-out CV action-score summaries as a reporting table.
  baseline <- match.arg(baseline)
  if (!inherits(object, "margot_policy_tree_cv")) {
    stop("object must be a margot_policy_tree_cv object for heldout_cv tables", call. = FALSE)
  }
  if (is.null(object$leaf_summary) || !nrow(object$leaf_summary)) {
    return(tibble::tibble())
  }
  df <- object$leaf_summary
  if (!is.null(model_name)) {
    candidates <- unique(c(as.character(model_name), paste0("model_", as.character(model_name))))
    df <- df[df$model %in% candidates, , drop = FALSE]
  }
  if (is.null(depth) && !is.null(object$depth_map) && length(object$depth_map)) {
    keep <- mapply(
      function(model, depth_value) {
        selected <- object$depth_map[[model]]
        !is.null(selected) && as.integer(depth_value) == as.integer(selected)
      },
      df$model,
      df$depth
    )
    df <- df[keep, , drop = FALSE]
  } else if (!is.null(depth)) {
    df <- df[as.integer(df$depth) == as.integer(depth), , drop = FALSE]
  }
  if (!nrow(df)) return(tibble::tibble())

  raw <- tibble::tibble(
    model = df$model,
    outcome = df$outcome,
    outcome_label = df$outcome_label,
    depth = as.integer(df$depth),
    node_id = NA_integer_,
    action = df$action,
    action_label = df$action_label,
    n_eval_leaf = df$n_eval_leaf,
    sample_share = df$sample_share_mean,
    treatment_control_contrast = df$treatment_control_contrast_mean,
    estimated_advantage = df$estimated_advantage_mean,
    value_contribution_vs_control = df$value_contribution_vs_control_mean,
    value_contribution_vs_treatment = df$value_contribution_vs_treatment_mean
  )
  out <- .margot_public_policy_leaf_table(
    raw,
    source = "heldout_cv",
    digits = digits,
    interval_low = df$treatment_control_contrast_q025,
    interval_high = df$treatment_control_contrast_q975,
    include_selected_action_difference = include_selected_action_difference,
    include_value_contribution = include_value_contribution,
    baseline = baseline
  )
  attr(out, "source") <- "heldout_cv"
  out
}

#' @keywords internal
.margot_policy_reporting_args <- function(defaults, overrides) {
  # merge caller overrides without duplicating formal arguments.
  if (is.null(overrides)) overrides <- list()
  defaults[names(overrides)] <- NULL
  c(defaults, overrides)
}

#' @keywords internal
.margot_policy_format_signed <- function(x, digits = 3L) {
  # format signed numeric contrasts for policy tables.
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- sprintf(paste0("%+.", digits, "f"), x[ok])
  out[!ok] <- "NA"
  out
}

#' @keywords internal
.margot_policy_format_ci <- function(low, high, digits = 3L) {
  # format lower and upper interval endpoints compactly.
  low_label <- .margot_policy_format_signed(low, digits)
  high_label <- .margot_policy_format_signed(high, digits)
  ifelse(low_label == "NA" | high_label == "NA", NA_character_, paste0("[", low_label, ", ", high_label, "]"))
}

#' @keywords internal
.margot_policy_format_percent <- function(x, digits = 1L) {
  # format proportions as percentages for reporting tables.
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- sprintf(paste0("%.", digits, "f%%"), 100 * x[ok])
  out[!ok] <- "NA"
  out
}
