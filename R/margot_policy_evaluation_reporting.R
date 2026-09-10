# validate display weights against the complete reference rows.
.margot_policy_display_weights <- function(weights, n, max_size = 4) {
  if (!is.numeric(weights) || length(weights) != n || !n ||
      any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0) || !is.finite(sum(weights))) {
    stop("display_weights must be finite non-negative weights for every reference row, with positive total weight.", call. = FALSE)
  }
  if (!is.numeric(max_size) || length(max_size) != 1L || !is.finite(max_size) || max_size <= 0) {
    stop("weight_max_size must be a positive finite number.", call. = FALSE)
  }
  invisible(weights)
}

# display a constant assignment without inventing a predictor threshold.
.margot_policy_constant_projection <- function(tree, reference, weights, max_size, alpha, seed, theme_function) {
  d <- data.frame(x = 1, y = rep(0, nrow(reference)))
  mapping <- ggplot2::aes(x = .data$x, y = .data$y)
  args <- list(size = 1.5)
  size_scale <- NULL
  if (!is.null(weights)) {
    d$display_weight <- weights
    mapping$size <- ggplot2::aes(size = .data$display_weight)$size
    args <- list(shape = 16)
    size_scale <- ggplot2::scale_size_area(max_size = max_size, limits = c(0, max(weights)), name = "Weight")
  }
  ggplot2::ggplot(d, mapping) + do.call(ggplot2::geom_point, c(list(alpha = alpha,
    position = ggplot2::position_jitter(width = 0, height = .06, seed = seed)), args)) +
    size_scale + ggplot2::scale_x_continuous(breaks = 1, labels = "All reference records") +
    ggplot2::labs(x = NULL, y = NULL, subtitle = paste("Constant assignment:", tree$action.names[tree$nodes[[1]]$action])) +
    theme_function() + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
}

# require explicit scalar identities and descriptions in stored reporting metadata.
.margot_policy_context <- function(context) {
  required <- c("outcome", "outcome_label", "rule_id", "population_id", "population_label",
    "scale_id", "scale_label", "orientation", "weight_id", "evaluation_mode", "contrast_label", "qualification")
  if (!is.list(context)) stop("context must be a named list.", call. = FALSE)
  for (field in required) {
    x <- context[[field]]
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop("context requires a non-empty character scalar: ", field, call. = FALSE)
    }
  }
  modes <- c("independent_fixed_rule", "selected_full_sample", "repeated_learning", "constructed")
  if (!context$evaluation_mode %in% modes) stop("Unsupported evaluation_mode.", call. = FALSE)
  if (!context$orientation %in% c("as_scored", "reversed")) stop("orientation must be as_scored or reversed; supplied estimates are already oriented.", call. = FALSE)
  if (context$evaluation_mode == "independent_fixed_rule") {
    for (field in c("development_id", "evaluation_id")) {
      if (!is.character(context[[field]]) || length(context[[field]]) != 1 ||
          is.na(context[[field]]) || !nzchar(context[[field]])) stop("Independent evaluation requires ", field, call. = FALSE)
    }
    if (identical(context$development_id, context$evaluation_id)) stop("Development and evaluation identities must differ.", call. = FALSE)
  }
  context
}

# validate supplied intervals without deriving uncertainty from estimates or repeats.
.margot_policy_interval_table <- function(d, context) {
  required <- c("estimate", "lower", "upper", "interval_type", "interval_level", "interval_method", "unavailable_reason")
  if (!is.data.frame(d) || !nrow(d) || !all(required %in% names(d))) {
    stop("Stored tables require rows and columns: ", paste(required, collapse = ", "), call. = FALSE)
  }
  for (field in c("estimate", "lower", "upper", "interval_level")) {
    if (!is.numeric(d[[field]])) stop(field, " must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(d$estimate))) stop("Stored estimates must be finite.", call. = FALSE)
  available <- !is.na(d$lower) & !is.na(d$upper)
  if (any(xor(is.na(d$lower), is.na(d$upper)))) stop("Supply both interval endpoints or neither.", call. = FALSE)
  if (any(!is.finite(d$lower[available]) | !is.finite(d$upper[available]) | d$lower[available] > d$upper[available])) stop("Invalid interval endpoints.", call. = FALSE)
  for (field in c("interval_type", "interval_method", "unavailable_reason")) {
    if (!is.character(d[[field]])) stop(field, " must be character.", call. = FALSE)
  }
  allowed <- switch(context$evaluation_mode,
    selected_full_sample = "nominal_fixed_leaves",
    constructed = "constructed",
    c("pointwise", "simultaneous"))
  if (any(is.na(d$interval_type)) || any(!d$interval_type[available] %in% allowed)) stop("Interval type is incompatible with evaluation_mode.", call. = FALSE)
  if (any(!is.finite(d$interval_level[available]) | d$interval_level[available] <= 0 | d$interval_level[available] >= 1)) stop("Available intervals require a level between zero and one.", call. = FALSE)
  if (any(is.na(d$interval_method[available]) | !nzchar(trimws(d$interval_method[available])))) stop("Available intervals require interval_method.", call. = FALSE)
  if (any(d$interval_type[!available] != "unavailable") ||
      any(is.na(d$unavailable_reason[!available]) | !nzchar(trimws(d$unavailable_reason[!available])))) stop("Missing intervals require type unavailable and an unavailable_reason.", call. = FALSE)
  d
}

# honour explicit action labels before the package's conventional defaults.
.margot_policy_reporting_action_label <- function(action, label_mapping = NULL) {
  if (!is.null(label_mapping[[action]])) return(as.character(label_mapping[[action]]))
  .margot_leaf_label_action(action, label_mapping)
}

# bind rule structure independently of fitted-object environments and timing attributes.
.margot_policy_rule_signature <- function(tree) {
  digest::digest(list(nodes = tree$nodes, columns = tree$columns, actions = tree$action.names), algo = "sha256")
}

#' Bind stored policy estimates to a rule and reporting context
#'
#' @description
#' Validates supplied leaf contrasts and a rule-minus-comparator value difference. Computes only descriptive reference counts and weighted shares when reference rows are supplied. Metadata records caller-supplied provenance. Validation checks internal compatibility. Scientific identification and interval coverage require independent justification.
#'
#' @param tree The stored policy tree whose terminal node identifiers occur in `leaves`.
#' @param leaves Data frame with `node_id`, unique `leaf_label`, `estimate`, `lower`, `upper`, `interval_type`, `interval_level`, `interval_method` and `unavailable_reason`. Estimates are already on the declared scale and orientation. Every terminal node must appear exactly once. Use numeric `NA` for unavailable endpoints.
#' @param value One-row data frame with the same estimate and interval fields, plus `comparator_id`, `comparator_label` and finite non-negative `gain_margin`. Supply the resolved analysis margin.
#' @param context Named list of character scalars: `outcome`, `outcome_label`, `rule_id`, `population_id`, `population_label`, `scale_id`, `scale_label`, `orientation` (`as_scored` or `reversed`), `weight_id`, `evaluation_mode`, `contrast_label` and `qualification`. Evaluation modes are `independent_fixed_rule`, `selected_full_sample`, `repeated_learning` and `constructed`. Independent evaluation additionally requires distinct `development_id` and `evaluation_id`. The qualification states the inferential limitations, including any multiplicity adjustment.
#' @param value_context Context for D; defaults to `context`. A separate rule identity is allowed only for explicitly labelled `selected_full_sample` leaves with `repeated_learning` value. Outcome, population, scale, orientation and weight identities must agree.
#' @param reference Optional complete prediction data frame for A/B. Its rows define the display population, which may differ from the evaluation population. Only tree columns are retained. Supply unique participant rows verified using participant identifiers.
#' @param display_weights Optional weights aligned with reference rows. `NULL` means equal display weights. A zero-weight record contributes to the unweighted count and has zero weight in the share calculation.
#' @param reference_label Character scalar describing the display population; required with `reference`.
#' @param display_weight_id Character scalar identifying the display weights; required with `reference`.
#'
#' @details
#' Available interval types are `pointwise` or `simultaneous` for independent rules and repeated procedures, `nominal_fixed_leaves` for selected full-sample results, and `constructed` for illustrative fixtures. A nominal fixed-leaf interval ignores selection. An unavailable interval requires type `unavailable` and a reason. Repeated-fold quantiles describe partition variability. Sampling intervals require a method that accounts for participant reuse. The supplied method and qualification appear in plots and text. Reversal metadata labels an already reversed estimate. The supplied numbers are preserved.
#'
#' @return A `margot_policy_reporting_data` list containing unrounded tables, contexts, rule signature and optional reference rows and weights. New plotting and text functions revalidate this object before use.
#' @md
#' @export
margot_policy_reporting_data <- function(tree, leaves, value, context, value_context = context,
                                         reference = NULL, display_weights = NULL,
                                         reference_label = NULL, display_weight_id = NULL) {
  # bind supplied scientific summaries to a stored rule and optional display rows.
  context <- .margot_policy_context(context)
  value_context <- .margot_policy_context(value_context)
  common <- c("outcome", "outcome_label", "population_id", "population_label", "scale_id", "scale_label", "orientation", "weight_id")
  for (field in common) {
    if (!identical(context[[field]], value_context[[field]])) stop("Incompatible ", field, " between leaves and value.", call. = FALSE)
  }
  mixed <- context$evaluation_mode == "selected_full_sample" && value_context$evaluation_mode == "repeated_learning"
  if (!mixed) {
    for (field in c("rule_id", "evaluation_mode", "development_id", "evaluation_id")) {
      if (!identical(context[[field]], value_context[[field]])) stop("Incompatible ", field, " between leaves and value.", call. = FALSE)
    }
  }
  if (context$evaluation_mode == "repeated_learning") stop("A single displayed tree cannot represent repeated-learning leaves.", call. = FALSE)
  leaves <- .margot_policy_interval_table(leaves, context)
  value <- .margot_policy_interval_table(value, value_context)
  if (is.null(tree$nodes) || !length(tree$nodes) || !length(tree$columns) || !length(tree$action.names)) stop("tree must contain nodes, columns and action names.", call. = FALSE)
  terminal <- which(vapply(tree$nodes, function(node) isTRUE(node$is_leaf), logical(1)))
  if (!is.numeric(leaves$node_id) || anyNA(leaves$node_id) || anyDuplicated(leaves$node_id) || !setequal(leaves$node_id, terminal)) stop("leaves must identify every terminal node exactly once.", call. = FALSE)
  if (!is.character(leaves$leaf_label) || anyNA(leaves$leaf_label) || any(!nzchar(leaves$leaf_label)) || anyDuplicated(leaves$leaf_label)) stop("leaf_label must be unique non-empty text.", call. = FALSE)
  if (nrow(value) != 1L) stop("value must have exactly one row.", call. = FALSE)
  for (field in c("comparator_id", "comparator_label")) {
    if (!is.character(value[[field]]) || length(value[[field]]) != 1L || is.na(value[[field]]) || !nzchar(value[[field]])) stop("value requires ", field, call. = FALSE)
  }
  if (!is.numeric(value$gain_margin) || length(value$gain_margin) != 1L || !is.finite(value$gain_margin) || value$gain_margin < 0) stop("value requires the resolved non-negative gain_margin.", call. = FALSE)
  leaves <- leaves[match(terminal, leaves$node_id), , drop = FALSE]
  action <- vapply(tree$nodes[terminal], function(node) as.integer(node$action), integer(1))
  if (anyNA(action) || any(action < 1 | action > length(tree$action.names))) stop("Invalid stored terminal action.", call. = FALSE)
  leaves$selected_action <- tree$action.names[action]
  if (!is.null(reference)) {
    if (!is.data.frame(reference) && !is.matrix(reference)) stop("reference must be a data frame or matrix.", call. = FALSE)
    reference <- as.data.frame(reference)[, tree$columns, drop = FALSE]
    for (label in list(reference_label, display_weight_id)) {
      if (!is.character(label) || length(label) != 1 || is.na(label) || !nzchar(label)) stop("Reference rows require reference_label and display_weight_id.", call. = FALSE)
    }
    if (is.null(display_weights)) display_weights <- rep(1, nrow(reference))
    .margot_policy_display_weights(display_weights, nrow(reference))
    ids <- .margot_policy_tree_leaf_ids(tree, reference)
    if (anyNA(ids)) stop("Every reference row must have a finite leaf assignment.", call. = FALSE)
    leaves$n_reference <- vapply(terminal, function(id) sum(ids == id), integer(1))
    leaves$reference_share <- vapply(terminal, function(id) sum(display_weights[ids == id]) / sum(display_weights), numeric(1))
  } else if (!is.null(display_weights)) stop("display_weights require reference rows.", call. = FALSE)
  structure(list(tree = tree, rule_signature = .margot_policy_rule_signature(tree),
    leaves = tibble::as_tibble(leaves), value = tibble::as_tibble(value), context = context,
    value_context = value_context, mixed_scope = mixed, reference = reference,
    display_signature = digest::digest(list(reference, display_weights), algo = "sha256"),
    display_weights = display_weights, reference_label = reference_label,
    display_weight_id = display_weight_id), class = "margot_policy_reporting_data")
}

# revalidate a saved reporting object before consuming its tables.
.margot_policy_reporting_validate <- function(x) {
  if (!inherits(x, "margot_policy_reporting_data")) stop("data must be margot_policy_reporting_data.", call. = FALSE)
  if (!identical(x$rule_signature, .margot_policy_rule_signature(x$tree))) stop("Stored rule signature has changed.", call. = FALSE)
  if (!identical(x$display_signature, digest::digest(list(x$reference, x$display_weights), algo = "sha256"))) stop("Stored display rows or weights have changed; rebuild the reporting object with explicit provenance.", call. = FALSE)
  margot_policy_reporting_data(x$tree, x$leaves, x$value, x$context, x$value_context,
    x$reference, x$display_weights, x$reference_label, x$display_weight_id)
}

# wrap captions within a half-width report panel while retaining explicit line breaks.
.margot_policy_wrap <- function(text, width = 64) {
  paste(vapply(strsplit(paste(text, collapse = "\n"), "\n")[[1]],
    function(line) paste(strwrap(line, width = width), collapse = "\n"), character(1)), collapse = "\n")
}

# format stored numbers only at the presentation boundary.
.margot_policy_number <- function(x, digits) {
  if (length(digits) != 1 || !is.numeric(digits) || !is.finite(digits) || digits != as.integer(digits) || digits < 0 || digits > 10) stop("digits must be an integer from 0 to 10.", call. = FALSE)
  formatC(x, format = "f", digits = digits)
}

# retain interval method and inferential status in a shared display string.
.margot_policy_interval_label <- function(d, digits) {
  vapply(seq_len(nrow(d)), function(i) {
    if (d$interval_type[i] == "unavailable") return(paste0("Interval unavailable: ", d$unavailable_reason[i]))
    type <- switch(d$interval_type[i], nominal_fixed_leaves = "nominal interval (ignores leaf selection)",
      pointwise = "pointwise interval", simultaneous = "simultaneous interval", constructed = "constructed interval")
    paste0(format(100 * d$interval_level[i], trim = TRUE), "% ", type, " [",
      .margot_policy_number(d$lower[i], digits), ", ", .margot_policy_number(d$upper[i], digits), "]; ", d$interval_method[i])
  }, character(1))
}

# provide concise scope labels shared by plots and interpretations.
.margot_policy_scope <- function(context) {
  switch(context$evaluation_mode, independent_fixed_rule = "Independently evaluated fixed rule",
    selected_full_sample = "Selected full-sample rule", repeated_learning = "Repeated-learning procedure",
    constructed = "Constructed illustration")
}

#' Plot stored contrasts within policy-tree leaves
#'
#' @param data A validated object from [margot_policy_reporting_data()].
#' @param digits Decimal places for displayed numbers; raw tables retain full precision.
#' @param title Optional plot title.
#' @return A ggplot. Its `data` contains the unrounded stored table and formatted labels.
#' @md
#' @export
margot_plot_policy_leaf_effects <- function(data, digits = 3L, title = "Contrasts within policy leaves") {
  # draw supplied leaf contrasts and compatible intervals without score estimation.
  x <- .margot_policy_reporting_validate(data)
  d <- x$leaves
  d$display_label <- paste0(d$leaf_label, "\n", .margot_policy_number(d$estimate, digits), "; ", .margot_policy_interval_label(d, digits))
  d$position <- rev(seq_len(nrow(d)))
  available <- d$interval_type != "unavailable"
  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$position)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_segment(data = d[available, ], ggplot2::aes(x = .data$lower, xend = .data$upper, yend = .data$position), linewidth = .8) +
    ggplot2::geom_point(size = 3, colour = "#28658b") +
    ggplot2::scale_y_continuous(breaks = d$position, labels = d$leaf_label) +
    ggplot2::labs(title = .margot_policy_wrap(title, 40), subtitle = .margot_policy_wrap(c(x$context$outcome_label, .margot_policy_scope(x$context)), 55),
      x = paste0(x$context$contrast_label, " (", x$context$scale_label, ")"), y = NULL,
      caption = .margot_policy_wrap(c(paste(d$leaf_label, .margot_policy_interval_label(d, digits), sep = ": "), x$context$qualification))) +
    ggplot2::theme_minimal() + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0), panel.grid.major.y = ggplot2::element_blank())
}

#' Plot a stored policy-value gain and practical margin
#'
#' @inheritParams margot_plot_policy_leaf_effects
#' @return A ggplot containing the stored value table. The dashed line marks the resolved practical gain margin. Intervention costs and confidence bounds are distinct quantities.
#' @md
#' @export
margot_plot_policy_value_gain <- function(data, digits = 3L, title = "Gain over constant assignment") {
  # draw the supplied paired value difference and its resolved analysis margin.
  x <- .margot_policy_reporting_validate(data)
  d <- x$value
  d$position <- 1
  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$position)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = d$gain_margin, linetype = "dashed", colour = "#263747") +
    ggplot2::geom_segment(data = d[d$interval_type != "unavailable", ],
      ggplot2::aes(x = .data$lower, xend = .data$upper, yend = .data$position), linewidth = .8) +
    ggplot2::geom_point(size = 3, shape = 18, colour = "#263747") +
    ggplot2::scale_y_continuous(breaks = NULL, limits = c(.5, 1.5)) +
    ggplot2::labs(title = .margot_policy_wrap(title, 40), subtitle = .margot_policy_wrap(c(x$context$outcome_label, .margot_policy_scope(x$value_context), d$comparator_label), 55),
      x = paste0("Rule minus comparator (", x$context$scale_label, ")"), y = NULL,
      caption = .margot_policy_wrap(paste0("Gain: ", .margot_policy_number(d$estimate, digits), ". ", .margot_policy_interval_label(d, digits),
        "\nDashed line: margin ", .margot_policy_number(d$gain_margin, digits), " ", x$context$scale_label, ".\n", x$value_context$qualification))) +
    ggplot2::theme_minimal() + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0), panel.grid.major.y = ggplot2::element_blank())
}

#' Describe stored contrasts within policy-tree leaves
#'
#' @inheritParams margot_plot_policy_leaf_effects
#' @return A character vector: scope, one statement per leaf, and the supplied inferential qualification. A difference between leaf effects requires a direct contrast and its uncertainty.
#' @md
#' @export
margot_text_policy_leaf_effects <- function(data, digits = 3L) {
  # narrate the same stored leaf quantities consumed by the plot.
  x <- .margot_policy_reporting_validate(data)
  d <- x$leaves
  counts <- if (!is.null(x$reference)) paste0(" Reference records: ", d$n_reference, "; weighted share ",
    .margot_policy_number(100 * d$reference_share, 1), "%.") else ""
  c(paste0(x$context$outcome_label, ": ", .margot_policy_scope(x$context), "; ", x$context$population_label, "."),
    paste0(d$leaf_label, ": ", x$context$contrast_label, " = ", .margot_policy_number(d$estimate, digits), " ",
      x$context$scale_label, "; ", .margot_policy_interval_label(d, digits), ".", counts),
    if (!is.null(x$reference)) paste0("Display population: ", x$reference_label, "; display weights: ", x$display_weight_id, "."),
    x$context$qualification,
    "A difference between leaf effects requires a direct contrast and its uncertainty. Splitting variables describe the assignment rule; their own causal effects require a separate analysis.")
}

#' Describe a stored policy-value gain and its practical margin
#'
#' @inheritParams margot_plot_policy_leaf_effects
#' @return A character vector distinguishing the value estimate, supplied interval and practical margin.
#' @md
#' @export
margot_text_policy_value_gain <- function(data, digits = 3L) {
  # compare unrounded values with the stored margin before formatting prose.
  x <- .margot_policy_reporting_validate(data)
  d <- x$value
  comparison <- if (d$estimate > d$gain_margin) "exceeds" else if (d$estimate == d$gain_margin) "equals" else "is below"
  uncertainty <- if (d$interval_type == "unavailable") {
    paste0("Interval unavailable: ", d$unavailable_reason, ".")
  } else {
    statements <- c(paste0(.margot_policy_interval_label(d, digits), "."))
    if (d$lower <= 0 && d$upper >= 0) statements <- c(statements, "The interval includes zero.")
    if (d$lower < d$gain_margin) statements <- c(statements, "The interval includes gains below the practical margin.")
    if (d$lower > d$gain_margin) statements <- c(statements, "The interval lies above the practical margin.")
    statements
  }
  c(paste0(x$context$outcome_label, ": ", .margot_policy_scope(x$value_context), "; ", x$value_context$population_label, "."),
    paste0("Relative to ", d$comparator_label, ", the stored gain is ", .margot_policy_number(d$estimate, digits), " ",
      x$context$scale_label, ". The estimate ", comparison, " the practical margin of ", .margot_policy_number(d$gain_margin, digits), " ", x$context$scale_label, "."),
    uncertainty, x$value_context$qualification)
}

# assemble a stored report using the maintained combo and explicit reference identity.
.margot_report_stored_policy <- function(result_object, model_name, data, depth, original_df,
                                         digits, label_mapping, include_plots, include_table,
                                         include_text, projection_args, decision_tree_args,
                                         heights, annotation) {
  x <- .margot_policy_reporting_validate(data)
  model <- .margot_leaf_resolve_model_name(result_object, model_name)
  if (!identical(sub("^model_", "", model), sub("^model_", "", x$context$outcome))) stop("Reporting outcome does not match model_name.", call. = FALSE)
  if (is.null(depth)) {
    candidates <- which(vapply(1:2, function(d) {
      tree <- result_object$results[[model]][[paste0("policy_tree_depth_", d)]]
      !is.null(tree) && identical(.margot_policy_rule_signature(tree), x$rule_signature)
    }, logical(1)))
    if (length(candidates) != 1) stop("Supply depth to identify the stored rule slot unambiguously.", call. = FALSE)
    depth <- candidates
  }
  if (length(depth) != 1 || !depth %in% 1:2) stop("depth must be 1 or 2.", call. = FALSE)
  tree <- result_object$results[[model]][[paste0("policy_tree_depth_", depth)]]
  if (is.null(tree) || !identical(.margot_policy_rule_signature(tree), x$rule_signature)) stop("Reporting rule does not match the plotted tree.", call. = FALSE)
  plots <- NULL
  if (isTRUE(include_plots)) {
    if (is.null(x$reference)) stop("The combined report requires complete reference rows and display weights.", call. = FALSE)
    actual <- .policy_tree_build_predict_df(result_object$results[[model]]$plot_data, tree$columns)
    if (!identical(as.data.frame(actual), as.data.frame(x$reference))) stop("Projection rows differ from the reporting reference rows or their order.", call. = FALSE)
    if (!is.numeric(heights) || length(heights) != 3 || any(!is.finite(heights) | heights <= 0)) stop("reporting_heights must contain three positive numbers.", call. = FALSE)
    forbidden <- intersect(names(projection_args), c("display_weights", "jitter_width", "jitter_height", "jitter_method", "plot_selection"))
    if (length(forbidden)) stop("Stored reports control projection weights, exact coordinates and complete branch display; remove: ", paste(forbidden, collapse = ", "), call. = FALSE)
    if (any(c("leaf_metrics", "show_leaf_metrics") %in% names(decision_tree_args))) stop("Stored reports supply their own leaf metrics.", call. = FALSE)
    leaves <- x$leaves
    actions <- vapply(leaves$selected_action, .margot_policy_reporting_action_label, character(1), label_mapping = label_mapping)
    metrics <- data.frame(node_id = leaves$node_id, label = paste0(leaves$leaf_label, ": ", actions,
      "\n", .margot_policy_number(100 * leaves$reference_share, 1), "% weighted\nn = ", leaves$n_reference))
    args <- .margot_policy_reporting_args(list(display_weights = x$display_weights,
      jitter_width = 0, jitter_height = if (depth == 1) .06 else 0, jitter_seed = 20260910), projection_args)
    panels <- margot_plot_policy_tree_panels(result_object, model, max_depth = depth,
      original_df = original_df, label_mapping = label_mapping, leaf_metrics = metrics,
      projection_args = args, decision_tree_args = decision_tree_args)
    heading <- ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.tag = ggplot2::element_text(face = "bold", hjust = 0), plot.caption = ggplot2::element_text(hjust = 0))
    panels$decision_tree$coordinates$clip <- "off"
    a <- panels$decision_tree + ggplot2::labs(title = x$context$outcome_label, subtitle = "Decision tree") + heading
    b <- panels$projection
    if (inherits(b, "patchwork")) {
      b <- b + patchwork::plot_annotation(title = NULL)
      b <- patchwork::wrap_elements(panel = b)
    }
    b <- b + ggplot2::labs(title = x$context$outcome_label,
      subtitle = paste0("Weighted projection: ", x$reference_label, "\nPoint area represents ", x$display_weight_id)) + heading
    c <- margot_plot_policy_leaf_effects(x, digits)
    d <- margot_plot_policy_value_gain(x, digits)
    caption <- if (x$mixed_scope) "A-C describe selected full-sample leaves; D evaluates the repeated-learning procedure." else "A-D refer to the same stored rule."
    combined <- patchwork::wrap_plots(a, b, patchwork::wrap_plots(c, d, nrow = 1), ncol = 1, heights = heights) +
      patchwork::plot_annotation(tag_levels = annotation$tag_levels, caption = caption) & heading
    plots <- list(decision_tree = a, projection = b, leaf_effects = c, value_gain = d, combined_plot = combined)
  }
  structure(list(table = if (include_table) x$leaves else NULL,
    policy_value = if (include_table) x$value else NULL,
    text = if (include_text) list(leaves = margot_text_policy_leaf_effects(x, digits), value = margot_text_policy_value_gain(x, digits)) else NULL,
    plots = plots, reporting_data = x, metadata = list(context = x$context, value_context = x$value_context,
      depth = depth, rule_signature = x$rule_signature, mixed_scope = x$mixed_scope)), class = c("margot_policy_tree_report", "list"))
}
