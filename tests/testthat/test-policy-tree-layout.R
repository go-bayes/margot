# construct explicit synthetic rules so boundary and topology checks are exact.
policy_layout_fixture <- function(kind = c("stump", "depth_two", "pruned", "constant")) {
  kind <- match.arg(kind)
  leaf <- function(action) list(is_leaf = TRUE, action = as.integer(action))
  split <- function(variable, value, left, right) {
    list(is_leaf = FALSE, split_variable = variable, split_value = value,
      left_child = as.integer(left), right_child = as.integer(right))
  }
  nodes <- switch(kind,
    stump = list(split(1, 0.123456, 2, 3), leaf(2), leaf(1)),
    depth_two = list(split(1, 0.123456, 2, 5), split(2, 2.5, 3, 4),
      leaf(2), leaf(1), split(2, 4.5, 6, 7), leaf(1), leaf(2)),
    pruned = list(split(1, 0.123456, 2, 3), leaf(2),
      split(2, 4.5, 4, 5), leaf(1), leaf(2)),
    constant = list(leaf(2)))
  structure(list(nodes = nodes, depth = if (kind %in% c("depth_two", "pruned")) 2L else if (kind == "constant") 0L else 1L,
    columns = c("hours_z", "second"), action.names = c("control", "treated"),
    n.actions = 2L, n.features = 2L), class = "policy_tree")
}

test_that("native policy trees draw without a causal-forest results wrapper", {
  for (kind in c("constant", "stump", "depth_two", "pruned")) {
    tree <- policy_layout_fixture(kind)
    before <- serialize(tree, NULL)
    p <- margot_plot_decision_tree(tree)
    expect_s3_class(p, "ggplot")
    expect_identical(p$labels$title, "Policy tree", info = kind)
    expect_equal(nrow(p$layers[[2]]$data), length(tree$nodes), info = kind)
    expect_equal(nrow(p$layers[[1]]$data), length(tree$nodes) - 1L, info = kind)
    expect_silent(ggplot2::ggplotGrob(p))
    expect_identical(serialize(tree, NULL), before, info = kind)
  }
})

test_that("legacy branch labels remain the default and named labels follow sides", {
  tree <- policy_layout_fixture("depth_two")
  p <- margot_plot_decision_tree(tree)
  edges <- p$layers[[3]]$data
  expect_identical(edges$edge_lab, ifelse(edges$side == "left", "True", "False"))
  p <- margot_plot_decision_tree(tree, branch_labels = c(right = "Above", left = "At or below"))
  edges <- p$layers[[3]]$data
  expect_identical(edges$edge_lab, ifelse(edges$side == "left", "At or below", "Above"))
  expect_error(margot_plot_decision_tree(tree, branch_labels = c(left = "Yes")))
  expect_error(margot_plot_decision_tree(tree, branch_labels = c(left = "Yes", other = "No")))
  expect_error(margot_plot_decision_tree(tree, branch_labels = c(left = NA_character_, right = "No")))
})

test_that("condition labels share threshold formatting and expose original values", {
  tree <- policy_layout_fixture()
  original <- data.frame(hours = c(10, 20, 30, 40))
  p <- margot_plot_decision_tree(tree, original_df = original, branch_labels = "condition")
  edges <- p$layers[[3]]$data
  nodes <- p$layers[[2]]$data
  expect_equal(edges$threshold, rep(tree$nodes[[1]]$split_value, 2))
  expect_equal(edges$original_threshold,
    rep(get_original_value_plot("hours_z", tree$nodes[[1]]$split_value, original), 2))
  expect_identical(edges$variable, rep("hours_z", 2))
  expect_true(all(vapply(seq_len(nrow(edges)), function(i) {
    grepl(edges$threshold_label[i], edges$edge_lab[i], fixed = TRUE) &&
      grepl(edges$threshold_label[i], nodes$label[nodes$id == edges$parent_id[i]], fixed = TRUE)
  }, logical(1))))
  expect_true(grepl("<=|\u2264", edges$edge_lab[edges$side == "left"]))
  expect_true(grepl(">", edges$edge_lab[edges$side == "right"], fixed = TRUE))
})

test_that("edge-specific label tables require exactly the plotted topology", {
  tree <- policy_layout_fixture("pruned")
  edges <- margot_plot_decision_tree(tree)$layers[[3]]$data
  labels <- data.frame(parent_id = edges$parent_id, side = edges$side,
    label = paste("Branch", seq_len(nrow(edges))))
  shuffled <- labels[rev(seq_len(nrow(labels))), ]
  p <- margot_plot_decision_tree(tree, branch_labels = shuffled)
  expect_identical(p$layers[[3]]$data$edge_lab, labels$label)
  expect_error(margot_plot_decision_tree(tree, branch_labels = labels[-1, ]))
  expect_error(margot_plot_decision_tree(tree, branch_labels = rbind(labels, labels[1, ])))
  unknown <- labels
  unknown$parent_id[1] <- 999L
  expect_error(margot_plot_decision_tree(tree, branch_labels = unknown))
  unknown <- labels
  unknown$side[1] <- "middle"
  expect_error(margot_plot_decision_tree(tree, branch_labels = unknown))
})

test_that("branch callbacks receive topology and require one text label per edge", {
  tree <- policy_layout_fixture("depth_two")
  captured <- NULL
  p <- margot_plot_decision_tree(tree, branch_labels = function(edges) {
    captured <<- edges
    paste(edges$variable, edges$side, edges$threshold_label)
  })
  expect_true(all(c("parent_id", "child_id", "side", "variable", "threshold",
    "original_threshold", "threshold_label") %in% names(captured)))
  expect_identical(p$layers[[3]]$data$edge_lab,
    paste(captured$variable, captured$side, captured$threshold_label))
  expect_error(margot_plot_decision_tree(tree, branch_labels = function(edges) "one label"))
  expect_error(margot_plot_decision_tree(tree, branch_labels = function(edges) seq_len(nrow(edges))))
  expect_error(margot_plot_decision_tree(tree, branch_labels = function(edges) rep(NA_character_, nrow(edges))))
})

test_that("compact layout handles pruned rules and preserves numerical tree content", {
  for (kind in c("constant", "stump", "depth_two", "pruned")) {
    tree <- policy_layout_fixture(kind)
    before <- serialize(tree, NULL)
    legacy <- margot_plot_decision_tree(tree, layout_style = "legacy")
    compact <- margot_plot_decision_tree(tree, layout_style = "compact")
    expect_true(is.finite(legacy$coordinates$ratio), info = kind)
    expect_s3_class(compact$coordinates, "CoordCartesian")
    expect_null(compact$coordinates$ratio)
    nodes <- compact$layers[[2]]$data
    expect_true(all(is.finite(nodes$x) & is.finite(nodes$y)), info = kind)
    expect_false(anyDuplicated(nodes$x[nodes$is_leaf]) > 0, info = kind)
    expect_equal(nodes$split_val, legacy$layers[[2]]$data$split_val, info = kind)
    expect_equal(nodes$action_id, legacy$layers[[2]]$data$action_id, info = kind)
    expect_silent(ggplot2::ggplotGrob(compact))
    expect_identical(serialize(tree, NULL), before, info = kind)
  }
})

test_that("custom titles and wrapping preserve caller labels", {
  tree <- policy_layout_fixture()
  title <- "Distress (reversed): a policy rule"
  p <- margot_plot_decision_tree(tree, title = title)
  expect_identical(p$labels$title, title)
  p <- margot_plot_decision_tree(tree, title = "")
  expect_null(p$labels$title)
  expect_equal(as.numeric(grid::convertHeight(ggplot2::ggplotGrob(p)$heights[3], "pt")), 0)
  mapping <- list(hours_z = "Paid work hours at the baseline measurement occasion")
  plain <- margot_plot_decision_tree(tree, label_mapping = mapping)
  wrapped <- margot_plot_decision_tree(tree, label_mapping = mapping, node_label_width = 12L)
  label_plain <- plain$layers[[2]]$data$label[1]
  label_wrapped <- wrapped$layers[[2]]$data$label[1]
  expect_gt(length(strsplit(label_wrapped, "\n", fixed = TRUE)[[1]]),
    length(strsplit(label_plain, "\n", fixed = TRUE)[[1]]))
  expect_identical(unname(gsub("[[:space:]]+", " ", label_wrapped)), unname(gsub("[[:space:]]+", " ", label_plain)))
  expect_null(names(wrapped$layers[[2]]$data$label))
  for (width in list(0, -1, 1.5, NA_real_, "narrow")) {
    expect_error(margot_plot_decision_tree(tree, node_label_width = width))
  }
})

test_that("malformed native tree topology fails before recursive layout", {
  tree <- policy_layout_fixture()
  cyclic <- tree
  cyclic$nodes[[1]]$left_child <- 1L
  expect_error(margot_plot_decision_tree(cyclic), "cycle|child|root|tree|topology")
  invalid <- tree
  invalid$nodes[[1]]$right_child <- 99L
  expect_error(margot_plot_decision_tree(invalid), "child|index|tree|topology")
  invalid <- tree
  invalid$nodes[[1]]$right_child <- NA_integer_
  expect_error(margot_plot_decision_tree(invalid))
})

test_that("native and wrapped inputs preserve the same rule and leaf labels", {
  tree <- policy_layout_fixture()
  object <- list(results = list(model_y = list(policy_tree_depth_1 = tree)))
  display <- structure(list(results = list(model_y = list(tree = tree, depth = 1L))),
    class = c("margot_policy_tree_display", "list"))
  metrics <- data.frame(node_id = c(2L, 3L),
    label = c("Assign positive hours\nPositive - zero: +0.12 SD", "Assign zero hours\nPositive - zero: -0.08 SD"))
  native <- margot_plot_decision_tree(tree, leaf_metrics = metrics, branch_labels = "condition")
  wrapped <- margot_plot_decision_tree(object, model_name = "y", max_depth = 1L,
    leaf_metrics = metrics, branch_labels = "condition")
  stored <- margot_plot_decision_tree(display, leaf_metrics = metrics, branch_labels = "condition")
  expect_equal(native$layers[[2]]$data, wrapped$layers[[2]]$data)
  expect_equal(native$layers[[2]]$data, stored$layers[[2]]$data)
  expect_identical(native$layers[[2]]$data$label[native$layers[[2]]$data$is_leaf], metrics$label)
})

test_that("native trees accept stored leaf metrics carrying their model attribute", {
  tree <- policy_layout_fixture()
  metrics <- structure(data.frame(node_id = c(2L, 3L), label = c("Leaf A", "Leaf B")),
    model = "model_y", depth = 1L)
  implicit <- margot_plot_decision_tree(tree, leaf_metrics = metrics)
  short <- margot_plot_decision_tree(tree, leaf_metrics = metrics, model_name = "y")
  full <- margot_plot_decision_tree(tree, leaf_metrics = metrics, model_name = "model_y")
  expect_identical(implicit$layers[[2]]$data$label[implicit$layers[[2]]$data$is_leaf], metrics$label)
  expect_equal(short$layers[[2]]$data, full$layers[[2]]$data)
  expect_equal(implicit$layers[[2]]$data, full$layers[[2]]$data)
  expect_error(margot_plot_decision_tree(tree, leaf_metrics = metrics, model_name = "other"), "model_y")
})

# bind synthetic display records and explicitly constructed intervals to a rule.
policy_layout_report_fixture <- function(depth = 1L, min_node_size = 1) {
  set.seed(20260911)
  reference <- expand.grid(hours = 1:7, second = 1:5)
  reward <- if (depth == 1L) ifelse(reference$hours <= 3, 1, -1) else {
    ifelse(reference$hours <= 3, ifelse(reference$second <= 2, 1, -1),
      ifelse(reference$second <= 4, -1, 1))
  }
  tree <- policytree::policy_tree(reference, cbind(control = 0, treated = reward),
    depth = depth, min.node.size = min_node_size)
  ids <- which(vapply(tree$nodes, function(node) isTRUE(node$is_leaf), logical(1)))
  leaves <- data.frame(node_id = ids, leaf_label = paste0("L", seq_along(ids)),
    estimate = rep(c(.12, -.04), length.out = length(ids)), lower = -.1, upper = .2,
    interval_type = "constructed", interval_level = .95,
    interval_method = "Specified illustration", unavailable_reason = NA_character_)
  value <- data.frame(estimate = .024, lower = .003, upper = .045,
    interval_type = "constructed", interval_level = .95,
    interval_method = "Specified illustration", unavailable_reason = NA_character_,
    comparator_id = "uniform-g1", comparator_label = "assigning g1 to everyone", gain_margin = .01)
  context <- list(outcome = "y", outcome_label = "Distress (reversed)", rule_id = "synthetic-y",
    population_id = "synthetic", population_label = "Illustrative population",
    scale_id = "y-sd", scale_label = "SD", orientation = "reversed", weight_id = "synthetic-weights",
    evaluation_mode = "constructed", contrast_label = "g1 minus g0",
    qualification = "Constructed estimates and intervals; no coverage claim.")
  object <- list(results = list(model_y = list(plot_data = list(X_test = reference))))
  object$results$model_y[[paste0("policy_tree_depth_", depth)]] <- tree
  data <- margot_policy_reporting_data(tree, leaves, value, context, reference = reference,
    display_weights = seq(.5, 1.5, length.out = nrow(reference)),
    reference_label = "Illustrative records", display_weight_id = "synthetic weights")
  list(object = object, data = data, depth = depth)
}

test_that("compact reports change presentation while preserving stored science", {
  for (depth in 1:2) {
    f <- policy_layout_report_fixture(depth)
    standard <- margot_report_policy_tree(f$object, "y", depth = depth, reporting_data = f$data)
    compact <- margot_report_policy_tree(f$object, "y", depth = depth, reporting_data = f$data,
      reporting_layout = "compact")
    expect_equal(standard$plots$combined_plot$patches$layout$heights, c(1.5, 1.7, 1))
    compact_heights <- compact$plots$combined_plot$patches$layout$heights
    expect_lte(compact_heights[1], standard$plots$combined_plot$patches$layout$heights[1])
    expect_equal(compact_heights, if (depth == 1) c(.8, 1.3, 1) else c(1.5, 1.8, 1))
    expect_null(compact$plots$decision_tree$coordinates$ratio)
    expect_identical(compact$plots$decision_tree$labels$title, f$data$context$outcome_label)
    expect_identical(standard$plots$projection$labels$title, f$data$context$outcome_label)
    expect_false(identical(compact$plots$projection$labels$title, f$data$context$outcome_label))
    for (panel in c("decision_tree", "projection", "leaf_effects", "value_gain")) {
      expect_null(compact$plots[[panel]]$labels$subtitle, info = panel)
    }
    for (panel in c("leaf_effects", "value_gain")) {
      expect_identical(compact$plots[[panel]]$data, standard$plots[[panel]]$data)
      expect_true(grepl(standard$plots[[panel]]$labels$caption, compact$plots[[panel]]$labels$caption, fixed = TRUE), info = panel)
    }
    expect_true(grepl(f$data$value$comparator_label, compact$plots$value_gain$labels$caption, fixed = TRUE))
    for (field in c("table", "policy_value", "text", "reporting_data", "metadata")) {
      expect_identical(compact[[field]], standard[[field]], info = field)
    }
    explicit <- margot_report_policy_tree(f$object, "y", depth = depth, reporting_data = f$data,
      reporting_layout = "compact", reporting_heights = c(2, 3, 4))
    expect_equal(explicit$plots$combined_plot$patches$layout$heights, c(2, 3, 4))
    expect_silent(patchwork::patchworkGrob(compact$plots$combined_plot))
  }
})

test_that("compact spacing reaches nested depth-two projections", {
  f <- policy_layout_report_fixture(2)
  standard <- margot_report_policy_tree(f$object, "y", depth = 2, reporting_data = f$data)
  compact <- margot_report_policy_tree(f$object, "y", depth = 2, reporting_data = f$data,
    reporting_layout = "compact")
  nested <- function(report) attr(report$plots$combined_plot$patches$plots[[2]], "grobs")$panel
  inner_compact <- nested(compact)
  inner_standard <- nested(standard)
  expect_s3_class(inner_compact, "patchwork")
  expect_equal(as.numeric(inner_compact$theme$plot.margin), c(4, 6, 4, 6))
  expect_equal(as.numeric(inner_compact$theme$legend.margin), c(0, 0, 0, 0))
  expect_null(inner_standard$theme$plot.margin)
  for (patch in inner_compact$patches$plots) {
    expect_equal(as.numeric(patch$theme$plot.margin), c(4, 6, 4, 6))
  }
  expect_identical(compact$reporting_data, standard$reporting_data)
})

test_that("compact default heights follow the fitted depth of the stored rule", {
  # a large minimum node size prunes the depth-two fit to a constant rule stored at depth 2
  f <- policy_layout_report_fixture(2, min_node_size = 15)
  expect_equal(f$data$leaves$node_id, 1L)
  expect_equal(f$object$results$model_y$policy_tree_depth_2$depth, 2)
  compact <- margot_report_policy_tree(f$object, "y", depth = 2, reporting_data = f$data,
    reporting_layout = "compact")
  expect_equal(compact$plots$combined_plot$patches$layout$heights, c(.8, 1.3, 1))
})

test_that("standard reports keep validating explicit heights and accept NULL panel labels", {
  f <- policy_layout_report_fixture()
  expect_error(margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data,
    reporting_heights = NULL), "reporting_heights")
  standard <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data)
  no_labels <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data,
    panel_labels = NULL)
  expect_equal(no_labels$plots$combined_plot$patches$layout$heights, c(1.5, 1.7, 1))
  expect_identical(no_labels$plots$decision_tree$labels, standard$plots$decision_tree$labels)
  expect_identical(no_labels$reporting_data, standard$reporting_data)
})

test_that("stored panel label overrides are literal and preserve provenance", {
  f <- policy_layout_report_fixture()
  standard <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data)
  custom <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data,
    reporting_layout = "compact", panel_labels = list(
      A = list(title = "Selected policy rule"),
      B = list(title = "Weighted projection", subtitle = "All illustrative records"),
      C = list(x = "Outcome contrast (SD)"),
      D = list(x = "Gain over uniform assignment (SD)", title = "Held-out gain")))
  expect_identical(custom$plots$decision_tree$labels$title, "Selected policy rule")
  expect_identical(custom$plots$projection$labels$title, "Weighted projection")
  expect_identical(custom$plots$projection$labels$subtitle, "All illustrative records")
  expect_identical(custom$plots$leaf_effects$labels$x, "Outcome contrast (SD)")
  expect_identical(custom$plots$value_gain$labels$x, "Gain over uniform assignment (SD)")
  expect_identical(custom$plots$value_gain$labels$title, "Held-out gain")
  for (field in c("table", "policy_value", "text", "reporting_data", "metadata")) {
    expect_identical(custom[[field]], standard[[field]], info = field)
  }
  removed <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data,
    panel_labels = list(A = list(title = NULL), C = list(caption = NULL)))
  expect_null(removed$plots$decision_tree$labels$title)
  expect_null(removed$plots$leaf_effects$labels$caption)
})

test_that("stored panel label overrides reject unknown panels and fields", {
  f <- policy_layout_report_fixture()
  invalid <- list(list(E = list(title = "Unknown")), list(A = list(colour = "red")),
    list(A = list(title = c("One", "Two"))), list(A = list(title = NA_character_)),
    list(A = list(title = 1)), list(list(title = "Unnamed")))
  for (labels in invalid) {
    expect_error(margot_report_policy_tree(f$object, "y", depth = 1,
      reporting_data = f$data, panel_labels = labels), "panel|Panel|label")
  }
  expect_error(margot_report_policy_tree(f$object, "y", depth = 1,
    reporting_layout = "compact"), "reporting_data")
  expect_error(margot_report_policy_tree(f$object, "y", depth = 1,
    panel_labels = list(A = list(title = "Rule"))), "reporting_data")
})

test_that("terminal child links are rejected before either layout traverses them", {
  for (style in c("legacy", "compact")) {
    tree <- policy_layout_fixture()
    tree$nodes[[2]]$left_child <- 1L
    expect_error(margot_plot_decision_tree(tree, layout_style = style), "terminal|leaf|child")
    tree$nodes[[2]]$left_child <- NA_integer_
    tree$nodes[[2]]$right_child <- NA_integer_
    expect_s3_class(margot_plot_decision_tree(tree, layout_style = style), "ggplot")
    tree$nodes[[2]]$right_child <- 3L
    expect_error(margot_plot_decision_tree(tree, layout_style = style), "terminal|leaf|child")
  }
})

test_that("depth-two projection axis overrides change rendered inner axes", {
  f <- policy_layout_report_fixture(2)
  grob_text <- function(grob) {
    c(if (inherits(grob, "text")) as.character(grob$label),
      unlist(lapply(grob$grobs, grob_text)), unlist(lapply(grob$children, grob_text)))
  }
  standard <- margot_report_policy_tree(f$object, "y", depth = 2, reporting_data = f$data)
  custom <- margot_report_policy_tree(f$object, "y", depth = 2, reporting_data = f$data,
    panel_labels = list(B = list(x = "Custom horizontal projection axis",
      y = "Custom vertical projection axis", title = "Custom projection title")))
  text_standard <- grob_text(patchwork::patchworkGrob(standard$plots$combined_plot))
  text_custom <- grob_text(patchwork::patchworkGrob(custom$plots$combined_plot))
  expect_true("Custom horizontal projection axis" %in% text_custom)
  expect_true("Custom vertical projection axis" %in% text_custom)
  expect_true("Custom projection title" %in% text_custom)
  expect_false("Custom horizontal projection axis" %in% text_standard)
  projection <- margot_plot_policy_projection(f$object, "y", max_depth = 2,
    display_weights = f$data$display_weights, jitter_seed = 20260911)
  old_axes <- unlist(lapply(seq_len(length(projection)), function(i) {
    c(projection[[i]]$labels$x, projection[[i]]$labels$y)
  }), use.names = FALSE)
  old_axes <- unique(old_axes[!is.na(old_axes) & nzchar(old_axes)])
  expect_gt(length(old_axes), 0)
  expect_true(all(old_axes %in% text_standard))
  expect_false(any(old_axes %in% text_custom))
  expect_identical(custom$reporting_data, standard$reporting_data)
})
