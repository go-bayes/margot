# construct stored summaries with known display membership; no empirical data.
policy_reporting_fixture <- function(depth = 1, constant = FALSE) {
  set.seed(610)
  x <- expand.grid(score = 1:7, second = 1:5)
  rewards <- if (constant) rep(1, nrow(x)) else if (depth == 1) ifelse(x$score <= 3, 1, -1) else ifelse(x$score <= 3, ifelse(x$second <= 2, 1, -1), ifelse(x$second <= 4, -1, 1))
  tree <- policytree::policy_tree(x, cbind(control = 0, treated = rewards), depth = depth, min.node.size = 1)
  ids <- which(vapply(tree$nodes, function(n) isTRUE(n$is_leaf), logical(1)))
  leaves <- data.frame(node_id = ids, leaf_label = paste0("L", seq_along(ids)),
    estimate = rep(c(.12, -.04), length.out = length(ids)), lower = -.1, upper = .2,
    interval_type = "constructed", interval_level = .95,
    interval_method = "Specified illustration", unavailable_reason = NA_character_)
  value <- data.frame(estimate = .024, lower = .003, upper = .045, interval_type = "constructed",
    interval_level = .95, interval_method = "Specified illustration", unavailable_reason = NA_character_,
    comparator_id = "all_g1", comparator_label = "assigning g1 to everyone", gain_margin = .01)
  context <- list(outcome = "y", outcome_label = "Outcome of interest", rule_id = "rule-y",
    population_id = "target", population_label = "Target population", scale_id = "y-sd-v1", scale_label = "outcome SD",
    orientation = "as_scored", weight_id = "analysis-v1", evaluation_mode = "constructed",
    contrast_label = "g1 minus g0", qualification = "Constructed estimates and intervals; no coverage claim.")
  object <- list(results = list(model_y = list(plot_data = list(X_test = x))))
  object$results$model_y[[paste0("policy_tree_depth_", depth)]] <- tree
  data <- margot_policy_reporting_data(tree, leaves, value, context, reference = x,
    display_weights = seq(.5, 1.5, length.out = nrow(x)), reference_label = "Illustrative records", display_weight_id = "reference weights")
  list(object = object, data = data, depth = depth)
}

test_that("stored reporting shares estimates across tables, plots and text", {
  f <- policy_reporting_fixture()
  x <- f$data
  expect_equal(sum(x$leaves$reference_share), 1)
  expect_equal(sum(x$leaves$n_reference), nrow(x$reference))
  p <- margot_plot_policy_leaf_effects(x)
  expect_equal(p$data$estimate, x$leaves$estimate)
  expect_equal(ggplot2::ggplot_build(p)$data[[2]]$x, x$leaves$lower)
  expect_equal(ggplot2::ggplot_build(p)$data[[2]]$xend, x$leaves$upper)
  text <- paste(margot_text_policy_value_gain(x), collapse = " ")
  expect_match(text, "0.024")
  expect_match(text, "exceeds")
  expect_match(text, "below the practical margin")
  expect_match(text, "constructed interval")
  report <- margot_report_policy_tree(f$object, "y", reporting_data = x, depth = 1)
  expect_s3_class(report$plots$combined_plot, "patchwork")
  expect_equal(report$table$estimate, p$data$estimate)
  expect_equal(report$policy_value, x$value)
  expect_identical(report$text$value, margot_text_policy_value_gain(x))
  expect_equal(report$plots$decision_tree$labels$title, "Outcome of interest")
  expect_equal(report$plots$projection$labels$title, "Outcome of interest")
})

test_that("weighted projections preserve coordinates and use globally comparable areas", {
  for (depth in 1:2) {
    f <- policy_reporting_fixture(depth)
    set.seed(661)
    before <- .Random.seed
    report <- margot_report_policy_tree(f$object, "y", reporting_data = f$data, depth = depth)
    p <- margot_plot_policy_tree(f$object, "model_y", max_depth = depth,
      display_weights = f$data$display_weights, jitter_seed = 123)
    panels <- if (depth == 1) list(p) else list(p[[1]], p[[2]])
    rows <- integer()
    for (panel in panels) {
      i <- which(vapply(panel$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
      layer <- panel$layers[[i]]
      points <- ggplot2::ggplot_build(panel)$data[[i]]
      expect_equal(points$size^2 / layer$data$display_weight, rep(16 / max(f$data$display_weights), nrow(points)))
      expect_true(all(points$shape == 16))
      if (depth == 1) expect_identical(points$x, f$data$reference$score * 1.0)
      if (depth == 2) rows <- c(rows, layer$data$reference_row)
    }
    if (depth == 2) expect_equal(sort(rows), seq_len(nrow(f$data$reference)))
    expect_identical(.Random.seed, before)
    # all tags must survive nesting; building the grob also catches layout errors.
    g <- patchwork::patchworkGrob(report$plots$combined_plot)
    grob_labels <- function(g) {
      c(if (inherits(g, "text")) as.character(g$label),
        unlist(lapply(g$grobs, grob_labels)), unlist(lapply(g$children, grob_labels)))
    }
    expect_true(all(LETTERS[1:4] %in% grob_labels(g)))
  }
})

test_that("constant trees and missing intervals produce complete reports", {
  f <- policy_reporting_fixture(constant = TRUE)
  f$data$value$lower <- f$data$value$upper <- NA_real_
  f$data$value$interval_type <- "unavailable"
  f$data$value$unavailable_reason <- "Participant contributions were not retained"
  report <- margot_report_policy_tree(f$object, "y", reporting_data = f$data, depth = 1)
  expect_equal(nrow(report$table), 1)
  expect_equal(report$table$reference_share, 1)
  expect_match(paste(report$text$value, collapse = " "), "Interval unavailable")
  expect_equal(nrow(ggplot2::ggplot_build(report$plots$value_gain)$data[[3]]), 0)
  expect_s3_class(patchwork::patchworkGrob(report$plots$combined_plot), "gtable")
})

test_that("incompatible identities and intervals fail before plotting", {
  x <- policy_reporting_fixture()$data
  for (field in c("outcome", "rule_id", "population_id", "scale_id", "orientation", "weight_id")) {
    y <- x
    y$value_context[[field]] <- if (field == "orientation") "reversed" else "different"
    expect_error(margot_plot_policy_value_gain(y), "Incompatible")
  }
  y <- x
  y$context$evaluation_mode <- y$value_context$evaluation_mode <- "selected_full_sample"
  expect_error(margot_plot_policy_leaf_effects(y), "Interval type")
  y$leaves$interval_type <- "nominal_fixed_leaves"
  y$value$lower <- y$value$upper <- NA_real_
  y$value$interval_type <- "unavailable"
  y$value$unavailable_reason <- "Unavailable for selected rule"
  y$value_context$evaluation_mode <- "repeated_learning"
  y$value_context$rule_id <- "learning-procedure-y"
  expect_match(paste(margot_text_policy_leaf_effects(y), collapse = " "), "ignores leaf selection")
  expect_match(paste(margot_text_policy_value_gain(y), collapse = " "), "Repeated-learning procedure")
  y <- x
  y$value$upper <- NA_real_
  expect_error(margot_plot_policy_value_gain(y), "both interval endpoints")
  y <- x
  y$value$gain_margin <- NA_real_
  expect_error(margot_text_policy_value_gain(y), "gain_margin")
  y <- x
  y$tree$nodes[[1]]$split_value <- 100
  expect_error(margot_plot_policy_value_gain(y), "signature")
})

test_that("reversal and margins are read from stored values without rescaling", {
  x <- policy_reporting_fixture()$data
  x$context$orientation <- x$value_context$orientation <- "reversed"
  x$context$outcome_label <- x$value_context$outcome_label <- "Distress (reversed)"
  for (gain in c(-.02, 0, .024)) {
    x$value$estimate <- gain
    x$value$lower <- -.03
    x$value$upper <- .04
    for (margin in c(.01, .03)) {
      x$value$gain_margin <- margin
      p <- margot_plot_policy_value_gain(x)
      expect_identical(p$data$estimate, gain)
      expect_equal(ggplot2::ggplot_build(p)$data[[2]]$xintercept, margin)
      txt <- paste(margot_text_policy_value_gain(x), collapse = " ")
      expect_match(txt, "Distress \\(reversed\\)")
      expect_match(txt, if (gain > margin) "exceeds" else "is below")
      expect_match(txt, "includes zero")
    }
  }
})

test_that("stored reporting invokes no estimator, score summary or resampling", {
  f <- policy_reporting_fixture()
  fail <- function(...) stop("Estimation invoked by reporting")
  local_mocked_bindings(margot_policy_leaf_summary = fail, margot_policy_tree_cv = fail,
    margot_policy_tree_bootstrap = fail, .margot_policy_leaf_interval = fail,
    .package = "margot")
  local_mocked_bindings(policy_tree = fail, .package = "policytree")
  expect_s3_class(margot_report_policy_tree(f$object, "y", reporting_data = f$data, depth = 1), "margot_policy_tree_report")
})

test_that("report rejects changed display rows and weight overrides", {
  f <- policy_reporting_fixture()
  changed <- f$object
  changed$results$model_y$plot_data$X_test <- f$data$reference[nrow(f$data$reference):1, ]
  expect_error(margot_report_policy_tree(changed, "y", reporting_data = f$data, depth = 1), "Projection rows")
  expect_error(margot_report_policy_tree(f$object, "y", reporting_data = f$data, depth = 1,
    projection_args = list(display_weights = rep(1, 35))), "control projection")
  for (w in list(rep(0, 35), rep(-1, 35), c(NA, rep(1, 34)), rep(1, 34))) {
    expect_error(margot_plot_policy_tree(f$object, "model_y", max_depth = 1, display_weights = w), "display_weights")
  }
})

test_that("independent evaluation metadata and interval methods are explicit", {
  x <- policy_reporting_fixture()$data
  x$context$evaluation_mode <- x$value_context$evaluation_mode <- "independent_fixed_rule"
  expect_error(margot_plot_policy_value_gain(x), "development_id")
  for (slot in c("context", "value_context")) {
    x[[slot]]$development_id <- "development-participants"
    x[[slot]]$evaluation_id <- "evaluation-participants"
  }
  x$leaves$interval_type <- "pointwise"
  x$value$interval_type <- "simultaneous"
  expect_match(paste(margot_text_policy_value_gain(x), collapse = " "), "simultaneous interval")
  x$value_context$evaluation_id <- "development-participants"
  expect_error(margot_plot_policy_value_gain(x), "must differ")
})

test_that("zero-weight rows remain counted and weight changes require a new binding", {
  f <- policy_reporting_fixture()
  x <- f$data
  w <- x$display_weights
  w[1:3] <- 0
  y <- margot_policy_reporting_data(x$tree, x$leaves, x$value, x$context, reference = x$reference,
    display_weights = w, reference_label = x$reference_label, display_weight_id = "weights-with-zeros")
  expect_equal(sum(y$leaves$n_reference), length(w))
  expect_equal(sum(y$leaves$reference_share), 1)
  ids <- .margot_policy_tree_leaf_ids(y$tree, y$reference)
  expect_equal(y$leaves$reference_share[1], sum(w[ids == y$leaves$node_id[1]]) / sum(w))
  y$display_weights[1] <- 2
  expect_error(margot_plot_policy_leaf_effects(y), "display rows or weights")
})

test_that("a pruned depth-two root branch has a complete unique projection", {
  f <- policy_reporting_fixture(depth = 2)
  set.seed(779)
  x <- expand.grid(score = 1:7, second = 1:5)
  rewards <- ifelse(x$score <= 3, 1, ifelse(x$second <= 2, -1, 1))
  tree <- policytree::policy_tree(x, cbind(control = 0, treated = rewards), depth = 2, min.node.size = 1)
  expect_true(isTRUE(tree$nodes[[tree$nodes[[1]]$left_child]]$is_leaf))
  f$object$results$model_y$policy_tree_depth_2 <- tree
  p <- margot_plot_policy_tree(f$object, "model_y", max_depth = 2, display_weights = rep(1, nrow(x)), jitter_seed = 779)
  rows <- unlist(lapply(1:2, function(i) {
    layer <- Filter(function(l) inherits(l$geom, "GeomPoint"), p[[i]]$layers)[[1]]
    layer$data$reference_row
  }))
  expect_equal(sort(rows), seq_len(nrow(x)))
  expect_s3_class(patchwork::patchworkGrob(p), "gtable")
})

test_that("weighted combo leaf labels use the projection weights", {
  f <- policy_reporting_fixture()
  f$object$weights <- rep(100, nrow(f$data$reference))
  p <- margot_plot_policy_tree_panels(f$object, "model_y", max_depth = 1,
    projection_args = list(display_weights = f$data$display_weights, jitter_seed = 42))
  nodes <- p$decision_tree$layers[[2]]$data
  expected <- paste0(formatC(100 * f$data$leaves$reference_share, format = "f", digits = 1), "% weighted")
  labels <- nodes$label[nodes$is_leaf]
  expect_true(all(vapply(seq_along(expected), function(i) grepl(expected[i], labels[i], fixed = TRUE), logical(1))))
})

# inspect the device geometry, including any stroke contribution to point radius.
policy_reporting_svg_radii <- function(plot) {
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  ggplot2::ggsave(path, plot + ggplot2::theme(legend.position = "none"),
    device = svglite::svglite, width = 6, height = 3)
  circles <- grep("<circle ", readLines(path, warn = FALSE), value = TRUE)
  as.numeric(sub(".* r=['\"]([0-9.]+)['\"].*", "\\1", circles))
}

test_that("rendered weighted circles have proportional area and zero-weight points disappear", {
  skip_if_not_installed("svglite")
  for (kind in c("stump", "depth_two", "constant")) {
    depth <- if (kind == "depth_two") 2 else 1
    f <- policy_reporting_fixture(depth, constant = kind == "constant")
    weights <- rep(c(0, .01, 1), length.out = nrow(f$data$reference))
    p <- margot_plot_policy_tree(f$object, "model_y", max_depth = depth,
      display_weights = weights, weight_max_size = 4, jitter_seed = 20260910)
    panels <- if (depth == 2) list(p[[1]], p[[2]]) else list(p)
    for (panel in panels) {
      layer <- Filter(function(l) inherits(l$geom, "GeomPoint"), panel$layers)[[1]]
      expected <- if (is.data.frame(layer$data)) layer$data$display_weight else panel$data$display_weight
      radii <- policy_reporting_svg_radii(panel)
      # devices may emit a zero-radius circle or omit it entirely.
      expect_true(length(radii) %in% c(length(expected), sum(expected > 0)), info = kind)
      if (length(radii) == sum(expected > 0)) expected <- expected[expected > 0]
      if (length(radii) != length(expected)) next
      expect_true(all(radii[expected == 0] == 0), info = kind)
      # svglite rounds SVG radii to two decimals; allow only that rendering error.
      expect_true(max(abs(radii^2 / max(radii)^2 - expected / max(expected))) <= .0003, info = kind)
    }
  }
})

test_that("constant projections honour custom action labels with either weight mode", {
  f <- policy_reporting_fixture(constant = TRUE)
  labels <- list(treated = "Positive volunteering hours", control = "Zero volunteering hours")
  for (action in c("treated", "control")) {
    if (action == "control") {
      f$object$results$model_y$policy_tree_depth_1 <- policytree::policy_tree(
        f$data$reference, cbind(control = 1, treated = rep(0, nrow(f$data$reference))),
        depth = 1, min.node.size = 1)
    }
    for (weights in list(NULL, f$data$display_weights)) {
      p <- margot_plot_policy_tree(f$object, "model_y", max_depth = 1,
        label_mapping = labels, display_weights = weights, jitter_seed = 20260910)
      expect_identical(p$labels$subtitle, paste("Constant assignment:", labels[[action]]))
    }
  }
})
