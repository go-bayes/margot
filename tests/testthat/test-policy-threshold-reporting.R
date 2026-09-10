# fixed synthetic rule and reporting identities with signed benefit thresholds.
threshold_reporting_fixture <- function(phi = .04, source = "ate") {
  x <- data.frame(score = rep(1:6, 3), second = rep(1:3, each = 6))
  tree <- policytree::policy_tree(x, cbind(control = 0, treated = ifelse(x$score < 4, -.01, .07)), depth = 1, min.node.size = 1)
  ids <- which(vapply(tree$nodes, function(node) isTRUE(node$is_leaf), logical(1)))
  leaves <- data.frame(node_id = ids, leaf_label = paste0("Group ", seq_along(ids)),
    estimate = c(.02, .08), lower = c(-.01, .04), upper = c(.05, .12),
    interval_type = "pointwise", interval_level = .95, interval_method = "Paired score illustration",
    unavailable_reason = NA_character_)
  value <- data.frame(estimate = .01, lower = -.01, upper = .03,
    interval_type = "pointwise", interval_level = .95, interval_method = "Paired score illustration",
    unavailable_reason = NA_character_, comparator_id = "development_constant",
    comparator_label = "development-selected uniform assignment", gain_margin = .01)
  context <- list(outcome = "y", outcome_label = "Wellbeing", rule_id = "development-tree",
    population_id = "target", population_label = "Target population", scale_id = "scale-v1",
    scale_label = "SD", orientation = "as_scored", weight_id = "analysis-v1",
    evaluation_mode = "independent_fixed_rule", development_id = "development", evaluation_id = "evaluation",
    contrast_label = "Treatment minus control", qualification = "Conditional on the development rule and scores.",
    value_threshold = list(value = phi, source = source, multiplier = 1, development_ate = phi))
  report <- margot_policy_reporting_data(tree, leaves, value, context,
    reference = x, display_weights = seq_len(nrow(x)), reference_label = "Evaluation participants",
    display_weight_id = "Evaluation weights")
  list(data = report, object = list(results = list(model_y = list(policy_tree_depth_1 = tree,
    plot_data = list(X_test = x)))))
}

test_that("signed thresholds retain original leaf effects and net objective labels", {
  for (phi in c(.04, 0, -.04)) {
    f <- threshold_reporting_fixture(phi)
    expect_equal(margot_table_policy_tree(f$data)$estimate, c(.02, .08))
    expect_equal(margot_table_policy_tree(f$data)$value_threshold, rep(phi, 2))
    expect_equal(margot_table_policy_value(f$data)$value_threshold, phi)
    text <- paste(margot_text_policy_leaf_effects(f$data), collapse = " ")
    expect_match(text, "ATE-referenced benefit threshold")
    expect_match(text, "below the threshold need not indicate harm")
    expect_identical(grepl("not a positive treatment expense", text), phi <= 0)
    expect_match(margot_plot_policy_value_gain(f$data)$labels$x, "Net value")
    expect_equal(margot_plot_policy_leaf_effects(f$data)$data$estimate, c(.02, .08))
    expect_equal(margot_plot_policy_value_gain(f$data)$data$lower, -.01)
  }
})

test_that("threshold identity and development reference are enforced", {
  x <- threshold_reporting_fixture()$data
  y <- x
  y$value_context$value_threshold$value <- .02
  y$value_context$value_threshold$development_ate <- .02
  expect_error(margot_text_policy_value_gain(y), "Incompatible value_threshold")
  y <- x
  y$context$value_threshold$development_ate <- .06
  expect_error(margot_plot_policy_leaf_effects(y), "must equal")
  y <- x
  y$value_context$rule_id <- "full-sample-refit"
  expect_error(margot_plot_policy_value_gain(y), "Incompatible rule_id")
  y <- x
  y$value_context$evaluation_mode <- "selected_full_sample"
  expect_error(margot_plot_policy_value_gain(y), "Incompatible evaluation_mode")
})

test_that("two-panel artwork retains separate uncertainty reporting", {
  f <- threshold_reporting_fixture()
  report <- margot_report_policy_tree(f$object, "y", depth = 1, reporting_data = f$data,
    reporting_layout = "two_panel")
  expect_s3_class(report$plots$combined_plot, "patchwork")
  expect_null(report$plots$combined_plot$patches$annotation$caption)
  expect_null(report$plots$decision_tree$labels$caption)
  expect_null(report$plots$projection$labels$caption)
  expect_match(report$plots$decision_tree$labels$subtitle, "0.040")
  expect_equal(report$plots$value_gain$data$lower, -.01)
  expect_equal(report$plots$leaf_effects$data$estimate, c(.02, .08))
  labels <- report$plots$decision_tree$layers[[2]]$data$label
  expect_true(any(grepl("Treatment minus control: 0.020 SD", labels, fixed = TRUE)))
  expect_s3_class(patchwork::patchworkGrob(report$plots$combined_plot), "gtable")
})

test_that("fixed-rule adapter preserves evaluated identity and supplied intervals", {
  f <- threshold_reporting_fixture()
  d <- f$data
  evaluation <- structure(list(tree = d$tree, rule_id = d$context$rule_id,
    threshold = d$context$value_threshold,
    metadata = list(development_id = "development", evaluation_id = "evaluation"),
    inference = list(qualification = "Uncertainty conditional on the fixed rule."),
    evaluation = list(leaves = d$leaves, value = d$value)), class = "margot_policy_tree_evaluation")
  evaluation$rule_id <- .margot_policy_rule_signature(evaluation$tree)
  evaluation$integrity_signature <- digest::digest(evaluation, algo = "sha256")
  context <- d$context
  context[c("rule_id", "development_id", "evaluation_id", "evaluation_mode", "value_threshold")] <- NULL
  result <- margot_policy_evaluation_reporting_data(evaluation, context)
  expect_identical(result$context$rule_id, evaluation$rule_id)
  expect_equal(result$leaves$lower, d$leaves$lower)
  expect_equal(result$value$lower, d$value$lower)
  expect_match(result$context$qualification, "conditional on the fixed rule")
  context$rule_id <- "full-data-refit"
  expect_error(margot_policy_evaluation_reporting_data(evaluation, context), "Incompatible rule_id")
})


test_that("real independent evaluation adapts without refitting or losing scope", {
  d <- data.frame(x = rep(1:6, 5), z = rep(1:5, each = 6))
  e <- data.frame(x = rep(1:6, 4), z = rep(1:4, each = 6))
  evaluation <- margot_policy_tree_evaluate(d, cbind(control = 0, treated = ifelse(d$x < 4, .02, .08)),
    e, cbind(control = 0, treated = ifelse(e$x < 4, .03, .07)),
    development_ids = paste0("d", seq_len(nrow(d))), evaluation_ids = paste0("e", seq_len(nrow(e))))
  context <- threshold_reporting_fixture()$data$context
  context[c("rule_id", "development_id", "evaluation_id", "evaluation_mode", "value_threshold")] <- NULL
  stored <- margot_policy_evaluation_reporting_data(evaluation, context,
    reference = e, reference_label = "Evaluation records", display_weight_id = "Equal weights")
  expect_equal(stored$leaves$estimate, evaluation$evaluation$leaves$estimate)
  expect_equal(stored$value$estimate, evaluation$evaluation$value$estimate)
  expect_equal(stored$context$value_threshold$value, .05)
  tampered <- evaluation
  tampered$threshold$value <- .08
  expect_error(margot_policy_evaluation_reporting_data(tampered, context), "signature|changed")
  object <- list(results = list(model_y = list(policy_tree_depth_1 = evaluation$tree, plot_data = list(X_test = e))))
  report <- margot_report_policy_tree(object, "y", reporting_data = stored, depth = 1,
    reporting_layout = "two_panel")
  expect_s3_class(patchwork::patchworkGrob(report$plots$combined_plot), "gtable")
})

test_that("unrepresented evaluation leaves retain explicit unavailable estimates", {
  f <- threshold_reporting_fixture()
  leaves <- f$data$leaves
  leaves$estimate[1] <- leaves$lower[1] <- leaves$upper[1] <- NA_real_
  leaves$interval_type[1] <- "unavailable"
  leaves$unavailable_reason[1] <- "No evaluation records in this leaf"
  stored <- margot_policy_reporting_data(f$data$tree, leaves, f$data$value, f$data$context)
  expect_true(is.na(stored$leaves$estimate[1]))
  expect_match(paste(margot_text_policy_leaf_effects(stored), collapse = " "), "No evaluation records")
  expect_no_warning(ggplot2::ggplotGrob(margot_plot_policy_leaf_effects(stored)))
})
