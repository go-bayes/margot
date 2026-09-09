# stored summaries must retain the training comparator and signed leaf contrast.
stored_policy_example <- function() {
  structure(list(policy_selection = data.frame(model = "model_y", outcome = "y",
    selected_tree_depth = 1L, value_selected_tree = -0.02, value_honest_constant = -0.04,
    tree_minus_honest_constant = 0.02, min_gain_over_constant = 0.01, preferred_policy = "tree"),
    value_summary = data.frame(value_treat_all = 0.1),
    leaf_summary = data.frame(model = "model_y", depth = 1L, action = "control",
      treatment_control_contrast_mean = 0.03, treatment_control_contrast_q025 = -0.1,
      treatment_control_contrast_q975 = 0.2)), class = "margot_policy_tree_cv")
}

test_that("interpretation uses stored training decisions and signed held-out summaries", {
  x <- stored_policy_example()
  text <- margot_text_policy_tree("heldout_cv", object = x, include_definitions = FALSE,
    label_mapping = list(y = "anxiety (reversed)"),
    action_names = c(control = "zero hours", treated = "positive hours"), value_units = "SD")
  expect_match(text, "anxiety (reversed)", fixed = TRUE)
  expect_match(text, "compared with -0.040", fixed = TRUE)
  expect_match(text, "difference is +0.020 SD", fixed = TRUE)
  expect_match(text, "assigned zero hours", fixed = TRUE)
  expect_match(text, "is +0.030", fixed = TRUE)
  expect_match(text, "rather than confidence intervals", fixed = TRUE)
  expect_false(grepl("recommended|advised", text))
  x$policy_selection$tree_minus_honest_constant <- 0.5
  expect_error(margot_text_policy_tree("heldout_cv", object = x), "inconsistent")
})

test_that("rounding cannot change the policy-class decision", {
  x <- stored_policy_example()
  x$policy_selection$tree_minus_honest_constant <- 0.0096
  x$policy_selection$value_selected_tree <- -0.04 + 0.0096
  x$policy_selection$preferred_policy <- "constant"
  text <- margot_text_policy_tree("heldout_cv", object = x, include_definitions = FALSE)
  expect_match(text, "falls below", fixed = TRUE)
  expect_match(text, "favours the same-action procedure", fixed = TRUE)
  x$policy_selection$preferred_policy <- "tree"
  expect_error(margot_text_policy_tree("heldout_cv", object = x), "disagrees")
})

test_that("tree interpretation follows child links through pruned trees", {
  tree <- list(action.names = c("control", "treated"), columns = c("x", "z"), nodes = list(
    list(is_leaf = FALSE, split_variable = 1L, split_value = 2, left_child = 2L, right_child = 5L),
    list(is_leaf = FALSE, split_variable = 2L, split_value = 3, left_child = 3L, right_child = 4L),
    list(is_leaf = TRUE, action = 1L), list(is_leaf = TRUE, action = 2L), list(is_leaf = TRUE, action = 1L)))
  x <- list(results = list(model_y = list(policy_tree_depth_2 = tree)))
  capture.output(text <- margot_interpret_policy_tree(x, "model_y", include_conditional_means = FALSE,
    custom_action_names = c("zero hours", "positive hours"), use_title_case = FALSE, output_format = "bullet"))
  expect_match(text, "baseline x <= 2 and baseline z > 3, the tree assigns positive hours", fixed = TRUE)
  expect_match(text, "baseline x > 2, the tree assigns zero hours", fixed = TRUE)
  expect_false(grepl("50%|advised|recommended", text))
  x$results$model_y$policy_tree_depth_2$nodes <- list(list(is_leaf = TRUE, action = 2L))
  capture.output(text <- margot_interpret_policy_tree(x, "model_y", include_conditional_means = FALSE))
  expect_match(text, "For all cases, the tree assigns", fixed = TRUE)
})
