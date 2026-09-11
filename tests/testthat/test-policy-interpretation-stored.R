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
  expect_false(grepl("Mean contrasts weight leaves|rather than confidence intervals", text))
  expect_match(text, "depth-one policy-tree procedure", fixed = TRUE)
  expect_match(text, "specified comparison", fixed = TRUE)
  with_definitions <- margot_text_policy_tree("heldout_cv", object = x)
  expect_match(with_definitions, "rather than confidence intervals", fixed = TRUE)
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


test_that("explicit policy labels retain one supplied direction marker", {
  label <- "Anxiety (Kessler) (reversed)"
  expect_identical(.policy_cv_label("kessler_latent_anxiety", list(kessler_latent_anxiety = label)), label)
  expect_identical(.policy_cv_label("anxiety_r", c(anxiety_r = label)), label)
  expect_identical(.policy_cv_label("anxiety_r", c(anxiety = label)), label)
  expect_identical(.policy_cv_label("anxiety_r", c(anxiety = "Anxiety")), "Anxiety (reversed)")
  x <- stored_policy_example()
  x$policy_selection$outcome <- "kessler_latent_anxiety"
  text <- margot_text_policy_tree("heldout_cv", object = x, include_definitions = FALSE,
    label_mapping = list(kessler_latent_anxiety = label))
  expect_match(text, paste0("For ", label, ","), fixed = TRUE)
  expect_false(grepl("(reduced)", text, fixed = TRUE))
  expect_equal(length(regmatches(text, gregexpr("(reversed)", text, fixed = TRUE))[[1]]), 1L)
})

# definition controls must survive the threshold-reporting integration.
test_that("leaf definitions honour the option for both value objectives", {
  for (threshold_adjusted in c(FALSE, TRUE)) {
    x <- stored_policy_example()
    x$policy_selection$selected_tree_depth <- 2L
    x$leaf_summary$depth <- 2L
    if (threshold_adjusted) x$metadata <- list(value_objective = "threshold_adjusted")
    original <- x
    concise <- margot_text_policy_tree("heldout_cv", object = x, include_definitions = FALSE)
    expanded <- margot_text_policy_tree("heldout_cv", object = x, include_definitions = TRUE)
    expect_match(concise, "depth-two policy-tree procedure", fixed = TRUE)
    expect_match(concise, "difference is +0.020", fixed = TRUE)
    expect_match(concise, "is +0.030", fixed = TRUE)
    definition <- if (threshold_adjusted) "Leaf ranges describe variation" else "Mean contrasts weight leaves"
    expect_false(grepl(definition, concise, fixed = TRUE))
    expect_match(expanded, definition, fixed = TRUE)
    if (threshold_adjusted) {
      expect_match(concise, "threshold-adjusted net value", fixed = TRUE)
      expect_false(grepl("training preference did not persist", expanded, fixed = TRUE))
    }
    expect_identical(x, original)
  }
})
