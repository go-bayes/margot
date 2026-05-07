test_that("margot_select_grf_policy_trees applies the default graphing rule", {
  policy_brief <- data.frame(
    Outcome = c("Agreeableness", "Conscientiousness", "Neuroticism"),
    Depth = c(1L, 2L, 2L),
    `Policy Value (95% CI)` = c(
      "0.037 [-0.009, 0.083]",
      "0.091 [0.040, 0.142]",
      "0.050 [0.013, 0.086]"
    ),
    `Uplift in Treated (95% CI)` = c(
      "0.075 [-0.017, 0.168]",
      "0.146 [0.064, 0.228]",
      "0.135 [0.035, 0.235]"
    ),
    check.names = FALSE
  )

  selected <- margot_select_grf_policy_trees(
    policy_brief,
    model_names = c("model_a", "model_c", "model_n"),
    outcome_labels = c("Agreeableness", "Conscientiousness", "Neuroticism")
  )

  expect_equal(selected$model_name, c("model_a", "model_c", "model_n"))
  expect_equal(selected$policy_value_lower, c(-0.009, 0.040, 0.013))
  expect_equal(selected$treated_uplift_lower, c(-0.017, 0.064, 0.035))
  expect_equal(selected$graph_policy_tree, c(FALSE, TRUE, TRUE))
})

test_that("margot_select_grf_policy_trees accepts stricter thresholds", {
  policy_brief <- data.frame(
    Outcome = c("Outcome A", "Outcome B"),
    `Policy Value (95% CI)` = c("0.020 [0.005, 0.040]", "0.030 [0.015, 0.060]"),
    `Uplift in Treated (95% CI)` = c("0.050 [0.010, 0.090]", "0.070 [0.025, 0.110]"),
    check.names = FALSE
  )

  selected <- margot_select_grf_policy_trees(
    policy_brief,
    policy_value_lower_threshold = 0.01,
    treated_uplift_lower_threshold = 0.02
  )

  expect_equal(selected$graph_policy_tree, c(FALSE, TRUE))
})

test_that("margot_select_grf_policy_trees validates inputs", {
  policy_brief <- data.frame(Outcome = "Outcome A")

  expect_error(
    margot_select_grf_policy_trees(policy_brief),
    "missing required column"
  )

  complete_policy_brief <- data.frame(
    Outcome = "Outcome A",
    `Policy Value (95% CI)` = "0.020 [0.005, 0.040]",
    `Uplift in Treated (95% CI)` = "0.050 [0.010, 0.090]",
    check.names = FALSE
  )

  expect_error(
    margot_select_grf_policy_trees(complete_policy_brief, model_names = "model_a"),
    "outcome_labels"
  )
})
