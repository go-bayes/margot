test_that("margot_policy_recurrence_summary summarises variables across outcomes", {
  policy_cv <- list(
    split_summary = data.frame(
      model = c("model_y1", "model_y1", "model_y2", "model_y2", "model_y2"),
      outcome = c("y1", "y1", "y2", "y2", "y2"),
      outcome_label = c("Y1", "Y1", "Y2", "Y2", "Y2"),
      depth = c(1L, 1L, 1L, 1L, 1L),
      node_id = c(1L, 1L, 1L, 1L, 2L),
      variable = c("x1", "x2", "x1", "x3", "x2"),
      variable_label = c("X1", "X2", "X1", "X3", "X2"),
      selection_frequency = c(0.7, 0.3, 0.4, 0.6, 0.5),
      stringsAsFactors = FALSE
    ),
    value_summary = data.frame(
      model = c("model_y1", "model_y2"),
      depth = c(1L, 1L),
      gain_vs_control_mean = c(0.2, 0.1)
    ),
    depth_map = c(model_y1 = 1L, model_y2 = 1L)
  )

  out <- margot_policy_recurrence_summary(policy_cv)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("variable", "n_outcomes_top_root", "mean_root_frequency") %in% names(out)))
  expect_equal(out$n_outcomes[out$variable == "x1"], 2)
  expect_equal(out$n_outcomes_root_selected[out$variable == "x1"], 2)
  expect_equal(out$n_outcomes_top_root[out$variable == "x1"], 1)
  expect_true(out$mean_any_node_frequency[out$variable == "x2"] > 0)
})
