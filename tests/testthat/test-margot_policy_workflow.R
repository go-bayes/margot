test_that("margot_policy_workflow defaults use preset keywords", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    margot_policy_summary_compare_depths = function(...) {
      list(
        depth_map = c(model_a = 1L),
        depth_summary_df = data.frame()
      )
    },
    margot_policy_summary_report = function(...) {
      data.frame(
        Outcome = "Outcome A",
        Depth = 1L,
        `Policy Value (95% CI)` = "0.050 [0.010, 0.090]",
        `Uplift in Treated (95% CI)` = "0.080 [0.020, 0.140]",
        `Coverage (%)` = 50,
        check.names = FALSE
      ) |>
        (\(brief) {
          list(
            wins_model_ids = "model_a",
            borderline_model_ids = character(0),
            recommended_model_ids = "model_a",
            neutral_model_ids = character(0),
            group_table_df = brief,
            report = "report text",
            report_prose = "report prose",
            coherent_policy_values = NULL,
            unit_masks = list()
          )
        })()
    },
    margot_interpret_policy_batch = function(stability, model_names, ...) {
      calls$interpret_model_names <- model_names
      list(report_detail = "report detail")
    },
    margot_policy = function(...) {
      calls$plot_called <- TRUE
      list()
    },
    margot_build_method_explanation = function(...) {
      list(long = "", short = "", prereg = "")
    }
  )

  expect_no_warning(
    wf <- margot_policy_workflow(list())
  )

  expect_equal(calls$interpret_model_names, "model_a")
  expect_null(calls$plot_called)
  expect_null(wf$plots)
  expect_equal(wf$report_prose, "report prose")
})

test_that("margot_policy_workflow plot_models same follows interpreted models", {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    margot_policy_summary_compare_depths = function(...) {
      list(
        depth_map = c(model_b = 1L),
        depth_summary_df = data.frame()
      )
    },
    margot_policy_summary_report = function(...) {
      data.frame(
        Outcome = "Outcome B",
        Depth = 1L,
        `Policy Value (95% CI)` = "0.060 [0.020, 0.100]",
        `Uplift in Treated (95% CI)` = "0.090 [0.030, 0.150]",
        `Coverage (%)` = 40,
        check.names = FALSE
      ) |>
        (\(brief) {
          list(
            wins_model_ids = "model_b",
            borderline_model_ids = character(0),
            recommended_model_ids = "model_b",
            neutral_model_ids = character(0),
            group_table_df = brief,
            report = "report text",
            report_prose = "report prose",
            coherent_policy_values = NULL,
            unit_masks = list()
          )
        })()
    },
    margot_interpret_policy_batch = function(stability, model_names, ...) {
      calls$interpret_model_names <- model_names
      list(report_detail = "report detail")
    },
    margot_policy = function(result_outcomes, model_names, ...) {
      calls$plot_model_names <- model_names
      list(model_b = list(combined_plot = "plot"))
    },
    margot_build_method_explanation = function(...) {
      list(long = "", short = "", prereg = "")
    }
  )

  wf <- margot_policy_workflow(list(), plot_models = "same")

  expect_equal(calls$interpret_model_names, "model_b")
  expect_equal(calls$plot_model_names, "model_b")
  expect_named(wf$plots, "model_b")
})
