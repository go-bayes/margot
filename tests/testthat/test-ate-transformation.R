# synthetic reporting contrasts keep source estimates distinct from display scales
ate_reporting_fixture <- function(outcome = "y", estimate = 0.5, lower = -0.1, upper = 1.3) {
  out <- data.frame(outcome = outcome, ATE = estimate, E_Value = 2.123456789,
                    E_Val_bound = 1.123456789, stringsAsFactors = FALSE)
  out[["2.5 %"]] <- lower
  out[["97.5 %"]] <- upper
  out
}

test_that("affine transformations preserve asymmetric intervals and source numbers", {
  input <- ate_reporting_fixture()
  metadata <- data.frame(outcome = "y", center = 27, scale = 2)
  out <- back_transform_estimates(input, scale_info = metadata)
  expect_equal(out$reported_estimate, 1)
  expect_equal(out$reported_lower, -0.2)
  expect_equal(out$reported_upper, 2.6)
  expect_identical(out$ATE, input$ATE)
  expect_identical(out$E_Value, input$E_Value)
  expect_identical(out$E_Val_bound, input$E_Val_bound)
  expect_equal(out$reporting_quantity, "mean_difference")
  metadata$orientation <- -1
  reversed <- back_transform_estimates(input, scale_info = metadata)
  expect_equal(reversed$reported_estimate, -1)
  expect_equal(reversed$reported_lower, -2.6)
  expect_equal(reversed$reported_upper, 0.2)
})

test_that("reporting keeps full precision and uses explicit saved scales", {
  input <- ate_reporting_fixture(estimate = 0.123456789, lower = 0.012345678, upper = 0.876543219)
  metadata <- data.frame(outcome = "y", scale = 3.456789, unit = "minutes", unit_multiplier = 60)
  out <- back_transform_estimates(input, original_df = data.frame(y = c(100, 200)), scale_info = metadata)
  expect_identical(out$reported_estimate, input$ATE * metadata$scale * 60)
  expect_identical(out$reported_lower, input[["2.5 %"]] * metadata$scale * 60)
  expect_identical(out$reporting_unit, "minutes")
  expect_equal(attr(out, "report_scale_info")$scale, metadata$scale)
})

test_that("log contrasts report geometric ratios rather than arithmetic effects", {
  input <- ate_reporting_fixture(estimate = log(2), lower = log(1.5), upper = log(3))
  metadata <- data.frame(outcome = "y", transformation = "log", center = 99, scale = 1)
  out <- back_transform_estimates(input, scale_info = metadata)
  expect_equal(out$reported_estimate, 2)
  expect_equal(out$reported_lower, 1.5)
  expect_equal(out$reported_upper, 3)
  expect_equal(out$reporting_quantity, "geometric_mean_ratio")
  expect_true(is.na(out$ATE_original))
  expect_true(is.na(out[["2.5 %_original"]]))
  # identical log contrasts can have different arithmetic mean differences
  control <- c(1, 4)
  treated <- c(2, 8)
  expect_equal(exp(mean(log(treated)) - mean(log(control))), out$reported_estimate)
  expect_equal(mean(treated) - mean(control), 2.5)
  expect_false(isTRUE(all.equal(out$reported_estimate, mean(treated) - mean(control))))
  metadata$orientation <- -1
  reverse <- back_transform_estimates(input, scale_info = metadata)
  expect_equal(reverse$reported_estimate, 0.5)
  expect_equal(reverse$reported_lower, 1 / 3)
  expect_equal(reverse$reported_upper, 1 / 1.5)
})

test_that("log1p shifts remain explicit even when zeros occur", {
  control <- c(0, 3)
  treated <- c(1, 7)
  input <- ate_reporting_fixture(estimate = mean(log1p(treated)) - mean(log1p(control)))
  metadata <- data.frame(outcome = "y", transformation = "log1p")
  out <- back_transform_estimates(input, scale_info = metadata)
  expect_equal(out$reported_estimate, 2)
  expect_equal(out$reporting_quantity, "shifted_geometric_mean_ratio")
  expect_match(out$reporting_unit, "Y \\+ 1")
  expect_true(is.na(out$ATE_original))
  expect_identical(out$E_Value, input$E_Value)
})

test_that("ratio reporting never multiplies the contrast by an outcome SD", {
  input <- ate_reporting_fixture(estimate = 2, lower = 1.5, upper = 3)
  metadata <- data.frame(outcome = "y", scale = 2, center = 0)
  out <- back_transform_estimates(input, scale_info = metadata, type = "RR")
  expect_equal(out$reported_estimate, 2)
  expect_equal(out$reported_lower, 1.5)
  expect_equal(out$reported_upper, 3)
  expect_equal(out$reporting_quantity, "risk_ratio")
  metadata$center <- 1
  expect_error(back_transform_estimates(input, scale_info = metadata, type = "RR"), "zero centre")
  metadata$center <- 0
  metadata$transformation <- "log"
  expect_error(back_transform_estimates(input, scale_info = metadata, type = "RR"), "nonlinear")
})

test_that("metadata validation rejects ambiguous and invalid transformation contracts", {
  input <- ate_reporting_fixture()
  expect_error(back_transform_estimates(input, scale_info = data.frame(outcome = "other")), "missing outcomes")
  expect_error(back_transform_estimates(input, scale_info = data.frame(outcome = c("y", "y"))), "unique")
  expect_error(back_transform_estimates(input, scale_info = data.frame(outcome = "y", scale = 0)), "positive")
  expect_error(back_transform_estimates(input, scale_info = data.frame(outcome = "y", orientation = 0)), "orientation")
  expect_error(back_transform_estimates(input, scale_info = data.frame(outcome = "y", transformation = "square")), "transformation")
})

test_that("legacy inference uses unstandardised sources and warns about recomputation", {
  input <- ate_reporting_fixture(outcome = "t2_y_z_r")
  source <- data.frame(t2_y = c(-2, 0, 2), t2_y_z = c(-1, 0, 1))
  expect_warning(out <- back_transform_estimates(input, source), "Inferring legacy")
  expect_equal(out$reported_estimate, -1)
  expect_equal(out$reported_lower, -2.6)
  expect_equal(out$reported_upper, 0.2)
  source$y <- c(1, 2, 3)
  expect_error(suppressWarnings(back_transform_estimates(input, source)), "Ambiguous")
  expect_error(suppressWarnings(back_transform_estimates(input, data.frame(t2_y_z = c(-1, 0, 1)))), "unstandardised")
})

test_that("plain-log lookup works and financial variable names never invent means", {
  source <- data.frame(t2_log_charity_donate = log1p(c(0, 2, 8)))
  info <- get_outcome_transformation_info("t2_log_charity_donate", source)
  expect_equal(info$log_mean, mean(source$t2_log_charity_donate))
  expect_equal(info$display_mean, mean(c(0, 2, 8)))
  expect_null(info$use_display_mean)
  expect_null(info$log_mean_display)
  expect_error(get_outcome_transformation_info("t2_log_charity_donate_z", data.frame(t2_charity_donate = c(0, 2, 8))), "unstandardised")
})

test_that("outcome keys remain stable after display labels change", {
  input <- ate_reporting_fixture()
  input$original_var_name <- "y"
  input$outcome <- "A reader-facing label"
  out <- back_transform_estimates(input, scale_info = data.frame(outcome = "y", scale = 2))
  expect_equal(out$reported_estimate, 1)
  expect_equal(out$outcome, input$outcome)
})


test_that("confidence coverage metadata and empty tables are preserved", {
  input <- ate_reporting_fixture()
  input$confidence_level <- 0.90
  out <- back_transform_estimates(input, scale_info = data.frame(outcome = "y", scale = 2))
  expect_identical(out$confidence_level, 0.90)
  expect_equal(out$reported_lower, -0.2)
  expect_equal(out$reported_upper, 2.6)
  empty <- back_transform_estimates(input[FALSE, ])
  expect_equal(nrow(empty), 0)
  expect_true(all(c("reported_estimate", "reported_lower", "reported_upper") %in% names(empty)))
})

test_that("ratio contrasts use the same detected column in sorting and reporting", {
  input <- ate_reporting_fixture(estimate = 2, lower = 1.5, upper = 3)
  out <- group_tab(input, type = "RR", order = "magnitude_desc")
  expect_equal(out$ATE, 2)
  expect_equal(as.character(out$Estimate), "positive")
  ratio <- input
  names(ratio)[names(ratio) == "ATE"] <- "E[Y(1)]/E[Y(0)]"
  expect_error(group_tab(ratio, type = "RD"), "requires type")
  expect_equal(group_tab(ratio, type = "RR")[["E[Y(1)]/E[Y(0)]"]], 2)
})

test_that("legacy policy reporting refuses unsupported conversions and permits model scale", {
  for (name in c("model_t2_log_income_z", "model_t2_y_z_r", "model_t2_log_income")) {
    expect_error(margot_assert_policy_reporting_scale(name, data.frame(y = 1)), "original_df = NULL")
    expect_null(margot_assert_policy_reporting_scale(name, NULL))
  }
  expect_null(margot_assert_policy_reporting_scale("model_t2_y_z", data.frame(y = 1)))
  input <- list(leaf_idx = list(), predictions = integer(), conditional_means = matrix(numeric(), ncol = 2),
                act_labels = c("Control", "Treat"), leaf_names = character(), model_name = "model_t2_log_income_z")
  expect_error(do.call(compute_leaf_means, c(input, list(original_df = data.frame(t2_log_income = log1p(c(0, 2)))))), "original_df = NULL")
  expect_identical(do.call(compute_leaf_means, c(input, list(original_df = NULL))), "")
  input$model_name <- "model_t2_y_z"
  expect_identical(do.call(compute_leaf_means, c(input, list(original_df = data.frame(t2_y = c(1, 3))))), "")
})


test_that("exported policy reporters cannot swallow an unsupported scale refusal", {
  original <- data.frame(t2_log_income = log1p(c(0, 2)))
  expect_error(margot_interpret_policy_tree(model = list(), model_name = "model_t2_log_income_z", original_df = original), "original_df = NULL")
  object <- structure(list(results = list(model_t2_log_income_z = list())), class = "margot_stability_policy_tree")
  expect_error(margot_policy_summary_report(object, original_df = original), "original_df = NULL")
})


test_that("development plot table and prose follow the graph from top to bottom", {
  input <- rbind(ate_reporting_fixture("a", 0.25, 0.1, 0.4), ate_reporting_fixture("b", 0.5, 0.2, 0.7))
  input$E_Val_bound <- 1.5
  metadata <- data.frame(outcome = c("a", "b"), scale = c(2, 3))
  output <- margot_plot_dev(input, adjust = "none", order = "custom", custom_order = c("b", "a"),
                            options = list(use_title_case = FALSE), scale_info = metadata)
  top_to_bottom <- rev(as.character(output$plot$data$outcome))
  expect_identical(as.character(output$transformed_table$outcome), top_to_bottom)
  expect_equal(output$transformed_table$reported_estimate, c(0.5, 1.5))
  expect_lt(regexpr("- a:", output$interpretation)[[1]], regexpr("- b:", output$interpretation)[[1]])
})
