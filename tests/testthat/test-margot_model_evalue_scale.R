.model_evalue_test_row <- function(value) {
  as.data.frame(value)[2, c("point", "lower", "upper"), drop = FALSE]
}

.model_evalue_test_bound <- function(value) {
  row <- .model_evalue_test_row(value)
  dplyr::coalesce(row$lower, row$upper, 1)
}

test_that("direct model E-values honour RD scale", {
  model_output <- data.frame(estimate = 0.25, std.err = 0.05)
  expected <- EValue::evalues.OLS(
    est = model_output$estimate,
    se = model_output$std.err,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .model_evalue_test_row(expected)

  out <- margot_model_evalue(model_output, scale = "RD", new_name = "direct")

  expect_named(out, c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound"))
  expect_identical(out$`E[Y(1)]-E[Y(0)]`, model_output$estimate)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .model_evalue_test_bound(expected), tolerance = 1e-14)
})

test_that("direct model E-values honour RR scale", {
  model_output <- data.frame(estimate = 1.25, std.err = 0.05)
  conf.low <- model_output$estimate - stats::qnorm(0.975) * model_output$std.err
  conf.high <- model_output$estimate + stats::qnorm(0.975) * model_output$std.err
  expected <- EValue::evalues.RR(
    est = model_output$estimate,
    lo = conf.low,
    hi = conf.high,
    true = 1
  )
  expected_row <- .model_evalue_test_row(expected)

  out <- margot_model_evalue(model_output, scale = "RR", new_name = "direct")

  expect_named(out, c("E[Y(1)]/E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound"))
  expect_identical(out$`E[Y(1)]/E[Y(0)]`, model_output$estimate)
  expect_identical(out$`2.5 %`, conf.low)
  expect_identical(out$`97.5 %`, conf.high)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .model_evalue_test_bound(expected), tolerance = 1e-14)
})

test_that("direct RR input rejects impossible ratio summaries", {
  expect_error(
    margot_model_evalue(data.frame(estimate = 0, std.err = 0.01), scale = "RR"),
    "strictly positive"
  )
  expect_error(
    margot_model_evalue(data.frame(estimate = 1.2, std.err = -0.01), scale = "RR"),
    "non-negative"
  )
  expect_error(
    margot_model_evalue(data.frame(estimate = 0.1, std.err = 0.2), scale = "RR"),
    "confidence limits must be strictly positive"
  )
})

test_that("causal forest E-values always use the additive contract", {
  testthat::local_mocked_bindings(
    average_treatment_effect = function(...) c(estimate = 0.2, std.err = 0.05),
    .package = "margot"
  )
  forest <- structure(list(), class = "causal_forest")

  out_rd <- margot_model_evalue(forest, scale = "RD", new_name = "forest")
  out_rr <- margot_model_evalue(forest, scale = "RR", new_name = "forest")

  expect_named(out_rr, c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound"))
  expect_identical(out_rr, out_rd)
})

test_that("multi-arm causal forest E-values always use the additive contract", {
  testthat::local_mocked_bindings(
    average_treatment_effect = function(...) {
      data.frame(
        estimate = c(0.2, -0.1),
        std.err = c(0.05, 0.04),
        contrast = c("B - A", "C - A")
      )
    },
    .package = "margot"
  )
  forest <- structure(list(), class = "multi_arm_causal_forest")

  out_rd <- margot_model_evalue(forest, scale = "RD", new_name = "forest")
  out_rr <- margot_model_evalue(forest, scale = "RR", new_name = "forest")

  expect_named(out_rr, c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound"))
  expect_identical(out_rr, out_rd)
  expect_identical(rownames(out_rr), c("forest - B - A", "forest - C - A"))
})
