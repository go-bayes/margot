.test_evalue_row <- function(value) {
  as.data.frame(value)[2, c("point", "lower", "upper"), drop = FALSE]
}

.test_evalue_bound <- function(value) {
  row <- .test_evalue_row(value)
  dplyr::coalesce(row$lower, row$upper, 1)
}

test_that("model E-values use unrounded estimates and standard errors", {
  model_output <- data.frame(estimate = 0.00049, std.err = 0.0002)
  expected <- EValue::evalues.OLS(
    est = model_output$estimate,
    se = model_output$std.err,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .test_evalue_row(expected)

  out <- margot_model_evalue(model_output, scale = "RD", new_name = "test")

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, model_output$estimate)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .test_evalue_bound(expected), tolerance = 1e-14)
  expect_gt(out$E_Value, 1)
  expect_gt(out$E_Val_bound, 1)
})

test_that("LMTP summaries retain precision through E-value calculation", {
  lmtp_output <- list(vals = list(
    theta = 0.00149,
    std.error = 0.0004,
    conf.low = 0.0007060144,
    conf.high = 0.0022739856
  ))
  expected <- EValue::evalues.OLS(
    est = lmtp_output$vals$theta,
    se = lmtp_output$vals$std.error,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .test_evalue_row(expected)

  tab <- margot:::margot_lmtp_tab(lmtp_output, scale = "RD", new_name = "test")
  out <- margot_lmtp_evalue(lmtp_output, scale = "RD", new_name = "test")

  expect_identical(tab$`E[Y(1)]-E[Y(0)]`, lmtp_output$vals$theta)
  expect_identical(tab$standard_error, lmtp_output$vals$std.error)
  expect_identical(out$`E[Y(1)]-E[Y(0)]`, lmtp_output$vals$theta)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .test_evalue_bound(expected), tolerance = 1e-14)
})

test_that("deprecated marginal tabulation computes before formatting", {
  x <- data.frame(
    Estimate = 0.0123456,
    `2.5 %` = 0.00387644,
    `97.5 %` = 0.02081476,
    check.names = FALSE,
    row.names = "RD"
  )
  se <- (x$`97.5 %` - x$`2.5 %`) / 3.92
  expected <- EValue::evalues.OLS(
    est = x$Estimate,
    se = se,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .test_evalue_row(expected)

  out <- margot:::tab_engine_marginal(
    x = x,
    new_name = "test",
    type = "RD"
  )

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, x$Estimate)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .test_evalue_bound(expected), tolerance = 1e-14)
})

test_that("corrected tables retain exact values for downstream E-values", {
  input <- data.frame(
    `E[Y(1)]-E[Y(0)]` = 0.0123456,
    `2.5 %` = 0.00387644,
    `97.5 %` = 0.02081476,
    check.names = FALSE
  )

  out <- margot_correct_combined_table(
    input,
    adjust = "bonferroni",
    scale = "RD"
  )
  se <- (out$`97.5 %` - out$`E[Y(1)]-E[Y(0)]`) / stats::qnorm(0.975)
  expected <- EValue::evalues.OLS(
    est = input$`E[Y(1)]-E[Y(0)]`,
    se = se,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .test_evalue_row(expected)

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, input$`E[Y(1)]-E[Y(0)]`)
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(out$E_Val_bound, .test_evalue_bound(expected), tolerance = 1e-14)
})

test_that("multi-bias E-values consume the exact corrected RR table", {
  input <- data.frame(
    `E[Y(1)]/E[Y(0)]` = 1.234567,
    `2.5 %` = 1.045678,
    `97.5 %` = 1.457891,
    check.names = FALSE
  )
  biases <- EValue::confounding()
  corrected <- margot_correct_combined_table(
    input,
    adjust = "bonferroni",
    scale = "RR"
  )
  expected <- EValue::multi_evalues.RR(
    biases,
    est = corrected$`E[Y(1)]/E[Y(0)]`,
    lo = corrected$`2.5 %`,
    hi = corrected$`97.5 %`,
    true = 1
  )
  expected_row <- .test_evalue_row(expected)

  out <- margot_multi_evalue(
    input,
    scale = "RR",
    biases = biases,
    apply_bonferroni_first = TRUE,
    notes = FALSE
  )$table

  expect_equal(out$`2.5 %`, corrected$`2.5 %`, tolerance = 1e-14)
  expect_equal(out$multi_E_value_point, expected_row$point, tolerance = 1e-14)
  expect_equal(out$multi_E_value_bound, .test_evalue_bound(expected), tolerance = 1e-14)
})
