test_that("risk-ratio E-values match locked equation fixtures", {
  expect_equal(
    margot:::.margot_evalues_rr(2, lo = 1.25, hi = 3.2),
    c(E_Value = 3.4142135623731, E_Val_bound = 1.80901699437495),
    tolerance = 1e-14
  )
  expect_equal(
    margot:::.margot_evalues_rr(0.5, lo = 0.3, hi = 0.8),
    c(E_Value = 3.4142135623731, E_Val_bound = 1.80901699437495),
    tolerance = 1e-14
  )
  expect_equal(
    margot:::.margot_evalues_rr(1.2, lo = 0.9, hi = 1.6),
    c(E_Value = 1.68989794855664, E_Val_bound = 1),
    tolerance = 1e-14
  )
})

test_that("continuous-outcome E-values match locked approximation fixtures", {
  expect_equal(
    margot:::.margot_evalues_ols(0.5, se = 0.1, sd = 2, delta = 1),
    c(E_Value = 1.82177532158364, E_Val_bound = 1.56160741296469),
    tolerance = 1e-14
  )
  expect_equal(
    margot:::.margot_evalues_ols(-0.5, se = 0.1, sd = 2, delta = 1),
    c(E_Value = 1.82177532158364, E_Val_bound = 1.56160741296469),
    tolerance = 1e-14
  )
  expect_equal(
    margot:::.margot_evalues_ols(0, se = 0.1, sd = 1, delta = 1),
    c(E_Value = 1, E_Val_bound = 1),
    tolerance = 1e-14
  )
})

test_that("E-value inputs are validated at their scientific scale", {
  expect_error(margot:::.margot_evalues_rr(-1), "cannot be negative")
  expect_error(margot:::.margot_evalues_rr(1.5, lo = 1.6, hi = 2), "within")
  expect_error(margot:::.margot_evalues_rr(1.5, lo = 2, hi = 1), "less than")
  expect_error(margot:::.margot_evalues_ols(0.2, se = -0.1, sd = 1), "non-negative")
  expect_error(margot:::.margot_evalues_ols(0.2, se = 0.1, sd = 0), "positive")
  expect_equal(margot:::.margot_evalue_threshold(0), Inf)
})

test_that("model E-values use unrounded estimates and standard errors", {
  model_output <- data.frame(estimate = 0.00049, std.err = 0.0002)
  expected <- margot:::.margot_evalues_ols(
    model_output$estimate,
    se = model_output$std.err,
    sd = 1,
    delta = 1
  )

  out <- margot_model_evalue(model_output, scale = "RD", new_name = "test")

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, model_output$estimate)
  expect_equal(out$E_Value, expected[["E_Value"]], tolerance = 1e-14)
  expect_equal(out$E_Val_bound, expected[["E_Val_bound"]], tolerance = 1e-14)
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
  expected <- margot:::.margot_evalues_ols(
    lmtp_output$vals$theta,
    se = lmtp_output$vals$std.error,
    sd = 1,
    delta = 1
  )

  tab <- margot:::margot_lmtp_tab(lmtp_output, scale = "RD", new_name = "test")
  out <- margot_lmtp_evalue(lmtp_output, scale = "RD", new_name = "test")

  expect_identical(tab$`E[Y(1)]-E[Y(0)]`, lmtp_output$vals$theta)
  expect_identical(tab$standard_error, lmtp_output$vals$std.error)
  expect_identical(out$`E[Y(1)]-E[Y(0)]`, lmtp_output$vals$theta)
  expect_equal(out$E_Value, expected[["E_Value"]], tolerance = 1e-14)
  expect_equal(out$E_Val_bound, expected[["E_Val_bound"]], tolerance = 1e-14)
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
  expected <- margot:::.margot_evalues_ols(x$Estimate, se = se, sd = 1, delta = 1)

  out <- margot:::tab_engine_marginal(
    x = x,
    new_name = "test",
    type = "RD"
  )

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, x$Estimate)
  expect_equal(out$E_Value, expected[["E_Value"]], tolerance = 1e-14)
  expect_equal(out$E_Val_bound, expected[["E_Val_bound"]], tolerance = 1e-14)
})

test_that("corrected tables retain unrounded values for downstream E-values", {
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
  expected <- margot:::.margot_evalues_ols(
    input$`E[Y(1)]-E[Y(0)]`,
    se = se,
    sd = 1,
    delta = 1
  )

  expect_identical(out$`E[Y(1)]-E[Y(0)]`, input$`E[Y(1)]-E[Y(0)]`)
  expect_equal(out$E_Value, expected[["E_Value"]], tolerance = 1e-14)
  expect_equal(out$E_Val_bound, expected[["E_Val_bound"]], tolerance = 1e-14)
})

test_that("internal E-values agree with the optional reference implementation", {
  skip_if_not_installed("EValue")

  rr_cases <- list(
    c(est = 2, lo = 1.25, hi = 3.2),
    c(est = 0.5, lo = 0.3, hi = 0.8),
    c(est = 1.2, lo = 0.9, hi = 1.6),
    c(est = 0.8, lo = 0.6, hi = 1.1)
  )
  for (case in rr_cases) {
    reference <- as.data.frame(EValue::evalues.RR(
      case[["est"]],
      lo = case[["lo"]],
      hi = case[["hi"]],
      true = 1
    ))[2, , drop = FALSE]
    expected_bound <- dplyr::coalesce(reference$lower, reference$upper, 1)
    actual <- margot:::.margot_evalues_rr(case[["est"]], case[["lo"]], case[["hi"]])

    expect_equal(actual[["E_Value"]], reference$point, tolerance = 1e-14)
    expect_equal(actual[["E_Val_bound"]], expected_bound, tolerance = 1e-14)
  }

  ols_cases <- list(
    c(est = 0.5, se = 0.1, sd = 2, delta = 1),
    c(est = -0.5, se = 0.1, sd = 2, delta = 1),
    c(est = 0.00049, se = 0.0002, sd = 1, delta = 1)
  )
  for (case in ols_cases) {
    reference <- as.data.frame(EValue::evalues.OLS(
      case[["est"]],
      se = case[["se"]],
      sd = case[["sd"]],
      delta = case[["delta"]],
      true = 0
    ))[2, , drop = FALSE]
    expected_bound <- dplyr::coalesce(reference$lower, reference$upper, 1)
    actual <- margot:::.margot_evalues_ols(
      case[["est"]],
      se = case[["se"]],
      sd = case[["sd"]],
      delta = case[["delta"]]
    )

    expect_equal(actual[["E_Value"]], reference$point, tolerance = 1e-14)
    expect_equal(actual[["E_Val_bound"]], expected_bound, tolerance = 1e-14)
  }
})

test_that("multi-bias E-values consume the unrounded corrected RR table", {
  skip_if_not_installed("EValue")

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
  expected <- as.data.frame(EValue::multi_evalues.RR(
    biases,
    est = corrected$`E[Y(1)]/E[Y(0)]`,
    lo = corrected$`2.5 %`,
    hi = corrected$`97.5 %`,
    true = 1
  ))[2, , drop = FALSE]

  out <- margot_multi_evalue(
    input,
    scale = "RR",
    biases = biases,
    apply_bonferroni_first = TRUE,
    notes = FALSE
  )$table

  expect_equal(out$`2.5 %`, corrected$`2.5 %`, tolerance = 1e-14)
  expect_equal(out$multi_E_value_point, expected$point, tolerance = 1e-14)
  expect_equal(
    out$multi_E_value_bound,
    dplyr::coalesce(expected$lower, expected$upper, 1),
    tolerance = 1e-14
  )
})
