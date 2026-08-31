.test_multi_evalue_row <- function(value) {
  as.data.frame(value)[2, c("point", "lower", "upper"), drop = FALSE]
}

.test_multi_evalue_bound <- function(value) {
  row <- .test_multi_evalue_row(value)
  dplyr::coalesce(row$lower, row$upper, 1)
}

test_that("RD multi-bias E-values use the exported EValue conversion", {
  input <- data.frame(
    `E[Y(1)]-E[Y(0)]` = 0.2,
    `2.5 %` = 0.05,
    `97.5 %` = 0.35,
    check.names = FALSE
  )
  biases <- EValue::confounding()
  corrected <- margot_correct_combined_table(
    input,
    adjust = "bonferroni",
    scale = "RD"
  )
  rr_point <- as.numeric(EValue::toRR(
    EValue::OLS(corrected$`E[Y(1)]-E[Y(0)]`, sd = 1),
    delta = 1
  ))
  rr_low <- as.numeric(EValue::toRR(
    EValue::OLS(corrected$`2.5 %`, sd = 1),
    delta = 1
  ))
  rr_high <- as.numeric(EValue::toRR(
    EValue::OLS(corrected$`97.5 %`, sd = 1),
    delta = 1
  ))
  expected <- EValue::multi_evalues.RR(
    biases,
    est = rr_point,
    lo = rr_low,
    hi = rr_high,
    true = 1
  )
  expected_row <- .test_multi_evalue_row(expected)

  out <- margot_multi_evalue(
    input,
    scale = "RD",
    biases = biases,
    notes = FALSE
  )$table

  expect_equal(out$multi_E_value_point, expected_row$point, tolerance = 1e-14)
  expect_equal(
    out$multi_E_value_bound,
    .test_multi_evalue_bound(expected),
    tolerance = 1e-14
  )
})

test_that("disabling Bonferroni is a genuine no-adjustment path", {
  input <- data.frame(
    `E[Y(1)]-E[Y(0)]` = 0.2,
    `2.5 %` = 0.05,
    `97.5 %` = 0.35,
    check.names = FALSE
  )
  se <- (input$`97.5 %` - input$`E[Y(1)]-E[Y(0)]`) /
    stats::qnorm(0.975)
  expected <- EValue::evalues.OLS(
    est = input$`E[Y(1)]-E[Y(0)]`,
    se = se,
    sd = 1,
    delta = 1,
    true = 0
  )
  expected_row <- .test_multi_evalue_row(expected)

  corrected <- margot_correct_combined_table(
    input,
    adjust = "none",
    scale = "RD"
  )
  out <- margot_multi_evalue(
    input,
    scale = "RD",
    apply_bonferroni_first = FALSE,
    notes = FALSE
  )$table

  expect_identical(corrected$`2.5 %`, input$`2.5 %`)
  expect_identical(corrected$`97.5 %`, input$`97.5 %`)
  expect_identical(out$`2.5 %`, input$`2.5 %`)
  expect_identical(out$`97.5 %`, input$`97.5 %`)
  expect_true(is.na(out$alpha_fwer))
  expect_equal(out$E_Value, expected_row$point, tolerance = 1e-14)
  expect_equal(
    out$E_Val_bound,
    .test_multi_evalue_bound(expected),
    tolerance = 1e-14
  )
})

test_that("supplied multiplicity controls Bonferroni adjustment", {
  input <- data.frame(
    `E[Y(1)]-E[Y(0)]` = 0.2,
    `2.5 %` = 0.05,
    `97.5 %` = 0.35,
    check.names = FALSE
  )
  m <- 20L
  z_original <- stats::qnorm(0.975)
  z_adjusted <- stats::qnorm(1 - 0.05 / (2 * m))
  half_width <- (input$`97.5 %` - input$`2.5 %`) / 2
  expected_low <- input$`E[Y(1)]-E[Y(0)]` -
    half_width * z_adjusted / z_original
  expected_high <- input$`E[Y(1)]-E[Y(0)]` +
    half_width * z_adjusted / z_original

  default <- margot_multi_evalue(
    input,
    scale = "RD",
    notes = FALSE
  )$table
  out <- margot_multi_evalue(
    input,
    scale = "RD",
    m = m,
    notes = FALSE
  )$table

  expect_lt(out$`2.5 %`, default$`2.5 %`)
  expect_gt(out$`97.5 %`, default$`97.5 %`)
  expect_equal(out$`2.5 %`, expected_low, tolerance = 1e-14)
  expect_equal(out$`97.5 %`, expected_high, tolerance = 1e-14)
  expect_identical(out$m, m)
  expect_identical(out$m_supplied, m)
  expect_identical(out$m_realised, m)
  expect_true(is.na(default$m_supplied))
  expect_identical(default$m, 1L)
  expect_identical(default$m_realised, 1L)
})

test_that("multiplicity is a positive whole number covering every row", {
  one_row <- data.frame(
    `E[Y(1)]-E[Y(0)]` = 0.2,
    `2.5 %` = 0.05,
    `97.5 %` = 0.35,
    check.names = FALSE
  )
  two_rows <- rbind(one_row, one_row)

  for (invalid in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), "2")) {
    expect_error(
      margot_multi_evalue(one_row, scale = "RD", m = invalid, notes = FALSE),
      "positive whole number",
      fixed = TRUE
    )
  }
  expect_error(
    margot_multi_evalue(two_rows, scale = "RD", m = 1, notes = FALSE),
    "at least the number of rows",
    fixed = TRUE
  )
})
