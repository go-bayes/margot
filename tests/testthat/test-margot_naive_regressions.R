# simulated data with a known slope and unequal survey weights
make_naive_data <- function(n = 300, seed = 2026) {
  set.seed(seed)
  a <- rnorm(n)
  data.frame(
    a = a,
    y1 = 0.4 * a + rnorm(n),
    y2 = -0.2 * a + rnorm(n),
    w = runif(n, 0.2, 3)
  )
}

run_naive <- function(...) {
  suppressMessages(margot_naive_regressions(..., save_output = FALSE))
}

effect_col <- "E[Y(1)]-E[Y(0)]"

test_that("weights = NULL reproduces the unweighted lm() fit", {
  d <- make_naive_data()
  out <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"))
  tab <- out$combined_table

  for (y in c("y1", "y2")) {
    fit <- lm(stats::as.formula(paste(y, "~ a")), data = d)
    ci <- confint(fit)["a", ]
    expect_equal(tab[y, effect_col], round(unname(coef(fit)["a"]), 3))
    expect_equal(tab[y, "standard_error"], round(summary(fit)$coefficients["a", "Std. Error"], 3))
    expect_equal(tab[y, "2.5 %"], round(unname(ci[1]), 3))
    expect_equal(tab[y, "97.5 %"], round(unname(ci[2]), 3))
    expect_null(stats::weights(out$models[[paste0("model_", y)]]))
  }
})

test_that("weighted estimates are weighted least squares with design-based intervals", {
  d <- make_naive_data()
  out_col <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"), weights = "w")
  out_vec <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"), weights = d$w)
  expect_identical(out_col$combined_table, out_vec$combined_table)

  tab <- out_col$combined_table
  for (y in c("y1", "y2")) {
    fit <- lm(stats::as.formula(paste(y, "~ a")), data = d, weights = w)

    # sandwich variance with the n / (n - 1) correction used by survey::svyglm()
    x <- model.matrix(fit)
    e <- residuals(fit)
    n <- nrow(x)
    bread <- solve(crossprod(x, x * d$w))
    v <- bread %*% crossprod(x * (d$w * e)) %*% bread * n / (n - 1)
    se <- sqrt(v["a", "a"])
    est <- unname(coef(fit)["a"])
    half <- stats::qt(0.975, df = n - 2) * se

    expect_equal(tab[y, effect_col], round(est, 3))
    expect_equal(tab[y, "standard_error"], round(se, 3))
    expect_equal(tab[y, "2.5 %"], round(est - half, 3))
    expect_equal(tab[y, "97.5 %"], round(est + half, 3))
    expect_equal(stats::weights(out_col$models[[paste0("model_", y)]]), d$w)
  }

  # rows dropped for a missing outcome stay in the design size, as in survey::svyglm()
  d_na <- d
  d_na$y1[1:10] <- NA
  tab_na <- run_naive(data = d_na, exposure_var = "a", outcome_vars = "y1", weights = "w")$combined_table
  fit_na <- lm(y1 ~ a, data = d_na, weights = w)
  x <- model.matrix(fit_na)
  w_used <- weights(fit_na)
  bread <- solve(crossprod(x, x * w_used))
  v <- bread %*% crossprod(x * (w_used * residuals(fit_na))) %*% bread * nrow(d_na) / (nrow(d_na) - 1)
  expect_equal(tab_na["y1", "standard_error"], round(sqrt(v["a", "a"]), 3))

  # the weighted fit should differ from the unweighted one on these data
  unweighted <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"))
  expect_false(isTRUE(all.equal(tab, unweighted$combined_table)))
})

test_that("a negative coefficient_scale flips the sign and keeps the limits in order", {
  d <- make_naive_data()
  base <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"))$combined_table

  for (w in list(NULL, "w")) {
    ref <- run_naive(data = d, exposure_var = "a", outcome_vars = c("y1", "y2"), weights = w)$combined_table
    flipped <- run_naive(
      data = d, exposure_var = "a", outcome_vars = c("y1", "y2"),
      weights = w, coefficient_scale = -1
    )$combined_table

    expect_equal(flipped[[effect_col]], -ref[[effect_col]])
    expect_equal(flipped$standard_error, ref$standard_error)
    expect_true(all(flipped$`2.5 %` <= flipped$`97.5 %`))
    expect_equal(flipped$`2.5 %`, -ref$`97.5 %`)
    expect_equal(flipped$`97.5 %`, -ref$`2.5 %`)
  }
  expect_true(all(base$`2.5 %` <= base$`97.5 %`))
})

test_that("coefficient_scale = 4 multiplies the estimate and limits by four", {
  d <- make_naive_data()
  fit <- lm(y1 ~ a, data = d)
  ci <- confint(fit)["a", ]
  scaled <- run_naive(data = d, exposure_var = "a", outcome_vars = "y1", coefficient_scale = 4)$combined_table

  expect_equal(scaled["y1", effect_col], round(4 * unname(coef(fit)["a"]), 3))
  expect_equal(scaled["y1", "standard_error"], round(4 * summary(fit)$coefficients["a", "Std. Error"], 3))
  expect_equal(scaled["y1", "2.5 %"], round(4 * unname(ci[1]), 3))
  expect_equal(scaled["y1", "97.5 %"], round(4 * unname(ci[2]), 3))
})

test_that("bad weights raise a classed error before any model is fitted", {
  d <- make_naive_data(n = 50)
  bad <- list(
    missing = replace(d$w, 3, NA),
    zero = replace(d$w, 3, 0),
    negative = replace(d$w, 3, -1),
    infinite = replace(d$w, 3, Inf),
    wrong_length = d$w[-1],
    unknown_column = "not_a_column",
    character_column = "label"
  )
  d$label <- "x"

  for (w in bad) {
    expect_error(
      run_naive(data = d, exposure_var = "a", outcome_vars = "y1", weights = w),
      class = "margot_error_invalid_input"
    )
  }

  # a weight column with a missing value is refused too
  d$w_missing <- replace(d$w, 5, NA)
  expect_error(
    run_naive(data = d, exposure_var = "a", outcome_vars = "y1", weights = "w_missing"),
    class = "margot_error_invalid_input"
  )
})

test_that("the combined table is accepted by margot_plot()", {
  d <- make_naive_data()
  tab <- run_naive(
    data = d, exposure_var = "a", outcome_vars = c("y1", "y2"),
    weights = "w", coefficient_scale = -1
  )$combined_table

  expect_true(all(c(effect_col, "2.5 %", "97.5 %", "E_Value", "E_Val_bound") %in% names(tab)))

  plot_out <- suppressMessages(margot_plot(tab, type = "RD", rename_ate = "Naive Association"))
  expect_s3_class(plot_out$plot, "ggplot")
  expect_equal(nrow(plot_out$transformed_table), 2L)
})
