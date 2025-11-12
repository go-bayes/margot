test_that("margot_compute_ipsi_probability reproduces IPSI mechanics", {
  trans <- data.frame(
    `From / To` = c("State 0", "State 1"),
    `State 0` = c(27380, 330),
    `State 1` = c(845, 170),
    Total = c(28225, 500),
    check.names = FALSE
  )

  res <- margot_compute_ipsi_probability(trans, deltas = c(2, 5, 10))

  expect_s3_class(res, "data.frame")
  expect_equal(res$delta, c(2, 5, 10))
  expect_equal(res$delta_inverse, c(0.5, 0.2, 0.1))
  expect_equal(res$natural_p, rep(845 / 28225, 3))
  expect_equal(res$natural_p_l[1], stats::binom.test(845, 28225)$conf.int[1])
  expect_equal(res$natural_p_u[1], stats::binom.test(845, 28225)$conf.int[2])
  expect_equal(res$counterfactual_p,
               c(0.5149689991, 0.8059875996, 0.9029937998))
  expect_equal(res$fold_increase,
               c(17.20118343, 26.92189349, 30.16213018))

  counts <- attr(res, "counts")
  expect_equal(counts$initiations, 845)
  expect_equal(counts$non_attenders, 28225)
  expect_equal(counts$natural_p, 845 / 28225)
  expect_equal(counts$natural_p_l, res$natural_p_l[1])
  expect_equal(counts$natural_p_u, res$natural_p_u[1])
})

test_that("margot_compute_ipsi_probability accepts knitr_kable tables", {
  trans <- data.frame(
    `From / To` = c("State 0", "State 1"),
    `State 0` = c(16631, 330),
    `State 1` = c(355, 170),
    Total = c(16986, 500),
    check.names = FALSE
  )
  kb <- knitr::kable(trans, format = "markdown")
  attr(kb, "table_data") <- trans

  res <- margot_compute_ipsi_probability(kb, deltas = 2)
  expect_equal(res$counterfactual_p, 1 - (1 - 355 / 16986) / 2)
})

test_that("margot_compute_ipsi_probability validates inputs", {
  bad_df <- data.frame(foo = 1)
  expect_error(margot_compute_ipsi_probability(bad_df),
               "missing required columns")

  trans <- data.frame(
    `From / To` = "State 0",
    `State 1` = 0,
    Total = 0,
    check.names = FALSE
  )
  expect_error(margot_compute_ipsi_probability(trans),
               "must be positive numbers")

  good_df <- data.frame(
    `From / To` = "State 0",
    `State 1` = 0,
    Total = 10,
    check.names = FALSE
  )
  expect_error(margot_compute_ipsi_probability(good_df, deltas = c(1, 2)),
               "greater than 1")
})
