test_that("margot_transition_ipsi_summary works with margot_transition_table object", {
  dt <- data.frame(
    id = rep(1:5, each = 3),
    wave = rep(c(2018, 2019, 2022), times = 5),
    religion = c(
      0, 0, 1,
      0, 1, 1,
      1, 1, 1,
      0, 0, 0,
      0, 0, 1
    ),
    observed = 1
  )

  transitions <- margot_transition_table(
    dt,
    state_var = "religion",
    id_var = "id",
    wave_var = "wave",
    observed_var = "observed",
    observed_val = 1,
    waves = c(2018, 2019, 2022)
  )

  summary <- margot_transition_ipsi_summary(transitions, deltas = c(2, 4))
  tbl <- summary$table
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("table_index", "delta", "natural_p", "counterfactual_p",
                    "initiations", "non_attenders") %in% names(tbl)))
  expect_equal(unique(tbl$delta), c(2, 4))
  expect_equal(length(unique(tbl$table_index)), 2)
  expect_equal(nrow(tbl), 4) # 2 tables * 2 deltas
  expect_true(length(summary$report) > 0)
  expect_true(any(grepl("\\\\to", summary$report)))
  expect_true(any(grepl("\\\\delta", summary$report)))
})

test_that("margot_transition_ipsi_summary accepts list inputs", {
  trans1 <- data.frame(
    `From / To` = c("State 0", "State 1"),
    `State 0` = c(10, 0),
    `State 1` = c(2, 5),
    Total = c(12, 5),
    check.names = FALSE
  )
  res <- margot_transition_ipsi_summary(list(trans1), deltas = 2)
  tbl <- res$table
  expect_equal(tbl$delta, 2)
  expect_equal(tbl$initiations, 2)
  expect_equal(tbl$non_attenders, 12)
  expect_true(length(res$report) > 0)

  kb <- knitr::kable(trans1, format = "markdown")
  attr(kb, "table_data") <- trans1
  res_kb <- margot_transition_ipsi_summary(list(kb), deltas = 2)
  expect_equal(res_kb$table$counterfactual_p, tbl$counterfactual_p)
})

test_that("margot_transition_ipsi_summary pretty output formats strings", {
  dt <- data.frame(
    id = rep(1:3, each = 2),
    wave = rep(c(2018, 2019), times = 3),
    religion = c(0, 0, 1, 1, 0, 1),
    observed = 1
  )
  transitions <- margot_transition_table(
    dt,
    state_var = "religion",
    id_var = "id",
    wave_var = "wave",
    observed_var = "observed",
    observed_val = 1,
    waves = c(2018, 2019)
  )
  out <- margot_transition_ipsi_summary(transitions, deltas = 2, pretty = TRUE)
  pretty_tbl <- out$table
  expect_true(all(c("Wave pair", "Delta", "Natural p", "Counterfactual p") %in% names(pretty_tbl)))
  expect_true(all(grepl("%", pretty_tbl$`Natural p`, fixed = TRUE)))
  expect_true(any(grepl("\\\\delta", out$report)))
  raw <- attr(pretty_tbl, "raw")
  expect_s3_class(raw, "data.frame")
  expect_equal(raw$delta, 2)
})
