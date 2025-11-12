test_that("transition table helper exposes raw counts", {
  dt <- data.frame(
    id = rep(1:4, each = 3),
    wave = rep(c(2018, 2019, 2022), times = 4),
    religion = c(
      0, 0, 1,
      0, 1, 1,
      1, 1, 1,
      0, 0, 0
    ),
    observed = 1
  )

  tables <- margot_transition_table(
    dt,
    state_var = "religion",
    id_var = "id",
    wave_var = "wave",
    observed_var = "observed",
    observed_val = 1,
    waves = c(2018, 2019, 2022)
  )

  tab1 <- tables$get_table_data(which = 1)
  expect_s3_class(tab1, "data.frame")
  expect_true(all(c("From / To", "State 0", "State 1", "Total") %in% names(tab1)))

  expect_error(tables$get_table_data(which = 3), "between 1 and 2")
})

test_that("transition table helper computes IPSI probabilities", {
  dt <- data.frame(
    id = rep(1:4, each = 3),
    wave = rep(c(2018, 2019, 2022), times = 4),
    religion = c(
      0, 0, 1,
      0, 1, 1,
      1, 1, 1,
      0, 0, 0
    ),
    observed = 1
  )

  tables <- margot_transition_table(
    dt,
    state_var = "religion",
    id_var = "id",
    wave_var = "wave",
    observed_var = "observed",
    observed_val = 1,
    waves = c(2018, 2019, 2022)
  )

  ipsi <- tables$compute_ipsi_probabilities(which = 1, deltas = 2)
  expect_equal(ipsi$table_index, 1)
  expect_equal(ipsi$waves, c(2018, 2019))
  expect_s3_class(ipsi$probabilities, "data.frame")
  expect_equal(ipsi$probabilities$delta, 2)
  expect_true(ipsi$counts$initiations <= ipsi$counts$non_attenders)

  ipsi_list <- tables$compute_ipsi_probabilities(which = NULL, deltas = c(2, 5), drop = FALSE)
  expect_length(ipsi_list, 2)
  expect_true(all(vapply(ipsi_list, function(x) inherits(x$probabilities, "data.frame"), logical(1))))
})
