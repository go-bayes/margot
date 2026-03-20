make_margot_interpret_test_data <- function() {
  test_data <- data.frame(
    outcome = c(
      "t2_log_charity_donate_z",
      "t2_belong_z",
      "t2_hours_exercise_z"
    ),
    check.names = FALSE
  )

  test_data[["E[Y(1)]-E[Y(0)]"]] <- c(0.14, 0.25, 0.30)
  test_data[["2.5 %"]] <- c(0.08, 0.15, 0.20)
  test_data[["97.5 %"]] <- c(0.20, 0.35, 0.40)
  test_data$E_Value <- c(1.8, 2.1, 2.3)
  test_data$E_Val_bound <- c(1.5, 1.8, 2.0)
  test_data$original_var_name <- c(
    "t2_log_charity_donate_z",
    "t2_belong_z",
    "t2_hours_exercise_z"
  )
  test_data[["E[Y(1)]-E[Y(0)]_original"]] <- c(
    0.14 * 2.74,
    0.25 * 1.5,
    0.30 * 3.2
  )
  test_data[["2.5 %_original"]] <- c(
    0.08 * 2.74,
    0.15 * 1.5,
    0.20 * 3.2
  )
  test_data[["97.5 %_original"]] <- c(
    0.20 * 2.74,
    0.35 * 1.5,
    0.40 * 3.2
  )
  test_data$unit <- c("", "", "")

  test_data
}

make_margot_interpret_original_df <- function() {
  set.seed(1)

  data.frame(
    t2_log_charity_donate = rnorm(1000, mean = 3.5, sd = 2.74),
    t2_belong = rnorm(1000, mean = 5, sd = 1.5),
    t2_hours_exercise = rnorm(1000, mean = 4, sd = 3.2)
  )
}

test_that("margot_interpret_marginal includes original-scale interpretations", {
  result <- margot_interpret_marginal(
    df = make_margot_interpret_test_data(),
    type = "RD",
    order = "alphabetical",
    original_df = make_margot_interpret_original_df(),
    e_val_bound_threshold = 1.2,
    adjust = "none",
    include_adjust_note = FALSE
  )

  expect_match(result$interpretation, "on the original scale", fixed = TRUE)
  expect_match(result$interpretation, "minutes", fixed = TRUE)
  expect_match(result$interpretation, "average increase", fixed = TRUE)
})
