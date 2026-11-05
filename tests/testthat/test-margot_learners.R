test_that("margot_plot_lmtp_learners uses readable component strip headers", {
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(
          density_ratios = structure(
            matrix(1, nrow = 4, ncol = 2),
            dimnames = list(NULL, c("wave_1", "wave_2"))
          ),
          fits_m = list(
            list(
              c(SL.mean = 0.20, SL.ranger = 0.80),
              c(SL.mean = 0.25, SL.ranger = 0.75)
            ),
            list(
              c(SL.mean = 0.35, SL.ranger = 0.65),
              c(SL.mean = 0.40, SL.ranger = 0.60)
            )
          ),
          fits_r = list(
            list(
              c(SL.mean = 0.60, SL.ranger = 0.40),
              c(SL.mean = 0.55, SL.ranger = 0.45)
            ),
            list(
              c(SL.mean = 0.50, SL.ranger = 0.50),
              c(SL.mean = 0.45, SL.ranger = 0.55)
            )
          )
        )
      )
    )
  )

  p <- margot_plot_lmtp_learners(
    x = fit,
    outcome = "outcome"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$facet$params$switch, "y")
  expect_true("component_strip_label" %in% names(p$data))
  expect_true("shift_label" %in% names(p$data))
  expect_equal(
    levels(p$data$component_strip_label),
    c("Outcome regression\n(m)", "Treatment regression\n(r)")
  )
  expect_equal(levels(p$data$shift_label), "Observed (Censoring-Adjusted)")
  expect_equal(p$theme$strip.placement, "outside")
  expect_equal(p$theme$strip.text.y.left$angle, 0)
  expect_equal(p$theme$strip.background.y$fill, "#6b7280")
})

test_that("margot_interpret_lmtp_learners returns text in list mode", {
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(
          density_ratios = structure(
            matrix(1, nrow = 4, ncol = 2),
            dimnames = list(NULL, c("wave_1", "wave_2"))
          ),
          fits_m = list(
            list(
              c(SL.mean = 0.20, SL.ranger = 0.80),
              c(SL.mean = 0.25, SL.ranger = 0.75)
            ),
            list(
              c(SL.mean = 0.35, SL.ranger = 0.65),
              c(SL.mean = 0.40, SL.ranger = 0.60)
            )
          ),
          fits_r = list(
            list(
              c(SL.mean = 0.60, SL.ranger = 0.40),
              c(SL.mean = 0.55, SL.ranger = 0.45)
            ),
            list(
              c(SL.mean = 0.50, SL.ranger = 0.50),
              c(SL.mean = 0.45, SL.ranger = 0.55)
            )
          )
        )
      )
    )
  )

  out <- margot_interpret_lmtp_learners(
    x = fit,
    outcome = "outcome",
    return = "list"
  )

  expect_type(out$text, "character")
  expect_match(out$text, "LMTP learner weights")
})

test_that("margot_report_lmtp_learners bundles learner outputs", {
  fit <- list(
    models = list(
      first_outcome = list(
        first_outcome_null = list(
          density_ratios = structure(
            matrix(1, nrow = 4, ncol = 2),
            dimnames = list(NULL, c("wave_1", "wave_2"))
          ),
          fits_m = list(
            list(
              c(SL.mean = 0.20, SL.ranger = 0.80),
              c(SL.mean = 0.25, SL.ranger = 0.75)
            ),
            list(
              c(SL.mean = 0.35, SL.ranger = 0.65),
              c(SL.mean = 0.40, SL.ranger = 0.60)
            )
          ),
          fits_r = list(
            list(
              c(SL.mean = 0.60, SL.ranger = 0.40),
              c(SL.mean = 0.55, SL.ranger = 0.45)
            ),
            list(
              c(SL.mean = 0.50, SL.ranger = 0.50),
              c(SL.mean = 0.45, SL.ranger = 0.55)
            )
          )
        ),
        first_outcome_shift_up = list(
          density_ratios = structure(
            matrix(1, nrow = 4, ncol = 2),
            dimnames = list(NULL, c("wave_1", "wave_2"))
          ),
          fits_m = list(
            list(
              c(SL.mean = 0.10, SL.ranger = 0.90),
              c(SL.mean = 0.15, SL.ranger = 0.85)
            ),
            list(
              c(SL.mean = 0.25, SL.ranger = 0.75),
              c(SL.mean = 0.30, SL.ranger = 0.70)
            )
          ),
          fits_r = list(
            list(
              c(SL.mean = 0.30, SL.ranger = 0.70),
              c(SL.mean = 0.35, SL.ranger = 0.65)
            ),
            list(
              c(SL.mean = 0.20, SL.ranger = 0.80),
              c(SL.mean = 0.25, SL.ranger = 0.75)
            )
          )
        )
      ),
      second_outcome = list(
        second_outcome_null = list(
          density_ratios = matrix(1, nrow = 4, ncol = 1),
          fits_m = list(list(c(SL.mean = 1))),
          fits_r = list(list(c(SL.mean = 1)))
        )
      )
    )
  )

  rep <- margot_report_lmtp_learners(
    x = fit,
    include_plot = FALSE
  )

  expect_true("summary_table" %in% names(rep))
  expect_true("learner_data" %in% names(rep))
  expect_true("narrative" %in% names(rep))
  expect_true("method_statement" %in% names(rep))
  expect_true(is.null(rep$plot))
  expect_true(is.data.frame(rep$summary_table))
  expect_true(is.data.frame(rep$learner_data))
  expect_equal(rep$metadata$outcome, "first_outcome")
  expect_equal(rep$metadata$shifts, c("first_outcome_null", "first_outcome_shift_up"))
  expect_equal(unique(rep$summary_table$Shift), c("Observed (Censoring-Adjusted)", "Shift Up"))
  expect_equal(unique(rep$summary_table$Component), c("Outcome regression (m)", "Treatment regression (r)"))
  expect_type(rep$narrative$text, "character")
  expect_match(rep$method_statement, "Super Learner diagnostics")
})

test_that("margot_report_lmtp_learners reads a lean margot_lmtp fit", {
  # the fixtures live in helper-lmtp-shared.R.
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data()
  output <- suppressMessages(margot_lmtp(data, outcome_vars = "perfectionism", trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down), include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = shared_ratio_defaults(), seed = 3031L,
    progress = "none"))
  # the default per-fit route keeps lmtp's lean fits: learner weights, not fitted learners.
  margot_lmtp_assert_lean(output$models)
  report <- margot_report_lmtp_learners(output, outcome = "perfectionism", include_plot = FALSE)
  learner_data <- report$learner_data
  expect_true(nrow(report$summary_table) > 0L)
  expect_setequal(unique(learner_data$component), c("outcome", "treatment"))
  expect_setequal(unique(learner_data$learner), c("SL.glm_All", "SL.mean_All"))
  expect_true(all(learner_data$weight_mean >= 0 & learner_data$weight_mean <= 1))
  # every reported weight comes from the coefficients lmtp kept in the lean model.
  model <- output$models$perfectionism$perfectionism_down
  kept <- unlist(lapply(model$fits_m, function(x) lapply(x, function(w) w[["SL.glm_All"]])))
  reported <- learner_data[learner_data$component == "outcome" & learner_data$learner == "SL.glm_All" &
    learner_data$shift_full == "perfectionism_down", ]
  expect_equal(sum(reported$weight_mean * reported$weight_n), sum(kept))
})

test_that("learner summaries group lmtp fits by time point, not by outer fold", {
  # lmtp 1.5.4 stores fits as fits[[fold]][[time]]; three time points, two folds here.
  fold <- function(a) lapply(a, function(w) c(SL.mean = w, SL.ranger = 1 - w))
  model <- list(
    density_ratios = matrix(1, nrow = 4, ncol = 3, dimnames = list(NULL, c("t1", "t2", "t3"))),
    fits_m = list(fold(c(0.1, 0.2, 0.3)), fold(c(0.3, 0.4, 0.5))),
    fits_r = list(fold(c(0.6, 0.6, 0.6)), fold(c(0.8, 0.8, 0.8)))
  )
  fit <- list(models = list(y = list(y_shift = model)))
  rows <- summarise_lmtp_learners(fit, outcome = "y")
  outcome_mean <- rows[rows$component == "outcome" & rows$learner == "SL.mean", ]
  expect_equal(nrow(outcome_mean), 3L)
  expect_equal(outcome_mean$wave_index, 1:3)
  expect_equal(outcome_mean$wave_label, c("t1", "t2", "t3"))
  expect_equal(outcome_mean$weight_n, rep(2L, 3L))
  expect_equal(outcome_mean$weight_mean, c(0.2, 0.3, 0.4))
  treatment_mean <- rows[rows$component == "treatment" & rows$learner == "SL.mean", ]
  expect_equal(treatment_mean$weight_mean, rep(0.7, 3L))
})
