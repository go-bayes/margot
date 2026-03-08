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
  expect_equal(levels(p$data$shift_label), "Identity")
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
  expect_equal(unique(rep$summary_table$Shift), c("Identity", "Shift Up"))
  expect_equal(unique(rep$summary_table$Component), c("Outcome regression (m)", "Treatment regression (r)"))
  expect_type(rep$narrative$text, "character")
  expect_match(rep$method_statement, "Super Learner diagnostics")
})
