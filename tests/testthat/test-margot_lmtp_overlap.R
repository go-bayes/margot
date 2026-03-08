test_that("margot_lmtp_overlap keeps null panels visible under shift colouring", {
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(
          density_ratios = matrix(c(1.1, 1.2,
                                    1.3, 1.4), ncol = 2, byrow = TRUE)
        ),
        outcome_shift_up = list(
          density_ratios = matrix(c(0.2, 0.3,
                                    0.4, 0.5), ncol = 2, byrow = TRUE)
        )
      )
    )
  )

  ol <- suppressWarnings(
    margot_lmtp_overlap(
      fit,
      outcomes = "outcome",
      shifts = c("null", "shift_up"),
      plot = TRUE,
      verbose = FALSE,
      color_by = "shift",
      include_tests = FALSE
    )
  )

  expect_true("outcome::outcome_null::1" %in% names(ol$ratio_plots))
  expect_true("outcome::outcome_shift_up::1" %in% names(ol$ratio_plots))
  expect_match(ol$ratio_plots[["outcome::outcome_null::1"]]$labels$title, "Identity")
  expect_true(is.na(ol$ratio_plots[["outcome::outcome_null::1"]]$layers[[1]]$aes_params$colour))
  expect_equal(ol$ratio_plots[["outcome::outcome_shift_up::1"]]$layers[[1]]$aes_params$colour, "white")
  null_layers <- ol$ratio_plots[["outcome::outcome_null::1"]]$layers
  expect_true(any(vapply(null_layers, function(layer) inherits(layer$geom, "GeomVline"), logical(1))))

  grid <- margot_lmtp_overlap_plot_grid(
    ol,
    outcome = "outcome",
    shifts = c("null", "shift_up")
  )

  expect_s3_class(grid, "patchwork")
})

test_that("overlap grid defaults to global y harmonisation", {
  expect_equal(eval(formals(margot_plot_lmtp_overlap_grid)$ymax_harmonize), "global")
  expect_equal(eval(formals(margot_lmtp_overlap_plot_grid)$ymax_harmonize), "global")
})

test_that("reference line can be disabled in overlap plots", {
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(
          density_ratios = matrix(c(1.1, 1.2,
                                    1.3, 1.4), ncol = 2, byrow = TRUE)
        )
      )
    )
  )

  ol <- suppressWarnings(
    margot_lmtp_overlap(
      fit,
      outcomes = "outcome",
      shifts = "null",
      plot = TRUE,
      verbose = FALSE,
      show_reference_line = FALSE,
      include_tests = FALSE
    )
  )

  null_layers <- ol$ratio_plots[["outcome::outcome_null::1"]]$layers
  expect_false(any(vapply(null_layers, function(layer) inherits(layer$geom, "GeomVline"), logical(1))))
})
