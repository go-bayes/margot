# construct estimates for the experimental reporting interface without fitting
make_dev_reporting_data <- function() {
  data.frame(
    outcome = c("t2_first_z", "t2_second_z"),
    ATE = c(0.2, -0.3),
    `2.5 %` = c(0.1, -0.5),
    `97.5 %` = c(0.3, -0.1),
    E_Value = c(1.8, 1.7),
    E_Val_bound = c(1.2, 1.3),
    check.names = FALSE
  )
}

test_that("experimental plotting transforms legacy scales once before relabelling", {
  warnings <- character()
  result <- withCallingHandlers(
    margot_plot_dev(
      make_dev_reporting_data(),
      original_df = data.frame(t2_first = c(1, 2, 3), t2_second = c(2, 4, 6)),
      label_mapping = list(t2_first_z = "First label", t2_second_z = "Second label"),
      include_coefficients = FALSE
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_named(result, c("plot", "interpretation", "transformed_table"))
  expect_equal(sum(grepl("Inferring legacy transformations", warnings, fixed = TRUE)), 1)
  rows <- match(make_dev_reporting_data()$ATE, result$transformed_table$ATE)
  expect_equal(result$transformed_table$reported_estimate[rows], c(0.2, -0.6))
  expect_match(result$interpretation, "Second label", fixed = TRUE)
  expect_match(result$interpretation, "mean difference", fixed = TRUE)
  expect_match(result$interpretation, "lower bound > 1.2", fixed = TRUE)
  expect_false(grepl("- First label:", result$interpretation, fixed = TRUE))
  expect_equal(as.character(result$plot$data$Estimate), c("positive", "negative"))
})

test_that("experimental scale metadata supports explicit and options arguments", {
  metadata <- data.frame(
    outcome = c("t2_first_z", "t2_second_z"),
    transformation = c("identity", "log1p"),
    scale = c(2, 3)
  )
  explicit <- margot_plot_dev(make_dev_reporting_data(), scale_info = metadata, include_coefficients = FALSE)
  inherited <- margot_plot_dev(make_dev_reporting_data(), options = list(scale_info = metadata), include_coefficients = FALSE)
  disabled <- margot_plot_dev(make_dev_reporting_data(), options = list(scale_info = metadata), scale_info = NULL, include_coefficients = FALSE)

  expect_identical(explicit$transformed_table, inherited$transformed_table)
  expect_identical(explicit$interpretation, inherited$interpretation)
  expect_false("reported_estimate" %in% names(disabled$transformed_table))
  expect_true("unit" %in% names(disabled$transformed_table))
  expect_match(explicit$interpretation, "ratio of geometric means of outcome + 1", fixed = TRUE)
  expect_false(grepl("average increase|average decrease", explicit$interpretation))
})

test_that("experimental ratio reporting preserves valid ratios and rejects centred scales", {
  ratios <- make_dev_reporting_data()[1, ]
  names(ratios)[names(ratios) == "ATE"] <- "E[Y(1)]/E[Y(0)]"
  ratios$outcome <- "binary_outcome"
  ratios[["E[Y(1)]/E[Y(0)]"]] <- 1.5
  ratios[["2.5 %"]] <- 1.2
  ratios[["97.5 %"]] <- 1.8
  metadata <- data.frame(outcome = "binary_outcome", center = 0, scale = 2)
  result <- margot_plot_dev(ratios, type = "RR", scale_info = metadata, include_coefficients = FALSE)

  expect_equal(result$transformed_table$reported_estimate, 1.5)
  expect_equal(result$transformed_table$reported_lower, 1.2)
  expect_equal(result$transformed_table$reported_upper, 1.8)
  metadata$center <- 1
  expect_error(margot_plot_dev(ratios, type = "RR", scale_info = metadata), "zero centre")
})

test_that("experimental custom ordering reaches the shared interpretation", {
  result <- margot_plot_dev(
    make_dev_reporting_data(),
    order = "custom",
    custom_order = c("Second", "First"),
    e_val_bound_threshold = 1,
    include_coefficients = FALSE
  )
  expect_identical(levels(result$plot$data$outcome), c("Second", "First"))
  lines <- strsplit(result$interpretation, "\n", fixed = TRUE)[[1]]
  labels <- sub(":.*$", "", substring(lines[startsWith(lines, "- ")], 3))
  expect_identical(labels, c("First", "Second"))
})


test_that("experimental horizontal intervals preserve endpoint and cap geometry", {
  result <- margot_plot_dev(make_dev_reporting_data(), include_coefficients = FALSE)
  intervals <- ggplot2::ggplot_build(result$plot)$data[[1]]
  expect_equal(intervals$xmin, result$plot$data[["2.5 %"]])
  expect_equal(intervals$xmax, result$plot$data[["97.5 %"]])
  expect_equal(as.numeric(intervals$ymax - intervals$ymin), rep(0.3, nrow(intervals)))
  expect_true(all(intervals$flipped_aes))
})
