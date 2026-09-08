# construct estimates without fitting models or reading participant data
make_ate_facade_data <- function() {
  data.frame(
    outcome = c("t2_first_z", "t2_second_z"),
    ATE = c(0.246813579, -0.3),
    `2.5 %` = c(0.123456789, -0.5),
    `97.5 %` = c(0.370170369, -0.1),
    E_Value = c(1.8, 1.7),
    E_Val_bound = c(1.4, 1.3),
    check.names = FALSE
  )
}

# construct the saved affine transformation constants for both outcomes
make_ate_facade_scale_info <- function() {
  data.frame(
    outcome = c("t2_first_z", "t2_second_z"),
    transformation = c("identity", "identity"),
    center = c(5, 12),
    scale = c(2, 3),
    orientation = c(1, 1),
    unit = c("points", "points"),
    unit_multiplier = c(1, 1)
  )
}

test_that("ATE interfaces preserve the legacy components and numerical precision", {
  estimates <- make_ate_facade_data()
  args <- list(.data = estimates, include_coefficients = FALSE)
  legacy <- do.call(margot_plot, args)
  figure <- do.call(margot_plot_ate, args)
  prose <- do.call(margot_interpret_ate, args)
  table <- do.call(margot_table_ate, args)

  expect_named(legacy, c("plot", "interpretation", "transformed_table"))
  expect_s3_class(figure, "ggplot")
  expect_type(prose, "character")
  expect_s3_class(table, "data.frame")
  expect_identical(figure$data, legacy$plot$data)
  expect_identical(figure$labels, legacy$plot$labels)
  expect_identical(prose, legacy$interpretation)
  expect_identical(table, legacy$transformed_table)
  expect_identical(sort(table$ATE), sort(estimates$ATE))
})

test_that("ATE interfaces preserve options precedence and custom outcome ordering", {
  labels <- list(t2_first_z = "First measure", t2_second_z = "Second measure")
  args <- list(
    .data = make_ate_facade_data(),
    options = list(order = "alphabetical", include_coefficients = TRUE),
    order = "custom",
    custom_order = c("Second measure", "First measure"),
    include_coefficients = FALSE,
    label_mapping = labels
  )
  legacy <- do.call(margot_plot, args)
  figure <- do.call(margot_plot_ate, args)
  prose <- do.call(margot_interpret_ate, args)
  table <- do.call(margot_table_ate, args)

  expect_identical(figure$data, legacy$plot$data)
  expect_identical(prose, legacy$interpretation)
  expect_identical(table, legacy$transformed_table)
  expect_identical(levels(figure$data$outcome), args$custom_order)
  expect_false(any(vapply(figure$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))))
})

test_that("ATE interfaces share supplied scale metadata without replacing model estimates", {
  estimates <- make_ate_facade_data()
  metadata <- make_ate_facade_scale_info()
  args <- list(
    .data = estimates,
    scale_info = metadata,
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )
  legacy <- do.call(margot_plot, args)
  figure <- do.call(margot_plot_ate, args)
  prose <- do.call(margot_interpret_ate, args)
  table <- do.call(margot_table_ate, args)

  expect_identical(figure$data, legacy$plot$data)
  expect_identical(prose, legacy$interpretation)
  expect_identical(table, legacy$transformed_table)
  expect_true(all(c("reported_estimate", "reported_lower", "reported_upper", "reporting_quantity", "reporting_unit") %in% names(table)))
  rows <- match(estimates$ATE, table$ATE)
  expect_false(anyNA(rows))
  expect_equal(table$reported_estimate[rows], estimates$ATE * metadata$scale)
  expect_equal(table$reported_lower[rows], estimates[["2.5 %"]] * metadata$scale)
  expect_equal(table$reported_upper[rows], estimates[["97.5 %"]] * metadata$scale)
  expect_identical(sort(table$ATE), sort(estimates$ATE))
  expect_true(all(table$reporting_unit == "points"))
})

test_that("ATE table renaming and multiplicity arguments reach the shared implementation", {
  args <- list(
    .data = make_ate_facade_data(),
    adjust = "bonferroni",
    alpha = 0.05,
    rename_ate = "Mean difference",
    rename_evalue = TRUE,
    include_coefficients = FALSE
  )
  legacy <- do.call(margot_plot, args)
  table <- do.call(margot_table_ate, args)

  expect_identical(table, legacy$transformed_table)
  expect_true(all(c("Mean difference", "E-Value", "E-Value Bound", "1.25 %", "98.75 %") %in% names(table)))
})


test_that("ATE interfaces distinguish an omitted scale argument from explicit NULL", {
  estimates <- make_ate_facade_data()
  args <- list(
    .data = estimates,
    options = list(scale_info = make_ate_facade_scale_info()),
    include_coefficients = FALSE
  )
  inherited <- do.call(margot_table_ate, args)
  direct <- margot_table_ate(estimates, scale_info = make_ate_facade_scale_info(), include_coefficients = FALSE)
  disabled <- do.call(margot_table_ate, c(args, list(scale_info = NULL)))

  expect_identical(inherited, direct)
  expect_false("reported_estimate" %in% names(disabled))
})

test_that("ATE prose and tables follow the figure from top to bottom", {
  for (ordering in c("alphabetical", "custom", "magnitude_desc", "evaluebound_asc")) {
    args <- list(
      .data = make_ate_facade_data(),
      order = ordering,
      include_coefficients = FALSE,
      label_mapping = list(t2_first_z = "First measure", t2_second_z = "Second measure")
    )
    if (ordering == "custom") args$custom_order <- c("Second measure", "First measure")
    figure <- do.call(margot_plot_ate, args)
    table <- do.call(margot_table_ate, args)
    prose <- do.call(margot_interpret_ate, args)
    bullets <- strsplit(prose, "\n", fixed = TRUE)[[1]]
    bullets <- bullets[startsWith(bullets, "- ")]
    prose_order <- sub(":.*$", "", substring(bullets, 3))

    expect_identical(rownames(table), rev(levels(figure$data$outcome)), info = ordering)
    expect_identical(prose_order, rownames(table), info = ordering)
  }
})

test_that("legacy scale inference runs once before display labels are applied", {
  warnings <- character()
  original <- data.frame(t2_first = c(1, 2, 3), t2_second = c(2, 4, 6))
  result <- withCallingHandlers(
    margot_plot(
      make_ate_facade_data(),
      original_df = original,
      include_coefficients = FALSE,
      label_mapping = list(t2_first_z = "Arbitrary display label", t2_second_z = "Another label")
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(sum(grepl("Inferring legacy transformations", warnings, fixed = TRUE)), 1)
  rows <- match(make_ate_facade_data()$ATE, result$transformed_table$ATE)
  expect_equal(result$transformed_table$reported_estimate[rows], make_ate_facade_data()$ATE * c(1, 2))
})

test_that("display labels and metadata row order do not change reported quantities", {
  estimates <- make_ate_facade_data()
  metadata <- make_ate_facade_scale_info()[2:1, ]
  result <- margot_table_ate(
    estimates,
    scale_info = metadata,
    include_coefficients = FALSE,
    label_mapping = list(t2_first_z = "Unrelated display name", t2_second_z = "Different display name")
  )
  rows <- match(estimates$ATE, result$ATE)

  expect_equal(result$reported_estimate[rows], estimates$ATE * c(2, 3))
  expect_setequal(rownames(result), c("Unrelated display name", "Different display name"))
})
