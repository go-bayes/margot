make_margot_plot_test_data <- function(
    estimates = c(0.2, 0.9, 0.5),
    lower = c(0.05, 0.6, 0.2),
    upper = c(0.35, 1.2, 0.8),
    bounds = c(1.2, 1.8, 1.4),
    outcomes = c("alpha", "beta", "gamma")) {
  tibble::tibble(
    outcome = outcomes,
    ATE = estimates,
    `2.5 %` = lower,
    `97.5 %` = upper,
    E_Value = pmax(bounds, 1),
    E_Val_bound = bounds
  )
}

test_that("recognised control arguments can be supplied through options", {
  out <- margot_plot(
    make_margot_plot_test_data(),
    type = "RD",
    options = list(
      order = "magnitude_desc",
      adjust = "bonferroni",
      alpha = 0.05,
      include_coefficients = FALSE,
      use_title_case = FALSE
    )
  )

  # ggplot draws the last level at the top, so magnitude_desc puts beta, the largest, there
  expect_equal(levels(out$plot$data$outcome), c("alpha", "gamma", "beta"))
  expect_equal(out$transformed_table$Outcome, c("beta", "gamma", "alpha"))
  expect_true(all(out$plot$data$confidence_level > 0.95))
  expect_false(any(vapply(out$plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))))
})

test_that("coefficient labels use fixed-width monospace formatting", {
  data <- make_margot_plot_test_data(
    estimates = c(-10.2, 2.3, 0.45),
    lower = c(-11.0, 1.5, 0.2),
    upper = c(-9.4, 3.1, 0.7),
    bounds = c(1.5, 1.7, 1.2)
  )

  # by default the coefficients form a column on a secondary axis to the right of the panel
  out <- margot_plot(data, type = "RD", options = list(use_title_case = FALSE))
  expect_false(any(vapply(out$plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))))
  expect_identical(out$plot$theme$axis.text.y.right$family, "mono")
  labels <- ggplot2::ggplot_build(out$plot)$layout$panel_params[[1]]$y.sec$get_labels()
  expect_length(labels, 3)
  expect_length(unique(nchar(labels)), 1)

  # a supplied x_offset draws them inside the panel, as earlier versions did
  inside <- margot_plot(data, type = "RD", options = list(use_title_case = FALSE, x_offset = -12))
  # the coefficient layer maps its labels; the column header is a separate annotation
  text_layer_index <- which(vapply(inside$plot$layers, function(layer) inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label), logical(1)))
  expect_length(text_layer_index, 1)

  text_layer <- inside$plot$layers[[text_layer_index]]
  text_family <- text_layer$aes_params$family
  if (is.null(text_family)) {
    text_family <- text_layer$geom_params$family
  }

  expect_equal(text_family, "mono")
  labels <- ggplot2::ggplot_build(inside$plot)$data[[text_layer_index]]$label
  expect_length(unique(nchar(labels)), 1)
})

test_that("difference-scale plots fit the default range to the data and the null", {
  data <- make_margot_plot_test_data(
    estimates = c(0.2, 0.3),
    lower = c(0.1, 0.2),
    upper = c(0.3, 0.4),
    bounds = c(1.4, 1.5),
    outcomes = c("first", "second")
  )
  out <- margot_plot(data, type = "RD", options = list(use_title_case = FALSE))

  limits <- out$plot$coordinates$limits$x
  expect_lte(limits[1], 0)
  expect_gte(limits[2], 0.4)
  expect_false(isTRUE(all.equal(limits, c(-0.5, 0.5))))

  # the earlier layout is available through explicit options
  earlier <- margot_plot(data, type = "RD",
    options = list(use_title_case = FALSE, x_lim_lo = -0.5, x_lim_hi = 0.5, x_offset = -0.5))
  expect_equal(earlier$plot$coordinates$limits$x, c(-0.5, 0.5))

  # the coefficient layer maps its labels; the column header is a separate annotation
  text_layer_index <- which(vapply(earlier$plot$layers, function(layer) inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label), logical(1)))
  text_data <- ggplot2::ggplot_build(earlier$plot)$data[[text_layer_index]]

  expect_true(all(text_data$x == -0.5))
})

test_that("threshold equality is flagged only under the inclusive comparison", {
  args <- list(
    make_margot_plot_test_data(
      estimates = 0.3,
      lower = 0.1,
      upper = 0.5,
      bounds = 1,
      outcomes = "equal_bound"
    ),
    type = "RD",
    e_val_bound_threshold = 1,
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )
  out <- do.call(margot_plot, args)
  inclusive <- do.call(margot_plot, c(args, list(e_val_bound_comparison = ">=")))
  legacy <- do.call(margot_plot, c(args, list(e_val_bound_comparison = ">=", category_rule = "interval_and_evalue")))

  # the interval decides the category; the flag records the comparison
  expect_equal(as.character(out$plot$data$Estimate[[1]]), "positive")
  expect_match(out$interpretation, "equal bound")
  expect_identical(out$transformed_table[["E-value bound > 1"]], "no")
  expect_identical(inclusive$transformed_table[["E-value bound >= 1"]], "yes")
  expect_equal(as.character(legacy$plot$data$Estimate[[1]]), "positive")
  expect_match(legacy$interpretation, "equal bound")
})

test_that("adjusted confidence intervals expose corrected headers and captions", {
  out <- margot_plot(
    make_margot_plot_test_data(
      estimates = c(0.3, 0.5),
      lower = c(0.1, 0.2),
      upper = c(0.5, 0.8),
      bounds = c(1.5, 1.6),
      outcomes = c("first", "second")
    ),
    type = "RD",
    adjust = "bonferroni",
    alpha = 0.05,
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )

  expect_true(all(c("97.5% CI lower", "97.5% CI upper") %in% names(out$transformed_table)))
  expect_false("confidence_level" %in% names(out$transformed_table))
  expect_match(out$plot$labels$caption, "97.5% confidence intervals")

  legacy <- margot_plot(
    make_margot_plot_test_data(
      estimates = c(0.3, 0.5),
      lower = c(0.1, 0.2),
      upper = c(0.5, 0.8),
      bounds = c(1.5, 1.6),
      outcomes = c("first", "second")
    ),
    type = "RD",
    adjust = "bonferroni",
    alpha = 0.05,
    include_coefficients = FALSE,
    table_columns = "legacy",
    options = list(use_title_case = FALSE)
  )
  expect_true(all(c("1.25 %", "98.75 %") %in% names(legacy$transformed_table)))
})

test_that("margot_plot_create_options does not inject adjust or alpha defaults", {
  opts <- margot_plot_create_options("Example subtitle")

  expect_false("adjust" %in% names(opts))
  expect_false("alpha" %in% names(opts))
})

test_that("margot_plot_multi reuses shared x-axis limits across panels", {
  out <- margot_plot_multi(
    tables = list(
      first = make_margot_plot_test_data(
        estimates = c(0.2, 0.4),
        lower = c(0.1, 0.3),
        upper = c(0.3, 0.5),
        bounds = c(1.4, 1.5),
        outcomes = c("a", "b")
      ),
      second = make_margot_plot_test_data(
        estimates = c(1.2, 1.6),
        lower = c(0.8, 1.1),
        upper = c(1.5, 2.0),
        bounds = c(2.0, 2.1),
        outcomes = c("a", "b")
      )
    ),
    panel_titles = c("First", "Second"),
    options = list(use_title_case = FALSE),
    include_coefficients = FALSE
  )

  expect_s3_class(out$plot, "patchwork")
  expect_equal(out$panels[[1]]$plot$coordinates$limits$x, out$panels[[2]]$plot$coordinates$limits$x)
  expect_equal(out$panels[[1]]$plot$coordinates$limits$x, out$shared_x_limits)
})

test_that("margot_plot_multi respects user-supplied shared x-axis limits", {
  out <- margot_plot_multi(
    tables = list(
      first = make_margot_plot_test_data(
        estimates = c(0.2, 0.4),
        lower = c(0.1, 0.3),
        upper = c(0.3, 0.5),
        bounds = c(1.4, 1.5),
        outcomes = c("a", "b")
      ),
      second = make_margot_plot_test_data(
        estimates = c(1.2, 1.6),
        lower = c(0.8, 1.1),
        upper = c(1.5, 2.0),
        bounds = c(2.0, 2.1),
        outcomes = c("a", "b")
      )
    ),
    shared_x_limits = c(-1, 3),
    panel_titles = c("First", "Second"),
    options = list(use_title_case = FALSE),
    include_coefficients = FALSE
  )

  expect_equal(out$shared_x_limits, c(-1, 3))
  expect_equal(out$panels[[1]]$plot$coordinates$limits$x, c(-1, 3))
  expect_equal(out$panels[[2]]$plot$coordinates$limits$x, c(-1, 3))
})
