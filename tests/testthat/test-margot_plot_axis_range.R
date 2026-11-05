axis_range_data <- function(type = "RD") {
  if (type == "RR") {
    return(tibble::tibble(
      outcome = c("protective", "harmful", "small"),
      `E[Y(1)]/E[Y(0)]` = c(0.8, 1.3, 1.05),
      `2.5 %` = c(0.7, 1.1, 0.97),
      `97.5 %` = c(0.9, 1.5, 1.14),
      E_Value = c(1.8, 1.9, 1.28),
      E_Val_bound = c(1.5, 1.4, 1)
    ))
  }
  tibble::tibble(
    outcome = c("small", "smaller", "crossing"),
    ATE = c(0.09, 0.04, 0.01),
    `2.5 %` = c(0.06, 0.01, -0.02),
    `97.5 %` = c(0.12, 0.07, 0.04),
    E_Value = c(1.4, 1.2, 1.1),
    E_Val_bound = c(1.3, 1.1, 1)
  )
}

axis_range_plot <- function(type = "RD", ...) {
  suppressMessages(margot_plot(axis_range_data(type), type = type,
    options = list(use_title_case = FALSE, ...)))
}

test_that("the default range contains the null and every limit on both scales", {
  for (type in c("RD", "RR")) {
    data <- axis_range_data(type)
    limits <- axis_range_plot(type)$plot$coordinates$limits$x
    null <- if (type == "RR") 1 else 0
    expect_lte(limits[1], min(c(null, data[["2.5 %"]])), label = type)
    expect_gte(limits[2], max(c(null, data[["97.5 %"]])), label = type)
    # fitted, with modest padding rather than a fixed range
    span <- diff(range(c(null, data[["2.5 %"]], data[["97.5 %"]])))
    expect_lt(diff(limits), 1.5 * span, label = type)
  }
})

test_that("coefficients stay visible and clear of intervals under any user range", {
  narrow <- list(x_lim_lo = 0, x_lim_hi = 0.13)

  # default: a column outside the panel, whatever the range
  out <- do.call(axis_range_plot, c(list("RD"), narrow))
  expect_equal(out$plot$coordinates$limits$x, c(0, 0.13))
  expect_false(any(vapply(out$plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))))
  labels <- ggplot2::ggplot_build(out$plot)$layout$panel_params[[1]]$y.sec$get_labels()
  expect_setequal(trimws(labels), c("0.09", "0.04", "0.01"))

  # inside the panel (coefficients coloured by category), the range widens to
  # the left so that the column lies within the drawn area and left of every interval
  inside <- do.call(axis_range_plot, c(list("RD"), narrow, list(coefficient_colour = NULL)))
  limits <- inside$plot$coordinates$limits$x
  layers <- inside$plot$layers
  text_index <- which(vapply(layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
  label_x <- unique(ggplot2::ggplot_build(inside$plot)$data[[text_index]]$x)
  expect_gte(label_x, limits[1])
  expect_lte(label_x + 0.15 * diff(limits), min(axis_range_data()[["2.5 %"]]))
  expect_equal(limits[2], 0.13)
})

test_that("a supplied x_offset is respected", {
  out <- axis_range_plot("RD", x_offset = -0.05, x_lim_lo = -0.06, x_lim_hi = 0.13)
  layers <- out$plot$layers
  text_index <- which(vapply(layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
  expect_length(text_index, 1)
  expect_true(all(ggplot2::ggplot_build(out$plot)$data[[text_index]]$x == -0.05))
  expect_equal(out$plot$coordinates$limits$x, c(-0.06, 0.13))
})
