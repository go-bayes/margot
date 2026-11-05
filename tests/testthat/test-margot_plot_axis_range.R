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

axis_range_plot <- function(type = "RD", ..., data = axis_range_data(type), order = "magnitude_desc",
                            order_direction = "top_to_bottom", custom_order = NULL) {
  suppressMessages(margot_plot(data, type = type, order = order, order_direction = order_direction,
    custom_order = custom_order, options = list(use_title_case = FALSE, ...)))
}

coefficient_layer <- function(out) {
  which(vapply(out$plot$layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
}

# every drawn coefficient equals its own row's estimate, panel by panel
expect_labels_match_estimates <- function(plot, info = NULL) {
  built <- ggplot2::ggplot_build(plot)
  layers <- plot$layers
  text_index <- which(vapply(layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
  point_index <- which(vapply(layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  labels <- built$data[[text_index]]
  points <- built$data[[point_index]]
  key <- function(d) paste(d$PANEL, round(d$y, 6))
  estimate <- points$x[match(key(labels), key(points))]
  expect_false(anyNA(estimate), info = info)
  expect_identical(trimws(labels$label), sprintf("%.2f", estimate), info = info)
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

test_that("coefficients sit outside the panel and clear of every interval", {
  for (limits in list(NULL, c(-0.05, 0.15))) {
    out <- if (is.null(limits)) axis_range_plot("RD") else axis_range_plot("RD", x_lim_lo = limits[1], x_lim_hi = limits[2])
    drawn <- out$plot$coordinates$limits$x
    built <- ggplot2::ggplot_build(out$plot)$data[[coefficient_layer(out)]]
    # placed at the right edge of the panel, beyond every interval
    expect_true(all(is.infinite(built$x) & built$x > 0))
    expect_lte(max(axis_range_data()[["97.5 %"]]), drawn[2])
    expect_identical(out$plot$coordinates$clip, "off")
    expect_gt(as.numeric(out$plot$theme$plot.margin[2]), 5.5)
    expect_labels_match_estimates(out$plot)
  }
})

test_that("a range that cuts the data keeps the panel clipped and the column inside it", {
  out <- axis_range_plot("RD", x_lim_lo = 0, x_lim_hi = 0.13)
  limits <- out$plot$coordinates$limits$x
  expect_identical(out$plot$coordinates$clip, "on")
  label_x <- unique(ggplot2::ggplot_build(out$plot)$data[[coefficient_layer(out)]]$x)
  # within the drawn area and left of every interval
  expect_gte(label_x, limits[1])
  expect_lte(label_x + 0.15 * diff(limits), min(axis_range_data()[["2.5 %"]]))
  expect_equal(limits[2], 0.13)
})

test_that("a supplied x_offset is respected", {
  out <- axis_range_plot("RD", x_offset = -0.05, x_lim_lo = -0.06, x_lim_hi = 0.13)
  index <- coefficient_layer(out)
  expect_length(index, 1)
  expect_true(all(ggplot2::ggplot_build(out$plot)$data[[index]]$x == -0.05))
  expect_equal(out$plot$coordinates$limits$x, c(-0.06, 0.13))
  expect_identical(out$plot$coordinates$clip, "on")
})

test_that("labels stay with their outcomes under free-y faceting and any ordering", {
  data <- tibble::tibble(
    outcome = c("alpha", "beta", "gamma"),
    ATE = c(0.11, 0.22, 0.33),
    `2.5 %` = c(0.05, 0.15, 0.25),
    `97.5 %` = c(0.17, 0.29, 0.41),
    E_Value = c(1.4, 1.6, 1.8),
    E_Val_bound = c(1.2, 1.4, 1.6),
    panel = c("one", "two", "two")
  )
  base <- axis_range_plot("RD", data = data)
  faceted <- base$plot + ggplot2::facet_wrap(~panel, scales = "free_y")
  expect_labels_match_estimates(faceted, "free_y")
  # each free-y panel keeps its own range: the header does not stretch it to
  # the overall outcome count
  ranges <- lapply(ggplot2::ggplot_build(faceted)$layout$panel_params, function(p) p$y$continuous_range)
  expect_lt(ranges[[1]][2], 3)
  expect_lt(ranges[[2]][2], 4)
  expect_labels_match_estimates(axis_range_plot("RD", data = data, order = "custom",
    custom_order = c("beta", "gamma", "alpha"))$plot, "custom")
  expect_labels_match_estimates(axis_range_plot("RD", data = data, order_direction = "bottom_to_top")$plot,
    "bottom_to_top")
})
