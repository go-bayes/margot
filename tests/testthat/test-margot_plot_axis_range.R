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
    E_Val_bound = c(1.3, 1.1, 1),
    panel = c("one", "two", "two")
  )
}

axis_range_plot <- function(type = "RD", ..., data = axis_range_data(type), order = "magnitude_desc",
                            order_direction = "top_to_bottom", custom_order = NULL) {
  suppressMessages(margot_plot(data, type = type, order = order, order_direction = order_direction,
    custom_order = custom_order, options = list(use_title_case = FALSE, ...)))
}

text_layers <- function(plot) {
  layers <- plot$layers
  is_text <- vapply(layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))
  maps_label <- vapply(layers, function(layer) !is.null(layer$mapping$label), logical(1))
  list(coefficients = which(is_text & maps_label), header = which(is_text & !maps_label))
}

# every drawn coefficient equals its own row's estimate, panel by panel
expect_labels_match_estimates <- function(plot, info = NULL) {
  built <- ggplot2::ggplot_build(plot)
  layers <- plot$layers
  point_index <- which(vapply(layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  labels <- built$data[[text_layers(plot)$coefficients]]
  points <- built$data[[point_index]]
  key <- function(d) paste(d$PANEL, round(d$y, 6))
  estimate <- points$x[match(key(labels), key(points))]
  expect_false(anyNA(estimate), info = info)
  expect_identical(trimws(labels$label), sprintf("%.2f", estimate), info = info)
}

# the column lies inside the drawn range and, with the default range, left of every interval
expect_column_clear <- function(out, data = axis_range_data()) {
  limits <- out$plot$coordinates$limits$x
  label_x <- unique(ggplot2::ggplot_build(out$plot)$data[[text_layers(out$plot)$coefficients]]$x)
  expect_gte(label_x, limits[1])
  expect_lte(label_x + 0.15 * diff(limits), min(c(data[[2]], data[["2.5 %"]])))
}

test_that("the default range contains the null and every limit, with a band for the column", {
  for (type in c("RD", "RR")) {
    data <- axis_range_data(type)
    out <- axis_range_plot(type)
    limits <- out$plot$coordinates$limits$x
    null <- if (type == "RR") 1 else 0
    expect_lte(limits[1], min(c(null, data[["2.5 %"]])), label = type)
    expect_gte(limits[2], max(c(null, data[["97.5 %"]])), label = type)
    # fitted, with modest padding and the column band rather than a fixed range
    span <- diff(range(c(null, data[["2.5 %"]], data[["97.5 %"]])))
    expect_lt(diff(limits), 1.6 * span, label = type)
    expect_column_clear(out, data)
    expect_labels_match_estimates(out$plot, type)
  }
})

test_that("explicit limits are honoured exactly, with the column at the left edge", {
  out <- axis_range_plot("RD", x_lim_lo = 0.1, x_lim_hi = 0.35)
  expect_equal(out$plot$coordinates$limits$x, c(0.1, 0.35))
  built <- ggplot2::ggplot_build(out$plot)$data[[text_layers(out$plot)$coefficients]]
  expect_true(all(built$x == 0.1))

  only_upper <- axis_range_plot("RD", x_lim_hi = 0.2)
  expect_equal(only_upper$plot$coordinates$limits$x[2], 0.2)
  expect_identical(out$plot$coordinates$clip, "on")
})

test_that("a supplied x_offset is respected", {
  out <- axis_range_plot("RD", x_offset = -0.05, x_lim_lo = -0.06, x_lim_hi = 0.13)
  index <- text_layers(out$plot)$coefficients
  expect_length(index, 1)
  expect_true(all(ggplot2::ggplot_build(out$plot)$data[[index]]$x == -0.05))
  expect_equal(out$plot$coordinates$limits$x, c(-0.06, 0.13))
})

test_that("legends, strips and coord_flip leave the column and header inside the panel", {
  out <- axis_range_plot("RD")
  limits <- out$plot$coordinates$limits$x
  variants <- list(
    legend_right = out$plot + ggplot2::theme(legend.position = "right"),
    grid_free = out$plot + ggplot2::facet_grid(panel ~ ., scales = "free_y", space = "free_y")
  )
  for (name in names(variants)) {
    plot <- variants[[name]]
    expect_no_error(ggplot2::ggplotGrob(plot))
    built <- ggplot2::ggplot_build(plot)$data[[text_layers(plot)$coefficients]]
    expect_true(all(built$x >= limits[1] & built$x <= limits[2]), info = name)
    expect_labels_match_estimates(plot, name)
  }

  flipped <- out$plot + ggplot2::coord_flip()
  expect_no_error(ggplot2::ggplotGrob(flipped))
  built <- ggplot2::ggplot_build(flipped)
  layers <- text_layers(flipped)
  # after flipping, the coefficients keep their place along the effect axis and
  # the header keeps a finite effect position with hjust = 0 and vjust below 0,
  # which keep it inside the panel
  expect_true(all(is.finite(built$data[[layers$coefficients]]$x)))
  header <- flipped$layers[[layers$header]]
  expect_identical(header$aes_params$label, "Estimate")
  expect_identical(header$aes_params$hjust, 0)
  expect_lt(header$aes_params$vjust, 0)
  expect_identical(header$data$y, -Inf)
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
