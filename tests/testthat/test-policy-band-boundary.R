# exercise symmetric bands, displayed separators, transformations, and unchanged actions.
test_that("band boundaries preserve symmetric jitter and the fitted rule", {
  skip_if_not_installed("policytree")
  x <- data.frame(value = rep(1:7, each = 100))
  scores <- cbind(control = 0, treated = ifelse(x$value <= 6, 1, -1))
  tree <- policytree::policy_tree(x, scores, depth = 1, min.node.size = 5)
  object <- list(results = list(model_y = list(policy_tree_depth_1 = tree,
    plot_data = list(X_test = x))))
  set.seed(99)
  before <- .Random.seed
  p <- margot_plot_policy_combo(object, "model_y", max_depth = 1,
    generate_decision_tree = FALSE, policy_tree_args = list(
      jitter_method = "band_boundary", jitter_seed = 42))$policy_tree
  b <- ggplot2::ggplot_build(p)$data
  expect_identical(.Random.seed, before)
  expect_equal(b[[2]]$xintercept, 6.3)
  expect_identical(b[[1]]$x <= b[[2]]$xintercept, x$value <= 6)
  standard <- ggplot2::ggplot(data.frame(x = x$value, y = 0), ggplot2::aes(x, y)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.3, height = 0.06, seed = 42))
  expect_identical(b[[1]]$x, ggplot2::ggplot_build(standard)$data[[1]]$x)
  expect_true(any(b[[1]]$x[x$value == 6] > 6))
  expect_true(any(b[[1]]$x[x$value == 6] < 6))
  expect_identical(b[[1]]$x, ggplot2::ggplot_build(p)$data[[1]]$x)
  expect_identical(as.integer(p$layers[[1]]$data$pred), as.integer(predict(tree, x)))
  expect_match(p$labels$subtitle, "6.000", fixed = TRUE)
  for (transform in c("log1p", "reverse")) {
    transformed <- p + ggplot2::scale_x_continuous(transform = transform)
    built <- ggplot2::ggplot_build(transformed)$data
    upper_closed <- transform != "reverse"
    side <- if (upper_closed) built[[1]]$x <= built[[2]]$xintercept else built[[1]]$x >= built[[2]]$xintercept
    expect_identical(side, x$value <= 6)
  }
  expect_error(margot_plot_policy_tree(object, "model_y", max_depth = 2,
    jitter_method = "band_boundary"), "requires max_depth")
})

test_that("close values reduce the whole band's width without truncating cut ties", {
  x <- rep(c(4, 5, 5.01, 6), each = 200)
  layout <- .margot_policy_band_layout(x, 5, 0.3)
  expect_equal(layout$width, 0.0049)
  expect_equal(layout$boundary, 5.0049)
  p <- ggplot2::ggplot(data.frame(x, y = 0), ggplot2::aes(x, y)) +
    ggplot2::geom_point(position = .margot_policy_band_position(x, 5, 0.3, 0.06, 42))
  actual <- ggplot2::ggplot_build(p)$data[[1]]$x
  standard <- ggplot2::ggplot(data.frame(x, y = 0), ggplot2::aes(x, y)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = layout$width, height = 0.06, seed = 42))
  expect_identical(actual, ggplot2::ggplot_build(standard)$data[[1]]$x)
  expect_identical(actual <= layout$boundary, x <= 5)
  expect_true(any(actual[x == 5] > 5))
  expect_true(any(actual[x == 5] < 5))
  expect_equal(.margot_policy_band_layout(x, 5, 0), list(width = 0, boundary = 5))
  expect_equal(.margot_policy_band_layout(x[x <= 5], 5, 0.3), list(width = 0, boundary = 5))
})
