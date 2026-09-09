# exercise cut-point ties and transformed plotting coordinates without participant data.
test_that("restricted jitter retains every split inequality and preserves RNG state", {
  x <- rep(c(0, 4.9, 5, 5.0001, 5.1, 5.2, 6, 7), each = 50)
  df <- data.frame(x, y = 0)
  set.seed(104)
  before <- .Random.seed
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point(
    position = .margot_policy_position_jitter(0.3, 0.06, 42, "within_splits", c(5, 5.1, 6)))
  points <- ggplot2::ggplot_build(p)$data[[1]]
  expect_identical(.Random.seed, before)
  for (cut in c(5, 5.1, 6)) expect_identical(points$x <= cut, x <= cut)
  expect_true(all(abs(points$x - x) <= 0.3 + 1e-12))
  expect_true(all(abs(points$y) <= 0.06))
  expect_gt(length(unique(points$x[x == 5])), 40)
  expect_identical(points$x, ggplot2::ggplot_build(p)$data[[1]]$x)
  expect_identical(points$y, ggplot2::ggplot_build(p)$data[[1]]$y)
})

test_that("split bounds follow log and reverse scales with the inclusive side intact", {
  x <- rep(c(0, 4.9, 5, 5.01, 6, 10), each = 20)
  for (transformation in c("log1p", "reverse")) {
    p <- ggplot2::ggplot(data.frame(x, y = 0), ggplot2::aes(x, y)) +
      ggplot2::geom_point(position = .margot_policy_position_jitter(
        0.3, 0.06, 42, "within_splits", 5)) +
      ggplot2::scale_x_continuous(transform = transformation)
    transform <- p$scales$get_scales("x")$get_transformation()
    points <- ggplot2::ggplot_build(p)$data[[1]]
    expect_identical(transform$inverse(points$x) <= 5, x <= 5)
    expect_true(all(abs(points$x - transform$transform(x)) <= 0.3 + 1e-12))
  }
})

test_that("both plotted coordinates respect split bounds and zero widths", {
  x <- rep(c(0, 1, 1.01), each = 30)
  y <- rep(c(2.99, 3, 3.01), 30)
  p <- ggplot2::ggplot(data.frame(x, y), ggplot2::aes(x, y)) +
    ggplot2::geom_point(position = .margot_policy_position_jitter(
      0.3, 0.3, 42, "within_splits", 1, 3))
  points <- ggplot2::ggplot_build(p)$data[[1]]
  expect_identical(points$x <= 1, x <= 1)
  expect_identical(points$y <= 3, y <= 3)
  expect_identical(.margot_jitter_within_splits(x, x, 0, 1), x)
  expect_error(.margot_policy_position_jitter(-1, 0, 42, "within_splits"), "non-negative")
})

test_that("standard jitter retains ggplot positions", {
  df <- data.frame(x = rep(5, 100), y = 0)
  native <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_jitter(width = 0.3, height = 0.06)
  # compare position objects directly with an explicit seed.
  native$layers[[1]]$position <- ggplot2::position_jitter(width = 0.3, height = 0.06, seed = 42)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point(
    position = .margot_policy_position_jitter(0.3, 0.06, 42, "standard", 5))
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$x, ggplot2::ggplot_build(native)$data[[1]]$x)
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$y, ggplot2::ggplot_build(native)$data[[1]]$y)
})

test_that("the combo API forwards bounded jitter without changing assigned actions", {
  skip_if_not_installed("policytree")
  x <- data.frame(value = rep(1:7, each = 30))
  scores <- cbind(control = 0, treated = ifelse(x$value <= 5, 1, -1))
  tree <- policytree::policy_tree(x, scores, depth = 1, min.node.size = 5)
  object <- list(results = list(model_y = list(policy_tree_depth_1 = tree,
    plot_data = list(X_test = x))))
  args <- list(jitter_method = "within_splits", jitter_width = 0.3,
    jitter_height = 0.06, jitter_seed = 42, point_alpha = 0.1)
  p <- margot_plot_policy_combo(object, "model_y", max_depth = 1,
    generate_decision_tree = FALSE, policy_tree_args = args)$policy_tree
  built <- ggplot2::ggplot_build(p)$data
  cut <- tree$nodes[[1]]$split_value
  expect_identical(built[[1]]$x <= cut, x$value <= cut)
  expect_identical(as.integer(p$layers[[1]]$data$pred), as.integer(predict(tree, x)))
  expect_equal(built[[2]]$xintercept, cut)
})

test_that("the public depth-two plot preserves root and child split inequalities", {
  skip_if_not_installed("policytree")
  x <- expand.grid(x1 = rep(1:4, each = 5), x2 = 1:4)
  good <- ifelse(x$x1 <= 2, x$x2 <= 2, x$x2 > 3)
  scores <- cbind(control = 0, treated = ifelse(good, 1, -1))
  tree <- policytree::policy_tree(x, scores, depth = 2, min.node.size = 5)
  object <- list(results = list(model_y = list(policy_tree_depth_2 = tree,
    plot_data = list(X_test = x))))
  p <- margot_plot_policy_tree(object, "model_y", max_depth = 2,
    jitter_method = "within_splits", jitter_seed = 42, shading = FALSE)
  for (i in 1:2) {
    panel <- p[[i]]
    point_layer <- which(vapply(panel$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
    raw <- panel
    raw$layers[[point_layer]] <- ggplot2::ggproto(NULL, panel$layers[[point_layer]],
      position = ggplot2::position_identity())
    actual <- ggplot2::ggplot_build(panel)$data[[point_layer]]
    original <- ggplot2::ggplot_build(raw)$data[[point_layer]]
    position <- panel$layers[[point_layer]]$position
    for (cut in position$x_splits) expect_identical(actual$x <= cut, original$x <= cut)
    for (cut in position$y_splits) expect_identical(actual$y <= cut, original$y <= cut)
    expect_identical(actual$colour, original$colour)
  }
})
