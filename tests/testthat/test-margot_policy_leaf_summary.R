test_that("margot_policy_leaf_summary reports action gains and sample shares", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(42)
  n <- 80
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 0.8, -0.3)
  )
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  out <- margot_policy_leaf_summary(object, "y", depth = 1)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("node_id", "sample_share", "estimated_gain", "label") %in% names(out)))
  expect_equal(sum(out$n), n)
  expect_equal(sum(out$sample_share), 1, tolerance = 1e-8)
  expect_true(all(is.finite(out$estimated_gain)))
  expect_true(any(grepl("sample:", out$label, fixed = TRUE)))
  expect_true(all(out$action_label %in% c("Treatment", "Control")))
})

test_that("margot_policy_leaf_summary rejects non-binary action scores", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(44)
  n <- 80
  x <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  gamma <- cbind(control = rep(0, n), treated = ifelse(x$x1 > 0, 0.7, -0.2))
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = cbind(gamma, third = stats::rnorm(n)),
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  expect_error(
    margot_policy_leaf_summary(object, "y", depth = 1),
    "binary treatment"
  )
})

test_that("margot_plot_decision_tree can annotate leaf metrics", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(43)
  n <- 80
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 0.7, -0.2)
  )
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  p <- margot_plot_decision_tree(
    object,
    model_name = "model_y",
    max_depth = 1,
    show_leaf_metrics = TRUE
  )

  expect_s3_class(p, "ggplot")
  labels <- ggplot2::ggplot_build(p)$data[[2]]$label
  expect_true(any(grepl("gain:", labels, fixed = TRUE)))
  expect_true(any(grepl("sample:", labels, fixed = TRUE)))
})

test_that("margot_plot_decision_tree displays explicit leaf metrics", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(46)
  n <- 80
  x <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  gamma <- cbind(control = rep(0, n), treated = ifelse(x$x1 > 0, 0.7, -0.2))
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )
  metrics <- margot_policy_leaf_summary(object, "y", depth = 1)

  p <- margot_plot_decision_tree(
    object,
    model_name = "model_y",
    max_depth = 1,
    leaf_metrics = metrics
  )

  expect_s3_class(p, "ggplot")
  labels <- ggplot2::ggplot_build(p)$data[[2]]$label
  expect_true(any(grepl("gain:", labels, fixed = TRUE)))
  expect_true(any(grepl("sample:", labels, fixed = TRUE)))
})

test_that("margot_plot_decision_tree rejects mismatched leaf metrics", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(45)
  n <- 80
  x <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  gamma <- cbind(control = rep(0, n), treated = ifelse(x$x1 > 0, 0.7, -0.2))
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )
  metrics <- margot_policy_leaf_summary(object, "y", depth = 1)
  attr(metrics, "depth") <- 2L

  expect_error(
    margot_plot_decision_tree(
      object,
      model_name = "model_y",
      max_depth = 1,
      leaf_metrics = metrics
    ),
    "leaf_metrics"
  )
})
