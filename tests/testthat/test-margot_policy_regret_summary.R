test_that("policy_regret_summary reports value and regret for stored trees", {
  set.seed(42)
  n <- 80
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 1, -0.5)
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

  out <- policy_regret_summary(
    object,
    model_names = "y",
    depths = 1,
    R = 25,
    seed = 1
  )

  expect_s3_class(out, "margot_policy_regret_summary")
  expect_equal(nrow(out), 1L)
  expect_equal(out$model, "model_y")
  expect_equal(out$depth, 1L)
  expect_true(out$gain_vs_control > 0)
  expect_true(out$regret_vs_oracle >= 0)
  expect_named(out, c(
    "model", "outcome", "outcome_label", "depth", "n", "treat_share",
    "oracle_treat_share", "oracle_disagreement", "value_policy",
    "value_control_all", "value_treat_all", "value_oracle",
    "gain_vs_control", "gain_vs_control_se", "gain_vs_control_lower",
    "gain_vs_control_upper", "gain_vs_treat", "gain_vs_treat_se",
    "gain_vs_treat_lower", "gain_vs_treat_upper", "regret_vs_oracle",
    "regret_vs_oracle_se", "regret_vs_oracle_lower",
    "regret_vs_oracle_upper", "split_variables"
  ))
})

test_that("margot_policy_regret_summary aligns not_missing rows", {
  set.seed(42)
  n <- 60
  x_full <- data.frame(
    x1 = c(NA_real_, stats::rnorm(n)),
    x2 = c(0, stats::rnorm(n))
  )
  keep <- which(stats::complete.cases(x_full))
  x <- x_full[keep, , drop = FALSE]
  gamma <- cbind(
    control = rep(0, nrow(x)),
    treated = ifelse(x$x1 > 0, 1, -0.5)
  )
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree
      )
    ),
    covariates = x_full,
    not_missing = keep,
    weights = rep(1, nrow(x_full))
  )

  out <- margot_policy_regret_summary(object, depths = 1, R = 25, seed = 1)

  expect_equal(out$n, nrow(x))
  expect_true(is.finite(out$value_policy))
})

test_that("margot_plot_policy_regret returns a ggplot", {
  regret <- tibble::tibble(
    model = c("model_y", "model_z"),
    outcome = c("y", "z"),
    outcome_label = c("Y", "Z"),
    depth = c(2L, 2L),
    regret_vs_oracle = c(0.20, 0.35),
    regret_vs_oracle_lower = c(0.10, 0.25),
    regret_vs_oracle_upper = c(0.30, 0.45),
    gain_vs_treat = c(0.04, -0.01),
    gain_vs_treat_lower = c(0.00, -0.05),
    gain_vs_treat_upper = c(0.08, 0.03),
    gain_vs_control = c(0.18, 0.22),
    gain_vs_control_lower = c(0.12, 0.16),
    gain_vs_control_upper = c(0.24, 0.28)
  )

  p <- margot_plot_policy_regret(regret, metrics = "regret_vs_oracle")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Policy-tree regret")
})

test_that("margot_plot_policy_regret filters outcomes and validates metrics", {
  regret <- tibble::tibble(
    model = c("model_y", "model_z"),
    outcome = c("y", "z"),
    outcome_label = c("Y", "Z"),
    depth = c(1L, 2L),
    regret_vs_oracle = c(0.20, 0.35),
    regret_vs_oracle_lower = c(0.10, 0.25),
    regret_vs_oracle_upper = c(0.30, 0.45)
  )

  p <- margot_plot_policy_regret(
    regret,
    metrics = "regret_vs_oracle",
    depths = 2,
    model_names = "z"
  )

  built <- ggplot2::ggplot_build(p)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(built$data, nrow, integer(1)) > 0L))
  expect_error(
    margot_plot_policy_regret(regret, metrics = "not_a_metric"),
    "unsupported metric"
  )
})
