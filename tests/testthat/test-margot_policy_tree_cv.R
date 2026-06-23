test_that("margot_policy_tree_cv evaluates held-out folds", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(42)
  n <- 120
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 1, -0.4) + ifelse(x$x2 > 1, 0.15, 0)
  )
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        top_vars = c("x1", "x2")
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  out <- margot_policy_tree_cv(
    object,
    model_names = "y",
    depths = c(1, 2),
    num_folds = 3,
    n_repeats = 2,
    tree_method = "policytree",
    seed = 99,
    verbose = FALSE
  )

  expect_s3_class(out, "margot_policy_tree_cv")
  expect_equal(nrow(out$fold_values), 12L)
  expect_true(all(out$fold_values$n_eval > 0))
  expect_named(out$depth_map, "model_y")
  expect_true(out$depth_map[["model_y"]] %in% c(1L, 2L))
  expect_true(all(c("gain_vs_control_mean", "gain_vs_treat_mean") %in% names(out$value_summary)))
  expect_true(any(out$split_summary$node_id == 1L))
  expect_true(nrow(out$leaf_values) > 0)
  expect_true(all(c("sample_share", "estimated_advantage", "estimated_gain", "contrast") %in% names(out$leaf_values)))
  expect_equal(out$leaf_values$estimated_advantage, out$leaf_values$estimated_gain)
  expect_true(nrow(out$leaf_summary) > 0)
})

test_that("margot_policy_tree_cv aligns not_missing rows and weights", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(43)
  n <- 90
  x_full <- data.frame(
    x1 = c(NA_real_, stats::rnorm(n)),
    x2 = c(0, stats::rnorm(n))
  )
  keep <- which(stats::complete.cases(x_full))
  x <- x_full[keep, , drop = FALSE]
  gamma <- cbind(
    control = rep(0, nrow(x)),
    treated = ifelse(x$x1 > 0, 0.8, -0.2)
  )
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        top_vars = c("x1", "x2")
      )
    ),
    covariates = x_full,
    not_missing = keep,
    weights = rep(1, nrow(x_full))
  )

  out <- margot_policy_tree_cv(
    object,
    depths = 1,
    num_folds = 3,
    n_repeats = 1,
    tree_method = "policytree",
    verbose = FALSE
  )

  expect_equal(sum(out$fold_values$n_eval), nrow(x))
  expect_true(is.finite(out$value_summary$gain_vs_control_mean))
})

test_that("margot_policy_workflow uses held-out depth map when available", {
  calls <- new.env(parent = emptyenv())
  stability <- structure(
    list(
      covariates = data.frame(x1 = 1:5),
      results = list(
        model_a = list(
          dr_scores = cbind(control = rep(0, 5), treated = rep(1, 5)),
          top_vars = "x1"
        ),
        model_b = list(
          dr_scores = cbind(control = rep(0, 5), treated = rep(-1, 5)),
          top_vars = "x1"
        )
      ),
      metadata = list(tree_method = "policytree")
    ),
    class = c("margot_stability_policy_tree", "margot_policy_tree", "list")
  )

  testthat::local_mocked_bindings(
    margot_policy_tree_cv = function(...) {
      list(
        depth_map = c(model_a = 1L),
        depth_selection = data.frame(
          model = "model_a",
          outcome = "a",
          outcome_label = "A",
          selected_depth = 1L,
          pv_depth1 = 0.2,
          pv_depth2 = 0.18,
          depth2_minus_depth1 = -0.02,
          depth1_root_stability = 0.8,
          depth2_root_stability = 0.6,
          stability_ok = FALSE,
          reason = "depth two loses too much root-split stability"
        ),
        metadata = list(num_folds = 5L, n_repeats = 10L)
      )
    },
    margot_policy_summary_compare_depths = function(..., model_names) {
      calls$compare_model_names <- model_names
      list(
        depth_map = c(model_a = 1L),
        depth_summary_df = data.frame(
          model = "model_a",
          outcome = "a",
          outcome_label = "A",
          depth_selected = 1L,
          depth_label = "depth 1",
          pv_depth1 = 0.1,
          pv_depth2 = 0.09,
          pv_selected = 0.1,
          pv_alternative = 0.09,
          pv_gain = 0.01
        )
      )
    },
    margot_policy_summary_report = function(..., model_names, depths_by_model) {
      calls$summary_model_names <- model_names
      calls$summary_depths <- depths_by_model
      list(wins_model_ids = character(0), borderline_model_ids = character(0),
           recommended_model_ids = character(0), neutral_model_ids = character(0),
           group_table_df = data.frame(), report = "", report_prose = "",
           coherent_policy_values = NULL, unit_masks = list())
    },
    margot_build_method_explanation = function(...) list(long = "", short = "", prereg = "")
  )

  wf <- margot_policy_workflow(stability, include_interpretation = FALSE)

  expect_equal(calls$compare_model_names, c(model_a = 1L))
  expect_equal(calls$summary_model_names, "model_a")
  expect_equal(calls$summary_depths, c(model_a = 1L))
  expect_equal(wf$heldout_policy$depth_map, c(model_a = 1L))
  expect_true(grepl("held-out policy-tree CV", wf$depth_comparison_report$text, fixed = TRUE))
  expect_true(grepl("depth two loses too much root-split stability", wf$depth_comparison_report$text, fixed = TRUE))
})

test_that("held-out policy CV export has manual docs and S3 registration", {
  expect_true(is.function(getS3method("print", "margot_policy_tree_cv", optional = TRUE)))
  root <- getwd()
  while (!file.exists(file.path(root, "DESCRIPTION")) && dirname(root) != root) {
    root <- dirname(root)
  }
  if (!file.exists(file.path(root, "NAMESPACE"))) {
    skip("source package files are not available in this installed-package check")
  }
  namespace <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  expect_true(any(namespace == "S3method(print,margot_policy_tree_cv)"))
  expect_true(file.exists(file.path(root, "man/margot_policy_tree_cv.Rd")))
})
