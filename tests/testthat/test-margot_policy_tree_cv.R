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
  expect_true(all(c(
    "gain_vs_control_mean", "gain_vs_treat_mean",
    "gain_vs_best_constant_mean", "value_best_constant_mean",
    "best_constant_action", "n_selected_actions_max",
    "uniform_selected_action_all"
  ) %in% names(out$value_summary)))
  expect_true("gain_vs_best_constant" %in% names(out$fold_values))
  expect_true(any(out$split_summary$node_id == 1L))
  expect_true(nrow(out$leaf_values) > 0)
  expect_true(all(c(
    "sample_share", "treatment_control_contrast",
    "estimated_treatment_contrast", "estimated_advantage",
    "estimated_gain", "contrast", "score_contrast"
  ) %in% names(out$leaf_values)))
  expect_true(all(out$leaf_values$score_contrast == "treatment_minus_control"))
  expect_equal(out$leaf_values$treatment_control_contrast, out$leaf_values$estimated_treatment_contrast)
  expect_equal(
    out$leaf_values$estimated_advantage,
    ifelse(out$leaf_values$action_id == 2L, out$leaf_values$treatment_control_contrast, -out$leaf_values$treatment_control_contrast)
  )
  expect_equal(out$leaf_values$estimated_advantage, out$leaf_values$estimated_gain)
  expect_true(nrow(out$leaf_summary) > 0)
  expect_true("treatment_control_contrast_mean" %in% names(out$leaf_summary))
  expect_true("score_contrast" %in% names(out$leaf_summary))
})

test_that("margot_policy_tree_cv resolves reversed named action-score columns", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(50)
  n <- 90
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    treated = rep(1, n),
    control = rep(0, n)
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
    depths = 1,
    num_folds = 3,
    n_repeats = 1,
    tree_method = "policytree",
    seed = 50,
    verbose = FALSE
  )

  expect_equal(out$value_summary$value_control_all_mean, 0, tolerance = 1e-8)
  expect_equal(out$value_summary$value_treat_all_mean, 1, tolerance = 1e-8)
  expect_equal(out$value_summary$best_constant_action, "treated")
  expect_true(all(out$fold_values$coverage == 1))
})

test_that("constant comparator is selected in training and evaluated held out", {
  old_options <- options(margot.policy_tree.min_node_size = 1L)
  on.exit(options(old_options), add = TRUE)
  n <- 12L
  fold_id <- .policy_cv_make_folds(n, num_folds = 3L, seed = 102L)
  x <- data.frame(x1 = seq_len(n), x2 = seq_len(n) %% 2L)
  treatment_score <- ifelse(fold_id == 1L, 10, -1)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = cbind(control = rep(0, n), treated = treatment_score),
        top_vars = c("x1", "x2")
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  out <- margot_policy_tree_cv(
    object,
    model_names = "y",
    depths = 1,
    num_folds = 3,
    n_repeats = 1,
    tree_method = "policytree",
    min_node_size = 1L,
    seed = 101L,
    verbose = FALSE
  )

  fold_one <- out$fold_values[out$fold_values$fold == 1L, , drop = FALSE]
  expect_equal(fold_one$best_constant_action, "control")
  expect_equal(fold_one$value_best_constant, 0)
  expect_equal(fold_one$validation_best_constant_action, "treated")
  expect_equal(fold_one$value_validation_best_constant, 10)
  expect_lt(out$value_summary$value_best_constant_mean,
            out$value_summary$value_validation_best_constant_mean)
})

test_that("training weights enter the tree objective exactly once", {
  scores <- cbind(
    control = c(1, 2),
    treated = c(3, 4)
  )
  weighted <- .policy_cv_training_scores(scores, weights = c(2, 5))

  expect_equal(weighted, scores * c(2, 5))
  expect_equal(.policy_cv_training_scores(scores), scores)
  expect_error(
    .policy_cv_training_scores(scores, weights = c(1, 0)),
    "finite, positive"
  )
})

test_that("value-only depth and constant thresholds use the registered 0.01 margin", {
  value_summary <- data.frame(
    model = c("model_y", "model_y"),
    outcome = c("y", "y"),
    outcome_label = c("Y", "Y"),
    depth = c(1L, 2L),
    gain_vs_control_mean = c(0.20, 0.21),
    value_policy_mean = c(0.20, 0.21),
    value_best_constant_mean = c(0.20, 0.20),
    best_constant_action = c("control", "control"),
    stringsAsFactors = FALSE
  )
  depth_selection <- .policy_cv_select_depths(
    value_summary = value_summary,
    split_summary = data.frame(),
    min_gain_for_depth_switch = 0.01,
    depth_selection_rule = "value_only",
    max_stability_loss_for_depth_switch = 0.05,
    min_root_stability_for_depth_switch = 0.5
  )
  policy_selection <- .policy_cv_select_policy(
    value_summary = value_summary,
    depth_selection = depth_selection,
    min_gain_over_constant = 0.01
  )

  expect_equal(depth_selection$selected_depth, 2L)
  expect_equal(depth_selection$depth_selection_rule, "value_only")
  expect_equal(depth_selection$reason, "depth two clears the held-out value threshold")
  expect_equal(policy_selection$preferred_policy, "tree")
  expect_equal(policy_selection$tree_minus_honest_constant, 0.01, tolerance = 1e-12)

  value_summary$value_policy_mean[value_summary$depth == 2L] <- 0.209
  below <- .policy_cv_select_policy(
    value_summary = value_summary,
    depth_selection = depth_selection,
    min_gain_over_constant = 0.01
  )
  expect_equal(below$preferred_policy, "constant")
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
      calls$cv_min_node_size <- list(...)$min_node_size
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

  wf <- margot_policy_workflow(
    stability,
    include_interpretation = FALSE,
    policy_tree_min_node_size = 23L
  )

  expect_equal(calls$cv_min_node_size, 23L)
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
