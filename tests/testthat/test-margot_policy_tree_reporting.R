test_that("policy-tree reporting helpers return modular artefacts", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(47)
  n <- 90
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 0.7, -0.2)
  )
  tree <- policytree::policy_tree(x, gamma, depth = 1, min.node.size = 5)
  tree_depth_2 <- policytree::policy_tree(x, gamma, depth = 2, min.node.size = 5)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        policy_tree_depth_1 = tree,
        policy_tree_depth_2 = tree_depth_2,
        plot_data = list(X_test = x)
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  table <- margot_table_policy_tree(object, "y", depth = 1)
  expect_s3_class(table, "tbl_df")
  expect_equal(attr(table, "source"), "display_tree")
  expect_true(all(c(
    "source", "model", "outcome_label", "depth", "node_id",
    "selected_action", "tc_score_contrast", "score_interval",
    "sample_percent", "direction", "n_selected_actions",
    "uniform_selected_action", "label"
  ) %in% names(table)))
  expect_identical(
    names(table)[seq_len(10)],
    c(
      "source", "model", "outcome_label", "depth", "node_id",
      "selected_action", "tc_score_contrast", "score_interval",
      "sample_percent", "direction"
    )
  )
  expect_false("estimated_advantage" %in% names(table))
  expect_false("value_contribution_vs_control" %in% names(table))
  expect_true(any(grepl("T-C:", table$label, fixed = TRUE)))

  diagnostic_table <- margot_table_policy_tree(
    object,
    "y",
    depth = 1,
    include_selected_action_difference = TRUE,
    include_value_contribution = TRUE
  )
  expect_true("selected_action_minus_alternative_score" %in% names(diagnostic_table))
  expect_true("value_contribution" %in% names(diagnostic_table))

  text <- margot_text_policy_tree(source = "display_tree")
  expect_type(text, "character")
  expect_true(grepl("T-C", text, fixed = TRUE))
  expect_true(grepl("post-selection", text, fixed = TRUE))

  decision <- margot_plot_policy_decision_tree(
    object,
    model_name = "model_y",
    max_depth = 1,
    leaf_metrics = table
  )
  projection <- margot_plot_policy_projection(
    object,
    model_name = "y",
    max_depth = 1
  )
  panels <- margot_plot_policy_tree_panels(
    object,
    model_name = "model_y",
    max_depth = 1,
    leaf_metrics = table
  )

  expect_s3_class(decision, "ggplot")
  expect_s3_class(projection, "ggplot")
  expect_s3_class(panels, "margot_policy_tree_panels")
  expect_false(is.null(panels$combined_plot))

  report <- margot_report_policy_tree(
    object,
    model_name = "y",
    depth = 1,
    include_plots = FALSE
  )
  expect_s3_class(report, "margot_policy_tree_report")
  expect_s3_class(report$table, "tbl_df")
  expect_true(grepl("T-C", report$text, fixed = TRUE))
  expect_equal(report$metadata$contrast, "treatment_minus_control")

  cv <- structure(
    list(
      leaf_summary = data.frame(),
      value_summary = data.frame(),
      depth_map = c(model_y = 2L)
    ),
    class = c("margot_policy_tree_cv", "list")
  )
  selected_report <- margot_report_policy_tree(
    object,
    model_name = "y",
    policy_cv = cv,
    include_plots = FALSE,
    include_policy_value = FALSE
  )
  expect_equal(selected_report$metadata$depth, 2L)
})

test_that("held-out policy reporting exposes action-score and value summaries", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(48)
  n <- 90
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 0.6, -0.2)
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

  cv <- margot_policy_tree_cv(
    object,
    model_names = "y",
    depths = 1,
    num_folds = 3,
    n_repeats = 1,
    tree_method = "policytree",
    seed = 48,
    verbose = FALSE
  )
  leaf_table <- margot_table_policy_tree(cv, model_name = "y", source = "heldout_cv")
  value_table <- margot_table_policy_value(cv, model_name = "model_y")

  expect_s3_class(leaf_table, "tbl_df")
  expect_true(all(c("tc_score_contrast", "selected_action", "score_interval") %in% names(leaf_table)))
  expect_false("estimated_advantage" %in% names(leaf_table))
  expect_s3_class(value_table, "tbl_df")
  expect_true(all(c(
    "value_control_all", "value_treat_all", "value_best_constant",
    "gain_vs_best_constant", "best_constant_action"
  ) %in% names(value_table)))
})

test_that("held-out policy tables report uniform actions within model-depth groups", {
  leaf_summary <- data.frame(
    model = c("model_a", "model_a", "model_b", "model_b"),
    outcome = c("a", "a", "b", "b"),
    outcome_label = c("A", "A", "B", "B"),
    depth = c(1L, 1L, 1L, 1L),
    action = c("control", "control", "control", "treated"),
    action_label = c("Control", "Control", "Control", "Treatment"),
    n_eval_leaf = c(20L, 30L, 25L, 25L),
    sample_share_mean = c(0.4, 0.6, 0.5, 0.5),
    treatment_control_contrast_mean = c(-0.1, -0.2, -0.1, 0.2),
    estimated_advantage_mean = c(0.1, 0.2, 0.1, 0.2),
    value_contribution_vs_control_mean = c(0, 0, 0, 0.1),
    value_contribution_vs_treatment_mean = c(0.04, 0.12, 0.05, 0),
    treatment_control_contrast_q025 = c(-0.2, -0.3, -0.2, 0.1),
    treatment_control_contrast_q975 = c(0, -0.1, 0, 0.3)
  )
  cv <- structure(
    list(
      leaf_summary = leaf_summary,
      depth_map = c(model_a = 1L, model_b = 1L)
    ),
    class = c("margot_policy_tree_cv", "list")
  )

  table <- margot_table_policy_tree(cv, source = "heldout_cv")
  model_a <- table[table$model == "model_a", , drop = FALSE]
  model_b <- table[table$model == "model_b", , drop = FALSE]

  expect_true(all(model_a$n_selected_actions == 1L))
  expect_true(all(model_a$uniform_selected_action))
  expect_true(all(model_b$n_selected_actions == 2L))
  expect_false(any(model_b$uniform_selected_action))
})
