test_that("threshold references use raw weighted contrasts on the stated scale", {
  scores <- cbind(control = c(10, 20, 30), treated = c(12, 28, 34))
  weights <- c(1, 3, 0)
  ref <- margot_policy_value_threshold(scores, weights, "ate")
  expect_s3_class(ref, "margot_policy_value_threshold")
  expect_equal(ref$value, 6.5)
  expect_equal(ref$development_ate, 6.5)
  expect_equal(ref$development_weight_sum, 4)
  expect_equal(ref$source, "ate")
  expect_equal(margot_policy_value_threshold(scores, weights, "ate", 0.5)$value, 3.25)
  expect_equal(margot_policy_value_threshold(scores * 100, weights, "ate")$value, 650)
  expect_equal(margot_policy_value_threshold(scores, weights * 9, "ate")$value, 6.5)
  expect_equal(margot_policy_value_threshold(scores[, 2:1], weights, "ate")$value, 6.5)
  expect_equal(margot_policy_value_threshold(unname(scores), weights, "ate", treatment_column = 2, control_column = 1)$value, 6.5)
  expect_equal(.policy_value_net_scores(scores, ref), cbind(control = c(10, 20, 30), treated = c(5.5, 21.5, 27.5)))
  expect_equal(.policy_cv_training_scores(.policy_value_net_scores(scores[1:2, ], ref), weights[1:2]),
               cbind(control = c(10, 60), treated = c(5.5, 64.5)))
})

test_that("fixed signed and null references preserve original rewards and signs", {
  scores <- cbind(control = c(0, 0), treated = c(-4, -2))
  zero <- margot_policy_value_threshold(scores)
  expect_identical(.policy_value_net_scores(scores, zero), scores)
  ref <- margot_policy_value_threshold(scores, value_threshold = "ate")
  expect_equal(ref$value, -3)
  expect_equal(.policy_value_net_scores(scores, ref)[, 2], c(-1, 1))
  expect_equal(scores[, 2], c(-4, -2))
  expect_equal(margot_policy_value_threshold(scores, value_threshold = -2, threshold_multiplier = 1.5)$value, -3)
  null <- cbind(control = c(0, 0), treated = c(0, 0))
  expect_equal(margot_policy_value_threshold(null, value_threshold = "ate")$value, 0)
  expect_equal(margot_policy_value_threshold(scores, value_threshold = "ate", threshold_multiplier = 0)$value, 0)
})

test_that("threshold inputs fail closed", {
  scores <- cbind(control = 0:2, treated = 1:3)
  for (invalid in list(NA_real_, Inf, numeric(), c(0, 1), "ATE", list(value = 0))) {
    expect_error(margot_policy_value_threshold(scores, value_threshold = invalid), "value_threshold")
  }
  for (invalid in list(NA_real_, Inf, -1, c(1, 2))) {
    expect_error(margot_policy_value_threshold(scores, threshold_multiplier = invalid), "threshold_multiplier")
  }
  for (invalid in list(c(1, 1), c(0, 0, 0), c(1, -1, 1), c(1, Inf, 1))) {
    expect_error(margot_policy_value_threshold(scores, weights = invalid), "weights")
  }
  expect_error(margot_policy_value_threshold(scores, treatment_column = 1), "distinct")
  expect_error(margot_policy_value_threshold(scores, treatment_column = 1, control_column = 1), "distinct")
  expect_error(margot_policy_value_threshold(unname(scores)), "requires dr_scores column names")
  scores[1, 1] <- NA_real_
  expect_error(margot_policy_value_threshold(scores), "finite numeric")
})

test_that("centred constant comparisons tie to control at machine precision", {
  scores <- cbind(treated = c(0.13, 1.25, 0.44, 7.29), control = c(0.1, 0.2, 0.3, 0.4))
  weights <- c(1, 1e5, 1e-3, 13)
  ref <- margot_policy_value_threshold(scores, weights, "ate")
  net <- .policy_value_net_scores(scores, ref)
  tol <- .policy_value_constant_tolerance(scores, weights)
  selection <- .policy_cv_select_constant(net, weights, tie_tolerance = tol)
  expect_equal(selection$action, "control")
  expect_equal(selection$action_id, 2L)
  net[, 1] <- net[, 1] + 100 * tol
  expect_equal(.policy_cv_select_constant(net, weights, tie_tolerance = tol)$action, "treated")
  expect_error(.policy_cv_select_constant(net, weights, tie_tolerance = Inf), "tie_tolerance")
})

# build a deterministic two-group fixture without fitting nuisance models.
threshold_cv_fixture <- function() {
  n <- 48L
  x <- data.frame(group = rep(0:1, each = n / 2), order = seq_len(n))
  list(covariates = x, weights = rep(c(1, 3, 2), length.out = n),
       results = list(model_y = list(top_vars = "group", dr_scores = cbind(
         control = rep(0.2, n), treated = 0.2 + ifelse(x$group == 1, 0.08, 0.02)))))
}

test_that("CV carries training ATE unchanged and keeps raw leaf effects", {
  skip_if_not_installed("policytree")
  object <- threshold_cv_fixture()
  args <- list(model_results = object, depths = 1, num_folds = 3, n_repeats = 1,
               tree_method = "policytree", min_node_size = 2, seed = 74, verbose = FALSE,
               value_threshold = "ate")
  out <- do.call(margot_policy_tree_cv, args)
  expect_equal(nrow(out$fold_values), 3)
  expect_true(all(out$fold_values$best_constant_action == "control"))
  expect_true(all(out$leaf_values$treatment_control_contrast > 0))
  expect_true(all(out$leaf_values$treatment_control_contrast %in% c(0.02, 0.08) |
                    abs(out$leaf_values$treatment_control_contrast - 0.02) < 1e-12 |
                    abs(out$leaf_values$treatment_control_contrast - 0.08) < 1e-12))
  folds <- .policy_cv_make_folds(48, 3, 75)
  for (fold in 1:3) {
    train <- folds != fold
    expected <- stats::weighted.mean(ifelse(object$covariates$group[train] == 1, .08, .02), object$weights[train])
    row <- out$fold_values[out$fold_values$fold == fold, ]
    expect_equal(row$value_threshold, expected)
    expect_equal(row$original_value_policy - row$value_policy, expected * row$coverage)
    expect_equal(row$original_value_treat_all - row$value_treat_all, expected)
    expect_equal(row$original_value_control_all, row$value_control_all)
  }
  changed <- object
  changed$results$model_y$dr_scores[folds == 1, 2] <- 100
  args$model_results <- changed
  other <- do.call(margot_policy_tree_cv, args)
  first <- out$fold_values[out$fold_values$fold == 1, ]
  altered <- other$fold_values[other$fold_values$fold == 1, ]
  expect_identical(first$value_threshold, altered$value_threshold)
  expect_identical(first$coverage, altered$coverage)
  expect_identical(first$best_constant_action, altered$best_constant_action)
  expect_gt(altered$value_policy, first$value_policy)
  display <- margot_policy_tree_display(object, out, tree_method = "policytree", min_node_size = 2, verbose = FALSE)
  expect_equal(display$results$model_y$value_threshold$value,
               stats::weighted.mean(ifelse(object$covariates$group == 1, .08, .02), object$weights))
  expect_true(all(display$leaf_table$treatment_control_contrast > 0))
})

test_that("default zero and explicit zero CV retain identical scientific outputs", {
  skip_if_not_installed("policytree")
  args <- list(model_results = threshold_cv_fixture(), depths = 1, num_folds = 2, n_repeats = 1,
               tree_method = "policytree", min_node_size = 2, seed = 12, verbose = FALSE)
  legacy <- do.call(margot_policy_tree_cv, args)
  args$value_threshold <- 0
  explicit <- do.call(margot_policy_tree_cv, args)
  expect_identical(legacy, explicit)
  expect_equal(legacy$fold_values$value_policy, legacy$fold_values$original_value_policy)
  expect_equal(legacy$metadata$value_objective, "original")
})

test_that("constant zero positive and negative effects have no centred targeting gain", {
  skip_if_not_installed("policytree")
  for (effect in c(0, 0.05, -0.05)) {
    object <- threshold_cv_fixture()
    object$results$model_y$dr_scores[, 2] <- 0.2 + effect
    out <- margot_policy_tree_cv(object, depths = 1, num_folds = 3, n_repeats = 1,
                                 tree_method = "policytree", min_node_size = 2,
                                 seed = 44, verbose = FALSE, value_threshold = "ate")
    expect_equal(out$fold_values$value_threshold, rep(effect, 3))
    expect_equal(out$fold_values$gain_vs_best_constant, rep(0, 3), tolerance = 1e-14)
    expect_true(all(out$fold_values$best_constant_action == "control"))
  }
})

test_that("summary preserves original values under both aggregation rules", {
  skip_if_not_installed("policytree")
  object <- threshold_cv_fixture()
  for (aggregation in c("fold_n_eval_weighted", "pool_score_numerators_and_weight_denominators_within_repeat")) {
    out <- margot_policy_tree_cv(object, depths = 1, num_folds = 3, n_repeats = 2,
                                 tree_method = "policytree", min_node_size = 2,
                                 seed = 29, verbose = FALSE, value_threshold = "ate",
                                 held_out_aggregation = aggregation)
    df <- out$fold_values
    expected <- if (aggregation == "fold_n_eval_weighted") {
      stats::weighted.mean(df$original_value_policy, df$n_eval)
    } else {
      mean(vapply(split(df, df$repeat_id), function(d) sum(d$original_policy_score_numerator) / sum(d$evaluation_weight_sum), numeric(1)))
    }
    expect_equal(out$value_summary$original_value_policy_mean, expected)
    expect_equal(out$value_summary$value_threshold_min, min(df$value_threshold))
    expect_equal(out$value_summary$value_threshold_max, max(df$value_threshold))
    expect_equal(out$value_summary$threshold_source, "ate")
  }
})
