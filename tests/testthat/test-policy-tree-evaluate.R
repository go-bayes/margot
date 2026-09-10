# deterministic binary fixtures separate learning inputs from evaluation inputs.
policy_evaluation_fixture <- function(tau = c(rep(.02, 4), rep(.08, 4)), evaluation_tau = tau,
                                      weights = NULL, threshold = "ate", ...) {
  X <- cbind(group = rep(0:1, each = 4))
  margot_policy_tree_evaluate(X, cbind(control = rep(0, 8), treated = tau),
    X, cbind(control = rep(0, 8), treated = evaluation_tau),
    development_weights = weights, evaluation_weights = weights,
    value_threshold = threshold, development_ids = paste0("d", 1:8),
    evaluation_ids = paste0("e", 1:8), ...)
}

test_that("homogeneous positive effects yield a constant under the ATE reference", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture(rep(.05, 8))
  expect_equal(fit$threshold$value, .05)
  expect_true(fit$constant$training_tie)
  expect_equal(fit$constant$action, "control")
  expect_true(fit$metadata$constant_selected)
  expect_equal(fit$tree$depth, 0L)
  expect_equal(fit$evaluation$value$estimate, 0)
  expect_equal(fit$evaluation$value$se, 0)
  expect_equal(fit$evaluation$leaves$estimate, .05)
  expect_equal(fit$evaluation$leaves$net_effect, 0)
  null <- policy_evaluation_fixture(rep(0, 8))
  expect_true(null$metadata$constant_selected)
  zero <- policy_evaluation_fixture(rep(.05, 8), threshold = 0)
  expect_equal(zero$constant$action, "treated")
  expect_true(zero$metadata$constant_selected)
})

test_that("same-sign heterogeneous effects produce ATE-referenced gains", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture()
  expect_equal(fit$threshold$value, .05)
  expect_false(fit$metadata$constant_selected)
  expect_equal(fit$evaluation$actions, c(rep(1L, 4), rep(2L, 4)))
  expect_equal(fit$evaluation$value$estimate, .015)
  expect_equal(fit$evaluation$leaves$estimate, c(.02, .08))
  expect_equal(fit$evaluation$leaves$net_effect, c(-.03, .03))
  expect_equal(fit$evaluation$leaves$weight_share, c(.5, .5))
  expect_equal(fit$evaluation$values$gross_value, c(.04, 0, 0, .05))
  expect_equal(fit$evaluation$values$cost, c(.025, 0, 0, .05))
  expect_equal(fit$evaluation$values$net_value, c(.015, 0, 0, 0))
  expect_equal(fit$evaluation$comparisons$estimate, rep(.015, 3))
  expect_equal(fit$evaluation$values$gross_value - fit$evaluation$values$cost,
    fit$evaluation$values$net_value)
})

test_that("negative ATE references preserve raw harms and signed net effects", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture(c(rep(-.08, 4), rep(-.02, 4)))
  expect_equal(fit$threshold$value, -.05)
  expect_equal(fit$evaluation$leaves$estimate, c(-.08, -.02))
  expect_equal(fit$evaluation$leaves$net_effect, c(-.03, .03))
  expect_equal(fit$evaluation$value$estimate, .015)
  expect_equal(fit$evaluation$values$cost[1], -.025)
})

test_that("weights apply once and common scaling leaves scientific estimates unchanged", {
  skip_if_not_installed("policytree")
  w <- rep(c(1, 3), each = 4)
  fit <- policy_evaluation_fixture(weights = w)
  rescaled <- policy_evaluation_fixture(weights = w * 157)
  expect_equal(fit$threshold$value, .065)
  expect_equal(fit$evaluation$leaves$weight_share, c(.25, .75))
  expect_equal(fit$evaluation$value$estimate, .01125)
  expect_equal(fit$evaluation$value, rescaled$evaluation$value)
  expect_equal(fit$evaluation$values, rescaled$evaluation$values)
  expect_equal(fit$evaluation$actions, rescaled$evaluation$actions)
  expect_equal(fit$evaluation$paired_influence, rescaled$evaluation$paired_influence)
})

test_that("paired inference uses disagreement scores and retains covariance", {
  skip_if_not_installed("policytree")
  tau <- c(.7, -.2, .5, -.4, .11, -.03, .08, .12)
  w <- c(1, 2, 1, 3, 2, 4, 1, 1)
  X <- cbind(group = rep(0:1, each = 4))
  dev <- cbind(control = rep(0, 8), treated = c(rep(.02, 4), rep(.08, 4)))
  # common score noise cancels within a paired comparison.
  shared <- c(9, -8, 2, 3, -5, 12, -20, 7)
  fit <- margot_policy_tree_evaluate(X, dev, X,
    cbind(control = shared, treated = shared + tau), evaluation_weights = w)
  contrast <- (fit$evaluation$actions == 2L) * (tau - .05)
  p <- w / sum(w)
  expected <- sum(p * contrast)
  se <- sqrt(8 / 7 * sum((p * (contrast - expected))^2))
  expect_equal(fit$evaluation$value$estimate, expected)
  expect_equal(fit$evaluation$value$se, se)
  expect_equal(fit$evaluation$value$lower, expected - qnorm(.975) * se)
  expect_equal(fit$evaluation$paired_influence[, 1], p * (contrast - expected))
  expect_true(all(grepl("supplied nuisance", fit$evaluation$comparisons$interval_method)))
})

test_that("evaluation outcomes cannot change threshold, comparator or fixed rule", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture()
  changed <- policy_evaluation_fixture(evaluation_tau = seq(80, 10, length.out = 8))
  expect_identical(fit$tree, changed$tree)
  expect_identical(fit$rule_id, changed$rule_id)
  expect_identical(fit$threshold, changed$threshold)
  expect_identical(fit$constant, changed$constant)
  expect_identical(fit$metadata$development_id, changed$metadata$development_id)
  expect_false(identical(fit$metadata$evaluation_id, changed$metadata$evaluation_id))
  expect_false(identical(fit$evaluation$value$estimate, changed$evaluation$value$estimate))
})

test_that("sample and column integrity errors never silently filter or reorder rows", {
  skip_if_not_installed("policytree")
  X <- cbind(group = rep(0:1, each = 4), second = 1:8)
  scores <- cbind(control = rep(0, 8), treated = seq_len(8))
  call <- function(...) margot_policy_tree_evaluate(X, scores, X, scores, ...)
  expect_error(call(development_ids = 1:8, evaluation_ids = 8:15), "disjoint")
  expect_error(call(development_ids = 1:8), "both")
  expect_error(call(development_ids = rep(1, 8), evaluation_ids = 9:16), "unique")
  expect_error(call(evaluation_weights = rep(0, 8)), "weights")
  expect_error(call(development_weights = c(-1, rep(1, 7))), "weights")
  expect_error(call(gain_margin = -.01), "gain_margin")
  expect_error(call(depth = 3), "depth")
  expect_error(call(min_node_size = 9), "exceeds")
  expect_error(margot_policy_tree_evaluate(X, scores, X[, 2:1], scores), "column names and order")
  expect_error(margot_policy_tree_evaluate(X, scores, X, scores[, 2:1]), "column names and order")
  scores[1, 1] <- NA_real_
  expect_error(margot_policy_tree_evaluate(X, scores, X, scores), "finite")
})

test_that("empty and single-record evaluation leaves are explicitly unavailable", {
  skip_if_not_installed("policytree")
  X <- cbind(group = rep(0:1, each = 4))
  scores <- cbind(control = rep(0, 8), treated = c(rep(.02, 4), rep(.08, 4)))
  fit <- margot_policy_tree_evaluate(X, scores, cbind(group = c(0, 1, 1)),
    cbind(control = rep(0, 3), treated = c(.02, .08, .08)))
  expect_equal(fit$evaluation$leaves$interval_type, c("unavailable", "pointwise"))
  empty <- margot_policy_tree_evaluate(X, scores, cbind(group = c(0, 0)),
    cbind(control = c(0, 0), treated = c(.02, .02)))
  expect_true(is.na(empty$evaluation$leaves$estimate[2]))
  expect_equal(empty$evaluation$leaves$weight_share, c(1, 0))
  expect_equal(empty$evaluation$leaves$interval_type[2], "unavailable")
})

test_that("explicit action columns, scale changes and deterministic constants work", {
  skip_if_not_installed("policytree")
  X <- cbind(group = rep(0:1, each = 4))
  tau <- c(rep(.02, 4), rep(.08, 4))
  scores <- cbind(receive = tau, avoid = rep(0, 8))
  fit <- margot_policy_tree_evaluate(X, scores, X, scores,
    treatment_column = 1, control_column = 2)
  expect_equal(fit$constant$action_id, 2L)
  expect_equal(fit$evaluation$actions, c(rep(2L, 4), rep(1L, 4)))
  scale <- margot_policy_tree_evaluate(X, scores * 5, X, scores * 5,
    treatment_column = 1, control_column = 2, gain_margin = .05)
  expect_equal(scale$evaluation$actions, fit$evaluation$actions)
  expect_equal(scale$evaluation$value$estimate, fit$evaluation$value$estimate * 5)
  expect_equal(scale$evaluation$value$se, fit$evaluation$value$se * 5)
  expect_equal(scale$threshold$value, fit$threshold$value * 5)
})

test_that("requested engines are honoured without fallback", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture()
  expect_identical(fit$metadata$requested_engine, "policytree")
  expect_identical(fit$metadata$realised_engine, "policytree")
  if (requireNamespace("fastpolicytree", quietly = TRUE)) {
    fast <- policy_evaluation_fixture(tree_method = "fastpolicytree")
    expect_identical(fast$metadata$realised_engine, "fastpolicytree")
    expect_equal(fast$evaluation$actions, fit$evaluation$actions)
    expect_equal(fast$evaluation$value, fit$evaluation$value)
  } else {
    expect_error(policy_evaluation_fixture(tree_method = "fastpolicytree"), "no fallback")
  }
})

test_that("saved evaluation integrity binds the rule and numerical summaries", {
  skip_if_not_installed("policytree")
  fit <- policy_evaluation_fixture()
  expect_invisible(.policy_evaluation_validate(fit))
  changed <- fit
  changed$threshold$value <- .2
  expect_error(.policy_evaluation_validate(changed), "integrity")
  changed <- fit
  changed$evaluation$value$estimate <- 7
  expect_error(.policy_evaluation_validate(changed), "integrity")
  changed <- fit
  changed$tree$nodes[[1]]$split_value <- 100
  expect_error(.policy_evaluation_validate(changed), "identity")
})

test_that("depth-two objective matches exhaustive binary partitions", {
  skip_if_not_installed("policytree")
  X <- as.matrix(expand.grid(x = 0:1, y = 0:1))
  X <- X[rep(1:4, each = 3), , drop = FALSE]
  # neither marginal split captures the interaction, whereas depth two can.
  tau <- ifelse(X[, 1] == X[, 2], .09, .01)
  scores <- cbind(control = rep(0, nrow(X)), treated = tau)
  fit <- margot_policy_tree_evaluate(X, scores, X, scores, depth = 2)
  expect_equal(fit$threshold$value, .05)
  expect_equal(fit$evaluation$values$net_value[1], mean(pmax(tau - .05, 0)))
  expect_equal(fit$evaluation$actions, ifelse(tau > .05, 2L, 1L))
  stump <- margot_policy_tree_evaluate(X, scores, X, scores, depth = 1)
  expect_true(stump$metadata$constant_selected)
  expect_equal(stump$evaluation$value$estimate, 0)
})
