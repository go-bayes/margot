# create independent synthetic partitions with overlapping treatment support.
policy_development_score_fixture <- function() {
  set.seed(8711)
  development_X <- matrix(stats::rnorm(180 * 3), ncol = 3,
                          dimnames = list(NULL, c("age", "load", "support")))
  evaluation_X <- matrix(stats::rnorm(60 * 3), ncol = 3,
                         dimnames = list(NULL, colnames(development_X)))
  development_W <- rep(0:1, 90)
  evaluation_W <- rep(0:1, 30)
  list(development_X = development_X,
       development_Y = development_X[, 1] + development_W * (.4 + .5 * development_X[, 2]) + stats::rnorm(180),
       development_W = development_W, evaluation_X = evaluation_X,
       evaluation_Y = evaluation_X[, 1] + evaluation_W * (.4 + .5 * evaluation_X[, 2]) + stats::rnorm(60),
       evaluation_W = evaluation_W, development_weights = exp(.2 * development_X[, 3]),
       forest_args = list(num.trees = 100, min.node.size = 5, mtry = 2, honesty = TRUE),
       seed = 144L, num_threads = 1L, save_models = TRUE)
}

test_that("evaluation responses and features cannot affect development learning", {
  skip_if_not_installed("grf")
  args <- policy_development_score_fixture()
  original <- do.call(margot_policy_development_scores, args)
  reference <- margot_policy_value_threshold(original$development_scores, args$development_weights, "ate")
  for (change in c("outcomes", "exposures", "features")) {
    changed_args <- args
    if (change == "outcomes") changed_args$evaluation_Y <- 1000 - args$evaluation_Y
    if (change == "exposures") changed_args$evaluation_W <- 1 - args$evaluation_W
    if (change == "features") changed_args$evaluation_X <- args$evaluation_X * 3 + 1
    changed <- do.call(margot_policy_development_scores, changed_args)
    expect_identical(changed$development_scores, original$development_scores, info = change)
    expect_identical(changed$development_predictions, original$development_predictions, info = change)
    expect_identical(changed$models, original$models, info = change)
    expect_identical(margot_policy_value_threshold(changed$development_scores, args$development_weights, "ate"), reference, info = change)
    expect_false(identical(changed$evaluation_scores, original$evaluation_scores), info = change)
    if (change != "features") {
      expect_identical(changed$evaluation_predictions, original$evaluation_predictions, info = change)
    }
  }
})

test_that("development scores use OOB predictions and replicate seeded forests", {
  skip_if_not_installed("grf")
  skip_if_not_installed("policytree")
  args <- policy_development_score_fixture()
  random_state <- .Random.seed
  first <- do.call(margot_policy_development_scores, args)
  expect_identical(.Random.seed, random_state)
  second <- do.call(margot_policy_development_scores, args)
  expect_identical(first, second)
  expect_identical(first$development_predictions$outcome_mean,
                   as.numeric(stats::predict(first$models$outcome)$predictions))
  expect_identical(first$development_predictions$propensity,
                   as.numeric(stats::predict(first$models$exposure)$predictions))
  expect_identical(first$development_predictions$treatment_effect,
                   as.numeric(stats::predict(first$models$causal)$predictions))
  expect_equal(unname(first$development_scores),
               unname(policytree::double_robust_scores(first$models$causal)), tolerance = 1e-12)
  expect_identical(first$evaluation_predictions$outcome_mean,
                   as.numeric(stats::predict(first$models$outcome, args$evaluation_X)$predictions))
  expect_identical(first$evaluation_predictions$propensity,
                   as.numeric(stats::predict(first$models$exposure, args$evaluation_X)$predictions))
  expect_identical(first$evaluation_predictions$treatment_effect,
                   as.numeric(stats::predict(first$models$causal, args$evaluation_X)$predictions))
  pred <- first$evaluation_predictions
  mu0 <- pred$outcome_mean - pred$propensity * pred$treatment_effect
  mu1 <- pred$outcome_mean + (1 - pred$propensity) * pred$treatment_effect
  expected <- cbind(control = mu0 + (1 - args$evaluation_W) * (args$evaluation_Y - mu0) / (1 - pred$propensity),
                    treated = mu1 + args$evaluation_W * (args$evaluation_Y - mu1) / pred$propensity)
  expect_equal(first$evaluation_scores, expected, tolerance = 1e-12)
  expect_identical(first$metadata$forest_seeds, c(outcome = 144L, exposure = 145L, causal = 146L))
  expect_identical(first$metadata$forest_args, args$forest_args)
  expect_identical(first$metadata$nuisance_args, args$forest_args)
  expect_identical(first$metadata$num_threads, 1L)
  expect_false(first$metadata$evaluation_outcomes_used_for_learning)
  expect_match(first$metadata$qualification, "conditional on supplied preparation and weights")
  args$save_models <- FALSE
  compact <- do.call(margot_policy_development_scores, args)
  expect_null(compact$models)
  first$models <- NULL
  expect_identical(compact, first)
})

test_that("forest settings cannot override the declared development boundary", {
  skip_if_not_installed("grf")
  args <- policy_development_score_fixture()
  for (name in c("X", "Y", "W", "Y.hat", "W.hat", "sample.weights", "seed", "num.threads", "clusters", "equalize.cluster.weights")) {
    injected <- args
    injected$forest_args[[name]] <- 1
    expect_error(do.call(margot_policy_development_scores, injected), "forest_args", info = name)
  }
  for (invalid in list(list(100), list(unknown_option = 1), stats::setNames(list(100, 200), c("num.trees", "num.trees")),
                       stats::setNames(list(100), NA_character_), stats::setNames(list(100), ""))) {
    args$forest_args <- invalid
    expect_error(do.call(margot_policy_development_scores, args), "forest_args")
  }
})

test_that("misaligned and unsupported partitions fail before forest fitting", {
  skip_if_not_installed("grf")
  args <- policy_development_score_fixture()
  for (change in c("column_order", "duplicate_columns", "nonfinite_X", "matrix_Y", "short_W", "missing_W", "constant_W", "zero_weight", "short_weight", "seed_overflow", "fractional_threads")) {
    bad <- args
    if (change == "column_order") bad$evaluation_X <- bad$evaluation_X[, 3:1]
    if (change == "duplicate_columns") colnames(bad$development_X) <- c("x", "x", "y")
    if (change == "nonfinite_X") bad$evaluation_X[1, 1] <- Inf
    if (change == "matrix_Y") bad$development_Y <- matrix(bad$development_Y)
    if (change == "short_W") bad$evaluation_W <- bad$evaluation_W[-1]
    if (change == "missing_W") bad$evaluation_W[1] <- NA_real_
    if (change == "constant_W") bad$development_W[] <- 0
    if (change == "zero_weight") bad$development_weights[1] <- 0
    if (change == "short_weight") bad$development_weights <- bad$development_weights[-1]
    if (change == "seed_overflow") bad$seed <- .Machine$integer.max
    if (change == "fractional_threads") bad$num_threads <- 1.5
    expect_error(do.call(margot_policy_development_scores, bad), info = change)
  }
})

test_that("feature names weight shape and native threads are validated explicitly", {
  skip_if_not_installed("grf")
  args <- policy_development_score_fixture()
  for (invalid_name in c(NA_character_, "")) {
    bad <- args
    colnames(bad$development_X)[1] <- colnames(bad$evaluation_X)[1] <- invalid_name
    expect_error(do.call(margot_policy_development_scores, bad), "Covariates")
  }
  bad <- args
  bad$development_weights <- matrix(bad$development_weights, ncol = 3)
  expect_error(do.call(margot_policy_development_scores, bad), "development_weights")
  bad <- args
  bad$num_threads <- 3e9
  expect_error(do.call(margot_policy_development_scores, bad), "num_threads")
})

test_that("independent nuisance scores reach fixed-rule evaluation without feedback", {
  skip_if_not_installed("grf")
  skip_if_not_installed("policytree")
  args <- policy_development_score_fixture()
  original <- do.call(margot_policy_development_scores, args)
  fit <- function(scores) {
    margot_policy_tree_evaluate(args$development_X, scores$development_scores,
      args$evaluation_X, scores$evaluation_scores,
      development_weights = args$development_weights,
      development_ids = paste0("dev", seq_len(nrow(args$development_X))),
      evaluation_ids = paste0("eval", seq_len(nrow(args$evaluation_X))),
      depth = 1, min_node_size = 15, tree_method = "policytree")
  }
  first <- fit(original)
  args$evaluation_Y <- args$evaluation_Y + 100 * args$evaluation_W
  changed <- do.call(margot_policy_development_scores, args)
  second <- fit(changed)
  expect_identical(first$rule_id, second$rule_id)
  expect_identical(first$tree, second$tree)
  expect_identical(first$threshold, second$threshold)
  expect_identical(first$development, second$development)
  expect_identical(first$constant, second$constant)
  expect_false(identical(first$evaluation, second$evaluation))
})
