# purpose: reproduce the wide-covariate policy rule that automatic mode missed.
# inputs: row count and seed; output: covariates, weighted scores, and true cut.
make_wide_policy_recovery_fixture <- function(n = 2000L, seed = 42L) {
  set.seed(seed)
  age <- sample(18:95, n, replace = TRUE)
  covariates <- data.frame(age = age)

  for (index in seq_len(50L)) {
    covariates[[sprintf("binary_%02d", index)]] <- stats::rbinom(
      n,
      size = 1L,
      prob = stats::runif(1L, 0.05, 0.60)
    )
  }
  for (index in seq_len(30L)) {
    covariates[[sprintf("ordinal_%02d", index)]] <- sample(
      1:7,
      n,
      replace = TRUE
    )
  }
  for (index in seq_len(30L)) {
    covariates[[sprintf("continuous_%02d", index)]] <- stats::rnorm(n)
  }

  weights <- exp(stats::rnorm(n))
  weights <- pmin(weights, stats::quantile(weights, 0.95))
  age_cut <- stats::median(age)
  treated_score <- ifelse(age > age_cut, 0.5, -0.5)

  list(
    covariates = covariates,
    scores = cbind(control = 0, treated = treated_score) * weights,
    age_cut = age_cut
  )
}

# purpose: evaluate a fitted policy on its action-score objective.
# inputs: fitted tree, covariates, and scores; output: mean selected score.
policy_fixture_value <- function(tree, covariates, scores) {
  actions <- as.integer(stats::predict(tree, newdata = covariates))
  mean(scores[cbind(seq_len(nrow(scores)), actions)])
}

test_that("Margot's fast strategy recovers a wide deterministic policy rule", {
  skip_if_not_installed("fastpolicytree")
  fixture <- make_wide_policy_recovery_fixture()

  exact_tree <- suppressWarnings(.compute_policy_tree(
    fixture$covariates,
    fixture$scores,
    depth = 1L,
    tree_method = "policytree",
    min_node_size = 50L
  ))
  fast_tree <- .compute_policy_tree(
    fixture$covariates,
    fixture$scores,
    depth = 1L,
    tree_method = "fastpolicytree",
    min_node_size = 50L
  )

  exact_root <- exact_tree$columns[[exact_tree$nodes[[1L]]$split_variable]]
  fast_root <- fast_tree$columns[[fast_tree$nodes[[1L]]$split_variable]]
  exact_actions <- as.integer(stats::predict(
    exact_tree,
    newdata = fixture$covariates
  ))
  fast_actions <- as.integer(stats::predict(
    fast_tree,
    newdata = fixture$covariates
  ))

  expect_identical(.fastpolicytree_strategy_datatype(), 1L)
  expect_identical(
    attr(fast_tree, "margot_fastpolicytree_strategy_datatype"),
    1L
  )
  expect_true(is.na(attr(
    exact_tree,
    "margot_fastpolicytree_strategy_datatype"
  )))
  expect_identical(exact_root, "age")
  expect_identical(fast_root, "age")
  expect_equal(exact_tree$nodes[[1L]]$split_value, fixture$age_cut)
  expect_equal(fast_tree$nodes[[1L]]$split_value, fixture$age_cut)
  expect_identical(fast_actions, exact_actions)
  expect_equal(
    policy_fixture_value(fast_tree, fixture$covariates, fixture$scores),
    policy_fixture_value(exact_tree, fixture$covariates, fixture$scores)
  )
})

test_that("public cross-validation metadata records the realised strategy", {
  fixture <- make_wide_policy_recovery_fixture(n = 240L)
  object <- list(
    results = list(
      model_y = list(
        dr_scores = fixture$scores,
        top_vars = names(fixture$covariates)
      )
    ),
    covariates = fixture$covariates,
    not_missing = seq_len(nrow(fixture$covariates)),
    weights = rep(1, nrow(fixture$covariates))
  )

  fast_result <- margot_policy_tree_cv(
    object,
    depths = 1L,
    num_folds = 3L,
    n_repeats = 1L,
    tree_method = "fastpolicytree",
    min_node_size = 10L,
    verbose = FALSE
  )
  exact_result <- suppressWarnings(margot_policy_tree_cv(
    object,
    depths = 1L,
    num_folds = 3L,
    n_repeats = 1L,
    tree_method = "policytree",
    min_node_size = 10L,
    verbose = FALSE
  ))

  expect_identical(
    fast_result$metadata$fastpolicytree_strategy_datatype,
    1L
  )
  expect_true(is.na(
    exact_result$metadata$fastpolicytree_strategy_datatype
  ))
})
