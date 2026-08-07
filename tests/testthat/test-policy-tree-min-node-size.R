# purpose: build the minimal saved-data object used by policy-tree interface tests.
# inputs: sample size and seed; output: one-model margot-compatible list.
make_policy_node_size_fixture <- function(n = 60L, seed = 817L) {
  set.seed(seed)
  covariates <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  dr_scores <- cbind(
    control = rep(0, n),
    treated = ifelse(covariates$x1 > 0, 1, -0.5)
  )
  list(
    results = list(
      model_y = list(
        dr_scores = dr_scores,
        top_vars = c("x1", "x2")
      )
    ),
    covariates = covariates,
    not_missing = seq_len(n),
    weights = rep(1, n),
    computation_params = list(
      grf_defaults = list(min.node.size = 50L)
    )
  )
}

test_that("explicit policy-tree node size overrides the compatibility option", {
  withr::local_options(list(margot.policy_tree.min_node_size = 3L))

  expect_equal(.resolve_policy_tree_min_node_size(17L), 17L)
  expect_equal(.resolve_policy_tree_min_node_size(NULL), 3L)
  expect_error(.resolve_policy_tree_min_node_size(1.5), "whole number")
  expect_error(.resolve_policy_tree_min_node_size(0L), ">= 1")
})

test_that("every public fitting route records the explicit terminal-node size", {
  object <- make_policy_node_size_fixture()

  tree <- margot_policy_tree(
    object,
    depth = 1L,
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  )
  expect_equal(tree$results$model_y$policy_tree_metadata$min_node_size, 7L)
  expect_equal(tree$results$model_y$policy_tree_metadata$requested_depths, 1L)
  expect_equal(tree$results$model_y$policy_tree_metadata$realised_depths, 1L)

  cv <- margot_policy_tree_cv(
    object,
    depths = 1L,
    num_folds = 3L,
    n_repeats = 1L,
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  )
  expect_equal(cv$metadata$min_node_size, 7L)
  expect_equal(cv$metadata$requested_depths, 1L)
  expect_equal(cv$metadata$realised_depths, 1L)

  stability <- margot_policy_tree_stability(
    object,
    depth = 1L,
    n_iterations = 2L,
    return_consensus_trees = FALSE,
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  )
  expect_equal(stability$metadata$min_node_size, 7L)
  expect_equal(stability$metadata$requested_depths, 1L)
  expect_equal(stability$metadata$realised_depths, 1L)

  bootstrap <- suppressWarnings(margot_policy_tree_bootstrap(
    object,
    depth = 1L,
    n_bootstrap = 2L,
    return_consensus_trees = FALSE,
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  ))
  expect_equal(bootstrap$metadata$min_node_size, 7L)

  diagnostic <- margot_policy_split_diagnostic(
    object,
    depths = 1L,
    n_splits = 2L,
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  )
  expect_equal(attr(diagnostic, "metadata")$min_node_size, 7L)
  expect_equal(attr(diagnostic, "metadata")$depths, 1L)

  recalculated <- margot_recalculate_policy_trees(
    object,
    outcomes_to_recalculate = "y",
    tree_method = "policytree",
    min_node_size = 7L,
    verbose = FALSE
  )
  expect_equal(
    recalculated$results$model_y$policy_tree_metadata$min_node_size,
    7L
  )
})

test_that("the terminal-node size reaches both policy-tree engines", {
  object <- make_policy_node_size_fixture()

  for (engine in c("policytree", "fastpolicytree")) {
    out <- margot_policy_tree_cv(
      object,
      depths = 1L,
      num_folds = 3L,
      n_repeats = 1L,
      tree_method = engine,
      min_node_size = 7L,
      verbose = FALSE
    )
    expect_equal(out$metadata$requested_tree_method, engine)
    expect_equal(out$metadata$tree_method, engine)
    expect_false(out$metadata$engine_fallback)
    expect_equal(out$metadata$min_node_size, 7L)
  }
})

test_that("engine fallback and forest node size cannot be confused", {
  object <- make_policy_node_size_fixture()
  withr::local_options(list(margot.policy_tree.min_node_size = NULL))

  testthat::local_mocked_bindings(
    .get_tree_method = function(tree_method, verbose) "policytree"
  )

  out <- margot_policy_tree_cv(
    object,
    depths = 1L,
    num_folds = 3L,
    n_repeats = 1L,
    tree_method = "fastpolicytree",
    verbose = FALSE
  )

  expect_equal(out$metadata$requested_tree_method, "fastpolicytree")
  expect_equal(out$metadata$tree_method, "policytree")
  expect_true(out$metadata$engine_fallback)
  expect_equal(out$metadata$min_node_size, 1L)
  expect_false(identical(
    out$metadata$min_node_size,
    object$computation_params$grf_defaults$min.node.size
  ))
})
