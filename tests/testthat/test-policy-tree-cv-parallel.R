policy_cv_parallel_fixture <- function(n = 90L) {
  set.seed(20260831)
  covariates <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )
  make_scores <- function(offset) {
    cbind(
      control = 0.1 * covariates$x3,
      treated = ifelse(covariates$x1 + offset * covariates$x2 > 0, 0.8, -0.3)
    )
  }
  list(
    results = list(
      model_y1 = list(dr_scores = make_scores(0.1), top_vars = names(covariates)),
      model_y2 = list(dr_scores = make_scores(0.4), top_vars = names(covariates)),
      model_y3 = list(dr_scores = make_scores(0.8), top_vars = names(covariates))
    ),
    covariates = covariates,
    weights = seq(0.5, 1.5, length.out = n)
  )
}

policy_cv_parallel_args <- function(object, tree_method) {
  list(
    model_results = object,
    depths = c(1L, 2L),
    num_folds = 3L,
    n_repeats = 2L,
    tree_method = tree_method,
    min_node_size = 4L,
    seed = 810L,
    verbose = FALSE
  )
}

test_that("outcome-parallel fastpolicytree CV preserves the registered engine contract", {
  object <- policy_cv_parallel_fixture()
  output_fields <- c(
    "fold_values", "value_summary", "split_values", "split_summary",
    "leaf_values", "leaf_summary", "threshold_summary", "depth_selection",
    "policy_selection", "depth_map"
  )

  arguments <- policy_cv_parallel_args(object, "fastpolicytree")
  set.seed(314159)
  initial_random_seed <- .Random.seed
  serial <- do.call(margot_policy_tree_cv, arguments)
  serial_random_seed <- .Random.seed
  assign(".Random.seed", initial_random_seed, envir = .GlobalEnv)
  concurrent <- do.call(
    margot_policy_tree_cv,
    c(arguments, list(parallel = TRUE, n_workers = 2L))
  )

  expect_identical(concurrent[output_fields], serial[output_fields])
  expect_identical(.Random.seed, serial_random_seed)
  expect_false(serial$metadata$parallel)
  expect_identical(serial$metadata$n_workers, 1L)
  expect_true(concurrent$metadata$parallel)
  expect_identical(concurrent$metadata$n_workers, 2L)
  expect_identical(concurrent$metadata$seed, serial$metadata$seed)
  expect_identical(concurrent$metadata$num_folds, serial$metadata$num_folds)
  expect_identical(concurrent$metadata$n_repeats, serial$metadata$n_repeats)
  expect_identical(concurrent$metadata$requested_tree_method, "fastpolicytree")
  expect_identical(concurrent$metadata$tree_method, "fastpolicytree")
  expect_false(concurrent$metadata$engine_fallback)
  expect_identical(concurrent$metadata$fastpolicytree_strategy_datatype, 1L)
})

test_that("future workers export compact policy inputs rather than full model objects", {
  object <- policy_cv_parallel_fixture(60L)
  sentinel_bytes <- 12L * 1024L^2
  object$results$model_y1$full_forest_sentinel <- raw(sentinel_bytes)
  job <- .policy_cv_prepare_model_job(
    model_name = "model_y1",
    model_results = object,
    weights = NULL,
    custom_covariates = NULL,
    exclude_covariates = NULL,
    covariate_mode = "original",
    num_folds = 3L,
    n_repeats = 1L,
    seed = 810L,
    verbose = FALSE
  )
  worker_config <- list(
    depths = 1L,
    num_folds = 3L,
    n_repeats = 1L,
    actual_tree_method = "fastpolicytree",
    min_node_size = 4L,
    label_mapping = NULL
  )
  full_serialized_bytes <- length(serialize(object, NULL, version = 2L))
  worker_serialized_bytes <- length(serialize(
    list(job = job, worker_config = worker_config, worker = .policy_cv_future_worker),
    NULL,
    version = 2L
  ))

  expect_gte(full_serialized_bytes, sentinel_bytes)
  expect_lt(worker_serialized_bytes, 1024L^2)
  expect_gt(full_serialized_bytes, 10 * worker_serialized_bytes)
  expect_false("full_forest_sentinel" %in% names(job))
  expect_false("full_forest_sentinel" %in% names(job$model_data))

  arguments <- policy_cv_parallel_args(object, "fastpolicytree")
  arguments$depths <- 1L
  arguments$n_repeats <- 1L
  expect_no_error(do.call(
    margot_policy_tree_cv,
    c(
      arguments,
      list(
        parallel = TRUE,
        n_workers = 2L,
        future_globals_max_size = 1024L^2
      )
    )
  ))
})

test_that("policytree remains a serial-compatible secondary engine", {
  object <- policy_cv_parallel_fixture(60L)
  arguments <- policy_cv_parallel_args(object, "policytree")
  arguments$depths <- 1L
  arguments$n_repeats <- 1L
  serial <- do.call(margot_policy_tree_cv, arguments)
  concurrent <- do.call(
    margot_policy_tree_cv,
    c(arguments, list(parallel = TRUE, n_workers = 2L))
  )

  output_fields <- setdiff(names(serial), "metadata")
  expect_identical(concurrent[output_fields], serial[output_fields])
  expect_identical(concurrent$metadata$tree_method, "policytree")
  expect_false(concurrent$metadata$engine_fallback)
})

test_that("policy CV restores the caller future plan, globals limit, and thread settings", {
  old_plan <- future::plan("list")
  old_size <- getOption("future.globals.maxSize")
  old_omp <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  on.exit({
    future::plan(old_plan, substitute = FALSE)
    options(future.globals.maxSize = old_size)
    if (is.na(old_omp)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_omp)
  }, add = TRUE)

  future::plan(future::sequential)
  options(future.globals.maxSize = 123456789)
  Sys.setenv(OMP_NUM_THREADS = "7")
  object <- policy_cv_parallel_fixture(60L)
  arguments <- policy_cv_parallel_args(object, "fastpolicytree")
  concurrent <- do.call(
    margot_policy_tree_cv,
    c(arguments, list(parallel = TRUE, n_workers = 2L, future_globals_max_size = 1e9))
  )

  expect_true(inherits(future::plan("list")[[1L]], "sequential"))
  expect_identical(getOption("future.globals.maxSize"), 123456789)
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "7")
  expect_identical(concurrent$metadata$n_workers, 2L)
})

test_that("policy CV uses a conservative default and validates worker controls", {
  expect_identical(.policy_cv_resolve_workers(TRUE, NULL, 8L), 2L)
  expect_identical(.policy_cv_resolve_workers(TRUE, 20L, 3L), 3L)
  expect_identical(.policy_cv_resolve_workers(TRUE, 4L, 1L), 1L)
  expect_identical(.policy_cv_resolve_workers(FALSE, 4L, 8L), 1L)

  object <- policy_cv_parallel_fixture(30L)
  expect_error(margot_policy_tree_cv(object, parallel = NA), "parallel must be TRUE or FALSE")
  expect_error(margot_policy_tree_cv(object, parallel = TRUE, n_workers = 0), "positive whole number")
  expect_error(margot_policy_tree_cv(object, future_globals_max_size = -1), "positive number of bytes")
})
