# Task-parallel scheduling of the shared density-ratio path. The deterministic
# fixtures come from helper-lmtp-shared.R, so every comparison below runs the
# same frame, shift, and settings as the sequential shared-ratio tests.

# run one scheduled call quietly. A source-loaded margot makes `future` warn that
# the attached package may not be available on a worker; workers load the
# installed margot, and the Stage 0 eligibility probe already refuses one whose
# source fingerprint differs, so that development-time warning is muffled here.
run_task_parallel <- function(expr) {
  withCallingHandlers(
    base::suppressMessages(expr),
    warning = function(w) {
      if (grepl("may not be available when loading", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# supply the common margot_lmtp() arguments used by both scheduling modes
task_parallel_arguments <- function(data,
                                    defaults = shared_ratio_defaults(),
                                    n_cores = 2L) {
  list(
    data = data,
    outcome_vars = c("perfectionism", "distress"),
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = "null",
    contrast_scale = "additive",
    lmtp_defaults = defaults,
    n_cores = n_cores,
    progress = "none",
    seed = 3031L,
    reuse_density_ratios = TRUE
  )
}

# compare every policy-specific model of two runs on its numerical properties
expect_shared_agreement <- function(task, reference) {
  task_models <- unlist(task$models, recursive = FALSE)
  reference_models <- unlist(reference$models, recursive = FALSE)
  expect_identical(names(task_models), names(reference_models))
  for (model_name in names(reference_models)) {
    expect_identical(
      shared_ratio_numerics(task_models[[model_name]]),
      shared_ratio_numerics(reference_models[[model_name]])
    )
    for (attribute in c("margot_density_ratio_source", "margot_density_ratio_reused",
                        "margot_density_ratio_fit_id")) {
      expect_identical(
        attr(task_models[[model_name]], attribute, exact = TRUE),
        attr(reference_models[[model_name]], attribute, exact = TRUE)
      )
    }
  }
  expect_identical(task$combined_tables, reference$combined_tables)
  expect_identical(task$individual_tables, reference$individual_tables)
  expect_identical(task$contrasts, reference$contrasts)
  invisible(task)
}

test_that("task-parallel scheduling reproduces the sequential shared route exactly", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- make_shared_ratio_data()
  arguments <- task_parallel_arguments(data)

  future::plan(future::sequential)
  reference <- run_task_parallel(do.call(margot_lmtp, arguments))

  future::plan(future::multisession, workers = 2L)
  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(arguments, list(manage_future_plan = TRUE))
  ))

  expect_identical(
    names(task),
    c("models", "contrasts", "individual_tables", "combined_tables")
  )
  expect_shared_agreement(task, reference)
  reuse <- attr(task, "margot_density_ratio_reuse")
  expect_true(reuse$enabled)
  expect_identical(reuse$scheduler, "task")
  expect_identical(reuse$ratio_fit_count, 2L)
  expect_identical(reuse$legacy_ratio_fit_count, 4L)
  expect_identical(
    reuse$ratio_fit_count,
    attr(reference, "margot_density_ratio_reuse")$ratio_fit_count
  )

  # the worker count varies; task identities and result fingerprints do not
  future::plan(future::sequential)
  single_worker <- run_task_parallel(do.call(
    margot_lmtp,
    c(task_parallel_arguments(data, n_cores = 1L), list(manage_future_plan = TRUE))
  ))
  fingerprints <- function(output) {
    lapply(
      attr(output, "margot_density_ratio_reuse")$density_records,
      function(record) record[c("identity", "result_fingerprint", "folds_fingerprint")]
    )
  }
  expect_identical(fingerprints(single_worker), fingerprints(task))
  expect_shared_agreement(single_worker, reference)
})

test_that("a localhost cluster plan schedules the same shared results", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- make_shared_ratio_data(n = 100L, seed = 67L)
  arguments <- task_parallel_arguments(data)

  future::plan(future::sequential)
  reference <- run_task_parallel(do.call(margot_lmtp, arguments))

  workers <- parallelly::makeClusterPSOCK(2L)
  on.exit(try(parallel::stopCluster(workers), silent = TRUE), add = TRUE)
  future::plan(future::cluster, workers = workers)
  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(arguments, list(manage_future_plan = TRUE))
  ))

  expect_shared_agreement(task, reference)
  # a caller-supplied pool is scheduled over and never replaced
  expect_true(inherits(future::plan("list")[[1L]], "cluster"))
  expect_equal(future::nbrOfWorkers(), 2L)
})

test_that("the caller's plan survives task-parallel success and failure", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- make_shared_ratio_data(n = 80L, seed = 23L)
  arguments <- task_parallel_arguments(data)

  future::plan(future::sequential)
  run_task_parallel(do.call(margot_lmtp, c(arguments, list(manage_future_plan = TRUE))))
  expect_true(inherits(future::plan("list")[[1L]], "sequential"))
  expect_equal(future::nbrOfWorkers(), 1L)

  failing_defaults <- shared_ratio_defaults()
  failing_defaults$learners_outcome <- "SL.absent_learner"
  failing <- task_parallel_arguments(data, defaults = failing_defaults)
  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(failing, list(manage_future_plan = TRUE)))),
    class = "margot_error_task_worker_failure"
  )
  expect_true(inherits(future::plan("list")[[1L]], "sequential"))
  expect_equal(future::nbrOfWorkers(), 1L)
})

test_that("the managed future plan branch restores a stacked plan and its options", {
  old_plan <- future::plan("list")
  old_options <- list(
    mc.cores = getOption("mc.cores"),
    parallelly.maxWorkers.localhost = getOption("parallelly.maxWorkers.localhost")
  )
  on.exit(
    {
      future::plan(old_plan, substitute = FALSE)
      options(old_options)
    },
    add = TRUE
  )
  data <- make_shared_ratio_data(n = 60L, seed = 29L)
  options(mc.cores = 3L, parallelly.maxWorkers.localhost = 3L)
  future::plan(list(
    future::tweak(future::multisession, workers = 2L),
    future::tweak(future::multisession, workers = I(1L))
  ))
  caller_plan <- future::plan("list")
  legacy_call <- function(defaults) {
    margot_lmtp(
      data = data,
      outcome_vars = c("perfectionism", "distress"),
      trt = "exposure",
      shift_functions = list(down = shared_ratio_shift_down),
      include_null_shift = FALSE,
      lmtp_model_type = lmtp::lmtp_sdr,
      contrast_type = "pairwise",
      lmtp_defaults = defaults,
      n_cores = 2L,
      cv_workers = 1L,
      progress = "none",
      seed = 3031L,
      reuse_density_ratios = FALSE,
      manage_future_plan = TRUE
    )
  }

  run_task_parallel(legacy_call(shared_ratio_defaults()))
  # before the ordering fix this branch left the caller on a sequential plan, and
  # it never restored the two options it overwrites
  expect_length(future::plan("list"), length(caller_plan))
  expect_true(inherits(future::plan("list")[[1L]], "cluster"))
  expect_equal(future::nbrOfWorkers(), 2L)
  expect_identical(getOption("mc.cores"), 3L)
  expect_identical(getOption("parallelly.maxWorkers.localhost"), 3L)

  local({
    # fail after the branch has taken the plan and the options
    testthat::local_mocked_bindings(
      margot_lmtp_finalize_outputs = function(...) stop("induced assembly failure"),
      .package = "margot"
    )
    expect_error(
      run_task_parallel(legacy_call(shared_ratio_defaults())),
      "induced assembly failure"
    )
  })
  expect_length(future::plan("list"), length(caller_plan))
  expect_true(inherits(future::plan("list")[[1L]], "cluster"))
  expect_identical(getOption("mc.cores"), 3L)
  expect_identical(getOption("parallelly.maxWorkers.localhost"), 3L)
})

test_that("task-parallel scheduling records one density fit per policy identity", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 80L, seed = 31L)
  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(task_parallel_arguments(data), list(manage_future_plan = TRUE))
  ))

  records <- attr(task, "margot_density_ratio_reuse")$density_records
  expect_identical(names(records), c("down", "null"))
  expect_true(all(vapply(records, function(record) isTRUE(record$success), logical(1))))
  identities <- vapply(records, function(record) record$identity, character(1))
  expect_identical(length(unique(identities)), 2L)
  expect_identical(
    sum(vapply(records, function(record) isTRUE(record$success), logical(1))),
    2L
  )
  expect_true(all(vapply(records, function(record) nzchar(record$result_fingerprint), logical(1))))
  fit_ids <- vapply(
    unlist(task$models, recursive = FALSE),
    function(model) attr(model, "margot_density_ratio_fit_id", exact = TRUE),
    character(1)
  )
  expect_identical(sort(unique(unname(fit_ids))), c("down", "null"))
})

test_that("a terminal-outcome change leaves stage one results and fingerprints unchanged", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 80L, seed = 37L)
  altered <- data
  altered$distress <- rev(altered$distress) + 10

  original <- run_task_parallel(do.call(
    margot_lmtp,
    c(task_parallel_arguments(data), list(manage_future_plan = TRUE))
  ))
  changed <- run_task_parallel(do.call(
    margot_lmtp,
    c(task_parallel_arguments(altered), list(manage_future_plan = TRUE))
  ))

  original_records <- attr(original, "margot_density_ratio_reuse")$density_records
  changed_records <- attr(changed, "margot_density_ratio_reuse")$density_records
  for (policy in names(original_records)) {
    expect_identical(changed_records[[policy]]$identity, original_records[[policy]]$identity)
    expect_identical(
      changed_records[[policy]]$result_fingerprint,
      original_records[[policy]]$result_fingerprint
    )
    expect_identical(
      changed_records[[policy]]$folds_fingerprint,
      original_records[[policy]]$folds_fingerprint
    )
  }
  original_models <- unlist(original$models, recursive = FALSE)
  changed_models <- unlist(changed$models, recursive = FALSE)
  for (model_name in names(original_models)) {
    expect_identical(
      changed_models[[model_name]]$density_ratios,
      original_models[[model_name]]$density_ratios
    )
  }
})

test_that("a fold-map mismatch refuses stage two", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 43L)
  honest_stage <- margot_lmtp_density_stage
  testthat::local_mocked_bindings(
    margot_lmtp_density_stage = function(payload) {
      record <- honest_stage(payload)
      # a fold map that no longer matches the density fit must refuse the outcome stage
      record$folds <- rev(record$folds)
      record
    },
    .package = "margot"
  )

  expect_error(
    run_task_parallel(do.call(
      margot_lmtp,
      c(task_parallel_arguments(data, n_cores = 1L), list(manage_future_plan = TRUE))
    )),
    class = "margot_error_density_ratio_identity"
  )
})

test_that("an induced worker failure fails closed with an inspectable record", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 47L)
  defaults <- shared_ratio_defaults()
  defaults$learners_outcome <- "SL.absent_learner"

  condition <- tryCatch(
    run_task_parallel(do.call(
      margot_lmtp,
      c(task_parallel_arguments(data, defaults = defaults), list(manage_future_plan = TRUE))
    )),
    error = function(e) e
  )

  expect_s3_class(condition, "margot_error_task_worker_failure")
  expect_identical(condition$margot_task_stage, "outcome")
  records <- condition$margot_task_records
  expect_true(length(records) >= 2L)
  failed <- Filter(function(record) isFALSE(record$success), records)
  expect_length(failed, 1L)
  expect_identical(failed[[1L]]$stage, "outcome")
  expect_true(nzchar(failed[[1L]]$error))
  # the ledger carries the successful density tasks, not the failure alone
  succeeded_density <- Filter(
    function(record) isTRUE(record$success) && identical(record$stage, "density"),
    records
  )
  expect_length(succeeded_density, 2L)
  expect_false(is.list(condition$models))
})

test_that("task-parallel scheduling refuses an absent seed", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 11L)
  arguments <- task_parallel_arguments(data, n_cores = 1L)
  arguments$seed <- NULL

  expect_error(
    run_task_parallel(do.call(
      margot_lmtp,
      c(arguments, list(manage_future_plan = TRUE))
    )),
    class = "margot_error_task_seed_required"
  )
})

test_that("task-parallel scheduling leaves a reproducible caller random-number state", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 13L)
  arguments <- c(
    task_parallel_arguments(data, n_cores = 1L),
    list(manage_future_plan = TRUE)
  )

  set.seed(99L)
  run_task_parallel(do.call(margot_lmtp, arguments))
  first_state <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(99L)
  run_task_parallel(do.call(margot_lmtp, arguments))
  second_state <- get(".Random.seed", envir = .GlobalEnv)

  # the mode determines the caller's post-call state from the supplied seed; it
  # does not preserve the state at entry, exactly as the sequential route does
  expect_identical(second_state, first_state)
})

test_that("the outcome worker restores the random-number state immediately before fitting", {
  statements <- as.list(body(margot_lmtp_outcome_stage))
  calls_to <- function(expression, name) {
    if (!is.call(expression)) {
      return(FALSE)
    }
    any(vapply(
      as.list(expression),
      function(part) identical(part, as.name(name)) || calls_to(part, name),
      logical(1)
    ))
  }
  fitting <- which(vapply(statements, calls_to, logical(1), name = "margot_lmtp_fit_sdr_outcome"))
  expect_length(fitting, 1L)
  # nothing may execute between state restoration and the outcome regression
  expect_true(calls_to(statements[[fitting - 1L]], "margot_lmtp_restore_seed"))
})

test_that("checkpoint writes are atomic, verified, and safely named", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "example.rds")
  payload <- list(a = 1L, b = "two")

  expect_identical(margot_lmtp_write_rds_atomic(payload, path), path)
  # a duplicate completion with identical content reuses the installed file
  expect_identical(margot_lmtp_write_rds_atomic(payload, path), path)
  expect_identical(readRDS(path), payload)
  expect_error(
    margot_lmtp_write_rds_atomic(list(a = 2L, b = "two"), path),
    class = "margot_error_checkpoint_conflict"
  )
  # a duplicate completion that differs only outside the compared fields reuses
  stamped <- file.path(directory, "stamped.rds")
  first <- list(result = "same", timestamp = Sys.time())
  margot_lmtp_write_rds_atomic(first, stamped, compare = "result")
  expect_identical(
    margot_lmtp_write_rds_atomic(
      list(result = "same", timestamp = Sys.time()), stamped, compare = "result"
    ),
    stamped
  )
  expect_identical(readRDS(stamped)$timestamp, first$timestamp)
  expect_error(
    margot_lmtp_write_rds_atomic(
      list(result = "other", timestamp = Sys.time()), stamped, compare = "result"
    ),
    class = "margot_error_checkpoint_conflict"
  )
  # no scratch file survives either the successful or the refused write
  expect_length(list.files(directory, pattern = "^margot-checkpoint-"), 0L)

  # a clean name keeps its filename; an altered name carries a hash of the
  # original, so two names that sanitise alike cannot claim one path
  expect_identical(margot_lmtp_safe_path_name("down"), "down")
  expect_true(startsWith(margot_lmtp_safe_path_name("../a/b c"), ".._a_b_c-"))
  expect_false(identical(
    margot_lmtp_safe_path_name("a/b"),
    margot_lmtp_safe_path_name("a b")
  ))
  expect_false(identical(
    margot_lmtp_safe_path_name("a?b"),
    margot_lmtp_safe_path_name("a b")
  ))
})

test_that("the outcome payload carries no copy of the analysis data", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 41L)
  observed <- new.env(parent = emptyenv())
  observed$payloads <- list()
  honest_stage <- margot_lmtp_outcome_stage
  testthat::local_mocked_bindings(
    margot_lmtp_outcome_stage = function(payload) {
      observed$payloads[[length(observed$payloads) + 1L]] <- names(payload)
      observed$frames <- c(
        observed$frames,
        sum(vapply(
          payload,
          function(element) is.data.frame(element) && identical(dim(element), dim(data)),
          logical(1)
        ))
      )
      honest_stage(payload)
    },
    .package = "margot"
  )

  run_task_parallel(do.call(
    margot_lmtp,
    c(task_parallel_arguments(data, n_cores = 1L), list(manage_future_plan = TRUE))
  ))

  expect_length(observed$payloads, 4L)
  for (payload_names in observed$payloads) {
    # the coordinator builds and verifies the outcome task, so no worker needs
    # the estimator arguments, the analysis frame, or the source task
    expect_false("args" %in% payload_names)
    expect_false("data" %in% payload_names)
    expect_false("source_task" %in% payload_names)
  }
  expect_true(all(observed$frames == 0L))
})

test_that("a shift closure's captured values and the generator enter the identity", {
  data <- make_shared_ratio_data(n = 40L, seed = 17L)
  make_shift <- function(delta) {
    function(data, trt) data[[trt]] - delta
  }
  arguments_for <- function(shift) {
    margot_lmtp_task_resolve_args(c(
      list(
        data = data, trt = "exposure",
        outcomes = c("perfectionism", "distress"), shift = shift
      ),
      shared_ratio_defaults()
    ))
  }
  identity_for <- function(shift, kind = NULL) {
    args <- arguments_for(shift)
    previous <- RNGkind()
    on.exit(RNGkind(previous[1L], previous[2L], previous[3L]), add = TRUE)
    if (!is.null(kind)) {
      RNGkind(kind)
    }
    set.seed(3031L)
    margot_lmtp_density_identity(
      args = args,
      shift_name = "down",
      shift_label = paste(deparse(shift), collapse = " "),
      realised_shift = margot_lmtp_realise_shift(args, "down")$values,
      seed_state = get(".Random.seed", envir = .GlobalEnv),
      rng_kind = RNGkind(),
      code_fingerprint = "constant"
    )
  }

  shallow <- make_shift(0.1)
  deep <- make_shift(0.2)
  # the two closures deparse identically and differ only in a captured value
  expect_identical(paste(deparse(shallow), collapse = " "), paste(deparse(deep), collapse = " "))
  expect_false(identical(identity_for(shallow), identity_for(deep)))
  expect_identical(identity_for(shallow), identity_for(make_shift(0.1)))
  # one integer seed under a different generator is a different starting state
  expect_false(identical(
    identity_for(shallow),
    identity_for(shallow, kind = "L'Ecuyer-CMRG")
  ))
})

test_that("each policy's shift closure runs exactly once for the whole call", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 79L)
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  counted_shift <- function(data, trt) {
    calls$count <- calls$count + 1L
    data[[trt]] - 0.15
  }
  arguments <- task_parallel_arguments(data, n_cores = 1L)
  arguments$shift_functions <- list(down = counted_shift)

  run_task_parallel(do.call(
    margot_lmtp, c(arguments, list(manage_future_plan = TRUE))
  ))

  # preflight realises the policy once and every task reuses those values, so
  # neither the density worker nor either outcome task calls the closure again
  expect_identical(calls$count, 1L)
})

test_that("exit handlers run in the order the scheduler depends on", {
  # the scheduler registers its drain before the random-number restoration and
  # before an owned pool's plan restoration, and relies on them running in that
  # order under add = TRUE, after = TRUE
  order <- character()
  handled <- function() {
    on.exit(order <<- c(order, "drain"), add = TRUE, after = TRUE)
    on.exit(order <<- c(order, "rng"), add = TRUE, after = TRUE)
    on.exit(order <<- c(order, "plan"), add = TRUE, after = TRUE)
    stop("induced failure")
  }
  expect_error(handled(), "induced failure")
  expect_identical(order, c("drain", "rng", "plan"))
})

test_that("a stochastic or failing policy refuses task-parallel scheduling", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 73L)
  arguments <- task_parallel_arguments(data, n_cores = 1L)

  stochastic <- arguments
  stochastic$shift_functions <- list(
    jittered = function(data, trt) data[[trt]] - 0.15 + stats::rnorm(nrow(data), 0, 0.01)
  )
  expect_error(
    run_task_parallel(do.call(
      margot_lmtp, c(stochastic, list(manage_future_plan = TRUE))
    )),
    class = "margot_error_stochastic_shift_unsupported"
  )

  failing <- arguments
  failing$shift_functions <- list(
    broken = function(data, trt) stop("the policy cannot be applied")
  )
  # a policy that cannot be realised fails preflight; it never becomes an identity
  expect_error(
    run_task_parallel(do.call(
      margot_lmtp, c(failing, list(manage_future_plan = TRUE))
    )),
    class = "margot_error_shift_realisation_failed"
  )
})

test_that("a tampered density payload refuses the outcome stage", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 19L)
  arguments <- c(
    task_parallel_arguments(data, n_cores = 1L),
    list(manage_future_plan = TRUE)
  )
  honest_stage <- margot_lmtp_density_stage

  local({
    testthat::local_mocked_bindings(
      margot_lmtp_density_stage = function(payload) {
        record <- honest_stage(payload)
        record$source_task$id <- rev(record$source_task$id)
        record
      },
      .package = "margot"
    )
    expect_error(
      run_task_parallel(do.call(margot_lmtp, arguments)),
      class = "margot_error_density_ratio_identity"
    )
  })

  local({
    testthat::local_mocked_bindings(
      margot_lmtp_density_stage = function(payload) {
        record <- honest_stage(payload)
        record$source_task$weights <- rep(0.5, length(record$source_task$id))
        record
      },
      .package = "margot"
    )
    expect_error(
      run_task_parallel(do.call(margot_lmtp, arguments)),
      class = "margot_error_density_ratio_identity"
    )
  })
})

test_that("policy names carrying a delimiter reach their own grid cells", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 23L)

  task <- run_task_parallel(margot_lmtp(
    data = data,
    outcome_vars = c("perfectionism", "distress"),
    trt = "exposure",
    shift_functions = list(
      `a||b` = shared_ratio_shift_down,
      `a` = function(data, trt) data[[trt]] - 0.30
    ),
    include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = "pairwise",
    lmtp_defaults = shared_ratio_defaults(),
    n_cores = 1L,
    progress = "none",
    seed = 3031L,
    reuse_density_ratios = TRUE,
    manage_future_plan = TRUE
  ))

  models <- unlist(task$models, recursive = FALSE)
  expect_identical(
    sort(names(models)),
    sort(c(
      "perfectionism.perfectionism_a||b", "perfectionism.perfectionism_a",
      "distress.distress_a||b", "distress.distress_a"
    ))
  )
  for (model_name in names(models)) {
    fit_id <- attr(models[[model_name]], "margot_density_ratio_fit_id", exact = TRUE)
    expect_true(endsWith(model_name, paste0("_", fit_id)))
  }
  # the two policies are genuinely different fits, so a swapped cell would show
  expect_false(identical(
    models[["perfectionism.perfectionism_a||b"]]$density_ratios,
    models[["perfectionism.perfectionism_a"]]$density_ratios
  ))
})

test_that("a verified density checkpoint resumes a restart without refitting", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 29L)
  output_dir <- withr::local_tempdir()
  counter_file <- file.path(output_dir, "density-calls.log")
  honest_stage <- margot_lmtp_density_stage
  testthat::local_mocked_bindings(
    margot_lmtp_density_stage = function(payload) {
      # a file counter is visible across processes, unlike a mocked binding
      cat(payload$shift_name, "\n", file = counter_file, append = TRUE)
      honest_stage(payload)
    },
    .package = "margot"
  )
  density_calls <- function() {
    if (!file.exists(counter_file)) 0L else length(readLines(counter_file))
  }
  run <- function(defaults) {
    do.call(margot_lmtp, c(
      task_parallel_arguments(data, defaults = defaults, n_cores = 2L),
      list(
        manage_future_plan = TRUE,
        save_output = TRUE,
        save_path = output_dir,
        base_filename = "resume-test"
      )
    ))
  }

  failing <- shared_ratio_defaults()
  failing$learners_outcome <- "SL.absent_learner"
  expect_error(
    run_task_parallel(run(failing)),
    class = "margot_error_task_worker_failure"
  )
  expect_identical(density_calls(), 2L)

  restarted <- run_task_parallel(run(shared_ratio_defaults()))
  # the restart reads both verified checkpoints and calls no density stage again
  expect_identical(density_calls(), 2L)
  reuse <- attr(restarted, "margot_density_ratio_reuse")
  expect_identical(reuse$ratio_fit_count, 0L)
  expect_identical(reuse$ratio_checkpoint_reuse_count, 2L)
  expect_true(all(vapply(
    reuse$density_records,
    function(record) identical(record$density_source, "checkpoint"),
    logical(1)
  )))

  # a changed result-affecting input receives a new identity and a fresh fit
  changed <- data
  changed$baseline <- changed$baseline + 1
  moved <- run_task_parallel(do.call(margot_lmtp, c(
    task_parallel_arguments(changed, n_cores = 2L),
    list(
      manage_future_plan = TRUE, save_output = TRUE,
      save_path = output_dir, base_filename = "resume-test"
    )
  )))
  expect_identical(density_calls(), 4L)
  expect_identical(attr(moved, "margot_density_ratio_reuse")$ratio_fit_count, 2L)
})

test_that("a resumed run analyses the current first-outcome values", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 83L)
  cached_dir <- withr::local_tempdir()
  fresh_dir <- withr::local_tempdir()
  counter_file <- file.path(cached_dir, "density-calls.log")
  honest_stage <- margot_lmtp_density_stage
  testthat::local_mocked_bindings(
    margot_lmtp_density_stage = function(payload) {
      cat(payload$shift_name, "\n", file = counter_file, append = TRUE)
      honest_stage(payload)
    },
    .package = "margot"
  )
  density_calls <- function() {
    if (!file.exists(counter_file)) 0L else length(readLines(counter_file))
  }
  run <- function(frame, directory, outcomes = c("perfectionism", "distress")) {
    arguments <- task_parallel_arguments(frame, n_cores = 1L)
    arguments$outcome_vars <- outcomes
    run_task_parallel(do.call(margot_lmtp, c(
      arguments,
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = directory, base_filename = "first-outcome-test"
      )
    )))
  }

  run(data, cached_dir)
  expect_identical(density_calls(), 2L)

  # the first listed outcome changes; every density input stays as it was
  changed <- data
  changed$perfectionism <- rev(changed$perfectionism)
  resumed <- run(changed, cached_dir)
  expect_identical(density_calls(), 2L)
  expect_identical(attr(resumed, "margot_density_ratio_reuse")$ratio_fit_count, 0L)

  refitted <- run(changed, fresh_dir)
  expect_identical(attr(refitted, "margot_density_ratio_reuse")$ratio_fit_count, 2L)
  # a resumed analysis must equal a forced refit on the same current data
  expect_shared_agreement(resumed, refitted)
})

test_that("a resumed single-outcome run analyses the current outcome values", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 89L)
  cached_dir <- withr::local_tempdir()
  fresh_dir <- withr::local_tempdir()
  run <- function(frame, directory) {
    arguments <- task_parallel_arguments(frame, n_cores = 1L)
    arguments$outcome_vars <- "perfectionism"
    run_task_parallel(do.call(margot_lmtp, c(
      arguments,
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = directory, base_filename = "single-outcome-test"
      )
    )))
  }

  run(data, cached_dir)
  changed <- data
  changed$perfectionism <- rev(changed$perfectionism)
  resumed <- run(changed, cached_dir)
  refitted <- run(changed, fresh_dir)

  expect_identical(attr(resumed, "margot_density_ratio_reuse")$ratio_fit_count, 0L)
  expect_identical(attr(refitted, "margot_density_ratio_reuse")$ratio_fit_count, 2L)
  # with one outcome no later task can mask a stale source task
  expect_shared_agreement(resumed, refitted)
})

test_that("checkpoint reuse leaves the same caller state as a refit", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  previous_kind <- RNGkind()
  on.exit(RNGkind(previous_kind[1L], previous_kind[2L], previous_kind[3L]), add = TRUE)
  data <- make_shared_ratio_data(n = 60L, seed = 97L)
  state_after <- function(directory) {
    set.seed(7L)
    run_task_parallel(do.call(margot_lmtp, c(
      task_parallel_arguments(data, n_cores = 1L),
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = directory, base_filename = "rng-contract-test"
      )
    )))
    get(".Random.seed", envir = .GlobalEnv)
  }

  for (kind in c("Mersenne-Twister", "L'Ecuyer-CMRG")) {
    RNGkind(kind)
    directory <- withr::local_tempdir()
    refit_state <- state_after(directory)
    cached_state <- state_after(directory)
    # checkpoint availability must not reach the caller's random-number state
    expect_identical(cached_state, refit_state)
  }
})

test_that("two stored results under one identity refuse reuse", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 101L)
  output_dir <- withr::local_tempdir()
  arguments <- c(
    task_parallel_arguments(data, n_cores = 1L),
    list(
      manage_future_plan = TRUE, save_output = TRUE,
      save_path = output_dir, base_filename = "conflict-test"
    )
  )

  run_task_parallel(do.call(margot_lmtp, arguments))
  density_dir <- file.path(output_dir, "checkpoints", "density")
  stored_files <- list.files(density_dir, pattern = "\\.rds$", full.names = TRUE)
  expect_length(stored_files, 2L)
  # a second, differently fingerprinted result for one identity is impossible
  # under a complete identity, so its presence must refuse rather than be picked
  rival <- sub("_[0-9a-f]+\\.rds$", "_0000000000000000000000000000000000000000000000000000000000000000.rds", stored_files[[1L]])
  file.copy(stored_files[[1L]], rival)

  expect_error(
    run_task_parallel(do.call(margot_lmtp, arguments)),
    class = "margot_error_density_checkpoint_conflict"
  )
})

test_that("a corrupt density checkpoint refuses rather than being refitted over", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 31L)
  output_dir <- withr::local_tempdir()
  arguments <- c(
    task_parallel_arguments(data, n_cores = 1L),
    list(
      manage_future_plan = TRUE, save_output = TRUE,
      save_path = output_dir, base_filename = "corrupt-test"
    )
  )

  run_task_parallel(do.call(margot_lmtp, arguments))
  density_files <- list.files(
    file.path(output_dir, "checkpoints", "density"),
    pattern = "\\.rds$", full.names = TRUE
  )
  expect_length(density_files, 2L)
  stored <- readRDS(density_files[[1L]])
  stored$density_fit$density_ratios[1L, 1L] <- stored$density_fit$density_ratios[1L, 1L] + 1
  saveRDS(stored, density_files[[1L]])

  expect_error(
    run_task_parallel(do.call(margot_lmtp, arguments)),
    class = "margot_error_density_checkpoint_invalid"
  )
})

test_that("the local pool counts performance cores unless the caller sizes it", {
  # an unavailable sysctl query falls back to the ordinary core count
  testthat::local_mocked_bindings(
    margot_lmtp_sysctl_cores = function(name) NA_integer_,
    .package = "margot"
  )
  expect_identical(
    margot_lmtp_performance_cores(),
    as.integer(parallel::detectCores())
  )
  # a valid report is parsed and used in place of the total core count
  if (identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      grepl("^(arm|aarch)", R.version$arch)) {
    testthat::local_mocked_bindings(
      margot_lmtp_sysctl_cores = function(name) 6L,
      .package = "margot"
    )
    expect_identical(margot_lmtp_performance_cores(), 6L)
  }

  testthat::local_mocked_bindings(
    margot_lmtp_performance_cores = function() 2L,
    .package = "margot"
  )
  # a derived size is capped at the performance cores
  expect_identical(
    margot_lmtp_task_pool_size(
      models_in_parallel = 8L, n_cores = 8L, task_budget = 8L,
      models_in_parallel_supplied = FALSE
    ),
    2L
  )
  # an explicit request from the caller wins outright
  expect_identical(
    margot_lmtp_task_pool_size(
      models_in_parallel = 5L, n_cores = 8L, task_budget = 8L,
      models_in_parallel_supplied = TRUE
    ),
    5L
  )
  # the task budget still bounds an explicit request
  expect_identical(
    margot_lmtp_task_pool_size(
      models_in_parallel = 5L, n_cores = 8L, task_budget = 3L,
      models_in_parallel_supplied = TRUE
    ),
    3L
  )
})

test_that("an explicit models_in_parallel sizes the task pool", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 71L)
  testthat::local_mocked_bindings(
    margot_lmtp_performance_cores = function() 1L,
    .package = "margot"
  )

  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(
      task_parallel_arguments(data, n_cores = 4L),
      list(manage_future_plan = TRUE, models_in_parallel = 2L)
    )
  ))

  # the caller's request survives a performance-core count that would cap it to one
  expect_identical(attr(task, "margot_density_ratio_reuse")$worker_count, 2L)
  expect_true(inherits(future::plan("list")[[1L]], "sequential"))
})

test_that("cv_workers above one refuses task-parallel scheduling", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 53L)
  arguments <- task_parallel_arguments(data, n_cores = 1L)

  expect_error(
    run_task_parallel(do.call(
      margot_lmtp,
      c(arguments, list(manage_future_plan = TRUE, cv_workers = 2L))
    )),
    class = "margot_error_nested_parallel_unsupported"
  )
  # the combination the shared path used to refuse outright now schedules tasks
  expect_no_error(run_task_parallel(do.call(
    margot_lmtp,
    c(arguments, list(manage_future_plan = TRUE, cv_workers = 1L))
  )))
})

test_that("one ineligible worker among eligible workers is refused", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- make_shared_ratio_data(n = 60L, seed = 67L)
  workers <- parallelly::makeClusterPSOCK(2L)
  on.exit(try(parallel::stopCluster(workers), silent = TRUE), add = TRUE)
  # patch one node's copy of a shared-path internal; its version strings still agree
  patched <- parallel::clusterCall(workers[1L], function() {
    loadNamespace("margot")
    namespace <- asNamespace("margot")
    original <- get("margot_lmtp_restore_seed", envir = namespace)
    replacement <- function(seed_state) {
      assign(".Random.seed", seed_state, envir = .GlobalEnv)
      invisible(seed_state)
    }
    unlockBinding("margot_lmtp_restore_seed", namespace)
    assign("margot_lmtp_restore_seed", replacement, envir = namespace)
    lockBinding("margot_lmtp_restore_seed", namespace)
    !identical(body(original), body(replacement))
  })
  skip_if_not(isTRUE(patched[[1L]]), "could not patch a worker namespace")

  future::plan(future::cluster, workers = workers)
  expect_error(
    run_task_parallel(do.call(
      margot_lmtp,
      c(task_parallel_arguments(data), list(manage_future_plan = TRUE))
    )),
    class = "margot_error_worker_ineligible"
  )
})

test_that("an ineligible worker is refused before any task is dispatched", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 57L)
  honest_report <- margot_lmtp_worker_report
  testthat::local_mocked_bindings(
    margot_lmtp_worker_report = function() {
      report <- honest_report()
      report$lmtp_version <- "0.0.0"
      report
    },
    .package = "margot"
  )

  expect_error(
    run_task_parallel(do.call(
      margot_lmtp,
      c(task_parallel_arguments(data), list(manage_future_plan = TRUE))
    )),
    class = "margot_error_worker_ineligible"
  )
})

test_that("downstream Margot consumers pass on the task-parallel object", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 80L, seed = 59L)
  output_dir <- withr::local_tempdir()
  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(
      task_parallel_arguments(data),
      list(
        manage_future_plan = TRUE,
        save_output = TRUE,
        save_path = output_dir,
        base_filename = "task-parallel-test"
      )
    )
  ))

  table <- task$combined_tables[["combined_outcomes_down_vs_null"]]
  expect_identical(
    names(table),
    c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound")
  )
  plotted <- margot_plot(
    table,
    type = "RD",
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )
  expect_s3_class(plotted$plot, "ggplot")
  expect_equal(nrow(plotted$plot$data), 2L)
  multi_plot <- margot_plot_multi(
    tables = list(primary = table, repeated = table),
    type = "RD",
    include_coefficients = FALSE
  )
  expect_s3_class(multi_plot$plot, "patchwork")

  learner_report <- margot_report_lmtp_learners(
    task,
    outcome = "perfectionism",
    include_plot = FALSE
  )
  expect_true(nrow(learner_report$summary_table) > 0L)

  recombined <- margot_lmtp_combine_and_contrast(
    task,
    contrasts = list(c("down", "null")),
    keep_models = TRUE,
    quiet = TRUE
  )
  expect_identical(
    names(recombined),
    c("models", "contrasts", "individual_tables", "combined_tables")
  )
  expect_no_error(margot_lmtp_overlap(task, plot = FALSE, verbose = FALSE))
  expect_no_error(margot_bind_models(task, task, quiet = TRUE))

  # the immutable stage-one density checkpoints live beside the per-run model
  # directory, so a later run can resume from them and the model restore ignores them
  all_dirs <- list.dirs(
    file.path(output_dir, "checkpoints"),
    recursive = FALSE,
    full.names = TRUE
  )
  checkpoint_dirs <- setdiff(
    all_dirs,
    file.path(output_dir, "checkpoints", c("density", "outcomes"))
  )
  expect_length(checkpoint_dirs, 1L)
  density_checkpoints <- list.files(
    file.path(output_dir, "checkpoints", "density"),
    pattern = "\\.rds$"
  )
  expect_length(density_checkpoints, 2L)
  restored <- margot_lmtp_restore_checkpoints(
    checkpoint_dir = checkpoint_dirs[[1L]],
    outcome_vars = c("perfectionism", "distress"),
    contrast_type = "null",
    quiet = TRUE
  )
  expect_identical(restored$combined_tables, task$combined_tables)
})

test_that("a stochastic learner agrees between the sequential and task routes", {
  skip_if_not_installed("ranger")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- make_shared_ratio_data(n = 100L, seed = 51L)
  defaults <- shared_ratio_defaults()
  defaults$learners_trt <- c("SL.glm", "SL.ranger")
  defaults$learners_outcome <- c("SL.glm", "SL.ranger")
  arguments <- task_parallel_arguments(data, defaults = defaults)

  future::plan(future::sequential)
  reference <- run_task_parallel(do.call(margot_lmtp, arguments))

  future::plan(future::multisession, workers = 2L)
  task <- run_task_parallel(do.call(
    margot_lmtp,
    c(arguments, list(manage_future_plan = TRUE))
  ))

  expect_shared_agreement(task, reference)
})

test_that("the density stage runs alone and hands its work to the outcome stage", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 103L)
  split_dir <- withr::local_tempdir()
  straight_dir <- withr::local_tempdir()
  arguments <- function(directory, stages) {
    c(
      task_parallel_arguments(data, n_cores = 1L),
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = directory, base_filename = "stage-split-test", stages = stages
      )
    )
  }

  density_stage <- run_task_parallel(do.call(margot_lmtp, arguments(split_dir, "density")))
  expect_s3_class(density_stage, "margot_lmtp_density_stage")
  expect_identical(names(density_stage$density_ratios), c("down", "null"))
  expect_true(all(vapply(density_stage$density_ratios, is.matrix, logical(1))))
  # the diagnostics come from Margot's existing positivity machinery
  expect_true(is.data.frame(density_stage$diagnostics$positivity$by_wave))
  expect_true(is.data.frame(density_stage$diagnostics$overlap$overlap_summary))
  expect_true(nzchar(density_stage$diagnostics$overlap$text_summary))
  expect_null(density_stage$models)
  expect_identical(suppressMessages(print(density_stage)), density_stage)
  # density checkpoints exist; no outcome model has been fitted
  expect_length(
    list.files(file.path(split_dir, "checkpoints", "density"), pattern = "\\.rds$"), 2L
  )
  expect_length(
    list.files(file.path(split_dir, "checkpoints", "outcomes"), pattern = "\\.rds$"), 0L
  )

  outcome_stage <- run_task_parallel(do.call(margot_lmtp, arguments(split_dir, "outcome")))
  reuse <- attr(outcome_stage, "margot_density_ratio_reuse")
  expect_identical(reuse$ratio_fit_count, 0L)
  expect_identical(reuse$ratio_checkpoint_reuse_count, 2L)
  expect_identical(reuse$outcome_fit_count, 4L)

  straight <- run_task_parallel(do.call(margot_lmtp, arguments(straight_dir, "all")))
  # splitting the stages must not move a single number
  expect_shared_agreement(outcome_stage, straight)
})

test_that("the outcome stage refuses to refit an absent density stage", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 107L)
  output_dir <- withr::local_tempdir()

  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(
      task_parallel_arguments(data, n_cores = 1L),
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = output_dir, base_filename = "no-density-test", stages = "outcome"
      )
    ))),
    class = "margot_error_density_checkpoint_required"
  )
  # nothing was fitted, so no outcome model was written either
  expect_length(
    list.files(file.path(output_dir, "checkpoints", "outcomes"), pattern = "\\.rds$"), 0L
  )
})

test_that("stage-split execution refuses the routes that cannot support it", {
  data <- make_shared_ratio_data(n = 40L, seed = 109L)
  output_dir <- withr::local_tempdir()
  base <- task_parallel_arguments(data, n_cores = 1L)

  # the sequential shared route
  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(
      base, list(save_output = TRUE, save_path = output_dir, stages = "density")
    ))),
    class = "margot_error_unsupported_stage_split"
  )
  # a stage that writes nothing cannot hand its work on
  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(
      base, list(manage_future_plan = TRUE, stages = "density")
    ))),
    class = "margot_error_unsupported_stage_split"
  )
})

test_that("an interrupted run resumes its completed outcome models", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 113L)
  resumed_dir <- withr::local_tempdir()
  fresh_dir <- withr::local_tempdir()
  counter_file <- file.path(resumed_dir, "outcome-calls.log")
  state <- new.env(parent = emptyenv())
  state$interrupted <- FALSE
  honest_stage <- margot_lmtp_outcome_stage
  testthat::local_mocked_bindings(
    margot_lmtp_outcome_stage = function(payload) {
      cat(payload$outcome, payload$shift_name, "\n", file = counter_file, append = TRUE)
      if (identical(payload$outcome, "distress") &&
          identical(payload$shift_name, "null") && !state$interrupted) {
        # the power fails after the earlier outcomes have been written
        state$interrupted <- TRUE
        stop("simulated interruption")
      }
      honest_stage(payload)
    },
    .package = "margot"
  )
  outcome_calls <- function() {
    if (!file.exists(counter_file)) 0L else length(readLines(counter_file))
  }
  run <- function(directory) {
    run_task_parallel(do.call(margot_lmtp, c(
      task_parallel_arguments(data, n_cores = 1L),
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = directory, base_filename = "resume-outcome-test"
      )
    )))
  }

  expect_error(run(resumed_dir), class = "margot_error_task_worker_failure")
  expect_identical(outcome_calls(), 4L)
  # three of the four outcome models completed and were written
  expect_length(
    list.files(file.path(resumed_dir, "checkpoints", "outcomes"), pattern = "\\.rds$"), 3L
  )

  resumed <- run(resumed_dir)
  reuse <- attr(resumed, "margot_density_ratio_reuse")
  expect_identical(reuse$ratio_fit_count, 0L)
  expect_identical(reuse$ratio_checkpoint_reuse_count, 2L)
  expect_identical(reuse$outcome_checkpoint_reuse_count, 3L)
  expect_identical(reuse$outcome_fit_count, 1L)
  # exactly one outcome was fitted again
  expect_identical(outcome_calls(), 5L)

  uninterrupted <- run(fresh_dir)
  expect_shared_agreement(resumed, uninterrupted)
})

test_that("a changed outcome column refits that outcome alone", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 60L, seed = 127L)
  output_dir <- withr::local_tempdir()
  run <- function(frame) {
    run_task_parallel(do.call(margot_lmtp, c(
      task_parallel_arguments(frame, n_cores = 1L),
      list(
        manage_future_plan = TRUE, save_output = TRUE,
        save_path = output_dir, base_filename = "changed-outcome-test"
      )
    )))
  }

  run(data)
  changed <- data
  changed$distress <- rev(changed$distress)
  second <- run(changed)

  reuse <- attr(second, "margot_density_ratio_reuse")
  # the density identity excludes outcome values, so the shared stage is reused
  expect_identical(reuse$ratio_fit_count, 0L)
  # distress has a new task fingerprint under both policies; perfectionism does not
  expect_identical(reuse$outcome_fit_count, 2L)
  expect_identical(reuse$outcome_checkpoint_reuse_count, 2L)
})

test_that("a run that keeps checkpoints requires an explicit save_path", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 40L, seed = 131L)
  base <- task_parallel_arguments(data, n_cores = 1L)
  output_dir <- withr::local_tempdir()

  # a stage split reads what an earlier call stored
  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(
      base, list(manage_future_plan = TRUE, save_output = TRUE, stages = "density")
    ))),
    class = "margot_error_save_path_required"
  )
  # so does the task route's cross-run checkpoint store
  expect_error(
    run_task_parallel(do.call(margot_lmtp, c(
      base, list(manage_future_plan = TRUE, save_output = TRUE)
    ))),
    class = "margot_error_save_path_required"
  )
  # the same call with a storage root the caller chose runs
  expect_s3_class(
    run_task_parallel(do.call(margot_lmtp, c(
      base,
      list(
        manage_future_plan = TRUE, save_output = TRUE, save_path = output_dir,
        base_filename = "save-path-test", stages = "density"
      )
    ))),
    "margot_lmtp_density_stage"
  )
  # an in-memory task run keeps the existing default untouched
  expect_no_error(run_task_parallel(do.call(
    margot_lmtp, c(base, list(manage_future_plan = TRUE))
  )))
})

test_that("the legacy and sequential routes keep the existing save_path default", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 40L, seed = 137L)

  # the sequential shared route, in memory
  expect_no_error(run_task_parallel(do.call(
    margot_lmtp, task_parallel_arguments(data, n_cores = 1L)
  )))
  # the legacy independent route with its managed plan
  expect_no_error(run_task_parallel(margot_lmtp(
    data = data,
    outcome_vars = c("perfectionism", "distress"),
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = "pairwise",
    lmtp_defaults = shared_ratio_defaults(),
    n_cores = 1L,
    cv_workers = 1L,
    progress = "none",
    seed = 3031L,
    reuse_density_ratios = FALSE,
    manage_future_plan = TRUE
  )))
})
