# margot_lmtp() records each model's warnings, and the learners SuperLearner
# drops, without muffling them or changing estimates. Generated data only.

# a learner that always errors, and one that warns with its process id and
# otherwise predicts the mean; SuperLearner looks learners up from its own
# namespace, whose search path reaches the global environment
warning_learners <- function() {
  fail <- function(Y, X, newX, family, obsWeights, id, ...) {
    stop("forced learner failure")
  }
  noisy <- function(Y, X, newX, family, obsWeights, id, ...) {
    warning("margot test learner warning from process ", Sys.getpid(), call. = FALSE)
    SuperLearner::SL.mean(
      Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, id = id, ...
    )
  }
  environment(fail) <- globalenv()
  environment(noisy) <- globalenv()
  list(SL.margot_fail = fail, SL.margot_noisy = noisy)
}

local_warning_learners <- function(envir = parent.frame()) {
  learners <- warning_learners()
  for (name in names(learners)) assign(name, learners[[name]], envir = globalenv())
  withr::defer(
    rm(list = intersect(names(learners), ls(globalenv())), envir = globalenv()),
    envir = envir
  )
}

# a cluster future clears the worker's global environment, so the learners are
# attached on each worker's search path, which SuperLearner's lookup also reaches
register_cluster_learners <- function(cluster) {
  parallel::clusterCall(cluster, function(learners) {
    attach(list2env(learners), name = "margot_test_learners", warn.conflicts = FALSE)
    NULL
  }, warning_learners())
  invisible(cluster)
}

local_sequential_plan <- function(envir = parent.frame()) {
  old_plan <- future::plan()
  future::plan(future::sequential)
  withr::defer(future::plan(old_plan, substitute = FALSE), envir = envir)
}

warning_defaults <- function(learners_trt = c("SL.glm", "SL.mean"),
                             learners_outcome = c("SL.glm", "SL.mean")) {
  list(
    baseline = "baseline",
    outcome_type = "continuous",
    id = "id",
    folds = 2L,
    learners_trt = learners_trt,
    learners_outcome = learners_outcome
  )
}

# generated once, outside any seeded call, because the generator resets the seed
warning_data <- make_shared_ratio_data(n = 60L, seed = 11L)

run_warnings <- function(lmtp_defaults, ..., outcome_vars = "perfectionism", seed = 2026L) {
  margot_lmtp(
    data = warning_data,
    outcome_vars = outcome_vars,
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr,
    lmtp_defaults = lmtp_defaults,
    progress = "none",
    seed = seed,
    ...
  )
}

# run quietly, keeping the warnings out of the test output
quiet <- function(code) suppressWarnings(suppressMessages(code))

native_sdr <- function(lmtp_defaults, outcome = "perfectionism", seed = 2026L) {
  set.seed(seed)
  quiet(do.call(lmtp::lmtp_sdr, c(
    list(
      data = warning_data, trt = "exposure",
      outcome = outcome, shift = shared_ratio_shift_down
    ),
    lmtp_defaults
  )))
}

dropped_warning <- function(learner) {
  paste(
    "Error in algorithm", learner,
    "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n"
  )
}

test_that("the parser names each dropped learner once per stage", {
  warnings <- c(
    density = dropped_warning("SL.glmnet"),
    density = dropped_warning("SL.glmnet"),
    density = paste("Error in algorithm SL.glmnet", " on full data",
      "\n  The Algorithm will be removed from the Super Learner (i.e. given weight 0) \n"),
    outcome = dropped_warning("SL.glmnet"),
    outcome = "Coefficients already 0 for all failed algorithm(s)",
    outcome = dropped_warning("SL.ranger")
  )
  expect_identical(
    margot_lmtp_dropped_learners(warnings),
    c(density = "SL.glmnet", outcome = "SL.glmnet", outcome = "SL.ranger")
  )
  expect_length(margot_lmtp_dropped_learners(character()), 0L)
  expect_length(margot_lmtp_dropped_learners(c(fit = "an unrelated warning")), 0L)
})

test_that("a failing learner is recorded, alerted and still shown on the independent route", {
  local_sequential_plan()
  local_warning_learners()
  defaults <- warning_defaults(learners_trt = c("SL.glm", "SL.margot_fail"))
  shown <- character()
  alerts <- character()
  output <- withCallingHandlers(
    run_warnings(defaults),
    warning = function(w) {
      shown <<- c(shown, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      alerts <<- c(alerts, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  model <- output$models$perfectionism$perfectionism_down
  expect_true(any(grepl("Error in algorithm SL.margot_fail", model$warnings, fixed = TRUE)))
  expect_identical(model$dropped_learners, c(fit = "SL.margot_fail"))
  expect_true(all(names(model$warnings) == "fit"))
  # nothing is muffled: every recorded warning also reached the caller
  expect_true(all(model$warnings %in% shown))
  expect_length(grep("SuperLearner dropped", alerts, fixed = TRUE), 1L)
})

test_that("a clean fit has empty warning fields", {
  local_sequential_plan()
  for (shared in c(FALSE, TRUE)) {
    output <- quiet(run_warnings(warning_defaults(), reuse_density_ratios = shared))
    model <- output$models$perfectionism$perfectionism_down
    expect_identical(unname(model$warnings), character(), info = shared)
    expect_length(model$dropped_learners, 0L)
  }
})

test_that("estimates are identical with and without the capture", {
  local_sequential_plan()
  local_warning_learners()
  defaults <- warning_defaults(
    learners_trt = c("SL.glm", "SL.margot_fail"),
    learners_outcome = c("SL.glm", "SL.margot_noisy")
  )
  native <- native_sdr(defaults)
  independent <- quiet(run_warnings(defaults))$models$perfectionism$perfectionism_down
  shared <- quiet(run_warnings(defaults, reuse_density_ratios = TRUE))$models$perfectionism$perfectionism_down
  expect_identical(shared_ratio_numerics(independent), shared_ratio_numerics(native))
  expect_identical(shared_ratio_numerics(shared), shared_ratio_numerics(native))
  expect_identical(independent$fits_m, native$fits_m)
  expect_identical(shared$fits_r, native$fits_r)
})

test_that("the shared route names density and outcome warnings by stage", {
  local_sequential_plan()
  local_warning_learners()
  defaults <- warning_defaults(
    learners_trt = c("SL.glm", "SL.margot_fail"),
    learners_outcome = c("SL.glm", "SL.margot_noisy")
  )
  output <- quiet(run_warnings(
    defaults,
    outcome_vars = c("perfectionism", "distress"),
    reuse_density_ratios = TRUE
  ))
  for (outcome in c("perfectionism", "distress")) {
    model <- output$models[[outcome]][[paste0(outcome, "_down")]]
    # the density stage is fitted once and its warnings belong to every outcome
    expect_identical(model$dropped_learners, c(density = "SL.margot_fail"), info = outcome)
    expect_true(any(grepl("SL.margot_fail", model$warnings[names(model$warnings) == "density"])))
    expect_true(any(grepl(
      "margot test learner warning", model$warnings[names(model$warnings) == "outcome"]
    )))
  }
})

test_that("the record survives saving and reading", {
  local_sequential_plan()
  local_warning_learners()
  output_dir <- withr::local_tempdir("model-warnings-")
  defaults <- warning_defaults(learners_outcome = c("SL.glm", "SL.margot_fail"))
  output <- quiet(run_warnings(
    defaults, save_output = TRUE, save_path = output_dir, base_filename = "warnings-test"
  ))
  model <- output$models$perfectionism$perfectionism_down
  expect_identical(model$dropped_learners, c(fit = "SL.margot_fail"))
  saved <- readRDS(file.path(output_dir, "warnings-test.rds"))
  expect_identical(saved$models$perfectionism$perfectionism_down$warnings, model$warnings)
  expect_identical(
    saved$models$perfectionism$perfectionism_down$dropped_learners,
    model$dropped_learners
  )
  checkpoint <- list.files(
    file.path(output_dir, "checkpoints"), "perfectionism_down[.]rds$",
    recursive = TRUE, full.names = TRUE
  )
  expect_length(checkpoint, 1L)
  expect_identical(readRDS(checkpoint)$model$warnings, model$warnings)
})

test_that("warnings raised in cluster workers are relayed and recorded", {
  skip_on_cran()
  skip_if_not_installed("parallelly")
  local_warning_learners()
  cluster <- parallelly::makeClusterPSOCK(2L, autoStop = TRUE)
  register_cluster_learners(cluster)
  worker_pids <- unlist(parallel::clusterCall(cluster, Sys.getpid))
  old_plan <- future::plan()
  on.exit(future::plan(old_plan, substitute = FALSE), add = TRUE)
  future::plan(future::cluster, workers = cluster)
  defaults <- warning_defaults(
    learners_trt = c("SL.glm", "SL.margot_fail"),
    learners_outcome = c("SL.glm", "SL.margot_noisy")
  )
  # lmtp fits each cross-fitting fold in a future on the cluster; the
  # coordinator records what future re-signals when it collects the folds
  for (shared in c(FALSE, TRUE)) {
    output <- quiet(run_warnings(defaults, reuse_density_ratios = shared))
    model <- output$models$perfectionism$perfectionism_down
    noisy <- grep("margot test learner warning from process", model$warnings, value = TRUE)
    expect_gt(length(noisy), 0L)
    pids <- as.integer(sub(".*from process ([0-9]+).*", "\\1", noisy))
    expect_true(all(pids %in% worker_pids), info = shared)
    expect_false(Sys.getpid() %in% pids)
    expect_true("SL.margot_fail" %in% model$dropped_learners)
  }
  # the cluster gives the same numbers as a sequential fit
  future::plan(future::sequential)
  expect_identical(
    shared_ratio_numerics(quiet(run_warnings(defaults))$models$perfectionism$perfectionism_down),
    shared_ratio_numerics(native_sdr(defaults))
  )
})

test_that("the task-parallel route records worker warnings and keeps them in checkpoints", {
  skip_on_cran()
  skip_if_not_installed("parallelly")
  local_warning_learners()
  cluster <- parallelly::makeClusterPSOCK(2L, autoStop = TRUE)
  register_cluster_learners(cluster)
  worker_pids <- unlist(parallel::clusterCall(cluster, Sys.getpid))
  old_plan <- future::plan()
  on.exit(future::plan(old_plan, substitute = FALSE), add = TRUE)
  future::plan(future::cluster, workers = cluster)
  output_dir <- withr::local_tempdir("task-warnings-")
  defaults <- warning_defaults(
    learners_trt = c("SL.glm", "SL.margot_fail"),
    learners_outcome = c("SL.glm", "SL.margot_noisy")
  )
  run_task <- function() {
    quiet(run_warnings(
      defaults,
      outcome_vars = c("perfectionism", "distress"),
      reuse_density_ratios = TRUE, manage_future_plan = TRUE,
      save_output = TRUE, save_path = output_dir
    ))
  }
  output <- tryCatch(run_task(), margot_error_worker_ineligible = function(e) NULL)
  if (is.null(output)) {
    skip("the cluster workers do not run this margot build")
  }
  for (outcome in c("perfectionism", "distress")) {
    model <- output$models[[outcome]][[paste0(outcome, "_down")]]
    expect_identical(model$dropped_learners, c(density = "SL.margot_fail"), info = outcome)
    noisy <- grep("from process", model$warnings[names(model$warnings) == "outcome"], value = TRUE)
    expect_gt(length(noisy), 0L)
    expect_true(all(as.integer(sub(".*from process ([0-9]+).*", "\\1", noisy)) %in% worker_pids))
  }
  density_file <- list.files(file.path(output_dir, "checkpoints", "density"), full.names = TRUE)
  expect_length(density_file, 1L)
  expect_true(any(grepl("SL.margot_fail", readRDS(density_file)$warnings)))
  first <- output$models$perfectionism$perfectionism_down

  # a restart reuses both checkpoints and keeps the recorded warnings
  again <- run_task()
  reuse <- attr(again, "margot_density_ratio_reuse")
  expect_identical(reuse$ratio_checkpoint_reuse_count, 1L)
  expect_identical(reuse$outcome_checkpoint_reuse_count, 2L)
  expect_identical(again$models$perfectionism$perfectionism_down$warnings, first$warnings)
  expect_identical(
    shared_ratio_numerics(again$models$perfectionism$perfectionism_down),
    shared_ratio_numerics(first)
  )
})

test_that("margot_lmtp_sdr_from_components records its outcome warnings", {
  frame <- data.frame(id = 1:4, w = 1, l1 = 2, l2 = 3, a1 = 1, a2 = 2, c1 = 1, c2 = 1)
  data <- transform(frame, y = seq_len(4))
  shift <- function(data, node) data[[node]]
  environment(shift) <- baseenv()
  components <- list(
    frame = frame, history = c("w", "l1", "l2"),
    baseline = "w", time_vary = list("l1", "l2"), trt = c("a1", "a2"),
    cens = c("c1", "c2"), density_ratios = matrix(1, nrow(frame), 2),
    folds = list(
      list(training_set = 3:4, validation_set = 1:2),
      list(training_set = 1:2, validation_set = 3:4)),
    weights = rep(1, nrow(frame)), seed = 42L, rng_kind = RNGkind(),
    shifted_exposures = lapply(c("a1", "a2"), function(node) shift(frame, node)),
    shift_source_sha256 = digest::digest(shift, algo = "sha256"))
  local_mocked_bindings(
    margot_lmtp_fit_sdr_outcome = function(task, density_fit, ...) {
      warning(dropped_warning("SL.margot_fail"), call. = FALSE)
      list(density_ratios = density_fit$density_ratios)
    })
  local_mocked_bindings(nbrOfWorkers = function(...) 2L, .package = "future")
  expect_warning(
    fit <- margot_lmtp_sdr_from_components(
      data, components$trt, "y", shift, components, "w", list("l1", "l2"),
      learners_outcome = "SL.mean", bounds = c(0, 5)),
    "removed from the Super Learner"
  )
  expect_identical(fit$dropped_learners, c(outcome = "SL.margot_fail"))
  expect_identical(names(fit$warnings), "outcome")
})

test_that("the untrimmed sensitivity model gets its own dropped-learner alert", {
  local_sequential_plan()
  local_warning_learners()
  defaults <- warning_defaults(learners_outcome = c("SL.glm", "SL.margot_fail"))
  defaults$control <- lmtp::lmtp_control(.trim = 0.9)
  messages <- testthat::capture_messages(suppressWarnings(run_warnings(
    defaults,
    reuse_density_ratios = TRUE,
    untrimmed_sensitivity = TRUE
  )))
  dropped <- messages[grepl("SuperLearner dropped", messages)]
  expect_true(any(grepl("perfectionism - down:", dropped, fixed = TRUE)))
  expect_true(any(grepl("perfectionism - down (untrimmed):", dropped, fixed = TRUE)))
})
