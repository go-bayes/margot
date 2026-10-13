test_that("custom estimators receive unchanged multi-wave shifts and save results", {
  data <- data.frame(id = 1:10, a1 = 1, a2 = 2, y = seq_len(10))
  make_shift <- function(amount) {
    force(amount)
    shift <- function(data, node) data[[node]] - amount
    environment(shift) <- list2env(list(amount = amount), parent = baseenv())
    shift
  }
  shifts <- list(natural = make_shift(0), reduction = make_shift(1))
  fingerprints <- vapply(shifts, digest::digest, character(1), algo = "sha256")
  calls <- 0L
  estimator <- function(data, trt, outcome, shift, learners_trt, learners_outcome) {
    calls <<- calls + 1L
    expect_true(digest::digest(shift, algo = "sha256") %in% fingerprints)
    structure(list(estimate = ife::ife(5, as.numeric(seq_len(10)), rep(1, 10), as.character(1:10)),
      outcome_type = "continuous", density_ratios = matrix(1, 10, 2)), class = "lmtp")
  }
  destination <- tempfile("wrapper-checkpoints-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  output <- suppressMessages(margot_lmtp(data, outcome_vars = "y", trt = c("a1", "a2"),
    shift_functions = shifts, include_null_shift = FALSE, lmtp_model_type = estimator,
    manage_future_plan = FALSE, save_output = TRUE, save_path = destination, progress = "none"))
  expect_equal(calls, 2L)
  expect_length(output$models$y, 2L)
  expect_length(list.files(file.path(destination, "checkpoints"), "[.]rds$", recursive = TRUE), 2L)
})

test_that("verified checkpoints reuse only compatible complete payloads", {
  destination <- tempfile("verified-tasks-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  identity <- list(inputs = 1:10, seed = 42L)
  calls <- 0L
  compute <- function() { calls <<- calls + 1L; list(predictions = 1:10) }
  first <- margot_lmtp_checkpoint_compute(destination, "exposure", identity, compute)
  second <- margot_lmtp_checkpoint_compute(destination, "exposure", identity, compute)
  expect_equal(calls, 1L)
  expect_false(first$reused)
  expect_true(second$reused)
  expect_identical(first$value, second$value)
  expect_false(margot_lmtp_checkpoint_inventory(destination,
    list(exposure = identity, observation = identity))$complete)
  expect_error(margot_lmtp_checkpoint_compute(destination, "exposure", list(inputs = 2), compute))
  path <- file.path(destination, "exposure.rds")
  record <- readRDS(path)
  record$payload[1L] <- as.raw(0)
  saveRDS(record, path)
  expect_error(margot_lmtp_checkpoint_read(destination, "exposure", identity))
  expect_equal(calls, 1L)
})

test_that("supplied components reject changed node histories before task construction", {
  frame <- data.frame(id = 1:4, w = 1, l1 = 2, l2 = 3, a1 = 1, a2 = 2, c1 = 1, c2 = 1)
  data <- transform(frame, y = seq_len(4))
  shift <- function(data, node) data[[node]]
  environment(shift) <- baseenv()
  baseline <- "w"
  time_vary <- list("l1", "l2")
  components <- list(
    frame = frame, history = c(baseline, unlist(time_vary)),
    baseline = baseline, time_vary = time_vary, trt = c("a1", "a2"),
    cens = c("c1", "c2"), density_ratios = matrix(1, nrow(frame), 2),
    folds = list(
      list(training_set = 3:4, validation_set = 1:2),
      list(training_set = 1:2, validation_set = 3:4)),
    weights = rep(1, nrow(frame)), seed = 42L, rng_kind = RNGkind(),
    shifted_exposures = lapply(c("a1", "a2"), function(node) shift(frame, node)),
    shift_source_sha256 = digest::digest(shift, algo = "sha256"))
  task_calls <- 0L
  fit_calls <- 0L
  local_mocked_bindings(
    margot_lmtp_make_task = function(...) {
      task_calls <<- task_calls + 1L
      stop("validated components reached task construction")
    },
    margot_lmtp_fit_sdr_outcome = function(...) {
      fit_calls <<- fit_calls + 1L
      stop("outcome fitting must not run")
    })
  local_mocked_bindings(nbrOfWorkers = function(...) 2L, .package = "future")
  invoke <- function(baseline, time_vary) margot_lmtp_sdr_from_components(
    data, components$trt, "y", shift, components, baseline, time_vary,
    learners_outcome = "SL.mean", bounds = c(0, 5))

  # establish that the fixture passes every input check without fitting.
  expect_error(invoke(baseline, time_vary), "validated components reached task construction")
  expect_identical(task_calls, 1L)
  task_calls <- 0L

  moved_time_vary <- list(character(), c("l1", "l2"))
  expect_setequal(c(baseline, unlist(moved_time_vary)), components$history)
  expect_error(invoke(baseline, moved_time_vary), "identical\\(time_vary, x\\$time_vary\\)")
  expect_error(invoke(c(baseline, "l1"), list(character(), "l2")),
    "identical\\(baseline, x\\$baseline\\)")
  expect_identical(task_calls, 0L)
  expect_identical(fit_calls, 0L)
})

test_that("nonconsecutive exposures preserve explicitly grouped covariate histories", {
  frame <- data.frame(
    id = 1:6, age_t10 = 40, wellbeing_t9 = 5,
    belief_t10 = 3, context_t10 = 1,
    wellbeing_t11 = 4, context_t12 = 2, belief_t13 = 3,
    wellbeing_t14 = 5, context_t13 = 1,
    attendance_t11 = 2, attendance_t14 = 3, attendance_t15 = 4,
    observed_t12 = 1, observed_t15 = 1, observed_t16 = 1)
  data <- transform(frame, y = seq_len(6))
  trt <- c("attendance_t11", "attendance_t14", "attendance_t15")
  baseline <- c("age_t10", "wellbeing_t9")
  time_vary <- list(
    c("belief_t10", "context_t10"),
    c("wellbeing_t11", "context_t12", "belief_t13"),
    c("wellbeing_t14", "context_t13"))
  shift <- function(data, node) data[[node]]
  environment(shift) <- baseenv()
  components <- list(
    frame = frame, history = c(baseline, unlist(time_vary)),
    baseline = baseline, time_vary = time_vary, trt = trt,
    cens = c("observed_t12", "observed_t15", "observed_t16"),
    density_ratios = matrix(1, nrow(frame), length(trt)),
    folds = lapply(seq_len(3), function(fold) {
      validation <- which(rep(seq_len(3), each = 2) == fold)
      list(training_set = setdiff(seq_len(nrow(frame)), validation),
        validation_set = validation)
    }),
    weights = rep(1, nrow(frame)), seed = 42L, rng_kind = RNGkind(),
    shifted_exposures = lapply(trt, function(node) shift(frame, node)),
    shift_source_sha256 = digest::digest(shift, algo = "sha256"))
  task_calls <- 0L
  expected <- list(data = data, trt = trt, baseline = baseline, time_vary = time_vary)
  local_mocked_bindings(margot_lmtp_make_task = function(data, trt, outcome, baseline, time_vary, ...) {
    task_calls <<- task_calls + 1L
    expect_identical(data, expected$data)
    expect_identical(trt, expected$trt)
    expect_identical(baseline, expected$baseline)
    expect_identical(time_vary, expected$time_vary)
    stop("declared histories reached task construction unchanged")
  })
  local_mocked_bindings(nbrOfWorkers = function(...) 3L, .package = "future")
  expect_error(margot_lmtp_sdr_from_components(
    data, trt, "y", shift, components, baseline, time_vary,
    learners_outcome = "SL.mean", bounds = c(0, 7)),
    "declared histories reached task construction unchanged")
  expect_identical(task_calls, 1L)
})
