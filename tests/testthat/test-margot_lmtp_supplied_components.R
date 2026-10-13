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
