test_that("checkpoint identities ignore the location a study file was read from", {
  first <- file.path(tempfile("study-a-"), "study.toml")
  second <- file.path(tempfile("study-b-"), "study.toml")
  for (path in c(first, second)) {
    dir.create(dirname(path))
    writeLines(c("seed = 42", "[estimation]", "folds = 5"), path)
  }
  on.exit(unlink(dirname(c(first, second)), recursive = TRUE), add = TRUE)
  study_a <- RcppTOML::parseTOML(first)
  study_b <- RcppTOML::parseTOML(second)
  # the parsed objects differ only in the file attribute RcppTOML records.
  expect_false(identical(study_a, study_b))
  expect_identical(margot_lmtp_checkpoint_identity(list(study = study_a, seed = 42L)),
    margot_lmtp_checkpoint_identity(list(study = study_b, seed = 42L)))
  destination <- tempfile("canonical-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  calls <- 0L
  compute <- function() { calls <<- calls + 1L; list(ratio = c(1, 2)) }
  margot_lmtp_checkpoint_compute(destination, "support", list(study = study_a), compute)
  reused <- margot_lmtp_checkpoint_compute(destination, "support", list(study = study_b), compute)
  expect_equal(calls, 1L)
  expect_true(reused$reused)
  expect_false(is.null(margot_lmtp_checkpoint_read(destination, "support", list(study = study_b))))
})

test_that("canonical identities keep content attributes and refuse reference objects", {
  frame <- data.frame(id = 1:3, group = factor(c("a", "b", "a")))
  identity <- list(frame = frame, matrix = matrix(1:4, 2, dimnames = list(NULL, c("x", "y"))),
    labelled = structure(1:3, note = "drop me"))
  canonical <- margot_lmtp_checkpoint_identity(identity)
  expect_identical(canonical$frame, frame)
  expect_identical(canonical$matrix, identity$matrix)
  expect_null(attr(canonical$labelled, "note"))
  expect_error(margot_lmtp_checkpoint_identity(list(env = new.env())), "environment")
  expect_error(margot_lmtp_checkpoint_identity(list(nested = list(f = function(x) x))), "closure")
})

test_that("the default inventory verifies files without loading saved values", {
  destination <- tempfile("inventory-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  identities <- list(exposure = list(seed = 1L), observation = list(seed = 2L))
  for (key in names(identities)) {
    margot_lmtp_checkpoint_compute(destination, key, identities[[key]], function() list(fit = rnorm(5)))
  }
  lean <- margot_lmtp_checkpoint_inventory(destination, identities)
  expect_true(lean$complete)
  expect_null(lean$records$exposure$value)
  expect_match(lean$records$exposure$payload_sha256, "^[0-9a-f]{64}$")
  full <- margot_lmtp_checkpoint_inventory(destination, identities, load = TRUE)
  expect_length(full$records$observation$value$fit, 5L)
  # a damaged payload is still detected without loading.
  path <- file.path(destination, "exposure.rds")
  record <- readRDS(path)
  record$payload[1L] <- as.raw(0)
  saveRDS(record, path)
  expect_error(margot_lmtp_checkpoint_inventory(destination, identities))
})

test_that("assert_lean stops on fitted learners and passes estimates through", {
  lean <- list(estimate = 1, weights = c(glmnet = 0.4, ranger = 0.6), diagnostics = data.frame(x = 1))
  expect_identical(margot_lmtp_assert_lean(lean), lean)
  fitted <- list(outcome = list(fits = list(structure(list(coef = 1), class = c("lmtp_ensemble", "SuperLearner")))))
  expect_error(margot_lmtp_assert_lean(fitted), "fitted\\$outcome\\$fits\\$1 \\(lmtp_ensemble\\)")
  expect_error(margot_lmtp_assert_lean(list(forest = structure(list(), class = "ranger")), what = "stage 6"),
    "stage 6")
})
