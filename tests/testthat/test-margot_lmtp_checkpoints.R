test_that("checkpoint identities ignore the location a study file was read from", {
  skip_if_not_installed("RcppTOML")
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

test_that("identities without provenance attributes are returned byte for byte", {
  identity <- list(frame = data.frame(a = 1:3, b = c("x", "y", "z")),
    versions = list(r = getRversion(), margot = utils::packageVersion("margot")),
    when = as.POSIXlt("2026-11-04 08:00:00", tz = "UTC"), seed = 42L)
  canonical <- margot_lmtp_checkpoint_identity(identity)
  expect_identical(serialize(canonical, NULL), serialize(identity, NULL))
  # a record written by margot 1.3.2 hashed the identity as given; it still verifies.
  destination <- tempfile("legacy-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  dir.create(destination)
  payload <- serialize(list(probability = c(0.2, 0.8)), NULL, version = 3L)
  saveRDS(list(schema = "mlmtp_verified_checkpoint_v1", key = "legacy", identity = identity,
    identity_sha256 = digest::digest(identity, algo = "sha256"), payload = payload,
    payload_sha256 = digest::digest(payload, algo = "sha256")), file.path(destination, "legacy.rds"))
  expect_identical(margot_lmtp_checkpoint_read(destination, "legacy", identity)$value$probability, c(0.2, 0.8))
})

test_that("content attributes are kept, so different inputs never share an identity", {
  hash <- function(x) digest::digest(margot_lmtp_checkpoint_identity(list(x = x)), algo = "sha256")
  expect_false(hash(as.difftime(5, units = "secs")) == hash(as.difftime(5, units = "days")))
  expect_false(hash(stats::ts(1:4, start = 2000)) == hash(stats::ts(1:4, start = 2010)))
  with_contrasts <- factor(c("a", "b"))
  stats::contrasts(with_contrasts) <- stats::contr.sum(2)
  expect_false(hash(factor(c("a", "b"))) == hash(with_contrasts))
  expect_false(hash(structure(1, comment = "a")) == hash(structure(1, comment = "b")))
  # provenance attributes are removed at any depth.
  nested <- list(study = list(design = structure(list(seed = 1L), file = "/a/study.toml")))
  expect_null(attr(margot_lmtp_checkpoint_identity(nested)$study$design, "file"))
  # a rebuilt data frame serialises like the same frame without the provenance attribute.
  plain <- data.frame(a = c(4L, 1L, 9L))
  plain$a <- plain$a
  tagged <- data.frame(a = c(4L, 1L, 9L))
  tagged$a <- structure(tagged$a, srcref = "x")
  expect_identical(serialize(margot_lmtp_checkpoint_identity(list(f = tagged)), NULL),
    serialize(list(f = plain), NULL))
})

test_that("identities refuse reference and S4 objects", {
  expect_error(margot_lmtp_checkpoint_identity(list(env = new.env())), "environment objects")
  expect_error(margot_lmtp_checkpoint_identity(list(nested = list(f = function(x) x))), "closure objects")
  expect_error(margot_lmtp_checkpoint_identity(list(expr = as.expression(list(new.env())))), "environment objects")
  setClass("MargotIdentityProbe", representation(x = "numeric"), where = environment())
  expect_error(margot_lmtp_checkpoint_identity(list(s4 = methods::new("MargotIdentityProbe", x = 1))),
    "S4 objects")
})

test_that("a lean inventory verifies files without loading saved values", {
  destination <- tempfile("inventory-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  identities <- list(exposure = list(seed = 1L), observation = list(seed = 2L))
  for (key in names(identities)) {
    margot_lmtp_checkpoint_compute(destination, key, identities[[key]], function() list(fit = rnorm(5)))
  }
  lean <- margot_lmtp_checkpoint_inventory(destination, identities, load = FALSE)
  expect_true(lean$complete)
  expect_null(lean$records$exposure$value)
  expect_match(lean$records$exposure$payload_sha256, "^[0-9a-f]{64}$")
  # the default keeps the earlier behaviour and returns values.
  full <- margot_lmtp_checkpoint_inventory(destination, identities)
  expect_length(full$records$observation$value$fit, 5L)
  # a damaged payload is still detected without loading.
  path <- file.path(destination, "exposure.rds")
  record <- readRDS(path)
  record$payload[1L] <- as.raw(0)
  saveRDS(record, path)
  expect_error(margot_lmtp_checkpoint_inventory(destination, identities, load = FALSE), "payload_sha256")
})

test_that("assert_lean stops on fitted learners and passes estimates through", {
  lean <- list(estimate = 1, weights = c(glmnet = 0.4, ranger = 0.6), diagnostics = data.frame(x = 1))
  expect_identical(margot_lmtp_assert_lean(lean), lean)
  fitted <- list(outcome = list(fits = list(structure(list(coef = 1), class = c("lmtp_ensemble", "SuperLearner")))))
  expect_error(margot_lmtp_assert_lean(fitted), "fitted\\$outcome\\$fits\\$1 \\(lmtp_ensemble\\)")
  skip_if_not_installed("ranger")
  set.seed(42)
  data <- data.frame(y = rnorm(50), x = rnorm(50))
  forest <- ranger::ranger(y ~ x, data = data, num.trees = 5, num.threads = 1)
  expect_error(margot_lmtp_assert_lean(list(stage = list(forest = forest)), what = "stage 6"),
    "stage 6\\$stage\\$forest \\(ranger\\)")
})
