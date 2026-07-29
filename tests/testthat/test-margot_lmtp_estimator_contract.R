# margot_lmtp() built from a sealed margot.lmtp estimator contract. the sealed
# object is a fixture: constructing a real one drives the whole margot.lmtp
# workflow, and this test asserts the bridge, not the sealing. The fixture is
# sealed the way margot.lmtp seals, because the bridge re-verifies the content
# hash on entry and a fixture carrying an invented hash would be refused.

fixture_spec <- function(seed = 20260714L,
                         profile = "glm",
                         arms = list(
                           list(arm_id = "null", mtp = FALSE, engine_class = "static"),
                           list(arm_id = "shift_up", mtp = TRUE, engine_class = "mtp")
                         )) {
  seal_fixture(
    list(
      question_id = "q1",
      estimator = "lmtp::lmtp_sdr",
      call_arguments = list(
        trt = c("t0_a", "t1_a"),
        baseline = c("b1", "b2"),
        time_vary = list("t0_l", "t1_l"),
        cens = c("t0_c", "t1_c"),
        compete = NULL,
        outcome = "t2_y",
        outcome_type = "continuous",
        id = "id",
        folds = 5L,
        bounds = NULL
      ),
      arms = arms,
      contrast = c("shift_up", "null"),
      seed = seed,
      trim = 0.999,
      learner_profile = profile
    )
  )
}

# a sealed object whose content hash is the hash margot.lmtp would have computed
# for it, so that the bridge's seal verification accepts it
seal_fixture <- function(payload) {
  seal <- structure(
    list(
      object_type = "estimator_spec",
      schema_version = "0",
      payload = payload,
      parents = character(0),
      content_hash = "fixture"
    ),
    class = c("margot_lmtp_estimator_spec", "margot_seal", "list")
  )
  if (requireNamespace("margot.lmtp", quietly = TRUE)) {
    seal$schema_version <- margot.lmtp::margot_lmtp_schema_version()
    seal$content_hash <- margot.lmtp:::seal_content_hash(seal)
  }
  seal
}

shift_up <- function(data, trt) data[[trt]] + 1

# --- the missing-dependency guard ------------------------------------------

test_that("a sealed contract without margot.lmtp errors as a missing dependency", {
  local_mocked_bindings(has_margot_lmtp = function() FALSE)
  expect_error(
    margot_lmtp(
      data = data.frame(id = 1),
      shift_functions = list(null = NULL, shift_up = shift_up),
      estimator_spec = fixture_spec()
    ),
    class = "margot_error_missing_dependency"
  )
})

test_that("a non-seal errors before anything else is read", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  expect_error(
    margot_lmtp(
      data = data.frame(id = 1),
      shift_functions = list(null = NULL),
      estimator_spec = list(payload = list())
    ),
    class = "margot_error_invalid_input"
  )
})

# --- the derived call ------------------------------------------------------

test_that("the lmtp call is built from the sealed contract", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  derived <- margot_lmtp_args_from_spec(
    estimator_spec = fixture_spec(),
    trt = NULL,
    outcome_vars = NULL,
    lmtp_defaults = list(),
    lmtp_model_type = lmtp::lmtp_tmle,
    seed = NULL,
    shift_functions = list(null = NULL, shift_up = shift_up),
    supplied = character()
  )

  expect_equal(derived$trt, c("t0_a", "t1_a"))
  expect_equal(derived$outcome_vars, "t2_y")
  expect_equal(derived$seed, 20260714L)
  expect_identical(derived$lmtp_model_type, lmtp::lmtp_sdr)
  expect_equal(derived$lmtp_defaults$baseline, c("b1", "b2"))
  expect_equal(derived$lmtp_defaults$cens, c("t0_c", "t1_c"))
  expect_equal(derived$lmtp_defaults$folds, 5L)
  expect_equal(derived$lmtp_defaults$outcome_type, "continuous")
  # the sealed cap arrives as an lmtp_control object, not as a bare number
  expect_equal(derived$lmtp_defaults$control$.trim, 0.999)
  expect_equal(derived$mtp_by_arm, c(null = FALSE, shift_up = TRUE))
})

test_that("the learner profile maps to the registered SuperLearner library", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  glm_spec <- margot_lmtp_args_from_spec(
    fixture_spec(profile = "glm"), NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
    list(null = NULL, shift_up = shift_up), character()
  )
  expect_equal(glm_spec$lmtp_defaults$learners_trt, "SL.glm")

  ensemble <- margot_lmtp_args_from_spec(
    fixture_spec(profile = "ensemble"), NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
    list(null = NULL, shift_up = shift_up), character()
  )
  expect_equal(
    ensemble$lmtp_defaults$learners_outcome,
    c("SL.mean", "SL.ranger", "SL.xgboost", "SL.glmnet")
  )

  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(profile = "unregistered"), NULL, NULL, list(), lmtp::lmtp_tmle,
      NULL, list(null = NULL, shift_up = shift_up), character()
    ),
    class = "margot_error_invalid_input"
  )
})

# --- conflicts -------------------------------------------------------------

test_that("every conflicting user argument errors and names the conflict", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  arms <- list(null = NULL, shift_up = shift_up)

  conflicting <- list(
    trt = list(trt = "other_a", supplied = "trt"),
    outcome_vars = list(outcome_vars = "other_y", supplied = "outcome_vars"),
    seed = list(seed = 1L, supplied = "seed"),
    lmtp_model_type = list(lmtp_model_type = lmtp::lmtp_tmle, supplied = "lmtp_model_type"),
    folds = list(lmtp_defaults = list(folds = 2L), supplied = character())
  )

  for (nm in names(conflicting)) {
    args <- list(
      estimator_spec = fixture_spec(), trt = NULL, outcome_vars = NULL,
      lmtp_defaults = list(), lmtp_model_type = lmtp::lmtp_sdr, seed = NULL,
      shift_functions = arms, supplied = character()
    )
    args <- utils::modifyList(args, conflicting[[nm]])
    err <- tryCatch(
      do.call(margot_lmtp_args_from_spec, args),
      margot_error_estimator_spec_conflict = function(e) e
    )
    expect_s3_class(err, "margot_error_estimator_spec_conflict")
    expect_match(conditionMessage(err), nm, info = nm)
  }
})

test_that("an lmtp_defaults entry outside the sealed set is named, not discarded", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  arms <- list(null = NULL, shift_up = shift_up)

  # `k` is not an argument the seal fixes, so the derived list would drop it in
  # silence and the fit would run without the setting the caller asked for
  err <- tryCatch(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(k = 1L), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    margot_error_estimator_spec_conflict = function(e) e
  )
  expect_s3_class(err, "margot_error_estimator_spec_conflict")
  expect_match(conditionMessage(err), "k")

  # an unnamed entry is named as such rather than passing unnoticed
  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(1L), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    class = "margot_error_estimator_spec_conflict"
  )

  # a sealed argument and an unsealed one arrive in the same condition
  err <- tryCatch(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(folds = 2L, k = 1L), lmtp::lmtp_sdr, NULL,
      arms, character()
    ),
    margot_error_estimator_spec_conflict = function(e) e
  )
  expect_match(conditionMessage(err), "folds")
  expect_match(conditionMessage(err), "k")

  # and an empty list still builds the call
  expect_type(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    "list"
  )
})

test_that("the bridge re-verifies the seal and refuses a doctored payload", {
  skip_if_not_installed("margot.lmtp")
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  arms <- list(null = NULL, shift_up = shift_up)

  doctored <- fixture_spec()
  doctored$payload$seed <- 999L
  expect_error(
    margot_lmtp_args_from_spec(
      doctored, NULL, NULL, list(), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    class = "margot_error_hash_mismatch"
  )

  doctored <- fixture_spec()
  doctored$payload$call_arguments$outcome <- "hacked"
  expect_error(
    margot_lmtp_args_from_spec(
      doctored, NULL, NULL, list(), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    class = "margot_error_hash_mismatch"
  )
})

test_that("the learner mapping is margot.lmtp's, not a mirrored copy", {
  skip_if_not_installed("margot.lmtp")
  for (profile in c("glm", "ensemble")) {
    expect_identical(
      margot_lmtp_spec_learners(profile), margot.lmtp::lmtp_learner_library(profile)
    )
  }
})

test_that("shift functions must name exactly the sealed arms", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(), lmtp::lmtp_sdr, NULL,
      list(shift_up = shift_up), character()
    ),
    class = "margot_error_estimator_spec_conflict"
  )
  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(), lmtp::lmtp_sdr, NULL,
      list(), character()
    ),
    class = "margot_error_estimator_spec_conflict"
  )
})

test_that("a matching argument is not a conflict", {
  local_mocked_bindings(has_margot_lmtp = function() TRUE)
  derived <- margot_lmtp_args_from_spec(
    fixture_spec(), c("t0_a", "t1_a"), "t2_y", list(), lmtp::lmtp_sdr, 20260714L,
    list(null = NULL, shift_up = shift_up),
    supplied = c("trt", "outcome_vars", "seed")
  )
  expect_equal(derived$seed, 20260714L)
})

# --- the seed argument -----------------------------------------------------

test_that("margot_lmtp() accepts a seed and rejects a non-scalar one", {
  expect_true("seed" %in% names(formals(margot_lmtp)))
  expect_null(eval(formals(margot_lmtp)$seed))
  expect_true("estimator_spec" %in% names(formals(margot_lmtp)))

  expect_error(
    margot_lmtp(
      data = data.frame(y = 1, a = 1),
      outcome_vars = "y", trt = "a",
      seed = c(1, 2)
    ),
    "single whole number"
  )
})
