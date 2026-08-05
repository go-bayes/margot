# Margot-native estimator specifications lock execution settings without
# coupling the estimator to a separate workflow package.

# create a small locked execution specification for bridge tests
fixture_spec <- function(seed = 20260714L,
                         profile = "glm",
                         policies = c(null = TRUE, shift_up = TRUE),
                         outcomes = "t2_y",
                         weight_column = NULL) {
  margot_lmtp_estimator_spec(
    trt = c("t0_a", "t1_a"),
    outcomes = outcomes,
    policies = policies,
    seed = seed,
    baseline = c("b1", "b2"),
    time_vary = list("t0_l", "t1_l"),
    cens = c("t0_c", "t1_c"),
    outcome_type = "continuous",
    id = "id",
    folds = 5L,
    learner_profile = profile,
    trim = 0.999,
    weight_column = weight_column
  )
}

# shift an exposure upward by one unit
shift_up <- function(data, trt) data[[trt]] + 1

test_that("the estimator specification belongs to Margot", {
  spec <- fixture_spec()
  expect_s3_class(spec, "margot_lmtp_estimator_spec")
  expect_identical(spec$schema_version, margot_lmtp_estimator_spec_version)
  expect_silent(margot_lmtp_spec_verify(spec))
})

test_that("a non-specification errors before anything else is read", {
  expect_error(
    margot_lmtp(
      data = data.frame(id = 1),
      shift_functions = list(null = NULL),
      estimator_spec = list(payload = list())
    ),
    class = "margot_error_invalid_input"
  )
})

test_that("the lmtp call is built from the locked specification", {
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
  expect_equal(derived$lmtp_defaults$control$.trim, 0.999)
  expect_equal(derived$mtp_by_arm, c(null = TRUE, shift_up = TRUE))
})

test_that("one specification can lock several terminal outcomes", {
  derived <- margot_lmtp_args_from_spec(
    fixture_spec(outcomes = c("perfectionism", "distress")),
    NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
    list(null = NULL, shift_up = shift_up), character()
  )
  expect_identical(derived$outcome_vars, c("perfectionism", "distress"))
})

test_that("the learner profile maps to Margot's SuperLearner library", {
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

  doctored <- fixture_spec()
  doctored$payload$learner_profile <- "unregistered"
  doctored$content_hash <- margot_lmtp_spec_hash(doctored$schema_version, doctored$payload)
  expect_error(
    margot_lmtp_args_from_spec(
      doctored, NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
      list(null = NULL, shift_up = shift_up), character()
    ),
    class = "margot_error_invalid_input"
  )
})

test_that("the locked weight column is resolved from the analysis data", {
  analysis_data <- data.frame(analysis_weight = c(0.5, 1, 1.5))
  derived <- margot_lmtp_args_from_spec(
    fixture_spec(weight_column = "analysis_weight"),
    NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
    list(null = NULL, shift_up = shift_up), character(),
    data = analysis_data
  )
  expect_identical(derived$lmtp_defaults$weights, analysis_data$analysis_weight)

  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(weight_column = "analysis_weight"),
      NULL, NULL, list(), lmtp::lmtp_tmle, NULL,
      list(null = NULL, shift_up = shift_up), character(),
      data = data.frame(other = 1:3)
    ),
    class = "margot_error_invalid_input"
  )
})

test_that("every conflicting user argument errors and names the conflict", {
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

test_that("an unlocked lmtp_defaults entry is named rather than discarded", {
  arms <- list(null = NULL, shift_up = shift_up)
  err <- tryCatch(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(k = 1L), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    margot_error_estimator_spec_conflict = function(e) e
  )
  expect_s3_class(err, "margot_error_estimator_spec_conflict")
  expect_match(conditionMessage(err), "k")

  expect_error(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(1L), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    class = "margot_error_estimator_spec_conflict"
  )

  err <- tryCatch(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(folds = 2L, k = 1L), lmtp::lmtp_sdr, NULL,
      arms, character()
    ),
    margot_error_estimator_spec_conflict = function(e) e
  )
  expect_match(conditionMessage(err), "folds")
  expect_match(conditionMessage(err), "k")

  expect_type(
    margot_lmtp_args_from_spec(
      fixture_spec(), NULL, NULL, list(), lmtp::lmtp_sdr, NULL, arms, character()
    ),
    "list"
  )
})

test_that("the bridge refuses a doctored payload", {
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

test_that("shift functions must name exactly the locked arms", {
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
  derived <- margot_lmtp_args_from_spec(
    fixture_spec(), c("t0_a", "t1_a"), "t2_y", list(), lmtp::lmtp_sdr, 20260714L,
    list(null = NULL, shift_up = shift_up),
    supplied = c("trt", "outcome_vars", "seed")
  )
  expect_equal(derived$seed, 20260714L)
})

test_that("margot_lmtp accepts and validates its seed", {
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
