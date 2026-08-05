# Margot-native estimator specifications for reproducible LMTP calls. The
# specification fixes execution settings alone; it does not encode the causal
# question, identification assumptions, or investigators' design judgement.

margot_lmtp_estimator_spec_version <- "1.0.0"

# hash the declarative estimator payload for later integrity verification
margot_lmtp_spec_hash <- function(schema_version, payload) {
  digest::digest(
    list(schema_version = schema_version, payload = payload),
    algo = "sha256",
    serialize = TRUE,
    serializeVersion = 2L
  )
}

#' Lock the execution settings for a Margot LMTP analysis
#'
#' `margot_lmtp_estimator_spec()` records the arguments that determine an LMTP
#' fit and protects them with a content hash. The specification belongs to
#' Margot and requires no companion package. It deliberately records execution
#' settings alone: the causal question, causal estimand, identification
#' assumptions, and policy rationale remain in the study protocol.
#'
#' The specification can name several terminal outcomes. When it is passed to
#' [margot_lmtp()] with `reuse_density_ratios = TRUE`, Margot fits each
#' policy-specific treatment and censoring density-ratio process once and reuses
#' it across those outcomes.
#'
#' @param trt Character vector naming the exposure at each policy node.
#' @param outcomes Character vector naming the terminal outcomes.
#' @param policies Named logical vector. Each name is a policy arm and each
#'   value is the `mtp` setting passed to `lmtp` for that arm. Continuous shifts,
#'   including a natural-course arm represented by `shift = NULL`, ordinarily
#'   use `TRUE`.
#' @param seed Single whole-number estimation seed.
#' @param baseline Optional character vector of baseline covariates.
#' @param time_vary Optional time-varying covariate specification passed to
#'   `lmtp`.
#' @param cens Optional character vector of censoring indicators.
#' @param compete Optional character vector of competing-event indicators.
#' @param outcome_type Outcome model, `"continuous"` or `"binomial"`.
#' @param id Optional participant identifier column.
#' @param folds Number of cross-fitting folds.
#' @param bounds Optional common outcome bounds passed to `lmtp`.
#' @param learner_profile Registered learner profile, `"glm"` or
#'   `"ensemble"`.
#' @param trim Pooled density-ratio quantile cap passed to
#'   [lmtp::lmtp_control()].
#' @param weight_column Optional data column containing non-negative analysis
#'   weights. The values remain in the analysis data rather than the
#'   specification.
#'
#' @return An object of class `margot_lmtp_estimator_spec`.
#' @export
margot_lmtp_estimator_spec <- function(trt,
                                       outcomes,
                                       policies,
                                       seed,
                                       baseline = NULL,
                                       time_vary = NULL,
                                       cens = NULL,
                                       compete = NULL,
                                       outcome_type = c("continuous", "binomial"),
                                       id = NULL,
                                       folds = 5L,
                                       bounds = NULL,
                                       learner_profile = c("ensemble", "glm"),
                                       trim = 0.999,
                                       weight_column = NULL) {
  outcome_type <- match.arg(outcome_type)
  learner_profile <- match.arg(learner_profile)
  character_arguments <- list(trt = trt, outcomes = outcomes)
  invalid_character <- vapply(
    character_arguments,
    function(x) !is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)),
    logical(1)
  )
  if (any(invalid_character)) {
    cli::cli_abort(
      "{.arg {names(invalid_character)[invalid_character][1]}} must contain one or more column names.",
      class = "margot_error_invalid_input"
    )
  }
  optional_character <- list(baseline = baseline, cens = cens, compete = compete)
  invalid_optional <- vapply(
    optional_character,
    function(x) !is.null(x) && (!is.character(x) || anyNA(x) || any(!nzchar(x))),
    logical(1)
  )
  if (any(invalid_optional)) {
    cli::cli_abort(
      "{.arg {names(invalid_optional)[invalid_optional][1]}} must be `NULL` or a character vector of column names.",
      class = "margot_error_invalid_input"
    )
  }
  if (!is.null(id) && (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id))) {
    cli::cli_abort("{.arg id} must be `NULL` or one column name.", class = "margot_error_invalid_input")
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != trunc(seed)) {
    cli::cli_abort("{.arg seed} must be a single whole number.", class = "margot_error_invalid_input")
  }
  if (!is.numeric(folds) || length(folds) != 1L || is.na(folds) ||
      folds < 2L || folds != trunc(folds)) {
    cli::cli_abort("{.arg folds} must be a whole number of at least two.", class = "margot_error_invalid_input")
  }
  if (!is.numeric(trim) || length(trim) != 1L || is.na(trim) || trim <= 0 || trim > 1) {
    cli::cli_abort("{.arg trim} must be a single number in (0, 1].", class = "margot_error_invalid_input")
  }
  if (!is.null(bounds) &&
      (!is.numeric(bounds) || length(bounds) != 2L || anyNA(bounds) || bounds[1L] >= bounds[2L])) {
    cli::cli_abort("{.arg bounds} must be `NULL` or an ordered pair of numbers.", class = "margot_error_invalid_input")
  }
  if (!is.logical(policies) || !length(policies) || is.null(names(policies)) ||
      anyNA(policies) || any(!nzchar(names(policies))) || anyDuplicated(names(policies))) {
    cli::cli_abort(
      "{.arg policies} must be a named logical vector with unique, non-empty arm names.",
      class = "margot_error_invalid_input"
    )
  }
  if (!is.null(weight_column) &&
      (!is.character(weight_column) || length(weight_column) != 1L ||
        is.na(weight_column) || !nzchar(weight_column))) {
    cli::cli_abort(
      "{.arg weight_column} must be `NULL` or one column name.",
      class = "margot_error_invalid_input"
    )
  }

  payload <- list(
    estimator = "lmtp::lmtp_sdr",
    call_arguments = list(
      trt = as.character(trt),
      baseline = baseline,
      time_vary = time_vary,
      cens = cens,
      compete = compete,
      outcome = as.character(outcomes),
      outcome_type = outcome_type,
      id = id,
      folds = as.integer(folds),
      bounds = bounds
    ),
    arms = lapply(names(policies), function(arm_id) {
      list(arm_id = arm_id, mtp = isTRUE(policies[[arm_id]]))
    }),
    seed = as.integer(seed),
    trim = trim,
    learner_profile = learner_profile,
    weight_column = weight_column
  )
  content_hash <- margot_lmtp_spec_hash(margot_lmtp_estimator_spec_version, payload)
  structure(
    list(
      schema_version = margot_lmtp_estimator_spec_version,
      payload = payload,
      content_hash = content_hash
    ),
    class = c("margot_lmtp_estimator_spec", "list")
  )
}

# return the SuperLearner library named by a Margot estimator profile
margot_lmtp_spec_learners <- function(profile, call = rlang::caller_env()) {
  switch(profile,
    ensemble = c("SL.mean", "SL.ranger", "SL.xgboost", "SL.glmnet"),
    glm = "SL.glm",
    cli::cli_abort(
      c(
        "No {.pkg lmtp} learner library is registered for profile {.val {profile}}.",
        "i" = "The registered profiles are {.val ensemble} and {.val glm}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  )
}

# re-verify a Margot estimator specification before reading its payload
margot_lmtp_spec_verify <- function(estimator_spec, call = rlang::caller_env()) {
  if (!inherits(estimator_spec, "margot_lmtp_estimator_spec")) {
    cli::cli_abort(
      c(
        "{.arg estimator_spec} must come from {.fn margot_lmtp_estimator_spec}.",
        "x" = "Received an object of class {.cls {class(estimator_spec)[1]}}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  if (!identical(estimator_spec$schema_version, margot_lmtp_estimator_spec_version)) {
    cli::cli_abort(
      c(
        "The estimator specification uses an unsupported schema.",
        "x" = "Received {.val {estimator_spec$schema_version}}; expected {.val {margot_lmtp_estimator_spec_version}}.",
        "i" = "Rebuild it with {.fn margot_lmtp_estimator_spec}."
      ),
      class = "margot_error_lmtp_conformity",
      call = call
    )
  }
  realised <- margot_lmtp_spec_hash(estimator_spec$schema_version, estimator_spec$payload)
  if (!identical(realised, estimator_spec$content_hash)) {
    cli::cli_abort(
      c(
        "{.arg estimator_spec} does not match its content hash.",
        "i" = "The specification has been altered since it was created."
      ),
      class = "margot_error_hash_mismatch",
      call = call
    )
  }
  invisible(estimator_spec)
}

# report caller arguments that conflict with a locked estimator specification
margot_lmtp_spec_conflict <- function(conflicts = character(),
                                      discarded = character(),
                                      call = rlang::caller_env()) {
  bullets <- character()
  if (length(conflicts)) {
    bullets <- c(bullets, "x" = paste0(
      "Already fixed by the specification: ",
      paste(paste0("`", conflicts, "`"), collapse = ", "), "."
    ))
  }
  if (length(discarded)) {
    bullets <- c(bullets, "x" = paste0(
      "Named in `lmtp_defaults` and not part of the locked call: ",
      paste(paste0("`", discarded, "`"), collapse = ", "), "."
    ))
  }
  cli::cli_abort(
    c(
      "The estimator specification builds the whole `lmtp` call.",
      bullets,
      "i" = paste(
        "Drop the conflicting arguments and let `estimator_spec` supply them,",
        "or rebuild the specification with `margot_lmtp_estimator_spec()`."
      )
    ),
    class = "margot_error_estimator_spec_conflict",
    call = call
  )
}

# derive margot_lmtp arguments from a verified Margot estimator specification
margot_lmtp_args_from_spec <- function(estimator_spec,
                                       trt,
                                       outcome_vars,
                                       lmtp_defaults,
                                       lmtp_model_type,
                                       seed,
                                       shift_functions,
                                       supplied = character(),
                                       data = NULL,
                                       call = rlang::caller_env()) {
  margot_lmtp_spec_verify(estimator_spec, call = call)
  payload <- estimator_spec$payload
  arguments <- payload$call_arguments
  if (!is.list(arguments) || is.null(arguments$trt) || is.null(arguments$outcome)) {
    cli::cli_abort(
      c(
        "The estimator specification carries no usable {.field call_arguments}.",
        "x" = "{.field trt} and {.field outcome} must both be present."
      ),
      class = "margot_error_lmtp_conformity",
      call = call
    )
  }

  locked_defaults <- c(
    "baseline", "time_vary", "cens", "compete", "outcome_type", "id",
    "folds", "bounds", "learners_trt", "learners_outcome", "control",
    "mtp", "shift", "outcome", "trt", "weights"
  )
  conflicts <- character()
  if ("trt" %in% supplied && !identical(as.character(trt), as.character(arguments$trt))) {
    conflicts <- c(conflicts, "trt")
  }
  if ("outcome_vars" %in% supplied &&
      !identical(as.character(outcome_vars), as.character(arguments$outcome))) {
    conflicts <- c(conflicts, "outcome_vars")
  }
  supplied_defaults <- names(lmtp_defaults) %||% character()
  supplied_defaults <- supplied_defaults[nzchar(supplied_defaults)]
  if (length(lmtp_defaults) > length(supplied_defaults)) {
    supplied_defaults <- c(supplied_defaults, "<unnamed>")
  }
  conflicts <- c(conflicts, intersect(locked_defaults, supplied_defaults))
  discarded <- setdiff(supplied_defaults, locked_defaults)
  if ("lmtp_model_type" %in% supplied &&
      !identical(lmtp_model_type, lmtp::lmtp_sdr)) {
    conflicts <- c(conflicts, "lmtp_model_type")
  }
  if ("seed" %in% supplied && !identical(as.integer(seed), as.integer(payload$seed))) {
    conflicts <- c(conflicts, "seed")
  }
  arm_ids <- vapply(payload$arms, function(a) as.character(a$arm_id), character(1))
  if ("include_null_shift" %in% supplied && !("null" %in% arm_ids)) {
    conflicts <- c(conflicts, "include_null_shift")
  }
  if (length(conflicts) || length(discarded)) {
    margot_lmtp_spec_conflict(unique(conflicts), unique(discarded), call = call)
  }
  if (!length(shift_functions) || !setequal(names(shift_functions), arm_ids)) {
    cli::cli_abort(
      c(
        "{.arg shift_functions} must name exactly the arms the specification locks.",
        "x" = paste0("Locked arms: ", paste(arm_ids, collapse = ", "), "."),
        "x" = paste0(
          "Supplied: ",
          if (length(names(shift_functions))) paste(names(shift_functions), collapse = ", ") else "<none>",
          "."
        )
      ),
      class = "margot_error_estimator_spec_conflict",
      call = call
    )
  }

  library <- margot_lmtp_spec_learners(payload$learner_profile, call = call)
  derived <- list(
    baseline = if (length(arguments$baseline)) arguments$baseline else NULL,
    time_vary = if (length(arguments$time_vary)) arguments$time_vary else NULL,
    cens = arguments$cens,
    compete = arguments$compete,
    outcome_type = arguments$outcome_type,
    id = arguments$id,
    folds = arguments$folds,
    bounds = arguments$bounds,
    learners_trt = library,
    learners_outcome = library,
    control = lmtp::lmtp_control(.trim = payload$trim)
  )
  weight_column <- payload$weight_column
  if (!is.null(weight_column)) {
    if (!is.data.frame(data) || !weight_column %in% names(data)) {
      cli::cli_abort(
        "The analysis data carry no locked weight column {.field {weight_column}}.",
        class = "margot_error_invalid_input",
        call = call
      )
    }
    weights <- data[[weight_column]]
    if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) || any(weights < 0)) {
      cli::cli_abort(
        "The locked weight column {.field {weight_column}} must be numeric, complete, finite, and non-negative.",
        class = "margot_error_invalid_input",
        call = call
      )
    }
    derived$weights <- as.numeric(weights)
  }
  derived <- derived[!vapply(derived, is.null, logical(1))]

  mtp_by_arm <- vapply(payload$arms, function(a) isTRUE(a$mtp), logical(1))
  names(mtp_by_arm) <- arm_ids
  list(
    trt = arguments$trt,
    outcome_vars = arguments$outcome,
    lmtp_defaults = derived,
    lmtp_model_type = lmtp::lmtp_sdr,
    seed = as.integer(payload$seed),
    mtp_by_arm = mtp_by_arm
  )
}
