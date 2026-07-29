# bridge from a sealed margot.lmtp estimator contract to the arguments
# margot_lmtp() passes to lmtp. the contract is authoritative: every argument it
# seals is taken from it, and a conflicting user argument errors by name rather
# than being silently overridden.

# the SuperLearner library each sealed learner profile names, in the form lmtp
# takes. read from margot.lmtp rather than mirrored here: a mirrored copy drifts
# silently, and a study's estimation then runs under learners its contract did
# not seal. the caller has already established that margot.lmtp is installed.
margot_lmtp_spec_learners <- function(profile, call = rlang::caller_env()) {
  library <- tryCatch(
    margot.lmtp::lmtp_learner_library(profile),
    error = function(e) e
  )
  if (inherits(library, "condition")) {
    cli::cli_abort(
      c(
        "No {.pkg lmtp} learner library is registered for profile {.val {profile}}.",
        "i" = "The sealed profiles are {.val ensemble} and {.val glm}.",
        "i" = "The mapping comes from {.fn margot.lmtp::lmtp_learner_library}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  library
}

# re-verifies the seal on entry, so a sealed object edited between sealing and use
# is refused here rather than read. the class vector alone proves nothing.
margot_lmtp_spec_verify <- function(estimator_spec, call = rlang::caller_env()) {
  verify <- tryCatch(
    utils::getFromNamespace("margot_lmtp_verify_seal", "margot.lmtp"),
    error = function(e) NULL
  )
  if (is.function(verify)) {
    verify(estimator_spec, "estimator_spec")
  }
  invisible(estimator_spec)
}

# errors naming the arguments the caller supplied that the sealed contract
# already fixes, and the `lmtp_defaults` entries the contract does not fix and
# would otherwise discard in silence
margot_lmtp_spec_conflict <- function(conflicts = character(),
                                      discarded = character(),
                                      call = rlang::caller_env()) {
  bullets <- character()
  if (length(conflicts)) {
    bullets <- c(bullets, "x" = paste0(
      "Already fixed by the seal: ",
      paste(paste0("`", conflicts, "`"), collapse = ", "), "."
    ))
  }
  if (length(discarded)) {
    bullets <- c(bullets, "x" = paste0(
      "Named in `lmtp_defaults` and not part of the sealed call: ",
      paste(paste0("`", discarded, "`"), collapse = ", "), "."
    ))
  }
  cli::cli_abort(
    c(
      "The sealed estimator contract builds the whole `lmtp` call.",
      bullets,
      "i" = paste(
        "Drop them and let `estimator_spec` supply them, or reseal the contract",
        "with `margot.lmtp::margot_lmtp_estimator_spec()`."
      )
    ),
    class = "margot_error_estimator_spec_conflict",
    call = call
  )
}

#' Derive `margot_lmtp()` arguments from a sealed estimator contract
#'
#' Reads the `call_arguments` payload of a sealed `margot_lmtp_estimator_spec`
#' and returns the treatment, outcome, `lmtp_defaults`, seed, and per-arm `mtp`
#' settings the contract fixes. Every argument the caller supplied that the
#' contract already fixes is reported as a conflict rather than overridden.
#'
#' @param estimator_spec A sealed `margot_lmtp_estimator_spec` object.
#' @param trt,outcome_vars,lmtp_defaults,lmtp_model_type,seed,shift_functions
#'   The arguments as `margot_lmtp()` received them.
#' @param supplied Character vector naming which of those arguments the caller
#'   supplied explicitly.
#' @param call The calling environment, for the error condition.
#'
#' @return A list with `trt`, `outcome_vars`, `lmtp_defaults`,
#'   `lmtp_model_type`, `seed`, and `mtp_by_arm`.
#' @keywords internal
#' @noRd
margot_lmtp_args_from_spec <- function(estimator_spec,
                                       trt,
                                       outcome_vars,
                                       lmtp_defaults,
                                       lmtp_model_type,
                                       seed,
                                       shift_functions,
                                       supplied = character(),
                                       call = rlang::caller_env()) {
  if (!has_margot_lmtp()) {
    cli::cli_abort(
      c(
        "Package {.pkg margot.lmtp} is required to run {.fn margot_lmtp} from a sealed estimator contract.",
        "i" = "Install it with: {.code pak::pak('go-bayes/margot.lmtp')}",
        "i" = "{.pkg margot} suggests {.pkg margot.lmtp}; the exploratory driver runs without it."
      ),
      class = "margot_error_missing_dependency",
      call = call
    )
  }
  if (!inherits(estimator_spec, "margot_lmtp_estimator_spec")) {
    cli::cli_abort(
      c(
        "{.arg estimator_spec} must be a sealed {.cls margot_lmtp_estimator_spec} object.",
        "x" = "Received an object of class {.cls {class(estimator_spec)[1]}}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  margot_lmtp_spec_verify(estimator_spec, call = call)
  payload <- estimator_spec$payload
  arguments <- payload$call_arguments
  if (!is.list(arguments) || is.null(arguments$trt) || is.null(arguments$outcome)) {
    cli::cli_abort(
      c(
        "The sealed contract carries no usable {.field call_arguments}.",
        "x" = "{.field trt} and {.field outcome} must both be present."
      ),
      class = "margot_error_lmtp_conformity",
      call = call
    )
  }

  # every argument below is fixed by the seal; naming one again is a conflict
  sealed_defaults <- c(
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
  # the derived `lmtp_defaults` list is built from the seal alone, so every entry
  # the caller names is either one the seal already fixes or one the derived list
  # would drop on the floor. neither may pass in silence.
  supplied_defaults <- names(lmtp_defaults) %||% character()
  supplied_defaults <- supplied_defaults[nzchar(supplied_defaults)]
  if (length(lmtp_defaults) > length(supplied_defaults)) {
    supplied_defaults <- c(supplied_defaults, "<unnamed>")
  }
  conflicts <- c(conflicts, intersect(sealed_defaults, supplied_defaults))
  discarded <- setdiff(supplied_defaults, sealed_defaults)
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
        "`shift_functions` must name exactly the arms the contract seals.",
        "x" = paste0("Sealed arms: ", paste(arm_ids, collapse = ", "), "."),
        "x" = paste0(
          "Supplied: ",
          if (length(names(shift_functions))) paste(names(shift_functions), collapse = ", ") else "<none>",
          "."
        ),
        "i" = "The seal fingerprints the policies by source; it does not carry the closures."
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
