# record the warnings raised while an lmtp model is fitted.
#
# With lean fits (`.return_full_fits = FALSE`, lmtp's default) each SuperLearner
# keeps its coefficients alone, so `errorsInCVLibrary` is lost and the warning
# SuperLearner raises when it drops a failing learner is the only trace that the
# learner was removed. These helpers collect those warnings without muffling
# them: every warning still reaches the caller as before. lmtp fits its folds
# inside `future::future()`, and future re-signals a worker's warnings when
# `future::value()` collects the fold, so a calling handler in the process that
# collects the value sees them.

# evaluate `expr`, collecting the message of every warning it raises. Nothing is
# muffled, and no random numbers are consumed.
margot_lmtp_capture_warnings <- function(expr) {
  captured <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      captured <<- c(captured, conditionMessage(w))
    }
  )
  list(value = value, warnings = captured)
}

# name each warning by the stage that raised it: "density" for the shared
# treatment-and-censoring stage, "outcome" for an outcome regression, and "fit"
# for a single lmtp call that fits both
margot_lmtp_stage_warnings <- function(warnings, stage) {
  warnings <- as.character(warnings %||% character())
  stats::setNames(warnings, rep(stage, length(warnings)))
}

# the learners SuperLearner reports removing, named by stage; one entry per
# learner and stage
margot_lmtp_dropped_learners <- function(warnings) {
  warnings <- warnings %||% character()
  if (length(warnings) == 0L) {
    return(stats::setNames(character(), character()))
  }
  stages <- names(warnings) %||% rep("fit", length(warnings))
  pattern <- "(?s)Error in algorithm\\s+(\\S+).*removed from the Super Learner"
  hits <- regmatches(warnings, regexec(pattern, warnings, perl = TRUE))
  learners <- vapply(
    hits,
    function(hit) if (length(hit) >= 2L) hit[[2L]] else NA_character_,
    character(1)
  )
  keep <- !is.na(learners)
  dropped <- stats::setNames(learners[keep], stages[keep])
  dropped[!duplicated(paste(names(dropped), dropped, sep = "\r"))]
}

# attach the captured warnings and the dropped learners to one fitted model
margot_lmtp_record_warnings <- function(model, warnings) {
  warnings <- warnings %||% stats::setNames(character(), character())
  model$warnings <- warnings
  model$dropped_learners <- margot_lmtp_dropped_learners(warnings)
  model
}

# one alert per model that lost a learner
margot_lmtp_alert_dropped_learners <- function(model, label) {
  dropped <- model$dropped_learners
  if (length(dropped) == 0L) {
    return(invisible(FALSE))
  }
  described <- paste0(unname(dropped), " (", names(dropped), ")")
  cli::cli_alert_warning(
    "{label}: SuperLearner dropped {.val {described}} after {?an error/errors}; see {.code $warnings} and {.code $dropped_learners} on the model."
  )
  invisible(TRUE)
}
