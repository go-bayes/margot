# learner audit and lean reduction for lmtp models.

# the numbers an audit keeps from one fitted SuperLearner ensemble.
margot_lmtp_lean_sl <- function(fit) {
  stopifnot(inherits(fit, "SuperLearner"))
  structure(list(
    library = names(fit$coef),
    coefficient = unname(fit$coef),
    cv_risk = unname(fit$cvRisk),
    error = unname(fit$errorsInLibrary),
    cv_error = unname(fit$errorsInCVLibrary),
    inner_folds = length(fit$validRows),
    inner_fold_sizes = unname(lengths(fit$validRows))
  ), class = "margot_lmtp_lean_fit")
}

#' Record the fitted learners of an lmtp model
#'
#' Reads every SuperLearner ensemble held in an lmtp model fitted with
#' `lmtp::lmtp_control(.return_full_fits = TRUE)`: the outcome regressions
#' (`fits_m`) and the density-ratio classifiers (`fits_r`), for each outer fold
#' and time point (lmtp 1.5.4 nests fits by fold, then time). For each ensemble
#' it records the learner names, ensemble coefficients, cross-validated risks,
#' the two error flags and the realised inner folds. These are the quantities a learner audit reports; the fitted
#' learners themselves are not kept.
#'
#' @param model An lmtp model holding full fits.
#' @return A `margot_lmtp_learner_audit` list with two data frames: `learners`
#'   (one row per stage, time, outer fold and learner) and `inner_folds` (one row
#'   per ensemble, with the number and sizes of its inner validation folds).
#' @seealso [margot_lmtp_reduce_fits()], [margot_lmtp()] with `learner_audit = TRUE`.
#' @export
margot_lmtp_learner_audit <- function(model) {
  stopifnot(inherits(model, "lmtp"))
  stages <- list(outcome = model$fits_m, density = model$fits_r)
  learners <- list()
  inner <- list()
  for (stage in names(stages)) {
    # lmtp 1.5.4 nests fits by outer fold, then by time point.
    for (fold in seq_along(stages[[stage]])) {
      for (time in seq_along(stages[[stage]][[fold]])) {
        fit <- stages[[stage]][[fold]][[time]]
        if (is.null(fit)) next
        if (!inherits(fit, "SuperLearner")) {
          stop("The learner audit needs full fits; fit the model with ",
            "lmtp::lmtp_control(.return_full_fits = TRUE). Found ", class(fit)[1L],
            " at ", stage, " time ", time, " fold ", fold, ".", call. = FALSE)
        }
        record <- margot_lmtp_lean_sl(fit)
        learners[[length(learners) + 1L]] <- data.frame(stage = stage, time = time, fold = fold,
          learner = record$library, coefficient = record$coefficient, cv_risk = record$cv_risk,
          error = record$error, cv_error = record$cv_error, stringsAsFactors = FALSE)
        inner[[length(inner) + 1L]] <- data.frame(stage = stage, time = time, fold = fold,
          inner_folds = record$inner_folds,
          inner_fold_sizes = paste(record$inner_fold_sizes, collapse = ","), stringsAsFactors = FALSE)
      }
    }
  }
  structure(list(learners = do.call(rbind, learners), inner_folds = do.call(rbind, inner)),
    class = "margot_lmtp_learner_audit")
}

#' Reduce an lmtp model's fitted learners to ensemble weights
#'
#' Replaces each SuperLearner ensemble in `fits_m` and `fits_r` by its ensemble
#' coefficients, which is what lmtp 1.5.4 stores when a model is fitted with the
#' default `.return_full_fits = FALSE`. Estimates, influence functions, density
#' ratios and every other component are unchanged, so the reduced model equals
#' the model lmtp returns under its default setting.
#'
#' @param model An lmtp model.
#' @return The model with learner weights in place of fitted learners.
#' @export
margot_lmtp_reduce_fits <- function(model) {
  stopifnot(inherits(model, "lmtp"))
  reduce <- function(fits) {
    lapply(fits, function(fold) lapply(fold, function(fit) {
      if (inherits(fit, "SuperLearner")) fit$coef else fit
    }))
  }
  if (!is.null(model$fits_m)) model$fits_m <- reduce(model$fits_m)
  if (!is.null(model$fits_r)) model$fits_r <- reduce(model$fits_r)
  model
}

# audit a model fitted with full fits, then return its lean form with the audit attached.
margot_lmtp_audit_model <- function(model) {
  audit <- margot_lmtp_learner_audit(model)
  model <- margot_lmtp_reduce_fits(model)
  model$learner_audit <- audit
  model
}
