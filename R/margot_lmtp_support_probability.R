#' Fit one binary probability model for a support assessment
#'
#' Fit a SuperLearner ensemble on an explicitly supplied training set and
#' predict an explicitly supplied validation set. Use this for each conditional
#' exposure regression and each observation regression in the study design.
#'
#' @param y Binary training responses.
#' @param x,new_x Numeric data frames with identical columns for training and
#'   validation. Only variables available before the relevant event belong here.
#' @param ids Unique training participant identifiers, passed to SuperLearner.
#' @param learners Ordered SuperLearner library selected by the study.
#' @param inner_folds Number of inner cross-validation folds, at least two.
#' @param seed Explicit integer seed used before fitting a nonconstant response.
#' @param method SuperLearner combination method; defaults to "method.NNLS".
#' @return A list with state (fitted, constant, or unassessable),
#'   ensemble probability, learner_probability, a compact fit record,
#'   warnings, reason and seed. The compact record retains coefficients,
#'   cross-validated risks, error flags and fold sizes via the same helper as
#'   [margot_lmtp_exposure_ratio()]. Fitted learner objects are not returned.
#' @details Empty and one-class training responses return constant predictions.
#'   An empty conditional set is usable only when its parent probability is
#'   zero; the calling conditional-law calculation must check this condition.
#'   Failed learners and invalid predictions give an unassessable result.
#'   Probabilities within 1e-12 outside [0, 1] are snapped to the nearest bound;
#'   larger departures remain invalid. Warnings are recorded.
#'   The caller owns outer folds, checkpoints and worker/thread settings.
#'   This function sets the seed before a nonconstant fit and uses no future
#'   plan of its own. It does not inspect terminal outcomes or select a schedule.
#' @examples
#' margot_lmtp_support_probability(rep(1, 4), data.frame(x = 1:4),
#'   data.frame(x = 5:6), letters[1:4], "SL.mean", 2L, 42L)
#' @export
margot_lmtp_support_probability <- function(y, x, new_x, ids, learners,
                                          inner_folds, seed, method = "method.NNLS") {
  stopifnot(is.numeric(y) || is.logical(y), all(y %in% 0:1),
    is.data.frame(x), is.data.frame(new_x), nrow(x) == length(y),
    ncol(x) > 0L, identical(names(x), names(new_x)), !anyDuplicated(names(x)),
    all(vapply(x, is.numeric, logical(1))), all(vapply(new_x, is.numeric, logical(1))),
    all(is.finite(as.matrix(x))), all(is.finite(as.matrix(new_x))),
    length(ids) == length(y), !anyNA(ids), !anyDuplicated(ids),
    is.character(learners), length(learners) > 0L, !anyNA(learners),
    all(nzchar(learners)), !anyDuplicated(learners),
    is.numeric(inner_folds), length(inner_folds) == 1L, is.finite(inner_folds),
    inner_folds >= 2L, inner_folds == floor(inner_folds),
    is.numeric(seed), length(seed) == 1L, is.finite(seed), seed >= 0,
    seed <= .Machine$integer.max, seed == floor(seed),
    is.character(method), length(method) == 1L, !is.na(method), nzchar(method))
  settings <- list(learners = learners, inner_folds = inner_folds, method = method)
  warnings <- character()
  run <- function() {
    if (!length(y)) return(list(state = "constant", probability = rep(0, nrow(new_x)),
      learner_probability = matrix(0, nrow(new_x), length(settings$learners)),
      fit = NULL, reason = "empty conditional training set; parent probability must be zero"))
    if (length(unique(y)) == 1L) return(list(state = "constant",
      probability = rep(y[1L], nrow(new_x)),
      learner_probability = matrix(y[1L], nrow(new_x), length(settings$learners)),
      fit = NULL, reason = "one observed class in the conditional training set"))
    if (length(y) < settings$inner_folds) stop("Too few conditional training records for the requested inner folds.")
    set.seed(seed)
    fit <- SuperLearner::SuperLearner(Y = y, X = x, newX = new_x,
      family = stats::binomial(), SL.library = settings$learners,
      method = settings$method, id = ids,
      cvControl = list(V = settings$inner_folds),
      control = list(saveFitLibrary = TRUE),
      env = environment(SuperLearner::SuperLearner))
    failed <- any(unlist(fit$errorsInCVLibrary)) || any(unlist(fit$errorsInLibrary))
    p <- margot_lmtp_support_snap_probability(as.numeric(fit$SL.predict))
    library_p <- margot_lmtp_support_snap_probability(as.matrix(fit$library.predict))
    valid <- length(p) == nrow(new_x) && all(is.finite(p)) && all(p >= 0 & p <= 1) &&
      ncol(library_p) == length(settings$learners) && all(is.finite(library_p)) &&
      all(library_p >= 0 & library_p <= 1)
    list(state = if (!failed && valid) "fitted" else "unassessable",
      probability = p, learner_probability = library_p, fit = margot_lmtp_lean_sl(fit),
      reason = if (!failed && valid) NULL else "A requested learner failed or returned invalid probabilities.")
  }
  result <- tryCatch(withCallingHandlers(run(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) list(state = "unassessable", fit = NULL,
    probability = NULL, learner_probability = NULL, reason = conditionMessage(e)))
  result$warnings <- warnings
  result$seed <- seed
  result
}


margot_lmtp_support_snap_probability <- function(x) {
  x[!is.na(x) & x < 0 & x >= -1e-12] <- 0
  x[!is.na(x) & x > 1 & x <= 1 + 1e-12] <- 1
  x
}

