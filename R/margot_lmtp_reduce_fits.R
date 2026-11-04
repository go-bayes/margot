# lean reduction for lmtp models.

# the fold-then-time nesting and the coefficient-only lean form are those of the
# validated lmtp release; another release must be checked before reducing fits.
margot_lmtp_check_reduce_version <- function() {
  installed <- as.character(utils::packageVersion("lmtp"))
  if (!identical(installed, margot_lmtp_shared_lmtp_version)) {
    stop("Reducing lmtp fits is validated for lmtp ", margot_lmtp_shared_lmtp_version,
      "; lmtp ", installed, " is installed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Reduce an lmtp model's fitted learners to ensemble weights
#'
#' Replaces each SuperLearner ensemble in `fits_m` and `fits_r` by its ensemble coefficients, which is what lmtp 1.5.4 stores when a model is fitted with the default `.return_full_fits = FALSE`. Estimates, influence functions, density ratios and every other component are unchanged, so the reduced model equals the model lmtp returns under its default setting. The function is validated for lmtp 1.5.4 and stops under another lmtp version.
#'
#' @param model An lmtp model.
#' @return The model with learner weights in place of fitted learners.
#' @export
margot_lmtp_reduce_fits <- function(model) {
  stopifnot(inherits(model, "lmtp"))
  margot_lmtp_check_reduce_version()
  reduce <- function(fits) {
    lapply(fits, function(fold) lapply(fold, function(fit) {
      if (inherits(fit, "SuperLearner")) fit$coef else fit
    }))
  }
  if (!is.null(model$fits_m)) model$fits_m <- reduce(model$fits_m)
  if (!is.null(model$fits_r)) model$fits_r <- reduce(model$fits_r)
  model
}
