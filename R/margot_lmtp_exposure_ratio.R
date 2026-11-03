#' Cross-fitted exposure-only density ratios
#'
#' Estimate the ratio of the shifted exposure law to the natural exposure law
#' by classifying equally weighted copies of each observed record. Each copy
#' has the same history; its exposure is either observed or policy-shifted.
#' Observation indicators and outcomes must be excluded from `history`.
#' Both copies remain together in outer and inner validation, including the
#' penalty-selection folds used by the default lasso learner.
#'
#' No probability floor, ratio cap or observation probability is applied.
#' A probability of one stops estimation unless a known structural zero is
#' supplied for that evaluation record. The caller must justify structural
#' zeros from the policy's image, rather than from fitted probabilities.
#'
#' @param data Data frame containing the exposure and pre-exposure history.
#' @param exposure Name of one numeric exposure column.
#' @param history Unique names of pre-exposure predictors, excluding exposure.
#' @param shifted Numeric vector of policy-shifted exposures in input row order.
#' @param id Unique, nonmissing participant identifiers in input row order.
#' @param fold_id Integer outer-fold assignments, numbered consecutively from one.
#' @param reached Logical vector identifying records observed at this assignment.
#' @param learners SuperLearner library, in order.
#' @param inner_folds Number of participant-grouped inner validation folds.
#'   The default lasso learner retains ten separate penalty-selection folds.
#' @param structural_zero Logical vector identifying observed exposure values
#'   outside the policy image. Defaults to no known structural zeros.
#' @param retain_fits Logical. When `FALSE`, each outer fold's fitted
#'   SuperLearner is replaced, inside its worker, by a small record of learner
#'   names, ensemble coefficients, cross-validated risks, error flags and inner
#'   folds, so fitted learners never reach the calling process. Ratios,
#'   probabilities, validation membership and warnings are unchanged. Default
#'   `TRUE` keeps the earlier behaviour.
#' @return A list containing uncapped `ratio`, classifier `probability`, complete
#'   outer-fold `fits`, input row identities and the requested learner settings.
#'   Unreached records have missing ratios and probabilities. Fits retain models,
#'   validation membership, warnings and worker process identifiers.
#' @details Configure the future plan, one thread per learner and the RNG before
#'   calling. The function uses `future.seed = TRUE`; it does not change the plan.
#'   Each outer fold is one future. It does not select an intervention schedule,
#'   estimate an outcome or establish adequate support.
#' @references Diaz et al. (2023), Nonparametric causal effects based on
#'   longitudinal modified treatment policies, Section 5.4.
#' @export
margot_lmtp_exposure_ratio <- function(data, exposure, history, shifted, id,
    fold_id, reached, learners = c("SL.mean", "SL.glmnet", "SL.ranger", "SL.xgboost"),
    inner_folds = 10L, structural_zero = rep(FALSE, nrow(data)), retain_fits = TRUE) {
  n <- nrow(data)
  stopifnot(is.data.frame(data), n > 0L, is.character(exposure), length(exposure) == 1L,
    is.character(history), !anyNA(history), !anyDuplicated(history),
    !exposure %in% history, all(c(exposure, history) %in% names(data)),
    length(id) == n, !anyNA(id), !anyDuplicated(id),
    is.integer(fold_id), length(fold_id) == n, !anyNA(fold_id),
    is.logical(reached), length(reached) == n, !anyNA(reached), any(reached),
    is.numeric(data[[exposure]]), is.numeric(shifted), length(shifted) == n,
    is.logical(structural_zero), length(structural_zero) == n, !anyNA(structural_zero),
    is.character(learners), length(learners) > 0L, !anyNA(learners), !anyDuplicated(learners),
    length(inner_folds) == 1L, is.finite(inner_folds), inner_folds >= 2L,
    inner_folds == as.integer(inner_folds), isTRUE(retain_fits) || isFALSE(retain_fits))
  folds <- sort(unique(fold_id))
  stopifnot(length(folds) >= 2L, identical(folds, seq_along(folds)),
    all(is.finite(data[[exposure]][reached])), all(is.finite(shifted[reached])),
    all(stats::complete.cases(data[reached, history, drop = FALSE])))
  for (name in history) {
    if (is.numeric(data[[name]]) && any(!is.finite(data[[name]][reached]))) {
      stop("Nonfinite history: ", name, call. = FALSE)
    }
  }
  # reject insufficient participant counts before fitting any fold.
  minimum_training <- max(inner_folds, if ("SL.glmnet" %in% learners) 10L else 2L)
  for (k in folds) {
    count <- sum(reached & fold_id != k)
    if (!any(reached & fold_id == k) || count - ceiling(count / inner_folds) < minimum_training) {
      stop("Insufficient reached participants for grouped inner validation in outer fold ", k,
        call. = FALSE)
    }
  }
  fits <- future.apply::future_lapply(folds, function(k) {
    train <- which(reached & fold_id != k)
    valid <- which(reached & fold_id == k)
    natural <- data[train, c(history, exposure), drop = FALSE]
    policy <- natural
    policy[[exposure]] <- shifted[train]
    stacked <- rbind(natural, policy)
    stack_ids <- rep(id[train], 2L)
    labels <- rep(c(0L, 1L), each = length(train))
    inner_id_fold <- sample(rep(seq_len(inner_folds), length.out = length(train)))
    stack_fold <- rep(inner_id_fold, 2L)
    valid_rows <- lapply(seq_len(inner_folds), function(j) which(stack_fold == j))
    learner_environment <- new.env(parent = asNamespace("SuperLearner"))
    learner_environment$.lasso_folds <- list()
    # pass grouped penalty folds through the existing lasso learner.
    learner_environment$SL.glmnet <- function(Y, X, newX, family, obsWeights, id,
        alpha = 1, nfolds = 10L, nlambda = 100L, useMin = TRUE,
        loss = "deviance", ...) {
      unique_ids <- unique(id)
      if (length(unique_ids) < nfolds) stop("Insufficient participants for lasso folds.")
      grouped <- sample(rep(seq_len(nfolds), length.out = length(unique_ids)))
      learner_environment$.lasso_folds[[length(learner_environment$.lasso_folds) + 1L]] <-
        data.frame(id = id, fold = grouped[match(id, unique_ids)])
      result <- SuperLearner::SL.glmnet(Y = Y, X = X, newX = newX, family = family,
        obsWeights = obsWeights, id = id, alpha = alpha, nfolds = nfolds,
        nlambda = nlambda, useMin = useMin, loss = loss,
        foldid = grouped[match(id, unique_ids)], ...)
      result$fit$participant_folds <- data.frame(id = unique_ids, fold = grouped)
      result
    }
    warnings <- character()
    fit <- withCallingHandlers(SuperLearner::SuperLearner(Y = labels, X = stacked,
      newX = data[valid, c(history, exposure), drop = FALSE], family = stats::binomial(),
      SL.library = learners, method = "method.NNLS", id = stack_ids,
      cvControl = list(V = inner_folds, validRows = valid_rows),
      env = learner_environment), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (any(fit$errorsInLibrary) || any(fit$errorsInCVLibrary) || any(!is.finite(fit$cvRisk))) {
      stop("Exposure classifier learner failed in outer fold ", k, ": ",
        paste(unique(warnings), collapse = " | "), call. = FALSE)
    }
    p <- as.numeric(fit$SL.predict)
    ratios <- margot_exposure_odds(p, structural_zero[valid])
    list(model = if (retain_fits) fit else margot_lmtp_lean_sl(fit), probability = p, ratio = ratios,
      training = train, validation = valid, training_ids = id[train],
      validation_ids = id[valid], stack_ids = stack_ids,
      inner_valid_rows = valid_rows, lasso_folds = learner_environment$.lasso_folds,
      warnings = unique(warnings), pid = Sys.getpid())
  }, future.seed = TRUE, future.scheduling = 1)
  ratio <- probability <- rep(NA_real_, n)
  for (fit in fits) {
    ratio[fit$validation] <- fit$ratio
    probability[fit$validation] <- fit$probability
  }
  stopifnot(all(is.finite(ratio[reached])), all(ratio[reached] >= 0))
  list(ratio = ratio, probability = probability, fits = fits, id = id,
    fold_id = fold_id, reached = reached, history = history, exposure = exposure,
    shifted = shifted, structural_zero = structural_zero, learners = learners,
    inner_folds = as.integer(inner_folds), workers = future::nbrOfWorkers())
}

# convert classifier probabilities without smoothing or capping.
margot_exposure_odds <- function(probability, structural_zero) {
  stopifnot(is.numeric(probability), is.logical(structural_zero),
    length(probability) == length(structural_zero), !anyNA(structural_zero))
  if (any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Exposure classifier returned invalid probabilities.", call. = FALSE)
  }
  if (any(probability[!structural_zero] == 1)) {
    stop("Exposure classifier odds are undefined at probability one.", call. = FALSE)
  }
  ratio <- numeric(length(probability))
  keep <- !structural_zero
  ratio[keep] <- probability[keep] / (1 - probability[keep])
  if (any(!is.finite(ratio))) stop("Exposure classifier returned nonfinite odds.", call. = FALSE)
  ratio
}
