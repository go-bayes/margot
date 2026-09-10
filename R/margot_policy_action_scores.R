#' Construct binary action scores from independent nuisance predictions
#'
#' @description
#' Computes unweighted doubly robust control and treatment scores. The caller
#' supplies predictions estimated without the scored observation's outcome;
#' evaluation predictions must respect the policy-development boundary.
#'
#' @param outcome Numeric outcome vector, already on the intended oriented scale.
#' @param treatment Binary numeric vector, zero for control and one for treatment.
#' @param outcome_mean Predicted conditional outcome mean under the observed
#'   exposure distribution.
#' @param propensity Predicted treatment probabilities strictly between zero and
#'   one. No truncation or clipping is performed.
#' @param treatment_effect Predicted conditional treatment-minus-control effect.
#'
#' @details
#' Conditional action means are recovered as `outcome_mean - propensity *
#' treatment_effect` for control and `outcome_mean + (1 - propensity) *
#' treatment_effect` for treatment. The observed action's residual is corrected
#' by its inverse propensity. Correct interpretation requires the causal
#' identification and nuisance-estimation conditions for the supplied design.
#' This function verifies numerical inputs, not independence or those conditions.
#' It applies neither analysis weights nor a benefit threshold. Apply each once
#' in the subsequent learning and evaluation procedure.
#'
#' @return A two-column numeric matrix named `control` and `treated` on the
#'   supplied outcome scale.
#' @export
margot_policy_action_scores <- function(outcome, treatment, outcome_mean,
                                        propensity, treatment_effect) {
  # convert aligned outcome, treatment and nuisance predictions to action scores.
  values <- list(outcome = outcome, treatment = treatment,
    outcome_mean = outcome_mean, propensity = propensity,
    treatment_effect = treatment_effect)
  n <- length(outcome)
  if (!n) stop("outcome must contain observations.", call. = FALSE)
  for (name in names(values)) {
    x <- values[[name]]
    if (!is.numeric(x) || !is.null(dim(x)) || length(x) != n ||
        any(!is.finite(x))) {
      stop(name, " must be a finite numeric vector aligned with outcome.", call. = FALSE)
    }
  }
  if (any(!treatment %in% c(0, 1))) {
    stop("treatment must contain only zero and one.", call. = FALSE)
  }
  if (any(propensity <= 0 | propensity >= 1)) {
    stop("propensity must be strictly between zero and one; no clipping is performed.", call. = FALSE)
  }
  mu0 <- outcome_mean - propensity * treatment_effect
  mu1 <- outcome_mean + (1 - propensity) * treatment_effect
  scores <- cbind(control = mu0 + (1 - treatment) * (outcome - mu0) / (1 - propensity),
    treated = mu1 + treatment * (outcome - mu1) / propensity)
  if (any(!is.finite(scores))) stop("Action scores are non-finite.", call. = FALSE)
  scores
}

#' Fit development-only forest nuisances for policy action scores
#'
#' @description
#' Fits outcome, exposure and causal forests on development participants. Uses
#' out-of-bag predictions for development scores and development-trained
#' predictions for evaluation scores. Evaluation outcomes enter only the latter
#' residual corrections. Preparation and supplied weights remain the caller's
#' declared inputs.
#'
#' @param development_X,evaluation_X Numeric covariate matrices with identical
#'   named columns, already prepared on compatible scales.
#' @param development_Y,evaluation_Y Oriented outcome vectors.
#' @param development_W,evaluation_W Binary exposure vectors.
#' @param development_weights Positive development analysis weights, or `NULL`
#'   for equal weights. Evaluation weights enter the later policy evaluator.
#' @param forest_args Named list of causal-forest settings. Settings also
#'   accepted by `grf::regression_forest` are used for the nuisance forests.
#'   Data, nuisance predictions, seeds and thread counts cannot be overridden.
#' @param seed Integer seed. The three forests use this seed plus zero, one and
#'   two respectively.
#' @param num_threads Positive integer native thread count, default one.
#' @param save_models Whether to retain the three fitted forests.
#'
#' @return A list of development and evaluation action scores and predictions,
#'   requested forest settings, per-forest seeds and a preparation qualification.
#'   Optionally includes the development-trained forests.
#' @details
#' This supplies a nuisance-estimation boundary, rather than verifying causal
#' identification or removing bias in the supplied preparation or analysis
#' weights. Pointwise policy-score intervals require appropriate nuisance rates
#' and sampling assumptions. Propensities at zero or one cause an error; the
#' function does not select a clipping rule from the results.
#' @export
margot_policy_development_scores <- function(development_X, development_Y,
                                              development_W, evaluation_X,
                                              evaluation_Y, evaluation_W,
                                              development_weights = NULL,
                                              forest_args = list(num.trees = 2000),
                                              seed = 42L, num_threads = 1L,
                                              save_models = FALSE) {
  # fit development nuisance models and create aligned original action scores.
  if (!requireNamespace("grf", quietly = TRUE)) stop("grf is required.", call. = FALSE)
  if (!is.matrix(development_X) || !is.numeric(development_X) ||
      !is.matrix(evaluation_X) || !is.numeric(evaluation_X) ||
      !nrow(development_X) || !nrow(evaluation_X) || !ncol(development_X) ||
      is.null(colnames(development_X)) || anyNA(colnames(development_X)) ||
      any(!nzchar(colnames(development_X))) || anyDuplicated(colnames(development_X)) ||
      !identical(colnames(development_X), colnames(evaluation_X)) ||
      any(!is.finite(development_X)) || any(!is.finite(evaluation_X))) {
    stop("Covariates must be finite numeric matrices with identical unique named columns.", call. = FALSE)
  }
  for (group in c("development", "evaluation")) {
    y <- get(paste0(group, "_Y"))
    w <- get(paste0(group, "_W"))
    n <- nrow(get(paste0(group, "_X")))
    if (!is.numeric(y) || length(y) != n || !is.null(dim(y)) || any(!is.finite(y)) ||
        !is.numeric(w) || length(w) != n || !is.null(dim(w)) || any(!w %in% c(0, 1))) {
      stop(group, " outcome and exposure must be aligned finite numeric vectors, exposure binary.", call. = FALSE)
    }
  }
  if (length(unique(development_W)) != 2L) stop("Development requires both exposure states.", call. = FALSE)
  if (!is.null(development_weights) && (!is.numeric(development_weights) ||
      !is.null(dim(development_weights)) || length(development_weights) != nrow(development_X) || any(!is.finite(development_weights)) ||
      any(development_weights <= 0) || !is.finite(sum(development_weights)))) {
    stop("development_weights must be finite positive aligned weights.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed != floor(seed) || seed > .Machine$integer.max - 2L) stop("seed must be a valid integer seed.", call. = FALSE)
  if (!is.numeric(num_threads) || length(num_threads) != 1L || !is.finite(num_threads) ||
      num_threads < 1 || num_threads > .Machine$integer.max ||
      num_threads != floor(num_threads)) stop("num_threads must be a positive integer.", call. = FALSE)
  if (!is.logical(save_models) || length(save_models) != 1L || is.na(save_models)) stop("save_models must be TRUE or FALSE.", call. = FALSE)
  forbidden <- c("X", "Y", "W", "Y.hat", "W.hat", "sample.weights", "seed", "num.threads", "clusters", "equalize.cluster.weights")
  if (!is.list(forest_args) || (length(forest_args) && (is.null(names(forest_args)) ||
      anyNA(names(forest_args)) || any(!nzchar(names(forest_args))) || anyDuplicated(names(forest_args)))) ||
      any(names(forest_args) %in% forbidden) ||
      any(!names(forest_args) %in% names(formals(grf::causal_forest)))) {
    stop("forest_args must contain unique supported settings, excluding data, nuisance predictions, clusters, seeds and threads.", call. = FALSE)
  }
  nuisance_args <- forest_args[intersect(names(forest_args), names(formals(grf::regression_forest)))]
  base <- list(X = development_X, sample.weights = development_weights,
    num.threads = as.integer(num_threads))
  outcome_forest <- do.call(grf::regression_forest,
    c(base, list(Y = development_Y, seed = as.integer(seed)), nuisance_args))
  exposure_forest <- do.call(grf::regression_forest,
    c(base, list(Y = development_W, seed = as.integer(seed + 1L)), nuisance_args))
  m_dev <- as.numeric(predict(outcome_forest)$predictions)
  e_dev <- as.numeric(predict(exposure_forest)$predictions)
  causal_forest <- do.call(grf::causal_forest, c(base,
    list(Y = development_Y, W = development_W, Y.hat = m_dev,
      W.hat = e_dev, seed = as.integer(seed + 2L)), forest_args))
  tau_dev <- as.numeric(predict(causal_forest)$predictions)
  m_eval <- as.numeric(predict(outcome_forest, evaluation_X)$predictions)
  e_eval <- as.numeric(predict(exposure_forest, evaluation_X)$predictions)
  tau_eval <- as.numeric(predict(causal_forest, evaluation_X)$predictions)
  out <- list(
    development_scores = margot_policy_action_scores(development_Y, development_W, m_dev, e_dev, tau_dev),
    evaluation_scores = margot_policy_action_scores(evaluation_Y, evaluation_W, m_eval, e_eval, tau_eval),
    development_predictions = list(outcome_mean = m_dev, propensity = e_dev, treatment_effect = tau_dev),
    evaluation_predictions = list(outcome_mean = m_eval, propensity = e_eval, treatment_effect = tau_eval),
    metadata = list(seed = seed, forest_seeds = c(outcome = seed, exposure = seed + 1L, causal = seed + 2L),
      num_threads = num_threads, forest_args = forest_args,
      nuisance_args = nuisance_args, evaluation_outcomes_used_for_learning = FALSE,
      qualification = "Development-only nuisance fitting; conditional on supplied preparation and weights; causal identification and nuisance-rate assumptions require justification."))
  if (save_models) out$models <- list(outcome = outcome_forest, exposure = exposure_forest, causal = causal_forest)
  out
}
