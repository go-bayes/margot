#' Causal Contrast Marginal Effects Estimation
#'
#' This function estimates the average treatment effect (ATE) or average treatment effect on the treated (ATT)
#' using generalized linear models (GLMs). It supports handling of continuous and categorical treatments, optional
#' use of spline transformations, and adjustments for multiple imputation datasets.
#'
#' @param df Data frame containing the data.
#' @param Y The response variable in the data frame.
#' @param X The treatment or exposure variable in the data frame.
#' @param baseline_vars A vector of names of baseline covariates to adjust for in the model.
#' @param treat_0 The reference level of the treatment variable, corresponding to no treatment or control condition.
#' @param treat_1 The active level of the treatment variable, corresponding to receiving the treatment.
#' @param estimand A character vector specifying the estimand; "ATE" for Average Treatment Effect or "ATT" for Average Treatment Effect on the Treated.
#' @param type A character vector specifying the type of effect size; "RD" for Risk Difference or "RR" for Risk Ratio.
#' @param nsims Number of simulations to perform, relevant when handling multiple imputation datasets.
#' @param cores Number of cores to use for parallel processing.
#' @param family The family of the GLM to be used (e.g., "gaussian" for linear models).
#' @param weights The name of the weights variable in the data frame, or NULL if no weights are to be used.
#' @param continuous_X Logical indicating whether the treatment variable X is continuous.
#' @param splines Logical indicating whether to use spline transformations for the treatment variable X.
#' @param vcov The method to use for variance-covariance matrix estimation.
#' @param verbose Logical indicating whether to display detailed output during model fitting.
#'
#' @return Depending on the 'type' specified, it returns a summary object containing either risk differences or risk ratios along with additional statistics like confidence intervals.
#'
#' @examples
#' # Assuming df is your dataset with variables 'outcome', 'treatment', 'age', and 'gender'
#' result <- causal_contrast_marginal(df = df, Y = "outcome", X = "treatment",
#'                                    baseline_vars = c("age", "gender"),
#'                                    treat_0 = "control", treat_1 = "exposed",
#'                                    estimand = "ATE", type = "RD", nsims = 100,
#'                                    cores = 2, family = "gaussian", weights = "weight_var",
#'                                    continuous_X = FALSE, splines = FALSE,
#'                                    vcov = "HC3", verbose = TRUE)
#'
#' @importFrom stats glm
#' @importFrom parallel detectCores
#' @importFrom purrr map
#' @import glue
#' @import rlang
#' @export
causal_contrast_marginal <- function(df, Y, X, baseline_vars = "1", treat_0 = treat_0,
                                     treat_1 = treat_1, estimand = c("ATE", "ATT"), type = c("RR", "RD"),
                                     nsims = 200, cores = parallel::detectCores(), family = "gaussian",
                                     weights = NULL, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
                                     verbose = FALSE) {

  # Validate family
  if (is.character(family)) {
    if (!family %in% c("gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi")) {
      stop("Invalid 'family' argument. Please specify a valid family function.")
    }
    family_fun <- get(family, mode = "function", envir = parent.frame())
  } else if (inherits(family, "family")) {
    family_fun <- family
  } else {
    stop("Invalid 'family' argument. Please specify a valid family function or character string.")
  }

  # Build formula
  build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
    baseline_part <- if (length(baseline_vars) > 0 &&
                         !(length(baseline_vars) == 1 && baseline_vars[1] == "1")) {
      paste(baseline_vars, collapse = "+")
    } else {
      "1"  # If baseline_vars is "1" or empty, use "1" to fit just an intercept
    }

    if (continuous_X && splines) {
      return(paste(Y, "~ bs(", X , ")", "*", "(", baseline_part, ")"))
    } else {
      return(paste(Y, "~", X , "*", "(", baseline_part, ")"))
    }
  }


  # Apply model
  weight_var <- if (!is.null(weights) && weights %in% names(df)) df[[weights]] else NULL
  formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
  fit <- glm(as.formula(formula_str), weights = weight_var, family = family_fun, data = df)
  sim_imp <- sim(fit, n = nsims, vcov = vcov)

  # Output processing and return
  sim_estimand <- sim_ame(sim_imp, var = X, cl = cores, verbose = verbose)
  summary(sim_estimand)
}
