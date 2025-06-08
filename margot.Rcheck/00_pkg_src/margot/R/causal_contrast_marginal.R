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
#' # Assume that df is your dataset with variables 'outcome', 'treatment', 'age', and 'gender'
#' result <- causal_contrast_marginal(df = df, Y = "outcome", X = "treatment",
#'                                    baseline_vars = c("age", "gender"),
#'                                    treat_0 = "control", treat_1 = "exposed",
#'                                    estimand = "ATE", type = "RD", nsims = 100,
#'                                    cores = 2, family = "gaussian", weights = "weight_var",
#'                                    continuous_X = FALSE, splines = FALSE,
#'                                    vcov = "HC3", verbose = TRUE)
#'
#' @importFrom parallel detectCores
#' @importFrom purrr map
#' @importFrom glue glue
#' @importFrom rlang .data quo enquo eval_tidy
#' @keywords internal
causal_contrast_marginal <- function(df, Y, X, baseline_vars = "1", treat_0, treat_1,
                                     estimand = c("ATE", "ATT"), type = c("RR", "RD"),
                                     nsims = 200, cores = parallel::detectCores(), family = "gaussian",
                                     weights = NULL, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
                                     verbose = FALSE) {
  # Validate the type argument
  type <- match.arg(type, choices = c("RR", "RD"))

  # Validate the family argument
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

  # Use the updated causal_contrast_engine function
  results <- causal_contrast_engine(
    df = df,
    Y = Y,
    X = X,
    baseline_vars = baseline_vars,
    treat_0 = treat_0,
    treat_1 = treat_1,
    estimand = estimand,
    type = type,
    nsims = nsims,
    cores = cores,
    family = family,
    weights = weights,
    continuous_X = continuous_X,
    splines = splines,
    vcov = vcov,
    verbose = verbose
  )

  return(results)
}
# # helper function defined outside of both main functions
# build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
#   baseline_part <- if (!(length(baseline_vars) == 1 && baseline_vars == "1")) {
#     paste(baseline_vars, collapse = "+")
#   } else {
#     "1"
#   }
#
#   if (continuous_X && splines) {
#     paste(Y, "~ bs(", X, ")", "*", "(", baseline_part, ")")
#   } else {
#     paste(Y, "~", X, "*", "(", baseline_part, ")")
#   }
# }
# # function
# old function
# causal_contrast_marginal <- function(df, Y, X, baseline_vars = "1", treat_0, treat_1,
#                                      estimand = c("ATE", "ATT"), type = c("RR", "RD"),
#                                      nsims = 200, cores = parallel::detectCores(), family = "gaussian",
#                                      weights = NULL, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
#                                      verbose = FALSE) {
#   # Validate the type argument
#   type <- match.arg(type, choices = c("RR", "RD"))
#
#   # Validate the family argument
#   if (is.character(family)) {
#     if (!family %in% c("gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi")) {
#       stop("Invalid 'family' argument. Please specify a valid family function.")
#     }
#     family_fun <- get(family, mode = "function", envir = parent.frame())
#   } else if (inherits(family, "family")) {
#     family_fun <- family
#   } else {
#     stop("Invalid 'family' argument. Please specify a valid family function or character string.")
#   }
#
#   # Construct the model formula
#   build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
#     baseline_part <- if (!(length(baseline_vars) == 1 && baseline_vars == "1")) {
#       paste(baseline_vars, collapse = "+")
#     } else {
#       "1"
#     }
#
#     if (continuous_X && splines) {
#       paste(Y, "~ bs(", X, ")", "*", "(", baseline_part, ")")
#     } else {
#       paste(Y, "~", X, "*", "(", baseline_part, ")")
#     }
#   }
#
#   # Apply model
#   weight_var <- if (!is.null(weights) && weights %in% names(df)) df[[weights]] else NULL
#   formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
#   fit <- try(glm(as.formula(formula_str), weights = weight_var, family = family_fun, data = df), silent = !verbose)
#   if (inherits(fit, "try-error")) {
#     if (verbose) cat("Model fitting failed with error:", conditionMessage(fit), "\n")
#     return(NULL)
#   }
#   sim_imp <- sim(fit, n = nsims, vcov = vcov)
#
#   # Output processing and return
#   sim_estimand <- sim_ame(sim_imp, var = X, cl = cores, verbose = verbose)
#   treat_0_name <- paste0("`E[Y(", treat_0, ")]`")
#   treat_1_name <- paste0("`E[Y(", treat_1, ")]`")
#
#   if (type == "RR") {
#     rr_expression_str <- glue::glue("{treat_1_name}/{treat_0_name}")
#     rr_expression <- rlang::parse_expr(rr_expression_str)
#     sim_estimand <- transform(sim_estimand, RR = eval(rr_expression))
#   } else {
#     rd_expression_str <- glue::glue("{treat_1_name} - {treat_0_name}")
#     rd_expression <- rlang::parse_expr(rd_expression_str)
#     sim_estimand <- transform(sim_estimand, RD = eval(rd_expression))
#   }
#
#   summary(sim_estimand)
# }


