#' Generalized Linear Regression with Covariates
#'
#' This unitility function performs a generalized linear regression on a specified dataset using an outcome variable, an exposure variable, and a set of baseline covariates. By default, the function uses the Gaussian family (linear regression), but it allows for specifying other families for generalized linear models (GLM).
#'
#' @param data A data frame containing the variables for the analysis.
#' @param outcome A character string specifying the name of the outcome variable in the data frame.
#' @param exposure A character string specifying the name of the main exposure variable in the data frame.
#' @param baseline_vars A character vector specifying the names of baseline covariates to include in the model in addition to the exposure variable.
#' @param family A family object or a character string naming the family (default is \code{gaussian()}, which performs linear regression). This parameter determines the error distribution and link function to be used in the model.
#'
#' @details The function constructs a model formula using the outcome, exposure, and baseline variables. It then fits a generalized linear model using this formula. The baseline variables are filtered to exclude the outcome and exposure variables before model fitting. The function prints the formula used for the regression analysis for verification.
#'
#' @return An object of class \code{glm} representing the fitted model, which includes coefficients, residuals, and other model diagnostics. This object can be further analyzed using standard methods for GLM objects, such as \code{summary()} for model summaries or \code{anova()} for analysis of variance.
#'
#' @examples
#' # using `df_nz` is your data frame with "income" as the continuous outcome variable,
#' # "age" as an exposure variable, and other covariates
#' outcome_var <- "income"
#' exposure_var <- "age"
#' baseline_vars <- c("age", "education", "partner")
#' model <- regress_with_covariates(df_nz, outcome_var, exposure_var, baseline_vars, family =  gaussian())
#' summary(model)
#'
#' @export
#' @importFrom stats glm
#' @importFrom stats gaussian
regress_with_covariates <- function(data, outcome, exposure, baseline_vars, family = gaussian()) {
  # ensure baseline_vars is correctly formatted as a character vector if passed as a single string
  if (!is.character(baseline_vars)) {
    stop("baseline_vars must be a character vector.")
  }

  # filter out the outcome and exposure from the baseline variables
  covariates <- setdiff(baseline_vars, c(outcome, exposure))

  # construct formula string
  covariate_str <- paste(covariates, collapse = " + ")
  formula_str <- paste(outcome, "~", exposure, "+", covariate_str)
  formula <- as.formula(formula_str)

  # print the formula for verification
  print(formula)

  # regress using glm
  model <- glm(formula, data = data, family = family)

  return(model)
}
