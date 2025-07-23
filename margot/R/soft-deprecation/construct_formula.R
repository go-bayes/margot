#' Construct a Formula for Regression Models
#'
#' This function dynamically constructs a regression formula based on provided parameters.
#' It supports incorporating splines for continuous variables and can handle interaction
#' terms, including a subclass variable. The function ensures that the subclass variable
#' is not redundantly included in the baseline variables.
#'
#' @param Y A string specifying the dependent variable in the model.
#' @param X A string specifying the independent treatment or exposure variable; defaults to 1 (intercept only model).
#' @param baseline_vars A character vector of baseline covariate names to include in the model.
#' @param continuous_X A logical indicating whether `X` is a continuous variable; if TRUE and `splines` is TRUE, applies spline transformation to `X`.
#' @param splines A logical indicating whether to apply spline transformations to the treatment variable `X`.
#' @param subclass An optional string specifying a subclass variable for interaction with `X` and baseline covariates.
#'
#' @return A string representing the constructed formula for use in regression modeling functions like `glm`.
#'
#' @examples
#' # Example with basic interaction terms without subclass:
#' construct_formula("health_outcome", "treatment", c("age", "sex"), FALSE, FALSE)
#'
#' # Example with spline transformation for a continuous treatment:
#' construct_formula("health_outcome", "treatment", c("age", "sex"), TRUE, TRUE)
#'
#' # Example including a subclass variable:
#' construct_formula("health_outcome", "treatment", c("age", "sex", "income"), FALSE, FALSE, "region")
#'
#' # Example with continuous treatment, splines, and subclass interaction:
#' construct_formula("health_outcome", "treatment", c("age", "sex", "income"), TRUE, TRUE, "region")
#'
#' @importFrom splines bs
#' @importFrom stats glm
#' @keywords internal
construct_formula <- function(Y, X = 1, baseline_vars, continuous_X = FALSE, splines = FALSE, subclass = NULL) {
  # Early return for simple model
  if (X == 1) {
    return(paste(Y, "~ 1"))
  }

  # Remove subclass from baseline_vars if necessary
  if (!is.null(subclass) && subclass %in% baseline_vars) {
    baseline_vars <- setdiff(baseline_vars, subclass)  # Exclude subclass from baseline_vars
    message("The subclass variable '", subclass, "' has been removed from baseline_vars to avoid redundancy.")
  }

  # Construct interaction terms
  if (!is.null(subclass)) {
    if (continuous_X && splines) {
      # Include splines transformation within the interaction term
      interaction_terms <- paste(subclass, "* ( bs(", X, ")", "*", "(", paste(baseline_vars, collapse = "+"), ") )")
    } else {
      interaction_terms <- paste(subclass, "* (", X, "*", "(", paste(baseline_vars, collapse = "+"), ") )")
    }
  } else {
    if (continuous_X && splines) {
      interaction_terms <- paste("bs(", X, ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
    } else {
      interaction_terms <- paste(X, "*", "(", paste(baseline_vars, collapse = "+"), ")")
    }
  }

  # Construct the final formula
  formula_str <- paste(Y, "~", interaction_terms)

  return(formula_str)
}
