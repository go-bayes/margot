#' Build Formula String for GLM
#'
#' This helper function constructs a formula string for use in generalized linear models (GLMs).
#' It handles both continuous and categorical predictor variables, and optionally includes
#' spline transformations and baseline variables.
#'
#' @param Y A character string specifying the name of the response variable.
#' @param X A character string specifying the name of the predictor variable (treatment or exposure).
#' @param continuous_X Logical; if TRUE, X is treated as a continuous variable.
#' @param splines Logical; if TRUE, applies a spline transformation to X (only when continuous_X is TRUE).
#' @param baseline_vars A character vector of baseline covariates to include in the model.
#'
#' @return A character string representing the formula for use in a GLM.
#'
#' @examples
#' build_formula_str("outcome", "treatment", FALSE, FALSE, c("age", "gender"))
#' # Returns: "outcome ~ treatment * (age+gender)"
#'
#' build_formula_str("outcome", "exposure", TRUE, TRUE, c("age", "gender"))
#' # Returns: "outcome ~ bs(exposure) * (age+gender)"
#'
#' @importFrom splines bs
#' @noRd
build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
  baseline_part <- if (!(length(baseline_vars) == 1 && baseline_vars == "1")) {
    paste(baseline_vars, collapse = "+")
  } else {
    "1"
  }

  if (continuous_X && splines) {
    paste(Y, "~ bs(", X, ")", "*", "(", baseline_part, ")")
  } else {
    paste(Y, "~", X, "*", "(", baseline_part, ")")
  }
}
