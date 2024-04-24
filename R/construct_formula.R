#' Construct Formula for Model Fitting
#'
#' This function constructs a formula string for model fitting based on provided specifications,
#' including handling of continuous variables and application of splines if required.
#'
#' @param Y The response variable name as a string.
#' @param X The treatment or exposure variable name as a string.
#' @param baseline_vars Vector of baseline covariate names.
#' @param continuous_X Logical, whether X is a continuous variable.
#' @param splines Logical, whether to apply spline transformation to X.
#' @return A character string representing the formula for the glm function.
#' @export
construct_formula <-
  function(Y,
           X = 1,
           baseline_vars,
           continuous_X = FALSE,
           splines = FALSE,
           subclass = NULL) {
    if (X == 1) {
      return(paste(Y, "~ 1"))
    }

    # Interaction terms
    interaction_terms <- if (!is.null(subclass)) {
      paste0(subclass,
             "*",
             "(",
             X,
             "*",
             "(",
             paste(baseline_vars, collapse = "+"),
             ")",
             ")")
    } else {
      paste0(X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
    }

    if (continuous_X && splines) {
      require(splines)
      formula_str <-
        paste(Y,
              "~ bs(",
              X,
              ")",
              "+",
              interaction_terms)
    } else {
      formula_str <- paste(Y, "~", interaction_terms)
    }

    return(formula_str)
  }


