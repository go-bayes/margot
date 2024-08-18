#' Generate Statistical Estimator Description
#'
#' @param estimators Character vector specifying the statistical estimators used.
#'
#' @return A character string containing the statistical estimator description in markdown format.
#'
#' @keywords internal
boilerplate_methods_statistical_estimator <- function(estimators = "lmtp") {
  if (is.character(estimators) && length(estimators) == 1) {
    estimators <- list(estimators)
  }

  estimator_texts <- list(
    lmtp = "
### Longitudinal Modified Treatment Policy (LMTP) Estimator

This study employs the Longitudinal Modified Treatment Policy (LMTP) estimator. The LMTP estimator is designed to estimate the causal effect of time-varying exposures in the presence of time-varying confounders. It allows for the estimation of the effect of complex longitudinal interventions while accounting for the feedback between time-varying exposures and confounders.

Key features of the LMTP estimator include:
- Ability to handle time-varying treatments and confounders
- Robustness to model misspecification (double robustness property)
- Flexibility in specifying the intervention of interest
    ",
    sdr = "
### Sequentially Doubly Robust (SDR) Estimator

This study uses the Sequentially Doubly Robust (SDR) estimator. The SDR estimator is designed for longitudinal studies with time-varying exposures and confounders. It provides consistent estimates of the causal effect even if either the outcome model or the treatment mechanism model is misspecified (but not both).

Key features of the SDR estimator include:
- Double robustness property
- Efficiency in the presence of time-varying confounding
- Ability to incorporate time-varying effect modification
    ",
    grf = "
### Generalized Random Forests (GRF)

This study utilizes the Generalized Random Forests (GRF) method for causal inference. GRF extends the random forest algorithm to estimate heterogeneous treatment effects. It can be used to estimate conditional average treatment effects (CATE) and provides a non-parametric approach to causal inference.

Key features of the GRF method include:
- Ability to capture complex, non-linear relationships
- Estimation of heterogeneous treatment effects
- Built-in variable importance measures
- Flexibility in handling various types of outcome variables
    "
  )

  selected_estimators <- estimator_texts[unlist(estimators)]

  markdown_text <- paste0("
### Statistical Estimator
  ",
                          paste(unlist(selected_estimators), collapse = "\n\n")
  )

  return(markdown_text)
}
