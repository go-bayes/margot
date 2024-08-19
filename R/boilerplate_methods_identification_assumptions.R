#' Generate Identification Assumptions Description
#'
#' @param exposure_var Character string specifying the primary exposure variable.
#'
#' @return A character string containing the identification assumptions description in markdown format.
#'
#' @export
boilerplate_methods_identification_assumptions <- function(exposure_var) {
  markdown_text <- glue::glue("
### Identification Assumptions

This study relies on the following key identification assumptions for estimating the causal effect of {exposure_var}:

1. **Consistency**: The observed outcome under the observed {exposure_var} is equal to the potential outcome under that exposure level.

2. **Positivity**: There is a non-zero probability of receiving each level of {exposure_var} for every combination of values of {exposure_var} and confounders in the population.

3. **No unmeasured confounding**: All variables that affect both {exposure_var} and the outcome have been measured and accounted for in the analysis.

4. **No interference**: The potential outcomes for one individual are not affected by the {exposure_var} status of other individuals.

These assumptions are critical for the causal interpretability of our results and should be carefully considered in light of the study design and available data.
  ")

  return(markdown_text)
}
