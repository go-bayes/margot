#' Generate Causal Interventions and Contrasts Section for Methods (DEPRECATED, use `boilerplate` package: <https://go-bayes.github.io/boilerplate/>))
#'
#' @description
#' `r lifecycle::badge("deprecated")`  Use `boilerplate` package: <https://go-bayes.github.io/boilerplate/>
#' This function is deprecated. Please use `boilerplate::boilerplate_report_causal_interventions()` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @examples
#' \dontrun{
#' # This function is deprecated. Use instead:
#' # devtools::install_github("go-bayes/boilerplate")
#' # library(boilerplate)
#' # boilerplate::boilerplate_report_causal_interventions(...)
#' }
#'
#' @import lifecycle
#'
#' @keywords internal
boilerplate_methods_causal_interventions <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "boilerplate_methods_causal_interventions()",
    with = "boilerplate::boilerplate_report_causal_interventions()"
  )

  message("This function is deprecated. Please use boilerplate::boilerplate_report_causal_interventions() instead.")
  message("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  message("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Causal Interventions and Contrasts Section for Methods
#' #'
#' #' This function generates a markdown-formatted section describing the causal
#' #' interventions and contrasts for a causal inference study. It details the
#' #' interventions considered and the approach to contrasts (pairwise, null, or custom).
#' #'
#' #' @param causal_interventions A character vector specifying the causal interventions.
#' #'   Use "exposure_var" as a placeholder for the exposure variable name.
#' #' @param exposure_var A character string specifying the name of the exposure variable.
#' #' @param contrasts A character string specifying the type of contrasts.
#' #'   Options are "pairwise", "null", or "custom". Default is "pairwise".
#' #' @param null_intervention A character string specifying the null intervention
#' #'   when using "null" contrasts. Default is NULL.
#' #'
#' #' @return A character string containing the markdown-formatted section on
#' #'   causal interventions and contrasts.
#' #'
#' #' @details
#' #' The function replaces "exposure_var" in the causal_interventions and
#' #' null_intervention with the actual name of the exposure variable.
#' #'
#' #' For contrasts:
#' #' - "pairwise": Indicates that all interventions will be compared to each other.
#' #' - "null": Indicates that all interventions will be compared to a specified null intervention.
#' #' - "custom": Indicates that custom contrasts will be used, which should be defined elsewhere.
#' #'
#' #' @examples
#' #' boilerplate_methods_causal_interventions(
#' #'   causal_interventions = c("Increase exposure_var", "Do not change exposure_var"),
#' #'   exposure_var = "political_conservative",
#' #'   contrasts = "null",
#' #'   null_intervention = "Do not change exposure_var"
#' #' )
#' #'
#' #' @export
#' boilerplate_methods_causal_interventions <- function(causal_interventions, exposure_var, contrasts = "pairwise", null_intervention = NULL) {
#'   if (is.null(causal_interventions)) {
#'     return("### Causal Interventions\n\nCausal intervention information is not available.")
#'   }
#'
#'   # Replace 'exposure_var' with the actual variable name in the interventions and null_intervention
#'   replace_exposure_var <- function(x) {
#'     gsub("exposure_var", exposure_var, x, fixed = TRUE)
#'   }
#'
#'   causal_interventions <- sapply(causal_interventions, replace_exposure_var)
#'   if (!is.null(null_intervention)) {
#'     null_intervention <- replace_exposure_var(null_intervention)
#'   }
#'
#'   # Prepare the interventions list
#'   interventions_list <- paste('- ', causal_interventions, collapse = '\n')
#'
#'   # Prepare the contrasts section
#'   contrasts_text <- switch(contrasts,
#'                            "pairwise" = "We conduct pairwise comparisons between all interventions to assess their relative effects.",
#'                            "null" = if (!is.null(null_intervention)) {
#'                              glue::glue("We compare each intervention to the null intervention: \"{null_intervention}\". This approach allows us to evaluate the effect of each intervention relative to a baseline scenario.")
#'                            } else {
#'                              "A null intervention was specified for contrasts, but no null intervention was provided. Please specify a null intervention."
#'                            },
#'                            "custom" = "We use custom contrasts in this analysis. The specific contrasts are defined elsewhere in the study protocol.",
#'                            "The contrast method is not specified or is invalid. Please specify a valid contrast method (pairwise, null, or custom)."
#'   )
#'
#'   markdown_text <- glue::glue("
#' ### Causal Interventions and Contrasts
#'
#' #### Interventions
#' This study considers the following causal interventions on the exposure variable '{exposure_var}':
#'
#' {interventions_list}
#'
#' #### Contrasts
#' {contrasts_text}
#'
#' This approach to defining interventions and contrasts allows us to systematically evaluate the causal effects of interest in our study.
#'   ")
#'
#'   return(markdown_text)
#' }
