#' Generate Variables Section for Methods
#'
#' This function generates a markdown-formatted section describing the variables used in the study.
#'
#' @param exposure_var A character string specifying the name of the exposure variable.
#' @param outcome_vars A character vector specifying the names of the outcome variables.
#' @param ... Additional arguments (not used in this basic version).
#'
#' @return A character string containing the markdown-formatted section on variables.
#'
#' @export
boilerplate_methods_variables <- function(exposure_var, outcome_vars, ...) {
  outcome_vars_list <- paste("- ", outcome_vars, collapse = "\n")

  markdown_text <- glue::glue("
## Variables

### Exposure Variable
The primary exposure variable in this study is '{exposure_var}'.

### Outcome Variables
The outcome variables examined in this study are:

{outcome_vars_list}

Detailed descriptions of how these variables were measured and operationalized can be found in the study protocol.
  ")

  return(markdown_text)
}
