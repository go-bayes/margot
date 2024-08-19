#' Generate Sample Description
#'
#' @param n_total Numeric value indicating the total number of participants.
#' @param appendices_sample Character string specifying the appendices containing sample data.
#' @param custom_template Character string containing a custom template for the sample description.
#'   Use placeholders like {{n_total}} and {{appendices_sample}} for variable insertion.
#' @param replacements Named list of additional custom replacements for the template.
#'
#' @return A character string containing the sample description in markdown format.
#'
#' @examples
#' # Using default template
#' boilerplate_methods_sample(n_total = 47000, appendices_sample = "A-C")
#'
#' # Using custom template
#' custom_template <- "
#' ### Study Sample
#' This study used data from {{n_total}} participants in the NZAVS.
#' Appendices {{appendices_sample}} contain detailed data summaries.
#' {{custom_info}}
#' "
#' boilerplate_methods_sample(
#'   n_total = 47000,
#'   appendices_sample = "A-C",
#'   custom_template = custom_template,
#'   replacements = list(custom_info = "Additional custom information here.")
#' )
#'
#' @export
boilerplate_methods_sample <- function(n_total,
                                       appendices_sample = "B-D",
                                       custom_template = NULL,
                                       replacements = list(),
                                       baseline_wave,
                                       exposure_wave,
                                       outcome_wave) {
  default_template <- "
### Sample

Data were collected by the New Zealand Attitudes and Values Study (NZAVS), an annual longitudinal national probability panel study of social attitudes, personality, ideology, and health outcomes in New Zealand. Chris G. Sibley started the New Zealand Attitudes and Values Study in 2009, which has grown to include a community of over fifty researchers. Since its inception, the New Zealand Attitudes and Values Study has accumulated questionnaire responses from {n_total} New Zealand residents. The study operates independently of political or corporate funding and is based in a university setting. Data summaries for our study sample on all measures used in this study are found in **Appendices {appendices_sample}**. For more details about the New Zealand Attitudes and Values Study see: [OSF.IO/75SNB](https://doi.org/10.17605/OSF.IO/75SNB). Here, our study uses data from {baseline_wave}, {exposure_wave}, {outcome_wave}
  "
  # Use custom template if provided, otherwise use default
  template <- if (!is.null(custom_template)) custom_template else default_template
  # Create a list of replacements
  default_replacements <- list(
    n_total = n_total,
    appendices_sample = appendices_sample,
    baseline_wave = baseline_wave,
    exposure_wave = exposure_wave,
    outcome_wave = outcome_wave
  )
  # Combine default replacements with custom replacements
  all_replacements <- c(default_replacements, replacements)
  # Apply replacements to the template
  markdown_text <- glue::glue(template, .envir = all_replacements)
  return(markdown_text)
}
