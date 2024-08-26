#' Generate Additional Sections for Methods (DEPRECATED)
#'
#' @description
#' \lifecycle{deprecated}
#' This function is deprecated. Please use `boilerplate::boilerplate_report_additional_sections` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @export
boilerplate_methods_additional_sections <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.22",
    what = "boilerplate_methods_additional_sections()",
    with = "boilerplate::boilerplate_report_additional_sections()"
  )

  message("This function is deprecated. Please use boilerplate::boilerplate_report_additional_sections instead.")
  message("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #' Generate Additional Sections for Methods
#' #'
#' #' This function generates markdown-formatted additional sections for the methods,
#' #' including Sensitivity Analysis, Scope of Interventions, and Evidence for Change
#' #' in the Treatment Variable. It uses Quarto-compatible figure and table references
#' #' and allows for the inclusion of R code blocks.
#' #'
#' #' @param sensitivity_analysis A list containing details for the Sensitivity Analysis section.
#' #'   Should include a 'description' element. Default provides a description of the E-value method.
#' #' @param scope_interventions A list containing details for the Scope of Interventions section.
#' #'   Should include 'figure_ref' and 'code' elements. 'figure_ref' is a string for the figure reference,
#' #'   and 'code' is a string containing the R code block for generating the figure.
#' #' @param evidence_change A list containing details for the Evidence for Change section.
#' #'   Should include 'table_ref' and 'code' elements. 'table_ref' is a string for the table reference,
#' #'   and 'code' is a string containing the R code block for generating the table.
#' #' @param ... Additional arguments (not used in this version).
#' #'
#' #' @return A character string containing the markdown-formatted additional sections,
#' #'   including R code blocks for figure and table generation.
#' #'
#' #' @examples
#' #' boilerplate_methods_additional_sections()
#' #'
#' #' # Custom figure and table references with R code blocks
#' #' boilerplate_methods_additional_sections(
#' #'   scope_interventions = list(
#' #'     figure_ref = "@fig-custom-hist",
#' #'     code = '
#' #' ```{r}
#' #' #| label: fig-custom-hist
#' #' #| fig-cap: "Distribution of treatments"
#' #' #| eval: true
#' #' #| include: true
#' #' #| echo: false
#' #' graph_histogram <- margot::here_read("graph_histogram")
#' #' graph_histogram + theme_classic()
#' #' ```
#' #' '
#' #'   ),
#' #'   evidence_change = list(
#' #'     table_ref = "@tbl-custom-transition",
#' #'     code = '
#' #' ```{r}
#' #' #| label: tbl-custom-transition
#' #' #| tbl-cap: "Treatment variable changes"
#' #' #| eval: true
#' #' #| echo: false
#' #' transition_table <- margot::here_read("transition_table")
#' #' transition_table$table
#' #' ```
#' #' '
#' #'   )
#' #' )
#' #'
#' #' @export
#' boilerplate_methods_additional_sections <- function(
#'     sensitivity_analysis = list(
#'       description = "To assess the sensitivity of results to unmeasured confounding, we report VanderWeele and Ding's \"E-value\" in all analyses [@vanderweele2017]. The E-value quantifies the minimum strength of association (on the risk ratio scale) that an unmeasured confounder would need to have with both the exposure and the outcome (after considering the measured covariates) to explain away the observed exposure-outcome association [@vanderweele2020; @linden2020EVALUE]. To evaluate the strength of evidence, we use the bound of the E-value 95% confidence interval closest to 1. This provides a clear metric for understanding the robustness of our findings in the presence of potential unmeasured confounding."
#'     ),
#'     scope_interventions = list(
#'       figure_ref = "@fig-hist",
#'       code = '
#' ```{r}
#' #| label: fig-hist
#' #| fig-cap: " "
#' #| eval: true
#' #| include: true
#' #| echo: false
#' #| fig-width: 10
#' #| fig-height: 12
#' graph_histogram <- margot::here_read("graph_histogram")
#' graph_histogram + theme_classic()
#' ```
#' '
#'     ),
#'     evidence_change = list(
#'       table_ref = "@tbl-transition",
#'       code = '
#' ```{r}
#' #| label: tbl-transition
#' #| tbl-cap: "TBA."
#' #| eval: true
#' #| echo: false
#' transition_table <- margot::here_read("transition_table")
#' transition_table$table
#' ```
#' '
#'     ),
#'     ...
#' ) {
#'   # Ensure sensitivity_analysis is a list with a description
#'   if (!is.list(sensitivity_analysis) || is.null(sensitivity_analysis$description)) {
#'     sensitivity_analysis <- list(description = "We use the E-value method to assess sensitivity to unmeasured confounding.")
#'   }
#'
#'   # Ensure scope_interventions is a list with a figure_ref and code
#'   if (!is.list(scope_interventions) || is.null(scope_interventions$figure_ref) || is.null(scope_interventions$code)) {
#'     scope_interventions <- list(
#'       figure_ref = "@fig-hist",
#'       code = '
#' ```{r}
#' #| label: fig-hist
#' #| fig-cap: " "
#' #| eval: true
#' #| include: true
#' #| echo: false
#' #| fig-width: 10
#' #| fig-height: 12
#' graph_histogram <- margot::here_read("graph_histogram")
#' graph_histogram + theme_classic()
#' ```
#' '
#'     )
#'   }
#'
#'   # Ensure evidence_change is a list with a table_ref and code
#'   if (!is.list(evidence_change) || is.null(evidence_change$table_ref) || is.null(evidence_change$code)) {
#'     evidence_change <- list(
#'       table_ref = "@tbl-transition",
#'       code = '
#' ```{r}
#' #| label: tbl-transition
#' #| tbl-cap: "TBA."
#' #| eval: true
#' #| echo: false
#' transition_table <- margot::here_read("transition_table")
#' transition_table$table
#' ```
#' '
#'     )
#'   }
#'
#'   markdown_text <- glue::glue("
#' ### Sensitivity Analysis Using the E-value
#'
#' {sensitivity_analysis$description}
#'
#' ### Scope of Interventions
#'
#' To illustrate the magnitude of the shift interventions we contrast, we provide histograms in {scope_interventions$figure_ref}, that display the distribution of treatments during the treatment wave.
#'
#' {scope_interventions$code}
#'
#' {{{{< pagebreak >}}}}
#'
#' ### Evidence for Change in the Treatment Variable
#'
#' {evidence_change$table_ref} clarifies the change in the treatment variable from the baseline wave to the baseline + 1 wave across the sample. Assessing change in a variable is essential for evaluating the positivity assumption and recovering evidence for the incident exposure effect of the treatment variable [@vanderweele2020; @danaei2012; @hernan2024WHATIF].
#'
#' {evidence_change$code}
#'
#' {{{{< pagebreak >}}}}
#'   ")
#'
#'   return(markdown_text)
#' }

