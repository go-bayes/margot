#' Generate Additional Sections for Methods
#'
#' This function generates markdown-formatted additional sections for the methods,
#' including Sensitivity Analysis, Scope of Interventions, and Evidence for Change
#' in the Treatment Variable. It uses Quarto-compatible figure and table references.
#'
#' @param sensitivity_analysis A list containing details for the Sensitivity Analysis section.
#'   Default is a list with a description of the E-value method.
#' @param scope_interventions A list containing details for the Scope of Interventions section.
#'   Default is a list with a reference to a histogram figure.
#' @param evidence_change A list containing details for the Evidence for Change section.
#'   Default is a list with a reference to a transition table.
#' @param ... Additional arguments (not used in this version).
#'
#' @return A character string containing the markdown-formatted additional sections.
#'
#' @examples
#' boilerplate_methods_additional_sections()
#'
#' # Custom figure and table references
#' boilerplate_methods_additional_sections(
#'   scope_interventions = list(figure_ref = "@fig-custom-hist"),
#'   evidence_change = list(table_ref = "@tbl-custom-transition")
#' )
#'
#' @export
boilerplate_methods_additional_sections <- function(
    sensitivity_analysis = list(
      description = "To assess the sensitivity of results to unmeasured confounding, we report VanderWeele and Ding's \"E-value\" in all analyses [@vanderweele2017]. The E-value quantifies the minimum strength of association (on the risk ratio scale) that an unmeasured confounder would need to have with both the exposure and the outcome (after considering the measured covariates) to explain away the observed exposure-outcome association [@vanderweele2020; @linden2020EVALUE]. To evaluate the strength of evidence, we use the bound of the E-value 95% confidence interval closest to 1. This provides a clear metric for understanding the robustness of our findings in the presence of potential unmeasured confounding."
    ),
    scope_interventions = list(
      figure_ref = "@fig-hist"
    ),
    evidence_change = list(
      table_ref = "@tbl-transition"
    ),
    ...
) {
  markdown_text <- glue::glue("
### Sensitivity Analysis Using the E-value
{sensitivity_analysis$description}

### Scope of Interventions
To illustrate the magnitude of the shift interventions we contrast, we provide histograms in {scope_interventions$figure_ref}, that display the distribution of treatments during the treatment wave. {scope_interventions$figure_ref}.

{{{{< pagebreak >}}}}

### Evidence for Change in the Treatment Variable
{evidence_change$table_ref} clarifies the change in the treatment variable from the baseline wave to the baseline + 1 wave across the sample. Assessing change in a variable is essential for evaluating the positivity assumption and recovering evidence for the incident exposure effect of the treatment variable [@vanderweele2020; @danaei2012; @hernan2024WHATIF].

{{{{< pagebreak >}}}}
  ")

  return(markdown_text)
}
