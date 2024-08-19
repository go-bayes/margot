#' Generate Target Population Description
#'
#' @param statistical_estimator Character string specifying the statistical estimator used.
#'
#' @return A character string containing the target population description in markdown format.
#'
#' @export
boilerplate_methods_target_population <- function(statistical_estimator = "lmtp", baseline_wave) {
  base_text <- glue::glue("## Target Population
The target population for this study comprises New Zealand residents as represented in the {baseline_wave} of the New Zealand Attitudes and Values Study (NZAVS) during the years {baseline_wave} weighted by New Zealand Census weights for age, gender, and ethnicity (refer to @sibley2021). The NZAVS is a national probability study designed to reflect the broader New Zealand population accurately. Despite its comprehensive scope, the NZAVS has some limitations in its demographic representation. Notably, it tends to under-sample males and individuals of Asian descent while over-sampling females and Māori (the indigenous peoples of New Zealand). To address these disparities and enhance the accuracy of our findings, we apply New Zealand Census survey weights to the sample data.")
  estimator_specific_text <- if (statistical_estimator == "lmtp") {
    "These weights adjust for variations in age, gender, and ethnicity to better approximate the national demographic composition [@sibley2021]. Survey weights were integrated into statistical models using the `weights` option in `lmtp` [@williams2021], following protocols stated in [@bulbulia2024PRACTICAL]."
  } else if (statistical_estimator == "grf") {
    "These weights adjust for variations in age, gender, and ethnicity to better approximate the national demographic composition [@sibley2021]. Survey weights were integrated into statistical models using functionality in the `grf` package [@grf2024]."
  } else {
    "The method for integrating survey weights into the statistical models depends on the specific estimator used in this study."
  }
  markdown_text <- paste(base_text, estimator_specific_text, sep = "\n\n")
  return(markdown_text)
}
