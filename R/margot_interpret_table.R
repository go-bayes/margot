
#' Interpret and Describe Causal Effect Estimates Using E-values
#'
#' This function interprets the output of causal effect analysis, providing textual descriptions
#' of causal effect estimates. It categorises the strength of evidence for causality based on
#' E-values and confidence intervals, and generates a detailed interpretation of the effect
#' estimates according to specified causal scales (i.e., "causal_difference" or "risk_ratio")
#' and estimands. This function now supports interpreting results on both the causal difference
#' and risk ratio scales.
#'
#' @param df Data frame containing causal effect estimates, expected to include columns for
#' outcome names, effect estimates (either differences or ratios), confidence intervals,
#' E-values, and a summary estimate label. The structure of `df` should align with the specified
#' `causal_scale`.
#' @param causal_scale Character string specifying the causal scale used in the analysis.
#' Currently supports "causal_difference" for differences in means or medians, and "risk_ratio"
#' for comparing ratios of probabilities or risks.
#' @param estimand Character string indicating the type of causal estimand interpreted: "PATE"
#' (Population Average Treatment Effect), "ATE" (Average Treatment Effect), "ATT" (Average
#' Treatment Effect in the Treated), or "CATE" (Conditional Average Treatment Effect).
#'
#' @return A character vector containing a detailed interpretation of each outcome in `df`,
#' including the type of estimand, the causal contrast, E-values, and the strength of evidence
#' for causality. The interpretation includes whether there is evidence for causality based on
#' the E-value and confidence interval, tailored to the specified causal scale.
#'
#' @examples
#' \dontrun{
#' # Assuming `group_tab_output` is the result from a causal analysis
#' margot_interpret_table(group_tab_output, "causal_difference", "ATE")
#' margot_interpret_table(group_tab_output, "risk_ratio", "PATE")
#' }
#'
#' @export
#' @importFrom dplyr case_when mutate rowwise ungroup if_else
#' @importFrom glue glue
margot_interpret_table <- function(df, causal_scale, estimand) {
  # Estimand descriptions
  estimand_description <- dplyr::case_when(
    estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
    estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
    estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
    estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
    estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
    TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
  )

  # Dynamic calculation based on causal_scale
  causal_contrast_column <- if (causal_scale == "causal_difference") {
    "E[Y(1)]-E[Y(0)]"
  } else if (causal_scale == "risk_ratio") {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Invalid causal_scale. Valid options are 'causal_difference' or 'risk_ratio'.")
  }

  if (!causal_contrast_column %in% names(df)) {
    stop(paste("Dataframe does not contain the required column:", causal_contrast_column))
  }

  # Data processing and interpretation
  interpretation <- df |>
    dplyr::mutate(
      causal_contrast = round(.data[[causal_contrast_column]], 3),
      E_Value = round(E_Value, 3),
      E_Val_bound = round(E_Val_bound, 3),
      `2.5 %` = round(`2.5 %`, 3),
      `97.5 %` = round(`97.5 %`, 3)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      strength_of_evidence = case_when(
        E_Val_bound == 1 ~ "**that evidence for causality is not reliable**",
        E_Val_bound <= 1 | (`2.5 %` <= 0 & `97.5 %` >= 0) ~ "that the **evidence for causality is not reliable**",
        E_Val_bound > 1 & E_Val_bound < 1.1 ~ "that the **evidence for causality is weak**",
        E_Val_bound > 2 ~ "that **the evidence for causality is not reliable**",
        TRUE ~ "**evidence for causality**"
      ),
      outcome_interpretation = glue::glue(
        "For '{outcome}', the effect estimate is {causal_contrast} [{`2.5 %`}, {`97.5 %`}]. ",
        "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
        "At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of {E_Val_bound} to negate the observed effect. Weaker confounding would not overturn it. ",
        "We infer {strength_of_evidence}."
      )
    ) |>
    dplyr::ungroup()

  # Compile results
  result <- glue::glue(
    "\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}"
  )
  return(result)
}


