#' Interpret and Describe Causal Effect Estimates
#'
#' This function interprets the output of the `group_tab` function, providing textual descriptions of causal effect estimates. It categorizes the strength of evidence for causality based on E-values and confidence intervals, and generates a detailed interpretation of the effect estimates according to specified causal scales and estimands.
#'
#' @param df Data frame output from `group_tab` containing categorized causal effect estimates.
#' @param causal_scale Character string specifying the causal scale used in the analysis, currently only supports "causal_difference". Future versions may include additional scales.
#' @param estimand Character string indicating the type of causal estimand interpreted: "PATE" (Population Average Treatment Effect), "ATE" (Average Treatment Effect), "ATT" (Average Treatment Effect in the Treated), or "CATE" (Conditional Average Treatment Effect).
#'
#' @return A character vector containing a detailed interpretation of each outcome in `df`, including the type of estimand, the causal contrast, E-values, and the strength of evidence for causality.
#'
#' @examples
#' \dontrun{
#' # Assuming `group_tab_output` is the result from `group_tab`
#' margot_interpret_table(group_tab_output, "causal_difference", "ATE")
#' }
#'
#' @export
#' @importFrom dplyr case_when mutate rowwise ungroup if_else
#' @importFrom glue glue
margot_interpret_table <- function(df, causal_scale, estimand) {
  estimand_description <- dplyr::case_when(
    estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) represents the expected difference in outcomes between treatment and control groups for the New Zealand population.",
    estimand == "ATE" ~ "The Average Treatment Effect (ATE) represents the expected difference in outcomes between treatment and control groups for the population.",
    estimand == "ATT" ~ "The Average Treatment Effect in the Treated (ATT) represents the expected difference in outcomes between treatment and control groups for the treated population.",
    estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) represents the expected difference in outcomes between treatment and control groups within a stratum of the population.",
    TRUE ~ "The specified estimand is not recognized. Please use one of the following: 'PATE', 'ATE', 'ATT', 'CATE'."
  )

  interpretation <- df %>%
    dplyr::mutate(
      causal_contrast = dplyr::case_when(
        causal_scale == "causal_difference" ~ round(`E[Y(1)]-E[Y(0)]`, 3),
        TRUE ~ NA_real_  # Placeholder, adjust as needed if adding other scales
      ),
      E_Value = round(E_Value, 3),
      E_Val_bound = round(E_Val_bound, 3),
      `2.5 %` = round(`2.5 %`, 3),
      `97.5 %` = round(`97.5 %`, 3)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      strength_of_evidence = dplyr::case_when(
        E_Val_bound == 1 ~ "no reliable evidence for causality",
        E_Val_bound <= 1 |
          (`2.5 %` <= 0 &
             `97.5 %` >= 0) ~ "no reliable evidence for causality",
        E_Val_bound > 1 &
          E_Val_bound < 1.1 ~ "the evidence for causality is weak",
        E_Val_bound > 2 ~ "strong evidence for causality",
        TRUE ~ "evidence for causality"
      ),
      outcome_interpretation = if_else(
        E_Val_bound == 1,
        glue::glue(
          "For the outcome '{outcome}', given the lower bound of the E-value equals 1, we find no reliable evidence for causality."
        ),
        glue::glue(
          "For the outcome '{outcome}', the {estimand} is {causal_contrast} [{`2.5 %`},{`97.5 %`}]. ",
          "The E-value for this effect estimate is {E_Value} ",
          "with a lower bound of {E_Val_bound}. In this context, if there exists an unmeasured confounder that is associated with both the treatment and the outcome, and this association has a risk ratio of {E_Val_bound}, then it is possible for such a confounder to negate the observed effect. Conversely, any confounder with a weaker association (i.e., a risk ratio less than {E_Val_bound}) would not be sufficient to fully account for the observed effect.",
          "Here, we find {strength_of_evidence}."
        )
      )
    ) %>%
    dplyr::ungroup()

  result <- glue::glue(
    "\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}"
  )
  return(result)
}
