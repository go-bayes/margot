
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
  estimand_description <- dplyr::case_when(
    estimand == "lmtp" ~ "This longitudinal modified treatment policy computes the expected difference in outcomes between treatment and contrast groups for the target population.",
    estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) computes the expected difference in outcomes between treatment and contrast groups for the New Zealand population.",
    estimand == "ATE" ~ "The Average Treatment Effect (ATE) represents the difference in outcomes between treatment and contrast groups for the target population.",
    estimand == "ATT" ~ "The Average Treatment Effect in the Treated (ATT) computes the expected difference in outcomes between treatment and contrast groups for the treated in the target population.",
    estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) computes the expected difference in outcomes between treatment and contrast groups within a stratum of the population.",
    TRUE ~ "The specified estimand is not recognized. Please use one of the following: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
  )

  interpretation <- df %>%
    dplyr::mutate(
      causal_contrast = if(causal_scale == "causal_difference") {
        round(`E[Y(1)]-E[Y(0)]`, 3)
      } else if(causal_scale == "risk_ratio") {
        round(`E[Y(1)]/E[Y(0)]`, 3)
      } else {
        NA_real_
      },
      E_Value = round(E_Value, 3),
      E_Val_bound = round(E_Val_bound, 3),
      `2.5 %` = round(`2.5 %`, 3),
      `97.5 %` = round(`97.5 %`, 3),
      causal_contrast_data_scale = if(!is.null(df$sd_outcome) && causal_scale == "causal_difference") {
        causal_contrast * df$sd_outcome
      } else {
        NA_real_
      }
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      strength_of_evidence = case_when(
        E_Val_bound == 1 ~ "the evidence for causality is not reliable",
        E_Val_bound <= 1 | (`2.5 %` <= 0 & `97.5 %` >= 0) ~ "**the evidence for causality is not reliable**",
        E_Val_bound > 1 & E_Val_bound < 1.1 ~ "**the evidence for causality is weak**",
        E_Val_bound > 2 ~ "**the evidence for causality is strong**",
        TRUE ~ "**evidence for causality**"
      ),
      outcome_interpretation = if_else(
        E_Val_bound == 1,
        glue::glue(
          "For the outcome '{outcome}', given the lower bound of the E-value equals 1; we infer that evidence for causality is not reliable."
        ),
        glue::glue(
          "For the outcome '{outcome}', the {estimand} is {causal_contrast} [{`2.5 %`},{`97.5 %`}]. ",
          "The E-value for this effect estimate is {E_Value} ",
          "with a lower bound of {E_Val_bound}. In this scenario, assuming all modelled covariates, an unmeasured confounder that exhibits an association with both the treatment and outcome with a minimum strength (on the risk ratio scale) of {E_Val_bound} could nullify the observed effect. However, the effect would survive weaker unmeasured confounding.",
          "Here, we find {strength_of_evidence}.",
          if(!is.na(causal_contrast_data_scale)) {
            glue(" This contrast amounts to a {causal_contrast_data_scale} expected difference on the data scale.")
          } else {
            ""
          }
        )
      )
    ) %>%
    dplyr::ungroup()
  result <- glue::glue(
    "\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}"
  )
  return(result)
}

