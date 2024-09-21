#' Interpret and Describe Causal Effect Estimates Using E-values (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use `margot_interpret_marginal()` instead.
#'
#' This function interprets the output of causal effect analysis, providing textual descriptions
#' of causal effect estimates. It categorises the strength of evidence for causality based on
#' E-values and confidence intervals, and generates a detailed interpretation of the effect
#' estimates according to specified causal scales (i.e., "causal_difference" or "risk_ratio")
#' and estimands. This function supports interpreting results on both the causal difference
#' and risk ratio scales.
#'
#' @param df Data frame containing causal effect estimates, expected to include columns for
#' outcome names, effect estimates (either differences or ratios), confidence intervals,
#' E-values, and a summary estimate label. The structure of `df` should align with the specified
#' `causal_scale`.
#' @param causal_scale Character string specifying the causal scale used in the analysis.
#' Currently supports "causal_difference" for differences in means or medians, and "risk_ratio"
#' for comparing ratios of probabilities or risks.
#' @param estimand Optional character string indicating the type of causal estimand interpreted: "PATE"
#' (Population Average Treatment Effect), "ATE" (Average Treatment Effect), "ATT" (Average
#' Treatment Effect in the Treated), "CATE" (Conditional Average Treatment Effect), or "LMTP"
#' (Longitudinal Modified Treatment Policy). Default is NULL.
#' @param order Character string specifying the order of results. Default is "default".
#'
#' @return A list containing two elements:
#'   \item{estimand_description}{A character string describing the specified estimand, or NULL if no estimand was provided.}
#'   \item{interpretation}{A character string containing a detailed interpretation of each outcome in `df`,
#'   including the causal contrast, E-values, and the strength of evidence for causality.}
#'
#' @examples
#' \dontrun{
#' # Assuming `group_tab_output` is the result from a causal analysis
#' result <- margot_interpret_table(group_tab_output, "causal_difference", "ATE")
#' cat(result$estimand_description)
#' cat(result$interpretation)
#'
#' # Without specifying an estimand
#' result <- margot_interpret_table(group_tab_output, "risk_ratio")
#' cat(result$interpretation)
#' }
#'
#' @export
#' @importFrom dplyr case_when mutate rowwise ungroup if_else
#' @importFrom glue glue
#' @importFrom cli cli_alert_danger cli_alert_warning
margot_interpret_table <- function(df, causal_scale, estimand = NULL, order = "default") {
  # Deprecation warning with emoji and color
  cli::cli_alert_danger("{cli::col_red(cli::symbol$warning)} {.strong The `margot_interpret_table()` function is deprecated.}")
  cli::cli_alert_warning("{cli::col_yellow(cli::symbol$info)} Please use {.code margot_interpret_marginal()} instead. See {.help [?margot_interpret_marginal]} for details.")

  require(dplyr)
  require(glue)
  require(tibble)

  # Rest of the function remains the same
  # ...

  # Define estimand descriptions
  estimand_description <- if (!is.null(estimand)) {
    dplyr::case_when(
      estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
      estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
      estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
      estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
      estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
      TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
    )
  } else {
    NULL
  }
  # Determine the correct type based on causal_scale
  effect_type <- if (causal_scale == "causal_difference") "RD" else "RR"

  # Use group_tab to ensure the dataframe is correctly formatted
  if (!"Estimate" %in% names(df) || !"outcome" %in% names(df)) {
    df <- group_tab(df, type = effect_type, order = order)
  }

  # Further checks to ensure the dataframe contains the necessary columns
  causal_contrast_column <- if (causal_scale == "causal_difference") {
    "E[Y(1)]-E[Y(0)]"
  } else {
    "E[Y(1)]/E[Y(0)]"
  }

  if (!causal_contrast_column %in% names(df)) {
    stop(paste("Dataframe does not contain the required column:", causal_contrast_column))
  }

  # data processing and interpretation
  interpretation <- df %>%
    dplyr::mutate(
      causal_contrast = round(.data[[causal_contrast_column]], 3),
      formatted_strength = case_when(
        E_Val_bound <= 1 | (`2.5 %` <= 0 & `97.5 %` >= 0) ~ "**the evidence for causality is not reliable**",
        E_Val_bound > 1 & E_Val_bound < 1.1 ~ "**the evidence for causality is weak**",
        E_Val_bound > 2 ~ "**the evidence for causality is strong**",
        TRUE ~ "**there is evidence for causality**"
      ),
      confounder_warning = if_else(E_Val_bound > 1,
                                   paste0("At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of ", E_Val_bound, " to negate the observed effect. Weaker confounding would not overturn it. "),
                                   ""),
      outcome_interpretation = glue(
        "For '{outcome}', the effect estimate on the {causal_scale} scale is {causal_contrast} [{`2.5 %`}, {`97.5 %`}]. ","The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ", "{confounder_warning}", "Here, {formatted_strength}."
      )
    )

  # compile results
  interpretation_text <- paste(interpretation$outcome_interpretation, collapse = '\n\n')

  # Return results as a list
  return(list(
    estimand_description = estimand_description,
    interpretation = interpretation_text
  ))
}

