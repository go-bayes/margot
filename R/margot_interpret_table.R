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
margot_interpret_marginal <- function(df, type = c("RD", "RR"), estimand = NULL, order = "default", original_df = NULL) {
  .Deprecated("margot_interpret_marginal")
  margot_interpret_marginal(df = df, type = type, estimand = estimand, order = order, original_df = original_df)
}
#   type <- match.arg(type)
#
#   cli::cli_alert_info("Starting interpretation of causal effect estimates...")
#
#   # Define estimand descriptions
#   estimand_description <- if (!is.null(estimand)) {
#     dplyr::case_when(
#       estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
#       estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
#       estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
#       estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
#       estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
#       TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
#     )
#   } else {
#     NULL
#   }
#
#   cli::cli_alert_info("Processing and interpreting data...")
#
#   # Process df via group_tab to ensure 'Estimate' variable is present
#   df <- group_tab(df, type = type, order = order)
#
#   # If original_df is provided, back-transform estimates
#   if (!is.null(original_df)) {
#     df <- back_transform_estimates(df, original_df)
#   }
#
#   # Determine the effect size column
#   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
#     "E[Y(1)]/E[Y(0)]"
#   } else {
#     cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
#   }
#
#   # Determine if we have original scale results
#   has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)
#
#   # Define the null_value based on type
#   null_value <- ifelse(type == "RR", 1, 0)
#
#   interpretation <- df %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       # Determine if the variable was log-transformed
#       was_log_transformed = grepl("_log_", original_var_name),
#       # Define evidence_strength based on E_Val_bound and CI
#       evidence_strength = dplyr::case_when(
#         (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ cli::col_green('**the evidence for causality is strong**'),
#         (`2.5 %` > null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) | (`97.5 %` < null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) ~ cli::col_blue('**there is evidence for causality**'),
#         (`2.5 %` > null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) | (`97.5 %` < null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) ~ cli::col_yellow('**the evidence for causality is weak**'),
#         TRUE ~ cli::col_red('**the evidence for causality is not reliable**')
#       ),
#       # Units
#       unit = ifelse(!is.na(unit) & unit != "", unit, ""),
#       # Define estimate_lab
#       estimate_lab = paste0(
#         round(!!rlang::sym(effect_size_col), 3), " (",
#         round(`2.5 %`, 3), ", ",
#         round(`97.5 %`, 3), ")"
#       ),
#       # Define estimate_lab_original if available
#       estimate_lab_original = if (has_original_scale) {
#         if (unit != "") {
#           paste0(
#             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
#             round(`2.5 %_original`, 3), " to ",
#             round(`97.5 %_original`, 3), " ", unit, ")"
#           )
#         } else {
#           paste0(
#             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
#             round(`2.5 %_original`, 3), ", ",
#             round(`97.5 %_original`, 3), ")"
#           )
#         }
#       } else {
#         NA_character_
#       },
#       outcome_interpretation = glue::glue(
#         "For '{outcome}', the effect estimate ({type}) is {estimate_lab}. ",
#         "{if (has_original_scale) paste0('On the original data scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
#         "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
#         "{if (E_Val_bound > 1) paste0('At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of ', E_Val_bound, ' to negate the observed effect. Weaker confounding would not overturn it. ') else ''}",
#         "Here, {evidence_strength}."
#       )
#     ) %>%
#     dplyr::ungroup()
#
#   # Compile results
#   interpretation_text <- paste(interpretation$outcome_interpretation, collapse = '\n\n')
#
#   cli::cli_alert_success("Interpretation completed successfully!")
#
#   # Return results as a list
#   return(list(
#     estimand_description = estimand_description,
#     interpretation = interpretation_text
#   ))
# }
#
