#' This function interprets the output of causal effect analysis, providing a compact report.
#' It only reports coefficients and E-values with **Evidence** or **Strong evidence** for causality.
#' The language is suitable for scientific reports, avoiding explanations of treatment effects and E-values.
#' Each outcome's interpretation starts with a separate paragraph heading using `####`.
#' Additionally, it includes a final paragraph indicating that all other effect estimates presented
#' either weak or unreliable evidence for causality.
#'
#' @param df Data frame containing causal effect estimates. Expected to include columns for
#' outcome names, effect estimates, confidence intervals, E-values, and summary labels.
#' Can also be the list output from `transform_to_original_scale()`.
#' @param type Character string specifying the type of effect estimate. Must be either "RD"
#' (Risk Difference) or "RR" (Risk Ratio). Default is "RD".
#' @param order Character string specifying the order of results. Default is "alphabetical".
#'   - `"alphabetical"`: Orders outcomes alphabetically.
#'   - `"magnitude"`: Orders outcomes by the absolute magnitude of the effect size in descending order.
#' @param original_df Optional data frame for back-transforming estimates to the original scale.
#'
#' @return A list containing one element:
#'   \item{interpretation}{A character string containing a compact interpretation of each outcome in `df`,
#'   including separate paragraph headings and sentence-cased descriptions, followed by a concluding paragraph
#'   if applicable.}
#'
#' @examples
#' \dontrun{
#' # Assuming `group_tab_output` is the result from a causal analysis
#' result <- margot_interpret_marginal(group_tab_output, type = "RD")
#' cat(result$interpretation)
#'
#' # Using Risk Ratio without specifying an estimand
#' result <- margot_interpret_marginal(group_tab_output, type = "RR")
#'
#' # Using output from transform_to_original_scale()
#' transformed_data <- transform_to_original_scale(results_df, original_df, label_mapping)
#' result <- margot_interpret_marginal(transformed_data, type = "RD")
#' }
#'
#' @importFrom dplyr case_when mutate rowwise ungroup if_else filter arrange desc
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence
#' @export
margot_interpret_marginal <- function(df, type = c("RD", "RR"), order = "alphabetical", original_df = NULL) {
  type <- match.arg(type)

  # Handle order parameter
  if (order == "default") {
    warning("'default' order is deprecated. Please use 'magnitude' instead.")
    order <- "magnitude"
  }

  # Validate order parameter
  if (!order %in% c("alphabetical", "magnitude")) {
    stop("Invalid 'order' parameter. Choose from 'alphabetical' or 'magnitude'.")
  }

  message("Starting compact interpretation of causal effect estimates...")

  # Process df via group_tab to ensure necessary columns are present
  df <- group_tab(df, type = type, order = order)

  # If original_df is provided, back-transform estimates
  if (!is.null(original_df)) {
    df <- back_transform_estimates(df, original_df)
  }

  # Determine the effect size column
  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column.")
  }

  # Determine if we have original scale results
  has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)

  # Define the null_value based on type
  null_value <- ifelse(type == "RR", 1, 0)

  # Filter for Evidence and Strong evidence
  df_filtered <- df %>%
    dplyr::mutate(
      evidence_strength = dplyr::case_when(
        (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ "Strong evidence",
        (`2.5 %` > null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) | (`97.5 %` < null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) ~ "Evidence",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(evidence_strength))

  # Calculate remaining outcomes for the final paragraph
  total_outcomes <- nrow(df)
  reported_outcomes <- nrow(df_filtered)
  remaining_outcomes <- total_outcomes - reported_outcomes

  if (nrow(df_filtered) == 0) {
    message("No effect estimates with Evidence or Strong evidence for causality were found.")
    return(list(interpretation = "No reliable causal evidence detected for the reported outcomes."))
  }

  # Create interpretation for each row
  interpretation <- df_filtered %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Units
      unit = ifelse(!is.na(unit) & unit != "", unit, ""),
      # Define estimate_lab
      estimate_lab = paste0(
        round(!!rlang::sym(effect_size_col), 3), " (",
        round(`2.5 %`, 3), ", ",
        round(`97.5 %`, 3), ")"
      ),
      # Define estimate_lab_original if available
      estimate_lab_original = if (has_original_scale) {
        if (unit != "") {
          paste0(
            round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
            round(`2.5 %_original`, 3), " to ",
            round(`97.5 %_original`, 3), " ", unit, ")"
          )
        } else {
          paste0(
            round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
            round(`2.5 %_original`, 3), ", ",
            round(`97.5 %_original`, 3), ")"
          )
        }
      } else {
        NA_character_
      },
      # Construct the interpretation with heading and sentence-cased text
      outcome_interpretation = glue::glue(
        "#### {outcome}",
        "",
        "The effect estimate ({type}) is {estimate_lab}. ",
        "{if (!is.na(estimate_lab_original)) paste0('On the original scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
        "E-value lower bound is {E_Val_bound}, indicating {tolower(evidence_strength)} for causality."
      ),
      # Convert the interpretation text to sentence case
      outcome_interpretation = stringr::str_to_sentence(outcome_interpretation)
    ) %>%
    dplyr::ungroup()

  # Combine interpretations with double newlines for proper Markdown formatting
  interpretation_text <- paste(interpretation$outcome_interpretation, collapse = "\n\n")

  # Add final paragraph indicating all other estimates have weak or unreliable evidence
  if (remaining_outcomes > 0) {
    final_paragraph <- "\n\nAll other effect estimates presented either weak or unreliable evidence for causality."
    # Ensure sentence case
    final_paragraph <- stringr::str_to_sentence(final_paragraph)
    interpretation_text <- paste(interpretation_text, final_paragraph, sep = "\n\n")
  }

  message("Compact interpretation completed successfully 👍")

  # Return results as a list
  return(list(
    interpretation = interpretation_text
  ))
}
