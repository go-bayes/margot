#' This function interprets the output of causal effect analysis, providing a compact report.
#' It only reports coefficients and E-values with **Evidence** or **Strong evidence** for causality,
#' unless all estimates with E_Value > 1 (and E_Val_bound > 1) are requested.
#' Each outcome's interpretation starts with a separate paragraph heading.
#' Additionally, it includes a final paragraph indicating that all other effect estimates presented
#' either weak or unreliable evidence for causality.
#'
#' @param df Data frame containing causal effect estimates. Expected to include columns for
#'   outcome names, effect estimates, confidence intervals, E-values, and summary labels.
#' @param type Character string specifying the type of effect estimate. Must be either "RD"
#'   (Risk Difference) or "RR" (Risk Ratio). Default is "RD".
#' @param order Character string specifying the order of results. Default is "alphabetical".
#'   - `"alphabetical"`: Orders outcomes alphabetically.
#'   - `"magnitude"`: Orders outcomes by the absolute magnitude of the effect size in descending order.
#' @param original_df Optional data frame for back-transforming estimates to the original scale.
#' @param interpret_all_E_gt1 Logical. If `TRUE`, interprets any effect estimate with an `E_Value` > 1
#'   and a lower E-value bound > 1. Default is `FALSE`.
#'
#' @return A list containing one element:
#'   \item{interpretation}{A character string containing a compact interpretation of each outcome in
#'   `df`, including separate paragraph headings and sentence-cased descriptions, followed by a
#'   concluding paragraph if applicable.}
#'
#' @examples
#' \dontrun{
#' result <- margot_interpret_marginal(group_tab_output, type = "RD", interpret_all_E_gt1 = TRUE)
#' cat(result$interpretation)
#' }
#'
#' @importFrom dplyr case_when mutate rowwise ungroup filter
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence
#' @importFrom rlang sym
#' @export
margot_interpret_marginal <- function(df,
                                      type = c("RD", "RR"),
                                      order = "alphabetical",
                                      original_df = NULL,
                                      interpret_all_E_gt1 = FALSE) {
  type <- match.arg(type)

  if (order == "default") {
    warning("'default' order is deprecated. please use 'magnitude' instead.")
    order <- "magnitude"
  }

  if (!order %in% c("alphabetical", "magnitude")) {
    stop("invalid 'order' parameter. choose from 'alphabetical' or 'magnitude'.")
  }

  message("starting compact interpretation of causal effect estimates...")

  df <- group_tab(df, type = type, order = order)

  # fix after grouping or at the start of margot_interpret_marginal:
  if (!"unit" %in% names(df)) {
    df$unit <- rep("", nrow(df))
  }

  if (!is.null(original_df)) {
    df <- back_transform_estimates(df, original_df)
  }

  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column.")
  }

  has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)
  null_value <- ifelse(type == "RR", 1, 0)

  # when interpreting all estimates with E_Value > 1, require the lower bound to be > 1.
  if (interpret_all_E_gt1) {
    df_filtered <- df %>%
      dplyr::filter(E_Value > 1, E_Val_bound > 1) %>%
      dplyr::mutate(evidence_strength = "Evidence")
  } else {
    df_filtered <- df %>%
      dplyr::mutate(
        evidence_strength = dplyr::case_when(
          (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ "Strong evidence",
          (`2.5 %` > null_value & E_Val_bound > 1.1) | (`97.5 %` < null_value & E_Val_bound > 1.1) ~ "Evidence",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(evidence_strength))
  }

  total_outcomes <- nrow(df)
  reported_outcomes <- nrow(df_filtered)
  remaining_outcomes <- total_outcomes - reported_outcomes

  if (nrow(df_filtered) == 0) {
    message("no effect estimates with evidence or strong evidence for causality were found.")
    return(list(interpretation = "No reliable causal evidence detected for the reported outcomes."))
  }

  interpretation <- df_filtered %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      unit = ifelse(!is.na(unit) & unit != "", unit, ""),
      estimate_lab = paste0(
        round(!!rlang::sym(effect_size_col), 3), " (",
        round(`2.5 %`, 3), ", ",
        round(`97.5 %`, 3), ")"
      ),
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
      outcome_interpretation = glue::glue(
        "#### {outcome}\n\n",
        "The effect estimate ({type}) is {estimate_lab}. ",
        "{if (!is.na(estimate_lab_original)) paste0('On the original scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
        "E-value lower bound is {E_Val_bound}, indicating {tolower(evidence_strength)} for causality."
      ),
      outcome_interpretation = stringr::str_to_sentence(outcome_interpretation)
    ) %>%
    dplyr::ungroup()

  interpretation_text <- paste(interpretation$outcome_interpretation, collapse = "\n\n")

  if (remaining_outcomes > 0) {
    final_paragraph <- if (interpret_all_E_gt1) {
      "\n\nAll other effect estimates presented unreliable evidence for causality."
    } else {
      "\n\nAll other effect estimates presented either weak or unreliable evidence for causality."
    }
    final_paragraph <- stringr::str_to_sentence(final_paragraph)
    interpretation_text <- paste(interpretation_text, final_paragraph, sep = "\n\n")
  }

  message("compact interpretation completed successfully 👍")

  return(list(
    interpretation = interpretation_text
  ))
}
