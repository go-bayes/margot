#' Group and Annotate Treatment Effect Estimates
#'
#' This function arranges and annotates a data frame based on specified
#' types of treatment effect estimates (RR or RD). It supports different sorting
#' options including default descending, alphabetical, and custom order.
#' It now also handles original scale estimates when available.
#'
#' @param df Data frame containing the variables of interest, or a list containing
#' the results dataframe and label mapping from transform_to_original_scale().
#' @param type Type of treatment effect to analyze. Expected values are 'RR' for Risk Ratio
#' and 'RD' for Risk Difference. Defaults to 'RD'.
#' @param order Specifies the order in which the outcomes should be arranged. Can be
#' 'default' for descending order of the estimate, 'alphabetical' for alphabetical order by outcome,
#' or 'custom' for a user-defined order. Default is 'default'.
#' @param custom_order A vector of custom ordering for the outcomes, applicable if `order` is set to 'custom'.
#' This should be a vector containing all outcomes in the desired order.
#'
#' @return A data frame that has been arranged based on the specified order and annotated
#' with treatment effect estimates, estimate labels, and evidence value annotations.
#' If original scale estimates are available, these will be included in the output.
#'
#' @details
#' The function now handles both transformed and original scale results. If original scale results
#' are available (indicated by the presence of columns with "_original" suffix), these will be included
#' in the output. The function also applies label mapping if provided.
#'
#' @examples
#' # Example using Risk Ratio (RR) and default sorting
#' result_df <- group_tab(df = analysis_df, type = 'RR')
#'
#' # Example using Risk Difference (RD) with alphabetical sorting
#' result_df <- group_tab(df = analysis_df, type = 'RD', order = 'alphabetical')
#'
#' # Example using custom sorting order
#' custom_order <- c("Outcome3", "Outcome1", "Outcome2")
#' result_df <- group_tab(df = analysis_df, type = 'RR', order = 'custom', custom_order = custom_order)
#'
#' # Example using output from transform_to_original_scale()
#' transformed_data <- transform_to_original_scale(results_df, original_df, label_mapping)
#' result_df <- group_tab(transformed_data, type = 'RD')
#'
#' @importFrom dplyr arrange mutate slice desc recode
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @keywords internal
group_tab <- function(df, type = c("RD", "RR"), order = c("default", "alphabetical", "custom"), custom_order = NULL) {
  require(dplyr)
  type <- match.arg(type)
  order <- match.arg(order)

  # Check if input is the list returned by transform_to_original_scale
  if (is.list(df) && "results_df" %in% names(df)) {
    results_df <- df$results_df
    label_mapping <- df$label_mapping
  } else {
    results_df <- df
    label_mapping <- NULL
  }

  # Ensure the 'outcome' column exists; if not, create from row names
  if (!"outcome" %in% names(results_df) && !is.null(rownames(results_df))) {
    results_df <- results_df %>% tibble::rownames_to_column(var = "outcome")
  } else if (!"outcome" %in% names(results_df)) {
    stop("No 'outcome' column or row names available to convert into an 'outcome' column.")
  }

  # Apply label mapping if available
  if (!is.null(label_mapping)) {
    results_df <- results_df %>%
      mutate(outcome = dplyr::recode(outcome, !!!label_mapping))
  }

  # Ensure 'outcome' is a character vector to facilitate proper alphabetical ordering
  results_df <- results_df %>%
    mutate(outcome = as.character(outcome))

  # Determine the column to sort by based on the type
  effect_column <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"

  # Apply ordering based on the specified 'order'
  if (order == "alphabetical") {
    results_df <- results_df %>% arrange(outcome)
  } else if (order == "custom" && !is.null(custom_order)) {
    results_df <- results_df %>% slice(match(custom_order, outcome))
  } else {  # default is alphabetical
    results_df <- results_df %>% arrange(outcome)
  }

  # Add Estimate categorization and label column
  results_df <- results_df %>% mutate(
    Estimate = factor(
      if (type == "RR") {
        ifelse(
          `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
          "positive",
          ifelse(`E[Y(1)]/E[Y(0)]` < 1 & `97.5 %` < 1, "negative", "not reliable")
        )
      } else {
        ifelse(
          `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
          "positive",
          ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0, "negative", "not reliable")
        )
      }
    ),
    estimate_lab = if (type == "RR") {
      paste(
        round(`E[Y(1)]/E[Y(0)]`, 3),
        " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
        sep = ""
      )
    } else {
      paste(
        round(`E[Y(1)]-E[Y(0)]`, 3),
        " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
        sep = ""
      )
    },
    label = estimate_lab  # Add this line to create the 'label' column
  )

  # Add original scale estimates if available
  if (paste0(effect_column, "_original") %in% names(results_df)) {
    results_df <- results_df %>% mutate(
      estimate_lab_original = paste(
        round(.data[[paste0(effect_column, "_original")]], 3),
        " (", round(.data[["2.5 %_original"]], 3), "-", round(.data[["97.5 %_original"]], 3), ")",
        sep = ""
      )
    )
  }

  return(results_df)
}
# old
# group_tab <- function(df, type = c("RR", "RD"), order = c("default", "alphabetical", "custom"), custom_order = NULL) {
#   require(dplyr)
#   type <- match.arg(type)
#   order <- match.arg(order)
#
#   # Ensure the 'outcome' column exists; if not, create from row names
#   if (!"outcome" %in% names(df) && !is.null(rownames(df))) {
#     df <- df %>% tibble::rownames_to_column(var = "outcome")
#   } else if (!"outcome" %in% names(df)) {
#     stop("No 'outcome' column or row names available to convert into an 'outcome' column.")
#   }
#
#   # Determine the column to sort by based on the type
#   effect_column <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
#
#   # Apply ordering based on the specified 'order'
#   if (order == "alphabetical") {
#     df <- df %>% arrange(outcome)
#   } else if (order == "custom" && !is.null(custom_order)) {
#     df <- df %>% slice(match(custom_order, df$outcome))
#   } else {  # default is descending order by effect size
#     df <- df %>% arrange(desc(!!sym(effect_column)))
#   }
#
#   # Add Estimate categorization
#   df <- df %>% mutate(
#     Estimate = factor(
#       if (type == "RR") {
#         ifelse(
#           `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
#           "positive",
#           ifelse(`E[Y(1)]/E[Y(0)]` < 1 & `97.5 %` < 1, "negative", "not reliable")
#         )
#       } else {
#         ifelse(
#           `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
#           "positive",
#           ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0, "negative", "not reliable")
#         )
#       }
#     ),
#     estimate_lab = if (type == "RR") {
#       paste(
#         round(`E[Y(1)]/E[Y(0)]`, 3),
#         " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
#         " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
#         sep = ""
#       )
#     } else {
#       paste(
#         round(`E[Y(1)]-E[Y(0)]`, 3),
#         " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
#         " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
#         sep = ""
#       )
#     }
#   )
#
#   return(df)
# }
