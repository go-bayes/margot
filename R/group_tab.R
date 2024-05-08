#' Group and Annotate Treatment Effect Estimates
#'
#' This function arranges and annotates a data frame based on specified
#' types of treatment effect estimates (RR or RD). It supports different sorting
#' options including default descending, alphabetical, and custom order.
#'
#' @param df Data frame containing the variables of interest.
#' @param type Type of treatment effect to analyze. Expected values are 'RR' for Risk Ratio
#' and 'RD' for Risk Difference. Defaults to 'RR'.
#' @param order Specifies the order in which the outcomes should be arranged. Can be
#' 'default' for descending order of the estimate, 'alphabetical' for alphabetical order by outcome,
#' or 'custom' for a user-defined order. Default is 'default'.
#' @param custom_order A vector of custom ordering for the outcomes, applicable if `order` is set to 'custom'.
#' This should be a vector containing all outcomes in the desired order.
#'
#' @return A data frame that has been arranged based on the specified order and annotated
#' with treatment effect estimates, estimate labels, and evidence value annotations.
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
#' @importFrom dplyr arrange mutate
#' @importFrom tibble rownames_to_column
#' @export
group_tab <- function(df, type = c("RR", "RD"), order = c("default", "alphabetical", "custom"), custom_order = NULL) {
  require(dplyr)

  type <- match.arg(type)
  order <- match.arg(order)

  # Handle custom ordering or default descending order based on the effect size
  measure_column <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"

  if (order == "alphabetical") {
    df <- df %>% arrange(outcome)
  } else if (order == "custom" && !is.null(custom_order)) {
    df <- df %>% slice(match(custom_order, outcome))
  } else {  # Default ordering
    df <- df %>% arrange(desc(!!sym(measure_column)))
  }

  # Create the Estimate categorization based on the type
  df <- df %>% mutate(
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
    )
  )

  # Create the estimate_lab for display in the plot
  df <- df %>%
    mutate(estimate_lab = if (type == "RR") {
      paste(`E[Y(1)]/E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/", `E_Val_bound`, "]", sep = "")
    } else {
      paste(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/", `E_Val_bound`, "]", sep = "")
    })

  return(df)
}

# old
# group_tab <- function(df, type = c("RR", "RD")) {
#   type <- match.arg(type)
#
#   require(dplyr)
#
#   if (type == "RR") {
#     out <- df %>%
#       arrange(desc(`E[Y(1)]/E[Y(0)]`)) %>%
#       dplyr::mutate(Estimate  = as.factor(
#         ifelse(
#           `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
#           "positive",
#           ifelse(`E[Y(1)]/E[Y(0)]` < 1 & `97.5 %` < 1, "negative", "not reliable")
#         )
#       )) %>%
#       tibble::rownames_to_column(var = "outcome") %>%
#       dplyr::mutate(
#         across(.cols = where(is.numeric), .fns = ~round(.x, digits = 3)),
#         estimate_lab = paste0(
#           `E[Y(1)]/E[Y(0)]`,
#           " (",
#           `2.5 %`,
#           "-",
#           `97.5 %`,
#           ")",
#           " [EV ",
#           `E_Value`,
#           "/",
#           `E_Val_bound`,
#           "]"
#         )
#       )
#   } else {
#     out <- df %>%
#       arrange(desc(`E[Y(1)]-E[Y(0)]`)) %>%
#       dplyr::mutate(Estimate  = as.factor(
#         ifelse(
#           `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
#           "positive",
#           ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0, "negative", "not reliable")
#         )
#       )) %>%
#       tibble::rownames_to_column(var = "outcome") %>%
#       dplyr::mutate(
#         across(.cols = where(is.numeric), .fns = ~round(.x, digits = 3)),
#         estimate_lab = paste0(
#           `E[Y(1)]-E[Y(0)]`,
#           " (",
#           `2.5 %`,
#           "-",
#           `97.5 %`,
#           ")",
#           " [EV ",
#           `E_Value`,
#           "/",
#           `E_Val_bound`,
#           "]"
#         )
#       )
#   }
#
#   return(out)
# }

