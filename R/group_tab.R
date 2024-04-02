#' Group and Categorise LMTP Estimates with E-Values
#'
#' This function takes a dataframe combining multiple outputs from `margot_lmtp_evalue` or `tab_evalue`,
#' categorises the estimates based on their confidence intervals and E-values, and prepares the data
#' for further analysis or plotting. Estimates are categorised as "positive", "negative", or "not reliable".
#' The function also arranges the data based on the estimate size and adds a column for a formatted label
#' of the estimate and its confidence interval, including the E-value.
#'
#' @param df A dataframe resulting from binding multiple outputs from `margot_lmtp_evalue` or `tab_evalue`.
#' @param type A character string specifying the scale of the estimate to be categorised and formatted.
#'   Valid options are "RR" for risk ratio or "RD" for risk difference. Default is "RR".
#'
#' @return A dataframe with estimates categorised as "positive", "negative", or "not reliable" based on their
#'   confidence intervals and E-values. The dataframe includes a new `estimate_lab` column with formatted
#'   estimate and confidence interval labels, and an `Estimate` column indicating the category of the estimate.
#'   Rows are arranged by the size of the estimate and renamed to include the outcome variable.
#'
#' @examples
#' \dontrun{
#' # Assuming `tab_all_perceived_support` is a combined dataframe of LMTP estimates
#' group_tab_all_perceived_support <- group_tab(tab_all_perceived_support)
#' print(group_tab_all_perceived_support)
#' }
#' @export
#' @importFrom dplyr arrange mutate across
#' @importFrom tibble rownames_to_column
group_tab <- function(df, type = c("RR", "RD")) {
  type <- match.arg(type)

  require(dplyr)

  if (type == "RR") {
    out <- df %>%
      arrange(desc(`E[Y(1)]/E[Y(0)]`)) %>%
      dplyr::mutate(Estimate  = as.factor(
        ifelse(
          `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
          "positive",
          ifelse(`E[Y(1)]/E[Y(0)]` < 1 &
                   `97.5 %` < 1, "negative",
                 "not reliable")
        )
      )) %>%
      rownames_to_column(var = "outcome") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(
          `E[Y(1)]/E[Y(0)]`,
          " (",
          `2.5 %`,
          "-",
          `97.5 %`,
          ")",
          " [EV ",
          `E_Value`,
          "/",
          `E_Val_bound`,
          "]"
        )
      )
  } else {
    out <- df %>%
      arrange(desc(`E[Y(1)]-E[Y(0)]`)) %>%
      dplyr::mutate(Estimate  = as.factor(
        ifelse(
          `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
          "positive",
          ifelse(`E[Y(1)]-E[Y(0)]` < 0 &
                   `97.5 %` < 0, "negative",
                 "not reliable")
        )
      )) %>%
      rownames_to_column(var = "outcome") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(
          `E[Y(1)]-E[Y(0)]`,
          " (",
          `2.5 %`,
          "-",
          `97.5 %`,
          ")",
          " [EV ",
          `E_Value`,
          "/",
          `E_Val_bound`,
          "]"
        )
      )
  }

  return(out)
}
