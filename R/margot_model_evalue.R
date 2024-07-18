#' Combine Model Summary and E-Value Calculation
#'
#' This function first creates a summary table from the output of `lmtp::lmtp_contrast()` or a causal forest model using
#' `margot_model_tab`, specifying the desired scale (RD or RR) and a new name for the row. It then calculates
#' E-values for the estimates in the table to assess the potential impact of unmeasured confounding,
#' appending these values to the summary table.
#'
#' @param model_output The output from `lmtp::lmtp_contrast()` or a causal forest model, to be summarized and analyzed for E-values.
#' @param scale Character string specifying the scale of the estimate to be used in the summary table and
#' E-value calculation. Valid options are "RD" (risk difference) or "RR" (risk ratio). Default is "RD". This parameter is ignored for causal forest models.
#' @param new_name Character string to name the row in the output summary table, representing the treatment
#' contrast. This name will be applied to the first row of the summary table.
#' @param delta The hypothesized increase in outcome for RD scale calculations. Used only when `scale` is "RD".
#' Default value is 1.
#' @param sd The standard deviation of the outcome for RD scale calculations. Used only when `scale` is "RD".
#' Default value is 1.
#'
#' @return A data frame with the original estimates and their E-values. The table includes columns for the
#' estimate (either RD or RR), its E-Value, and the E-Value lower bound, excluding the 'standard_error' column.
#'
#' @examples
#' \dontrun{
#' # Assuming `contrast_output` is the result from `lmtp::lmtp_contrast()`
#' summary_evalues <- margot_model_evalue(
#'   model_output = contrast_output,
#'   scale = "RD",
#'   new_name = "Treatment Effect"
#' )
#' print(summary_evalues)
#' }
#'
#' @export
#' @importFrom EValue evalues.OLS evalues.RR
#' @import dplyr select_if cbind select
#' @seealso \code{\link{margot_model_tab}}, \code{\link{lmtp_evalue_tab}} for the underlying functions used.
#'
margot_model_evalue <- function(model_output, scale = c("RD", "RR"), new_name = "character_string", delta = 1, sd = 1) {
  scale <- match.arg(scale)

  # Step 1: Generate the summary table from model_output
  tab_model <- margot_model_tab(
    model_output = model_output,
    scale = scale,
    new_name = new_name
  )

  # Step 2: Calculate E-values and append to the summary table
  if (scale == "RD") {
    tab_evalue <- tab_model %>%
      dplyr::mutate(
        E_value = EValue::evalues.OLS(
          est = .data[[1]],
          se = .data[[2]],
          delta = delta,
          sd = sd
        )[1, "E-value"],
        E_value_lower = EValue::evalues.OLS(
          est = .data[[1]],
          se = .data[[2]],
          delta = delta,
          sd = sd
        )[1, "lower"]
      ) %>%
      dplyr::select(-standard_error)
  } else if (scale == "RR") {
    tab_evalue <- tab_model %>%
      dplyr::mutate(
        E_value = EValue::evalues.RR(
          est = .data[[1]],
          lower = .data[[3]],
          upper = .data[[4]]
        )[1, "E-value"],
        E_value_lower = EValue::evalues.RR(
          est = .data[[1]],
          lower = .data[[3]],
          upper = .data[[4]]
        )[1, "lower"]
      ) %>%
      dplyr::select(-standard_error)
  } else {
    stop("Unsupported scale type for E-value calculation")
  }

  return(tab_evalue)
}
