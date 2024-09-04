#' Combine LMTP Summary and E-Value Calculation
#'
#' This function first creates a summary table from the output of `lmtp::lmtp_contrast()` using
#' `margot_tab_lmtp`, specifying the desired scale (RD or RR) and a new name for the row. It then calculates
#' E-values for the estimates in the table to assess the potential impact of unmeasured confounding,
#' appending these values to the summary table.
#'
#' @param lmtp_output The output from `lmtp::lmtp_contrast()`, to be summarized and analyzed for E-values.
#' @param scale Character string specifying the scale of the estimate to be used in the summary table and
#' E-value calculation. Valid options are "RD" (risk difference) or "RR" (risk ratio). Default is "RD".
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
#' summary_evalues <- margot_lmtp_evalue(
#'   lmtp_output = contrast_output,
#'   scale = "RD",
#'   new_name = "Treatment Effect"
#' )
#' print(summary_evalues)
#' }
#'
#' @export
#' @importFrom EValue evalues.OLS evalues.RR
#' @import dplyr
#' @seealso \code{\link{margot_tab_lmtp}}, \code{\link{lmtp_evalue_tab}} for the underlying functions used.
#'
margot_lmtp_evalue <- function(lmtp_output, scale = c("RD", "RR"), new_name = "character_string", delta = 1, sd = 1) {
  # Step 1: Generate the summary table from lmtp_output
  tab_lmtp <- margot_lmtp_tab(
    lmtp_output = lmtp_output,
    scale = scale,
    new_name = new_name
  )

  # Step 2: Calculate E-values and append to the summary table
  tab_evalue <- lmtp_evalue_tab(
    x = tab_lmtp,
    delta = delta,
    sd = sd,
    scale = scale
  )

  return(tab_evalue)
}
