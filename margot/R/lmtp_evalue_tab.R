#' Calculate E-values for LMTP Output
#'
#' This function takes the output from `margot_tab_lmtp()`, which contains estimates of treatment effects,
#' and calculates E-values to assess the robustness of the estimates to potential unmeasured confounding.
#' E-values quantify the minimum strength of association, on the risk ratio scale, that an unmeasured
#' confounder would need to have with both the treatment and the outcome, to fully explain away the
#' observed association. The function supports both risk differences (RD) and risk ratios (RR) scales.
#'
#' @param x A data frame output from `margot_tab_lmtp()` containing the estimates of interest.
#' @param delta The hypothesized increase in outcome, used only when `scale` is "RD". Default is 1.
#' @param sd The standard deviation of the outcome, used only when `scale` is "RD". Default is 1.
#' @param scale A character string indicating the scale of the estimate: "RD" for risk difference,
#'   or "RR" for risk ratio. Default is "RD".
#'
#' @return A data frame similar to `x`, with additional columns for E-Value and its lower bound, excluding
#'   the 'standard_error' column.
#'
#' @examples
#' \dontrun{
#' # Assuming 'tab_contrast_hours_charity_z_null' is a data frame output from `margot_lmtp_tab()`
#' lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RD")
#' lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RR")
#' }
#' @export
#' @importFrom EValue evalues.OLS evalues.RR
#' @importFrom dplyr select_if select
lmtp_evalue_tab <- function(x, delta = 1, sd = 1, scale = c("RD", "RR")) {
  require("EValue")
  require(dplyr)

  scale <- match.arg(scale)

  tab0 <- as.data.frame(x)

  if (scale == "RD") {
    evalout <- as.data.frame(round(
      EValue::evalues.OLS(
        tab0[1, 1],
        se = tab0[1, 2],
        sd = sd,
        delta = delta,
        true = 0
      ),
      3
    ))
  } else {
    evalout <- as.data.frame(round(EValue::evalues.RR(
      tab0[1, 1],
      lo = tab0[1, 3],
      hi = tab0[1, 4],
      true = 1
    ),
    3))
  }

  # Assuming evalout[2, ] fetches the row for E-Values, and you reshape it correctly.
  evalout2 <- evalout[2, , drop = FALSE] # Keep it as a dataframe
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.))) |>
    `colnames<-`(c("E_Value", "E_Val_bound"))

  # Bind evalout3 to tab0 while excluding 'standard_error'
  tab <- tab0 |>
    cbind(evalout3) |>
    select(-standard_error) # Correctly reference the column to exclude

  return(tab)
}
