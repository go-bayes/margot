#' Summarise LMTP Output into a Data Frame
#'
#' This function takes the output from `lmtp::lmtp_contrast()` and creates a data frame summarising the
#' estimates. It allows for scaling the estimates as either risk differences (RD) or risk ratios (RR).
#' The resulting data frame includes the estimate, standard error, and 95% confidence interval.
#'
#' @param lmtp_output The output object from `lmtp::lmtp_contrast()`.
#' @param scale A character string specifying the scale of the estimate. Valid options are "RD" for risk
#' difference and "RR" for risk ratio. Default is "RD".
#' @param new_name A character string to name the row of the output data frame, representing the treatment
#' contrast being summarised.
#' @return A data frame with four columns: the estimate under the specified scale, its standard error, and
#' the lower and upper bounds of the 95% confidence interval. Row names of the data frame are set to `new_name`.
#' @examples
#' \dontrun{
#' # Assuming `contrast_hours_charity_z_null` is output from `lmtp::lmtp_contrast()`
#' tab_contrast_hours_charity_z_null <- margot_lmtp_tab(
#'   contrast_hours_charity_z_null,
#'   scale = "RD",
#'   new_name = "relig service: hours volunteer"
#' )
#'  print(group_tab_all_perceived_support)
#' }
#' @export
margot_lmtp_tab <-
  function(lmtp_output,
           scale = c("RD", "RR"),
           new_name = "character_string") {
    scale <- match.arg(scale)

    require(dplyr)

    tab_tmle <- cbind.data.frame(
      lmtp_output$vals$theta,
      lmtp_output$vals$std.error,
      lmtp_output$vals$conf.low,
      lmtp_output$vals$conf.high
    )

    if (scale == "RD") {
      colnames(tab_tmle) <-
        c("E[Y(1)]-E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
    } else if (scale == "RR") {
      colnames(tab_tmle) <-
        c("E[Y(1)]/E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
    }

    tab_tmle_round <- tab_tmle |>
      dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = 3)))

    rownames(tab_tmle_round)[1] <- paste0(new_name)

    return(tab_tmle_round)
  }
