#' Summarise LMTP Output into a Data Frame
#'
#' This function takes the output from `lmtp::lmtp_contrast()` and creates a data frame summarising the
#' estimates. It allows for scaling the estimates as either risk differences (RD) or risk ratios (RR).
#' The resulting data frame includes the estimate, standard error, and 95% confidence interval.
#'
#' @param lmtp_output The output object from `lmtp::lmtp_contrast()`.
#' @param scale A character string specifying the scale of the estimate: "RD" or "RR". Default is "RD".
#' @param new_name A character string to name the row of the output data frame, representing the treatment
#'   contrast being summarised.
#' @return A data frame with four columns: the estimate under the specified scale, its standard error,
#'   and the lower and upper bounds of the 95% confidence interval. The row name is set to `new_name`.
#' @keywords internal
margot_lmtp_tab <- function(lmtp_output,
                            scale = c("RD", "RR"),
                            new_name = "") {
  scale <- match.arg(scale)
  require(dplyr)

  # extract theta, se, conf.low, conf.high from either old or new lmtp output
  if (!is.null(lmtp_output$vals)) {
    vals <- lmtp_output$vals
    theta <- vals$theta
    se <- vals$std.error
    conf_low <- vals$conf.low
    conf_high <- vals$conf.high
  } else if (!is.null(lmtp_output$estimates)) {
    est <- lmtp_output$estimates
    theta <- est$estimate
    se <- est$std.error
    conf_low <- est$conf.low
    conf_high <- est$conf.high
  } else {
    stop("Unrecognised LMTP output: no 'vals' or 'estimates' component")
  }

  # assemble into a 4‑column data frame
  tab_tmle <- data.frame(theta, se, conf_low, conf_high, check.names = FALSE)

  # assign effect‑size column names
  if (scale == "RD") {
    colnames(tab_tmle) <- c("E[Y(1)]-E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
  } else {
    colnames(tab_tmle) <- c("E[Y(1)]/E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
  }

  # round numeric columns and set the row name
  tab_tmle <- tab_tmle |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  rownames(tab_tmle) <- new_name

  return(tab_tmle)
}
