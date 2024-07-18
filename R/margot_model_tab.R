#' Summarise LMTP or Causal Forest Output into a Data Frame
#'
#' This function takes the output from `lmtp::lmtp_contrast()` or a causal forest model and creates a data frame summarising the
#' estimates. It allows for scaling the estimates as either risk differences (RD) or risk ratios (RR) for LMTP models.
#' For causal forest models, the scale is always "RD".
#' The resulting data frame includes the estimate, standard error, and 95% confidence interval.
#'
#' @param model_output The output object from `lmtp::lmtp_contrast()` or a causal forest model.
#' @param scale A character string specifying the scale of the estimate. Valid options are "RD" for risk
#' difference and "RR" for risk ratio. Default is "RD". This parameter is ignored for causal forest models.
#' @param new_name A character string to name the row of the output data frame, representing the treatment
#' contrast being summarised.
#' @return A data frame with four columns: the estimate under the specified scale, its standard error, and
#' the lower and upper bounds of the 95% confidence interval. Row names of the data frame are set to `new_name`.
#' @examples
#' \dontrun{
#' # Assuming `contrast_hours_charity_z_null` is output from `lmtp::lmtp_contrast()`
#' tab_contrast_hours_charity_z_null <- margot_model_tab(
#'   contrast_hours_charity_z_null,
#'   scale = "RD",
#'   new_name = "relig service: hours volunteer"
#' )
#' print(tab_contrast_hours_charity_z_null)
#' }
#' @export
margot_model_tab <- function(model_output, scale = c("RD", "RR"), new_name = "character_string") {
  scale <- match.arg(scale)

  require(dplyr)

  # Function to create the summary data frame
  create_summary_df <- function(estimate, std.error, conf.low, conf.high, scale, new_name) {
    tab <- cbind.data.frame(
      estimate,
      std.error,
      conf.low,
      conf.high
    )

    if (scale == "RD") {
      colnames(tab) <- c("E[Y(1)]-E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
    } else if (scale == "RR") {
      colnames(tab) <- c("E[Y(1)]/E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
    }

    tab_round <- tab |>
      dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = 3)))

    rownames(tab_round)[1] <- paste0(new_name)

    return(tab_round)
  }

  if ("lmtp_contrast" %in% class(model_output)) {
    # Processing LMTP model output
    tab_tmle <- create_summary_df(
      model_output$vals$theta,
      model_output$vals$std.error,
      model_output$vals$conf.low,
      model_output$vals$conf.high,
      scale,
      new_name
    )
  } else if ("causal_forest" %in% class(model_output)) {
    # Processing causal forest model output
    ate_summary <- average_treatment_effect(model_output)
    theta <- ate_summary[[1]]
    std.error <- ate_summary[[2]]
    conf.low <- theta - qnorm(0.975) * std.error
    conf.high <- theta + qnorm(0.975) * std.error

    tab_tmle <- create_summary_df(
      theta,
      std.error,
      conf.low,
      conf.high,
      "RD",
      new_name
    )
  } else {
    stop("Unsupported model output type")
  }

  return(tab_tmle)
}
