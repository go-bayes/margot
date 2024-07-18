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
#' @seealso \code{\link{margot_model_tab}}
#'
margot_model_evalue <- function(model_output, scale = c("RD", "RR"), new_name = "character_string", delta = 1, sd = 1) {
  scale <- match.arg(scale)

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

  # Calculate E-values and append to the summary table
  if (scale == "RD") {
    evalout <- as.data.frame(round(
      EValue::evalues.OLS(
        tab_tmle[1, "E[Y(1)]-E[Y(0)]"],
        se = tab_tmle[1, "standard_error"],
        sd = sd,
        delta = delta,
        true = 0
      ),
      3
    ))
  } else {
    evalout <- as.data.frame(round(EValue::evalues.RR(
      tab_tmle[1, "E[Y(1)]/E[Y(0)]"],
      lo = tab_tmle[1, "2.5 %"],
      hi = tab_tmle[1, "97.5 %"],
      true = 1
    ),
    3))
  }

  evalout2 <- evalout[2, , drop = FALSE] # Keep it as a dataframe
  evalout3 <- evalout2 |>
    dplyr::select_if(~ !any(is.na(.))) |>
    `colnames<-`(c("E_Value", "E_Val_bound"))

  tab <- tab_tmle |>
    cbind(evalout3) |>
    dplyr::select(-standard_error) # Correctly reference the column to exclude

  return(tab)
}
