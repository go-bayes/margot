#' Combine Model Summary and E-Value Calculation for Various Causal Models
#'
#' This function creates a summary table from the output of various causal models,
#' including `lmtp::lmtp_contrast()`, `grf::causal_forest()`, and `grf::multi_arm_causal_forest()`.
#' It calculates E-values for the estimates to assess the potential impact of unmeasured confounding,
#' appending these values to the summary table.
#'
#' @param model_output The output from a supported causal model. Supported types include:
#'   \itemize{
#'     \item Output from `lmtp::lmtp_contrast()`
#'     \item Output from `grf::causal_forest()`
#'     \item Output from `grf::multi_arm_causal_forest()`
#'     \item A data frame with columns 'estimate' and 'std.err'
#'   }
#' @param scale Character string specifying the scale of the estimate to be used in the summary table and
#'   E-value calculation. Valid options are "RD" (risk difference) or "RR" (risk ratio). Default is "RD".
#'   This parameter is ignored for causal forest models, which always use "RD".
#' @param new_name Character string to name the row(s) in the output summary table, representing the treatment
#'   contrast(s). For multi-arm causal forests, this will be combined with the contrast information.
#' @param delta The hypothesized increase in outcome for RD scale calculations. Used only when `scale` is "RD".
#'   Default value is 1.
#' @param sd The standard deviation of the outcome for RD scale calculations. Used only when `scale` is "RD".
#'   Default value is 1.
#' @param subset An optional logical vector for subsetting the data when the model is a `grf` model. Default is `NULL`.
#'
#' @return A data frame with the original estimates and their E-values. The table includes columns for the
#'   estimate (either RD or RR), its confidence interval, E-Value, and the E-Value lower bound.
#'   For multi-arm causal forests, multiple rows will be returned, one for each contrast.
#'
#' @examples
#' \dontrun{
#' # For lmtp_contrast output
#' summary_evalues <- margot_model_evalue(
#'   model_output = lmtp_contrast_output,
#'   scale = "RD",
#'   new_name = "Treatment Effect"
#' )
#'
#' # For causal_forest output
#' cf_summary <- margot_model_evalue(
#'   model_output = causal_forest_output,
#'   new_name = "Causal Forest Effect"
#' )
#'
#' # For multi_arm_causal_forest output
#' macf_summary <- margot_model_evalue(
#'   model_output = multi_arm_cf_output,
#'   new_name = "Multi-Arm Effect"
#' )
#'
#' # For direct input of estimate and standard error
#' direct_summary <- margot_model_evalue(
#'   model_output = data.frame(estimate = 0.5, std.err = 0.1),
#'   new_name = "Direct Effect"
#' )
#' }
#'
#' @export
#' @importFrom EValue evalues.OLS evalues.RR
#' @importFrom dplyr mutate across select_if
#' @importFrom stats qnorm
margot_model_evalue <- function(model_output, scale = c("RD", "RR"), new_name = "character_string", delta = 1, sd = 1, subset = NULL) {
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

    tab_round <- tab %>%
      dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = 3)))

    rownames(tab_round)[1] <- paste0(new_name)

    return(tab_round)
  }

  # Helper function to calculate E-values and format the final output
  process_evalue <- function(tab_tmle, scale, delta, sd) {
    if (scale == "RD") {
      evalout <- as.data.frame(round(
        EValue::evalues.OLS(
          est = tab_tmle$`E[Y(1)]-E[Y(0)]`,
          se = tab_tmle$standard_error,
          sd = sd,
          delta = delta,
          true = 0
        ),
        3
      ))
    } else {
      evalout <- as.data.frame(round(EValue::evalues.RR(
        est = tab_tmle$`E[Y(1)]/E[Y(0)]`,
        lo = tab_tmle$`2.5 %`,
        hi = tab_tmle$`97.5 %`,
        true = 1
      ),
      3))
    }

    evalout2 <- evalout[2, , drop = FALSE] # Keep it as a dataframe
    evalout3 <- evalout2 %>%
      dplyr::select_if(~ !any(is.na(.))) %>%
      `colnames<-`(c("E_Value", "E_Val_bound"))

    tab <- tab_tmle %>%
      cbind(evalout3) %>%
      dplyr::select(-standard_error) # Correctly reference the column to exclude

    return(tab)
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
    if (!is.null(subset)) {
      ate_summary <- average_treatment_effect(model_output, subset = subset)
    } else {
      ate_summary <- average_treatment_effect(model_output)
    }
    theta <- ate_summary[["estimate"]]
    std.error <- ate_summary[["std.err"]]
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
  } else if ("multi_arm_causal_forest" %in% class(model_output)) {
    # Processing multi-arm causal forest model output
    if (!is.null(subset)) {
      ate_summary <- average_treatment_effect(model_output, subset = subset)
    } else {
      ate_summary <- average_treatment_effect(model_output)
    }

    # Create a list to store results for each contrast
    results_list <- list()

    for (i in 1:nrow(ate_summary)) {
      theta <- ate_summary$estimate[i]
      std.error <- ate_summary$std.err[i]
      conf.low <- theta - qnorm(0.975) * std.error
      conf.high <- theta + qnorm(0.975) * std.error
      contrast_name <- paste0(new_name, " - ", ate_summary$contrast[i])

      tab_tmle <- create_summary_df(
        theta,
        std.error,
        conf.low,
        conf.high,
        "RD",
        contrast_name
      )

      results_list[[i]] <- process_evalue(tab_tmle, scale, delta, sd)
    }

    # Combine all results into a single data frame
    return(do.call(rbind, results_list))
  } else if (all(c("estimate", "std.err") %in% names(model_output))) {
    # Processing direct estimate and standard error input
    theta <- model_output$estimate
    std.error <- model_output$std.err
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

  # Process E-values and return result
  process_evalue(tab_tmle, scale, delta, sd)
}
