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
#'   This parameter is ignored for causal forest models, which always use the additive "RD" contract
#'   returned by [grf::average_treatment_effect()]. For direct `estimate` and `std.err` input on the
#'   `"RR"` scale, the estimate and its normal-approximation confidence limits must all be positive.
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
#'   For multi-arm causal forests, multiple rows will be returned, one for each contrast. Numeric columns
#'   retain their computational precision; round only when formatting the table for presentation.
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

  # forest average treatment effects are additive regardless of caller input
  effective_scale <- if (any(c("causal_forest", "multi_arm_causal_forest") %in% class(model_output))) {
    "RD"
  } else {
    scale
  }

  # validate ratio inputs before calling EValue's RR calculations
  validate_ratio_summary <- function(estimate, std.error, conf.low, conf.high) {
    ratio_values <- c(estimate, conf.low, conf.high)
    if (!is.numeric(estimate) || !is.numeric(std.error) ||
        !all(is.finite(ratio_values)) || !all(is.finite(std.error))) {
      stop("RR estimates, standard errors, and confidence limits must be finite numeric values.", call. = FALSE)
    }
    if (any(std.error < 0)) {
      stop("RR standard errors must be non-negative.", call. = FALSE)
    }
    if (any(ratio_values <= 0)) {
      stop("RR estimates and confidence limits must be strictly positive.", call. = FALSE)
    }
  }

  # Function to create the summary data frame
  create_summary_df <- function(estimate, std.error, conf.low, conf.high, scale, new_name) {
    if (scale == "RR") {
      validate_ratio_summary(estimate, std.error, conf.low, conf.high)
    }

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

    rownames(tab)[1] <- paste0(new_name)

    return(tab)
  }

  # Calculate E-values before any display formatting.
  process_evalue <- function(tab_tmle, scale, delta, sd) {
    tab <- tab_tmle %>%
      cbind(.margot_compute_evalues(tab_tmle, scale, delta, sd)) %>%
      dplyr::select(-standard_error)

    return(tab)
  }

  if ("lmtp_contrast" %in% class(model_output)) {
    # Processing LMTP model output
    tab_tmle <- create_summary_df(
      model_output$vals$theta,
      model_output$vals$std.error,
      model_output$vals$conf.low,
      model_output$vals$conf.high,
      effective_scale,
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

      results_list[[i]] <- process_evalue(tab_tmle, effective_scale, delta, sd)
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
      effective_scale,
      new_name
    )
  } else {
    stop("Unsupported model output type")
  }

  # Process E-values and return result
  process_evalue(tab_tmle, effective_scale, delta, sd)
}
