#' Compute Difference in Average Treatment Effects or Relative Risk Ratio Between Two Subgroups
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated and will be removed in a future release.
#' Please use the new `margot_compare_groups()` function and associated workflow instead.
#'
#' This function calculates either the difference in average treatment effects (ATE) or the relative risk ratio (RRR)
#' between two independent subgroups. Each subgroup is represented as a list that includes the estimated effect (theta) and
#' the standard error (std.error) of the effect. The result includes both a data frame and an interpretation
#' string formatted for easy use with `glue::glue` in Quarto documents etc. The subgroups are expected to be
#' outputs from the `lmtp::lmtp_contrast()` function.
#'
#' @param group1 A list containing the estimated effect and standard error of subgroup 1.
#'               Expected structure: list(vals = data.frame(theta = x, std.error = y)).
#' @param group2 A list containing the estimated effect and standard error of subgroup 2.
#'               Expected structure: list(vals = data.frame(theta = x, std.error = y)).
#' @param type A character string specifying the type of calculation. "RD" for risk difference (default),
#'             "RR" for relative risk ratio.
#'
#' @return A list containing:
#'         - `results`: A data frame with columns `mean_difference`, `std_error`, `conf_low`, and `conf_high` for type "RD",
#'           or `rrr`, `std_error_log`, `conf_low`, and `conf_high` for type "RR", each rounded to 4 decimal places.
#'           Suitable for direct use in reporting.
#'         - `interpretation`: A string providing a formatted interpretation of the results.
#'
#' @keywords internal
compute_difference <- function(group1, group2, type = "RD") {
  lifecycle::deprecate_soft("1.0.0", "compute_difference()",
    details = "Please use the new `margot_compare_groups()` function and associated workflow instead."
  )

  # The rest of the function remains the same
  # extract means and standard errors from each group
  mean_A <- group1$vals$theta
  mean_B <- group2$vals$theta
  se_A <- group1$vals$std.error
  se_B <- group2$vals$std.error

  if (type == "RD") {
    # compute difference in means and standard error of the difference
    mean_difference <- mean_A - mean_B
    se_diff <- sqrt(se_A^2 + se_B^2)

    # compute 95% confidence intervals (using 1.96 for Z-value)
    conf_low <- mean_difference - (1.96 * se_diff)
    conf_high <- mean_difference + (1.96 * se_diff)

    # create output data frame and round the results
    results <- data.frame(
      mean_difference = round(mean_difference, 4),
      std_error = round(se_diff, 4),
      conf_low = round(conf_low, 4),
      conf_high = round(conf_high, 4)
    )

    # generate interpretation using glue
    interpretation <- glue::glue(
      "the difference in average treatment effects is {results$mean_difference} with a standard error of {results$std_error} and a 95% ci of [{results$conf_low}, {results$conf_high}]."
    )
  } else if (type == "RR") {
    # compute the ratio of relative risks
    rrr <- mean_A / mean_B

    # compute the standard error of the log(RRR)
    se_log_rrr <- sqrt((se_A / mean_A)^2 + (se_B / mean_B)^2)

    # compute 95% confidence intervals on the log scale
    log_rrr <- log(rrr)
    conf_low_log <- log_rrr - 1.96 * se_log_rrr
    conf_high_log <- log_rrr + 1.96 * se_log_rrr

    # exponentiate to get the confidence intervals on the original scale
    conf_low <- exp(conf_low_log)
    conf_high <- exp(conf_high_log)

    # create output data frame and round the results
    results <- data.frame(
      rrr = round(rrr, 4),
      std_error_log = round(se_log_rrr, 4),
      conf_low = round(conf_low, 4),
      conf_high = round(conf_high, 4)
    )

    # generate interpretation using glue
    interpretation <- glue::glue(
      "the difference in the relative risk ratio between the focal group and the reference group is {results$rrr} with a standard error of {results$std_error_log} and a 95% ci of [{results$conf_low}, {results$conf_high}]."
    )
  } else {
    stop("Invalid type specified. Use 'RD' for risk difference or 'RR' for relative risk.")
  }

  # return both results and interpretation
  return(list(results = results, interpretation = interpretation))
}
