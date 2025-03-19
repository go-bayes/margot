#' Compare Treatment Effects Between Groups
#'
#' This function compares treatment effects between two groups, calculating either
#' the risk difference (RD) or relative risk ratio (RR). It supports label mapping
#' and transformation for better interpretability of results.
#'
#' @param group A data frame containing the first group's treatment effects and confidence intervals.
#' @param subgroup A data frame containing the second group's treatment effects and confidence intervals.
#' @param type Character string specifying the type of comparison. Either "RD" for risk difference (default) or "RR" for relative risk ratio.
#' @param label_mapping Optional list for mapping original variable names to more readable labels.
#' @param decimal_places Number of decimal places for rounding outputs.
#'
#' @return A list containing two elements:
#'   \item{results}{A data frame with the computed statistics for each comparison.}
#'   \item{interpretations}{A named vector of interpretation strings for each comparison.}
#'
#' @import glue
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Assuming you have group_full and group_rels data frames
#'
#' # Define label mapping
#' label_mapping <- list(
#'   "t2_env_not_climate_chg_concern_z" = "Deny Climate Change Concern",
#'   "t2_env_not_climate_chg_cause_z" = "Deny Humans Cause Climate Change",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real",
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Env Efficacy",
#'   "t2_env_not_sat_nz_environment_z" = "Not Sat with NZ Environment"
#' )
#'
#' # Compare groups using Risk Difference (default)
#' rd_results <- margot_compare_groups(group_full, group_rels,
#'                                     type = "RD",
#'                                     label_mapping = label_mapping)
#' print(rd_results$results)
#' print(rd_results$interpretations)
#'
#' # Compare groups using Relative Risk
#' rr_results <- margot_compare_groups(group_full, group_rels,
#'                                     type = "RR",
#'                                     label_mapping = label_mapping)
#' print(rr_results$results)
#' print(rr_results$interpretations)
#' }
#'
#' @export
margot_compare_groups <- function(group, subgroup, type = "RD", label_mapping = NULL, decimal_places = 4) {
  # Ensure the inputs are data frames with the same number of rows
  if (!is.data.frame(group) || !is.data.frame(subgroup) || nrow(group) != nrow(subgroup)) {
    stop("Inputs must be data frames with the same number of rows")
  }

  # Define options to mimic the previous transformation behaviour
  transform_options <- list(
    remove_tx_prefix = TRUE,
    remove_z_suffix = FALSE,
    remove_underscores = TRUE,
    use_title_case = TRUE
  )

  # Transform row names using the helper function 'transform_label'
  row_names <- rownames(group)
  transformed_names <- sapply(row_names, transform_label, label_mapping = label_mapping, options = transform_options)

  # Helper function to format estimates with confidence intervals
  format_estimate_ci <- function(estimate, ci_lower, ci_upper, dp) {
    formatted <- sprintf(paste0("%.", dp, "f [%.", dp, "f, %.", dp, "f]"),
                         estimate, ci_lower, ci_upper)
    # Bold formatting if the confidence interval excludes zero or one
    if (ci_lower > 0 || ci_upper < 0) {
      formatted <- paste0("**", formatted, "**")
    }
    return(formatted)
  }

  # Initialise results list
  results_list <- vector("list", nrow(group))

  for (i in 1:nrow(group)) {
    # Extract means and standard errors for each row
    mean_A <- group$`E[Y(1)]-E[Y(0)]`[i]
    mean_B <- subgroup$`E[Y(1)]-E[Y(0)]`[i]
    se_A <- (group$`97.5 %`[i] - group$`2.5 %`[i]) / (2 * 1.96)
    se_B <- (subgroup$`97.5 %`[i] - subgroup$`2.5 %`[i]) / (2 * 1.96)

    if (type == "RD") {
      # Compute difference in means and standard error of the difference
      mean_difference <- mean_B - mean_A  # Subgroup minus Group
      se_diff <- sqrt(se_A^2 + se_B^2)

      # Compute 95% confidence intervals
      conf_low <- mean_difference - (1.96 * se_diff)
      conf_high <- mean_difference + (1.96 * se_diff)

      # Format the result
      formatted_result <- format_estimate_ci(mean_difference, conf_low, conf_high, decimal_places)

      # Determine reliability
      reliable <- (conf_low > 0) || (conf_high < 0)

      # Generate interpretation using glue
      interpretation <- glue::glue(
        "For {transformed_names[i]}, the difference in average treatment effects (subgroup minus group) is {formatted_result}. ",
        "{if(reliable) 'We find evidence for a reliable treatment effect difference by group.' else 'We do not find evidence for a reliable treatment-effect difference.'}"
      )

    } else if (type == "RR") {
      # Compute the ratio of relative risks
      rrr <- mean_B / mean_A  # Subgroup divided by Group

      # Compute the standard error of the log(RRR)
      se_log_rrr <- sqrt((se_B / mean_B)^2 + (se_A / mean_A)^2)

      # Compute 95% confidence intervals on the log scale and exponentiate
      conf_low <- exp(log(rrr) - 1.96 * se_log_rrr)
      conf_high <- exp(log(rrr) + 1.96 * se_log_rrr)

      # Format the result
      formatted_result <- format_estimate_ci(rrr, conf_low, conf_high, decimal_places)

      # Determine reliability
      reliable <- (conf_low > 1) || (conf_high < 1)

      # Generate interpretation using glue
      interpretation <- glue::glue(
        "For {transformed_names[i]}, the relative risk ratio (subgroup divided by group) is {formatted_result}. ",
        "{if(reliable) 'We find evidence for a reliable treatment effect difference by group.' else 'We do not find evidence for a reliable treatment-effect difference.'}"
      )

    } else {
      stop("Invalid type specified. Use 'RD' for risk difference or 'RR' for relative risk.")
    }

    # Store result and interpretation for this row
    results_list[[i]] <- list(result = formatted_result, interpretation = interpretation)
  }

  # Combine all results into a single data frame
  all_results <- data.frame(
    result = sapply(results_list, function(x) x$result),
    row.names = transformed_names
  )

  # Combine all interpretations into a named vector
  all_interpretations <- sapply(results_list, function(x) x$interpretation)
  names(all_interpretations) <- transformed_names

  # Return both results and interpretations
  return(list(results = all_results, interpretations = all_interpretations))
}
