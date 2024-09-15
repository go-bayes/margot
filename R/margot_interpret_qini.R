#' Interpret Qini Results
#'
#' This function combines the output of all models processed by margot_batch_policy()
#' to produce a single table and provide explanations for each arm.
#'
#' @param multi_batch The output of margot_batch_policy() function
#' @param label_mapping A named list mapping model names to labels (optional)
#' @param alpha The significance level for confidence intervals (default is 0.05)
#' @param decimal_places Number of decimal places for rounding (default is 2)
#'
#' @return A list containing two elements for each arm:
#'   \item{summary_table}{A data frame with point estimates and confidence intervals for each model and spend level}
#'   \item{explanations}{A character vector with explanations for each model and spend level}
#'
#' @importFrom dplyr bind_rows mutate across everything rename_with
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map_dfr map
#' @importFrom glue glue
#' @importFrom stats qnorm
#' @importFrom tools toTitleCase
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a multi_batch object from margot_batch_policy()
#' label_mapping <- list(
#'   "t2_env_not_climate_chg_concern_z" = "Deny Climate Change Concern",
#'   "t2_env_not_climate_chg_cause_z" = "Deny Humans Cause Climate Change",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real",
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Env Efficacy",
#'   "t2_env_sat_nz_environment_z" = "Not Sat NZ Environment"
#' )
#'
#' # For binary treatment
#' binary_results <- margot_interpret_qini(binary_multi_batch, label_mapping = label_mapping)
#'
#' # View summary table for binary treatment
#' print(binary_results$binary$summary_table)
#'
#' # View explanations for binary treatment
#' cat(binary_results$binary$explanations, sep = "\n\n")
#'
#' # For multi-arm treatment
#' multi_arm_results <- margot_interpret_qini(multi_arm_multi_batch, label_mapping = label_mapping)
#'
#' # View summary table for arm2
#' print(multi_arm_results$arm2$summary_table)
#'
#' # View explanations for arm2
#' cat(multi_arm_results$arm2$explanations, sep = "\n\n")
#' }
margot_interpret_qini <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  # Function to extract estimates and create CI
  extract_estimates <- function(diff_gain_summary) {
    estimate <- as.numeric(gsub(".*?([-]?\\d+\\.\\d+).*", "\\1", diff_gain_summary$diff_gain))
    se <- as.numeric(gsub(".*?(\\d+\\.\\d+)\\).*", "\\1", diff_gain_summary$diff_gain))
    ci_lower <- estimate - qnorm(1 - alpha/2) * se
    ci_upper <- estimate + qnorm(1 - alpha/2) * se
    return(c(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper))
  }

  # Function to transform labels
  transform_label <- function(label) {
    original_label <- label
    if (!is.null(label_mapping)) {
      # Remove 'model_' prefix if present
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) {
        label <- label_mapping[[clean_label]]
      } else if (label %in% names(label_mapping)) {
        label <- label_mapping[[label]]
      } else {
        label <- sub("^model_", "", label)
        label <- sub("^t[0-9]+_", "", label)
        label <- sub("_z$", "", label)
        label <- gsub("_", " ", label)
        label <- tools::toTitleCase(label)
        # Preserve "NZ" capitalization
        label <- gsub("Nz", "NZ", label)
      }
    }
    return(label)
  }
  # Function to create explanation
  create_explanation <- function(diff_gain_summary, model_name, spend, arm = NULL) {
    estimates <- extract_estimates(diff_gain_summary)
    direction <- if(estimates["estimate"] > 0) "better" else if(estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliability <- if(estimates["ci_lower"] * estimates["ci_upper"] > 0) "is reliably" else "is not reliably"

    explanation <- glue::glue(
      "For the outcome {transform_label(model_name)}, at the {spend*100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments ",
      "{if(!is.null(arm)) paste('to', arm, 'treatment') else ''} yields {reliability} {direction} ",
      "than using the average treatment effect (ATE) to assign treatment. The difference when prioritising conditional average treatment effects is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} ",
      "[95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )

    if (reliability == "is reliably" && direction == "better") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "This result suggests it may be beneficial to target CATE at this spend level.")
    }

    return(explanation)
  }

  # Function to format estimates with CI
  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
            estimate, ci_lower, ci_upper)
  }

  # Determine if it's a binary or multi-arm model
  is_binary <- "diff_gain_summary" %in% names(multi_batch[[1]])

  if (is_binary) {
    # Process binary model
    summary_table <- purrr::map_dfr(names(multi_batch), function(model_name) {
      model_results <- multi_batch[[model_name]]$diff_gain_summary
      estimates <- extract_estimates(model_results)
      data.frame(
        Model = transform_label(model_name),
        `Spend 50%` = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
      )
    })

    explanations <- purrr::map_chr(names(multi_batch), function(model_name) {
      model_results <- multi_batch[[model_name]]$diff_gain_summary
      create_explanation(model_results, model_name, 0.5)
    })

    results <- list(binary = list(summary_table = summary_table, explanations = explanations))

  } else {
    # Process multi-arm model
    arms <- c("all_arms", names(multi_batch[[1]]$diff_gain_summaries$spend_0.2)[-1])
    results <- list()

    for (arm in arms) {
      summary_table <- purrr::map_dfr(names(multi_batch), function(model_name) {
        model_results <- multi_batch[[model_name]]$diff_gain_summaries
        purrr::map_dfr(names(model_results), function(spend) {
          estimates <- extract_estimates(model_results[[spend]][[arm]])
          data.frame(
            Model = transform_label(model_name),
            Spend = as.numeric(gsub("spend_", "", spend)) * 100,
            estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
          )
        })
      })

      # Reshape the summary table
      summary_table <- summary_table %>%
        tidyr::pivot_wider(
          names_from = Spend,
          values_from = estimate_ci,
          names_prefix = "Spend "
        ) %>%
        dplyr::rename_with(~paste0(., "%"), -Model)

      # Create explanations
      explanations <- purrr::map(names(multi_batch), function(model_name) {
        model_results <- multi_batch[[model_name]]$diff_gain_summaries
        explanation <- purrr::map_chr(names(model_results), function(spend) {
          create_explanation(
            model_results[[spend]][[arm]],
            model_name,
            as.numeric(gsub("spend_", "", spend)),
            arm
          )
        })
        paste(explanation, collapse = "\n\n")  # Add paragraph stop between spend levels
      })

      results[[arm]] <- list(summary_table = summary_table, explanations = unlist(explanations))
    }
  }

  return(results)
}
