#' Perform Cost Sensitivity Analysis for QINI Curves
#'
#' @description
#' Generates QINI curves across multiple treatment cost scenarios to understand
#' how optimal treatment allocation changes with cost. This allows exploration
#' of policy recommendations under different budget constraints without rerunning
#' the causal forest models.
#'
#' @param models List returned by margot_causal_forest(), containing results
#'   and optionally full_models.
#' @param costs Numeric vector of treatment costs to evaluate. Default is
#'   c(0.2, 0.5, 1, 2, 5) representing a range from cheap to expensive treatments.
#' @param model_names Optional character vector specifying which models to process.
#'   Default NULL (all models).
#' @param spend_levels Numeric vector of spend levels for difference gain summaries.
#'   Default is 0.1.
#' @param baseline_method Method for generating baseline. See margot_qini() for details.
#' @param verbose Logical; print progress messages (default TRUE).
#' @param seed Integer; base seed for reproducible computations (default 12345).
#'
#' @return A list with class "margot_qini_cost_sensitivity" containing:
#' \itemize{
#'   \item \code{results}: Named list where each element is a cost scenario containing
#'     QINI results from margot_qini()
#'   \item \code{costs}: The cost values used
#'   \item \code{models_processed}: Names of models that were processed
#'   \item \code{summary}: Data frame summarizing key metrics across costs
#' }
#'
#' @details
#' This function systematically varies the treatment_cost parameter in margot_qini()
#' to show how QINI curves and optimal treatment fractions change with cost.
#'
#' Lower costs (e.g., 0.2) represent scenarios where treatment is cheap relative
#' to budget, resulting in steeper QINI curves and larger optimal treatment fractions.
#' Higher costs (e.g., 5) represent expensive treatments where only the highest-effect
#' individuals justify treatment.
#'
#' The summary table includes the optimal treatment fraction and expected gain
#' at key spend levels for each cost scenario, facilitating comparison across
#' different budget constraints.
#'
#' @examples
#' \dontrun{
#' # Basic cost sensitivity analysis
#' cost_sens <- margot_qini_cost_sensitivity(
#'   causal_forest_results,
#'   costs = c(0.5, 1, 2)
#' )
#'
#' # View summary of optimal treatment fractions
#' print(cost_sens$summary)
#'
#' # Plot results for a specific model across costs
#' margot_plot_qini_cost_sensitivity(cost_sens, model_name = "model_anxiety")
#'
#' # Detailed analysis for specific models
#' cost_sens_detailed <- margot_qini_cost_sensitivity(
#'   causal_forest_results,
#'   costs = seq(0.2, 3, by = 0.2), # Fine-grained cost grid
#'   model_names = c("model_anxiety", "model_depression"),
#'   spend_levels = c(0.05, 0.1, 0.2, 0.4) # More spend levels
#' )
#' }
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_h2
#' @importFrom purrr map
margot_qini_cost_sensitivity <- function(models,
                                         costs = c(0.2, 0.5, 1, 2, 5),
                                         model_names = NULL,
                                         spend_levels = 0.1,
                                         baseline_method = "maq_no_covariates",
                                         verbose = TRUE,
                                         seed = 12345) {
  # validate inputs
  if (!is.numeric(costs) || any(costs <= 0)) {
    stop("costs must be a numeric vector of positive values")
  }

  costs <- sort(unique(costs)) # ensure sorted and unique

  if (verbose) {
    cli::cli_h2("QINI Cost Sensitivity Analysis")
    cli::cli_alert_info("Evaluating {length(costs)} cost scenarios: {paste(costs, collapse = ', ')}")
  }

  # run margot_qini for each cost
  results <- list()

  for (cost in costs) {
    if (verbose) {
      cli::cli_alert_info("Processing cost = {cost}")
    }

    cost_label <- paste0("cost_", cost)

    # create a deep copy of models to avoid modifying the original
    # remove any existing qini objects to force regeneration with new cost
    models_copy <- models
    if (!is.null(models_copy$results)) {
      for (nm in names(models_copy$results)) {
        models_copy$results[[nm]]$qini_objects <- NULL
        models_copy$results[[nm]]$qini_data <- NULL
        models_copy$results[[nm]]$qini_metadata <- NULL
      }
    }

    results[[cost_label]] <- margot_qini(
      models = models_copy,
      model_names = model_names,
      spend_levels = spend_levels,
      baseline_method = baseline_method,
      treatment_cost = cost,
      verbose = FALSE, # suppress individual run messages
      seed = seed
    )
  }

  # extract summary statistics
  summary_data <- list()

  # get model names from first cost scenario
  if (length(results) > 0 && length(results[[1]]) > 0) {
    models_processed <- names(results[[1]])

    for (model_name in models_processed) {
      model_summary <- list()

      for (i in seq_along(costs)) {
        cost <- costs[i]
        cost_label <- names(results)[i]

        # extract key metrics from diff_gain_summaries
        qini_result <- results[[cost_label]][[model_name]]

        if (!is.null(qini_result$diff_gain_summaries)) {
          for (spend_level_name in names(qini_result$diff_gain_summaries)) {
            spend_value <- as.numeric(gsub("spend_", "", spend_level_name))

            summary_entry <- data.frame(
              model = model_name,
              cost = cost,
              spend_level = spend_value,
              stringsAsFactors = FALSE
            )

            # extract gain values if available
            gain_summary <- qini_result$diff_gain_summaries[[spend_level_name]]
            if (!is.null(gain_summary)) {
              # extract numeric values from formatted strings
              if (!is.null(gain_summary$diff_gain)) {
                # parse "0.091 (SE: 0.043)" format
                diff_parts <- regmatches(
                  gain_summary$diff_gain,
                  regexec(
                    "^([+-]?[0-9.]+)\\s*\\(SE:\\s*([0-9.]+)\\)",
                    gain_summary$diff_gain
                  )
                )
                if (length(diff_parts[[1]]) >= 3) {
                  summary_entry$difference_gain <- as.numeric(diff_parts[[1]][2])
                  summary_entry$difference_se <- as.numeric(diff_parts[[1]][3])
                }
              }

              # try to get raw gain values from qini objects if available
              if (!is.null(qini_result$qini_objects)) {
                # compute average gains at this spend level
                tryCatch(
                  {
                    if ("ate" %in% names(qini_result$qini_objects) &&
                      "cate" %in% names(qini_result$qini_objects)) {
                      ate_obj <- qini_result$qini_objects$ate
                      cate_obj <- qini_result$qini_objects$cate

                      # compute average gains
                      if (inherits(ate_obj, "qini_simple_baseline")) {
                        ate_gain <- average_gain.qini_simple_baseline(ate_obj, spend = spend_value)
                      } else {
                        ate_gain <- maq::average_gain(ate_obj, spend = spend_value)
                      }

                      if (inherits(cate_obj, "qini_simple_baseline")) {
                        cate_gain <- average_gain.qini_simple_baseline(cate_obj, spend = spend_value)
                      } else {
                        cate_gain <- maq::average_gain(cate_obj, spend = spend_value)
                      }

                      # extract estimates
                      if (is.list(ate_gain)) {
                        summary_entry$reference_gain <- ate_gain$estimate
                      } else if (is.numeric(ate_gain) && length(ate_gain) >= 1) {
                        summary_entry$reference_gain <- ate_gain[1]
                      }

                      if (is.list(cate_gain)) {
                        summary_entry$comparison_gain <- cate_gain$estimate
                      } else if (is.numeric(cate_gain) && length(cate_gain) >= 1) {
                        summary_entry$comparison_gain <- cate_gain[1]
                      }
                    }
                  },
                  error = function(e) {
                    # silently continue if computation fails
                  }
                )
              }
            }

            model_summary[[length(model_summary) + 1]] <- summary_entry
          }
        }
      }

      if (length(model_summary) > 0) {
        summary_data[[model_name]] <- do.call(rbind, model_summary)
      }
    }
  } else {
    models_processed <- character(0)
  }

  # combine all summaries
  if (length(summary_data) > 0) {
    summary_df <- do.call(rbind, summary_data)
    rownames(summary_df) <- NULL
  } else {
    summary_df <- data.frame()
  }

  if (verbose) {
    cli::cli_alert_success("Cost sensitivity analysis complete")
  }

  # create output object
  output <- list(
    results = results,
    costs = costs,
    models_processed = models_processed,
    summary = summary_df
  )

  class(output) <- c("margot_qini_cost_sensitivity", class(output))

  return(output)
}
