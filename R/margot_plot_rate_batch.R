#' Batch Process and Plot RATE Curves for Multiple Models
#'
#' This function processes a subset of models (or all models by default), creates RATE (Rank Average Treatment Effect)
#' plots for each model using the margot package. Can plot either AUTOC or QINI RATE results.
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_warning cli_alert_success cli_alert_danger
#'
#' @param models_binary A list of model results, where each element contains a 'rate_result' and/or 'rate_qini' component.
#' @param model_names Optional character vector of model names to process. Default NULL (all models).
#' @param target Character; either "AUTOC" (default) or "QINI". Determines which RATE result to plot.
#' @param remove_tx_prefix Logical value indicating whether to remove the "tx_" prefix from labels. Default is TRUE.
#' @param remove_z_suffix Logical value indicating whether to remove the "_z" suffix from labels. Default is TRUE.
#' @param use_title_case Logical value indicating whether to convert labels to title case. Default is TRUE.
#' @param remove_underscores Logical value indicating whether to remove underscores from labels. Default is TRUE.
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param compute_on_demand Logical; if TRUE and RATE results are not pre-computed, will compute them
#'        on-demand from the causal forest (requires full_models to be saved). Default is FALSE.
#' @param q Numeric vector of quantiles at which to evaluate when computing on-demand.
#'        Default is seq(0.1, 1, by = 0.1).
#' @param policy Character; either "treat_best" or "withhold_best" when computing on-demand.
#'        Default is "treat_best".
#' @param use_oob_predictions Logical; if TRUE, use out-of-bag predictions when computing on-demand.
#'        Default is TRUE.
#' @param seed Optional random seed for reproducibility when computing on-demand.
#'
#' @return A list containing the generated ggplot objects for each processed model.
#' @export
margot_plot_rate_batch <- function(models_binary,
                                   model_names = NULL,
                                   target = c("AUTOC", "QINI"),
                                   remove_tx_prefix = TRUE,
                                   remove_z_suffix = TRUE,
                                   use_title_case = TRUE,
                                   remove_underscores = TRUE,
                                   label_mapping = NULL,
                                   compute_on_demand = TRUE,
                                   q = seq(0.1, 1, by = 0.1),
                                   policy = "treat_best",
                                   use_oob_predictions = TRUE,
                                   seed = 12345) {
  # ensure margot package is installed
  if (!requireNamespace("margot", quietly = TRUE)) {
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  # check if input is CV results
  if (inherits(models_binary, "margot_cv_results")) {
    stop(
      "Cannot create RATE plots from cross-validation results.\n",
      "CV results contain hypothesis tests, not RATE curves.\n",
      "Please use margot_plot_cv_results() for a forest plot visualization.\n",
      "To plot RATE curves, use the original causal forest results instead."
    )
  }

  target <- match.arg(target)
  cli::cli_h1("Margot Batch RATE Plots ({target})")

  # determine which models to process
  all_models <- names(models_binary$results)
  if (!is.null(model_names)) {
    # use helper to map model names (handles flipped outcomes)
    name_mapping <- .map_model_names_with_flips(
      requested_names = model_names,
      available_names = all_models,
      verbose = TRUE
    )
    
    selected_models <- name_mapping$mapped_names
    
    if (length(name_mapping$missing_names) > 0) {
      cli::cli_alert_warning("Models not found: {paste(name_mapping$missing_original, collapse = ', ')}")
    }
  } else {
    selected_models <- all_models
  }

  cli::cli_alert_info("Processing {length(selected_models)} models")

  # initialise list to store plots
  rate_plots <- list()

  # loop through each selected model
  for (model_name in selected_models) {
    cli::cli_h2("Processing model: {model_name}")

    tryCatch(
      {
        # extract rate result for this model based on target
        if (target == "AUTOC") {
          rate_result <- models_binary$results[[model_name]]$rate_result
        } else {
          rate_result <- models_binary$results[[model_name]]$rate_qini
        }

        if (is.null(rate_result) && compute_on_demand) {
          # try to compute on-demand if forest is available
          if (!is.null(models_binary$full_models) && !is.null(models_binary$full_models[[model_name]])) {
            cli::cli_alert_info("Computing {target} on-demand for {model_name}")
            forest <- models_binary$full_models[[model_name]]
            tau_hat <- models_binary$results[[model_name]]$tau_hat

            # check for evaluation subset from qini_metadata
            subset <- NULL
            if (!is.null(models_binary$results[[model_name]]$qini_metadata)) {
              qini_meta <- models_binary$results[[model_name]]$qini_metadata
              if (!is.null(qini_meta$test_indices)) {
                subset <- qini_meta$test_indices
              }
            }

            rate_result <- compute_rate_on_demand(
              forest = forest,
              tau_hat = tau_hat,
              target = target,
              q = q,
              policy = policy,
              subset = subset,
              use_oob_predictions = use_oob_predictions,
              verbose = TRUE, # changed to TRUE to show validation warnings
              seed = seed
            )
          } else {
            cli::cli_alert_warning("No {target} result found and cannot compute on-demand for model: {model_name}")
            next
          }
        } else if (is.null(rate_result)) {
          cli::cli_alert_warning("No {target} result found for model: {model_name}")
          next
        }

        # create RATE plot with transformed labels
        plot <- margot_plot_rate(
          x = rate_result,
          outcome_var = model_name,
          remove_tx_prefix = remove_tx_prefix,
          remove_z_suffix = remove_z_suffix,
          use_title_case = use_title_case,
          remove_underscores = remove_underscores,
          label_mapping = label_mapping
        )

        # store plot in list
        rate_plots[[model_name]] <- plot
      },
      error = function(e) {
        cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
      }
    )
  }

  cli::cli_alert_success("Completed processing {length(rate_plots)} models")

  # return list of plots
  return(rate_plots)
}
