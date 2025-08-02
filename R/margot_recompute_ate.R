#' Recompute Average Treatment Effects with Different Target Samples
#'
#' This function allows recomputation of average treatment effects (ATEs) from fitted
#' causal forest models using different target samples or estimation methods without
#' refitting the models. This is useful for sensitivity analysis and exploring
#' treatment effects in different subpopulations.
#'
#' @param causal_forest_output Output from `margot_causal_forest()` or
#'   `margot_causal_forest_parallel()`. Must have been run with `save_models = TRUE`.
#' @param target_sample Character string specifying the target sample for ATE estimation.
#'   Options are:
#'   \itemize{
#'     \item "all" - All units (default in original models)
#'     \item "treated" - Only treated units (ATT)
#'     \item "control" - Only control units (ATC)
#'     \item "overlap" - Units with propensity scores away from 0 and 1
#'   }
#' @param method Character string specifying the estimation method. Currently only
#'   "AIPW" (Augmented Inverse Probability Weighting) is supported by grf.
#' @param scale Character string specifying the scale of the estimate. Either "RD"
#'   (risk difference) or "RR" (risk ratio). Default is "RD".
#' @param delta The hypothesized increase in outcome for RD scale E-value calculations.
#'   Default is 1.
#' @param sd The standard deviation of the outcome for RD scale E-value calculations.
#'   Default is 1.
#' @param subset Optional logical vector for subsetting the data. Default is NULL.
#' @param respect_train_test_split Logical. If TRUE and the original analysis used
#'   use_train_test_split=TRUE, computes treatment effects on the test set only for
#'   consistency with policy evaluation. Default is TRUE.
#'
#' @return A list with the same structure as the original causal forest output, but with:
#'   \itemize{
#'     \item Updated `ate` values in each model's results
#'     \item Recomputed `custom_table` with new E-values and renamed effect columns (ATE/ATT/ATC/ATO)
#'     \item Updated `combined_table` with all new estimates
#'     \item A new element `ate_params` documenting the parameters used
#'   }
#'
#' @details
#' This function requires that the original causal forest models were saved
#' (using `save_models = TRUE`). It extracts the fitted models and recomputes
#' ATEs using the specified target sample.
#'
#' The different target samples represent different estimands:
#' - "all": Average Treatment Effect (ATE) for the entire population
#' - "treated": Average Treatment Effect on the Treated (ATT)
#' - "control": Average Treatment Effect on the Control (ATC)
#' - "overlap": ATE for units with good overlap in propensity scores
#'
#' @examples
#' \dontrun{
#' # First fit causal forest models with saved models
#' cf_results <- margot_causal_forest(
#'   data = mydata,
#'   outcome_vars = outcomes,
#'   covariates = X,
#'   save_models = TRUE
#' )
#'
#' # Later, get ATEs for different target populations
#' ate_overlap <- margot_recompute_ate(cf_results, target_sample = "overlap")
#' ate_treated <- margot_recompute_ate(cf_results, target_sample = "treated")
#'
#' # Compare estimates
#' cf_results$combined_table # Original (all units)
#' ate_overlap$combined_table # Overlap units only
#' ate_treated$combined_table # Treated units only
#' }
#'
#' @export
#' @importFrom grf average_treatment_effect
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
margot_recompute_ate <- function(
    causal_forest_output,
    target_sample = c("all", "treated", "control", "overlap"),
    method = "AIPW",
    scale = c("RD", "RR"),
    delta = 1,
    sd = 1,
    subset = NULL,
    respect_train_test_split = TRUE) {
  cli::cli_h1("Margot Recompute ATE")

  # validate inputs
  target_sample <- match.arg(target_sample)
  scale <- match.arg(scale)

  if (method != "AIPW") {
    cli::cli_abort("Currently only AIPW method is supported by grf")
  }

  # check if models were saved
  if (is.null(causal_forest_output$full_models) || length(causal_forest_output$full_models) == 0) {
    cli::cli_abort("No saved models found. Original analysis must be run with save_models = TRUE")
  }

  cli::cli_alert_info("Recomputing ATEs with target_sample = '{target_sample}'")

  # create a copy of the output to modify
  new_output <- causal_forest_output

  # extract model info
  results <- new_output$results
  outcome_vars <- new_output$outcome_vars

  # process each model
  new_tables <- list()

  for (i in seq_along(results)) {
    model_name <- names(results)[i]
    outcome_name <- gsub("^model_", "", model_name)

    cli::cli_alert_info("Processing {outcome_name}...")

    # get the saved model - check multiple possible locations
    model <- NULL

    # first check full_models with different naming conventions
    if (!is.null(causal_forest_output$full_models)) {
      if (!is.null(causal_forest_output$full_models[[outcome_name]])) {
        model <- causal_forest_output$full_models[[outcome_name]]
      } else if (!is.null(causal_forest_output$full_models[[model_name]])) {
        model <- causal_forest_output$full_models[[model_name]]
      }
    }

    # if not found, check if model is stored directly in results
    if (is.null(model) && !is.null(results[[model_name]]$model)) {
      model <- results[[model_name]]$model
    }

    if (is.null(model)) {
      cli::cli_alert_warning("Model not found for {outcome_name}, skipping")
      next
    }

    # check for split_info and determine subset to use
    effective_subset <- subset
    if (respect_train_test_split && !is.null(results[[model_name]]$split_info)) {
      split_info <- results[[model_name]]$split_info
      if (split_info$use_train_test_split && !is.null(split_info$test_indices)) {
        cli::cli_alert_info("Using test set indices from original train/test split")
        effective_subset <- split_info$test_indices
      }
    }

    # recompute ATE with new target_sample
    if (!is.null(effective_subset)) {
      ate_new <- grf::average_treatment_effect(
        model,
        target.sample = target_sample,
        method = method,
        subset = effective_subset
      )
    } else {
      ate_new <- grf::average_treatment_effect(
        model,
        target.sample = target_sample,
        method = method
      )
    }

    # update the ate in results
    new_output$results[[model_name]]$ate <- round(ate_new, 3)

    # recompute the custom table with E-values using the new ATE
    # we need to pass the new ATE information to margot_model_evalue
    # create a temporary structure that margot_model_evalue can understand
    temp_model <- model

    # override the average_treatment_effect method temporarily
    old_ate <- grf::average_treatment_effect(model)

    # create modified output that will use our new ATE
    ate_df <- data.frame(
      estimate = ate_new[["estimate"]],
      std.err = ate_new[["std.err"]]
    )

    # use margot_model_evalue with the new ATE values
    new_table <- margot_model_evalue(
      model_output = ate_df,
      scale = scale,
      new_name = outcome_name,
      delta = delta,
      sd = sd
    )

    # rename the effect column based on target_sample
    old_col <- ifelse(scale == "RD", "E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]")
    new_col <- switch(target_sample,
      "all" = "ATE",
      "treated" = "ATT",
      "control" = "ATC",
      "overlap" = "ATO"
    )

    if (old_col %in% names(new_table)) {
      names(new_table)[names(new_table) == old_col] <- new_col
    }

    # update the custom table in results
    new_output$results[[model_name]]$custom_table <- new_table
    new_tables[[outcome_name]] <- new_table

    # preserve split_info if it exists
    if (!is.null(results[[model_name]]$split_info)) {
      new_output$results[[model_name]]$split_info <- results[[model_name]]$split_info
      # update the appropriate results based on whether test indices were used
      if (respect_train_test_split && results[[model_name]]$split_info$use_train_test_split) {
        # main results already use test set, so no need to update them
        # but we should update the all_data results in split_info
        if (is.null(effective_subset)) {
          # if no subset was used, we computed on all data
          new_output$results[[model_name]]$split_info$ate_all_data <- round(ate_new, 3)
          new_output$results[[model_name]]$split_info$custom_table_all_data <- new_table
        }
      }
    }
  }

  # rebuild combined table
  if (length(new_tables) > 0) {
    new_output$combined_table <- do.call(rbind, new_tables)

    # ensure row names match original format
    if (!is.null(rownames(new_output$combined_table))) {
      rownames(new_output$combined_table) <- names(new_tables)
    }

    cli::cli_alert_success("Combined table updated with new ATEs")
  }

  # add metadata about the recomputation
  new_output$ate_params <- list(
    target_sample = target_sample,
    method = method,
    scale = scale,
    delta = delta,
    sd = sd,
    subset = subset,
    recomputed_at = Sys.time()
  )

  cli::cli_alert_success("ATE recomputation complete!")

  return(new_output)
}

#' Batch Recompute ATEs for Multiple Target Samples
#'
#' Convenience function to recompute ATEs for multiple target samples at once,
#' returning a comparison table.
#'
#' @param causal_forest_output Output from `margot_causal_forest()` with saved models
#' @param target_samples Character vector of target samples to compute.
#'   Default is all four: c("all", "treated", "control", "overlap")
#' @param ... Additional arguments passed to `margot_recompute_ate()`
#'
#' @return A list containing:
#'   \itemize{
#'     \item `results` - Named list of recomputed outputs for each target sample
#'     \item `comparison_table` - Data frame comparing ATEs across target samples
#'     \item `parameters` - Parameters used for recomputation
#'   }
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info
margot_recompute_ate_batch <- function(
    causal_forest_output,
    target_samples = c("all", "treated", "control", "overlap"),
    ...) {
  cli::cli_h1("Batch ATE Recomputation")

  # store results for each target sample
  results <- list()

  # process each target sample
  for (ts in target_samples) {
    cli::cli_alert_info("Computing ATEs for target_sample = '{ts}'")
    results[[ts]] <- margot_recompute_ate(
      causal_forest_output,
      target_sample = ts,
      ...
    )
  }

  # create comparison table
  comparison_list <- list()

  # get outcome names from first result
  outcome_names <- rownames(results[[1]]$combined_table)

  for (outcome in outcome_names) {
    outcome_comparison <- data.frame(
      outcome = outcome,
      stringsAsFactors = FALSE
    )

    for (ts in target_samples) {
      # extract the estimate and CI for this outcome and target sample
      tbl <- results[[ts]]$combined_table
      outcome_row <- tbl[outcome, , drop = FALSE]

      # get the appropriate effect column - now could be ATE, ATT, ATC, or ATO
      effect_col <- NULL
      possible_cols <- c("ATE", "ATT", "ATC", "ATO", "E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]")
      for (col in possible_cols) {
        if (col %in% names(outcome_row)) {
          effect_col <- col
          break
        }
      }

      if (is.null(effect_col)) {
        next
      }

      # create label with estimate and CI
      est_label <- sprintf(
        "%.3f (%.3f, %.3f)",
        outcome_row[[effect_col]],
        outcome_row[["2.5 %"]],
        outcome_row[["97.5 %"]]
      )

      # use appropriate column name based on target sample
      col_prefix <- switch(ts,
        "all" = "ATE",
        "treated" = "ATT",
        "control" = "ATC",
        "overlap" = "ATO"
      )

      outcome_comparison[[col_prefix]] <- est_label
      outcome_comparison[[paste0(col_prefix, "_EValue")]] <- outcome_row[["E_Val_bound"]]
    }

    comparison_list[[outcome]] <- outcome_comparison
  }

  comparison_table <- do.call(rbind, comparison_list)
  rownames(comparison_table) <- comparison_table$outcome
  comparison_table$outcome <- NULL

  cli::cli_alert_info("Comparison table created")

  # return results
  output <- list(
    results = results,
    comparison_table = comparison_table,
    parameters = list(
      target_samples = target_samples,
      additional_args = list(...)
    )
  )

  return(output)
}
