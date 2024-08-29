#' Subset Model Results for Binary and Categorical Exposures
#'
#' This function extracts and combines evaluation tables for specified outcome variables
#' from model results, handling both binary and categorical exposure models.
#'
#' @param model_results A list of model results from `model_causal_forest` or similar functions.
#' @param outcome_vars Optional. A character vector of outcome variable names. If NULL,
#'        the function will use all models in the input.
#' @param subset_condition A logical vector indicating the subset of data to use. Default is NULL.
#' @param scale A character string indicating the scale to use. Default is "RD".
#' @param contrast For categorical exposures, a single character string specifying the contrast to extract.
#' @param debug Logical. If TRUE, print debug information. Default is FALSE.
#'
#' @return A data frame combining all custom evaluation tables for the specified outcomes and contrast.
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#'
#' @examples
#' \dontrun{
#' # Example for binary exposure
#' model_cat_subset_pols_binary <- margot_subset_model(
#'   model_results = models_binary,
#'   outcome_vars = t2_outcome_vars_z,
#'   subset_condition = subset_condition_pols,
#'   scale = "RD",
#'   debug = FALSE
#' )
#'
#' # Example for categorical exposure
#' model_cat_subset_pols_categorical <- margot_subset_model(
#'   model_results = models_cat,
#'   outcome_vars = t2_outcome_vars_z,
#'   subset_condition = subset_condition_pols,
#'   scale = "RD",
#'   contrast = "[6.0,7.0] - [1.0,5.0)",
#'   debug = TRUE
#' )
#' }
#'
#' @export
margot_subset_model <- function(model_results, outcome_vars = NULL, subset_condition = NULL,
                                scale = "RD", contrast = NULL, debug = FALSE) {
  cli::cli_alert_info("Starting margot_subset_model function")


  # Helper function to escape regex special characters
  escape_regex <- function(string) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
  }


  if (is.null(model_results$full_models) && is.null(model_results$results)) {
    cli::cli_alert_danger("No full models or results found in the input. Please ensure you're using the correct model output.")
    return(NULL)
  }

  # Determine if we're dealing with binary or categorical exposure
  is_categorical <- !is.null(contrast)

  # Set outcome variables if not provided
  if (is.null(outcome_vars)) {
    if (!is.null(model_results$full_models)) {
      outcome_vars <- gsub("^model_", "", names(model_results$full_models))
    } else {
      outcome_vars <- gsub("^model_", "", names(model_results$results))
    }
    if (debug) cli::cli_alert_info("Outcome variables set to: {paste(outcome_vars, collapse=', ')}")
  }

  tables <- list()
  for (outcome in outcome_vars) {
    cli::cli_alert_info("Processing outcome: {outcome}")
    model_name <- paste0("model_", outcome)

    if (!is.null(model_results$full_models) && model_name %in% names(model_results$full_models)) {
      # Binary exposure case
      model <- model_results$full_models[[model_name]]
      if (debug) {
        cli::cli_alert_info("Model class: {class(model)}")
        cli::cli_alert_info("Model names: {paste(names(model), collapse=', ')}")
      }
      tryCatch({
        custom_table <- margot::margot_model_evalue(model, scale = scale, new_name = outcome, subset = subset_condition)
        tables[[outcome]] <- custom_table
        cli::cli_alert_success("Successfully created table for {outcome}")
      }, error = function(e) {
        cli::cli_alert_warning("Error in margot_model_evalue for {outcome}: {e$message}")
      })
    } else if (!is.null(model_results$results) && model_name %in% names(model_results$results)) {
      # Categorical exposure case
      result <- model_results$results[[model_name]]
      if (!is.null(result$custom_table)) {
        tables[[outcome]] <- result$custom_table
        cli::cli_alert_success("Successfully extracted table for {outcome}")
      } else {
        cli::cli_alert_warning("No custom table found for {outcome}")
      }
    } else {
      cli::cli_alert_warning("Results for outcome {outcome} not found in the input models.")
    }
  }

  if (length(tables) == 0) {
    cli::cli_alert_danger("No tables were created.")
    return(NULL)
  }

  combined_table <- do.call(rbind, tables)

  # Clean up row names
  rownames(combined_table) <- gsub("^[^.]+\\.", "", rownames(combined_table))

  # Filter the combined table for the specified contrast if it's a categorical model
  if (is_categorical) {
    escaped_contrast <- escape_regex(contrast)
    filtered_table <- combined_table[grepl(escaped_contrast, rownames(combined_table), fixed = FALSE), , drop = FALSE]
    if (nrow(filtered_table) > 0) {
      combined_table <- filtered_table
      cli::cli_alert_success("Successfully filtered table for contrast: {contrast}")
    } else {
      cli::cli_alert_warning("Specified contrast not found in any outcomes. Returning unfiltered table.")
    }
  }

  cli::cli_alert_success("Processing complete. Returning results.\U0001F44D")
  return(combined_table)
}
