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
#' # Example with binary exposure
#' binary_result <- margot_subset_model(
#'   model_results = models_binary,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   scale = "RD",
#'   debug = TRUE
#' )
#'
#' # Example with categorical exposure
#' categorical_result <- margot_subset_model(
#'   model_results = models_cat,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   scale = "RD",
#'   contrast = "[5.0,6.0) - [1.0,5.0)",
#'   debug = TRUE
#' )
#'
#' # Example with subset condition
#' subset_result <- margot_subset_model(
#'   model_results = models_cat,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   subset_condition = age > 30,
#'   scale = "RD",
#'   contrast = "[6.0,7.0] - [1.0,5.0)",
#'   debug = TRUE
#' )
#'
#' # Example using all outcomes
#' all_outcomes_result <- margot_subset_model(
#'   model_results = models_cat,
#'   scale = "RD",
#'   contrast = "[5.0,6.0) - [1.0,5.0)",
#'   debug = FALSE
#' )
#' }
#'
#' @export
margot_subset_model <- function(model_results, outcome_vars = NULL, subset_condition = NULL,
                                scale = "RD", contrast = NULL, debug = FALSE) {
  cli::cli_alert_info("Starting margot_subset_model function")

  if (is.null(model_results$results)) {
    cli::cli_alert_danger("No results found in the input. Please ensure you're using the correct model output.")
    return(NULL)
  }

  if (is.null(outcome_vars)) {
    outcome_vars <- gsub("^model_", "", names(model_results$results))
    if (debug) cli::cli_alert_info("Outcome variables set to: {paste(outcome_vars, collapse=', ')}")
  }

  is_categorical <- !is.null(contrast)

  tables <- list()
  for (outcome in outcome_vars) {
    cli::cli_alert_info("Processing outcome: {outcome}")
    model_name <- paste0("model_", outcome)
    if (model_name %in% names(model_results$results)) {
      result <- model_results$results[[model_name]]

      if (!is.null(result$custom_table)) {
        if (is_categorical) {
          contrast_row <- result$custom_table[grepl(fixed(contrast), rownames(result$custom_table)), , drop = FALSE]
          if (nrow(contrast_row) > 0) {
            tables[[outcome]] <- contrast_row
            cli::cli_alert_success("Successfully extracted contrast for {outcome}")
          } else {
            cli::cli_alert_warning("Specified contrast not found for {outcome}")
          }
        } else {
          tables[[outcome]] <- result$custom_table
          cli::cli_alert_success("Successfully extracted table for {outcome}")
        }
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

  cli::cli_alert_success("Processing complete. Returning results.")
  return(combined_table)
}
