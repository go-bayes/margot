#' Create Combined Tables for Subset Condition Using GRF Models
#'
#' This function calculates new evaluation tables for specified outcome variables
#' using a subset condition, and combines them into a single data frame. It uses
#' the models generated by `margot_run_models_grf` to create these tables.
#'
#' @param model_results A list of model results from `margot_run_models_grf`.
#' @param outcome_vars Optional. A character vector of outcome variable names. If NULL,
#'        the function will attempt to use the outcome_vars from the model_results input.
#' @param subset_condition A logical vector indicating the subset of data to use. Default is NULL.
#' @param scale A character string indicating the scale to use in `margot_model_evalue`. Default is "RD".
#'
#' @return A data frame combining all custom evaluation tables for the specified subset condition.
#'
#' @details
#' The function uses `margot_model_evalue` to calculate evaluation metrics for each model
#' in the subset of data specified by `subset_condition`. If outcome_vars is not provided,
#' it will attempt to use the outcome_vars from the model_results input (assuming it's the output
#' from margot_run_models_grf). If neither is available, it will use all models in the input.
#'
#' @note
#' Ensure that the `model_results` list contains results for all specified `outcome_vars`,
#' otherwise some outcomes may be omitted from the final table.
#'
#' @seealso
#' \code{\link{margot_run_models_grf}} for generating the initial models.
#'
#' @importFrom margot margot_model_evalue
#'
#' @examples
#' \dontrun{
#' # Assuming result_psychological_well_being is your results object from margot_run_models_grf
#' subset_condition <- X[, "t0_self_esteem_z"] < 1
#' combined_table_subset <- margot_subset_table_grf(
#'   model_results = result_psychological_well_being,
#'   outcome_vars = NULL,
#'   subset_condition = subset_condition,
#'   scale = "RD"
#' )
#' print(combined_table_subset)
#' }
#'
#' @export
margot_subset_table_grf <- function(model_results, outcome_vars = NULL, subset_condition = NULL, scale = "RD") {
  # If outcome_vars is not provided, try to get it from model_results, or use all model names
  if (is.null(outcome_vars)) {
    if ("outcome_vars" %in% names(model_results)) {
      outcome_vars <- model_results$outcome_vars
    } else {
      outcome_vars <- gsub("^model_", "", names(model_results$results))
    }
  }

  tables <- list()
  for (outcome in outcome_vars) {
    model_name <- paste0("model_", outcome)
    if (model_name %in% names(model_results$results)) {
      model <- model_results$results[[model_name]]$model
      new_name <- outcome
      # Calculate new evaluation table for the subset
      custom_table <- margot::margot_model_evalue(model, scale = scale, new_name = new_name, subset = subset_condition)
      # Store custom table for binding later
      tables[[outcome]] <- custom_table
    } else {
      warning(paste("Model for outcome", outcome, "not found in the input models."))
    }
  }

  # If no tables were created, return NULL with a warning
  if (length(tables) == 0) {
    warning("No matching models found for the specified outcome variables.")
    return(NULL)
  }

  # Bind tables for outcomes within each domain
  combined_table <- do.call(rbind, tables)
  return(combined_table)
}
