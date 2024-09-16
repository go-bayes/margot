#' Create Combined Tables for Subset Condition Using GRF Models (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `margot_subset_model()` instead.
#'
#' @param ... Arguments passed to the deprecated function
#'
#'
#' @examples
#' \dontrun{
#' # Instead of using this deprecated function:
#' # result_subset <- margot_grf_subset_table(...)
#'
#' # Use the new function:
#' # result_subset <- margot_subset_model(...)
#' }
#' @import lifecycle
#'
#' @keywords internal
margot_grf_subset_table <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.28",
    what = "margot_grf_subset_table()",
    with = "margot_subset_model()"
  )
  cli::cli_alert_warning("This function is deprecated. Please use margot_subset_model() instead.")
  cli::cli_alert_info("If you haven't already, update to the latest version of the package.")
  cli::cli_alert_info("After updating, you can use the new function: margot_subset_model()")
}
# margot_grf_subset_table <- function(model_results, outcome_vars = NULL, subset_condition = NULL, scale = "RD", debug = FALSE) {
#   if (debug) cat("Debug: class of subset_condition:", class(subset_condition), "\n")
#   if (debug) cat("Debug: length of subset_condition:", length(subset_condition), "\n")
#
#   if (is.null(model_results$full_models)) {
#     message("Error: No full models found in the input. Please ensure you set 'save_models = TRUE' when running the original GRF model function.")
#     return(NULL)
#   }
#
#   if (is.null(outcome_vars)) {
#     outcome_vars <- gsub("^model_", "", names(model_results$full_models))
#     if (debug) cat("Debug: outcome_vars set to:", paste(outcome_vars, collapse=", "), "\n")
#   }
#
#   tables <- list()
#   for (outcome in outcome_vars) {
#     if (debug) cat("Debug: Processing outcome:", outcome, "\n")
#     model_name <- paste0("model_", outcome)
#     if (model_name %in% names(model_results$full_models)) {
#       model <- model_results$full_models[[model_name]]
#       if (debug) {
#         cat("Debug: Model class:", class(model), "\n")
#         cat("Debug: Model names:", paste(names(model), collapse=", "), "\n")
#       }
#       tryCatch({
#         custom_table <- margot::margot_model_evalue(model, scale = scale, new_name = outcome, subset = subset_condition)
#         tables[[outcome]] <- custom_table
#         if (debug) cat("Debug: Successfully created table for", outcome, "\n")
#       }, error = function(e) {
#         if (debug) cat("Debug: Error in margot_model_evalue for", outcome, ":", e$message, "\n")
#       })
#     } else {
#       if (debug) cat("Debug: Model for outcome", outcome, "not found in the input models.\n")
#     }
#   }
#
#   if (length(tables) == 0) {
#     if (debug) cat("Debug: No tables were created.\n")
#     return(NULL)
#   }
#
#   combined_table <- do.call(rbind, tables)
#   return(combined_table)
# }
