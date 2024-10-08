#' Create a Multi-arm Margot Plot with User-specified Contrast
#'
#' This function is a wrapper for the `margot_plot()` function, designed for multi-arm causal forest models.
#' It allows the user to specify the contrast of interest, and automatically passes it to the `margot_plot()` function.
#' If the specified contrast is not found, an error is reported.
#'
#' @param model_table A list or data frame containing the model output tables.
#' @param contrast A character string specifying the contrast to be used (e.g. `"(5.0,7.0] - [1.0,3.0]"`).
#' @param original_df Optional data frame containing the original (non-transformed) data for back-transformation of results.
#' @param options A list of additional options for customising the plot, passed directly to `margot_plot()`.
#' @param label_mapping A named list for custom label mapping of the outcomes, also passed to `margot_plot()`.
#' @param save_output Logical. If TRUE, saves the complete output to a file. Default is FALSE.
#' @param use_timestamp Logical. If TRUE, adds a timestamp to the saved filename. Default is FALSE.
#' @param base_filename Character string. The base name for the saved file. Default is "margot_plot_output".
#' @param prefix Character string. An optional prefix for the saved filename. Default is NULL.
#' @param save_path Character string. The directory path where the output will be saved. Default is here::here("push_mods").
#'
#' @details
#' The user must specify a contrast from the `model_table`. If the contrast is not found, an error will be raised using `cli` reporting.
#'
#' The `margot_plot()` function provides various options for customising the plot, including the ability to save the plot,
#' modify labels, and adjust plot aesthetics. The full range of options available to `margot_plot()` can be passed through the `options` argument.
#'
#' If `original_df` is provided, the function will use it to back-transform the results to the original scale.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item `plot`: A ggplot object of the Margot plot.
#'   \item `interpretation`: A character string with the interpretation of the results.
#'   \item `transformed_table`: A data frame with transformed labels according to the options and label mappings.
#' }
#'
#' If `save_output` is TRUE, the complete output will be saved to a file using margot::here_save_qs().
#'
#' @examples
#' \dontrun{
#' # Example usage with multi-arm models
#' multi_results <- margot_plot_multi_arm(
#'   models_multi$combined_tables,
#'   contrast = "(5.0,7.0] - [1.0,3.0]",
#'   original_df = df_raw_outcomes,
#'   options = multi_options,
#'   label_mapping = label_mapping,
#'   save_output = TRUE,
#'   save_path = here::here("output", "margot_plots"),
#'   base_filename = "margot_plot_output",
#'   prefix = "test"
#' )
#' print(multi_results$plot)
#' cat(multi_results$interpretation)
#' print(multi_results$transformed_table)
#' }
#'@importFrom cli cli_abort
#' @export
margot_plot_multi_arm <- function(model_table, contrast, original_df = NULL, options, label_mapping,
                                  save_output = FALSE, use_timestamp = FALSE,
                                  base_filename = "margot_plot_output", prefix = NULL,
                                  save_path = here::here("push_mods")) {
  # check if the specified contrast exists in the model_table
  if (!contrast %in% names(model_table)) {
    cli::cli_abort(c(
      "The specified contrast '{contrast}' was not found in the model_table.",
      "x" = "Available contrasts are: {paste(names(model_table), collapse = ', ')}."
    ))
  }
  # retrieve the contrast data
  contrast_data <- model_table[[contrast]]
  # call the margot_plot function
  margot_plot(
    contrast_data,
    original_df = original_df,
    options = options,
    label_mapping = label_mapping,
    save_output = save_output,
    use_timestamp = use_timestamp,
    base_filename = base_filename,
    prefix = prefix,
    save_path = save_path
  )
}

# margot_plot_multi_arm <- function(model_table, contrast, options, label_mapping,
#                                   save_output = FALSE, use_timestamp = FALSE,
#                                   base_filename = "margot_plot_output", prefix = NULL,
#                                   save_path = here::here("push_mods")) {
#   # check if the specified contrast exists in the model_table
#   if (!contrast %in% names(model_table)) {
#     cli::cli_abort(c(
#       "The specified contrast '{contrast}' was not found in the model_table.",
#       "x" = "Available contrasts are: {paste(names(model_table), collapse = ', ')}."
#     ))
#   }
#
#   # retrieve the contrast data
#   contrast_data <- model_table[[contrast]]
#
#   # call the margot_plot function
#   margot_plot(
#     contrast_data,
#     options = options,
#     label_mapping = label_mapping,
#     save_output = save_output,
#     use_timestamp = use_timestamp,
#     base_filename = base_filename,
#     prefix = prefix,
#     save_path = save_path
#   )
# }
#

