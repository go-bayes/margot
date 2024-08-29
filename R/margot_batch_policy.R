#' Apply Policy Tree Analysis to Multiple Models
#'
#' This function applies the `margot_policy_tree()` function to multiple models
#' contained within a single results object. It generates policy tree plots,
#' interpretations, Qini plots, decision tree visualizations, and policy combo
#' plots for each model. It also provides options for saving the generated plots.
#'
#' @param result_outcomes A list containing the results of multiple models.
#'   This object should have a structure similar to the output of a multi-arm
#'   causal forest model, with a `results` element containing named model results.
#' @param policy_tree_args A list of arguments to be passed to `margot_plot_policy_tree()`.
#' @param decision_tree_args A list of arguments to be passed to `margot_plot_decision_tree()`.
#' @param dpi The resolution in dots per inch for saved plots. Default is 300.
#' @param width The width of the saved plots in inches. Default is 10.
#' @param height The height of the saved plots in inches. Default is 8.
#' @param save_plots Logical; if TRUE, plots will be saved to disk. Default is TRUE.
#' @param output_dir The directory where plots should be saved. Default is "plots".
#'
#' @return A list where each element corresponds to a model in the input
#'   `result_outcomes`. Each element is itself a list containing:
#'   \item{policy_tree_plot}{A ggplot object of the policy tree plot}
#'   \item{policy_tree_interpretation}{A character string interpreting the policy tree}
#'   \item{qini_plot}{A ggplot object of the Qini plot}
#'   \item{decision_tree_visualisation}{A ggplot object visualizing the decision tree}
#'   \item{policy_combo_plot}{A ggplot object of the policy combo plot}
#'
#' @examples
#' \dontrun{
#' # Assuming result_outcomes_health contains multiple model results
#' batch_results <- margot_batch_policy(
#'   result_outcomes_health,
#'   policy_tree_args = list(point_alpha = 0.7),
#'   decision_tree_args = list(text_size = 4),
#'   dpi = 300,
#'   width = 12,
#'   height = 10,
#'   save_plots = TRUE,
#'   output_dir = "output_plots"
#' )
#'
#' # To access results for a specific model:
#' smoker_results <- batch_results$model_t2_smoker_binary
#'
#' # The plots are saved in the "output_plots" directory
#' # You can also access the plot objects directly:
#' print(smoker_results$policy_tree_plot)
#' }
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom crayon bold green red
#' @importFrom ggplot2 ggsave
#'
#' @export
margot_batch_policy <- function(result_outcomes,
                                policy_tree_args = list(),
                                decision_tree_args = list(),
                                dpi = 300,
                                width = 12,
                                height = 12,
                                save_plots = TRUE,
                                output_dir = push_mods) {
  cli::cli_alert_info(crayon::bold("Starting margot_batch_policy function"))

  if (!requireNamespace("margot", quietly = TRUE)) {
    cli::cli_alert_danger(crayon::red("Package 'margot' is required but not installed. Please install it first."))
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success(crayon::green(paste("Created output directory:", output_dir)))
  }

  model_names <- names(result_outcomes$results)
  cli::cli_alert_info(paste("Number of models to process:", length(model_names)))

  output_list <- list()
  cli::cli_alert_info(crayon::bold("Processing models"))
  pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    tryCatch({
      model_output <- margot::margot_policy_tree(
        mc_test = result_outcomes,
        model_name = model_name,
        policy_tree_args = policy_tree_args,
        decision_tree_args = decision_tree_args
      )

      if (save_plots) {
        plot_names <- c("policy_tree_plot", "qini_plot", "decision_tree_visualisation", "policy_combo_plot")
        for (plot_name in plot_names) {
          if (!is.null(model_output[[plot_name]])) {
            file_name <- file.path(output_dir, paste0(model_name, "_", plot_name, ".png"))
            ggsave(file_name, model_output[[plot_name]], dpi = dpi, width = width, height = height)
            cli::cli_alert_success(crayon::green(paste("Saved", file_name, "with resolution", dpi, "DPI")))
          }
        }
      }

      output_list[[model_name]] <- model_output
      cli::cli_alert_success(crayon::green(paste("Successfully processed model:", model_name)))
    }, error = function(e) {
      cli::cli_alert_danger(crayon::red(paste("Error processing model", model_name, ":", e$message)))
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success(crayon::bold(crayon::green("margot_batch_policy function completed successfully \U0001F44D")))
  return(output_list)
}
# margot_batch_policy <- function(result_outcomes,
#                                 policy_tree_args = list(),
#                                 decision_tree_args = list(),
#                                 ...) {
#   cli::cli_alert_info(crayon::bold("Starting margot_batch_policy function"))
#
#   # Ensure the margot package is loaded
#   if (!requireNamespace("margot", quietly = TRUE)) {
#     cli::cli_alert_danger(crayon::red("Package 'margot' is required but not installed. Please install it first."))
#     stop("Package 'margot' is required but not installed. Please install it first.")
#   }
#
#   # Extract the names of the models from the results list
#   model_names <- names(result_outcomes$results)
#   cli::cli_alert_info(paste("Number of models to process:", length(model_names)))
#
#   # Initialize an empty list to store the results
#   output_list <- list()
#
#   # Set up progress bar
#   cli::cli_alert_info(crayon::bold("Processing models"))
#   pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#   # Loop through each model
#   for (model_name in model_names) {
#     cli::cli_alert_info(paste("Processing model:", model_name))
#
#     tryCatch({
#       # Apply margot_policy_tree() to each model
#       output_list[[model_name]] <- margot_policy_tree(
#         mc_test = result_outcomes,
#         model_name = model_name,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args,
#         ...
#       )
#       cli::cli_alert_success(crayon::green(paste("Successfully processed model:", model_name)))
#     }, error = function(e) {
#       cli::cli_alert_danger(crayon::red(paste("Error processing model", model_name, ":", e$message)))
#     })
#
#     cli::cli_progress_update()
#   }
#
#   cli::cli_progress_done()
#   cli::cli_alert_success(crayon::bold(crayon::green("margot_batch_policy function completed successfully \U0001F44D")))
#
#   return(output_list)
# }
