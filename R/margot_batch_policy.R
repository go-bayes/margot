#' @title Batch Processing of Policy Trees and Related Visualizations
#' @return A list where each element corresponds to a model in the input
#'   `result_outcomes`. Each element is itself a list containing:
#'   \item{policy_tree_plot}{A ggplot object of the policy tree plot}
#'   \item{policy_tree_interpretation}{A character string interpreting the policy tree}
#'   \item{qini_plot}{A ggplot object of the Qini plot}
#'   \item{decision_tree_visualisation}{A ggplot object visualizing the decision tree}
#'   \item{policy_combo_plot}{A ggplot object of the policy combo plot}
#'   \item{diff_gain_summaries}{A nested list containing difference gain summaries for each spend level}
#'
#' @examples
#' \dontrun{
#' # Example for binary treatment
#' binary_results <- margot_batch_policy(
#'   result_outcomes = cf_results,
#'   policy_tree_args = list(point_alpha = 0.7),
#'   decision_tree_args = list(text_size = 4),
#'   dpi = 300,
#'   width = 12,
#'   height = 10,
#'   save_plots = TRUE,
#'   output_dir = "output_plots_binary",
#'   spend = c(0.2, 0.5, 0.8)
#' )
#'
#' # Access results for a specific binary model and spend level
#' binary_model_results <- binary_results$model_t2_binary
#' print(binary_model_results$diff_gain_summaries$spend_0.2$summary)
#' print(binary_model_results$diff_gain_summaries$spend_0.5$summary)
#'
#' # Example for multi-arm treatment
#' multi_arm_results <- margot_batch_policy(
#'   result_outcomes = mc_results,
#'   policy_tree_args = list(point_alpha = 0.7),
#'   decision_tree_args = list(text_size = 4),
#'   dpi = 300,
#'   width = 12,
#'   height = 10,
#'   save_plots = TRUE,
#'   output_dir = "output_plots_multi_arm",
#'   spend = c(0.1, 0.3, 0.5)
#' )
#'
#' # Access results for a specific multi-arm model, spend level, and arm
#' multi_arm_model_results <- multi_arm_results$model_t2_multi_arm
#' print(multi_arm_model_results$diff_gain_summaries$spend_0.3$all_arms$summary)
#' print(multi_arm_model_results$diff_gain_summaries$spend_0.5$arm1$summary)
#'
#' # The plots are saved in the specified output directories
#' # You can also access the plot objects directly:
#' print(binary_model_results$policy_tree_plot)
#' print(multi_arm_model_results$qini_plot)
#' }
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @import here
#' @importFrom ggplot2 ggsave
#'
#' @export
margot_batch_policy <- function(result_outcomes,
                                policy_tree_args = list(),
                                decision_tree_args = list(),
                                dpi = 600,
                                width = 12,
                                height = 12,
                                save_plots = TRUE,
                                output_dir = here::here(push_mods),
                                spend = c(0.2, 0.5)) {
  cli::cli_alert_info("Starting margot_batch_policy function")

  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success(paste("Created output directory:", output_dir))
  }

  model_names <- names(result_outcomes$results)
  cli::cli_alert_info(paste("Number of models to process:", length(model_names)))

  output_list <- list()
  cli::cli_alert_info("Processing models")
  pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    tryCatch({
      model_output <- margot_policy_tree(
        mc_test = result_outcomes,
        model_name = model_name,
        policy_tree_args = policy_tree_args,
        decision_tree_args = decision_tree_args
      )

      # Add difference gain summary for each spend level
      qini_objects <- result_outcomes$results[[model_name]]$qini_objects
      is_binary <- "cate" %in% names(qini_objects) && "ate" %in% names(qini_objects)

      if (is_binary) {
        model_output$diff_gain_summaries <- list()
        for (s in spend) {
          diff_gain_summary <- margot_summary_cate_difference_gain(
            result_outcomes,
            outcome_var = model_name,
            reference_curve = "ate",
            comparison_curve = "cate",
            spend = s
          )
          model_output$diff_gain_summaries[[paste0("spend_", s)]] <- diff_gain_summary
        }
      } else if ("baseline" %in% names(qini_objects)) {
        # Multi-arm treatment
        model_output$diff_gain_summaries <- list()

        for (s in spend) {
          spend_summaries <- list()

          # Compare 'all_arms' with 'baseline'
          spend_summaries[["all_arms"]] <- margot_summary_cate_difference_gain(
            result_outcomes,
            outcome_var = model_name,
            reference_curve = "baseline",
            comparison_curve = "all_arms",
            spend = s
          )

          # Compare each individual arm with 'baseline'
          arm_names <- setdiff(names(qini_objects), c("all_arms", "baseline"))
          for (arm in arm_names) {
            spend_summaries[[arm]] <- margot_summary_cate_difference_gain(
              result_outcomes,
              outcome_var = model_name,
              reference_curve = "baseline",
              comparison_curve = arm,
              spend = s
            )
          }

          model_output$diff_gain_summaries[[paste0("spend_", s)]] <- spend_summaries
        }
      }

      if (save_plots) {
        plot_names <- c("policy_tree_plot", "qini_plot", "decision_tree_visualisation", "policy_combo_plot")
        for (plot_name in plot_names) {
          if (!is.null(model_output[[plot_name]])) {
            file_name <- file.path(output_dir, paste0(model_name, "_", plot_name, ".png"))
            ggsave(file_name, model_output[[plot_name]], dpi = dpi, width = width, height = height)
            cli::cli_alert_success(paste("Saved", file_name))
          }
        }
      }

      output_list[[model_name]] <- model_output
      cli::cli_alert_success(paste("Successfully processed model:", model_name))
    }, error = function(e) {
      cli::cli_alert_danger(paste("Error processing model", model_name, ":", e$message))
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("margot_batch_policy function completed successfully \U0001F44D")
  return(output_list)
}
# margot_batch_policy <- function(result_outcomes,
#                                 policy_tree_args = list(),
#                                 decision_tree_args = list(),
#                                 dpi = 300,
#                                 width = 12,
#                                 height = 12,
#                                 save_plots = TRUE,
#                                 output_dir = push_mods,
#                                 spend = 0.2) {
#   cli::cli_alert_info(crayon::bold("Starting margot_batch_policy function"))
#
#   if (!requireNamespace("margot", quietly = TRUE)) {
#     cli::cli_alert_danger(crayon::red("Package 'margot' is required but not installed. Please install it first."))
#     stop("Package 'margot' is required but not installed. Please install it first.")
#   }
#
#   if (save_plots && !dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#     cli::cli_alert_success(crayon::green(paste("Created output directory:", output_dir)))
#   }
#
#   model_names <- names(result_outcomes$results)
#   cli::cli_alert_info(paste("Number of models to process:", length(model_names)))
#
#   output_list <- list()
#   cli::cli_alert_info(crayon::bold("Processing models"))
#   pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#   for (model_name in model_names) {
#     cli::cli_alert_info(paste("Processing model:", model_name))
#     tryCatch({
#       model_output <- margot::margot_policy_tree(
#         mc_test = result_outcomes,
#         model_name = model_name,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args
#       )
#
#       # Add difference gain summary
#       qini_objects <- result_outcomes$results[[model_name]]$qini_objects
#       is_binary <- "cate" %in% names(qini_objects) && "ate" %in% names(qini_objects)
#
#       if (is_binary) {
#         diff_gain_summary <- margot_summary_cate_difference_gain(
#           result_outcomes,
#           outcome_var = model_name,
#           reference_curve = "ate",
#           comparison_curve = "cate",
#           spend = spend
#         )
#         model_output$diff_gain_summary <- diff_gain_summary
#       } else if ("baseline" %in% names(qini_objects)) {
#         # Multi-arm treatment
#         diff_gain_summaries <- list()
#
#         # Compare 'all_arms' with 'baseline'
#         diff_gain_summaries[["all_arms"]] <- margot_summary_cate_difference_gain(
#           result_outcomes,
#           outcome_var = model_name,
#           reference_curve = "baseline",
#           comparison_curve = "all_arms",
#           spend = spend
#         )
#
#         # Compare each individual arm with 'baseline'
#         arm_names <- setdiff(names(qini_objects), c("all_arms", "baseline"))
#         for (arm in arm_names) {
#           diff_gain_summaries[[arm]] <- margot_summary_cate_difference_gain(
#             result_outcomes,
#             outcome_var = model_name,
#             reference_curve = "baseline",
#             comparison_curve = arm,
#             spend = spend
#           )
#         }
#         model_output$diff_gain_summaries <- diff_gain_summaries
#       }
#
#       if (save_plots) {
#         plot_names <- c("policy_tree_plot", "qini_plot", "decision_tree_visualisation", "policy_combo_plot")
#         for (plot_name in plot_names) {
#           if (!is.null(model_output[[plot_name]])) {
#             file_name <- file.path(output_dir, paste0(model_name, "_", plot_name, ".png"))
#             ggsave(file_name, model_output[[plot_name]], dpi = dpi, width = width, height = height)
#             cli::cli_alert_success(crayon::green(paste("Saved", file_name)))
#           }
#         }
#       }
#
#       output_list[[model_name]] <- model_output
#       cli::cli_alert_success(crayon::green(paste("Successfully processed model:", model_name)))
#     }, error = function(e) {
#       cli::cli_alert_danger(crayon::red(paste("Error processing model", model_name, ":", e$message)))
#     })
#     cli::cli_progress_update()
#   }
#
#   cli::cli_progress_done()
#   cli::cli_alert_success(crayon::bold(crayon::green("margot_batch_policy function completed successfully \U0001F44D")))
#   return(output_list)
# }
# old
# margot_batch_policy <- function(result_outcomes,
#                                 policy_tree_args = list(),
#                                 decision_tree_args = list(),
#                                 dpi = 300,
#                                 width = 12,
#                                 height = 12,
#                                 save_plots = TRUE,
#                                 output_dir = push_mods) {
#   cli::cli_alert_info(crayon::bold("Starting margot_batch_policy function"))
#
#   if (!requireNamespace("margot", quietly = TRUE)) {
#     cli::cli_alert_danger(crayon::red("Package 'margot' is required but not installed. Please install it first."))
#     stop("Package 'margot' is required but not installed. Please install it first.")
#   }
#
#   if (save_plots && !dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#     cli::cli_alert_success(crayon::green(paste("Created output directory:", output_dir)))
#   }
#
#   model_names <- names(result_outcomes$results)
#   cli::cli_alert_info(paste("Number of models to process:", length(model_names)))
#
#   output_list <- list()
#   cli::cli_alert_info(crayon::bold("Processing models"))
#   pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#   for (model_name in model_names) {
#     cli::cli_alert_info(paste("Processing model:", model_name))
#     tryCatch({
#       model_output <- margot::margot_policy_tree(
#         mc_test = result_outcomes,
#         model_name = model_name,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args
#       )
#
#       if (save_plots) {
#         plot_names <- c("policy_tree_plot", "qini_plot", "decision_tree_visualisation", "policy_combo_plot")
#         for (plot_name in plot_names) {
#           if (!is.null(model_output[[plot_name]])) {
#             file_name <- file.path(output_dir, paste0(model_name, "_", plot_name, ".png"))
#             ggsave(file_name, model_output[[plot_name]], dpi = dpi, width = width, height = height)
#             cli::cli_alert_success(crayon::green(paste("Saved", file_name, "with resolution", dpi, "DPI")))
#           }
#         }
#       }
#
#       output_list[[model_name]] <- model_output
#       cli::cli_alert_success(crayon::green(paste("Successfully processed model:", model_name)))
#     }, error = function(e) {
#       cli::cli_alert_danger(crayon::red(paste("Error processing model", model_name, ":", e$message)))
#     })
#     cli::cli_progress_update()
#   }
#
#   cli::cli_progress_done()
#   cli::cli_alert_success(crayon::bold(crayon::green("margot_batch_policy function completed successfully \U0001F44D")))
#   return(output_list)
# }
