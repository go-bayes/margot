#' @title Batch Processing of Policy Trees and Related Visualizations (Deprecated)
#' @description This function is deprecated as of margot 0.2.1.65. Please use margot_policy() instead.
#' @param result_outcomes A list containing the results from margot_multi_arm_causal_forest().
#' @param policy_tree_args A list of arguments to pass to margot_plot_policy_tree(). Default is list().
#' @param decision_tree_args A list of arguments to pass to margot_plot_decision_tree(). Default is list().
#' @param dpi The resolution of saved plots in dots per inch. Default is 600.
#' @param width The width of saved plots in inches. Default is 12.
#' @param height The height of saved plots in inches. Default is 12.
#' @param save_plots Logical indicating whether to save plots to disk. Default is TRUE.
#' @param output_dir The directory to save plots in. Default is here::here(push_mods).
#' @param spend_levels A vector of spend levels to use for difference gain summaries. Default is c(0.1, 0.4).
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @return A list where each element corresponds to a model in the input
#'   `result_outcomes`. Each element is itself a list containing:
#'   \item{policy_tree_plot}{A ggplot object of the policy tree plot}
#'   \item{policy_tree_interpretation}{A character string interpreting the policy tree}
#'   \item{qini_plot}{A ggplot object of the Qini plot}
#'   \item{decision_tree_visualisation}{A ggplot object visualizing the decision tree}
#'   \item{policy_combo_plot}{A ggplot object of the policy combo plot}
#'   \item{diff_gain_summaries}{A nested list containing difference gain summaries for each spend level}
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @import here
#' @importFrom ggplot2 ggsave
#'
#' @keywords internal
margot_batch_policy <- function(result_outcomes,
                                policy_tree_args = list(),
                                decision_tree_args = list(),
                                dpi = 600,
                                width = 12,
                                height = 12,
                                save_plots = TRUE,
                                output_dir = here::here(push_mods),
                                spend_levels = c(0.1, 0.4),
                                label_mapping = NULL) {
  # Deprecation warning
  warning("The margot_batch_policy() function is deprecated as of margot 0.2.1.65. Please use margot_policy() instead.",
          call. = FALSE)

  cli::cli_alert_info("Starting margot_batch_policy function (Deprecated)")

  # Rest of the function remains the same...
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
      # Update margot_plot_policy_combo call to include label_mapping
      model_output <- margot_plot_policy_combo(
        result_object = result_outcomes,
        model_name = model_name,
        policy_tree_args = policy_tree_args,
        decision_tree_args = decision_tree_args,
        label_mapping = label_mapping
      )

      # Add Qini plot
      # Note: margot_batch_policy is deprecated and doesn't support qini_args
      # For CI support, use margot_policy() instead
      model_output$qini_plot <- margot_plot_qini(
        mc_result = result_outcomes,
        outcome_var = model_name,
        label_mapping = label_mapping,
        spend_levels = spend_levels
      )

      # Add difference gain summary for each spend level
      qini_objects <- result_outcomes$results[[model_name]]$qini_objects
      is_binary <- "cate" %in% names(qini_objects) && "ate" %in% names(qini_objects)

      if (is_binary) {
        model_output$diff_gain_summaries <- list()
        for (s in spend_levels) {
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

        for (s in spend_levels) {
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
  cli::cli_alert_success("margot_batch_policy function (Deprecated) completed successfully \U0001F44D")
  return(output_list)
}
# margot_batch_policy <- function(result_outcomes,
#                                 policy_tree_args = list(),
#                                 decision_tree_args = list(),
#                                 dpi = 600,
#                                 width = 12,
#                                 height = 12,
#                                 save_plots = TRUE,
#                                 output_dir = here::here(push_mods),
#                                 spend = c(0.2, 0.5)) {
#   cli::cli_alert_info("Starting margot_batch_policy function")
#
#   if (save_plots && !dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#     cli::cli_alert_success(paste("Created output directory:", output_dir))
#   }
#
#   model_names <- names(result_outcomes$results)
#   cli::cli_alert_info(paste("Number of models to process:", length(model_names)))
#
#   output_list <- list()
#   cli::cli_alert_info("Processing models")
#   pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#   for (model_name in model_names) {
#     cli::cli_alert_info(paste("Processing model:", model_name))
#     tryCatch({
#       model_output <- margot_policy_tree(
#         mc_test = result_outcomes,
#         model_name = model_name,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args
#       )
#
#       # Add difference gain summary for each spend level
#       qini_objects <- result_outcomes$results[[model_name]]$qini_objects
#       is_binary <- "cate" %in% names(qini_objects) && "ate" %in% names(qini_objects)
#
#       if (is_binary) {
#         model_output$diff_gain_summaries <- list()
#         for (s in spend_levels) {
#           diff_gain_summary <- margot_summary_cate_difference_gain(
#             result_outcomes,
#             outcome_var = model_name,
#             reference_curve = "ate",
#             comparison_curve = "cate",
#             spend = s
#           )
#           model_output$diff_gain_summaries[[paste0("spend_", s)]] <- diff_gain_summary
#         }
#       } else if ("baseline" %in% names(qini_objects)) {
#         # Multi-arm treatment
#         model_output$diff_gain_summaries <- list()
#
#         for (s in spend_levels) {
#           spend_summaries <- list()
#
#           # Compare 'all_arms' with 'baseline'
#           spend_summaries[["all_arms"]] <- margot_summary_cate_difference_gain(
#             result_outcomes,
#             outcome_var = model_name,
#             reference_curve = "baseline",
#             comparison_curve = "all_arms",
#             spend = s
#           )
#
#           # Compare each individual arm with 'baseline'
#           arm_names <- setdiff(names(qini_objects), c("all_arms", "baseline"))
#           for (arm in arm_names) {
#             spend_summaries[[arm]] <- margot_summary_cate_difference_gain(
#               result_outcomes,
#               outcome_var = model_name,
#               reference_curve = "baseline",
#               comparison_curve = arm,
#               spend = s
#             )
#           }
#
#           model_output$diff_gain_summaries[[paste0("spend_", s)]] <- spend_summaries
#         }
#       }
#
#       if (save_plots) {
#         plot_names <- c("policy_tree_plot", "qini_plot", "decision_tree_visualisation", "policy_combo_plot")
#         for (plot_name in plot_names) {
#           if (!is.null(model_output[[plot_name]])) {
#             file_name <- file.path(output_dir, paste0(model_name, "_", plot_name, ".png"))
#             ggsave(file_name, model_output[[plot_name]], dpi = dpi, width = width, height = height)
#             cli::cli_alert_success(paste("Saved", file_name))
#           }
#         }
#       }
#
#       output_list[[model_name]] <- model_output
#       cli::cli_alert_success(paste("Successfully processed model:", model_name))
#     }, error = function(e) {
#       cli::cli_alert_danger(paste("Error processing model", model_name, ":", e$message))
#     })
#     cli::cli_progress_update()
#   }
#
#   cli::cli_progress_done()
#   cli::cli_alert_success("margot_batch_policy function completed successfully \U0001F44D")
#   return(output_list)
# }
