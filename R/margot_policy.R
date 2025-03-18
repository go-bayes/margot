#' @title Batch Processing of Policy Trees and Related Visualizations
#' @param result_outcomes A list containing the results from margot_multi_arm_causal_forest().
#' @param policy_tree_args A list of arguments to pass to margot_plot_policy_tree(). Default is list().
#' @param decision_tree_args A list of arguments to pass to margot_plot_decision_tree(). Default is list().
#' @param dpi The resolution of saved plots in dots per inch. Default is 600.
#' @param width The width of saved plots in inches. Default is 12.
#' @param height The height of saved plots in inches. Default is 12.
#' @param save_plots Logical indicating whether to save plots to disk. Default is TRUE.
#' @param output_dir The directory to save plots in. Default is here::here(push_mods).
#' @param spend A vector of spend levels to use for difference gain summaries. Default is c(0.2, 0.5).
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param original_df Optional dataframe with untransformed variables, used to display split values on the data scale.
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
#' @export
#' @title Batch Processing of Policy Trees and Related Visualisations
#' @param result_outcomes A list containing the results from margot_multi_arm_causal_forest().
#' @param policy_tree_args A list of arguments to pass to margot_plot_policy_tree(). Default is list().
#' @param decision_tree_args A list of arguments to pass to margot_plot_decision_tree(). Default is list().
#' @param dpi The resolution of saved plots in dots per inch. Default is 600.
#' @param width The width of saved plots in inches. Default is 12.
#' @param height The height of saved plots in inches. Default is 12.
#' @param save_plots Logical indicating whether to save plots to disk. Default is TRUE.
#' @param output_dir The directory to save plots in. Default is here::here(push_mods).
#' @param spend A vector of spend levels to use for difference gain summaries. Default is c(.2, .5).
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param original_df Optional dataframe with untransformed variables, used to display split values on the data scale.
#' @param model_names Optional character vector specifying which models to process. If NULL, all models in result_outcomes are processed.
#' @return A list where each element corresponds to a model in the input
#'   `result_outcomes`. Each element is itself a list containing:
#'   \item{policy_tree_plot}{A ggplot object of the policy tree plot}
#'   \item{policy_tree_interpretation}{A character string interpreting the policy tree}
#'   \item{qini_plot}{A ggplot object of the Qini plot}
#'   \item{decision_tree_visualisation}{A ggplot object visualising the decision tree}
#'   \item{policy_combo_plot}{A ggplot object of the policy combo plot}
#'   \item{diff_gain_summaries}{A nested list containing difference gain summaries for each spend level}
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @import here
#' @importFrom ggplot2 ggsave
#'
#' @export
margot_policy <- function(result_outcomes,
                          policy_tree_args = list(),
                          decision_tree_args = list(),
                          dpi = 600,
                          width = 12,
                          height = 12,
                          save_plots = TRUE,
                          output_dir = here::here(push_mods),
                          spend = c(.2, .5),
                          label_mapping = NULL,
                          original_df = NULL,
                          model_names = NULL) {
  cli::cli_alert_info("Starting margot_policy function")

  # Set model_names to all if not provided.
  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- names(result_outcomes$results)
  }

  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success(paste("Created output directory:", output_dir))
  }

  cli::cli_alert_info(paste("Number of models to process:", length(model_names)))

  output_list <- list()
  cli::cli_alert_info("Processing models")
  pb <- cli::cli_progress_bar(total = length(model_names),
                              format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    tryCatch({
      # Prepare arguments for margot_plot_policy_combo.
      combo_args <- list(
        result_object = result_outcomes,
        model_name = model_name,
        label_mapping = label_mapping,
        original_df = original_df,
        policy_tree_args = policy_tree_args,
        decision_tree_args = decision_tree_args
      )

      # Remove NULL arguments.
      combo_args <- combo_args[!sapply(combo_args, is.null)]

      # Call margot_plot_policy_combo with the prepared arguments.
      model_output <- do.call(margot_plot_policy_combo, combo_args)

      # Add Qini plot.
      model_output$qini_plot <- margot_plot_qini(
        mc_result = result_outcomes,
        outcome_var = model_name,
        label_mapping = label_mapping
      )

      # Add difference gain summary for each spend level.
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
        # Multi-arm treatment.
        model_output$diff_gain_summaries <- list()

        for (s in spend) {
          spend_summaries <- list()

          # Compare 'all_arms' with 'baseline'.
          spend_summaries[["all_arms"]] <- margot_summary_cate_difference_gain(
            result_outcomes,
            outcome_var = model_name,
            reference_curve = "baseline",
            comparison_curve = "all_arms",
            spend = s
          )

          # Compare each individual arm with 'baseline'.
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
        # Updated plot names based on produced output objects.
        plot_names <- c("combined_plot", "policy_tree", "decision_tree", "qini_plot")
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
  cli::cli_alert_success("margot_policy function completed successfully \U0001F44D")
  return(output_list)
}
# margot_policy <- function(result_outcomes,
#                           policy_tree_args = list(),
#                           decision_tree_args = list(),
#                           dpi = 600,
#                           width = 12,
#                           height = 12,
#                           save_plots = TRUE,
#                           output_dir = here::here(push_mods),
#                           spend = c(0.2, 0.5),
#                           label_mapping = NULL,
#                           original_df = NULL) {
#   cli::cli_alert_info("Starting margot_policy function")
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
#       # Prepare arguments for margot_plot_policy_combo
#       combo_args <- list(
#         result_object = result_outcomes,
#         model_name = model_name,
#         label_mapping = label_mapping,
#         original_df = original_df,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args
#       )
#
#       # Remove NULL arguments
#       combo_args <- combo_args[!sapply(combo_args, is.null)]
#
#       # Call margot_plot_policy_combo with the prepared arguments
#       model_output <- do.call(margot_plot_policy_combo, combo_args)
#
#       # Add Qini plot
#       model_output$qini_plot <- margot_plot_qini(
#         mc_result = result_outcomes,
#         outcome_var = model_name,
#         label_mapping = label_mapping
#       )
#
#       # Add difference gain summary for each spend level
#       qini_objects <- result_outcomes$results[[model_name]]$qini_objects
#       is_binary <- "cate" %in% names(qini_objects) && "ate" %in% names(qini_objects)
#
#       if (is_binary) {
#         model_output$diff_gain_summaries <- list()
#         for (s in spend) {
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
#         for (s in spend) {
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
#   cli::cli_alert_success("margot_policy function completed successfully \U0001F44D")
#   return(output_list)
# }
