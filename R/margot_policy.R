#' @title Batch Processing of Policy Trees and Related Visualisations
#'
#' @description
#' Process a list of multi-arm causal forest results: generate policy-tree and decision-tree plots,
#' Qini curves, and difference-gain summaries, optionally saving each to disk.
#'
#' @param result_outcomes List returned by \code{margot_multi_arm_causal_forest()}.
#' @param policy_tree_args List of args for \code{margot_plot_policy_tree()}. Default: \code{list()}.
#' @param decision_tree_args List of args for \code{margot_plot_decision_tree()}. Default: \code{list()}.
#' @param dpi Resolution (dpi) for saved plots. Default: 600.
#' @param width Width (in inches) for saved plots. Default: 12.
#' @param height Height (in inches) for saved plots. Default: 12.
#' @param save_plots Logical; save plots to disk? Default: TRUE.
#' @param output_dir Directory for saving plots. Default: \code{here::here(push_mods)}.
#' @param spend Numeric vector of spend levels for difference-gain summaries. Default: \code{c(0.2, 0.5)}.
#' @param label_mapping Named list mapping variable names to display labels. Default: NULL.
#' @param original_df Optional data.frame of untransformed variables for axis annotations. Default: NULL.
#' @param model_names Character vector of model names to process; NULL = all. Default: NULL.
#'
#' @return A named list; each element corresponds to a model and contains:
#'   \describe{
#'     \item{policy_tree_plot}{ggplot of the policy-tree plot}
#'     \item{policy_tree_interpretation}{character interpretation}
#'     \item{qini_plot}{ggplot of the Qini curve}
#'     \item{decision_tree_visualisation}{ggplot of the decision tree}
#'     \item{policy_combo_plot}{ggplot of combined policy vs decision}
#'     \item{diff_gain_summaries}{list of difference-gain summaries per spend}
#'   }
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @import here
#' @importFrom ggplot2 ggsave
#' @export
margot_policy <- function(
    result_outcomes,
    policy_tree_args   = list(),
    decision_tree_args = list(),
    dpi                = 600,
    width              = 12,
    height             = 12,
    save_plots         = TRUE,
    output_dir         = here::here(push_mods),
    spend              = c(0.2, 0.5),
    label_mapping      = NULL,
    original_df        = NULL,
    model_names        = NULL
) {
  cli::cli_alert_info("starting margot_policy function")

  # Legacy compatibility: map deprecated shade_non_decision → shading
     if ("shade_non_decision" %in% names(policy_tree_args)) {
          policy_tree_args$shading <- isTRUE(policy_tree_args$shade_non_decision)
          policy_tree_args$shade_non_decision <- NULL
       }

  # Determine models to process
  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- names(result_outcomes$results)
  }

  # Prepare output directory
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success("created output directory: {output_dir}")
  }

  cli::cli_alert_info("processing {length(model_names)} models")
  pb <- cli::cli_progress_bar(
    total  = length(model_names),
    format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )

  output_list <- vector("list", length(model_names))
  names(output_list) <- model_names

  for (model_name in model_names) {
    cli::cli_alert_info("processing model: {model_name}")
    tryCatch({
      # Call policy+decision combo
      combo_args <- list(
        result_object      = result_outcomes,
        model_name         = model_name,
        label_mapping      = label_mapping,
        original_df        = original_df,
        policy_tree_args   = policy_tree_args,
        decision_tree_args = decision_tree_args
      )
      model_output <- do.call(margot_plot_policy_combo, combo_args)

      # Qini plot
      model_output$qini_plot <- margot_plot_qini(
        mc_result     = result_outcomes,
        outcome_var   = model_name,
        label_mapping = label_mapping
      )

      # Difference-gain summaries
      qini_objs  <- result_outcomes$results[[model_name]]$qini_objects
      is_binary  <- all(c("cate","ate") %in% names(qini_objs))
      model_output$diff_gain_summaries <- list()

      if (is_binary) {
        for (s in spend) {
          model_output$diff_gain_summaries[[paste0("spend_", s)]] <-
            margot_summary_cate_difference_gain(
              result_outcomes,
              outcome_var      = model_name,
              reference_curve  = "ate",
              comparison_curve = "cate",
              spend            = s
            )
        }
      } else if ("baseline" %in% names(qini_objs)) {
        arm_names <- setdiff(names(qini_objs), c("all_arms","baseline"))
        for (s in spend) {
          sums <- list(
            all_arms = margot_summary_cate_difference_gain(
              result_outcomes,
              outcome_var      = model_name,
              reference_curve  = "baseline",
              comparison_curve = "all_arms",
              spend            = s
            )
          )
          for (arm in arm_names) {
            sums[[arm]] <- margot_summary_cate_difference_gain(
              result_outcomes,
              outcome_var      = model_name,
              reference_curve  = "baseline",
              comparison_curve = arm,
              spend            = s
            )
          }
          model_output$diff_gain_summaries[[paste0("spend_", s)]] <- sums
        }
      }

      # Save plots
      if (save_plots) {
        plots_to_save <- c("combined_plot","policy_tree","decision_tree","qini_plot")
        for (pn in plots_to_save) {
          if (!is.null(model_output[[pn]])) {
            fname <- file.path(output_dir, paste0(model_name, "_", pn, ".png"))
            ggplot2::ggsave(
              filename = fname,
              plot     = model_output[[pn]],
              dpi      = dpi,
              width    = width,
              height   = height
            )
            cli::cli_alert_success("saved plot to {fname}")
          }
        }
      }

      output_list[[model_name]] <- model_output
      cli::cli_alert_success("successfully processed {model_name}")
    }, error = function(e) {
      cli::cli_alert_danger("error processing {model_name}: {e$message}")
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("margot_policy completed 👍")
  output_list
}

# margot_policy <- function(result_outcomes,
#                           policy_tree_args = list(),
#                           decision_tree_args = list(),
#                           dpi = 600,
#                           width = 12,
#                           height = 12,
#                           save_plots = TRUE,
#                           output_dir = here::here(push_mods),
#                           spend = c(.2, .5),
#                           label_mapping = NULL,
#                           original_df = NULL,
#                           model_names = NULL) {
#   cli::cli_alert_info("Starting margot_policy function")
#
#   # Set model_names to all if not provided.
#   if (is.null(model_names) || length(model_names) == 0) {
#     model_names <- names(result_outcomes$results)
#   }
#
#   if (save_plots && !dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#     cli::cli_alert_success(paste("Created output directory:", output_dir))
#   }
#
#   cli::cli_alert_info(paste("Number of models to process:", length(model_names)))
#
#   output_list <- list()
#   cli::cli_alert_info("Processing models")
#   pb <- cli::cli_progress_bar(total = length(model_names),
#                               format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#   for (model_name in model_names) {
#     cli::cli_alert_info(paste("Processing model:", model_name))
#     tryCatch({
#       # Prepare arguments for margot_plot_policy_combo.
#       combo_args <- list(
#         result_object = result_outcomes,
#         model_name = model_name,
#         label_mapping = label_mapping,
#         original_df = original_df,
#         policy_tree_args = policy_tree_args,
#         decision_tree_args = decision_tree_args
#       )
#
#       # Remove NULL arguments.
#       combo_args <- combo_args[!sapply(combo_args, is.null)]
#
#       # Call margot_plot_policy_combo with the prepared arguments.
#       model_output <- do.call(margot_plot_policy_combo, combo_args)
#
#       # Add Qini plot.
#       model_output$qini_plot <- margot_plot_qini(
#         mc_result = result_outcomes,
#         outcome_var = model_name,
#         label_mapping = label_mapping
#       )
#
#       # Add difference gain summary for each spend level.
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
#         # Multi-arm treatment.
#         model_output$diff_gain_summaries <- list()
#
#         for (s in spend) {
#           spend_summaries <- list()
#
#           # Compare 'all_arms' with 'baseline'.
#           spend_summaries[["all_arms"]] <- margot_summary_cate_difference_gain(
#             result_outcomes,
#             outcome_var = model_name,
#             reference_curve = "baseline",
#             comparison_curve = "all_arms",
#             spend = s
#           )
#
#           # Compare each individual arm with 'baseline'.
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
#         # Updated plot names based on produced output objects.
#         plot_names <- c("combined_plot", "policy_tree", "decision_tree", "qini_plot")
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
