#' @title Batch Processing of Policy Trees and Related Visualisations
#'
#' @description
#' Process a list of multi-arm causal forest results: generate policy-tree and
#' decision-tree plots, Qini curves, and difference-gain summaries.
#' Users can toggle which outputs to include via the `output_objects` parameter.
#'
#' @param result_outcomes List returned by \code{margot_multi_arm_causal_forest()}.
#' @param policy_tree_args List of args for \code{margot_plot_policy_tree()}. Default: \code{list()}.
#' @param decision_tree_args List of args for \code{margot_plot_decision_tree()}. Default: \code{list()}.
#' @param max_depth Integer, 1 or 2; which decision tree depth to plot. Default: 2.
#' @param spend_levels Numeric vector of spend levels for difference-gain summaries. Default: \code{0.1}.
#' @param label_mapping Named list mapping variable names to display labels. Default: NULL.
#' @param original_df Optional data.frame of untransformed variables for axis annotations. Default: NULL.
#' @param model_names Character vector of model names to process; NULL = all. Default: NULL.
#' @param output_objects Character vector specifying which outputs to include.
#'   Options: "policy_tree", "decision_tree", "combined_plot", "qini_plot", "diff_gain_summaries".
#'   Default: all.
#' @param qini_args List of additional arguments to pass to margot_plot_qini(). Default: list().
#' @param baseline_method Method for generating baseline: "maq_no_covariates" (default),
#'   "auto", "simple", "maq_only", or "none". See details in margot_generate_qini_data().
#' @param seed Integer. Random seed for reproducibility in QINI computations (default 12345).
#'
#' @return A named list; each element corresponds to a model and contains only
#' the requested outputs.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom ggplot2 ggsave
#' @import here
#' @export
margot_policy <- function(
    result_outcomes,
    policy_tree_args = list(),
    decision_tree_args = list(),
    max_depth = 2L,
    spend_levels = c(0.1, 0.4),
    label_mapping = NULL,
    original_df = NULL,
    model_names = NULL,
    output_objects = c("policy_tree", "decision_tree", "combined_plot", "qini_plot", "diff_gain_summaries"),
    qini_args = list(),
    baseline_method = "maq_no_covariates",
    seed = 12345) {
  cli::cli_alert_info("starting margot_policy function")

  # validate output_objects
  allowed <- c("policy_tree", "decision_tree", "combined_plot", "qini_plot", "diff_gain_summaries")
  invalid <- setdiff(output_objects, allowed)
  if (length(invalid)) {
    stop("invalid output_objects: ", paste(invalid, collapse = ", "))
  }

  # determine models to process
  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- names(result_outcomes$results)
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
    tryCatch(
      {
        model_output <- list()

        # policy + decision outputs
        combo_needs <- intersect(output_objects, c("policy_tree", "decision_tree", "combined_plot"))
        if (length(combo_needs) > 0) {
          combo_args <- list(
            result_object      = result_outcomes,
            model_name         = model_name,
            max_depth          = max_depth,
            label_mapping      = label_mapping,
            original_df        = original_df,
            policy_tree_args   = policy_tree_args,
            decision_tree_args = decision_tree_args
          )
          combo <- do.call(margot_plot_policy_combo, combo_args)
          for (nm in combo_needs) {
            model_output[[nm]] <- combo[[nm]]
          }
        }

        # qini curve
        if ("qini_plot" %in% output_objects) {
          # prepare qini plot arguments
          qini_plot_args <- list(
            mc_result = result_outcomes,
            outcome_var = model_name,
            label_mapping = label_mapping,
            spend_levels = spend_levels,
            baseline_method = baseline_method
          )

          # merge with user-provided qini_args
          if (length(qini_args) > 0) {
            # user args override defaults
            qini_plot_args <- modifyList(qini_plot_args, qini_args)
          }

          model_output$qini_plot <- tryCatch(
            do.call(margot_plot_qini, qini_plot_args),
            error = function(e) {
              cli::cli_alert_warning("qini plot failed for {model_name}: {e$message}")
              NULL
            }
          )
        }

        # difference-gain summaries
        if ("diff_gain_summaries" %in% output_objects) {
          qini_objs <- result_outcomes$results[[model_name]]$qini_objects

          # generate qini objects if missing
          if (is.null(qini_objs)) {
            cli::cli_alert_info("Generating QINI objects on-demand for diff_gain_summaries")

            # extract necessary components
            model_result <- result_outcomes$results[[model_name]]
            outcome_name_clean <- gsub("^model_", "", model_name)
            is_flipped <- grepl("_r$", outcome_name_clean)
            outcome_data <- NULL

            # for flipped models, prioritize getting data from forest object
            if (is_flipped && !is.null(model_result$model) && !is.null(model_result$model$Y.orig)) {
              outcome_data <- model_result$model$Y.orig
            } else if (!is.null(result_outcomes$data) && outcome_name_clean %in% names(result_outcomes$data)) {
              outcome_data <- result_outcomes$data[[outcome_name_clean]]
            } else if (!is.null(result_outcomes$data) && model_name %in% names(result_outcomes$data)) {
              outcome_data <- result_outcomes$data[[model_name]]
            }

            if (!is.null(outcome_data) && !is.null(result_outcomes$W)) {
              # First check if we have data for improved QINI computation
              if (!is.null(result_outcomes$covariates) && !is.null(model_result$tau_hat)) {
                cli::cli_alert_info("Using improved QINI computation with evaluation forest")

                # determine test indices
                test_indices <- model_result$qini_metadata$test_indices
                if (is.null(test_indices)) {
                  test_indices <- which(!is.na(outcome_data) & !is.na(result_outcomes$W))
                }

                # compute QINI with improved approach
                qini_result <- compute_qini_improved(
                  Y = outcome_data[test_indices],
                  W = result_outcomes$W[test_indices],
                  X = result_outcomes$covariates[test_indices, ],
                  tau_hat = model_result$tau_hat[test_indices],
                  weights = if (!is.null(result_outcomes$weights)) result_outcomes$weights[test_indices] else NULL,
                  seed = seed,
                  n_bootstrap = 200,
                  verbose = FALSE
                )

                if (!is.null(qini_result)) {
                  qini_objs <- qini_result$qini_objects
                  # update the results with new QINI data
                  result_outcomes$results[[model_name]]$qini_objects <- qini_objs
                  result_outcomes$results[[model_name]]$qini_data <- qini_result$qini_data
                  if (!is.null(qini_result$ate_evaluation_forest)) {
                    result_outcomes$results[[model_name]]$ate_evaluation_forest <- qini_result$ate_evaluation_forest
                  }
                }
              } else {
                # fallback to old method if necessary data is missing
                cli::cli_alert_info("Using standard QINI computation")
                qini_result <- margot_generate_qini_data(
                  model_result = model_result,
                  outcome_data = outcome_data,
                  treatment = result_outcomes$W,
                  weights = result_outcomes$weights,
                  baseline_method = baseline_method,
                  seed = seed,
                  verbose = FALSE
                )
                qini_objs <- qini_result$qini_objects
              }
            }
          }

          if (is.null(qini_objs)) {
            cli::cli_alert_warning("Could not generate QINI objects for {model_name}, skipping diff_gain_summaries")
            model_output$diff_gain_summaries <- NULL
          } else {
            is_binary <- all(c("cate", "ate") %in% names(qini_objs))
            dg <- list()
            if (is_binary) {
              for (s in spend_levels) {
                dg[[paste0("spend_", s)]] <-
                  margot_summary_cate_difference_gain(
                    result_outcomes,
                    outcome_var      = model_name,
                    reference_curve  = "ate",
                    comparison_curve = "cate",
                    spend            = s
                  )
              }
            } else if ("baseline" %in% names(qini_objs)) {
              arm_names <- setdiff(names(qini_objs), c("all_arms", "baseline"))
              for (s in spend_levels) {
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
                dg[[paste0("spend_", s)]] <- sums
              }
            }
            model_output$diff_gain_summaries <- dg
            model_output$baseline_method <- baseline_method
          }
        }


        output_list[[model_name]] <- model_output
        cli::cli_alert_success("successfully processed {model_name}")
      },
      error = function(e) {
        cli::cli_alert_danger("error processing {model_name}: {e$message}")
      }
    )
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("margot_policy completed 👍")
  output_list
}
