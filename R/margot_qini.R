#' Generate QINI Curves and Difference Gain Summaries
#'
#' Computes QINI curves and difference gain summaries for causal forest models.
#' This function provides a direct way to generate QINI results without running
#' full policy analysis, paralleling the functionality of margot_rate().
#'
#' @param models List returned by margot_causal_forest(), containing results
#'   and optionally full_models.
#' @param model_names Optional character vector specifying which models to process.
#'   Default NULL (all models).
#' @param spend_levels Numeric vector of spend levels for difference gain summaries.
#'   Default is 0.1 (10 percent spend captures early heterogeneity patterns effectively).
#' @param baseline_method Method for generating baseline: "maq_no_covariates" (default),
#'   "auto", "simple", "maq_only", or "none". See margot_generate_qini_data() for details.
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param remove_tx_prefix Logical; remove treatment prefix from variable names (default TRUE).
#' @param remove_z_suffix Logical; remove z-score suffix from variable names (default TRUE).
#' @param use_title_case Logical; convert variable names to title case (default TRUE).
#' @param remove_underscores Logical; replace underscores with spaces (default TRUE).
#' @param verbose Logical; print progress messages (default TRUE).
#' @param seed Integer; base seed for reproducible computations (default 12345).
#' @param treatment_cost Scalar treatment cost per unit. Default 1. Lower values (e.g., 0.2)
#'   represent cheap treatments; higher values (e.g., 5) represent expensive treatments.
#'   Affects QINI curve shape - lower costs create steeper curves, higher costs create
#'   shallower curves.
#'
#' @return A list where each element corresponds to a model and contains:
#'   qini_objects (maq objects for CATE and baseline curves),
#'   qini_data (data.frame with proportion, gain, and curve columns for plotting),
#'   diff_gain_summaries (list of difference gain summaries at each spend level),
#'   model_name (the processed model name).
#'
#' @details
#' This function generates QINI curves on-demand using margot_generate_qini_data().
#' For binary treatments, it creates both CATE and baseline (e.g., ATE) curves.
#' The difference gain summaries quantify how much better CATE-based targeting
#' performs compared to the baseline at specified spend levels.
#'
#' The treatment_cost parameter allows for cost sensitivity analysis without
#' rerunning the causal forest models. Lower costs (e.g., 0.2) create steeper
#' QINI curves indicating more people can be treated cost-effectively, while
#' higher costs (e.g., 5) create shallower curves indicating only the highest-effect
#' individuals justify treatment.
#'
#' Cost Invariance Property: When treatment costs are uniform across
#' individuals, the relative benefit of CATE-based targeting over uniform allocation
#' remains constant regardless of cost level. While absolute gains scale inversely
#' with cost (gain at cost c = gain at cost 1 / c), the difference between CATE
#' and ATE curves scales proportionally, preserving the relative advantage of
#' personalized treatment. This means the value of heterogeneous treatment effects
#' for targeting decisions is independent of the uniform cost level.
#'
#' The output is structured to be compatible with margot_interpret_qini() and
#' other QINI visualization functions.
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_h2
#' @importFrom purrr map
#' @importFrom stats setNames
margot_qini <- function(models,
                        model_names = NULL,
                        spend_levels = 0.1,
                        baseline_method = "maq_no_covariates",
                        label_mapping = NULL,
                        remove_tx_prefix = TRUE,
                        remove_z_suffix = TRUE,
                        use_title_case = TRUE,
                        remove_underscores = TRUE,
                        verbose = TRUE,
                        seed = 12345,
                        treatment_cost = 1) {
  # validate inputs
  if (!is.list(models) || is.null(models$results)) {
    stop("models must be a list returned by margot_causal_forest() with a 'results' component")
  }

  # determine which models to process
  all_model_names <- names(models$results)
  if (!is.null(model_names)) {
    # handle model names with or without "model_" prefix
    model_names_with_prefix <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )

    missing <- setdiff(model_names_with_prefix, all_model_names)
    if (length(missing) > 0) {
      cli::cli_alert_warning("Models not found: {paste(gsub('model_', '', missing), collapse = ', ')}")
    }
    selected_models <- intersect(model_names_with_prefix, all_model_names)
  } else {
    selected_models <- all_model_names
  }

  if (length(selected_models) == 0) {
    stop("No valid models to process")
  }

  if (verbose) {
    cli::cli_h2("Generating QINI curves for {length(selected_models)} models")
  }

  # process each model
  results <- list()

  for (model_name in selected_models) {
    if (verbose) {
      cli::cli_alert_info("Processing {model_name}")
    }

    tryCatch(
      {
        model_result <- models$results[[model_name]]

        # extract necessary data
        outcome_name_clean <- gsub("^model_", "", model_name)
        is_flipped <- grepl("_r$", outcome_name_clean)
        outcome_data <- NULL
        treatment <- NULL
        weights <- models$weights

        # check if we need to regenerate due to baseline method or treatment cost change
        force_regenerate <- FALSE
        if (!is.null(model_result$qini_objects) && !is.null(model_result$qini_metadata$baseline_method)) {
          if (model_result$qini_metadata$baseline_method != baseline_method) {
            if (verbose) {
              cli::cli_alert_info("Baseline method changed from {model_result$qini_metadata$baseline_method} to {baseline_method}, will regenerate QINI curves")
            }
            force_regenerate <- TRUE
          }
        }

        # also check if treatment cost has changed
        if (!is.null(model_result$qini_objects) && !is.null(model_result$qini_metadata$treatment_cost)) {
          if (model_result$qini_metadata$treatment_cost != treatment_cost) {
            if (verbose) {
              cli::cli_alert_info("Treatment cost changed from {model_result$qini_metadata$treatment_cost} to {treatment_cost}, will regenerate QINI curves")
            }
            force_regenerate <- TRUE
          }
        }

        # try to get outcome data
        if (!is.null(models$data)) {
          if (outcome_name_clean %in% names(models$data)) {
            outcome_data <- models$data[[outcome_name_clean]]
          } else if (model_name %in% names(models$data)) {
            outcome_data <- models$data[[model_name]]
          }
        }

        # try to get treatment data
        if (!is.null(models$W)) {
          treatment <- models$W
        }

        # for flipped models, prioritize data from forest object
        if (is_flipped || is.null(outcome_data) || is.null(treatment)) {
          forest <- NULL
          if (!is.null(models$full_models) && !is.null(models$full_models[[model_name]])) {
            forest <- models$full_models[[model_name]]
          } else if (!is.null(model_result$model)) {
            forest <- model_result$model
          }

          if (!is.null(forest)) {
            if (is.null(outcome_data) && !is.null(forest$Y.orig)) {
              outcome_data <- as.vector(forest$Y.orig)
              if (is_flipped && verbose) {
                cli::cli_alert_info("Using outcome data from forest object for flipped model")
              }
            }
            if (is.null(treatment) && !is.null(forest$W.orig)) {
              treatment <- as.vector(forest$W.orig)
            }
          }
        }

        if (is.null(outcome_data) || is.null(treatment)) {
          cli::cli_alert_warning("Cannot find outcome or treatment data for {model_name}, skipping")
          next
        }

        # clear existing QINI objects if we need to regenerate
        if (force_regenerate) {
          model_result$qini_objects <- NULL
          model_result$qini_data <- NULL
        }

        # generate qini data with seed for reproducibility
        withr::with_seed(seed + as.integer(factor(model_name, levels = all_model_names)), {
          qini_result <- margot_generate_qini_data(
            model_result = model_result,
            outcome_data = outcome_data,
            treatment = treatment,
            weights = weights,
            baseline_method = baseline_method,
            seed = seed,
            verbose = verbose, # pass through verbose flag
            treatment_cost = treatment_cost # pass through treatment cost
          )
        })

        # update the models object with the new QINI objects
        if (!is.null(qini_result$qini_objects)) {
          # always update if we generated new objects (either first time or regenerated)
          if (is.null(model_result$qini_objects) || force_regenerate) {
            models$results[[model_name]]$qini_objects <- qini_result$qini_objects
            models$results[[model_name]]$qini_data <- qini_result$qini_data
            if (is.null(models$results[[model_name]]$qini_metadata)) {
              models$results[[model_name]]$qini_metadata <- list()
            }
            models$results[[model_name]]$qini_metadata$baseline_method <- baseline_method
            models$results[[model_name]]$qini_metadata$treatment_cost <- treatment_cost
          }
        }

        # compute difference gain summaries
        diff_gain_summaries <- list()

        if (!is.null(qini_result$qini_objects)) {
          qini_objs <- qini_result$qini_objects

          # for binary treatment
          if (verbose) {
            cli::cli_alert_info("QINI objects available: {paste(names(qini_objs), collapse = ', ')}")
          }
          # only compute diff gains if we have both cate and ate curves
          has_cate <- "cate" %in% names(qini_objs)
          has_ate <- "ate" %in% names(qini_objs)

          if (has_cate && has_ate) {
            for (spend in spend_levels) {
              if (verbose) {
                cli::cli_alert_info("Computing difference gain at spend={spend} for {model_name}")
              }
              diff_gain_summaries[[paste0("spend_", spend)]] <- tryCatch(
                {
                  margot_summary_cate_difference_gain(
                    models,
                    outcome_var = model_name,
                    reference_curve = "ate",
                    comparison_curve = "cate",
                    spend = spend
                  )
                },
                error = function(e) {
                  cli::cli_alert_warning("Failed to compute difference gain at spend={spend} for {model_name}: {e$message}")
                  NULL
                }
              )
            }
          } else {
            if (verbose) {
              missing_curves <- character()
              if (!has_cate) missing_curves <- c(missing_curves, "cate")
              if (!has_ate) missing_curves <- c(missing_curves, "ate")
              cli::cli_alert_warning("Cannot compute difference gains for {model_name}: missing {paste(missing_curves, collapse = ' and ')} curve(s)")
            }
          }
        } else {
          if (verbose) {
            cli::cli_alert_warning("No QINI objects found for {model_name}")
          }
        }

        # apply label transformations to model name
        display_name <- transform_var_name(
          outcome_name_clean,
          label_mapping,
          remove_tx_prefix,
          remove_z_suffix,
          use_title_case,
          remove_underscores
        )

        # store results
        results[[model_name]] <- list(
          qini_objects = qini_result$qini_objects,
          qini_data = qini_result$qini_data,
          diff_gain_summaries = diff_gain_summaries,
          model_name = display_name,
          baseline_method = baseline_method,
          qini_metadata = list(
            baseline_method = baseline_method,
            treatment_cost = treatment_cost
          )
        )

        if (verbose) {
          cli::cli_alert_success("Completed {model_name}")
        }
      },
      error = function(e) {
        cli::cli_alert_warning("Error processing {model_name}: {e$message}")
      }
    )
  }

  # add class for potential method dispatch
  class(results) <- c("margot_qini_results", class(results))

  return(results)
}
