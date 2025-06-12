#' Flip CATE Estimates for Selected Outcomes in GRF Model Results
#'
#' @description
#' This function post-processes the results from margot_causal_forest to flip CATE estimates,
#' RATE results, QINI RATE results, and QINI curves for selected outcomes. This is useful when
#' you want to ensure consistent interpretation of effects across differently coded outcomes.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which CATE estimates should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A modified copy of the model_results list with flipped CATE estimates and related outputs
#'         for the specified outcomes.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom grf rank_average_treatment_effect
#'
margot_flip_forests <- function(model_results, flip_outcomes, model_prefix = "model_", verbose = TRUE) {

  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
  }

  if (!is.character(flip_outcomes) || length(flip_outcomes) == 0) {
    warning("flip_outcomes is empty or not a character vector, no flipping will be performed")
    return(model_results)
  }

  # create a copy of the model_results to avoid modifying the original
  results_copy <- model_results

  # get the names of all models in the results
  all_models <- names(results_copy$results)

  # create list of model names to flip (adding prefix if needed)
  models_to_flip <- paste0(ifelse(grepl(paste0("^", model_prefix), flip_outcomes),
                                  "",
                                  model_prefix),
                           flip_outcomes)
  models_to_flip <- models_to_flip[models_to_flip %in% all_models]

  if (length(models_to_flip) == 0) {
    warning("none of the specified outcomes match any models in the results")
    return(model_results)
  }

  if (verbose) {
    cli::cli_alert_info(paste("flipping cate estimates for", length(models_to_flip),
                              "outcomes:", paste(gsub(model_prefix, "", models_to_flip), collapse = ", ")))
  }

  # process each model that needs flipping
  for (model_name in models_to_flip) {
    if (verbose) cli::cli_alert_info(paste("processing", model_name))

    # get the original model results
    model_result <- results_copy$results[[model_name]]

    # check if this model has valid results
    if (is.null(model_result) || !is.list(model_result)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- invalid or missing results"))
      next
    }

    # 1. Flip tau_hat if it exists
    if (!is.null(model_result$tau_hat)) {
      model_result$tau_hat_original <- model_result$tau_hat  # preserve the original
      model_result$tau_hat_flipped <- -model_result$tau_hat  # create a flipped version
      if (verbose) cli::cli_alert_info(paste("flipped tau_hat for", model_name))
    }

    # 2. Recalculate RATE and QINI RATE if the originals exist and we have a model object
    if (!is.null(results_copy$full_models) && (model_name %in% names(results_copy$full_models))) {
      model_obj <- results_copy$full_models[[model_name]]
      flipped_tau_hat <- -model_result$tau_hat

      # 2a. Regular RATE
      if (!is.null(model_result$rate_result)) {
        tryCatch({
          model_result$rate_result_original <- model_result$rate_result  # preserve original
          model_result$rate_result <- grf::rank_average_treatment_effect(model_obj, flipped_tau_hat)
          if (verbose) cli::cli_alert_info(paste("recalculated RATE for", model_name))
        }, error = function(e) {
          if (verbose) cli::cli_alert_warning(paste("error recalculating RATE for", model_name, "-", e$message))
        })
      }

      # 2b. QINI RATE (if it exists)
      if (!is.null(model_result$rate_qini)) {
        tryCatch({
          model_result$rate_qini_original <- model_result$rate_qini  # preserve original
          model_result$rate_qini <- grf::rank_average_treatment_effect(model_obj, flipped_tau_hat, target = "QINI")
          if (verbose) cli::cli_alert_info(paste("recalculated QINI RATE for", model_name))
        }, error = function(e) {
          if (verbose) cli::cli_alert_warning(paste("error recalculating QINI RATE for", model_name, "-", e$message))
        })
      }
    }

    # 3. Handle QINI curves if they exist
    if (!is.null(model_result$qini_data)) {
      model_result$qini_data_original <- model_result$qini_data  # preserve original
      model_result$qini_needs_recalculation <- TRUE
      if (verbose) cli::cli_alert_info(paste("marked QINI for recalculation for", model_name))
    }

    # # 4. Handle policy trees - check dr_scores structure carefully
    # if (!is.null(model_result$dr_scores)) {
    #   # First, check if dr_scores has the expected structure
    #   dr_scores <- model_result$dr_scores
    #
    #   tryCatch({
    #     # Check for dimensions and expected structure
    #     if (is.matrix(dr_scores) && ncol(dr_scores) >= 3) {
    #       model_result$dr_scores_original <- dr_scores  # preserve original
    #
    #       # Create flipped dr_scores by swapping columns 2 and 3
    #       flipped_dr_scores <- dr_scores
    #       flipped_dr_scores[, 2:3] <- flipped_dr_scores[, c(3, 2)]
    #       model_result$dr_scores_flipped <- flipped_dr_scores
    #       model_result$policy_trees_need_recalculation <- TRUE
    #
    #       if (verbose) cli::cli_alert_info(paste("prepared flipped dr_scores for", model_name))
    #     } else {
    #       if (verbose) {
    #         if (!is.matrix(dr_scores)) {
    #           cli::cli_alert_warning(paste("dr_scores for", model_name, "is not a matrix"))
    #         } else {
    #           cli::cli_alert_warning(paste("dr_scores for", model_name,
    #                                        "doesn't have enough columns (has", ncol(dr_scores), "columns)"))
    #         }
    #       }
    #     }
    #   }, error = function(e) {
    #     if (verbose) cli::cli_alert_warning(paste("error processing dr_scores for", model_name, "-", e$message))
    #   })
    # }
    # 4. handle policy trees – flip dr_scores
    if (!is.null(model_result$dr_scores)) {

      dr_scores <- model_result$dr_scores

      tryCatch({
        if (is.matrix(dr_scores)) {

          # keep a copy of the original
          model_result$dr_scores_original <- dr_scores

          # flip the orientation by multiplying every column by -1
          flipped_dr_scores <- -dr_scores

          model_result$dr_scores_flipped           <- flipped_dr_scores
          model_result$policy_trees_need_recalculation <- TRUE

          if (verbose)
            cli::cli_alert_info(paste("flipped dr_scores for", model_name))

        } else if (verbose) {
          cli::cli_alert_warning(paste("dr_scores for", model_name, "is not a matrix"))
        }

      }, error = function(e) {
        if (verbose)
          cli::cli_alert_warning(paste("error processing dr_scores for", model_name, "-", e$message))
      })
    }
    # update the model result in our copy
    results_copy$results[[model_name]] <- model_result
  }

  # Add information about which outcomes were flipped
  results_copy$flip_outcomes <- gsub(model_prefix, "", models_to_flip)
  results_copy$flip_outcomes_postprocessed <- TRUE

  if (verbose) cli::cli_alert_success("finished flipping specified outcomes")

  return(results_copy)
}

#' Recalculate Policy Trees with Flipped Double Robust Scores
#'
#' @description
#' This function recalculates policy trees using flipped double robust scores for outcomes
#' that were processed by margot_flip_forests.
#'
#' @param model_results A list containing the model results after running margot_flip_forests().
#' @param outcomes_to_recalculate A character vector of outcome names to recalculate policy trees for.
#'                               If NULL, recalculates all outcomes marked as needing recalculation.
#' @param model_prefix A character string indicating the prefix used for model names. Default is "model_".
#' @param verbose Logical indicating whether to display detailed messages. Default is TRUE.
#'
#' @return An updated copy of model_results with recalculated policy trees.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom policytree policy_tree
#'
#' @keywords internal
margot_recalculate_policy_trees <- function(model_results, outcomes_to_recalculate = NULL,
                                            model_prefix = "model_", verbose = TRUE) {

  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element")
  }

  # create a copy to avoid modifying the original
  results_copy <- model_results

  # if no specific outcomes provided, find all that need recalculation
  if (is.null(outcomes_to_recalculate)) {
    outcomes_to_recalculate <- names(results_copy$results)

    # filter to only those that have been marked as needing recalculation
    outcomes_to_recalculate <- outcomes_to_recalculate[sapply(outcomes_to_recalculate, function(name) {
      !is.null(results_copy$results[[name]]$policy_trees_need_recalculation) &&
        results_copy$results[[name]]$policy_trees_need_recalculation
    })]
  } else {
    # add prefix if needed
    outcomes_to_recalculate <- paste0(
      ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate),
             "",
             model_prefix),
      outcomes_to_recalculate
    )
  }

  if (length(outcomes_to_recalculate) == 0) {
    if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
    return(results_copy)
  }

  if (verbose) {
    cli::cli_alert_info(paste("recalculating policy trees for", length(outcomes_to_recalculate),
                              "outcomes:", paste(gsub(model_prefix, "", outcomes_to_recalculate), collapse = ", ")))
  }

  # ensure we have covariates
  if (is.null(model_results$covariates)) {
    warning("covariates not found in model_results - policy trees cannot be recalculated")
    return(results_copy)
  }

  covariates <- model_results$covariates
  not_missing <- model_results$not_missing

  if (is.null(not_missing)) {
    not_missing <- which(complete.cases(covariates))
    if (verbose) cli::cli_alert_info("using complete cases for not_missing")
  }

  full <- seq_len(nrow(covariates))
  full <- full[which(full %in% not_missing)]

  # process each outcome
  for (model_name in outcomes_to_recalculate) {
    if (verbose) cli::cli_alert_info(paste("recalculating policy trees for", model_name))

    model_result <- results_copy$results[[model_name]]

    # check if this model has valid flipped dr_scores
    if (is.null(model_result) || is.null(model_result$dr_scores_flipped)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- missing flipped dr_scores"))
      next
    }

    # get the flipped dr_scores
    flipped_dr_scores <- model_result$dr_scores_flipped

    # get top variables
    top_vars <- model_result$top_vars
    if (is.null(top_vars)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- missing top_vars"))
      next
    }

    # recalculate policy tree depth 1
    tryCatch({
      model_result$policy_tree_depth_1_original <- model_result$policy_tree_depth_1  # preserve original
      model_result$policy_tree_depth_1 <- policytree::policy_tree(
        covariates[full, ], flipped_dr_scores[full, ], depth = 1
      )
      if (verbose) cli::cli_alert_info(paste("recalculated policy_tree_depth_1 for", model_name))
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning(paste("error recalculating policy_tree_depth_1 for", model_name, "-", e$message))
    })

    # recalculate policy tree depth 2
    train_proportion <- 0.7  # default value, could be made a parameter
    n_non_missing <- length(not_missing)
    train_size <- floor(train_proportion * n_non_missing)
    train_indices <- sample(not_missing, train_size)

    tryCatch({
      model_result$policy_tree_depth_2_original <- model_result$policy_tree_depth_2  # preserve original
      model_result$policy_tree_depth_2 <- policytree::policy_tree(
        covariates[train_indices, top_vars], flipped_dr_scores[train_indices, ], depth = 2
      )

      # update plot data too
      test_indices <- setdiff(not_missing, train_indices)
      X_test <- covariates[test_indices, top_vars]
      predictions <- predict(model_result$policy_tree_depth_2, X_test)

      model_result$plot_data_original <- model_result$plot_data  # preserve original
      model_result$plot_data <- list(
        X_test = X_test,
        predictions = predictions
      )

      if (verbose) cli::cli_alert_info(paste("recalculated policy_tree_depth_2 for", model_name))
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning(paste("error recalculating policy_tree_depth_2 for", model_name, "-", e$message))
    })

    # mark as recalculated
    model_result$policy_trees_need_recalculation <- FALSE
    model_result$policy_trees_recalculated <- TRUE

    # update the model result in our copy
    results_copy$results[[model_name]] <- model_result
  }

  if (verbose) cli::cli_alert_success("finished recalculating policy trees")

  return(results_copy)
}

#' Inspect and Debug Model Result Structure
#'
#' @description
#' This utility function inspects the structure of a specific model result or all models
#' in the results list, helping to debug issues with flipping estimates.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param model_name Name of a specific model to inspect. If NULL, summarizes all models.
#' @param details Logical indicating whether to show detailed structure of objects. Default is FALSE.
#'
#' @return Invisibly returns a summary of the model structure.
#'
#' @keywords internal
margot_inspect_model <- function(model_results, model_name = NULL, details = FALSE) {
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    cat("ERROR: model_results is not a valid model results object\n")
    return(invisible(NULL))
  }

  # If specific model name provided
  if (!is.null(model_name)) {
    # Add prefix if needed
    if (!grepl("^model_", model_name)) {
      model_name <- paste0("model_", model_name)
    }

    if (!model_name %in% names(model_results$results)) {
      cat("ERROR: model", model_name, "not found in results\n")
      cat("Available models:", paste(names(model_results$results), collapse = ", "), "\n")
      return(invisible(NULL))
    }

    model_result <- model_results$results[[model_name]]
    cat("Model:", model_name, "\n")
    cat("Components:\n")

    # List the components
    for (component in names(model_result)) {
      obj <- model_result[[component]]
      obj_class <- class(obj)[1]
      obj_dim <- if (is.matrix(obj) || is.data.frame(obj)) {
        paste0(dim(obj)[1], "×", dim(obj)[2])
      } else if (is.vector(obj)) {
        length(obj)
      } else {
        "N/A"
      }

      cat(sprintf("- %s: [%s] Size: %s\n", component, obj_class, obj_dim))

      # Print more details if requested
      if (details) {
        if (component == "dr_scores" && is.matrix(obj)) {
          cat("  DR Scores structure:\n")
          cat("  - Number of columns:", ncol(obj), "\n")
          if (ncol(obj) > 0) {
            cat("  - First few rows:\n")
            print(head(obj, 3))
          }
        } else if (component == "tau_hat" && is.vector(obj)) {
          cat("  tau_hat summary: min=", min(obj), ", max=", max(obj),
              ", mean=", mean(obj), "\n")
        } else if (component %in% c("rate_result", "rate_qini") && is.data.frame(obj)) {
          cat("  RATE summary:\n")
          print(obj)
        }
      }
    }

    # Check if model has full model object
    if (!is.null(model_results$full_models) && model_name %in% names(model_results$full_models)) {
      cat("Full model object available: YES\n")
    } else {
      cat("Full model object available: NO\n")
    }

  } else {
    # Summarize all models
    cat("Model Results Summary\n")
    cat("Number of models:", length(model_results$results), "\n")
    cat("Models:\n")

    for (name in names(model_results$results)) {
      model <- model_results$results[[name]]
      components <- names(model)

      # Check for key components
      has_tau_hat <- "tau_hat" %in% components
      has_dr_scores <- "dr_scores" %in% components
      has_policy_trees <- any(grepl("policy_tree", components))
      has_rate <- "rate_result" %in% components
      has_qini_rate <- "rate_qini" %in% components
      has_qini <- "qini_data" %in% components

      # Dr_scores structure
      dr_scores_structure <- if (has_dr_scores) {
        dr_scores <- model$dr_scores
        if (is.matrix(dr_scores)) {
          paste0(nrow(dr_scores), "×", ncol(dr_scores))
        } else {
          "not a matrix"
        }
      } else {
        "N/A"
      }

      cat(sprintf("- %s:\n", name))
      cat(sprintf("  tau_hat: %s, dr_scores: %s (%s), policy_trees: %s\n",
                  if (has_tau_hat) "YES" else "NO",
                  if (has_dr_scores) "YES" else "NO",
                  dr_scores_structure,
                  if (has_policy_trees) "YES" else "NO"))
      cat(sprintf("  RATE: %s, QINI RATE: %s, QINI: %s\n",
                  if (has_rate) "YES" else "NO",
                  if (has_qini_rate) "YES" else "NO",
                  if (has_qini) "YES" else "NO"))
    }

    # Additional info
    cat("\nAdditional Information:\n")
    cat("Has covariates:", if (!is.null(model_results$covariates)) "YES" else "NO", "\n")
    cat("Has full models:", if (!is.null(model_results$full_models)) "YES" else "NO", "\n")
    if (!is.null(model_results$full_models)) {
      cat("Full models for:", paste(names(model_results$full_models), collapse = ", "), "\n")
    }
    cat("Flipped outcomes:", if (!is.null(model_results$flip_outcomes))
      paste(model_results$flip_outcomes, collapse = ", ") else "NONE", "\n")
  }

  return(invisible(NULL))
}

#' flip cate estimates and, optionally, redo policy trees
#'
#' @param ... same arguments as `margot_flip_forests()`
#' @param recalc_policy one of "none" (default), "needed", "all".
#'        * "needed"  = redo only models that were flipped;
#'        * "all"     = redo every model in the results list.
#'
#' @return model_results list with updated components
#' @export
margot_flip_and_recalc <- function(...,
                                   recalc_policy = c("none", "needed", "all")) {

  recalc_policy <- match.arg(recalc_policy)

  flipped <- margot_flip_forests(...)

  if (recalc_policy == "none")
    return(flipped)

  outcomes <- switch(recalc_policy,
                     needed = flipped$flip_outcomes,
                     all    = sub("^model_", "", names(flipped$results)))

  margot_recalculate_policy_trees(flipped,
                                  outcomes_to_recalculate = outcomes,
                                  verbose = TRUE)
}
