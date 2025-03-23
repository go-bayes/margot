#' Run Multiple Generalized Random Forest (GRF) Multi-Arm Causal Forest Models with Enhanced Features
#'
#' This function is a wrapper for grf::multi_arm_causal_forest that runs multiple GRF multi-arm causal forest models
#' for specified outcome variables. It calculates average treatment effects, creates custom evaluation tables,
#' and includes additional features such as tau.hat estimates, policy trees, variable importance rankings,
#' and Qini curves. It also prepares data for policy tree visualization, using a specified proportion of the data for training.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W_multi A factor vector of multi-arm treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param exposure_name A character string specifying the name of the exposure variable.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 10.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is FALSE.
#' @param compute_qini Logical indicating whether to compute Qini curves for each model. Default is TRUE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for training policy trees. Default is 0.7.
#' @param W.hat Optional vector specifying known treatment assignment probabilities for each arm.
#' @param cost Optional vector specifying the cost associated with each treatment arm.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A list containing:
#'   \item{results}{A list of model results, one for each outcome variable. Each model result includes:
#'     \itemize{
#'       \item{ate}{Average treatment effect}
#'       \item{custom_table}{Custom evaluation table}
#'       \item{tau_hat}{Individual treatment effect estimates}
#'       \item{top_vars}{Top variables by importance}
#'       \item{variable_importance}{Data frame of variable importance rankings}
#'       \item{dr_scores}{Double robust scores}
#'       \item{policy_tree_depth_2}{Policy tree of depth 2, trained on train_proportion of non-missing data}
#'       \item{plot_data}{Data prepared for policy tree visualization, using the remaining proportion of non-missing data}
#'       \item{qini_data}{Data frame containing Qini curve data for plotting (if compute_qini is TRUE)}
#'       \item{qini_objects}{List of maq objects for each curve, used for computing average gain}
#'     }
#'   }
#'   \item{combined_tables}{A list of data frames combining custom evaluation tables grouped by comparison levels.}
#'   \item{outcome_vars}{The character vector of outcome variable names that were modeled.}
#'   \item{not_missing}{A vector of indices for complete cases.}
#'   \item{exposure_name}{The name of the exposure variable.}
#'   \item{data}{The input data (if save_data is TRUE).}
#'   \item{covariates}{The input covariates (if save_data is TRUE).}
#'   \item{weights}{The input weights (if save_data is TRUE).}
#'   \item{full_models}{A list of full GRF model objects (if save_models is TRUE).}
#'
#' @importFrom grf multi_arm_causal_forest average_treatment_effect variable_importance
#' @importFrom dplyr arrange desc
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom crayon bold green red yellow
#'
#' @note Setting save_models = TRUE typically results in very large objects (often several GB).
#'       Ensure you have sufficient memory available when using this option.
#'
#' @export
margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
                                           exposure_name, grf_defaults = list(),
                                           save_data = FALSE, top_n_vars = 20,
                                           save_models = FALSE, compute_qini = TRUE,
                                           train_proportion = 0.7, W.hat = NULL, cost = NULL,
                                           verbose = TRUE) {
  if (verbose) cli::cli_alert_info(crayon::bold("starting margot_multi_arm_causal_forest function"))

  if (save_models) {
    if (verbose) cli::cli_alert_warning(crayon::yellow(
      "note: setting save_models = TRUE typically results in very large objects (often several GB). ",
      "ensure you have sufficient memory available."
    ))
  }

  if (!is.factor(W_multi)) {
    W_multi <- as.factor(W_multi)
    if (verbose) cli::cli_alert_info("converted W_multi to factor")
  }

  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[full %in% not_missing]

  if (verbose) cli::cli_alert_info(paste("number of complete cases:", length(not_missing)))

  run_models_with_progress <- function() {
    results <- list()
    full_models <- list()

    if (verbose) cli::cli_alert_info(crayon::bold("running models for each outcome variable"))
    pb <- cli::cli_progress_bar(total = length(outcome_vars), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

    for (outcome in outcome_vars) {
      model_name <- paste0("model_", outcome)
      Y <- as.matrix(data[[outcome]])

      tryCatch({
        model <- do.call(grf::multi_arm_causal_forest,
                         c(list(X = covariates, Y = Y, W = W_multi, sample.weights = weights),
                           grf_defaults))

        ate <- grf::average_treatment_effect(model)
        custom_table <- margot::margot_model_evalue(model, new_name = outcome, subset = NULL)
        tau_hat <- predict(model, X = covariates, estimate.variance = TRUE)

        results[[model_name]] <- list(
          ate = ate,
          custom_table = custom_table,
          tau_hat = tau_hat$predictions
        )

        varimp <- grf::variable_importance(model)
        ranked_vars <- order(varimp, decreasing = TRUE)
        top_vars <- colnames(covariates)[ranked_vars[1:min(top_n_vars, length(ranked_vars))]]
        results[[model_name]]$top_vars <- top_vars
        results[[model_name]]$variable_importance <- data.frame(
          variable = colnames(covariates),
          importance = varimp
        ) %>% dplyr::arrange(dplyr::desc(importance))

        dr_scores <- policytree::double_robust_scores(model)
        results[[model_name]]$dr_scores <- dr_scores

        n_non_missing <- length(not_missing)
        train_size <- floor(train_proportion * n_non_missing)
        train_indices <- sample(not_missing, train_size)

        X_train <- covariates[train_indices, top_vars, drop = FALSE]
        dr_scores_train <- dr_scores[train_indices, , drop = FALSE]

        policy_tree_model <- tryCatch({
          policytree::policy_tree(X_train, dr_scores_train, depth = 2)
        }, error = function(e) {
          if (verbose) cli::cli_alert_warning(crayon::yellow(paste("error in policy tree for", outcome, ":", e$message)))
          NULL
        })

        results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

        if (!is.null(policy_tree_model)) {
          test_indices <- setdiff(not_missing, train_indices)
          X_test <- covariates[test_indices, top_vars, drop = FALSE]
          predictions <- predict(policy_tree_model, X_test)

          results[[model_name]]$plot_data <- list(
            X_test = X_test,
            predictions = predictions
          )
        } else {
          results[[model_name]]$plot_data <- NULL
        }

        if (compute_qini) {
          # compute qini curves
          if (verbose) cli::cli_alert_info(paste("computing qini curves for", outcome))

          qini_results <- compute_qini_curves_multi_arm(
            model = model,
            tau_hat = tau_hat$predictions,
            Y = Y,
            W_multi = W_multi,
            W.hat = W.hat,
            cost = cost,
            verbose = verbose
          )

          # validate qini results
          if (is.null(qini_results)) {
            if (verbose) cli::cli_alert_warning(paste("qini results are NULL for", outcome, "- creating fallback objects"))

            # create fallback qini objects
            qini_data <- create_fallback_qini_data(tau_hat$predictions, W_multi)
            qini_objects <- create_fallback_qini_objects(tau_hat$predictions, W_multi)

            results[[model_name]]$qini_data <- qini_data
            results[[model_name]]$qini_objects <- qini_objects
            results[[model_name]]$qini_imputed <- TRUE

            if (verbose) cli::cli_alert_success(paste("created fallback qini objects for", outcome))
          } else {
            # use the calculated qini results
            qini_data <- qini_results$qini_data
            qini_objects <- qini_results$qini_objects

            # validate objects structure
            qini_data_valid <- !is.null(qini_data) &&
              is.data.frame(qini_data) &&
              nrow(qini_data) > 0

            qini_objects_valid <- !is.null(qini_objects) &&
              is.list(qini_objects) &&
              length(qini_objects) > 0

            if (!qini_data_valid || !qini_objects_valid) {
              if (verbose) {
                cli::cli_alert_warning(crayon::yellow(paste("invalid qini structure for", outcome, "- creating fallback objects")))
                if (!qini_data_valid) cli::cli_alert_info("qini_data validation failed")
                if (!qini_objects_valid) cli::cli_alert_info("qini_objects validation failed")
              }

              # create replacements for invalid objects
              if (!qini_data_valid) {
                qini_data <- create_fallback_qini_data(tau_hat$predictions, W_multi)
              }
              if (!qini_objects_valid) {
                qini_objects <- create_fallback_qini_objects(tau_hat$predictions, W_multi)
              }

              results[[model_name]]$qini_imputed <- TRUE
            } else {
              if (attr(qini_data, "imputed", exact = TRUE)) {
                if (verbose) cli::cli_alert_warning(paste("qini data for", outcome, "was partially imputed. exercise caution when interpreting results."))
                results[[model_name]]$qini_imputed <- TRUE
              } else {
                results[[model_name]]$qini_imputed <- FALSE
                if (verbose) cli::cli_alert_success(paste("successfully created qini objects for", outcome))
              }
            }

            # store qini_data and qini_objects in the results
            results[[model_name]]$qini_data <- qini_data
            results[[model_name]]$qini_objects <- qini_objects
          }
        } else {
          # create minimal placeholder objects even when computation is turned off
          results[[model_name]]$qini_data <- create_fallback_qini_data(tau_hat$predictions, W_multi)
          results[[model_name]]$qini_objects <- create_fallback_qini_objects(tau_hat$predictions, W_multi)
          results[[model_name]]$qini_imputed <- TRUE
        }

        if (save_models) {
          full_models[[model_name]] <- model
        }

      }, error = function(e) {
        if (verbose) cli::cli_alert_danger(crayon::red(paste("error in model for", outcome, ":", e$message)))
        results[[model_name]] <- list(
          error = e$message,
          # create minimal placeholder objects even when model fails
          qini_data = data.frame(
            proportion = seq(0, 1, length.out = 100),
            gain = rep(0, 100),
            curve = "error"
          ),
          qini_objects = list(error = list("_path" = list(gain = rep(0, 100)))),
          qini_imputed = TRUE
        )
      })

      cli::cli_progress_update()
    }

    cli::cli_progress_done()

    # group results by comparison
    tryCatch({
      combined_tables <- group_results_by_comparison(results)
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning(paste("error in grouping results:", e$message))
      # create an empty list as fallback
      combined_tables <- list()
    })

    list(results = results, full_models = full_models, combined_tables = combined_tables)
  }

  model_results <- run_models_with_progress()

  if (verbose) cli::cli_alert_success(crayon::green("model runs completed successfully"))

  output <- list(
    results = model_results$results,
    combined_tables = model_results$combined_tables,
    outcome_vars = outcome_vars,
    not_missing = not_missing,
    exposure_name = exposure_name
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
    if (verbose) cli::cli_alert_info("data, covariates, and weights saved in output")
  }

  if (save_models) {
    output$full_models <- model_results$full_models
    if (verbose) cli::cli_alert_info("full model objects saved in output")
  }

  # verify that qini objects and data were created for each model
  missing_qini <- character(0)
  for (model_name in names(model_results$results)) {
    if (is.null(model_results$results[[model_name]]$qini_objects) ||
        is.null(model_results$results[[model_name]]$qini_data)) {
      missing_qini <- c(missing_qini, model_name)
    }
  }

  if (length(missing_qini) > 0) {
    if (verbose) cli::cli_alert_warning(crayon::yellow(paste("missing qini objects for models:", paste(missing_qini, collapse=", "))))
  } else {
    if (verbose) cli::cli_alert_success(crayon::green("all models have qini_objects and qini_data"))
  }

  if (verbose) cli::cli_alert_success(crayon::bold(crayon::green("margot_multi_arm_causal_forest function completed successfully \U0001F44D")))

  return(output)
}

#' Compute Qini Curves for Multi-Arm Treatments
#'
#' This function computes Qini curves for multi-arm causal forests using doubly robust scores.
#' It handles various edge cases and provides fallback mechanisms to ensure valid outputs.
#'
#' @param model A fitted GRF multi-arm causal forest model.
#' @param tau_hat Matrix of estimated treatment effects (one column per treatment arm comparison).
#' @param Y Vector or matrix of observed outcomes.
#' @param W_multi Factor vector of multi-arm treatment assignments.
#' @param W.hat Optional vector of treatment probabilities (one for each arm).
#' @param cost Optional vector of costs (one for each arm).
#' @param verbose Logical; if TRUE, print diagnostic information during execution.
#'
#' @return A list containing:
#'   \item{qini_data}{A data frame containing Qini curve data for plotting.}
#'   \item{qini_objects}{A list of maq objects for each treatment comparison.}
#'   Returns NULL if an error occurs.
#'
#' @importFrom maq maq
#' @importFrom purrr map2_dfr
#' @importFrom cli cli_alert_info cli_alert_warning
#'
#' @keywords internal
compute_qini_curves_multi_arm <- function(model, tau_hat, Y, W_multi, W.hat = NULL, cost = NULL, verbose = TRUE) {
  tryCatch({
    if (verbose) {
      cli::cli_alert_info("using doubly robust scores for multi-arm qini estimation")
      cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
      cli::cli_alert_info(paste("tau_hat dimensions:", paste(dim(tau_hat), collapse = "x")))
      cli::cli_alert_info(paste("Y class:", class(Y)))
      cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
      cli::cli_alert_info(paste("W_multi class:", class(W_multi)))
      cli::cli_alert_info(paste("W_multi length:", length(W_multi)))
    }

    # ensure Y is in the correct format
    if (is.matrix(Y) || is.array(Y)) {
      if (ncol(Y) == 1) {
        Y <- as.vector(Y)
      }
    }

    # get doubly robust scores from the model
    dr_scores <- tryCatch({
      policytree::double_robust_scores(model)
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning(paste("error getting DR scores:", e$message))
      # fallback to a simpler approach if DR scores fail
      return(NULL)
    })

    if (is.null(dr_scores)) {
      if (verbose) cli::cli_alert_warning("DR scores failed, using alternative approach")

      # create simplified qini data and objects
      qini_data <- create_fallback_qini_data(tau_hat, W_multi)
      qini_objects <- create_fallback_qini_objects(tau_hat, W_multi)

      # mark as imputed
      attr(qini_data, "imputed") <- TRUE

      return(list(
        qini_data = qini_data,
        qini_objects = qini_objects
      ))
    }

    if (verbose) {
      cli::cli_alert_info(paste("DR scores dimensions:", paste(dim(dr_scores), collapse = "x")))
    }

    # extract treatment arms
    arms <- levels(W_multi)
    n_arms <- length(arms)

    if (is.null(cost)) {
      cost <- rep(1, n_arms)
    } else if (length(cost) != n_arms) {
      if (verbose) cli::cli_alert_warning(paste("cost vector length", length(cost), "doesn't match number of arms", n_arms, "- using uniform costs"))
      cost <- rep(1, n_arms)
    }

    # compute qini objects for each arm comparison
    qini_objects <- list()
    imputed <- FALSE

    # process each column of tau_hat (each treatment comparison)
    for (i in 1:ncol(tau_hat)) {
      col_name <- colnames(tau_hat)[i]
      if (verbose) cli::cli_alert_info(paste("processing comparison:", col_name))

      tau_hat_i <- tau_hat[, i]

      # try to compute the maq object
      qini_i <- tryCatch({
        maq::maq(tau_hat_i, cost, dr_scores, R = 200)
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning(paste("error in maq for", col_name, ":", e$message))
        # create a simple fallback object
        imputed <- TRUE
        list("_path" = list(gain = cumsum(sort(tau_hat_i, decreasing = TRUE))/length(tau_hat_i)))
      })

      qini_objects[[col_name]] <- qini_i
    }

    # compute ATE qini curves
    for (i in 1:ncol(tau_hat)) {
      col_name <- colnames(tau_hat)[i]
      ate_name <- paste0("ate_", col_name)

      tau_hat_i <- tau_hat[, i]
      mean_tau <- mean(tau_hat_i)

      # try to compute the ATE maq object
      qini_ate <- tryCatch({
        maq::maq(rep(mean_tau, length(tau_hat_i)), cost, dr_scores, R = 200)
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning(paste("error in ATE maq for", col_name, ":", e$message))
        # create a simple fallback object
        imputed <- TRUE
        list("_path" = list(gain = seq(0, mean_tau, length.out = 100)))
      })

      qini_objects[[ate_name]] <- qini_ate
    }

    # determine max index across all qini objects
    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (!is.null(qini_obj) && !is.null(qini_obj[["_path"]]) && !is.null(qini_obj[["_path"]]$gain)) {
        length(qini_obj[["_path"]]$gain)
      } else {
        return(0)
      }
    }))

    if (max_index == 0) {
      if (verbose) cli::cli_alert_warning("all qini objects have empty gain, creating fallback objects")

      # create fallback objects
      qini_data <- create_fallback_qini_data(tau_hat, W_multi)

      # mark as imputed
      attr(qini_data, "imputed") <- TRUE

      return(list(
        qini_data = qini_data,
        qini_objects = qini_objects
      ))
    }

    # extract data from each qini object
    qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects),
                                 ~ extract_qini_data_multi(.x, .y, max_index, verbose))

    if (nrow(qini_data) == 0) {
      if (verbose) cli::cli_alert_warning("extracted qini data is empty, creating fallback data")

      # create fallback data
      qini_data <- create_fallback_qini_data(tau_hat, W_multi)
      imputed <- TRUE
    }

    # mark if any imputation happened
    attr(qini_data, "imputed") <- imputed

    if (verbose) {
      cli::cli_alert_success("successfully created qini_objects and qini_data")
      cli::cli_alert_info(paste("qini_data dimensions:", nrow(qini_data), "x", ncol(qini_data)))
      cli::cli_alert_info(paste("number of qini_objects:", length(qini_objects)))
      if (imputed) cli::cli_alert_warning("note: some qini data was imputed")
    }

    return(list(
      qini_data = qini_data,
      qini_objects = qini_objects
    ))
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning(paste("error in compute_qini_curves_multi_arm:", e$message))
    return(NULL)
  })
}

#' Extract Qini Data for Multi-Arm Treatments
#'
#' Extracts and formats Qini curve data from a maq object for multi-arm treatments.
#'
#' @param qini_obj A Qini object from maq.
#' @param name Name of the treatment comparison.
#' @param max_index Maximum index to extend the curve to.
#' @param verbose Logical indicating whether to display detailed messages.
#'
#' @return A data frame with proportion, gain, and curve columns.
#'
#' @keywords internal
extract_qini_data_multi <- function(qini_obj, name, max_index, verbose = TRUE) {
  gain <- if (!is.null(qini_obj) && !is.null(qini_obj[["_path"]]) && !is.null(qini_obj[["_path"]]$gain)) {
    qini_obj[["_path"]]$gain
  } else {
    if (verbose) cli::cli_alert_warning(paste("qini object", name, "is null or missing required components, extending with zeros"))
    rep(0, max_index)
  }

  gain_length <- length(gain)
  if (gain_length < max_index) {
    gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
  } else if (gain_length > max_index) {
    gain <- gain[1:max_index]
  }

  proportion <- seq_len(max_index) / max_index

  data.frame(
    proportion = proportion,
    gain = gain,
    curve = name
  )
}

#' Create Fallback Qini Data for Multi-Arm Treatments
#'
#' Creates a basic data frame to use when Qini calculations fail.
#'
#' @param tau_hat Matrix of treatment effect estimates.
#' @param W_multi Factor vector of treatment assignments.
#'
#' @return A data frame with proportion, gain, and curve columns.
#'
#' @keywords internal
create_fallback_qini_data <- function(tau_hat, W_multi) {
  n_points <- 100
  n_curves <- if (is.matrix(tau_hat)) ncol(tau_hat) else 1

  # create curve names based on tau_hat columns
  curve_names <- if (is.matrix(tau_hat) && !is.null(colnames(tau_hat))) {
    c(colnames(tau_hat), paste0("ate_", colnames(tau_hat)))
  } else {
    c(paste0("cate_", 1:n_curves), paste0("ate_", 1:n_curves))
  }

  # create a dataframe with all curves
  result <- data.frame()

  for (curve in curve_names) {
    is_ate <- grepl("^ate_", curve)

    if (is_ate) {
      # for ATE curves, create a straight line
      curve_data <- data.frame(
        proportion = seq(0, 1, length.out = n_points),
        gain = seq(0, 1, length.out = n_points),
        curve = curve
      )
    } else {
      # for CATE curves, create a curved line
      curve_data <- data.frame(
        proportion = seq(0, 1, length.out = n_points),
        gain = (seq(0, 1, length.out = n_points))^0.5,  # square root curve
        curve = curve
      )
    }

    result <- rbind(result, curve_data)
  }

  return(result)
}

#' Create Fallback Qini Objects for Multi-Arm Treatments
#'
#' Creates basic Qini objects to use when calculations fail.
#'
#' @param tau_hat Matrix of treatment effect estimates.
#' @param W_multi Factor vector of treatment assignments.
#'
#' @return A list of Qini objects with minimal structure.
#'
#' @keywords internal
create_fallback_qini_objects <- function(tau_hat, W_multi) {
  n_points <- 100
  n_curves <- if (is.matrix(tau_hat)) ncol(tau_hat) else 1

  # create curve names based on tau_hat columns
  curve_names <- if (is.matrix(tau_hat) && !is.null(colnames(tau_hat))) {
    c(colnames(tau_hat), paste0("ate_", colnames(tau_hat)))
  } else {
    c(paste0("cate_", 1:n_curves), paste0("ate_", 1:n_curves))
  }

  result <- list()

  for (curve in curve_names) {
    is_ate <- grepl("^ate_", curve)

    if (is_ate) {
      # for ATE curves, create a straight line
      result[[curve]] <- list("_path" = list(gain = seq(0, 1, length.out = n_points)))
    } else {
      # for CATE curves, create a curved line
      result[[curve]] <- list("_path" = list(gain = (seq(0, 1, length.out = n_points))^0.5))  # square root curve
    }
  }

  return(result)
}

#' Group Results by Treatment Comparison
#'
#' Groups custom evaluation tables by treatment comparison level.
#'
#' @param results A list of model results.
#'
#' @return A list of data frames, one for each treatment comparison.
#'
#' @keywords internal
group_results_by_comparison <- function(results) {
  tryCatch({
    # extract custom tables
    tables <- lapply(results, function(x) {
      if (!is.null(x$custom_table)) {
        return(x$custom_table)
      } else {
        return(NULL)
      }
    })

    # filter out NULL entries
    tables <- tables[!sapply(tables, is.null)]

    if (length(tables) == 0) {
      return(list())
    }

    # identify unique comparison levels
    all_levels <- unique(unlist(lapply(tables, function(x) {
      if (is.null(x$comparison)) return(NULL)
      unique(x$comparison)
    })))

    # group tables by comparison level
    grouped_tables <- lapply(all_levels, function(level) {
      filtered_tables <- lapply(tables, function(table) {
        if (level %in% table$comparison) {
          subset(table, comparison == level)
        } else {
          NULL
        }
      })

      filtered_tables <- filtered_tables[!sapply(filtered_tables, is.null)]

      if (length(filtered_tables) > 0) {
        do.call(rbind, filtered_tables)
      } else {
        NULL
      }
    })

    # name the list elements
    names(grouped_tables) <- all_levels

    # filter out NULL entries
    grouped_tables <- grouped_tables[!sapply(grouped_tables, is.null)]

    return(grouped_tables)
  }, error = function(e) {
    warning(paste("Error in group_results_by_comparison:", e$message))
    return(list())
  })
}
# old
# margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
#                                            exposure_name, grf_defaults = list(),
#                                            save_data = FALSE, top_n_vars = 20,
#                                            save_models = FALSE, compute_qini = TRUE,
#                                            train_proportion = 0.7, W.hat = NULL, cost = NULL,
#                                            verbose = TRUE) {
#   if (verbose) cli::cli_alert_info(crayon::bold("Starting margot_multi_arm_causal_forest function"))
#
#   if (save_models) {
#     if (verbose) cli::cli_alert_warning(crayon::yellow(
#       "Note: setting save_models = TRUE typically results in very large objects (often several GB). ",
#       "Ensure you have sufficient memory available."
#     ))
#   }
#
#   if (!is.factor(W_multi)) {
#     W_multi <- as.factor(W_multi)
#     if (verbose) cli::cli_alert_info("Converted W_multi to factor")
#   }
#
#   not_missing <- which(complete.cases(covariates))
#   full <- seq_len(nrow(covariates))
#   full <- full[which(full %in% not_missing)]
#
#   if (verbose) cli::cli_alert_info(paste("Number of complete cases:", length(not_missing)))
#
#   run_models_with_progress <- function() {
#     results <- list()
#     full_models <- list()
#
#     if (verbose) cli::cli_alert_info(crayon::bold("Running models for each outcome variable"))
#     pb <- cli::cli_progress_bar(total = length(outcome_vars), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#     for (outcome in outcome_vars) {
#       model_name <- paste0("model_", outcome)
#       Y <- as.matrix(data[[outcome]])
#
#       tryCatch({
#         model <- do.call(grf::multi_arm_causal_forest,
#                          c(list(X = covariates, Y = Y, W = W_multi, sample.weights = weights),
#                            grf_defaults))
#
#         ate <- grf::average_treatment_effect(model)
#         custom_table <- margot::margot_model_evalue(model, new_name = outcome, subset = NULL)
#         tau_hat <- predict(model, X = covariates, estimate.variance = TRUE)
#
#         results[[model_name]] <- list(
#           ate = ate,
#           custom_table = custom_table,
#           tau_hat = tau_hat$predictions
#         )
#
#         varimp <- grf::variable_importance(model)
#         ranked_vars <- order(varimp, decreasing = TRUE)
#         top_vars <- colnames(covariates)[ranked_vars[1:min(top_n_vars, length(ranked_vars))]]
#         results[[model_name]]$top_vars <- top_vars
#         results[[model_name]]$variable_importance <- data.frame(
#           variable = colnames(covariates),
#           importance = varimp
#         ) %>% dplyr::arrange(dplyr::desc(importance))
#
#         dr_scores <- policytree::double_robust_scores(model)
#         results[[model_name]]$dr_scores <- dr_scores
#
#         n_non_missing <- length(not_missing)
#         train_size <- floor(train_proportion * n_non_missing)
#         train_indices <- sample(not_missing, train_size)
#
#         X_train <- covariates[train_indices, top_vars, drop = FALSE]
#         dr_scores_train <- dr_scores[train_indices, , drop = FALSE]
#
#         policy_tree_model <- tryCatch({
#           policytree::policy_tree(X_train, dr_scores_train, depth = 2)
#         }, error = function(e) {
#           if (verbose) cli::cli_alert_warning(crayon::yellow(paste("Error in policy tree for", outcome, ":", e$message)))
#           NULL
#         })
#
#         results[[model_name]]$policy_tree_depth_2 <- policy_tree_model
#
#         if (!is.null(policy_tree_model)) {
#           test_indices <- setdiff(not_missing, train_indices)
#           X_test <- covariates[test_indices, top_vars, drop = FALSE]
#           predictions <- predict(policy_tree_model, X_test)
#
#           results[[model_name]]$plot_data <- list(
#             X_test = X_test,
#             predictions = predictions
#           )
#         } else {
#           results[[model_name]]$plot_data <- NULL
#         }
#
#         if (compute_qini) {
#           # Compute Qini curves
#           if (verbose) cli::cli_alert_info(paste("Computing Qini curves for", outcome))
#
#           qini_results <- compute_qini_curves_multi_arm(
#             tau_hat = tau_hat$predictions,
#             Y = Y,
#             W_multi = W_multi,
#             W.hat = W.hat,
#             cost = cost,
#             verbose = verbose
#           )
#
#           if (is.null(qini_results)) {
#             if (verbose) cli::cli_alert_warning(paste("Qini results are NULL for", outcome))
#           } else {
#             qini_data <- qini_results$qini_data
#             qini_objects <- qini_results$qini_objects
#
#             if (attr(qini_data, "imputed")) {
#               if (verbose) cli::cli_alert_warning(paste("Qini data for", outcome, "was imputed with zeros. Exercise caution when interpreting results."))
#               results[[model_name]]$qini_imputed <- TRUE
#             } else {
#               results[[model_name]]$qini_imputed <- FALSE
#             }
#
#             # Store qini_data and qini_objects in the results
#             results[[model_name]]$qini_data <- qini_data
#             results[[model_name]]$qini_objects <- qini_objects
#           }
#         }
#
#         if (save_models) {
#           full_models[[model_name]] <- model
#         }
#
#       }, error = function(e) {
#         if (verbose) cli::cli_alert_danger(crayon::red(paste("Error in model for", outcome, ":", e$message)))
#         results[[model_name]] <- list(error = e$message)
#       })
#
#       cli::cli_progress_update()
#     }
#
#     cli::cli_progress_done()
#     combined_tables <- group_results_by_comparison(results)
#     list(results = results, full_models = full_models, combined_tables = combined_tables)
#   }
#
#   model_results <- run_models_with_progress()
#
#   if (verbose) cli::cli_alert_success(crayon::green("Model runs completed successfully"))
#
#   output <- list(
#     results = model_results$results,
#     combined_tables = model_results$combined_tables,
#     outcome_vars = outcome_vars,
#     not_missing = not_missing,
#     exposure_name = exposure_name
#   )
#
#   if (save_data) {
#     output$data <- data
#     output$covariates <- covariates
#     output$weights <- weights
#     if (verbose) cli::cli_alert_info("Data, covariates, and weights saved in output")
#   }
#
#   if (save_models) {
#     output$full_models <- model_results$full_models
#     if (verbose) cli::cli_alert_info("Full model objects saved in output")
#   }
#
#   if (verbose) cli::cli_alert_success(crayon::bold(crayon::green("margot_multi_arm_causal_forest function completed successfully \U0001F44D")))
#
#   return(output)
# }
# margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
#                                            exposure_name, grf_defaults = list(),
#                                            save_data = FALSE, top_n_vars = 10,
#                                            save_models = FALSE, compute_qini = TRUE,
#                                            train_proportion = 0.5) {
#   cli::cli_alert_info(crayon::bold("Starting margot_multi_arm_causal_forest function"))
#
#   if (save_models) {
#     cli::cli_alert_warning(crayon::yellow(
#       "Note: setting save_models = TRUE typically results in very large objects (often several GB). ",
#       "Ensure you have sufficient memory available."
#     ))
#   }
#
#   if (!is.factor(W_multi)) {
#     W_multi <- as.factor(W_multi)
#     cli::cli_alert_info("Converted W_multi to factor")
#   }
#
#   not_missing <- which(complete.cases(covariates))
#   full <- seq_len(nrow(covariates))
#   full <- full[which(full %in% not_missing)]
#
#   cli::cli_alert_info(paste("Number of complete cases:", length(not_missing)))
#
#
#   run_models_with_progress <- function() {
#     results <- list()
#     full_models <- list()
#
#     cli::cli_alert_info(crayon::bold("Running models for each outcome variable"))
#     pb <- cli::cli_progress_bar(total = length(outcome_vars), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#
#     for (outcome in outcome_vars) {
#       model_name <- paste0("model_", outcome)
#       Y <- as.matrix(data[[outcome]])
#
#       tryCatch({
#         model <- do.call(grf::multi_arm_causal_forest,
#                          c(list(X = covariates, Y = Y, W = W_multi, sample.weights = weights),
#                            grf_defaults))
#
#         ate <- average_treatment_effect(model)
#         custom_table <- margot::margot_model_evalue(model, new_name = outcome, subset = NULL)
#         tau_hat <- predict(model, X = covariates, estimate.variance = TRUE)
#
#         results[[model_name]] <- list(
#           ate = ate,
#           custom_table = custom_table,
#           tau_hat = tau_hat$predictions
#         )
#         varimp <- variable_importance(model)
#         ranked_vars <- order(varimp, decreasing = TRUE)
#         top_vars <- colnames(covariates)[ranked_vars[1:min(top_n_vars, length(ranked_vars))]]
#         results[[model_name]]$top_vars <- top_vars
#         results[[model_name]]$variable_importance <- data.frame(
#           variable = colnames(covariates),
#           importance = varimp
#         ) |> dplyr::arrange(dplyr::desc(importance))
#
#         dr_scores <- policytree::double_robust_scores(model)
#         results[[model_name]]$dr_scores <- dr_scores
#
#         n_non_missing <- length(not_missing)
#         train_size <- floor(train_proportion * n_non_missing)
#         train_indices <- sample(not_missing, train_size)
#
#         X_train <- covariates[train_indices, top_vars, drop = FALSE]
#         dr_scores_train <- dr_scores[train_indices, , drop = FALSE]
#
#         policy_tree_model <- tryCatch({
#           policytree::policy_tree(X_train, dr_scores_train, depth = 2)
#         }, error = function(e) {
#           cli::cli_alert_warning(crayon::yellow(paste("Error in policy tree for", outcome, ":", e$message)))
#           NULL
#         })
#
#         results[[model_name]]$policy_tree_depth_2 <- policy_tree_model
#
#         if (!is.null(policy_tree_model)) {
#           test_indices <- setdiff(not_missing, train_indices)
#           X_test <- covariates[test_indices, top_vars, drop = FALSE]
#           predictions <- predict(policy_tree_model, X_test)
#
#           results[[model_name]]$plot_data <- list(
#             X_test = X_test,
#             predictions = predictions
#           )
#         } else {
#           results[[model_name]]$plot_data <- NULL
#         }
#
#         if (compute_qini) {
#           # Compute Qini curves
#           cli::cli_alert_info(paste("Computing Qini curves for", outcome))
#           qini_data <- compute_qini_curves_multi_arm(tau_hat = tau_hat$predictions,
#                                                      Y = Y,
#                                                      W_multi = W_multi)
#
#           if (is.null(qini_data)) {
#             cli::cli_alert_warning(paste("Qini data is NULL for", outcome))
#           } else if (attr(qini_data, "imputed")) {
#             cli::cli_alert_warning(paste("Qini data for", outcome, "was imputed with zeros. Exercise caution when interpreting results."))
#             results[[model_name]]$qini_data <- qini_data
#             results[[model_name]]$qini_imputed <- TRUE
#           } else {
#             results[[model_name]]$qini_data <- qini_data
#             results[[model_name]]$qini_imputed <- FALSE
#           }
#         }
#
#         if (save_models) {
#           full_models[[model_name]] <- model
#         }
#
#       }, error = function(e) {
#         cli::cli_alert_danger(crayon::red(paste("Error in model for", outcome, ":", e$message)))
#         results[[model_name]] <- list(error = e$message)
#       })
#
#       cli::cli_progress_update()
#     }
#
#     cli::cli_progress_done()
#     combined_tables <- group_results_by_comparison(results)
#     list(results = results, full_models = full_models, combined_tables = combined_tables)
#   }
#
#   model_results <- run_models_with_progress()
#
#   cli::cli_alert_success(crayon::green("Model runs completed successfully"))
#
#   output <- list(
#     results = model_results$results,
#     combined_tables = model_results$combined_tables,
#     outcome_vars = outcome_vars,
#     not_missing = not_missing,
#     exposure_name = exposure_name
#   )
#
#   if (save_data) {
#     output$data <- data
#     output$covariates <- covariates
#     output$weights <- weights
#     cli::cli_alert_info("Data, covariates, and weights saved in output")
#   }
#
#   if (save_models) {
#     output$full_models <- model_results$full_models
#     cli::cli_alert_info("Full model objects saved in output")
#   }
#
#   cli::cli_alert_success(crayon::bold(crayon::green("margot_multi_arm_causal_forest function completed successfully \U0001F44D")))
#
#   return(output)
# }
