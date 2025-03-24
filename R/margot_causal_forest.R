#' Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Features
#'
#' This function runs multiple GRF causal forest models with enhanced features. In addition to estimating
#' causal effects, it can compute the Rank-Weighted Average Treatment Effect (RATE) for each model. RATE
#' ranks individuals based on their predicted treatment effects and calculates the average effect for selected
#' percentiles. When computing RATE, the \code{target_rate} parameter determines the variant: \code{"AUTOC"}
#' (default) exhibits greater power when only a small subset of the population experiences nontrivial heterogeneous
#' treatment effects, whereas \code{"QINI"} is more powerful when many individuals experience diffuse or substantial
#' treatment effect heterogeneity.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modelled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A vector of binary treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#' @param target_rate Character string specifying the type of RATE estimate. Options are \code{"AUTOC"} (default)
#'   and \code{"QINI"}.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 15.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is TRUE
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for
#'   training policy trees. Default is 0.7.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A list containing model results, a combined table, and other relevant information.
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect variable_importance best_linear_projection
#' @importFrom dplyr %>%
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom crayon bold green red yellow
#'
#' @export
margot_causal_forest <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list(),
                                 save_data = FALSE, compute_rate = TRUE, top_n_vars = 15, save_models = TRUE,
                                 train_proportion = 0.7, verbose = TRUE) {

  if (verbose) cli::cli_alert_info(crayon::bold("Starting margot_causal_forest function"))

  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[which(full %in% not_missing)]

  if (verbose) cli::cli_alert_info(paste("Number of complete cases:", length(not_missing)))

  results <- list()
  full_models <- list()

  if (verbose) cli::cli_alert_info("Running models for each outcome variable")
  pb <- cli::cli_progress_bar(total = length(outcome_vars), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (outcome in outcome_vars) {
    model_name <- paste0("model_", outcome)
    Y <- as.matrix(data[[outcome]])

    tryCatch({
      model <- do.call(grf::causal_forest, c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))

      results[[model_name]] <- list(
        ate = round(grf::average_treatment_effect(model), 3),
        test_calibration = round(grf::test_calibration(model), 3),
        custom_table = margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL),
        tau_hat = predict(model)$predictions
      )

      tau_hat <- results[[model_name]]$tau_hat

      if (compute_rate) {
        results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
        results[[model_name]]$rate_qini   <- grf::rank_average_treatment_effect(model, tau_hat, target = "QINI")
      }

      results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(covariates[full, ], results[[model_name]]$dr_scores[full, ], depth = 1)

      varimp <- grf::variable_importance(model)
      ranked_vars <- order(varimp, decreasing = TRUE)
      top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
      results[[model_name]]$top_vars <- top_vars

      results[[model_name]]$blp_top <- grf::best_linear_projection(model, covariates[, top_vars], target.sample = "all")

      n_non_missing <- length(not_missing)
      train_size <- floor(train_proportion * n_non_missing)
      train_indices <- sample(not_missing, train_size)

      policy_tree_model <- policytree::policy_tree(
        covariates[train_indices, top_vars], results[[model_name]]$dr_scores[train_indices, ], depth = 2
      )
      results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

      test_indices <- setdiff(not_missing, train_indices)
      X_test <- covariates[test_indices, top_vars]
      predictions <- predict(policy_tree_model, X_test)

      results[[model_name]]$plot_data <- list(
        X_test = X_test,
        predictions = predictions
      )

      if (verbose) cli::cli_alert_info("Computing binary Qini curves")
      qini_result <- compute_qini_curves_binary(tau_hat, Y, W)
      if (!is.null(qini_result)) {
        results[[model_name]]$qini_data <- qini_result$qini_data
        results[[model_name]]$qini_objects <- qini_result$qini_objects
      } else {
        if (verbose) cli::cli_alert_warning(crayon::yellow(paste("Unable to compute binary Qini curves for", outcome)))
      }

      if (save_models) {
        full_models[[model_name]] <- model
      }

    }, error = function(e) {
      if (verbose) cli::cli_alert_danger(crayon::red(paste("Error in model for", outcome, ":", e$message)))
    })

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  combined_table <- do.call(rbind, lapply(results, function(x) x$custom_table))
  rownames(combined_table) <- gsub("model_", "", rownames(combined_table))

  if (verbose) cli::cli_alert_success(crayon::green("Model runs completed successfully"))

  output <- list(
    results = results,
    combined_table = combined_table,
    outcome_vars = outcome_vars,
    not_missing = not_missing
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
    if (verbose) cli::cli_alert_info("Data, covariates, and weights saved in output")
  }

  if (save_models) {
    output$full_models <- full_models
    if (verbose) cli::cli_alert_info("Full model objects saved in output")
  }

  if (verbose) cli::cli_alert_success(crayon::bold(crayon::green("margot_causal_forest function completed successfully \U0001F44D")))

  return(output)
}



#' Compute Qini Curves for Binary Treatments
#'
#' @description
#' Computes Qini curves for binary treatments using the maq package. This function
#' calculates both Conditional Average Treatment Effect (CATE) and Average Treatment
#' Effect (ATE) Qini curves.
#'
#' @param tau_hat Numeric vector of estimated treatment effects.
#' @param Y Vector or matrix of observed outcomes.
#' @param W Vector of treatment assignments (binary).
#' @param verbose Logical; if TRUE, print diagnostic information during execution.
#'
#' @return A list containing two elements:
#'   \item{qini_data}{A data frame containing Qini curve data for plotting.}
#'   \item{qini_objects}{A list of maq objects for CATE and ATE Qini curves.}
#'   Returns NULL if an error occurs or if the resulting Qini data is empty.
#'
#' @importFrom maq get_ipw_scores maq
#' @importFrom purrr map2_dfr
#' @importFrom cli cli_alert_info cli_alert_warning
#'
#' @details
#' This function computes Qini curves for binary treatments. It handles potential
#' errors and edge cases, providing verbose output when requested. The function
#' calculates both CATE and ATE Qini curves using the maq package.
#'
#' @keywords internal
compute_qini_curves_binary <- function(tau_hat, Y, W, verbose = TRUE) {
  tryCatch({
    if (verbose) {
      cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
      cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
      cli::cli_alert_info(paste("Y class:", class(Y)))
      cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
      cli::cli_alert_info(paste("W class:", class(W)))
      cli::cli_alert_info(paste("W length:", length(W)))
    }

    tau_hat <- as.vector(tau_hat)
    treatment <- as.factor(W)

    IPW_scores <- maq::get_ipw_scores(Y, treatment)
    cost <- 1

    if (verbose) {
      cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
      cli::cli_alert_info(paste("cost length:", length(cost)))
      cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))
    }

    # Calculate CATE Qini curve
    cate_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)

    # Calculate ATE Qini curve
    ate_qini <- maq::maq(rep(mean(tau_hat), length(tau_hat)), cost, IPW_scores, R = 200)

    qini_objects <- list(cate = cate_qini, ate = ate_qini)

    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
        return(0)
      }
      length(qini_obj[["_path"]]$gain)
    }))

    if (max_index == 0) {
      if (verbose) cli::cli_alert_warning("All Qini objects have empty gain. Returning NULL.")
      return(NULL)
    }

    qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data_binary(.x, .y, max_index))

    if (nrow(qini_data) == 0) {
      if (verbose) cli::cli_alert_warning("Extracted Qini data is empty. Returning NULL.")
      return(NULL)
    }

    return(list(qini_data = qini_data, qini_objects = qini_objects))
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning(paste("Error in compute_qini_curves_binary:", e$message))
    return(NULL)
  })
}




#' Extract Qini Data for Binary Treatment Plotting
#'
#' @description
#' Extracts Qini curve data from a Qini object for binary treatments and prepares it for plotting.
#'
#' @param qini_obj A Qini object.
#' @param name Name of the curve type (either "ate" or "cate").
#' @param max_index Maximum index to extend the curve to.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A data frame with extracted Qini data.
#'
#' @keywords internal
extract_qini_data_binary <- function(qini_obj, name, max_index, verbose = TRUE) {
  if (name == "ate") {
    # For ATE, create a straight line from (0,0) to (1, max_gain)
    max_gain <- max(qini_obj[["_path"]]$gain, na.rm = TRUE)
    proportion <- seq(0, 1, length.out = max_index)
    gain <- proportion * max_gain
  } else if (name == "cate") {
    # For CATE, use the actual gain values
    gain <- qini_obj[["_path"]]$gain
    gain_length <- length(gain)

    if (gain_length < max_index) {
      # Extend gain vector
      gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
    } else if (gain_length > max_index) {
      # Truncate gain vector
      gain <- gain[1:max_index]
    }

    proportion <- seq_len(max_index) / max_index
  } else {
    if (verbose) cli::cli_alert_warning(paste("Unknown curve type:", name))
    return(NULL)
  }

  data.frame(
    proportion = proportion,
    gain = gain,
    curve = name
  )
}



#' OLD
#' margot_causal_forest <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list(),
#'                                  save_data = FALSE, compute_rate = TRUE, top_n_vars = 15, save_models = TRUE,
#'                                  train_proportion = 0.7, verbose = TRUE) {
#'
#'   if (verbose) cli::cli_alert_info(crayon::bold("starting margot_causal_forest function"))
#'
#'   not_missing <- which(complete.cases(covariates))
#'   full <- seq_len(nrow(covariates))
#'   full <- full[full %in% not_missing]
#'
#'   if (verbose) cli::cli_alert_info(paste("number of complete cases:", length(not_missing)))
#'
#'   results <- list()
#'   full_models <- list()
#'
#'   if (verbose) cli::cli_alert_info("running models for each outcome variable")
#'   pb <- cli::cli_progress_bar(total = length(outcome_vars), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
#'
#'   for (outcome in outcome_vars) {
#'     model_name <- paste0("model_", outcome)
#'     Y <- as.matrix(data[[outcome]])
#'
#'     tryCatch({
#'       model <- do.call(grf::causal_forest, c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))
#'
#'       results[[model_name]] <- list(
#'         ate = round(grf::average_treatment_effect(model), 3),
#'         test_calibration = round(grf::test_calibration(model), 3),
#'         custom_table = margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL),
#'         tau_hat = predict(model)$predictions
#'       )
#'
#'       tau_hat <- results[[model_name]]$tau_hat
#'
#'       if (compute_rate) {
#'         results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
#'         results[[model_name]]$rate_qini   <- grf::rank_average_treatment_effect(model, tau_hat, target = "QINI")
#'       }
#'
#'       results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)
#'       results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(covariates[full, ],
#'                                                                            results[[model_name]]$dr_scores[full, ],
#'                                                                            depth = 1)
#'
#'       varimp <- grf::variable_importance(model)
#'       ranked_vars <- order(varimp, decreasing = TRUE)
#'       top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
#'       results[[model_name]]$top_vars <- top_vars
#'
#'       results[[model_name]]$blp_top <- grf::best_linear_projection(model, covariates[, top_vars], target.sample = "all")
#'
#'       n_non_missing <- length(not_missing)
#'       train_size <- floor(train_proportion * n_non_missing)
#'       train_indices <- sample(not_missing, train_size)
#'
#'       policy_tree_model <- policytree::policy_tree(
#'         covariates[train_indices, top_vars], results[[model_name]]$dr_scores[train_indices, ], depth = 2
#'       )
#'       results[[model_name]]$policy_tree_depth_2 <- policy_tree_model
#'
#'       test_indices <- setdiff(not_missing, train_indices)
#'       X_test <- covariates[test_indices, top_vars]
#'       predictions <- predict(policy_tree_model, X_test)
#'
#'       results[[model_name]]$plot_data <- list(
#'         X_test = X_test,
#'         predictions = predictions
#'       )
#'
#'       if (verbose) cli::cli_alert_info("computing binary qini curves using doubly robust scores")
#'       qini_result <- compute_qini_curves_binary(model, tau_hat, Y, W, verbose = verbose)
#'       if (!is.null(qini_result)) {
#'         results[[model_name]]$qini_data <- qini_result$qini_data
#'         results[[model_name]]$qini_objects <- qini_result$qini_objects
#'
#'         # Verify the objects were created properly
#'         qini_data_valid <- !is.null(results[[model_name]]$qini_data) &&
#'           is.data.frame(results[[model_name]]$qini_data) &&
#'           nrow(results[[model_name]]$qini_data) > 0
#'
#'         qini_objects_valid <- !is.null(results[[model_name]]$qini_objects) &&
#'           is.list(results[[model_name]]$qini_objects) &&
#'           length(results[[model_name]]$qini_objects) > 0
#'
#'         if (qini_data_valid && qini_objects_valid) {
#'           if (verbose) cli::cli_alert_success(paste("generated qini_data and qini_objects for", outcome))
#'         } else {
#'           if (verbose) {
#'             cli::cli_alert_warning(crayon::yellow(paste("qini objects created but validation failed for", outcome)))
#'             if (!qini_data_valid) cli::cli_alert_info("qini_data validation failed")
#'             if (!qini_objects_valid) cli::cli_alert_info("qini_objects validation failed")
#'           }
#'           # Try to create minimal valid objects if the validation failed
#'           if (!qini_data_valid) {
#'             results[[model_name]]$qini_data <- data.frame(
#'               proportion = seq(0, 1, length.out = 100),
#'               gain = seq(0, mean(tau_hat) * 10, length.out = 100),
#'               curve = rep(c("cate", "ate"), each = 50)
#'             )
#'           }
#'           if (!qini_objects_valid) {
#'             results[[model_name]]$qini_objects <- list(
#'               cate = list("_path" = list(gain = cumsum(sort(tau_hat, decreasing = TRUE))/length(tau_hat))),
#'               ate = list("_path" = list(gain = cumsum(rep(mean(tau_hat), length(tau_hat)))/length(tau_hat)))
#'             )
#'           }
#'           if (verbose) cli::cli_alert_success(paste("created fallback qini objects for", outcome))
#'         }
#'       } else {
#'         if (verbose) cli::cli_alert_warning(crayon::yellow(paste("unable to compute binary qini curves for", outcome)))
#'         # Create minimal valid objects even when computation fails
#'         results[[model_name]]$qini_data <- data.frame(
#'           proportion = seq(0, 1, length.out = 100),
#'           gain = seq(0, mean(tau_hat) * 10, length.out = 100),
#'           curve = rep(c("cate", "ate"), each = 50)
#'         )
#'         results[[model_name]]$qini_objects <- list(
#'           cate = list("_path" = list(gain = cumsum(sort(tau_hat, decreasing = TRUE))/length(tau_hat))),
#'           ate = list("_path" = list(gain = cumsum(rep(mean(tau_hat), length(tau_hat)))/length(tau_hat)))
#'         )
#'         if (verbose) cli::cli_alert_success(paste("created fallback qini objects for", outcome))
#'       }
#'
#'       # save the model if requested - this was missing in the revised version
#'       if (save_models) {
#'         full_models[[model_name]] <- model
#'       }
#'
#'       cli::cli_progress_update()
#'     }, error = function(e) {
#'       if (verbose) cli::cli_alert_danger(crayon::red(paste("error in model for", outcome, ":", e$message)))
#'     })
#'   }
#'
#'   cli::cli_progress_done()
#'
#'   combined_table <- do.call(rbind, lapply(results, function(x) x$custom_table))
#'   rownames(combined_table) <- gsub("model_", "", rownames(combined_table))
#'
#'   if (verbose) cli::cli_alert_success(crayon::green("model runs completed successfully"))
#'
#'   output <- list(
#'     results = results,
#'     combined_table = combined_table,
#'     outcome_vars = outcome_vars,
#'     not_missing = not_missing
#'   )
#'
#'   if (save_data) {
#'     output$data <- data
#'     output$covariates <- covariates
#'     output$weights <- weights
#'     if (verbose) cli::cli_alert_info("data, covariates, and weights saved in output")
#'   }
#'
#'   if (save_models) {
#'     output$full_models <- full_models
#'     if (verbose) cli::cli_alert_info("full model objects saved in output")
#'   }
#'
#'   # verify that qini objects and data were created for each model
#'   missing_qini <- character(0)
#'   for (model_name in names(results)) {
#'     if (is.null(results[[model_name]]$qini_objects) || is.null(results[[model_name]]$qini_data)) {
#'       missing_qini <- c(missing_qini, model_name)
#'     }
#'   }
#'
#'   if (length(missing_qini) > 0) {
#'     if (verbose) cli::cli_alert_warning(crayon::yellow(paste("missing qini objects for models:", paste(missing_qini, collapse=", "))))
#'   } else {
#'     if (verbose) cli::cli_alert_success(crayon::green("all models have qini_objects and qini_data"))
#'   }
#'
#'   if (verbose) cli::cli_alert_success(crayon::bold(crayon::green("margot_causal_forest function completed successfully \U0001F44D")))
#'
#'   return(output)
#' }
#' #' Compute Qini Curves for Binary Treatments Using Doubly Robust Scores
#' #'
#' #' @description
#' #' Computes Qini curves for binary treatments using the maq package and doubly robust (DR) scores from GRF.
#' #' This function calculates both Conditional Average Treatment Effect (CATE) and Average Treatment
#' #' Effect (ATE) Qini curves. The function includes robust error handling and multiple fallback mechanisms
#' #' to ensure valid output even when the primary calculation methods fail.
#' #'
#' #' @param model A fitted GRF causal forest model.
#' #' @param tau_hat Numeric vector of estimated treatment effects.
#' #' @param Y Vector or matrix of observed outcomes.
#' #' @param W Vector of treatment assignments (binary).
#' #' @param verbose Logical; if TRUE, print diagnostic information during execution.
#' #'
#' #' @return A list containing two elements:
#' #'   \item{qini_data}{A data frame containing Qini curve data for plotting.}
#' #'   \item{qini_objects}{A list of maq objects for CATE and ATE Qini curves.}
#' #'   Returns placeholder objects if calculations fail.
#' #'
#' #' @details
#' #' The function first attempts to compute doubly robust scores from the model. If this fails,
#' #' it falls back to several alternative methods, progressively simplifying the approach
#' #' until valid output structures are produced. This ensures that downstream functions
#' #' have the expected data structures available.
#' #'
#' #' @importFrom maq maq get_ipw_scores
#' #' @importFrom purrr map2_dfr
#' #' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' #'
#' #' @keywords internal
#' compute_qini_curves_binary <- function(model, tau_hat, Y, W, verbose = TRUE) {
#'   tryCatch({
#'     if (verbose) {
#'       cli::cli_alert_info("using doubly robust scores for qini estimation")
#'       cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
#'       cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
#'       cli::cli_alert_info(paste("y class:", class(Y)))
#'       cli::cli_alert_info(paste("y dimensions:", paste(dim(Y), collapse = "x")))
#'       cli::cli_alert_info(paste("w class:", class(W)))
#'       cli::cli_alert_info(paste("w length:", length(W)))
#'     }
#'
#'     tau_hat <- as.vector(tau_hat)
#'
#'     # ensure Y is in the correct format
#'     if (is.matrix(Y) || is.array(Y)) {
#'       if (ncol(Y) == 1) {
#'         Y <- as.vector(Y)
#'       }
#'     }
#'
#'     # extract dr scores from the model
#'     dr_scores <- tryCatch({
#'       grf::get_scores(model)
#'     }, error = function(e) {
#'       if (verbose) cli::cli_alert_warning(paste("couldn't get scores from model:", e$message))
#'       # fall back to computing scores using policytree
#'       return(policytree::double_robust_scores(model))
#'     })
#'
#'     if (verbose) {
#'       cli::cli_alert_info(paste("dr_scores class:", class(dr_scores)))
#'       cli::cli_alert_info(paste("dr_scores dimensions:", paste(dim(dr_scores), collapse = "x")))
#'     }
#'
#'     # handle different possible structures of dr_scores
#'     if (is.matrix(dr_scores)) {
#'       if (ncol(dr_scores) == 2) {
#'         # Already in the right format
#'         DR_matrix <- dr_scores
#'       } else {
#'         # attempt to get column names robustly
#'         cols <- colnames(dr_scores)
#'         if (is.null(cols)) {
#'           dims <- attr(dr_scores, "dimnames")
#'           if (!is.null(dims) && length(dims) >= 2 && !is.null(dims[[2]])) {
#'             cols <- dims[[2]]
#'           }
#'         }
#'         if (is.null(cols)) {
#'           cols <- paste0("V", 1:ncol(dr_scores))
#'         }
#'
#'         if (verbose) {
#'           cli::cli_alert_info(paste("column names:", paste(cols, collapse = ", ")))
#'         }
#'
#'         if ("dr.y.hat.0" %in% cols && "dr.y.hat.1" %in% cols) {
#'           DR_matrix <- cbind(dr_scores[, "dr.y.hat.0"], dr_scores[, "dr.y.hat.1"])
#'         } else if ("control" %in% cols && "treated" %in% cols) {
#'           DR_matrix <- cbind(dr_scores[, "control"], dr_scores[, "treated"])
#'         } else if (ncol(dr_scores) >= 2) {
#'           # Just use the first two columns
#'           DR_matrix <- dr_scores[, 1:2]
#'           if (verbose) cli::cli_alert_warning("using first two columns of dr_scores")
#'         } else {
#'           # Fall back to IPW scores as a last resort
#'           if (verbose) cli::cli_alert_warning("falling back to IPW scores")
#'           treatment <- as.factor(W)
#'           DR_matrix <- maq::get_ipw_scores(Y, treatment)
#'         }
#'       }
#'     } else {
#'       # If dr_scores is not a matrix, fallback to IPW scores
#'       if (verbose) cli::cli_alert_warning("dr_scores is not a matrix, falling back to IPW scores")
#'       treatment <- as.factor(W)
#'       DR_matrix <- maq::get_ipw_scores(Y, treatment)
#'     }
#'
#'     cost <- 1
#'
#'     if (verbose) {
#'       cli::cli_alert_info(paste("dr matrix dimensions:", paste(dim(DR_matrix), collapse = "x")))
#'     }
#'
#'     # calculate qini curves using maq::maq() with DR_matrix
#'     cate_qini <- tryCatch({
#'       maq::maq(tau_hat, cost, DR_matrix, R = 200)
#'     }, error = function(e) {
#'       if (verbose) cli::cli_alert_warning(paste("error in maq calculation for cate:", e$message))
#'       # create a placeholder object with basic structure
#'       list("_path" = list(gain = cumsum(sort(tau_hat, decreasing = TRUE))/length(tau_hat)))
#'     })
#'
#'     ate_qini <- tryCatch({
#'       maq::maq(rep(mean(tau_hat), length(tau_hat)), cost, DR_matrix, R = 200)
#'     }, error = function(e) {
#'       if (verbose) cli::cli_alert_warning(paste("error in maq calculation for ate:", e$message))
#'       # create a placeholder object with basic structure
#'       mean_tau <- mean(tau_hat)
#'       list("_path" = list(gain = cumsum(rep(mean_tau, length(tau_hat)))/length(tau_hat)))
#'     })
#'
#'     qini_objects <- list(cate = cate_qini, ate = ate_qini)
#'
#'     max_index <- max(sapply(qini_objects, function(qini_obj) {
#'       if (!is.null(qini_obj[["_path"]]) && !is.null(qini_obj[["_path"]]$gain)) {
#'         length(qini_obj[["_path"]]$gain)
#'       } else if (is.numeric(qini_obj)) {
#'         length(qini_obj)
#'       } else {
#'         return(0)
#'       }
#'     }))
#'
#'     if (max_index == 0) {
#'       if (verbose) cli::cli_alert_warning("all qini objects have empty gain, returning null")
#'       return(NULL)
#'     }
#'
#'     # use the extraction function to create a data frame
#'     qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects),
#'                                  ~ extract_qini_data(.x, .y, max_index, verbose = verbose))
#'
#'     if (nrow(qini_data) == 0) {
#'       if (verbose) cli::cli_alert_warning("extracted qini data is empty, returning null")
#'       return(NULL)
#'     }
#'
#'     if (verbose) {
#'       cli::cli_alert_success("successfully created qini_objects and qini_data")
#'       cli::cli_alert_info(paste("qini_data dimensions:", nrow(qini_data), "x", ncol(qini_data)))
#'       cli::cli_alert_info(paste("number of qini_objects:", length(qini_objects)))
#'     }
#'
#'     return(list(qini_data = qini_data, qini_objects = qini_objects))
#'   }, error = function(e) {
#'     if (verbose) cli::cli_alert_warning(paste("error in compute_qini_curves_binary:", e$message))
#'     return(NULL)
#'   })
#' }
#'
#' #' Extract Qini Data for Plotting
#' #'
#' #' @description
#' #' Extracts Qini curve data from a Qini object and prepares it for plotting.
#' #' This function handles cases where Qini objects may be NULL or missing
#' #' required components, extending data as needed.
#' #'
#' #' @param qini_obj A Qini object.
#' #' @param name Name of the treatment arm.
#' #' @param max_index Maximum index to extend the curve to.
#' #' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#' #'
#' #' @return A data frame with extracted Qini data containing:
#' #'   \item{proportion}{Proportion of population ranked by predicted treatment effect}
#' #'   \item{gain}{Gain at each proportion}
#' #'   \item{curve}{Name of the curve (e.g., "cate" or "ate")}
#' #'
#' #' @keywords internal
#' extract_qini_data <- function(qini_obj, name, max_index, verbose = TRUE) {
#'   gain <- if (!is.null(qini_obj) && !is.null(qini_obj[["_path"]]) && !is.null(qini_obj[["_path"]]$gain)) {
#'     qini_obj[["_path"]]$gain
#'   } else {
#'     if (verbose) cli::cli_alert_warning(paste("qini object", name, "is null or missing required components, extending with zeros"))
#'     rep(0, max_index)
#'   }
#'
#'   gain_length <- length(gain)
#'   if (gain_length < max_index) {
#'     gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
#'   } else if (gain_length > max_index) {
#'     gain <- gain[1:max_index]
#'   }
#'
#'   proportion <- seq_len(max_index) / max_index
#'
#'   data.frame(
#'     proportion = proportion,
#'     gain = gain,
#'     curve = name
#'   )
#' }
