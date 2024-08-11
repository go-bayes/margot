#' Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Features
#'
#' This function is a wrapper for grf::causal_forest that runs multiple GRF causal forest models
#' for specified outcome variables. It calculates average treatment effects, tests calibration,
#' creates custom evaluation tables, and includes additional features such as tau.hat estimates,
#' RATE calculations, policy trees, variable importance rankings, best linear projections,
#' and Qini curves.
#' It also prepares data for policy tree visualization, using a specified proportion of the data for training.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A vector of binary treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 10.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is FALSE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for training policy trees. Default is 0.8.
#'
#' @return A list containing:
#'   \item{results}{A list of model results, one for each outcome variable. Each model result includes:
#'     \itemize{
#'       \item{ate}{Average treatment effect}
#'       \item{test_calibration}{Calibration test results}
#'       \item{custom_table}{Custom evaluation table}
#'       \item{tau_hat}{Individual treatment effect estimates}
#'       \item{rate_result}{Rank Average Treatment Effect (if compute_rate is TRUE)}
#'       \item{dr_scores}{Double robust scores}
#'       \item{policy_tree_depth_1}{Policy tree of depth 1}
#'       \item{top_vars}{Top variables by importance}
#'       \item{blp_top}{Best linear projection results for top variables}
#'       \item{policy_tree_depth_2}{Policy tree of depth 2, trained on train_proportion of non-missing data}
#'       \item{plot_data}{Data prepared for policy tree visualization, using the remaining proportion of non-missing data}
#'       \item{qini_data}{Data frame containing Qini curve data for plotting}
#'     }
#'   }
#'   \item{combined_table}{A data frame combining all custom evaluation tables.}
#'   \item{outcome_vars}{The character vector of outcome variable names that were modeled.}
#'   \item{not_missing}{A vector of indices for complete cases.}
#'   \item{data}{The input data (if save_data is TRUE).}
#'   \item{covariates}{The input covariates (if save_data is TRUE).}
#'   \item{weights}{The input weights (if save_data is TRUE).}
#'   \item{full_models}{A list of full GRF model objects (if save_models is TRUE).}
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect variable_importance best_linear_projection
#' @importFrom dplyr %>%
#' @importFrom progressr progressor with_progress
#' @importFrom margot margot_model_evalue
#' @importFrom ggplot2 ggplot geom_histogram theme_minimal labs
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom maq get_ipw_scores maq
#'
#' @export
margot_causal_forest <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list(),
                                 save_data = FALSE, compute_rate = TRUE, top_n_vars = 10, save_models = FALSE,
                                 train_proportion = 0.8) {

  # warning about large object size when save_models is TRUE
  if (save_models) {
    warning("Note: setting save_models = TRUE typically results in very large objects (often several GB). ",
            "Ensure you have sufficient memory available.",
            call. = FALSE)
  }
  # Create not_missing vector (only needs to be done once)
  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[which(full %in% not_missing)]

  run_models_with_progress <- function() {
    results <- list()
    full_models <- list()  # To store full models if save_models is TRUE

    p <- progressor(along = outcome_vars)
    for (outcome in outcome_vars) {
      model_name <- paste0("model_", outcome)
      Y <- as.matrix(data[[outcome]])
      model <- do.call(grf::causal_forest, c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))

      # Always save these results
      results[[model_name]] <- list(
        ate = round(grf::average_treatment_effect(model), 3),
        test_calibration = round(grf::test_calibration(model), 3),
        custom_table = margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL),
        tau_hat = predict(model)$predictions
      )

      tau_hat <- results[[model_name]]$tau_hat

      if (compute_rate) {
        results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
      }

      results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(covariates[full, ], results[[model_name]]$dr_scores[full, ], depth = 1)

      varimp <- grf::variable_importance(model)
      ranked_vars <- order(varimp, decreasing = TRUE)
      top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
      results[[model_name]]$top_vars <- top_vars

      results[[model_name]]$blp_top <- grf::best_linear_projection(model, covariates[, top_vars], target.sample = "all")

      # Train policy tree on train_proportion of non-missing data
      n_non_missing <- length(not_missing)
      train_size <- floor(train_proportion * n_non_missing)
      train_indices <- sample(not_missing, train_size)

      policy_tree_model <- policytree::policy_tree(
        covariates[train_indices, top_vars], results[[model_name]]$dr_scores[train_indices, ], depth = 2
      )
      results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

      # Extract split variable names # wrong, not used
      #split_vars <- sapply(1:3, function(i) colnames(covariates)[policy_tree_model_2$nodes[[i]]$split_variable])
      #results[[model_name]]$split_variables <- split_vars

      # Prepare data for plotting
      test_indices <- setdiff(not_missing, train_indices)
      X_test <- covariates[test_indices, top_vars]
      predictions <- predict(policy_tree_model, X_test)

      results[[model_name]]$plot_data <- list(
        X_test = X_test,
        predictions = predictions#, not used
      #  split_variables = split_vars
      )

      # Compute qini_data
      results[[model_name]]$qini_data <- tryCatch({
        compute_qini_curves(tau_hat, Y, W = W)
      }, error = function(e) {
        warning(paste("Error computing Qini curves for", outcome, ":", e$message))
        NULL
      })

      # Optionally save the full model
      if (save_models) {
        full_models[[model_name]] <- model
      }

      p(sprintf("Completed %s", outcome))
    }

    list(results = results, full_models = full_models)
  }

  model_results <- with_progress(run_models_with_progress())

  # combine tables from all models
  combined_table <- do.call(rbind, lapply(model_results$results, function(x) x$custom_table)) # hack
  rownames(combined_table) <- gsub("model_", "", rownames(combined_table))


  output <- list(
    results = model_results$results,
    combined_table = combined_table,
    outcome_vars = outcome_vars,
    not_missing = not_missing
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
  }

  if (save_models) {
    output$full_models <- model_results$full_models
  }

  return(output)
}
