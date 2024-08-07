#' Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Features
#'
#' This function is a wrapper for grf::causal_forest that runs multiple GRF causal forest models
#' for specified outcome variables. It calculates average treatment effects, tests calibration,
#' creates custom evaluation tables, and includes additional features such as tau.hat estimates,
#' RATE calculations, policy trees, variable importance rankings, and best linear projections.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A matrix of treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 10.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is FALSE.
#'
#' @return A list containing:
#'   \item{results}{A list of model results, one for each outcome variable.}
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
#'
#' @export
margot_grf_causal_forest <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list(),
                                     save_data = FALSE, compute_rate = TRUE, top_n_vars = 10, save_models = FALSE) {
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

      results[[model_name]]$tau_hat_plot <- ggplot2::ggplot(data.frame(tau_hat = tau_hat), ggplot2::aes(x = tau_hat)) +
        ggplot2::geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Distribution of tau.hat for", outcome),
                      x = "tau.hat", y = "Count")

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
      results[[model_name]]$policy_tree_depth_2 <- policytree::policy_tree(covariates[full, top_vars], results[[model_name]]$dr_scores[full, ], depth = 2)

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
  combined_table <- do.call(rbind, lapply(model_results$results, function(x) x$custom_table))

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
