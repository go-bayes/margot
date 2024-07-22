#' Run Multiple Generalized Random Forest (GRF) Models with Enhanced Features
#'
#' This function runs multiple GRF models for specified outcome variables,
#' calculates average treatment effects, tests calibration, creates custom
#' evaluation tables, and now includes additional features such as tau.hat estimates,
#' RATE calculations, and policy trees.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A matrix of treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#'
#' @return A list containing:
#'   \item{results}{A list of model results, one for each outcome variable.}
#'   \item{combined_table}{A data frame combining all custom evaluation tables.}
#'   \item{outcome_vars}{The character vector of outcome variable names that were modeled.}
#'   \item{tau_hats}{A list of tau.hat estimates for each model.}
#'   \item{tau_hat_plots}{A list of ggplot objects for tau.hat histograms.}
#'   \item{rate_results}{A list of RATE results (if compute_rate is TRUE).}
#'   \item{dr_scores}{A list of double robust scores for each model.}
#'   \item{policy_trees}{A list of policy trees of depth 1 for each model.}
#'   \item{not_missing}{A vector of indices for complete cases.}
#'   \item{data}{The input data (if save_data is TRUE).}
#'   \item{covariates}{The input covariates (if save_data is TRUE).}
#'   \item{weights}{The input weights (if save_data is TRUE).}
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect
#' @importFrom dplyr %>%
#' @importFrom progressr progressor with_progress
#' @importFrom margot margot_model_evalue
#' @importFrom ggplot2 ggplot geom_histogram theme_minimal labs
#' @importFrom policytree double_robust_scores policy_tree
#'
#' @export
margot_run_models_grf <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list(),
                                  save_data = FALSE, compute_rate = TRUE) {
  # Create not_missing vector (only needs to be done once)
  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[which(full %in% not_missing)]

  run_models_with_progress <- function() {
    results <- list()
    tables <- list()
    tau_hats <- list()
    tau_hat_plots <- list()
    rate_results <- list()
    dr_scores_list <- list()
    policy_trees <- list()

    p <- progressor(along = outcome_vars)
    for (outcome in outcome_vars) {
      model_name <- paste0("model_", outcome)
      Y <- as.matrix(data[[outcome]])
      model <- do.call(grf::causal_forest, c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))
      ate <- round(grf::average_treatment_effect(model), 3)
      test_calibration_measure <- round(grf::test_calibration(model), 3)
      custom_table <- margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL)

      tau_hat <- predict(model)$predictions

      tau_hat_plot <- ggplot2::ggplot(data.frame(tau_hat = tau_hat), ggplot2::aes(x = tau_hat)) +
        ggplot2::geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Distribution of tau.hat for", outcome),
                      x = "tau.hat", y = "Count")

      rate_result <- NULL
      if (compute_rate) {
        rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
      }

      dr_scores <- policytree::double_robust_scores(model)
      policy_tree <- policytree::policy_tree(covariates[full, ], dr_scores[full, ], depth = 1)

      results[[model_name]] <- list(
        model = model,
        ate = ate,
        test_calibration = test_calibration_measure,
        custom_table = custom_table
      )

      tables[[outcome]] <- custom_table
      tau_hats[[outcome]] <- tau_hat
      tau_hat_plots[[outcome]] <- tau_hat_plot
      if (compute_rate) rate_results[[outcome]] <- rate_result
      dr_scores_list[[outcome]] <- dr_scores
      policy_trees[[outcome]] <- policy_tree

      p(sprintf("Completed %s", outcome))
    }

    list(results = results, tables = tables, tau_hats = tau_hats, tau_hat_plots = tau_hat_plots,
         rate_results = rate_results, dr_scores_list = dr_scores_list, policy_trees = policy_trees)
  }

  model_results <- with_progress(run_models_with_progress())

  combined_table <- do.call(rbind, model_results$tables)

  output <- list(
    results = model_results$results,
    combined_table = combined_table,
    outcome_vars = outcome_vars,
    tau_hats = model_results$tau_hats,
    tau_hat_plots = model_results$tau_hat_plots,
    rate_results = model_results$rate_results,
    dr_scores = model_results$dr_scores_list,
    policy_trees = model_results$policy_trees,
    not_missing = not_missing
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
  }

  return(output)
}
