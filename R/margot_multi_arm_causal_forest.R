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
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for training policy trees. Default is 0.8.
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
#'       \item{split_variables}{Names of variables used for splits in policy_tree_depth_2}
#'       \item{plot_data}{Data prepared for policy tree visualization, using the remaining proportion of non-missing data}
#'       \item{qini_data}{Data frame containing Qini curve data for plotting (if compute_qini is TRUE)}
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
#' @importFrom progressr progressor with_progress
#' @importFrom margot margot_model_evalue
#' @importFrom policytree double_robust_scores policy_tree
#'
#' @note Setting save_models = TRUE typically results in very large objects (often several GB).
#'       Ensure you have sufficient memory available when using this option.
#'
#' @examples
#' \dontrun{
#' result_multi_arm <- margot_multi_arm_causal_forest(
#'   data = df_grf,
#'   outcome_vars = outcomes,
#'   covariates = X,
#'   W_multi = W_multi,
#'   weights = weights,
#'   exposure_name = "treatment",
#'   grf_defaults = list(num.trees = 2000),
#'   top_n_vars = 15,
#'   train_proportion = 0.8,
#'   save_models = TRUE
#' )
#' }
#'
#' @export
margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
                                           exposure_name, grf_defaults = list(),
                                           save_data = FALSE, top_n_vars = 10,
                                           save_models = FALSE, compute_qini = TRUE,
                                           train_proportion = 0.8) {
  # warning
  if (save_models) {
    warning("Note: setting save_models = TRUE typically results in very large objects (often several GB). ",
            "Ensure you have sufficient memory available.",
            call. = FALSE)
  }

  # ensure W_multi is a factor
  if (!is.factor(W_multi)) {
    W_multi <- as.factor(W_multi)
  }

  # create not_missing vector
  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[which(full %in% not_missing)]

  run_models_with_progress <- function() {
    results <- list()
    full_models <- list()
    p <- progressor(along = outcome_vars)

    for (outcome in outcome_vars) {
      model_name <- paste0("model_", outcome)
      Y <- as.matrix(data[[outcome]])
      model <- do.call(grf::multi_arm_causal_forest,
                       c(list(X = covariates, Y = Y, W = W_multi, sample.weights = weights),
                         grf_defaults))

      # process results
      ate <- average_treatment_effect(model)
      custom_table <- margot::margot_model_evalue(model, new_name = outcome, subset = NULL)
      tau_hat <- predict(model, X = covariates, drop = TRUE)$predictions

      # store results
      results[[model_name]] <- list(
        ate = ate,
        custom_table = custom_table,
        tau_hat = tau_hat
      )

      # variable importance
      varimp <- variable_importance(model)
      ranked_vars <- order(varimp, decreasing = TRUE)
      top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
      results[[model_name]]$top_vars <- top_vars
      results[[model_name]]$variable_importance <- data.frame(
        variable = colnames(covariates),
        importance = varimp
      ) |> arrange(desc(importance))

      # policy learning
      dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$dr_scores <- dr_scores

      # train policy tree on 90% of non-missing data
      n_non_missing <- length(not_missing)
      train_size <- floor(train_proportion * n_non_missing)
      train_indices <- sample(not_missing, train_size)

      policy_tree_model <- policytree::policy_tree(
        covariates[train_indices, top_vars], dr_scores[train_indices, ], depth = 2
      )
      results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

      # Extract split variable names
      split_vars <- sapply(1:3, function(i) colnames(covariates)[policy_tree_model$nodes[[i]]$split_variable])
      results[[model_name]]$split_variables <- split_vars

      # Prepare data for plotting
      test_indices <- setdiff(not_missing, train_indices)
      X_test <- covariates[test_indices, top_vars]
      predictions <- predict(policy_tree_model, X_test)

      results[[model_name]]$plot_data <- list(
        X_test = X_test,
        predictions = predictions,
        split_variables = split_vars
      )

      # Compute qini curves if requested
      if (compute_qini) {
        tryCatch({
          results[[model_name]]$qini_data <- compute_qini_curves(tau_hat, Y, W_multi = W_multi)
        }, error = function(e) {
          warning("Error in computing Qini curves for ", outcome, ": ", e$message)
          results[[model_name]]$qini_data <- NULL
        })
      }

      p(sprintf("Completed %s", outcome))
    }

    # Group results by comparison levels
    combined_tables <- group_results_by_comparison(results)
    list(results = results, full_models = full_models, combined_tables = combined_tables)
  }

  model_results <- with_progress(run_models_with_progress())

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
  }

  if (save_models) {
    output$full_models <- model_results$full_models
  }

  return(output)
}
