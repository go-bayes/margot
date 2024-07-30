#' Multi-Arm Causal Forest Analysis
#'
#' @description
#' Performs multi-arm causal forest analysis on multiple outcomes, including
#' average treatment effects, variable importance, policy learning, and
#' optionally Qini curves.
#'
#' @param data A data frame containing all variables.
#' @param outcome_vars A character vector of outcome variable names.
#' @param covariates A matrix or data frame of covariates.
#' @param W_multi A matrix of treatment assignments.
#' @param weights A numeric vector of weights for each observation.
#' @param exposure_name A character string naming the exposure variable.
#' @param grf_defaults A list of default parameters for grf::multi_arm_causal_forest.
#' @param save_data Logical; if TRUE, saves input data in the output.
#' @param top_n_vars Integer; number of top variables to consider for importance.
#' @param save_models Logical; if TRUE, saves full model objects.
#' @param compute_qini Logical; if TRUE, computes Qini curves.
#'
#' @return A list containing:
#'   \item{results}{A list of results for each outcome.}
#'   \item{combined_tables}{Grouped results by comparison levels.}
#'   \item{outcome_vars}{Names of outcome variables analyzed.}
#'   \item{not_missing}{Indices of non-missing observations.}
#'   \item{exposure_name}{Name of the exposure variable.}
#'   \item{data}{Input data (if save_data is TRUE).}
#'   \item{covariates}{Input covariates (if save_data is TRUE).}
#'   \item{weights}{Input weights (if save_data is TRUE).}
#'   \item{full_models}{Full model objects (if save_models is TRUE).}
#'
#' @importFrom ggplot2 ggplot geom_histogram theme_minimal labs facet_wrap
#' @importFrom dplyr arrange desc
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map2_dfr
#' @importFrom grf multi_arm_causal_forest average_treatment_effect variable_importance
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom progressr progressor with_progress
#' @importFrom maq get_ipw_scores maq
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#' result <- margot_multi_arm_causal_forest(
#'   data = my_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   covariates = my_covariates,
#'   W_multi = treatment_matrix,
#'   weights = observation_weights,
#'   exposure_name = "treatment"
#' )
#' }
#'
#' @export
margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
                                           exposure_name, grf_defaults = list(),
                                           save_data = FALSE, top_n_vars = 10,
                                           save_models = FALSE, compute_qini = TRUE) {
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
      ) %>% arrange(desc(importance))

      # policy learning
      dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$policy_tree_depth_2 <- policytree::policy_tree(
        covariates[full, top_vars], dr_scores[full, ], depth = 2
      )

      # compute qini curves if requested
      if (compute_qini) {
        tryCatch({
          results[[model_name]]$qini_data <- compute_qini_curves(tau_hat, Y, W_multi)
        }, error = function(e) {
          warning("Error in computing Qini curves for ", outcome, ": ", e$message)
          results[[model_name]]$qini_data <- NULL
        })
      }

      p(sprintf("Completed %s", outcome))
    }

    # group results by comparison levels
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
