#' Plot Policy Tree Results from Multi-Arm Causal Forest Analysis
#'
#' @description
#' This function creates a visualization of policy tree results from a multi-arm causal forest model.
#' It generates two side-by-side plots showing the relationships between the top three split variables,
#' with points colored by the predicted optimal treatment. The function uses jittered points to reduce
#' overplotting and improve visibility of data distribution.
#'
#' @param mc_test A list containing the results from margot_multi_arm_causal_forest().
#' @param model_name A string specifying which model's results to plot (e.g., "model_t2_belong_z").
#' @param color_scale A ggplot2 color scale. Default is ggokabeito::scale_colour_okabe_ito().
#' @param point_alpha Numeric value between 0 and 1 for point transparency. Default is 0.6.
#' @param theme_function A ggplot2 theme function. Default is theme_classic.
#' @param title_size Numeric value for the size of the plot title. Default is 14.
#' @param subtitle_size Numeric value for the size of the plot subtitle. Default is 12.
#' @param axis_title_size Numeric value for the size of axis titles. Default is 10.
#' @param legend_title_size Numeric value for the size of the legend title. Default is 10.
#' @param jitter_width Numeric value for the amount of horizontal jitter. Default is 0.3.
#' @param jitter_height Numeric value for the amount of vertical jitter. Default is 0.3.
#'
#' @return A ggplot object containing two side-by-side plots of the policy tree results.
#'   The left plot shows the relationship between the first and second split variables,
#'   while the right plot shows the relationship between the second and third split variables.
#'   Points are colored according to the predicted optimal treatment.
#'
#' @details
#' The function automatically extracts the relevant data from the mc_test object,
#' which should be the output of margot_multi_arm_causal_forest(). It uses the
#' plot_data component of the specified model's results, which contains the test set
#' data, predictions, and split variable names.
#'
#' The function uses geom_jitter() instead of geom_point() to add a small amount of
#' random variation to the position of each point. This helps to separate overlapping
#' points and provides a better view of the data distribution, especially when there
#' are many data points or when the variables are discrete.
#'
#' @examples
#' \dontrun{
#' # Assuming mc_test is the output from margot_multi_arm_causal_forest()
#' plot <- plot_policy_tree_results(mc_test, "model_t2_belong_z")
#' print(plot)
#'
#' # Customizing the plot
#' plot <- plot_policy_tree_results(mc_test, "model_t2_belong_z",
#'                                  point_alpha = 0.8,
#'                                  jitter_width = 0.2,
#'                                  jitter_height = 0.2,
#'                                  theme_function = theme_minimal)
#' print(plot)
#' }
#'
#' @importFrom ggplot2 ggplot geom_jitter theme_classic labs scale_color_discrete theme element_text
#' @importFrom patchwork plot_layout
#' @importFrom ggokabeito scale_colour_okabe_ito
#'
#' @export
margot_multi_arm_causal_forest <- function(data, outcome_vars, covariates, W_multi, weights,
                                           exposure_name, grf_defaults = list(),
                                           save_data = FALSE, top_n_vars = 10,
                                           save_models = FALSE, compute_qini = TRUE,
                                           train_proportion = 0.9) {
  # Ensure W_multi is a factor
  if (!is.factor(W_multi)) {
    W_multi <- as.factor(W_multi)
  }

  # Create not_missing vector
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

      # Process results
      ate <- average_treatment_effect(model)
      custom_table <- margot::margot_model_evalue(model, new_name = outcome, subset = NULL)
      tau_hat <- predict(model, X = covariates, drop = TRUE)$predictions

      # Store results
      results[[model_name]] <- list(
        ate = ate,
        custom_table = custom_table,
        tau_hat = tau_hat
      )

      # Variable importance
      varimp <- variable_importance(model)
      ranked_vars <- order(varimp, decreasing = TRUE)
      top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
      results[[model_name]]$top_vars <- top_vars
      results[[model_name]]$variable_importance <- data.frame(
        variable = colnames(covariates),
        importance = varimp
      ) |> arrange(desc(importance))

      # Policy learning
      dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$dr_scores <- dr_scores

      # Train policy tree on 90% of non-missing data
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


