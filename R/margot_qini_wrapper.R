#' Wrapper Function for QINI Analysis with Backward Compatibility
#'
#' @description
#' This function provides backward compatibility for margot_interpret_qini by
#' formatting the output of margot_qini_alternative into the expected structure.
#'
#' @param margot_result Output from margot_causal_forest()
#' @param model_names Character vector of model names to process (NULL = all)
#' @param seed Random seed for reproducibility
#' @param n_bootstrap Number of bootstrap replicates for confidence intervals
#' @param verbose Print progress messages
#' @param spend_levels Numeric vector of spend levels for analysis (default 0.1)
#' @param label_mapping Named list for label transformations
#'
#' @return List formatted for margot_interpret_qini with diff_gain_summaries
#'
#' @export
margot_qini <- function(margot_result,
                        model_names = NULL,
                        seed = 12345,
                        n_bootstrap = 200,
                        verbose = TRUE,
                        spend_levels = c(0.1, 0.4),
                        label_mapping = NULL) {
  # apply new qini computation
  result <- margot_qini_alternative(
    margot_result = margot_result,
    model_names = model_names,
    seed = seed,
    n_bootstrap = n_bootstrap,
    verbose = verbose,
    spend_levels = spend_levels,
    label_mapping = label_mapping
  )

  # format for margot_interpret_qini compatibility
  multi_batch <- list()

  # extract results with diff_gain_summaries
  for (model_name in names(result$results)) {
    if (!is.null(result$results[[model_name]]$diff_gain_summaries)) {
      # copy the model result including diff_gain_summaries
      multi_batch[[model_name]] <- result$results[[model_name]]

      # also add baseline_method for compatibility
      if (is.null(multi_batch[[model_name]]$baseline_method)) {
        multi_batch[[model_name]]$baseline_method <- "grf_standard"
      }
    }
  }

  return(multi_batch)
}
