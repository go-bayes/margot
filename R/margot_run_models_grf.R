#' Run Multiple Generalized Random Forest (GRF) Models with Progress Tracking
#'
#' This function runs multiple GRF models for specified outcome variables,
#' calculates average treatment effects, tests calibration, and creates custom
#' evaluation tables. It includes a progress bar to track the model fitting process.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A matrix of treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#'
#' @return A list containing two elements:
#'   \item{results}{A list of model results, one for each outcome variable.
#'                  Each element contains the model object, average treatment effect,
#'                  test calibration measure, and a custom evaluation table.}
#'   \item{combined_table}{A data frame combining all custom evaluation tables.}
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration
#' @importFrom dplyr %>%
#' @importFrom progressr progressor with_progress
#' @importFrom margot margot_model_evalue
#'
#' @examples
#' \dontrun{
#' grf_defaults <- list(seed = 123, stabilize.splits = TRUE,
#'                      tune.parameters = "all", num.trees = 5000)
#' result <- margot_run_models_grf(
#'   data = df_grf,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   covariates = X,
#'   W = W,
#'   weights = weights,
#'   grf_defaults = grf_defaults
#' )
#' }
#'
#' @export
margot_run_models_grf <- function(data, outcome_vars, covariates, W, weights, grf_defaults = list()) {
  results <- list()
  tables <- list()

  run_models_with_progress <- function() {
    p <- progressor(along = outcome_vars)

    for (outcome in outcome_vars) {
      # check if the model has already been run
      model_name <- paste0("model_", outcome)
      if (model_name %in% names(results)) {
        p(sprintf("Skipping %s (already run)", outcome))
        next
      }

      # set outcome variable
      Y <- as.matrix(data[[outcome]])

      # run grf model
      model <- do.call(grf::causal_forest, c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))

      # calculate average treatment effect
      ate <- round(grf::average_treatment_effect(model), 3)

      # test calibration measure
      test_calibration_measure <- round(grf::test_calibration(model), 3)

      # custom function table
      custom_table <- margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL)

      # store results
      results[[model_name]] <<- list(
        model = model,
        ate = ate,
        test_calibration = test_calibration_measure,
        custom_table = custom_table
      )

      # store custom table for binding later
      tables[[outcome]] <<- custom_table

      # update progress bar
      p(sprintf("Completed %s", outcome))
    }
  }

  # use with_progress inside the function
  with_progress(run_models_with_progress())

  # bind tables for outcomes within each domain
  combined_table <- do.call(rbind, tables)

  return(list(
    results = results,
    combined_table = combined_table
  ))
}
