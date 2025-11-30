#' Subset Model Results for Binary and Categorical Exposures
#'
#' This function extracts and combines evaluation tables for specified outcome variables
#' from model results, handling both binary and categorical exposure models. It provides
#' enhanced functionality for creating and analyzing subsets based on various conditions.
#'
#' @param model_results A list of model results from `model_causal_forest` or similar functions.
#' @param outcome_vars Optional. A character vector of outcome variable names. If NULL,
#'        the function will use all models in the input.
#' @param subset_condition A logical vector indicating the subset of data to use. Default is NULL.
#'        Can be generated using the subset_var, subset_value, and subset_operator parameters.
#' @param scale A character string indicating the scale to use. Default is "RD".
#' @param contrast For categorical exposures, a single character string specifying the contrast to extract.
#' @param X Optional. The feature matrix used in the original models. Required if using subset_var.
#' @param subset_var Optional. The name of the variable to use for subsetting.
#' @param subset_value Optional. The value to compare against for subsetting. Default is 1.
#' @param subset_operator Optional. The operator to use for comparison ("==", ">", ">=", "<", "<=", "!="). Default is "==".
#' @param subset_description Optional. A description of the subset for reporting. If NULL, one will be generated.
#' @param debug Logical. If TRUE, print debug information. Default is FALSE.
#'
#' @return A list containing:
#'   \item{results}{A data frame combining all custom evaluation tables for the specified outcomes and contrast.}
#'   \item{subset_condition}{The logical vector used for subsetting.}
#'   \item{subset_description}{A description of the subset.}
#'   \item{subset_info}{Additional information about the subset, if available.}
#'
#' @details
#' The function can be used in two ways:
#' 1. By providing a pre-computed logical vector as `subset_condition`.
#' 2. By providing a variable name (`subset_var`), value (`subset_value`), and operator (`subset_operator`)
#'    to generate the subset condition automatically.
#'
#' For categorical exposures, specify the contrast parameter to extract specific comparisons.
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic subsetting with a pre-computed condition
#' subset_condition_conservative <- X[, "t0_political_conservative_z"] > 1
#' model_subset_conservative <- margot_subset_model(
#'   model_results = models_binary,
#'   subset_condition = subset_condition_conservative,
#'   debug = FALSE
#' )
#'
#' # Example 2: Using the built-in subsetting functionality
#' model_subset_religious <- margot_subset_model(
#'   model_results = models_binary,
#'   X = X,
#'   subset_var = "t0_religion_bigger_denominations_not_rel_binary",
#'   subset_value = 1,
#'   subset_description = "Effects among religious participants",
#'   debug = TRUE
#' )
#'
#' # Example 3: For categorical exposures with specific contrast
#' model_subset_gen_z <- margot_subset_model(
#'   model_results = models_cat,
#'   X = X,
#'   subset_var = "t0_gen_cohort_gen_Z_binary",
#'   subset_value = 1,
#'   contrast = "[6.0,7.0] - [1.0,5.0)",
#'   scale = "RD",
#'   debug = FALSE
#' )
#'
#' # Example 4: Multiple outcome variables
#' model_subset_boomers <- margot_subset_model(
#'   model_results = models_binary,
#'   outcome_vars = c("t2_wellbeing_z", "t2_depression_z"),
#'   X = X,
#'   subset_var = "t0_gen_cohort_gen_Boomers_binary",
#'   subset_value = 1,
#'   subset_operator = "==",
#'   subset_description = "Effects among Baby Boomer participants",
#'   debug = TRUE
#' )
#'
#' # Example 5: More complex subsetting (multiple conditions)
#' # Define a complex condition directly
#' complex_condition <- X[, "t0_political_conservative_z"] > 1 & X[, "t0_age_z"] > -2
#' model_subset_complex <- margot_subset_model(
#'   model_results = models_binary,
#'   subset_condition = complex_condition,
#'   subset_description = "Conservative (>1 SD) and not very young (>-2 SD in age)",
#'   debug = FALSE
#' )
#' }
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#'
#' @export
margot_subset_model <- function(model_results,
                                outcome_vars = NULL,
                                subset_condition = NULL,
                                scale = "RD",
                                contrast = NULL,
                                X = NULL,
                                subset_var = NULL,
                                subset_value = 1,
                                subset_operator = "==",
                                subset_description = NULL,
                                debug = FALSE) {
  # start logging
  cli::cli_alert_info("starting margot_subset_model function")

  # validate inputs
  if (is.null(model_results$full_models) && is.null(model_results$results)) {
    cli::cli_alert_danger("no full models or results found in the input. please ensure you're using the correct model output.")
    return(NULL)
  }

  # helper function to escape regex special characters
  escape_regex <- function(string) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
  }

  # generate subset condition if not provided but subset_var is
  if (is.null(subset_condition) && !is.null(subset_var)) {
    if (is.null(X)) {
      cli::cli_alert_danger("if using subset_var, X must be provided.")
      return(NULL)
    }

    # check if subset_var exists in X
    if (!subset_var %in% colnames(X)) {
      cli::cli_alert_danger("variable '{subset_var}' not found in X.")
      return(NULL)
    }

    # validate the operator
    valid_ops <- c("==", ">", ">=", "<", "<=", "!=")
    if (!subset_operator %in% valid_ops) {
      cli::cli_alert_danger("operator '{subset_operator}' is not supported. choose one of {paste(valid_ops, collapse=', ')}")
      return(NULL)
    }

    # define the operator function
    # define the operator function
    subset_op <- switch(subset_operator,
      "==" = `==`,
      ">"  = `>`,
      ">=" = `>=`,
      "<"  = `<`,
      "<=" = `<=`,
      "!=" = `!=`
    )

    # create the subset condition and convert NA to FALSE
    subset_condition <- subset_op(X[, subset_var], subset_value)
    subset_condition[is.na(subset_condition)] <- FALSE

    if (debug) {
      cli::cli_alert_info("created subset condition: {subset_var} {subset_operator} {subset_value}")
      cli::cli_alert_info("subset size: {sum(subset_condition)} out of {length(subset_condition)}")
    }

    if (debug) {
      cli::cli_alert_info("created subset condition: {subset_var} {subset_operator} {subset_value}")
      cli::cli_alert_info("subset size: {sum(subset_condition)} out of {length(subset_condition)}")
    }

    # create the subset condition
    subset_condition <- subset_op(X[, subset_var], subset_value)

    if (debug) {
      cli::cli_alert_info("created subset condition: {subset_var} {subset_operator} {subset_value}")
      cli::cli_alert_info("subset size: {sum(subset_condition)} out of {length(subset_condition)}")
    }
  }

  # create descriptive title if not provided but we have subset info
  if (is.null(subset_description) && !is.null(subset_var)) {
    subset_description <- paste("effects among those with", subset_var, subset_operator, subset_value)
    if (debug) cli::cli_alert_info("generated subset description: {subset_description}")
  }

  # determine if we're dealing with binary or categorical exposure
  is_categorical <- !is.null(contrast)
  if (debug) cli::cli_alert_info("exposure type: {ifelse(is_categorical, 'categorical', 'binary')}")

  # set outcome variables if not provided
  if (is.null(outcome_vars)) {
    if (!is.null(model_results$full_models)) {
      outcome_vars <- gsub("^model_", "", names(model_results$full_models))
    } else {
      outcome_vars <- gsub("^model_", "", names(model_results$results))
    }
    if (debug) cli::cli_alert_info("outcome variables set to: {paste(outcome_vars, collapse=', ')}")
  }

  # process each outcome
  tables <- list()
  for (outcome in outcome_vars) {
    cli::cli_alert_info("processing outcome: {outcome}")
    model_name <- paste0("model_", outcome)

    if (!is.null(model_results$full_models) && model_name %in% names(model_results$full_models)) {
      # binary exposure case
      model <- model_results$full_models[[model_name]]
      if (debug) {
        cli::cli_alert_info("model class: {class(model)}")
        cli::cli_alert_info("model names: {paste(names(model), collapse=', ')}")
      }
      tryCatch(
        {
          custom_table <- margot::margot_model_evalue(model, scale = scale, new_name = outcome, subset = subset_condition)
          tables[[outcome]] <- custom_table
          cli::cli_alert_success("successfully created table for {outcome}")
        },
        error = function(e) {
          cli::cli_alert_warning("error in margot_model_evalue for {outcome}: {e$message}")
        }
      )
    } else if (!is.null(model_results$results) && model_name %in% names(model_results$results)) {
      # categorical exposure case
      result <- model_results$results[[model_name]]
      if (!is.null(result$custom_table)) {
        tables[[outcome]] <- result$custom_table
        cli::cli_alert_success("successfully extracted table for {outcome}")
      } else {
        cli::cli_alert_warning("no custom table found for {outcome}")
      }
    } else {
      cli::cli_alert_warning("results for outcome {outcome} not found in the input models.")
    }
  }

  if (length(tables) == 0) {
    cli::cli_alert_danger("no tables were created.")
    return(NULL)
  }

  combined_table <- do.call(rbind, tables)

  # clean up row names
  rownames(combined_table) <- gsub("^[^.]+\\.", "", rownames(combined_table))

  # filter the combined table for the specified contrast if it's a categorical model
  if (is_categorical) {
    escaped_contrast <- escape_regex(contrast)
    filtered_table <- combined_table[grepl(escaped_contrast, rownames(combined_table), fixed = FALSE), , drop = FALSE]
    if (nrow(filtered_table) > 0) {
      combined_table <- filtered_table
      cli::cli_alert_success("successfully filtered table for contrast: {contrast}")
    } else {
      cli::cli_alert_warning("specified contrast not found in any outcomes. returning unfiltered table.")
    }
  }

  # prepare return object
  result <- list(
    results = combined_table,
    subset_condition = subset_condition,
    subset_description = subset_description,
    subset_info = list(
      subset_var = subset_var,
      subset_value = subset_value,
      subset_operator = subset_operator,
      subset_size = sum(subset_condition, na.rm = TRUE),
      total_size = if (!is.null(subset_condition)) length(subset_condition) else NULL
    )
  )

  cli::cli_alert_success("processing complete. returning results.")
  return(result)
}
