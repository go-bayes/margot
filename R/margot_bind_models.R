#' Combine multiple batched causal forest model objects
#'
#' @description
#' This function combines multiple batched causal forest model objects into a single
#' combined model object, provided they have compatible structures.
#'
#' @param ... One or more batched model objects with the structure shown in the examples.
#' @param quiet Logical; if TRUE, suppresses CLI feedback messages. Default is FALSE.
#'
#' @return A combined model object with the same structure as the inputs but containing
#'   all models from the input objects.
#'
#' @details
#' The function checks that all input model objects have the same structure and
#' compatible elements before combining them. It ensures that the 'not_missing'
#' vectors are identical across all model objects.
#'
#' @examples
#' # assuming models_binary_social, models_binary_psych, and models_binary_health exist
#' combined_models <- margot_bind_models(models_binary_social, models_binary_psych)
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom purrr map_lgl
#' @importFrom dplyr bind_rows
#' @export
margot_bind_models <- function(..., quiet = FALSE) {
  # import required packages
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # collect all model objects
  models <- list(...)

  # check if there are any models to combine
  if (length(models) < 1) {
    cli::cli_alert_danger("No models provided!")
    return(NULL)
  }

  if (length(models) == 1) {
    cli::cli_alert_warning("Only one model provided, returning as is.")
    return(models[[1]])
  }

  if (!quiet) cli::cli_alert_info("Checking compatibility of {length(models)} model objects...")

  # verify all objects have the same structure (5 elements with the same names)
  expected_names <- c("results", "combined_table", "outcome_vars", "not_missing", "full_models")
  structure_check <- purrr::map_lgl(models, function(model) {
    all(names(model) %in% expected_names) && length(names(model)) == 5
  })

  if (!all(structure_check)) {
    cli::cli_alert_danger("Not all model objects have the expected structure!")
    return(NULL)
  }

  # check if not_missing vectors are identical across all models
  not_missing_check <- purrr::map(models, ~ .x$not_missing)
  identical_not_missing <- all(
    purrr::map_lgl(not_missing_check[-1], ~ identical(.x, not_missing_check[[1]]))
  )

  if (!identical_not_missing) {
    cli::cli_alert_danger("The 'not_missing' vectors are not identical across all models!")
    return(NULL)
  }

  if (!quiet) cli::cli_alert_success("All models are compatible!")

  # combine the models
  if (!quiet) cli::cli_alert_info("Combining models...")

  # initialise combined model
  combined_model <- list()

  # combine results lists
  combined_model$results <- unlist(
    purrr::map(models, ~ .x$results),
    recursive = FALSE
  )

  # combine combined_tables
  combined_model$combined_table <- dplyr::bind_rows(
    purrr::map(models, ~ .x$combined_table)
  )

  # combine outcome_vars
  combined_model$outcome_vars <- unlist(
    purrr::map(models, ~ .x$outcome_vars)
  )

  # use not_missing from first model (they're all identical)
  combined_model$not_missing <- models[[1]]$not_missing

  # combine full_models
  combined_model$full_models <- unlist(
    purrr::map(models, ~ .x$full_models),
    recursive = FALSE
  )

  if (!quiet) {
    cli::cli_alert_success("Successfully combined {length(models)} model objects!")
    cli::cli_alert_info("Combined object contains {length(combined_model$results)} individual models.")
  }

  return(combined_model)
}
