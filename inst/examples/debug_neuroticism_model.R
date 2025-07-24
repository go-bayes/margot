# Debug script to examine the neuroticism flipped model structure
library(margot)
library(cli)

# Assuming models_binary_flipped_all is loaded
model_name <- "model_t2_neuroticism_z_r"

cli::cli_h1("Debugging {model_name}")

# Check if model exists
if (model_name %in% names(models_binary_flipped_all$results)) {
  cli::cli_alert_success("Model found in results")
  
  model_result <- models_binary_flipped_all$results[[model_name]]
  
  # Check what's in the model result
  cli::cli_h2("Model result structure")
  cli::cli_alert_info("Names in model_result: {paste(names(model_result), collapse = ', ')}")
  
  # Check for model/full_model
  cli::cli_h2("Forest objects")
  cli::cli_alert_info("Has 'model' field: {!is.null(model_result$model)}")
  cli::cli_alert_info("Has 'full_model' field: {!is.null(model_result$full_model)}")
  
  if (!is.null(model_result$model)) {
    cli::cli_alert_info("model class: {class(model_result$model)}")
    cli::cli_alert_info("Has Y.orig: {!is.null(model_result$model$Y.orig)}")
    cli::cli_alert_info("Has W.orig: {!is.null(model_result$model$W.orig)}")
    if (!is.null(model_result$model$Y.orig)) {
      cli::cli_alert_info("Y.orig length: {length(model_result$model$Y.orig)}")
      cli::cli_alert_info("Y.orig mean: {round(mean(model_result$model$Y.orig), 3)}")
    }
  }
  
  # Check full_models
  cli::cli_h2("Full models storage")
  cli::cli_alert_info("Has full_models: {!is.null(models_binary_flipped_all$full_models)}")
  if (!is.null(models_binary_flipped_all$full_models)) {
    cli::cli_alert_info("Model in full_models: {model_name %in% names(models_binary_flipped_all$full_models)}")
    
    if (model_name %in% names(models_binary_flipped_all$full_models)) {
      full_model <- models_binary_flipped_all$full_models[[model_name]]
      cli::cli_alert_info("full_model class: {class(full_model)}")
      cli::cli_alert_info("Has Y.orig: {!is.null(full_model$Y.orig)}")
      cli::cli_alert_info("Has W.orig: {!is.null(full_model$W.orig)}")
      if (!is.null(full_model$Y.orig)) {
        cli::cli_alert_info("Y.orig length: {length(full_model$Y.orig)}")
        cli::cli_alert_info("Y.orig mean: {round(mean(full_model$Y.orig), 3)}")
      }
    }
  }
  
  # Check data storage
  cli::cli_h2("Data storage")
  cli::cli_alert_info("Has models_binary_flipped_all$data: {!is.null(models_binary_flipped_all$data)}")
  if (!is.null(models_binary_flipped_all$data)) {
    cli::cli_alert_info("Data names: {paste(names(models_binary_flipped_all$data), collapse = ', ')}")
    
    # Check for various possible names
    possible_names <- c("t2_neuroticism_z_r", "neuroticism_z_r", "model_t2_neuroticism_z_r")
    for (name in possible_names) {
      if (name %in% names(models_binary_flipped_all$data)) {
        cli::cli_alert_success("Found data under name: {name}")
      }
    }
  }
  
  # Check other model fields that might have data
  cli::cli_h2("Other data fields")
  cli::cli_alert_info("Has Y field: {!is.null(model_result$Y)}")
  cli::cli_alert_info("Has outcome_data field: {!is.null(model_result$outcome_data)}")
  cli::cli_alert_info("Has tau_hat: {!is.null(model_result$tau_hat)}")
  if (!is.null(model_result$tau_hat)) {
    cli::cli_alert_info("tau_hat length: {length(model_result$tau_hat)}")
  }
  
} else {
  cli::cli_alert_danger("Model {model_name} not found in results")
  cli::cli_alert_info("Available models: {paste(names(models_binary_flipped_all$results), collapse = ', ')}")
}

# Check how the model was created
cli::cli_h2("Model creation info")
cli::cli_alert_info("Has flip_outcomes field: {!is.null(models_binary_flipped_all$flip_outcomes)}")
if (!is.null(models_binary_flipped_all$flip_outcomes)) {
  cli::cli_alert_info("Flipped outcomes: {paste(models_binary_flipped_all$flip_outcomes, collapse = ', ')}")
}