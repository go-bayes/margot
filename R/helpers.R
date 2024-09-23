#' @keywords internal
get_original_value <- function(var_name, split_value, original_df) {
  if (is.null(original_df)) return(NULL)

  # create a list of possible variable names
  orig_var_candidates <- character()

  # start with var_name
  orig_var_candidates <- c(orig_var_candidates, var_name)

  # remove '_z' suffix
  var_no_z <- sub("_z$", "", var_name)
  orig_var_candidates <- c(orig_var_candidates, var_no_z)

  # keep '_log_' in variable name
  var_with_log <- var_no_z
  orig_var_candidates <- c(orig_var_candidates, var_with_log)

  # remove 't0_' prefix
  var_no_t0 <- sub("^t[0-9]+_", "", var_with_log)
  orig_var_candidates <- c(orig_var_candidates, var_no_t0)

  # remove duplicates
  orig_var_candidates <- unique(orig_var_candidates)

  # try to find a matching variable in original_df
  orig_var <- NULL
  for (candidate in orig_var_candidates) {
    if (candidate %in% names(original_df)) {
      orig_var <- candidate
      break
    }
  }

  if (is.null(orig_var)) {
    cli::cli_warn("Original variable '{var_name}' not found in original_df. Skipping original scale value.")
    return(NULL)
  }

  # check if variable was log-transformed
  was_log_transformed <- grepl("_log_", var_name)

  orig_data <- original_df[[orig_var]]

  # calculate mean and sd from original data
  orig_mean <- mean(orig_data, na.rm = TRUE)
  orig_sd <- sd(orig_data, na.rm = TRUE)

  # Back-transform z-score to original scale
  original_value <- orig_mean + split_value * orig_sd

  # if variable was log-transformed, back-transform to original data scale
  if (was_log_transformed) {
    original_value <- exp(original_value) - 1
  }

  return(round(original_value, 3))
}
#' @keywords internal
get_original_value_plot <- function(var_name, split_value, original_df) {
  if (is.null(original_df)) return(NULL)

  # create list of possible variable names
  orig_var_candidates <- character()

  # start with var_name
  orig_var_candidates <- c(orig_var_candidates, var_name)

  # remove '_z' suffix
  var_no_z <- sub("_z$", "", var_name)
  orig_var_candidates <- c(orig_var_candidates, var_no_z)

  # keep '_log_' in variable name
  var_with_log <- var_no_z
  orig_var_candidates <- c(orig_var_candidates, var_with_log)

  # remove 't0_' prefix
  var_no_t0 <- sub("^t[0-9]+_", "", var_with_log)
  orig_var_candidates <- c(orig_var_candidates, var_no_t0)

  # remove duplicates
  orig_var_candidates <- unique(orig_var_candidates)

  # try to find a matching variable in original_df
  orig_var <- NULL
  for (candidate in orig_var_candidates) {
    if (candidate %in% names(original_df)) {
      orig_var <- candidate
      break
    }
  }

  if (is.null(orig_var)) {
    cli::cli_warn("Original variable '{var_name}' not found in original_df. Skipping original scale value.")
    return(NULL)
  }

  # check if variable was log-transformed
  was_log_transformed <- grepl("_log_", var_name)

  orig_data <- original_df[[orig_var]]

  # Calculate mean and sd from the original data
  orig_mean <- mean(orig_data, na.rm = TRUE)
  orig_sd <- sd(orig_data, na.rm = TRUE)

  # Back-transform z-score to the original scale
  original_value <- orig_mean + split_value * orig_sd

  # If variable was log-transformed, back-transform to the original data scale
  if (was_log_transformed) {
    original_value <- exp(original_value) - 1
  }

  return(round(original_value, 3))
}

#' @keywords internal
transform_var_name <- function(var_name, label_mapping = NULL,
                               remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
                               use_title_case = TRUE, remove_underscores = TRUE) {
  display_name <- var_name

  # Remove 'model_' prefix if present (for model names)
  if (startsWith(display_name, "model_")) {
    display_name <- sub("^model_", "", display_name)
  }

  # apply label mapping first, if exists
  if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
    mapped_label <- label_mapping[[display_name]]
    cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
    return(mapped_label)
  }

  # else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
  if (startsWith(display_name, "t0_")) {
    t2_var <- sub("^t0_", "t2_", display_name)
    if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
      mapped_label <- label_mapping[[t2_var]]
      cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
      return(mapped_label)
    }
  }

  # else, apply transformations
  if (remove_tx_prefix) {
    display_name <- sub("^t[0-9]+_", "", display_name)
  }
  if (remove_z_suffix) {
    display_name <- sub("_z$", "", display_name)
  }
  if (remove_underscores) {
    display_name <- gsub("_", " ", display_name)
  }

  if (use_title_case) {
    display_name <- tools::toTitleCase(display_name)
    # Replace "Nz" with "NZ"
    display_name <- gsub("Nz", "NZ", display_name)
  }

  # Notify if transformed
  if (display_name != var_name) {
    cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
  }

  return(display_name)
}

