#' @keywords internal
#' works with transform to original scale
determine_transformation <- function(var_name) {
  list(
    was_log_transformed = grepl("_log_", var_name),
    was_z_transformed = grepl("_z$", var_name)
  )
}


#' @keywords internal
get_original_value <- function(var_name, split_value, original_df) {
  if (is.null(original_df)) return(NULL)

  # Create a list of possible variable names
  orig_var_candidates <- character()

  # Start with var_name
  orig_var_candidates <- c(orig_var_candidates, var_name)

  # Remove '_z' suffix
  var_no_z <- sub("_z$", "", var_name)
  orig_var_candidates <- c(orig_var_candidates, var_no_z)

  # Keep '_log_' in variable name
  var_with_log <- var_no_z
  orig_var_candidates <- c(orig_var_candidates, var_with_log)

  # Remove 't0_' prefix
  var_no_t0 <- sub("^t[0-9]+_", "", var_with_log)
  orig_var_candidates <- c(orig_var_candidates, var_no_t0)

  # Remove duplicates
  orig_var_candidates <- unique(orig_var_candidates)

  # Try to find a matching variable in original_df
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

  # Check if variable was log-transformed and/or z-transformed
  was_log_transformed <- grepl("_log_", var_name)
  was_z_transformed <- grepl("_z$", var_name)

  orig_data <- original_df[[orig_var]]

  if (was_z_transformed) {
    # Calculate mean and sd from the original data
    orig_mean <- mean(orig_data, na.rm = TRUE)
    orig_sd <- sd(orig_data, na.rm = TRUE)

    # Back-transform z-score to the original scale
    original_value <- orig_mean + split_value * orig_sd
  } else {
    # If not z-transformed, the split_value is already on the (log-transformed) scale
    original_value <- split_value
  }

  # If variable was log-transformed, back-transform to the original data scale
  if (was_log_transformed) {
    original_value <- exp(original_value) - 1
  }

  return(round(original_value, 3))
}

#' @keywords internal
get_original_var_info <- function(var_name, original_df) {
  # Create a list of possible variable names
  orig_var_candidates <- character()

  # Start with var_name
  orig_var_candidates <- c(orig_var_candidates, var_name)

  # Remove '_z' suffix
  var_no_z <- sub("_z$", "", var_name)
  orig_var_candidates <- c(orig_var_candidates, var_no_z)

  # Keep '_log_' in variable name
  var_with_log <- var_no_z
  orig_var_candidates <- c(orig_var_candidates, var_with_log)

  # Remove 't0_' prefix
  var_no_t0 <- sub("^t[0-9]+_", "", var_with_log)
  orig_var_candidates <- c(orig_var_candidates, var_no_t0)

  # Remove duplicates
  orig_var_candidates <- unique(orig_var_candidates)

  # Try to find a matching variable in original_df
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

  # Check if variable was log-transformed and/or z-transformed
  was_log_transformed <- grepl("_log_", var_name)
  was_z_transformed <- grepl("_z$", var_name)

  orig_data <- original_df[[orig_var]]

  if (was_z_transformed) {
    # Calculate mean and sd from the original data
    orig_mean <- mean(orig_data, na.rm = TRUE)
    orig_sd <- sd(orig_data, na.rm = TRUE)
  } else {
    orig_mean <- NA
    orig_sd <- NA
  }

  return(list(
    orig_mean = orig_mean,
    orig_sd = orig_sd,
    was_log_transformed = was_log_transformed,
    was_z_transformed = was_z_transformed
  ))
}


#' @keywords internal
get_original_value_plot <- function(var_name, split_value, original_df) {
  if (is.null(original_df)) return(NULL)

  # Create list of possible variable names
  orig_var_candidates <- c(
    var_name,
    sub("_z$", "", var_name),
    sub("^t[0-9]+_", "", sub("_z$", "", var_name))
  )
  orig_var_candidates <- unique(orig_var_candidates)

  # Try to find a matching variable in original_df
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

  # Determine transformation types
  was_log_transformed <- grepl("_log_", var_name)
  was_z_transformed <- grepl("_z$", var_name)

  orig_data <- original_df[[orig_var]]

  if (was_z_transformed) {
    # Calculate mean and sd from the original data
    orig_mean <- mean(orig_data, na.rm = TRUE)
    orig_sd <- sd(orig_data, na.rm = TRUE)

    # Back-transform z-score to the original scale
    original_value <- orig_mean + split_value * orig_sd
  } else {
    # If not z-transformed, the split_value is already on the (log-transformed) scale
    original_value <- split_value
  }

  # If variable was log-transformed, back-transform to the original data scale
  if (was_log_transformed) {
    original_value <- exp(original_value) - 1
  }

  return(round(original_value, 3))
}


#' @keywords internal
#' for margot_plot()
transform_label <- function(label, label_mapping = NULL, options = list()) {
  original_label <- label

  # Apply mapping with partial substitutions and remove numbers
  if (!is.null(label_mapping)) {
    for (pattern in names(label_mapping)) {
      if (grepl(pattern, label, fixed = TRUE)) {
        replacement <- label_mapping[[pattern]]
        label <- gsub(pattern, replacement, label, fixed = TRUE)
        cli::cli_alert_info("Mapped label: {pattern} -> {replacement}")
      }
    }
  }

  # Remove the numerical part (e.g., " - (3.0,7.0] - [1.0,2.0]")
  label <- sub(" - \\(.*\\]$", "", label)

  # Apply default transformations if the label wasn't fully replaced
  if (label == original_label) {
    if (options$remove_tx_prefix) {
      label <- sub("^t[0-9]+_", "", label)
    }
    if (options$remove_z_suffix) {
      label <- sub("_z$", "", label)
    }
    if (options$remove_underscores) {
      label <- gsub("_", " ", label)
    }
    if (options$use_title_case) {
      label <- tools::toTitleCase(label)
      # Preserve "NZ" capitalization
      label <- gsub("Nz", "NZ", label)
    }
  }

  if (label != original_label) {
    cli::cli_alert_info("Transformed label: {original_label} -> {label}")
  }

  return(label)
}


#' @keywords internal
transform_to_original_scale <- function(results_df, original_df, label_mapping = NULL) {
  # Determine the effect size column based on the data structure
  if ("E[Y(1)]-E[Y(0)]" %in% names(results_df)) {
    effect_size_col <- "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(results_df)) {
    effect_size_col <- "E[Y(1)]/E[Y(0)]"
  } else {
    cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
  }

  # Ensure that results_df has an 'outcome' column
  if (!"outcome" %in% names(results_df)) {
    results_df$outcome <- rownames(results_df)
    cli::cli_alert_info("Added 'outcome' column based on row names")
  }

  # Initialize columns in results_df for back-transformed estimates
  results_df[[paste0(effect_size_col, "_original")]] <- NA
  results_df[["2.5 %_original"]] <- NA
  results_df[["97.5 %_original"]] <- NA

  # Loop over each outcome
  for (i in seq_len(nrow(results_df))) {
    var_name <- results_df$outcome[i]
    estimate <- results_df[[effect_size_col]][i]
    lower <- results_df$`2.5 %`[i]
    upper <- results_df$`97.5 %`[i]

    # Compute standard error on the standardized scale
    z_value <- qnorm(0.975)  # For 95% CI
    SE_standardized <- (upper - lower) / (2 * z_value)

    # Get original variable info
    orig_var_info <- get_original_var_info(var_name, original_df)
    orig_mean <- orig_var_info$orig_mean
    orig_sd <- orig_var_info$orig_sd
    was_log_transformed <- orig_var_info$was_log_transformed
    was_z_transformed <- orig_var_info$was_z_transformed

    if (was_z_transformed) {
      # Back-transform estimate to original scale
      original_estimate <- orig_mean + estimate * orig_sd

      # Compute SE on original scale using the delta method
      if (was_log_transformed) {
        # For log-transformed variables, use the delta method for exp(x) - 1
        SE_original <- SE_standardized * orig_sd * exp(original_estimate)
      } else {
        # For linear transformations
        SE_original <- SE_standardized * orig_sd
      }
    } else {
      # If not z-transformed, the estimate is already on the (log-transformed) scale
      original_estimate <- estimate
      SE_original <- SE_standardized  # Assume SE is the same

      if (was_log_transformed) {
        # For log-transformed variables, adjust SE using the delta method
        SE_original <- SE_standardized * exp(original_estimate)
      }
    }

    # If variable was log-transformed, back-transform to the original data scale
    if (was_log_transformed) {
      original_estimate <- exp(original_estimate) - 1
    }

    # Compute confidence intervals on original scale
    original_lower <- original_estimate - z_value * SE_original
    original_upper <- original_estimate + z_value * SE_original

    # Assign back-transformed values to results_df
    results_df[[paste0(effect_size_col, "_original")]][i] <- round(original_estimate, 3)
    results_df[["2.5 %_original"]][i] <- round(original_lower, 3)
    results_df[["97.5 %_original"]][i] <- round(original_upper, 3)
  }

  # Return the transformed data and label_mapping
  return(list(results_df = results_df, label_mapping = label_mapping))
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

