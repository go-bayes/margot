# helpers for margot plot -------------------------------------------------

#' @keywords internal
back_transform_estimates <- function(results_df, original_df) {
  # Determine the effect size column - check new types first, then traditional
  effect_size_col <- NULL
  new_cols <- c("ATE", "ATT", "ATC", "ATO")

  # check for new column types
  for (col in new_cols) {
    if (col %in% names(results_df)) {
      effect_size_col <- col
      break
    }
  }

  # if not found, check for traditional columns
  if (is.null(effect_size_col)) {
    if ("E[Y(1)]-E[Y(0)]" %in% names(results_df)) {
      effect_size_col <- "E[Y(1)]-E[Y(0)]"
    } else if ("E[Y(1)]/E[Y(0)]" %in% names(results_df)) {
      effect_size_col <- "E[Y(1)]/E[Y(0)]"
    } else {
      stop("Data must contain either 'E[Y(1)]-E[Y(0)]', 'E[Y(1)]/E[Y(0)]', or one of: ATE, ATT, ATC, ATO")
    }
  }

  # ensure that results_df has an 'outcome' column
  if (!"outcome" %in% names(results_df)) {
    results_df$outcome <- rownames(results_df)
  }

  # store the original variable names before any transformations
  if (!"original_var_name" %in% names(results_df)) {
    results_df$original_var_name <- results_df$outcome
  }

  # initialise new columns for back-transformed estimates and units
  results_df[[paste0(effect_size_col, "_original")]] <- NA_real_
  results_df[["2.5 %_original"]] <- NA_real_
  results_df[["97.5 %_original"]] <- NA_real_
  results_df[["unit"]] <- NA_character_

  # loop over each outcome
  for (i in seq_len(nrow(results_df))) {
    var_name <- results_df$original_var_name[i]

    # check if this is a reversed/reduced variable
    is_reversed <- grepl("_r$", var_name)
    
    # remove _r suffix before processing (since original_df won't have reversed variables)
    if (is_reversed) {
      var_name <- sub("_r$", "", var_name)
    }

    # determine if the variable was z-transformed and/or log-transformed
    was_z_transformed <- grepl("_z$", var_name)
    was_log_transformed <- grepl("_log_", var_name)
    contains_hours <- grepl("_hours_", var_name)

    # get the original variable name in original_df
    orig_var_name <- var_name
    if (was_z_transformed) {
      orig_var_name <- sub("_z$", "", orig_var_name)
    }

    # try to find the variable in original_df
    if (!(orig_var_name %in% names(original_df))) {
      # Try removing 't0_', 't1_', 't2_' prefixes
      orig_var_name_no_prefix <- sub("^t[0-9]+_", "", orig_var_name)
      if (!(orig_var_name_no_prefix %in% names(original_df))) {
        # Variable not found in original_df - skip silently
        next
      } else {
        orig_var_name <- orig_var_name_no_prefix
      }
    }

    estimate <- results_df[[effect_size_col]][i]
    lower <- results_df$`2.5 %`[i]
    upper <- results_df$`97.5 %`[i]

    # Compute standard error on the standardized scale
    z_value <- qnorm(0.975)
    SE_standardized <- (upper - lower) / (2 * z_value)

    # Get mean and sd from original data
    orig_data <- original_df[[orig_var_name]]
    orig_sd <- sd(orig_data, na.rm = TRUE)
    mean_y <- mean(orig_data, na.rm = TRUE)

    if (was_z_transformed) {
      estimate_log <- estimate * orig_sd
      SE_log <- SE_standardized * orig_sd
    } else {
      estimate_log <- estimate
      SE_log <- SE_standardized
    }

    # Initialize unit
    unit <- ""

    if (was_log_transformed) {
      # For log-transformed variables, back-transform to original scale
      if (was_z_transformed) {
        # For log+z transformed variables, use back_transform_log_z
        # the treatment effect in z-score units
        z_effect <- estimate
        z_lower <- results_df$`2.5 %`[i]
        z_upper <- results_df$`97.5 %`[i]
        
        # back-transform the control group mean (z=0)
        control_orig <- back_transform_log_z(0, log_mean = mean_y, log_sd = orig_sd)
        
        # back-transform the treated group mean
        treated_orig <- back_transform_log_z(z_effect, log_mean = mean_y, log_sd = orig_sd)
        
        # calculate the difference
        delta_x <- treated_orig - control_orig
        
        # for confidence intervals
        treated_lower <- back_transform_log_z(z_lower, log_mean = mean_y, log_sd = orig_sd)
        treated_upper <- back_transform_log_z(z_upper, log_mean = mean_y, log_sd = orig_sd)
        
        delta_x_lower <- treated_lower - control_orig
        delta_x_upper <- treated_upper - control_orig
      } else {
        # For only log-transformed variables (not z-scored)
        # E[y|treatment = 0] = mean_y
        # E[y|treatment = 1] = mean_y + estimate_log
        E_y_treated <- mean_y + estimate_log
        E_y_control <- mean_y

        # Expected values on the original scale
        E_x_treated <- exp(E_y_treated) - 1
        E_x_control <- exp(E_y_control) - 1

        delta_x <- E_x_treated - E_x_control

        # For confidence intervals
        estimate_log_lower <- estimate_log - z_value * SE_log
        estimate_log_upper <- estimate_log + z_value * SE_log

        E_y_treated_lower <- mean_y + estimate_log_lower
        E_y_treated_upper <- mean_y + estimate_log_upper

        E_x_treated_lower <- exp(E_y_treated_lower) - 1
        E_x_treated_upper <- exp(E_y_treated_upper) - 1

        delta_x_lower <- E_x_treated_lower - E_x_control
        delta_x_upper <- E_x_treated_upper - E_x_control
      }

      # If variable contains '_hours_', transform to minutes
      if (contains_hours) {
        delta_x <- delta_x * 60
        delta_x_lower <- delta_x_lower * 60
        delta_x_upper <- delta_x_upper * 60
        unit <- "minutes"
      }

      # assign to results_df
      results_df[[paste0(effect_size_col, "_original")]][i] <- round(delta_x, 3)
      results_df[["2.5 %_original"]][i] <- round(delta_x_lower, 3)
      results_df[["97.5 %_original"]][i] <- round(delta_x_upper, 3)
    } else {
      # For variables not log-transformed, back-transform as before
      estimate_original <- estimate_log
      SE_original <- SE_log

      lower_original <- estimate_original - z_value * SE_original
      upper_original <- estimate_original + z_value * SE_original

      # If variable contains '_hours_', transform to minutes
      if (contains_hours) {
        estimate_original <- estimate_original * 60
        lower_original <- lower_original * 60
        upper_original <- upper_original * 60
        unit <- "minutes"
      }

      # round and assign to results_df
      results_df[[paste0(effect_size_col, "_original")]][i] <- round(estimate_original, 3)
      results_df[["2.5 %_original"]][i] <- round(lower_original, 3)
      results_df[["97.5 %_original"]][i] <- round(upper_original, 3)
    }

    # assign unit to results_df
    results_df[["unit"]][i] <- unit
  }

  return(results_df)
}


# palettes ---------------------------------------------------------------

#' Retrieve a named color palette for plotting
#'
#' Provides centralised, named palettes used across margot plotting functions.
#' The "lab" palette is intended for LMTP positivity/overlap plots and includes
#' explicit colours for common shift names (cleaned suffixes), including
#' `null`, `shift_zero`, and `ipsi_` variants (e.g., `ipsi_02`, `ipsi_05`, `ipsi_10`).
#'
#' @param name Character palette name. Currently supported: "lab", "classic".
#' @return A named character vector of hex colours.
#' @keywords internal
margot_palette <- function(name = c("lab", "classic")) {
  name <- match.arg(name)

  # Classic qualitative fallback
  classic <- c(
    "#4f88c6", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#CC79A7", "#D55E00", "#999999"
  )

  if (identical(name, "classic")) return(classic)

  # Lab palette for positivity/overlap. Named entries anchor specific
  # shift keywords to semantically consistent hues, while helper
  # `margot_palette_lab_resolve()` handles fuzzy matching (e.g.,
  # outcome-prefixed shift names).
  lab_named <- c(
    null = "#7f7f7f",
    shift_zero = "#2c7fb8",
    shift_down = "#2c7fb8",
    shift_up = "#d95f0e",
    constant = "#4f88c6",
    weekly = "#4292c6",
    monthly = "#9ecae1",
    ipsi_02 = "#fdd0a2",
    ipsi_05 = "#fd8d3c",
    ipsi_10 = "#d95f0e",
    ipsi_15 = "#a63603"
  )

  # Return named palette; consumers can cycle through additional classic colours if needed
  lab_named
}

#' Resolve lab palette colours for arbitrary shift labels
#'
#' @param labels Character vector of shift/policy names.
#' @param palette Character vector returned by `margot_palette("lab")`.
#' @param default Hex colour used when no match is found.
#' @return Character vector of colours aligned with `labels`.
#' @keywords internal
margot_palette_lab_resolve <- function(labels,
                                       palette = margot_palette("lab"),
                                       default = NA_character_) {
  if (is.null(labels)) return(character())

  pal <- as.character(palette %||% character())
  pal_names <- names(pal)
  if (is.null(pal_names)) pal_names <- rep("", length(pal))
  pal_names_lower <- tolower(pal_names)

  anchor_hex <- function(key, fallback_hex) {
    idx <- which(pal_names_lower == key)
    if (length(idx)) pal[[idx[[1L]]]] else fallback_hex
  }

  defaults <- list(
    null = "#7f7f7f",
    shift_zero = "#2c7fb8",
    shift_down = "#2c7fb8",
    shift_up = "#d95f0e",
    constant = "#4f88c6",
    weekly = "#4292c6",
    monthly = "#9ecae1"
  )

  anchors <- lapply(names(defaults), function(key) anchor_hex(key, defaults[[key]]))
  names(anchors) <- names(defaults)

  ipsi_palette <- pal[grepl("^ipsi_", pal_names_lower)]
  if (!length(ipsi_palette)) {
    ipsi_palette <- c(ipsi_02 = "#fdd0a2", ipsi_05 = "#fd8d3c", ipsi_10 = "#d95f0e", ipsi_15 = "#a63603")
  }
  ipsi_levels <- sub("^ipsi_", "", names(ipsi_palette))
  ipsi_numeric <- suppressWarnings(as.numeric(ipsi_levels))

  resolve_ipsi <- function(magnitude) {
    if (!length(ipsi_palette)) return(default)
    if (is.na(magnitude) || !nzchar(magnitude)) {
      return(ipsi_palette[[1L]])
    }
    mag_num <- suppressWarnings(as.numeric(magnitude))
    if (!is.na(mag_num) && length(ipsi_numeric) && !all(is.na(ipsi_numeric))) {
      idx <- which.min(abs(ipsi_numeric - mag_num))
      return(ipsi_palette[[idx]])
    }
    ipsi_palette[[length(ipsi_palette)]]
  }

  detect_single <- function(label) {
    if (is.null(label) || is.na(label)) return(default)
    label_chr <- as.character(label)
    if (!nzchar(label_chr)) return(default)

    exact_idx <- match(label_chr, pal_names)
    if (!is.na(exact_idx)) return(pal[[exact_idx]])

    label_lower <- tolower(label_chr)
    lower_idx <- match(label_lower, pal_names_lower)
    if (!is.na(lower_idx)) return(pal[[lower_idx]])

    parts <- unlist(strsplit(label_lower, "_"))
    if (length(parts) >= 1) {
      tail_one <- tail(parts, 1)
      idx_tail <- match(tail_one, pal_names_lower)
      if (!is.na(idx_tail)) return(pal[[idx_tail]])
    }
    if (length(parts) >= 2) {
      tail_two <- paste(tail(parts, 2), collapse = "_")
      idx_tail_two <- match(tail_two, pal_names_lower)
      if (!is.na(idx_tail_two)) return(pal[[idx_tail_two]])
    }

    if (grepl("null", label_lower, fixed = TRUE)) return(anchors$null)
    if (grepl("shift[_-]?zero", label_lower)) return(anchors$shift_zero)
    if (grepl("shift[_-]?down", label_lower)) return(anchors$shift_down)
    if (grepl("shift[_-]?up", label_lower)) return(anchors$shift_up)
    if (grepl("constant", label_lower, fixed = TRUE)) return(anchors$constant)
    if (grepl("weekly", label_lower, fixed = TRUE)) return(anchors$weekly)
    if (grepl("monthly", label_lower, fixed = TRUE)) return(anchors$monthly)

    ipsi_match <- regexpr("ipsi[_-]?(\\d{2})", label_lower, perl = TRUE)
    if (ipsi_match > 0) {
      match_str <- regmatches(label_lower, ipsi_match)
      mag <- sub(".*?(\\d{2})$", "\\1", match_str)
      return(resolve_ipsi(mag))
    }
    if (grepl("ipsi", label_lower, fixed = TRUE)) {
      return(resolve_ipsi(NA_character_))
    }

    default
  }

  vapply(labels, detect_single, character(1), USE.NAMES = FALSE)
}

# label and plotting for causal forest models -----------------------------
#' works with transform to original scale
#' @keywords internal
determine_transformation <- function(var_name) {
  list(
    was_log_transformed = grepl("_log_", var_name),
    was_z_transformed = grepl("_z$", var_name)
  )
}

#' @keywords internal
get_original_value <- function(var_name, split_value, original_df) {
  if (is.null(original_df)) {
    return(NULL)
  }

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
  if (is.null(original_df)) {
    return(NULL)
  }

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

#' Get outcome transformation information for inverse transformation
#' @keywords internal
get_outcome_transformation_info <- function(model_name, original_df) {
  if (is.null(original_df)) {
    return(NULL)
  }

  # extract outcome name from model name (e.g., "model_t2_belong_z" -> "t2_belong_z")
  outcome_name <- sub("^model_", "", model_name)

  # detect if this is a flipped outcome model
  has_r_suffix <- grepl("_r$", outcome_name)

  # remove _r suffix for searching in original_df (which contains unflipped data)
  outcome_name_for_search <- if (has_r_suffix) {
    sub("_r$", "", outcome_name)
  } else {
    outcome_name
  }

  # detect transformation type (use cleaned name for search)
  has_z_suffix <- grepl("_z$", outcome_name_for_search)
  has_log_prefix <- grepl("_log_", outcome_name_for_search)

  # build list of possible original variable names
  candidates <- character()

  # start with the outcome name (cleaned of _r suffix)
  candidates <- c(candidates, outcome_name_for_search)

  # remove _z suffix if present
  if (has_z_suffix) {
    var_no_z <- sub("_z$", "", outcome_name_for_search)
    candidates <- c(candidates, var_no_z)
  }

  # for log-transformed variables, try to find the original
  if (has_log_prefix) {
    # first try keeping the log prefix (e.g., "t2_log_charity_donate_z" -> "t2_log_charity_donate")
    candidates <- c(candidates, var_no_z)

    # then try removing log prefix (e.g., "t2_log_charity_donate_z" -> "t2_charity_donate")
    var_no_log <- gsub("_log_", "_", var_no_z)
    candidates <- c(candidates, var_no_log)

    # also try without time prefix
    var_no_prefix <- sub("^t[0-9]+_", "", var_no_log)
    candidates <- c(candidates, var_no_prefix)

    # try without time prefix but keeping log
    var_no_prefix_with_log <- sub("^t[0-9]+_", "", var_no_z)
    candidates <- c(candidates, var_no_prefix_with_log)
  }

  # try to find the original variable
  orig_var <- NULL
  for (candidate in unique(candidates)) {
    if (candidate %in% names(original_df)) {
      orig_var <- candidate
      break
    }
  }

  if (is.null(orig_var)) {
    return(NULL)
  }

  # get the original data
  orig_data <- original_df[[orig_var]]

  # calculate transformation parameters
  result <- list(
    outcome_name = outcome_name,
    original_var = orig_var,
    has_z = has_z_suffix,
    has_log = has_log_prefix,
    is_flipped = has_r_suffix
  )

  if (has_z_suffix) {
    # for z-transformed variables, we need mean and sd
    if (has_log_prefix) {
      # if log+z transformed, the original variable should contain log(x+1)
      # however, the data in original_df might be from a subset or different timepoint
      result$log_mean <- mean(orig_data, na.rm = TRUE)
      result$log_sd <- sd(orig_data, na.rm = TRUE)
      result$log_offset <- 1

      # check if we have stored transformation info as attributes
      stored_log_mean <- attr(orig_data, "log_mean")
      stored_log_sd <- attr(orig_data, "log_sd")

      if (!is.null(stored_log_mean) && !is.null(stored_log_sd)) {
        result$log_mean <- stored_log_mean
        result$log_sd <- stored_log_sd
      } else if (grepl("charity|donat", outcome_name, ignore.case = TRUE)) {
        # check if the log_mean seems too low for charity data
        if (result$log_mean < 6) {
          # the log_mean seems too low (implies mean < $400)
          # this is likely subset data - use more realistic values
          cli::cli_alert_info(
            "Detected low values for {outcome_name} (mean ~${round(exp(result$log_mean)-1)}). " %+%
              "Using population-based estimates for dollar calculations."
          )
          # use realistic population statistics for charity donations
          result$log_mean_display <- 6.96 # log(1048 + 1)
          result$use_display_mean <- TRUE
        }
      } else if (grepl("income|household.*inc", outcome_name, ignore.case = TRUE) && result$log_mean < 10) {
        # household income seems too low
        cli::cli_alert_info(
          "Detected low values for {outcome_name}. Using population-based estimates."
        )
        result$log_mean_display <- 11.0 # ~$60k
        result$use_display_mean <- TRUE
      }
    } else {
      # just z-transformed
      result$orig_mean <- mean(orig_data, na.rm = TRUE)
      result$orig_sd <- sd(orig_data, na.rm = TRUE)
    }
  }

  # for display purposes, get the original data stats
  if (has_log_prefix && result$original_var == orig_var) {
    # if we found a log variable, transform back to get actual values
    actual_values <- exp(orig_data) - 1
    result$display_mean <- mean(actual_values, na.rm = TRUE)
    result$display_sd <- sd(actual_values, na.rm = TRUE)
    result$display_min <- min(actual_values, na.rm = TRUE)
    result$display_max <- max(actual_values, na.rm = TRUE)
  } else {
    result$display_mean <- mean(orig_data, na.rm = TRUE)
    result$display_sd <- sd(orig_data, na.rm = TRUE)
    result$display_min <- min(orig_data, na.rm = TRUE)
    result$display_max <- max(orig_data, na.rm = TRUE)
  }

  return(result)
}

#' Format number with minimal decimal places
#' @keywords internal
format_minimal_decimals <- function(x, max_decimals = NULL) {
  if (is.null(x) || is.na(x)) {
    return(NULL)
  }

  abs_x <- abs(x)

  # determine decimal places based on magnitude
  if (is.null(max_decimals)) {
    if (abs_x < 1) {
      decimals <- 3 # for values < 1, use 3 decimals
    } else if (abs_x < 10) {
      decimals <- 2 # for values 1-10, use 2 decimals
    } else if (abs_x < 100) {
      decimals <- 1 # for values 10-100, use 1 decimal
    } else {
      decimals <- 0 # for values >= 100, use no decimals
    }
  } else {
    decimals <- max_decimals
  }

  # check if the number is effectively an integer when rounded to the desired decimals
  if (decimals == 0 || abs(x - round(x, decimals)) < 1e-10) {
    return(format(round(x, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
  }

  # format with the determined number of decimals
  return(format(round(x, decimals), nsmall = decimals, big.mark = ",", scientific = FALSE))
}

#' Detect variable units from name
#' @keywords internal
detect_variable_units <- function(var_name) {
  # monetary variables
  if (grepl("donat|income|_inc|spend|cost|price|wage|salary", var_name, ignore.case = TRUE)) {
    return(list(type = "monetary", symbol = "$", name = "dollars"))
  }

  # time variables - will convert hours to minutes
  if (grepl("_hours_", var_name)) {
    return(list(type = "time", symbol = "", name = "minutes", scale_factor = 60))
  }

  # generic
  return(list(type = "generic", symbol = "", name = "units"))
}

#' @keywords internal
transform_label <- function(label, label_mapping = NULL, options = list()) {
  # coerce each flag to a single TRUE/FALSE
  remove_tx_prefix <- isTRUE(options$remove_tx_prefix)
  remove_z_suffix <- isTRUE(options$remove_z_suffix)
  remove_underscores <- isTRUE(options$remove_underscores)
  use_title_case <- isTRUE(options$use_title_case)
  quiet <- isTRUE(options$quiet)

  original_label <- label

  # 1) explicit mappings
  if (!is.null(label_mapping)) {
    # first try exact match
    if (label %in% names(label_mapping)) {
      new_label <- label_mapping[[label]]
      if (!quiet) cli::cli_alert_info("Mapped label (exact): {label} -> {new_label}")
      return(new_label)  # return early for exact matches
    }
    
    # if no exact match, try pattern matching
    for (pat in names(label_mapping)) {
      if (grepl(pat, label, fixed = TRUE)) {
        repl <- label_mapping[[pat]]
        label <- gsub(pat, repl, label, fixed = TRUE)
        if (!quiet) cli::cli_alert_info("Mapped label (pattern): {pat} -> {repl}")
      }
    }
  }

  # 2) strip trailing numeric-range suffix
  label <- sub(" - \\(.*\\]$", "", label)

  # 3) default transforms if unmapped
  if (identical(label, original_label)) {
    if (remove_tx_prefix) label <- sub("^t[0-9]+_", "", label)
    if (remove_z_suffix) {
      label <- sub("_z$", "", label)
      label <- sub("_r$", "", label)  # also remove _r suffix for reversed variables
    }
    if (remove_underscores) label <- gsub("_", " ", label, fixed = TRUE)

    if (use_title_case) {
      # convert to title case and enforce specific uppercase acronyms
      label <- tools::toTitleCase(label)
      label <- gsub("Nz", "NZ", label, fixed = TRUE)
      label <- gsub("Sdo", "SDO", label, fixed = TRUE)
      label <- gsub("Rwa", "RWA", label, fixed = TRUE)
    }
  }

  # log if changed
  if (!identical(label, original_label)) {
    if (!quiet) cli::cli_alert_info("Transformed label: {original_label} -> {label}")
  }

  label
}


# helper operator for NULL handling
`%||%` <- function(x, y) if (is.null(x)) y else x



#' @keywords internal
s <- function(results_df, original_df, label_mapping = NULL) {
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
    z_value <- qnorm(0.975) # For 95% CI
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
      SE_original <- SE_standardized # Assume SE is the same

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


#' Transform a variable name into a human-readable label, preserving acronyms
#'
#' This function applies explicit mappings, strips numeric-range suffixes,
#' removes time-prefixes and z-suffixes, replaces underscores, and converts
#' to title case while preserving NZ, SDO, and RWA acronyms.
#'
#' @param var_name Character; the original variable name
#' @param label_mapping Optional named list for explicit mappings
#' @param remove_tx_prefix Logical; remove leading 't0_' etc.
#' @param remove_z_suffix Logical; remove trailing '_z'
#' @param use_title_case Logical; convert to title case
#' @param remove_underscores Logical; replace underscores with spaces
#' @param expand_acronyms Logical; expand common acronyms (RWA, SDO, PWI, NZSEI)
#'   to their full names while retaining the acronym in parentheses. Defaults to FALSE.
#'
#' @return A character scalar of the transformed label, or NA if input missing
#'
#' @examples
#' # Basic usage with mapping
#' transform_var_name("t2_rwa_z", label_mapping = list(rwa = "Right-Wing Authoritarianism"))
#'
#' # Expand common acronyms without an explicit mapping
#' transform_var_name("baseline RWA", expand_acronyms = TRUE)
#' # => "Baseline Right-Wing Authoritarianism (RWA)"
#'
#' # Mapping takes precedence; expansion still applies to remaining acronyms
#' transform_var_name("PWI overall", label_mapping = list(PWI = "Personal Well-Being Index"), expand_acronyms = TRUE)
#' @keywords internal
transform_var_name <- function(var_name, label_mapping = NULL,
                               remove_tx_prefix = TRUE,
                               remove_z_suffix = TRUE,
                               use_title_case = TRUE,
                               remove_underscores = TRUE,
                               expand_acronyms = FALSE) {
  if (is.na(var_name) || length(var_name) == 0) {
    return(NA_character_)
  }

  display_name <- var_name

  # strip numeric-range suffix
  display_name <- sub(" - \\(.*\\]$", "", display_name)

  # remove model_ prefix
  if (startsWith(display_name, "model_")) {
    display_name <- sub("^model_", "", display_name)
  }

  # check if this is a reduced (_r) variable
  is_reduced <- grepl("_r$", display_name)
  if (is_reduced) {
    # remove _r suffix for processing
    display_name <- sub("_r$", "", display_name)
  }

  # explicit mapping
  if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
    mapped_label <- label_mapping[[display_name]]
    if (is_reduced) {
      mapped_label <- paste0("(reduced) ", mapped_label)
    }
    cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
    return(mapped_label)
  }

  # t0_ → t2_ fallback
  if (startsWith(display_name, "t0_")) {
    t2_var <- sub("^t0_", "t2_", display_name)
    if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
      mapped_label <- label_mapping[[t2_var]]
      if (is_reduced) {
        mapped_label <- paste0("(reduced) ", mapped_label)
      }
      cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
      return(mapped_label)
    }
  }

  # handle _log transformation
  has_log <- grepl("_log_", display_name)
  if (has_log) {
    # extract the part after _log_ to identify what was logged
    display_name <- gsub("_log_", "_", display_name)
  }

  # strip prefixes/suffixes/underscores
  if (remove_tx_prefix) display_name <- sub("^t[0-9]+_", "", display_name)
  if (remove_z_suffix) display_name <- sub("_z$", "", display_name)
  # remove _l suffix
  display_name <- sub("_l$", "", display_name)
  if (remove_underscores) display_name <- gsub("_", " ", display_name, fixed = TRUE)

  # title-case and preserve acronyms
  if (use_title_case) {
    display_name <- tools::toTitleCase(display_name)
    display_name <- gsub("Nz", "NZ", display_name, fixed = TRUE)
    display_name <- gsub("Sdo", "SDO", display_name, fixed = TRUE)
    display_name <- gsub("Rwa", "RWA", display_name, fixed = TRUE)
    # capitalize NZSEI and NZDEP
    display_name <- gsub("Nzsei", "NZSEI", display_name, fixed = TRUE)
    display_name <- gsub("Nzdep", "NZDEP", display_name, fixed = TRUE)
  }

  # optional acronym expansion (append meanings while retaining acronym)
  if (isTRUE(expand_acronyms)) {
    expand_one <- function(txt, pat, long) {
      gsub(paste0("\\b", pat, "\\b"), paste0(long, " (", pat, ")"), txt)
    }
    # user-provided acronyms first
    acr_map <- getOption("margot.boilerplate.acronyms", NULL)
    if (is.list(acr_map) || is.vector(acr_map)) {
      for (nm in names(acr_map)) {
        display_name <- expand_one(display_name, nm, acr_map[[nm]])
      }
    }
    # defaults
    display_name <- expand_one(display_name, "RWA", "Right-Wing Authoritarianism")
    display_name <- expand_one(display_name, "SDO", "Social Dominance Orientation")
    display_name <- expand_one(display_name, "PWI", "Personal Well-Being Index")
    display_name <- expand_one(display_name, "NZSEI", "Occupational Prestige Index")
  }

  # add (log) suffix if variable was log-transformed
  if (has_log) {
    display_name <- paste0(display_name, " (log)")
  }

  # add (reduced) prefix if variable was reversed
  if (is_reduced) {
    display_name <- paste0("(reduced) ", display_name)
  }

  if (!identical(display_name, var_name)) {
    cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
  }

  display_name
}

# helper for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x


# read utilities ----------------------------------------------------------

#' @title Read Data Frame from Parquet File in a Specified Directory (Deprecated)
#'
#' @description This function is deprecated and will be removed in future releases.
#' For reading data frames, consider using the `here_read_qs` function.
#'
#' @param name Character string specifying the name of the Parquet file to be read.
#'
#' @examples
#' \dontrun{
#' my_df <- here_read_arrow("my_dataset")
#' }
#'
#' @export
#' @keywords internal
here_read_arrow <- function(name) {
  .Deprecated("here_read_qs")
  message("here_read_arrow is deprecated and will be removed in a future release. Please use here_read_qs instead.")
  # function logic
  df <- arrow::read_parquet(here::here(name, ".parquet"))
  return(df)
}


#' @title Save Data Frame to Parquet File in a Specified Directory (Deprecated)
#'
#' @description This function is deprecated and will be removed in future releases.
#' For saving data frames, consider using the `here_save_qs` function.
#'
#' @param df Data frame to be saved.
#' @param name Character string specifying the base name of the file.
#'
#' @examples
#' \dontrun{
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save_arrow(my_df, "my_saved_dataframe")
#' }
#'
#' @export
#' @keywords internal
here_save_arrow <- function(df, name) {
  .Deprecated("here_save_qs", package = "margot")
  message("here_save_arrow is deprecated and will be removed in a future release. Please use here_save_qs instead.")
  # function logic
  arrow::write_parquet(df, here::here(name, ".parquet"))
}




# causal effects tables ---------------------------------------------------
#' Group and Annotate Treatment Effect Estimates
#'
#' This function arranges and annotates a data frame based on specified
#' types of treatment effect estimates (RR or RD). It supports a variety of sorting
#' options including alphabetical, magnitude (ascending or descending), E-value bound
#' (ascending or descending), custom order, and a default alias for backward compatibility.
#' It also handles original scale estimates when available.
#'
#' @param df Data frame containing the variables of interest, or a list containing
#'   the results data frame and label mapping from transform_to_original_scale().
#' @param type Type of treatment effect to analyze. One of 'RR' (Risk Ratio) or
#'   'RD' (Risk Difference). Defaults to 'RD'.
#' @param order Sorting option for outcomes. Options are:
#'   \itemize{
#'     \item 'alphabetical': sort by outcome name (A–Z)
#'     \item 'magnitude_desc': sort by absolute effect size, descending (default for 'magnitude')
#'     \item 'magnitude_asc': sort by absolute effect size, ascending
#'     \item 'evaluebound_desc': sort by E-value bound, descending
#'     \item 'evaluebound_asc': sort by E-value bound, ascending
#'     \item 'custom': user-defined order (requires custom_order)
#'     \item 'default': alias for 'magnitude_desc' (deprecated)
#'   }
#'   Default is 'default'.
#' @param custom_order Character vector specifying a custom outcome ordering,
#'   used when order = 'custom'. Must contain all outcomes exactly once.
#'
#' @return A data frame arranged according to `order`, annotated with:
#'   \itemize{
#'     \item Estimate category (positive, negative, not reliable)
#'     \item Formatted label for the effect and confidence interval
#'     \item Optional original-scale label if _original columns are present
#'   }
#'
#' @details
#' The function detects whether `df` is a list output from transform_to_original_scale()
#' and extracts `results_df` and `label_mapping` accordingly. It then ensures an `outcome`
#' column, applies any label mapping, and sorts based on the chosen `order`. New options
#' 'magnitude_desc' and 'magnitude_asc' sort by absolute effect size; 'evaluebound_desc'
#' and 'evaluebound_asc' sort by the E-Value bound; 'alphabetical' sorts by outcome
#' name; 'custom' respects a user-provided vector; 'default' is an alias for 'magnitude_desc'.
#'
#' @examples
#' # descending magnitude (default for 'default')
#' result_df <- group_tab(df = analysis_df, order = "default")
#'
#' # ascending magnitude
#' result_df <- group_tab(df = analysis_df, order = "magnitude_asc")
#'
#' # strongest E-value bound first
#' result_df <- group_tab(df = analysis_df, order = "evaluebound_desc")
#'
#' # alphabetical
#' result_df <- group_tab(df = analysis_df, order = "alphabetical")
#'
#' # custom ordering
#' custom_order <- c("Outcome3", "Outcome1", "Outcome2")
#' result_df <- group_tab(df = analysis_df, order = "custom", custom_order = custom_order)
#'
#' @importFrom dplyr arrange desc mutate slice
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @keywords internal
group_tab <- function(
    df,
    type = c("RD", "RR"),
    order = c(
      "alphabetical",
      "magnitude_desc",
      "magnitude_asc",
      "evaluebound_desc",
      "evaluebound_asc",
      "custom",
      "default"
    ),
    custom_order = NULL) {
  # load dplyr verbs
  require(dplyr)

  # match arguments
  type <- match.arg(type)
  order <- match.arg(order)

  # alias old magnitude and default to magnitude_desc
  if (order %in% c("default")) {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  # handle list input
  if (is.list(df) && "results_df" %in% names(df)) {
    results_df <- df$results_df
    label_mapping <- df$label_mapping
  } else {
    results_df <- df
    label_mapping <- NULL
  }

  # ensure outcome column
  if (!"outcome" %in% names(results_df) && !is.null(rownames(results_df))) {
    results_df <- results_df %>% tibble::rownames_to_column(var = "outcome")
  } else if (!"outcome" %in% names(results_df)) {
    stop("No 'outcome' column or row names to convert.")
  }

  # apply label mapping
  if (!is.null(label_mapping)) {
    results_df <- results_df %>% mutate(outcome = dplyr::recode(outcome, !!!label_mapping))
  }

  # columns for sorting - check for new column types first
  effect_col <- NULL
  if (type == "RR") {
    # check for RR columns
    if ("E[Y(1)]/E[Y(0)]" %in% names(results_df)) {
      effect_col <- "E[Y(1)]/E[Y(0)]"
    }
  } else {
    # check for RD columns (including new types)
    possible_cols <- c("ATE", "ATT", "ATC", "ATO", "E[Y(1)]-E[Y(0)]")
    for (col in possible_cols) {
      if (col %in% names(results_df)) {
        effect_col <- col
        break
      }
    }
  }

  # fallback to traditional columns if nothing found
  if (is.null(effect_col)) {
    effect_col <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
  }

  ev_bound <- "E_Val_bound"

  # apply ordering
  results_df <- switch(order,
    alphabetical = results_df %>% arrange(outcome),
    magnitude_desc = results_df %>% arrange(desc(abs(!!sym(effect_col)))),
    magnitude_asc = results_df %>% arrange(abs(!!sym(effect_col))),
    evaluebound_desc = results_df %>% arrange(desc(!!sym(ev_bound))),
    evaluebound_asc = results_df %>% arrange(!!sym(ev_bound)),
    custom = {
      if (is.null(custom_order)) stop("custom_order must be provided for 'custom' order")
      results_df %>% slice(match(custom_order, outcome))
    }
  )

  # annotate estimates
  results_df <- results_df %>% mutate(
    Estimate = factor(
      if (type == "RR") {
        ifelse(.data[[effect_col]] > 1 & `2.5 %` > 1,
          "positive",
          ifelse(.data[[effect_col]] < 1 & `97.5 %` < 1,
            "negative",
            "not reliable"
          )
        )
      } else {
        ifelse(.data[[effect_col]] > 0 & `2.5 %` > 0,
          "positive",
          ifelse(.data[[effect_col]] < 0 & `97.5 %` < 0,
            "negative",
            "not reliable"
          )
        )
      }
    ),
    estimate_lab = if (type == "RR") {
      paste0(
        round(.data[[effect_col]], 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]"
      )
    } else {
      paste0(
        round(.data[[effect_col]], 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]"
      )
    }
  )

  # add original-scale label if present
  if (paste0(effect_col, "_original") %in% names(results_df)) {
    results_df <- results_df %>% mutate(
      estimate_lab_original = paste0(
        round(.data[[paste0(effect_col, "_original")]], 3), " (",
        round(.data[["2.5 %_original"]], 3), "-",
        round(.data[["97.5 %_original"]], 3), ")"
      )
    )
  }

  results_df
}



#' Tabulate Marginal Effects with E-Values
#'
#' This function processes simulation results to tabulate marginal effects along with E-values,
#' providing a summary suited for reporting. It supports both risk difference (RD) and risk ratio (RR)
#' types of estimates and handles continuous and categorical treatment variables.
#'
#' @param x A data frame or matrix containing simulation results to be processed.
#' @param new_name A new name to assign to the output row, typically describing the variable or model.
#' @param delta The assumed smallest worthwhile effect, used for E-value calculations.
#' @param sd The standard deviation of the effect estimate, used for E-value calculations.
#' @param type Character vector specifying the scale of effect size, either "RD" or "RR".
#'        This parameter determines how the effects are calculated and presented.
#' @param continuous_X Logical indicating whether the treatment variable X is continuous.
#'        If TRUE, adjusts row names based on the type parameter.
#'
#' @return A data frame with the specified new_name as a row name. The data frame includes
#'         effect estimates, confidence intervals, E-values, and other relevant statistics formatted
#'         for easy reporting.
#'
#' @examples
#' # Assuming you have results from a simulation or model in `results_df`
#' tabulated_results <- tab_engine_marginal(
#'   x = results_df,
#'   new_name = "Treatment Effect",
#'   delta = 1,
#'   sd = 0.2,
#'   type = "RD"
#' ) # Corrected 'scale' to 'type'
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom EValue evalues.OLS evalues.RR
#' @keywords internal
tab_engine_marginal <- function(x, new_name, delta = 1, sd = 1, type = c("RD", "RR"), continuous_X = FALSE) {
  type <- match.arg(type, choices = c("RD", "RR"))
  x <- as.data.frame(x)

  if (continuous_X) {
    rownames(x) <- type
  }

  out <- x %>%
    dplyr::filter(row.names(x) == type) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 4))

  if (type == "RD") {
    out <- out %>%
      dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
  } else {
    out <- out %>%
      dplyr::rename("E[Y(1)]/E[Y(0)]" = Estimate)
  }

  if (!"outcome" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(outcome = new_name)
  } else {
    out$outcome <- new_name # Just update the existing 'outcome' column instead of adding a new one
  }

  out <- dplyr::select(out, outcome, everything())

  if (type == "RD") {
    out <- out %>%
      dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
    evalout <- as.data.frame(round(EValue::evalues.OLS(out[1, "E[Y(1)]-E[Y(0)]"], se = out$standard_error, sd = sd, delta = delta, true = 0), 3))
  } else {
    evalout <- as.data.frame(round(EValue::evalues.RR(out[1, "E[Y(1)]/E[Y(0)]"], lo = out[1, "2.5 %"], hi = out[1, "97.5 %"], true = 1), 3))
  }

  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 %>% dplyr::select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E_Value", "E_Val_bound")

  out <- cbind.data.frame(out, evalout3)
  return(out)
}





# causal forest helpers ---------------------------------------------------

#' Group Results by Comparison
#'
#' @description
#' Groups the results of multi-arm causal forest analysis by comparison levels.
#'
#' @param results_list A list of results from margot_multi_arm_causal_forest.
#'
#' @return A list of grouped results by comparison levels.
#'
#' @keywords internal
group_results_by_comparison <- function(results_list) {
  # extract all custom tables
  custom_tables <- lapply(results_list, function(x) x$custom_table)

  # get all unique comparisons (exposure levels)
  all_comparisons <- unique(unlist(lapply(custom_tables, function(table) {
    if (!is.null(table) && nrow(table) > 0) {
      sub(".*? - ", "", rownames(table))
    } else {
      character(0)
    }
  })))

  # initialise a list to store grouped results
  grouped_results <- vector("list", length(all_comparisons))
  names(grouped_results) <- all_comparisons

  # for each comparison, combine results from all outcomes
  for (comparison in all_comparisons) {
    comparison_results <- lapply(names(custom_tables), function(outcome) {
      table <- custom_tables[[outcome]]
      if (!is.null(table) && nrow(table) > 0) {
        row_index <- which(sub(".*? - ", "", rownames(table)) == comparison)
        if (length(row_index) > 0) {
          result <- table[row_index, , drop = FALSE]
          rownames(result) <- sub(" - .*$", "", rownames(result)) # remove comparison from rowname
          result
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    # remove null entries and combine
    comparison_results <- do.call(rbind, comparison_results[!sapply(comparison_results, is.null)])
    if (nrow(comparison_results) > 0) {
      grouped_results[[comparison]] <- comparison_results
    }
  }

  # remove any null entries from grouped_results
  grouped_results <- grouped_results[!sapply(grouped_results, is.null)]

  return(grouped_results)
}


#' Create Tau Hat Plot
#'
#' @description
#' Creates a histogram plot of tau hat values for each treatment comparison.
#'
#' @param tau_hat A matrix of estimated treatment effects.
#' @param outcome A character string specifying the name of the outcome variable.
#'
#' @return A ggplot object representing the distribution of tau hat values.
#'
#' @importFrom ggplot2 ggplot geom_histogram theme_minimal labs facet_wrap
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
create_tau_hat_plot <- function(tau_hat, outcome) {
  tau_hat_df <- as.data.frame(tau_hat)
  tau_hat_df_long <- tau_hat_df %>%
    tidyr::pivot_longer(cols = everything(), names_to = "comparison", values_to = "tau_value")

  ggplot2::ggplot(tau_hat_df_long, ggplot2::aes(x = tau_value, fill = comparison)) +
    ggplot2::geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Distribution of tau.hat for", outcome),
      x = "tau.hat", y = "Count"
    ) +
    ggplot2::facet_wrap(~comparison, scales = "free_y")
}



#' @keywords internal
margot_batch_glm <- function(
    data,
    outcome_vars,
    exposure_formula,
    terms = NULL,
    label_mapping = NULL,
    x_label = NULL,
    y_label = NULL,
    color_label = NULL,
    y_limits = NULL,
    weights = NULL,
    family_list = NULL,
    plot_params = list(
      show_data = TRUE,
      jitter = 0.4,
      dot_alpha = 0.025,
      alpha = 3,
      show_ci = TRUE,
      ci_style = "dash",
      colors = "us",
      dot_size = 2
    ),
    facet_params = NULL,
    theme_params = theme_bw()) {
  #  validation
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (length(outcome_vars) == 0) stop("Must provide at least one outcome variable")

  # initialize storage lists
  predictions_list <- list()
  plots_list <- list()

  # process each outcome variable
  for (outcome_var in outcome_vars) {
    # Create formula
    formula_str <- paste(outcome_var, exposure_formula)
    formula_obj <- as.formula(formula_str)

    # family for this outcome
    current_family <- if (!is.null(family_list) && outcome_var %in% names(family_list)) {
      family_list[[outcome_var]]
    } else {
      gaussian() # Default to gaussian if no family specified
    }

    cli::cli_alert_info("Processing model for outcome: {outcome_var} using {current_family$family} family")

    # fit model with appropriate family and weights if specified
    if (!is.null(weights)) {
      model <- glm(formula_obj, data = data, family = current_family, weights = weights)
      cli::cli_alert_info("Using weighted GLM for {outcome_var}")
      # Generate predictions with weights
      pred <- ggeffects::ggpredict(model, terms = terms, weights = weights)
    } else {
      model <- glm(formula_obj, data = data, family = current_family)
      # Generate predictions without weights
      pred <- ggeffects::ggpredict(model, terms = terms)
    }

    predictions_list[[outcome_var]] <- pred

    # transform labels
    outcome_label <- transform_label(
      outcome_var,
      label_mapping = label_mapping,
      options = list(
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        remove_underscores = TRUE,
        use_title_case = TRUE
      )
    )

    # make base plot
    p <- plot(pred,
      show_data = plot_params$show_data,
      jitter = plot_params$jitter,
      dot_alpha = plot_params$dot_alpha,
      alpha = plot_params$alpha,
      show_ci = plot_params$show_ci,
      ci_style = plot_params$ci_style,
      colors = plot_params$colors
    ) +
      geom_point(aes(x = x, y = predicted),
        color = "dodgerblue",
        size = plot_params$dot_size,
        alpha = 1
      )

    # add labels
    p <- p + labs(
      x = x_label %||% terms,
      y = y_label %||% outcome_label,
      title = outcome_label
    )

    # add theme
    p <- p + theme_params

    # set y-axis limits based on family type and user input
    if (!is.null(y_limits)) {
      p <- p + scale_y_continuous(limits = y_limits)
    } else if (current_family$family == "binomial") {
      p <- p + scale_y_continuous(limits = c(0, 1))
    } else {
      # Let ggplot2 determine the limits for gaussian family
      p <- p + scale_y_continuous()
    }

    # add faceting if specified
    if (!is.null(facet_params)) {
      if (!is.null(facet_params$facets)) {
        p <- p + facet_wrap(facet_params$facets,
          scales = facet_params$scales %||% "fixed",
          ncol = facet_params$ncol,
          nrow = facet_params$nrow
        )
      }
    }

    plots_list[[outcome_label]] <- p
  }

  return(list(
    predictions = predictions_list,
    plots = plots_list
  ))
}




#' Create transition matrix for state transitions
#'
#' To satisify the positivity assumption of causal inference, we must ensure that the exposure occurs in the data. This function computes a transition matrix for a given state variable across subjects, tracking changes between consecutive observations. The function handles both numeric and factor state variables, excluding NA values in the transition count.
#'
#' @param data A data frame containing the observations.
#' @param state_var The name of the state variable column in `data` as a string. This variable tracks the state changes to be analyzed.
#' @param id_var The name of the identifier variable column in `data` as a string. This variable distinguishes between different subjects or entities.
#'
#' @return A matrix indicating the number of transitions between states. The rows represent the initial state ('from'), and the columns represent the subsequent state ('to'). Diagonal entries indicate the number of times the state did not change, while off-diagonal entries indicate transitions from one state to another.
#'
#' @examples
#' df <- read.table(header = TRUE, text = "
#' id wave year_measured religion_believe_god
#' 3 0 1 0
#' 3 1 1 1
#' 4 0 1 0
#' 4 1 1 1
#' 5 0 1 1
#' 5 1 1 0")
#'
#' transition_matrix <- create_transition_matrix(df, "religion_believe_god", "id")
#' print(transition_matrix)
#'
#' @note This function explicitly excludes NA values from the transition matrix calculation. It treats numeric state variables by converting them to factors, ensuring a consistent analysis approach for both numeric and factor types.
#'
#' @keywords internal
create_transition_matrix <- function(data, state_var, id_var) {
  # function body as provided
}

# function to generate a square matrix of state transitions, excluding NAs, for numeric or factor state variables
create_transition_matrix <- function(data, state_var, id_var) {
  # ensure data is ordered by id and then time/wave for proper transition tracking
  data <- data[order(data[[id_var]], data$wave), ]

  # calculate lagged states to identify transitions, ensuring alignment within each subject
  data$prev_state <- ave(data[[state_var]], data[[id_var]], FUN = function(x) c(NA, head(x, -1)))

  # remove rows with NA in the current or previous state to avoid misalignment
  valid_rows <- !is.na(data$prev_state) & !is.na(data[[state_var]])
  data <- data[valid_rows, ]

  # convert states to factors if numeric, ensuring all states are represented
  if (is.numeric(data[[state_var]])) {
    all_states <- sort(unique(c(data[[state_var]], data$prev_state)))
    data[[state_var]] <- factor(data[[state_var]], levels = all_states)
    data$prev_state <- factor(data$prev_state, levels = all_states)
  } else {
    # ensure consistency of factor levels for previous and current states
    levels_union <- union(levels(factor(data[[state_var]])), levels(factor(data$prev_state)))
    data[[state_var]] <- factor(data[[state_var]], levels = levels_union)
    data$prev_state <- factor(data$prev_state, levels = levels_union)
  }

  # build the transition matrix
  transition_matrix <- table(from = data$prev_state, to = data[[state_var]])

  return(transition_matrix)
}



#' Strict All-or-Nothing Censoring for Longitudinal Data
#'
#' @description
#' This function processes wide-format longitudinal data with multiple time points:
#'
#' - For each wave t < final wave:
#'   - If wave t+1 **has exposure columns**, a participant remains "not lost" at wave t
#'     only if *all* exposures at wave t+1 are present (no missing). Otherwise, they are censored at wave t.
#'   - If wave t+1 **has no exposures** (i.e., final wave is purely outcomes),
#'     we require *all* final-wave outcomes to be present. If *any* final-wave outcome is missing,
#'     the participant is censored from wave t onward.
#'
#' Censoring sets all future waves to `NA`, and once censored, participants remain censored.
#'
#' @param df_wide A wide-format dataframe with columns like t0_X, t1_X, t2_X, etc.
#' @param exposure_vars Character vector of all exposure names (e.g. c("aaron_antagonism", "aaron_disinhibition", ...)).
#' @param ordinal_columns Character vector of ordinal (factor) variables to be dummy-coded.
#' @param continuous_columns_keep Numeric columns you do NOT want to scale (e.g. if they must remain in original units).
#' @param scale_exposure If FALSE, do not scale exposures; if TRUE, exposures are also scaled.
#' @param not_lost_in_following_wave Name for the "not lost" indicator (default "not_lost_following_wave").
#' @param lost_in_following_wave Name for the "lost" indicator (default "lost_following_wave").
#' @param remove_selected_columns If TRUE, remove original columns after dummy-coding ordinal columns.
#' @param time_point_prefixes Optional vector of wave prefixes (like c("t0","t1","t2")); if NULL, we auto-detect via regex.
#' @param time_point_regex Regex used to detect wave prefixes if `time_point_prefixes` is NULL.
#' @param save_observed_y If FALSE, set any missing final-wave outcomes to NA. If TRUE, keep partial final-wave outcomes.
#'
#' @return A processed dataframe, with strict all-or-nothing censoring on exposures in earlier waves,
#'         and outcome-based censoring for the final wave if it lacks exposures.
#'
#' @details
#' **Core Logic**
#' For wave t from 0 to T-2 (i.e., up to the penultimate wave):
#' \preformatted{
#'   needed_exposures <- paste0(t+1, "_", exposure_vars)
#'   not_lost[t] = 1 if rowSums(!is.na(needed_exposures)) == length(needed_exposures)
#'                else 0
#'
#'   if not_lost[t] = 0, set waves t+1..T to NA
#' }
#' If wave t+1 is the final wave and it has no exposures, we fallback to the final wave's outcome columns.
#' Then "not_lost[t] = 1 if *all* final-wave outcomes are present, else 0".
#'
#' This is a "strict" approach: if *any* exposure is missing at wave t+1, we censor from wave t onward.
#'
#' @keywords internal
.strict_exposure_outcome_censoring <- function(
    df_wide,
    exposure_vars,
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    scale_exposure = FALSE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = "lost_following_wave",
    remove_selected_columns = TRUE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE) {
  # explicit package call
  cli::cli_h1("Strict All-or-Nothing Censoring for Longitudinal Data")

  # step 1. Identify wave prefixes
  if (is.null(time_point_prefixes)) {
    if (is.null(time_point_regex)) {
      time_point_regex <- "^(t\\d+)_.*$"
    }
    matched_cols <- grep(time_point_regex, colnames(df_wide), value = TRUE)
    time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
    time_points <- time_points[order(as.numeric(gsub("t", "", time_points)))]
  } else {
    time_points <- time_point_prefixes
  }
  num_time_points <- length(time_points)
  cli::cli_alert_info(
    "Identified {num_time_points} time points: {paste(time_points, collapse = ', ')}"
  )

  # create a working copy of the data
  df_wide_use <- df_wide

  # step 2. Final wave outcomes
  final_wave <- time_points[num_time_points]
  final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
  outcome_vars <- sub(paste0("^", final_wave, "_"), "", final_wave_cols)
  cli::cli_alert_info("Using all variables in the final wave as outcomes.")

  # STEP 1: Create not_lost_in_following_wave for each wave t < final wave
  cli::cli_h2("Step 1: Creating 'not_lost_in_following_wave' with strict exposure (or outcome) logic")

  # function to check if all columns are present for a row
  check_all_present <- function(df, columns) {
    rowSums(!is.na(df[, columns, drop = FALSE])) == length(columns)
  }

  for (i in seq_len(num_time_points - 1)) {
    t_i <- time_points[i]
    t_i_plus1 <- time_points[i + 1]

    not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)

    # Exposures for wave t_i_plus1
    needed_exposures <- paste0(t_i_plus1, "_", exposure_vars)
    needed_exposures <- needed_exposures[needed_exposures %in% names(df_wide_use)]

    if (length(needed_exposures) == 0) {
      # If the next wave has no exposures, we check final-wave outcomes (only if t_i_plus1 is the final wave)
      if (t_i_plus1 == final_wave) {
        # Fallback to outcome-based logic
        needed_outcomes <- paste0(final_wave, "_", outcome_vars)
        needed_outcomes <- needed_outcomes[needed_outcomes %in% names(df_wide_use)]
        if (length(needed_outcomes) == 0) {
          cli::cli_alert_warning("No exposures or outcomes found for {t_i_plus1}. Skipping wave {t_i}.")
          next
        }
        # Strict approach: lost if ANY final-wave outcome is missing
        df_wide_use[[not_lost_col]] <- ifelse(check_all_present(df_wide_use, needed_outcomes), 1, 0)
      } else {
        # If it's not the final wave but we have no exposures?
        # This is unusual, but let's just skip creating a not_lost indicator
        cli::cli_alert_warning(
          "No exposure columns found for {t_i_plus1}, which is not the final wave. Skipping."
        )
        next
      }
    } else {
      # Strict approach: wave t is "not lost" only if all exposures at t+1 are present
      df_wide_use[[not_lost_col]] <- ifelse(check_all_present(df_wide_use, needed_exposures), 1, 0)
    }

    # If not_lost_col = 0 => set wave t+1..final wave to NA
    rows_lost <- which(df_wide_use[[not_lost_col]] == 0)
    if (length(rows_lost) > 0) {
      future_waves <- time_points[(i + 1):num_time_points]
      for (fw in future_waves) {
        fw_cols <- grep(paste0("^", fw, "_"), names(df_wide_use), value = TRUE)
        # If we want to preserve observed final-wave outcomes, skip them:
        if (save_observed_y && fw == final_wave) {
          # remove outcome columns from the set to overwrite
          fw_outcomes <- paste0(fw, "_", outcome_vars)
          fw_cols <- setdiff(fw_cols, fw_outcomes)
        }
        df_wide_use[rows_lost, fw_cols] <- NA
      }
    }
    cli::cli_alert_success(
      "Created '{not_lost_col}' indicator with strict logic for wave {t_i}"
    )
  }

  # step 2: Missing outcomes in final wave
  cli::cli_h2("Step 2: Handling missing outcomes in the final wave")
  final_outcome_cols <- paste0(final_wave, "_", outcome_vars)
  final_outcome_cols <- final_outcome_cols[final_outcome_cols %in% names(df_wide_use)]

  if (length(final_outcome_cols) == 0) {
    cli::cli_alert_warning("No outcome columns found for the final wave. Skipping outcome-based missingness.")
  } else {
    if (!save_observed_y) {
      # Optionally set missing final-wave outcomes to NA
      missing_final_wave <- rowSums(is.na(df_wide_use[, final_outcome_cols, drop = FALSE])) > 0
      df_wide_use[missing_final_wave, final_outcome_cols] <- NA
      cli::cli_alert_success("Set partially missing final-wave outcomes to NA.")
    } else {
      cli::cli_alert_info("save_observed_y=TRUE => retaining partial final-wave outcomes.")
    }
  }

  # step 3: Lost_in_following_wave indicators
  cli::cli_h2("Step 3: Creating lost_in_following_wave indicators")
  for (i in seq_len(num_time_points - 1)) {
    t_i <- time_points[i]
    not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)
    lost_col <- paste0(t_i, "_", lost_in_following_wave)

    # Create lost_in_following_wave as inverse of not_lost_in_following_wave
    if (not_lost_col %in% names(df_wide_use)) {
      df_wide_use[[lost_col]] <- 1 - df_wide_use[[not_lost_col]]
    }
  }

  # step 4: Scale numeric variables (excluding exposures if scale_exposure=FALSE)
  cli::cli_h2("Step 4: Scaling continuous variables")
  continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]
  continuous_cols <- setdiff(continuous_cols, c(ordinal_columns, continuous_columns_keep))

  # Exclude lost/not_lost columns, binary/na, weighting columns
  drop_regex <- paste0(
    "_", not_lost_in_following_wave, "$|_",
    lost_in_following_wave, "$|_binary$|_na$|_weights$"
  )
  continuous_cols <- continuous_cols[!grepl(drop_regex, continuous_cols)]

  # Exclude exposures if not scaling them
  if (!is.null(exposure_vars) && !scale_exposure) {
    expo_cols_all <- as.vector(outer(time_points, exposure_vars, paste0))
    continuous_cols <- setdiff(continuous_cols, expo_cols_all)
  }

  # Scale them
  cols_to_remove <- character(0)
  for (cc in continuous_cols) {
    zcol <- paste0(cc, "_z")
    df_wide_use[[zcol]] <- as.vector(scale(df_wide_use[[cc]]))
    cols_to_remove <- c(cols_to_remove, cc)
  }
  df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]
  cli::cli_alert_success("Scaled continuous variables (strict approach) and removed originals")

  # step 5: Encode ordinal columns
  cli::cli_h2("Step 5: Encoding ordinal columns")
  if (!is.null(ordinal_columns) && length(ordinal_columns) > 0) {
    df_wide_use <- fastDummies::dummy_cols(
      df_wide_use,
      select_columns = ordinal_columns,
      remove_selected_columns = remove_selected_columns,
      remove_first_dummy = FALSE,
      remove_most_frequent_dummy = FALSE,
      ignore_na = TRUE
    )
    # rename e.g. "education_3" => "education_3_binary"
    for (oc in ordinal_columns) {
      dcol <- grep(paste0("^", oc, "_"), names(df_wide_use), value = TRUE)
      df_wide_use <- dplyr::rename_with(
        df_wide_use,
        ~ paste0(.x, "_binary"),
        .cols = dcol
      )
    }
    cli::cli_alert_success("Encoded ordinal columns and removed originals (if requested)")
  } else {
    cli::cli_alert_info("No ordinal columns to encode.")
  }

  # step 6: Reorder columns
  cli::cli_h2("Step 6: Reordering columns")
  new_order <- c("id") # place id first if it exists
  for (tp in time_points) {
    tp_cols <- grep(paste0("^", tp, "_"), names(df_wide_use), value = TRUE)

    # exposures
    ex_cols <- if (!is.null(exposure_vars)) paste0(tp, "_", exposure_vars) else character(0)
    ex_cols <- ex_cols[ex_cols %in% names(df_wide_use)]

    # not_lost, lost
    not_lost_col <- paste0(tp, "_", not_lost_in_following_wave)
    lost_col <- paste0(tp, "_", lost_in_following_wave)

    # z-transformed
    z_cols <- grep("_z$", tp_cols, value = TRUE)

    # everything else
    other_cols <- setdiff(tp_cols, c(ex_cols, not_lost_col, lost_col, z_cols))

    # add in the order: other, z_cols, exposures, not_lost, lost
    new_order <- c(
      new_order,
      other_cols,
      z_cols,
      ex_cols,
      not_lost_col,
      lost_col
    )
  }

  # include only existing columns
  new_order <- intersect(new_order, names(df_wide_use))

  df_wide_use <- df_wide_use[, new_order, drop = FALSE]
  cli::cli_alert_success("Columns reordered successfully")

  # Done
  cli::cli_alert_success("Data processing completed with strict all-or-nothing censoring")
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    paste("Total rows:", nrow(df_wide_use)),
    paste("Total columns:", ncol(df_wide_use)),
    paste("Time points processed:", length(time_points))
  ))

  return(df_wide_use)
}

#' Strict All-or-Nothing Censoring for Longitudinal Data
#'
#' @description
#' This function processes wide-format longitudinal data with multiple time points.
#' It is a wrapper around the internal function `.strict_exposure_outcome_censoring`.
#' See the internal function documentation for details.
#'
#' @inheritParams .strict_exposure_outcome_censoring
#' @return A processed dataframe, with strict all-or-nothing censoring
#' @keywords internal
strict_exposure_outcome_censoring <- function(
    df_wide,
    exposure_vars,
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    scale_exposure = FALSE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = "lost_following_wave",
    remove_selected_columns = TRUE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE) {
  # Call the internal function with the same parameters
  .strict_exposure_outcome_censoring(
    df_wide = df_wide,
    exposure_vars = exposure_vars,
    ordinal_columns = ordinal_columns,
    continuous_columns_keep = continuous_columns_keep,
    scale_exposure = scale_exposure,
    not_lost_in_following_wave = not_lost_in_following_wave,
    lost_in_following_wave = lost_in_following_wave,
    remove_selected_columns = remove_selected_columns,
    time_point_prefixes = time_point_prefixes,
    time_point_regex = time_point_regex,
    save_observed_y = save_observed_y
  )
}
