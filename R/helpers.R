
# helpers for margot plot -------------------------------------------------

# #' @keywords internal
back_transform_estimates <- function(results_df, original_df) {
  # Determine the effect size column
  if ("E[Y(1)]-E[Y(0)]" %in% names(results_df)) {
    effect_size_col <- "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(results_df)) {
    effect_size_col <- "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
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
        # Variable not found in original_df
        cli::cli_alert_warning("Variable '{orig_var_name}' not found in original_df.")
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
#' types of treatment effect estimates (RR or RD). It supports different sorting
#' options including default descending, alphabetical, and custom order.
#' It now also handles original scale estimates when available.
#'
#' @param df Data frame containing the variables of interest, or a list containing
#' the results dataframe and label mapping from transform_to_original_scale().
#' @param type Type of treatment effect to analyze. Expected values are 'RR' for Risk Ratio
#' and 'RD' for Risk Difference. Defaults to 'RD'.
#' @param order Specifies the order in which the outcomes should be arranged. Can be
#' 'default' for descending order of the estimate, 'alphabetical' for alphabetical order by outcome,
#' or 'custom' for a user-defined order. Default is 'default'.
#' @param custom_order A vector of custom ordering for the outcomes, applicable if `order` is set to 'custom'.
#' This should be a vector containing all outcomes in the desired order.
#'
#' @return A data frame that has been arranged based on the specified order and annotated
#' with treatment effect estimates, estimate labels, and evidence value annotations.
#' If original scale estimates are available, these will be included in the output.
#'
#' @details
#' The function now handles both transformed and original scale results. If original scale results
#' are available (indicated by the presence of columns with "_original" suffix), these will be included
#' in the output. The function also applies label mapping if provided.
#'
#' @examples
#' # Example using Risk Ratio (RR) and default sorting
#' result_df <- group_tab(df = analysis_df, type = 'RR')
#'
#' # Example using Risk Difference (RD) with alphabetical sorting
#' result_df <- group_tab(df = analysis_df, type = 'RD', order = 'alphabetical')
#'
#' # Example using custom sorting order
#' custom_order <- c("Outcome3", "Outcome1", "Outcome2")
#' result_df <- group_tab(df = analysis_df, type = 'RR', order = 'custom', custom_order = custom_order)
#'
#' # Example using output from transform_to_original_scale()
#' transformed_data <- transform_to_original_scale(results_df, original_df, label_mapping)
#' result_df <- group_tab(transformed_data, type = 'RD')
#'
#' @importFrom dplyr arrange mutate slice desc recode
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @keywords internal
group_tab <- function(df, type = c("RD", "RR"), order = c("default", "alphabetical", "custom"), custom_order = NULL) {
  require(dplyr)
  type <- match.arg(type)
  order <- match.arg(order)

  # Check if input is the list returned by transform_to_original_scale
  if (is.list(df) && "results_df" %in% names(df)) {
    results_df <- df$results_df
    label_mapping <- df$label_mapping
  } else {
    results_df <- df
    label_mapping <- NULL
  }

  # Ensure the 'outcome' column exists; if not, create from row names
  if (!"outcome" %in% names(results_df) && !is.null(rownames(results_df))) {
    results_df <- results_df %>% tibble::rownames_to_column(var = "outcome")
  } else if (!"outcome" %in% names(results_df)) {
    stop("No 'outcome' column or row names available to convert into an 'outcome' column.")
  }

  # Apply label mapping if available
  if (!is.null(label_mapping)) {
    results_df <- results_df %>%
      mutate(outcome = dplyr::recode(outcome, !!!label_mapping))
  }

  # Determine the column to sort by based on the type
  effect_column <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"

  # Apply ordering based on the specified 'order'
  if (order == "alphabetical") {
    results_df <- results_df %>% arrange(outcome)
  } else if (order == "custom" && !is.null(custom_order)) {
    results_df <- results_df %>% slice(match(custom_order, outcome))
  } else {  # default is descending order by effect size
    results_df <- results_df %>% arrange(desc(!!sym(effect_column)))
  }

  # Add Estimate categorization and label column
  results_df <- results_df %>% mutate(
    Estimate = factor(
      if (type == "RR") {
        ifelse(
          `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
          "positive",
          ifelse(`E[Y(1)]/E[Y(0)]` < 1 & `97.5 %` < 1, "negative", "not reliable")
        )
      } else {
        ifelse(
          `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
          "positive",
          ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0, "negative", "not reliable")
        )
      }
    ),
    estimate_lab = if (type == "RR") {
      paste(
        round(`E[Y(1)]/E[Y(0)]`, 3),
        " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
        sep = ""
      )
    } else {
      paste(
        round(`E[Y(1)]-E[Y(0)]`, 3),
        " (", round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]",
        sep = ""
      )
    },
    label = estimate_lab  # Add this line to create the 'label' column
  )

  # Add original scale estimates if available
  if (paste0(effect_column, "_original") %in% names(results_df)) {
    results_df <- results_df %>% mutate(
      estimate_lab_original = paste(
        round(.data[[paste0(effect_column, "_original")]], 3),
        " (", round(.data[["2.5 %_original"]], 3), "-", round(.data[["97.5 %_original"]], 3), ")",
        sep = ""
      )
    )
  }

  return(results_df)
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
#' tabulated_results <- tab_engine_marginal(x = results_df,
#'                                          new_name = "Treatment Effect",
#'                                          delta = 1,
#'                                          sd = 0.2,
#'                                          type = "RD")  # Corrected 'scale' to 'type'
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
    out$outcome <- new_name  # Just update the existing 'outcome' column instead of adding a new one
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
    if(!is.null(table) && nrow(table) > 0) {
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
      if(!is.null(table) && nrow(table) > 0) {
        row_index <- which(sub(".*? - ", "", rownames(table)) == comparison)
        if (length(row_index) > 0) {
          result <- table[row_index, , drop = FALSE]
          rownames(result) <- sub(" - .*$", "", rownames(result))  # remove comparison from rowname
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
    if(nrow(comparison_results) > 0) {
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
    ggplot2::labs(title = paste("Distribution of tau.hat for", outcome),
                  x = "tau.hat", y = "Count") +
    ggplot2::facet_wrap(~ comparison, scales = "free_y")
}

#' Extract Qini Data for Plotting
#'
#' @description
#' Extracts Qini curve data from a Qini object and prepares it for plotting.
#'
#' @param qini_obj A Qini object.
#' @param name Name of the treatment arm.
#' @param max_index Maximum index to extend the curve to.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A data frame with extracted Qini data.
#'
#' @keywords internal
extract_qini_data <- function(qini_obj, name, max_index, verbose = TRUE) {
  gain <- if (!is.null(qini_obj[["_path"]]$gain)) {
    qini_obj[["_path"]]$gain
  } else {
    if (verbose) cli::cli_alert_warning(paste("Qini object", name, "is NULL or missing required components. Extending with zeros."))
    rep(0, max_index)
  }

  gain_length <- length(gain)
  if (gain_length < max_index) {
    # Extend gain vector
    gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
  } else if (gain_length > max_index) {
    # Truncate gain vector
    gain <- gain[1:max_index]
  }

  proportion <- seq_len(max_index) / max_index

  data.frame(
    proportion = proportion,
    gain = gain,
    curve = name
  )
}

# label and plotting for causal forest models -----------------------------


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
    theme_params = theme_bw()
) {
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
      gaussian()  # Default to gaussian if no family specified
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
              colors = plot_params$colors) +
      geom_point(aes(x = x, y = predicted),
                 color = "dodgerblue",
                 size = plot_params$dot_size,
                 alpha = 1)

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
                            nrow = facet_params$nrow)
      }
    }

    plots_list[[outcome_label]] <- p
  }

  return(list(
    predictions = predictions_list,
    plots = plots_list
  ))
}

# helper operator for NULL handling
`%||%` <- function(x, y) if (is.null(x)) y else x



#' @keywords internal
#' experimental cross-table functions
margot_summary_tables_cat <- function(data,
                                      baseline_wave,
                                      exposure_waves,
                                      outcome_wave,
                                      name_exposure,
                                      name_exposure_cat = NULL,
                                      baseline_vars = NULL,
                                      outcome_vars = NULL,
                                      extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
                                      baseline_labels = NULL,
                                      exposure_labels = NULL,
                                      outcome_labels = NULL,
                                      create_plots = FALSE,
                                      plot_type = "boxplot",
                                      show_progress = TRUE,
                                      cross_tabs = FALSE) {

  require(cli)
  require(dplyr)
  require(janitor)
  require(gtsummary)
  require(labelled)
  require(tidyr)
  require(ggplot2)
  require(crayon)

  # Basic checks
  if (is.null(baseline_vars) && is.null(outcome_vars)) {
    stop("At least one of baseline_vars or outcome_vars must be provided.")
  }

  if (!name_exposure %in% names(data)) {
    stop(sprintf("Exposure variable '%s' not found. Available columns: %s",
                 name_exposure, paste(sort(names(data)), collapse=", ")))
  }

  # If cross_tabs is TRUE, verify categorical exposure
  if (cross_tabs) {
    if (length(name_exposure) > 1) {
      stop("For cross-tabulation, only one exposure variable should be provided.")
    }

    # Ensure exposure is factor
    data[[name_exposure]] <- as.factor(data[[name_exposure]])
  }

  cli::cli_h1("Margot Summary Tables")

  # Calculate participants
  n_participants <- length(unique(data$id))
  n_participants_formatted <- format(n_participants, big.mark = ",")
  cli::cli_alert_success("Number of unique participants: {.val {n_participants_formatted}} \U0001F44D")

  # Baseline table
  table_baseline <- NULL
  if (!is.null(baseline_vars)) {
    # Create baseline dataset with exposure
    baseline_vars_with_exposure <- unique(c(baseline_vars, name_exposure))

    dt_baseline <- data %>%
      dplyr::filter(wave == baseline_wave) %>%
      dplyr::select(any_of(baseline_vars_with_exposure)) %>%
      droplevels()

    if (cross_tabs) {
      # Create stratified baseline table
      table_baseline <- dt_baseline %>%
        gtsummary::tbl_summary(
          by = name_exposure,
          missing = "ifany",
          percent = "row",
          statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
          type = list(all_continuous() ~ "continuous2"),
          label = baseline_labels
        ) %>%
        gtsummary::modify_header(label = "**Baseline Variables Stratified by Exposure**") %>%
        gtsummary::bold_labels() %>%
        gtsummary::add_overall()
    } else {
      table_baseline <- dt_baseline %>%
        gtsummary::tbl_summary(
          missing = "ifany",
          percent = "column",
          statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
          type = list(all_continuous() ~ "continuous2"),
          label = baseline_labels
        ) %>%
        gtsummary::modify_header(label = "**Exposure + Demographic Variables**") %>%
        gtsummary::bold_labels()
    }
  }

  # Initialize lists for tables and summaries
  exposure_tables <- list()
  exposure_summaries <- list()
  plots <- list()

  # Process exposure variable(s)
  for (exposure_var in name_exposure) {
    cli::cli_h2(paste("Processing exposure variable:", exposure_var))

    if (cross_tabs) {
      # Create transition frequencies table
      exposure_transitions <- data %>%
        dplyr::filter(wave %in% c(baseline_wave, exposure_waves)) %>%
        dplyr::select(id, wave, !!sym(exposure_var)) %>%
        tidyr::pivot_wider(
          names_from = wave,
          values_from = !!sym(exposure_var),
          names_prefix = "wave_"
        ) %>%
        tidyr::drop_na()

      # Create summary table using gtsummary
      exposure_tables[[exposure_var]] <- exposure_transitions %>%
        tidyr::pivot_longer(
          cols = starts_with("wave_"),
          names_to = "wave",
          values_to = "value"
        ) %>%
        dplyr::group_by(wave, value) %>%
        dplyr::summarise(n = n(), .groups = "drop") %>%
        dplyr::mutate(
          pct = n / sum(n) * 100,
          summary = sprintf("%d (%.1f%%)", n, pct)
        ) %>%
        tidyr::pivot_wider(
          names_from = wave,
          values_from = summary
        ) %>%
        # Convert to gtsummary table
        gtsummary::tbl_summary(
          label = list(value ~ "Category")
        ) %>%
        gtsummary::modify_header(label = paste("**Exposure Variable:", exposure_var, "Transitions**"))
    } else {
      dt_exposure <- data %>%
        dplyr::filter(wave %in% c(baseline_wave, exposure_waves)) %>%
        dplyr::select(all_of(c(exposure_var, "wave"))) %>%
        droplevels()

      exposure_tables[[exposure_var]] <- dt_exposure %>%
        gtsummary::tbl_summary(
          by = "wave",
          missing = "always",
          percent = "column",
          label = exposure_labels
        ) %>%
        gtsummary::modify_header(label = paste("**Exposure Variable:", exposure_var, "by Wave**")) %>%
        gtsummary::bold_labels()
    }
  }

  # Outcome table
  table_outcomes <- NULL
  if (!is.null(outcome_vars)) {
    outcome_vars_with_exposure <- unique(c(outcome_vars, name_exposure))

    dt_outcome <- data %>%
      dplyr::filter(wave %in% c(baseline_wave, outcome_wave)) %>%
      dplyr::select(any_of(outcome_vars_with_exposure)) %>%
      droplevels()

    if (cross_tabs) {
      table_outcomes <- dt_outcome %>%
        gtsummary::tbl_summary(
          by = name_exposure,
          missing = "always",
          percent = "row",
          label = outcome_labels
        ) %>%
        gtsummary::modify_header(label = "**Outcome Variables Stratified by Exposure**") %>%
        gtsummary::bold_labels() %>%
        gtsummary::add_overall()
    } else {
      table_outcomes <- dt_outcome %>%
        gtsummary::tbl_summary(
          by = "wave",
          missing = "always",
          percent = "column",
          label = outcome_labels
        ) %>%
        gtsummary::modify_header(label = "**Outcome Variables by Wave**") %>%
        gtsummary::bold_labels()
    }
  }

  return(list(
    baseline_table = table_baseline,
    exposure_tables = exposure_tables,
    outcome_table = table_outcomes,
    n_participants = n_participants_formatted,
    exposure_summaries = exposure_summaries,
    plots = plots
  ))
}
