#' Impute Missing Values Using Carry Forward in Longitudinal Data
#'
#' @description
#' Imputes missing values in longitudinal data by carrying forward previous observations
#' up to a specified number of time points back. Optionally, it can create indicator variables
#' for the imputed values.
#'
#' @param df_wide A wide-format dataframe containing longitudinal data.
#' @param columns_to_impute Character vector of base column names to impute (without time prefixes).
#' @param max_carry_forward Maximum number of time points to look back for carrying forward values.
#' @param time_point_prefixes Optional vector of time point prefixes (e.g., \code{c("t0", "t1", "t2")}).
#' @param time_point_regex Optional regex pattern to identify time points. Overrides \code{time_point_prefixes} if provided.
#' @param require_one_observed Logical. If \code{TRUE}, only impute if at least one value is observed in the next wave.
#' @param columns_no_future_required Character vector of columns that do not require future observations for imputation.
#'   Defaults to all columns if \code{require_one_observed = FALSE}, or none if \code{require_one_observed = TRUE}.
#' @param create_na_indicator Logical. If \code{TRUE}, creates indicator variables for imputed values.
#' @param indicator_suffix Suffix to add to the original column name for the indicator variable (default is \code{"_na"}).
#' @param indicator_as_suffix Logical. If \code{TRUE}, the indicator suffix is added as a suffix; if \code{FALSE}, it's added as a prefix.
#' @param verbose Logical. If \code{TRUE}, prints progress information.
#'
#' @return A dataframe with imputed values and optional indicator variables.
#'
#' @examples
#' # [Examples as provided in the original function]
#'
#' @import dplyr
#' @import cli
#'
#' @export
margot_impute_carry_forward <- function(
    df_wide,
    columns_to_impute,
    max_carry_forward = 1,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    require_one_observed = TRUE,
    columns_no_future_required = NULL,  # New parameter
    create_na_indicator = TRUE,
    indicator_suffix = "_na",
    indicator_as_suffix = TRUE,
    verbose = TRUE
) {
  # Set default for columns_no_future_required
  if (!is.null(columns_no_future_required)) {
    # Validate that columns_no_future_required is a subset of columns_to_impute
    invalid_cols <- setdiff(columns_no_future_required, columns_to_impute)
    if (length(invalid_cols) > 0) {
      stop("The following columns in columns_no_future_required are not in columns_to_impute: ",
           paste(invalid_cols, collapse = ", "))
    }
  } else {
    if (!require_one_observed) {
      columns_no_future_required <- columns_to_impute
    } else {
      columns_no_future_required <- character(0)
    }
  }

  # [Input validation remains unchanged]
  # (Assuming input validation is handled elsewhere or is not necessary for brevity)

  # Initialize progress reporting and imputation tracking
  if (verbose) {
    cli::cli_h1("Longitudinal Data Imputation")
    cli::cli_alert_info("Starting imputation process")
  }

  # Initialize imputation tracking list
  imputation_stats <- list()

  # Find time points
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

  # Sort time points numerically
  time_points <- time_points[order(as.numeric(gsub("\\D", "", time_points)))]
  num_time_points <- length(time_points)

  if (verbose) {
    cli::cli_alert_success("Identified {num_time_points} time points: {paste(time_points, collapse = ', ')}")
  }

  # Make copy of dataframe to modify
  df_imputed <- df_wide

  # Initialize a list to store indicator columns
  indicator_columns <- list()

  # For each column to impute
  for (col_base in columns_to_impute) {
    if (verbose) {
      cli::cli_h2("Processing variable: {col_base}")
    }

    # Initialize stats for this column
    imputation_stats[[col_base]] <- list()

    # Determine if this column requires future observation
    requires_future <- require_one_observed && !(col_base %in% columns_no_future_required)

    # Determine maximum time point index
    max_t <- if(requires_future) num_time_points - 1 else num_time_points

    for (t_idx in 1:max_t) {
      current_t <- time_points[t_idx]
      current_col <- paste0(current_t, "_", col_base)

      # Initialize stats for this time point
      imputation_stats[[col_base]][[current_t]] <- list(
        total_missing = 0,
        imputed = 0,
        remaining_missing = 0,
        source_timepoints = list()
      )

      # Skip if column doesn't exist
      if (!current_col %in% names(df_imputed)) {
        if (verbose) {
          cli::cli_alert_warning("Column `{current_col}` not found, skipping.")
        }
        next
      }

      # If requires_future, check next wave
      if (requires_future && t_idx < num_time_points) {
        next_t <- time_points[t_idx + 1]
        next_col <- paste0(next_t, "_", col_base)

        # Only process rows where there's at least one observation in the next wave
        if (next_col %in% names(df_imputed)) {
          rows_to_process <- !is.na(df_imputed[[next_col]])
        } else {
          if (verbose) {
            cli::cli_alert_warning("Next wave column `{next_col}` not found, skipping `{current_col}`.")
          }
          next
        }
      } else {
        # Process all rows
        rows_to_process <- rep(TRUE, nrow(df_imputed))
      }

      # Find missing values in the current column
      missing_mask <- is.na(df_imputed[[current_col]]) & rows_to_process
      initial_missing <- sum(missing_mask)

      imputation_stats[[col_base]][[current_t]]$total_missing <- initial_missing

      if (initial_missing > 0) {  # Only proceed if there are missing values
        if (create_na_indicator) {
          indicator_var_name <- if (indicator_as_suffix) {
            paste0(current_col, indicator_suffix)
          } else {
            paste0(indicator_suffix, current_col)
          }
          # Initialize indicator with zeros
          if (!indicator_var_name %in% names(indicator_columns)) {
            indicator_columns[[indicator_var_name]] <- integer(nrow(df_imputed))
          }
          # Mark positions where imputation occurs
          indicator_columns[[indicator_var_name]][missing_mask] <- 1
        }

        if (verbose) {
          cli::cli_alert_info("Processing `{current_t}`: {initial_missing} missing values to impute.")
        }

        # Look back up to `max_carry_forward` time points
        for (look_back in 1:max_carry_forward) {
          if ((t_idx - look_back) < 1) break  # No more previous time points

          prev_t <- time_points[t_idx - look_back]
          prev_col <- paste0(prev_t, "_", col_base)

          if (prev_col %in% names(df_imputed)) {
            # Identify rows still missing before imputation
            still_missing <- is.na(df_imputed[[current_col]]) & missing_mask

            if (sum(still_missing) == 0) break  # No more imputations needed

            # Impute values from the previous time point
            df_imputed[still_missing, current_col] <- df_imputed[still_missing, prev_col]

            # Count how many were imputed from this time point
            newly_imputed <- sum(!is.na(df_imputed[still_missing, current_col]))
            if (newly_imputed > 0) {
              imputation_stats[[col_base]][[current_t]]$imputed <-
                imputation_stats[[col_base]][[current_t]]$imputed + newly_imputed
              imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]] <-
                ifelse(is.null(imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]]),
                       newly_imputed,
                       imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]] + newly_imputed)
            }
          }
        }

        # Update remaining missing
        final_missing <- sum(is.na(df_imputed[[current_col]]) & missing_mask)
        imputation_stats[[col_base]][[current_t]]$remaining_missing <- final_missing
      }
    }

    # Handle the last time point if no future observation is required for this column
    if (!requires_future && num_time_points >= 1) {
      t_idx <- num_time_points
      current_t <- time_points[t_idx]
      current_col <- paste0(current_t, "_", col_base)

      # Initialize stats for this time point if not already initialized
      if (is.null(imputation_stats[[col_base]][[current_t]])) {
        imputation_stats[[col_base]][[current_t]] <- list(
          total_missing = 0,
          imputed = 0,
          remaining_missing = 0,
          source_timepoints = list()
        )
      }

      # Find missing values in the current column
      missing_mask <- is.na(df_imputed[[current_col]])
      initial_missing <- sum(missing_mask)

      imputation_stats[[col_base]][[current_t]]$total_missing <- initial_missing

      if (initial_missing > 0) {  # Only proceed if there are missing values
        if (create_na_indicator) {
          indicator_var_name <- if (indicator_as_suffix) {
            paste0(current_col, indicator_suffix)
          } else {
            paste0(indicator_suffix, current_col)
          }
          # Initialize indicator with zeros
          if (!indicator_var_name %in% names(indicator_columns)) {
            indicator_columns[[indicator_var_name]] <- integer(nrow(df_imputed))
          }
          # Mark positions where imputation occurs
          indicator_columns[[indicator_var_name]][missing_mask] <- 1
        }

        if (verbose) {
          cli::cli_alert_info("Processing `{current_t}`: {initial_missing} missing values to impute.")
        }

        # Look back up to `max_carry_forward` time points
        for (look_back in 1:max_carry_forward) {
          if ((t_idx - look_back) < 1) break  # No more previous time points

          prev_t <- time_points[t_idx - look_back]
          prev_col <- paste0(prev_t, "_", col_base)

          if (prev_col %in% names(df_imputed)) {
            # Identify rows still missing before imputation
            still_missing <- is.na(df_imputed[[current_col]]) & missing_mask

            if (sum(still_missing) == 0) break  # No more imputations needed

            # Impute values from the previous time point
            df_imputed[still_missing, current_col] <- df_imputed[still_missing, prev_col]

            # Count how many were imputed from this time point
            newly_imputed <- sum(!is.na(df_imputed[still_missing, current_col]))
            if (newly_imputed > 0) {
              imputation_stats[[col_base]][[current_t]]$imputed <-
                imputation_stats[[col_base]][[current_t]]$imputed + newly_imputed
              imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]] <-
                ifelse(is.null(imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]]),
                       newly_imputed,
                       imputation_stats[[col_base]][[current_t]]$source_timepoints[[prev_t]] + newly_imputed)
            }
          }
        }

        # Update remaining missing
        final_missing <- sum(is.na(df_imputed[[current_col]]) & missing_mask)
        imputation_stats[[col_base]][[current_t]]$remaining_missing <- final_missing
      }
    }
  }

  # Add indicator columns to the dataframe
  if (create_na_indicator && length(indicator_columns) > 0) {
    # Convert list to dataframe
    indicator_df <- as.data.frame(indicator_columns)
    # Ensure the order matches the original dataframe
    df_imputed <- cbind(df_imputed, indicator_df)
  }

  # Make report
  if (verbose) {
    cli::cli_h1("Imputation Summary Report")

    for (col_base in names(imputation_stats)) {
      cli::cli_h2("Variable: {col_base}")

      for (t in names(imputation_stats[[col_base]])) {
        stats <- imputation_stats[[col_base]][[t]]

        if (stats$total_missing > 0) {
          cli::cli_h3("Time point: {t}")
          cli::cli_alert_info("Initial missing values: {stats$total_missing}")
          cli::cli_alert_success("Successfully imputed: {stats$imputed}")
          if (stats$remaining_missing > 0) {
            cli::cli_alert_warning("Remaining missing: {stats$remaining_missing}")
          }

          if (length(stats$source_timepoints) > 0) {
            cli::cli_alert_info("Imputation sources:")
            cli::cli_ul({
              for (source_t in names(stats$source_timepoints)) {
                percentage <- if (stats$imputed > 0) {
                  round(stats$source_timepoints[[source_t]] / stats$imputed * 100, 1)
                } else {
                  0
                }
                cli::cli_li(
                  "From `{source_t}`: {stats$source_timepoints[[source_t]]} values ({percentage}% of imputed)"
                )
              }
            })
          }
        }
      }

      # Compute/display total statistics for this variable
      total_missing <- sum(sapply(imputation_stats[[col_base]], function(x) x$total_missing))
      total_imputed <- sum(sapply(imputation_stats[[col_base]], function(x) x$imputed))
      total_remaining <- sum(sapply(imputation_stats[[col_base]], function(x) x$remaining_missing))

      if (total_missing > 0) {
        impute_percent <- if (total_missing > 0) {
          round((total_imputed / total_missing) * 100, 1)
        } else {
          0
        }
        cli::cli_alert_success(
          "Total for `{col_base}`: {total_imputed}/{total_missing} values imputed ({impute_percent}%)"
        )
        if (total_remaining > 0) {
          cli::cli_alert_warning(
            "Total remaining missing for `{col_base}`: {total_remaining}"
          )
        }
        cli::cli_text("")
      } else {
        cli::cli_alert_info(
          "No missing values found for `{col_base}`."
        )
        cli::cli_text("")
      }
    }

    cli::cli_alert_success("Imputation process completed successfully \U0001F44D")
  }

  return(df_imputed)
}
