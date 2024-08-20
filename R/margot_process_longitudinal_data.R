#' Process Longitudinal Data for Three Waves
#'
#' @description
#' This function processes longitudinal data for exactly three waves (t0, t1, t2).
#' It handles attrition, scales baseline variables, and optionally encodes ordinal variables.
#' Note: This function is currently implemented for three waves only.
#'
#' @param df_wide A wide-format dataframe containing longitudinal data for three waves.
#' @param ordinal_columns A character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep A character vector of continuous column names to keep without scaling.
#'
#' @return A processed dataframe suitable for use in longitudinal analyses with three waves.
#'
#' @details
#' The function performs the following steps:
#' 1. Creates NA conditions for t0 and t1 based on missingness in subsequent waves.
#' 2. Handles non-factor and factor columns, applying attrition logic.
#' 3. Scales numeric baseline (t0) variables.
#' 4. Selects and orders columns.
#' 5. Optionally encodes ordinal columns.
#'
#' @note
#' This function is specifically designed for datasets with exactly three waves (t0, t1, t2).
#' It may not work correctly for datasets with fewer or more waves.
#'
#' @import dplyr
#' @import fastDummies
#' @import cli
#'
#' @examples
#' # Assuming df_wide is your wide-format dataframe with three waves
#' processed_data <- margot_process_longitudinal_data(
#'   df_wide,
#'   ordinal_columns = c("education", "income_category"),
#'   continuous_columns_keep = c("age", "bmi")
#' )
#' @export
margot_process_longitudinal_data <- function(df_wide, ordinal_columns = NULL, continuous_columns_keep = NULL) {
  cli::cli_h1("Longitudinal Data Processing")

  cli::cli_alert_info("Starting data processing for three waves (t0, t1, t2)")

  # Create NA conditions
  cli::cli_h2("Step 1: Creating NA conditions")
  t0_na_condition <- rowSums(is.na(select(df_wide, starts_with("t1_")))) > 0
  t1_na_condition <- rowSums(is.na(select(df_wide, starts_with("t2_")))) > 0
  cli::cli_alert_success("Created NA conditions for t0 and t1 \U0001F44D")

  cli::cli_h2("Step 2: Handling columns and applying attrition logic")
  df_wide_use <- df_wide |>
    # handle non-factor columns first
    mutate(
      t0_not_lost = ifelse(t0_na_condition, 0, 1),
      t1_not_lost = ifelse(t1_na_condition, 0, 1),
      across(where(is.numeric) & starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
      across(where(is.numeric) & starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
      across(where(is.numeric) & starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)),
      t0_lost = 1 - t0_not_lost,
      t1_lost = 1 - t1_not_lost
    ) |>
    # handle factor columns separately
    mutate(
      across(where(is.factor) & starts_with("t1_"),
             ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
               factor(levels = levels(.), ordered = is.ordered(.))),
      across(where(is.factor) & starts_with("t2_"),
             ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
               factor(levels = levels(.), ordered = is.ordered(.)))
    ) |>
    mutate(
      across(where(is.factor) & starts_with("t2_"),
             ~ if_else(t1_not_lost == 0, NA_character_, as.character(.)) |>
               factor(levels = levels(.), ordered = is.ordered(.)))
    )
  cli::cli_alert_success("Handled non-factor and factor columns \U0001F44D")

  cli::cli_h2("Step 3: Scaling numeric variables")
  df_wide_use <- df_wide_use |>
    mutate(
      across(
        .cols = where(is.numeric) & starts_with("t0_") &
          !ends_with("_binary") & !ends_with("_lost") &
          !ends_with("_weights") & !any_of(ordinal_columns),
        .fns = ~ scale(.) |> as.vector(),
        .names = "{.col}_z"
      )
    )
  cli::cli_alert_success("Scaled numeric variables and created binary variables \U0001F44D")

  cli::cli_h2("Step 4: Selecting and ordering columns")
  df_wide_use <- df_wide_use |>
    select(
      where(is.factor),
      ends_with("_binary"),
      ends_with("_lost"),
      all_of(ordinal_columns),
      all_of(continuous_columns_keep),
      ends_with("_z"),
      starts_with("t1_"),
      starts_with("t2_")
    ) |>
    relocate(t0_not_lost, .before = starts_with("t1_")) |>
    relocate(t1_not_lost, .before = starts_with("t2_")) |>
    relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
    relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
    droplevels()
  cli::cli_alert_success("Selected and ordered columns \U0001F44D")

  cli::cli_h2("Step 5: Encoding ordinal columns")
  df_wide_encoded <- dummy_cols(df_wide_use,
                                select_columns = ordinal_columns,
                                remove_first_dummy = FALSE,
                                remove_most_frequent_dummy = FALSE,
                                remove_selected_columns = TRUE,
                                ignore_na = TRUE
  ) |>
    rename_with(
      ~ paste0(., "_binary"),
      .cols = starts_with(paste0(ordinal_columns, "_"))
    ) |>
    relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
    relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
    relocate("t0_not_lost", .before = starts_with("t1_")) |>
    relocate("t1_not_lost", .before = starts_with("t2_"))
  cli::cli_alert_success("Encoded ordinal columns and finalized column order \U0001F44D")

  cli::cli_alert_success("Data processing completed successfully \U0001F44D")
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    paste("Total rows processed:", nrow(df_wide_encoded)),
    paste("Total columns:", ncol(df_wide_encoded)),
    paste("Ordinal columns encoded:", length(ordinal_columns)),
    paste("Continuous columns kept:", length(continuous_columns_keep))
  ))

  return(df_wide_encoded)
}
# margot_process_longitudinal_data <- function(df_wide, ordinal_columns = NULL, continuous_columns_keep = NULL) {
#   message("Starting data processing...")
#
#   # create na conditions
#   t0_na_condition <- rowSums(is.na(select(df_wide, starts_with("t1_")))) > 0
#   t1_na_condition <- rowSums(is.na(select(df_wide, starts_with("t2_")))) > 0
#
#   message("Created NA conditions for t0 and t1")
#
#   df_wide_use <- df_wide |>
#     # handle non-factor columns first
#     mutate(
#       t0_not_lost = ifelse(t0_na_condition, 0, 1),
#       t1_not_lost = ifelse(t1_na_condition, 0, 1),
#       across(where(is.numeric) & starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
#       across(where(is.numeric) & starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
#       across(where(is.numeric) & starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)),
#       t0_lost = 1 - t0_not_lost,
#       t1_lost = 1 - t1_not_lost
#     ) |>
#     # handle factor columns separately
#     mutate(
#       across(where(is.factor) & starts_with("t1_"),
#              ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
#                factor(levels = levels(.), ordered = is.ordered(.))),
#       across(where(is.factor) & starts_with("t2_"),
#              ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
#                factor(levels = levels(.), ordered = is.ordered(.)))
#     ) |>
#     mutate(
#       across(where(is.factor) & starts_with("t2_"),
#              ~ if_else(t1_not_lost == 0, NA_character_, as.character(.)) |>
#                factor(levels = levels(.), ordered = is.ordered(.)))
#     )
#
#   message("Handled non-factor and factor columns")
#
#   # scale and create binary variables
#   df_wide_use <- df_wide_use |>
#     mutate(
#       across(
#         .cols = where(is.numeric) & starts_with("t0_") &
#           !ends_with("_binary") & !ends_with("_lost") &
#           !ends_with("_weights") & !any_of(ordinal_columns),
#         .fns = ~ scale(.) |> as.vector(),
#         .names = "{.col}_z"
#       )
#     )
#
#   message("Scaled numeric variables and created binary variables")
#
#   # select and order columns
#   df_wide_use <- df_wide_use |>
#     select(
#       where(is.factor),
#       ends_with("_binary"),
#       ends_with("_lost"),
#       all_of(ordinal_columns),
#       all_of(continuous_columns_keep),
#       ends_with("_z"),
#       starts_with("t1_"),
#       starts_with("t2_")
#     ) |>
#     relocate(t0_not_lost, .before = starts_with("t1_")) |>
#     relocate(t1_not_lost, .before = starts_with("t2_")) |>
#     relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
#     relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
#     droplevels()
#
#   message("Selected and ordered columns")
#
#   # encode ordinal columns
#   df_wide_encoded <- dummy_cols(df_wide_use,
#                                 select_columns = ordinal_columns,
#                                 remove_first_dummy = FALSE,
#                                 remove_most_frequent_dummy = FALSE,
#                                 remove_selected_columns = TRUE,
#                                 ignore_na = TRUE
#   ) |>
#     rename_with(
#       ~ paste0(., "_binary"),
#       .cols = starts_with(paste0(ordinal_columns, "_"))
#     ) |>
#     relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
#     relocate(starts_with("t1_"), .before = starts_with("t2_")) |>
#     relocate("t0_not_lost", .before = starts_with("t1_")) |>
#     relocate("t1_not_lost", .before = starts_with("t2_"))
#
#   message("Encoded ordinal columns and finalized column order")
#
#   message("Data processing completed successfully.")
#
#   return(df_wide_encoded)
# }
