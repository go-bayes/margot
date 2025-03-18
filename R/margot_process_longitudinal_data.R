#' process longitudinal data for three waves
#'
#' @description
#' this function processes longitudinal data for exactly three waves (t0, t1, t2).
#' it handles attrition, scales baseline variables, and optionally encodes ordinal variables.
#' note: this function is currently implemented for three waves only.
#'
#' @param df_wide a wide-format dataframe containing longitudinal data for three waves.
#' @param ordinal_columns a character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep a character vector of continuous column names to keep without scaling.
#'
#' @return a processed dataframe suitable for use in longitudinal analyses with three waves.
#'
#' @details
#' the function performs the following steps:
#' 1. creates na conditions for t0 and t1 based on missingness in subsequent waves.
#' 2. handles non-factor and factor columns, applying attrition logic.
#' 3. scales numeric baseline (t0) variables.
#' 4. selects and orders columns.
#' 5. optionally encodes ordinal columns.
#'
#' @note
#' this function is specifically designed for datasets with exactly three waves (t0, t1, t2).
#' it may not work correctly for datasets with fewer or more waves.
#'
#' @importFrom dplyr select starts_with mutate across where ends_with any_of all_of relocate rename_with
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success cli_ul
#'
#' @examples
#' # assuming df_wide is your wide-format dataframe with three waves
#' processed_data <- margot_process_longitudinal_data(
#'   df_wide,
#'   ordinal_columns = c("education", "income_category"),
#'   continuous_columns_keep = c("age", "bmi")
#' )
#' @export
margot_process_longitudinal_data <- function(df_wide, ordinal_columns = NULL, continuous_columns_keep = NULL) {
  cli::cli_h1("Longitudinal Data Processing")

  cli::cli_alert_info("Starting data processing for three waves (t0, t1, t2)")

  # create na conditions
  cli::cli_h2("Step 1: Creating NA conditions")
  t0_na_condition <- rowSums(is.na(dplyr::select(df_wide, dplyr::starts_with("t1_")))) > 0
  t1_na_condition <- rowSums(is.na(dplyr::select(df_wide, dplyr::starts_with("t2_")))) > 0
  cli::cli_alert_success("Created NA conditions for t0 and t1 \U0001F44D")

  cli::cli_h2("Step 2: Handling columns and applying attrition logic")
  df_wide_use <- df_wide |>
    # handle non-factor columns first
    dplyr::mutate(
      t0_not_lost = ifelse(t0_na_condition, 0, 1),
      t1_not_lost = ifelse(t1_na_condition, 0, 1),
      dplyr::across(dplyr::where(is.numeric) & dplyr::starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
      dplyr::across(dplyr::where(is.numeric) & dplyr::starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
      dplyr::across(dplyr::where(is.numeric) & dplyr::starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)),
      t0_lost = 1 - t0_not_lost,
      t1_lost = 1 - t1_not_lost
    ) |>
    # handle factor columns separately
    dplyr::mutate(
      dplyr::across(dplyr::where(is.factor) & dplyr::starts_with("t1_"),
                    ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
                      factor(levels = levels(.), ordered = is.ordered(.))),
      dplyr::across(dplyr::where(is.factor) & dplyr::starts_with("t2_"),
                    ~ if_else(t0_not_lost == 0, NA_character_, as.character(.)) |>
                      factor(levels = levels(.), ordered = is.ordered(.)))
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.factor) & dplyr::starts_with("t2_"),
                    ~ if_else(t1_not_lost == 0, NA_character_, as.character(.)) |>
                      factor(levels = levels(.), ordered = is.ordered(.)))
    )
  cli::cli_alert_success("Handled non-factor and factor columns \U0001F44D")

  cli::cli_h2("Step 3: Scaling numeric variables")
  df_wide_use <- df_wide_use |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric) & dplyr::starts_with("t0_") &
          !dplyr::ends_with("_binary") & !dplyr::ends_with("_lost") &
          !dplyr::ends_with("_weights") & !dplyr::any_of(ordinal_columns),
        .fns = ~ scale(.) |> as.vector(),
        .names = "{.col}_z"
      )
    )
  cli::cli_alert_success("Scaled numeric variables and created binary variables \U0001F44D")

  cli::cli_h2("Step 4: Selecting and ordering columns")
  df_wide_use <- df_wide_use |>
    dplyr::select(
      dplyr::where(is.factor),
      dplyr::ends_with("_binary"),
      dplyr::ends_with("_lost"),
      dplyr::all_of(ordinal_columns),
      dplyr::all_of(continuous_columns_keep),
      dplyr::ends_with("_z"),
      dplyr::starts_with("t1_"),
      dplyr::starts_with("t2_")
    ) |>
    dplyr::relocate(t0_not_lost, .before = dplyr::starts_with("t1_")) |>
    dplyr::relocate(t1_not_lost, .before = dplyr::starts_with("t2_")) |>
    dplyr::relocate(dplyr::starts_with("t0_"), .before = dplyr::starts_with("t1_")) |>
    dplyr::relocate(dplyr::starts_with("t1_"), .before = dplyr::starts_with("t2_")) |>
    droplevels()
  cli::cli_alert_success("Selected and ordered columns \U0001F44D")

  cli::cli_h2("Step 5: Encoding ordinal columns")
  df_wide_encoded <- fastDummies::dummy_cols(df_wide_use,
                                             select_columns = ordinal_columns,
                                             remove_first_dummy = FALSE,
                                             remove_most_frequent_dummy = FALSE,
                                             remove_selected_columns = TRUE,
                                             ignore_na = TRUE
  ) |>
    dplyr::rename_with(
      ~ paste0(., "_binary"),
      .cols = dplyr::starts_with(paste0(ordinal_columns, "_"))
    ) |>
    dplyr::relocate(dplyr::starts_with("t0_"), .before = dplyr::starts_with("t1_")) |>
    dplyr::relocate(dplyr::starts_with("t1_"), .before = dplyr::starts_with("t2_")) |>
    dplyr::relocate("t0_not_lost", .before = dplyr::starts_with("t1_")) |>
    dplyr::relocate("t1_not_lost", .before = dplyr::starts_with("t2_"))
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
