# margot_process_longitudinal_data <- function(df_wide, ordinal_columns = NULL,continuous_columns_keep = NULL) {
#   cli::cli_h1("Longitudinal Data Processing")
#
#   cli::cli_alert_info("Starting data processing for three waves (t0, t1, t2)")
#
#   # Create NA conditions
#   cli::cli_h2("Step 1: Creating NA conditions")
#   t0_na_condition <- rowSums(is.na(select(df_wide, starts_with("t1_")))) > 0
#   t1_na_condition <- rowSums(is.na(select(df_wide, starts_with("t2_")))) > 0
#   cli::cli_alert_success("Created NA conditions for t0 and t1 \U0001F44D")
#
#   cli::cli_h2("Step 2: Handling columns and applying attrition logic")
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
#   cli::cli_alert_success("Handled non-factor and factor columns \U0001F44D")
#
#   cli::cli_h2("Step 3: Scaling numeric variables")
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
#   cli::cli_alert_success("Scaled numeric variables and created binary variables \U0001F44D")
#
#   cli::cli_h2("Step 4: Selecting and ordering columns")
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
#   cli::cli_alert_success("Selected and ordered columns \U0001F44D")
#
#   cli::cli_h2("Step 5: Encoding ordinal columns")
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
#   cli::cli_alert_success("Encoded ordinal columns and finalized column order \U0001F44D")
#
#   cli::cli_alert_success("Data processing completed successfully \U0001F44D")
#   cli::cli_h2("Summary")
#   cli::cli_ul(c(
#     paste("Total rows processed:", nrow(df_wide_encoded)),
#     paste("Total columns:", ncol(df_wide_encoded)),
#     paste("Ordinal columns encoded:", length(ordinal_columns)),
#     paste("Continuous columns kept:", length(continuous_columns_keep))
#   ))
#
#   return(df_wide_encoded)
# }
