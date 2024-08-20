#' Create Summary Tables for Longitudinal Data with Rich Reporting and Console Output
#'
#' This function creates three summary tables (baseline, exposure, and outcome)
#' for longitudinal data using gtsummary and janitor, provides additional summary statistics,
#' and prints the tables directly to the console using cli.
#'
#' @param data A data frame containing the longitudinal data.
#' @param baseline_wave A string specifying the baseline wave (e.g., "2020").
#' @param exposure_waves A vector of strings or a single comma-separated string
#'   specifying the exposure wave(s) (e.g., c("2021", "2022") or "2021,2022").
#' @param outcome_wave A string specifying the outcome wave (e.g., "2022").
#' @param name_exposure A string specifying the name of the exposure variable.
#' @param name_exposure_cat Optional. A string specifying the name of the
#'   categorical exposure variable. Default is NULL.
#' @param baseline_vars A vector of strings specifying the baseline variables.
#' @param outcome_vars A vector of strings specifying the outcome variables.
#' @param extra_vars Optional. A vector of strings specifying additional variables
#'   to include. Default is c("id", "wave", "year_measured", "not_lost", "sample_weights").
#' @param baseline_labels Optional. A named list of custom labels for baseline variables.
#' @param exposure_labels Optional. A named list of custom labels for exposure variables.
#' @param outcome_labels Optional. A named list of custom labels for outcome variables.
#'
#' @return A list containing summary tables and additional information.
#'
#' @import dplyr
#' @import gtsummary
#' @import janitor
#' @import labelled
#' @import cli
#' @import crayon
#'
#' @export
margot_summary_tables <- function(data,
                                  baseline_wave,
                                  exposure_waves,
                                  outcome_wave,
                                  name_exposure,
                                  name_exposure_cat = NULL,
                                  baseline_vars,
                                  outcome_vars,
                                  extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
                                  baseline_labels = NULL,
                                  exposure_labels = NULL,
                                  outcome_labels = NULL) {

  cli::cli_h1("Margot Summary Tables")

  # Start progress bar
  cli::cli_progress_bar("Generating summary tables", total = 5)

  # handle exposure_waves input
  if (length(exposure_waves) == 1 && grepl(",", exposure_waves)) {
    exposure_waves <- unlist(strsplit(exposure_waves, ","))
  }
  exposure_waves <- trimws(exposure_waves)  # Remove any whitespace

  # sort variables
  baseline_vars <- sort(baseline_vars)
  outcome_vars <- sort(outcome_vars)

  cli::cli_progress_update()

  # calculate number of unique participants
  n_participants <- length(unique(data$id))
  n_participants_formatted <- format(n_participants, big.mark = ",")

  cli::cli_alert_success("Number of unique participants: {.val {n_participants_formatted}}")

  # baseline table
  dt_baseline <- data |>
    filter(wave == baseline_wave) |>
    select(all_of(baseline_vars)) |>
    droplevels()

  table_baseline <- dt_baseline |>
    clean_names(case = "title") |>
    tbl_summary(
      missing = "ifany",
      percent = "column",
      statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
      type = list(all_continuous() ~ "continuous2"),
      label = baseline_labels
    ) |>
    modify_header(label = "**Exposure + Demographic Variables**") |>
    bold_labels()

  cli::cli_progress_update()

  # exposure table and summary
  exposure_vars <- c(name_exposure)
  if (!is.null(name_exposure_cat)) {
    exposure_vars <- c(exposure_vars, name_exposure_cat)
  }

  dt_exposure <- data |>
    filter(wave %in% c(baseline_wave, exposure_waves)) |>
    select(all_of(c(exposure_vars, "wave"))) |>
    droplevels()

  table_exposures <- dt_exposure |>
    clean_names(case = "title") |>
    labelled::to_factor() |>
    tbl_summary(
      by = "Wave",
      missing = "always",
      percent = "column",
      label = exposure_labels
    ) |>
    modify_header(label = "**Exposure Variables by Wave**") |>
    bold_labels()

  cli::cli_progress_update()

  # calculate exposure summary for each wave
  exposure_summary <- lapply(c(baseline_wave, exposure_waves), function(wave) {
    wave_data <- dt_exposure |> filter(wave == !!wave)
    list(
      wave = wave,
      mean_exposure = mean(wave_data[[name_exposure]], na.rm = TRUE),
      sd_exposure = sd(wave_data[[name_exposure]], na.rm = TRUE),
      median_exposure = median(wave_data[[name_exposure]], na.rm = TRUE)
    )
  })

  cli::cli_progress_update()

  # outcome table
  dt_outcome <- data |>
    filter(wave %in% c(baseline_wave, outcome_wave)) |>
    select(all_of(c(outcome_vars, "wave"))) |>
    droplevels()

  table_outcomes <- dt_outcome |>
    clean_names(case = "title") |>
    labelled::to_factor() |>
    tbl_summary(
      by = "Wave",
      missing = "always",
      percent = "column",
      label = outcome_labels
    ) |>
    modify_header(label = "**Outcome Variables by Wave**") |>
    bold_labels()

  cli::cli_progress_update()

  # Function to print tables using cli
  print_cli_table <- function(table, title) {
    cli::cli_h2(title)
    cli::cli_text(capture.output(print(table)))
    cli::cli_text("")  # Add a blank line after the table
  }

  # Print tables to console
  print_cli_table(table_baseline, "Baseline Table")
  print_cli_table(table_exposures, "Exposure Table")
  print_cli_table(table_outcomes, "Outcome Table")

  # Display summary information
  cli::cli_h2("Summary Information")
  cli::cli_ul()
  cli::cli_li("Baseline wave: {.val {baseline_wave}}")
  cli::cli_li("Exposure waves: {.val {paste(exposure_waves, collapse = ', ')}}")
  cli::cli_li("Outcome wave: {.val {outcome_wave}}")
  cli::cli_li("Exposure variable: {.val {name_exposure}}")
  if (!is.null(name_exposure_cat)) {
    cli::cli_li("Categorical exposure variable: {.val {name_exposure_cat}}")
  }
  cli::cli_end()

  # Display exposure summary
  cli::cli_h2("Exposure Summary")
  for (summary in exposure_summary) {
    cli::cli_alert_info(
      "Wave {.val {summary$wave}}: " %>%
        crayon::bold() %>%
        crayon::blue()
    )
    cli::cli_ul()
    cli::cli_li("Mean: {.val {round(summary$mean_exposure, 2)}}")
    cli::cli_li("SD: {.val {round(summary$sd_exposure, 2)}}")
    cli::cli_li("Median: {.val {round(summary$median_exposure, 2)}}")
    cli::cli_end()
  }

  # Close progress bar
  cli::cli_progress_done()

  # Return results
  cli::cli_alert_success("Summary tables and information generated successfully!")
  return(list(
    baseline_table = table_baseline,
    exposure_table = table_exposures,
    outcome_table = table_outcomes,
    n_participants = n_participants_formatted,
    exposure_summary = exposure_summary
  ))
}
# margot_summary_tables <- function(data,
#                                   baseline_wave,
#                                   exposure_waves,
#                                   outcome_wave,
#                                   name_exposure,
#                                   name_exposure_cat = NULL,
#                                   baseline_vars,
#                                   outcome_vars,
#                                   extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
#                                   baseline_labels = NULL,
#                                   exposure_labels = NULL,
#                                   outcome_labels = NULL) {
#
#   # handle exposure_waves input
#   if (length(exposure_waves) == 1 && grepl(",", exposure_waves)) {
#     exposure_waves <- unlist(strsplit(exposure_waves, ","))
#   }
#   exposure_waves <- trimws(exposure_waves)  # Remove any whitespace
#
#   # sort variables
#   baseline_vars <- sort(baseline_vars)
#   outcome_vars <- sort(outcome_vars)
#
#   # calculate number of unique participants
#   n_participants <- length(unique(data$id))
#   n_participants <- format(n_participants, big.mark = ",")
#
#   # baseline table
#   dt_baseline <- data |>
#     filter(wave == baseline_wave) |>
#     select(all_of(baseline_vars)) |>
#     droplevels()
#
#   table_baseline <- dt_baseline |>
#     clean_names(case = "title") |>
#     tbl_summary(
#       missing = "ifany",
#       percent = "column",
#       statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
#       type = list(all_continuous() ~ "continuous2"),
#       label = baseline_labels
#     ) |>
#     modify_header(label = "**Exposure + Demographic Variables**") |>
#     bold_labels()
#
#   # exposure table and summary
#   exposure_vars <- c(name_exposure)
#   if (!is.null(name_exposure_cat)) {
#     exposure_vars <- c(exposure_vars, name_exposure_cat)
#   }
#
#   dt_exposure <- data |>
#     filter(wave %in% c(baseline_wave, exposure_waves)) |>
#     select(all_of(c(exposure_vars, "wave"))) |>
#     droplevels()
#
#   table_exposures <- dt_exposure |>
#     clean_names(case = "title") |>
#     labelled::to_factor() |>
#     tbl_summary(
#       by = "Wave",
#       missing = "always",
#       percent = "column",
#       label = exposure_labels
#     ) |>
#     modify_header(label = "**Exposure Variables by Wave**") |>
#     bold_labels()
#
#   # calculate exposure summary for each wave
#   exposure_summary <- lapply(c(baseline_wave, exposure_waves), function(wave) {
#     wave_data <- dt_exposure |> filter(wave == !!wave)
#     list(
#       wave = wave,
#       mean_exposure = mean(wave_data[[name_exposure]], na.rm = TRUE),
#       sd_exposure = sd(wave_data[[name_exposure]], na.rm = TRUE),
#       median_exposure = median(wave_data[[name_exposure]], na.rm = TRUE)
#     )
#   })
#
#   # outcome table
#   dt_outcome <- data |>
#     filter(wave %in% c(baseline_wave, outcome_wave)) |>
#     select(all_of(c(outcome_vars, "wave"))) |>
#     droplevels()
#
#   table_outcomes <- dt_outcome |>
#     clean_names(case = "title") |>
#     labelled::to_factor() |>
#     tbl_summary(
#       by = "Wave",
#       missing = "always",
#       percent = "column",
#       label = outcome_labels
#     ) |>
#     modify_header(label = "**Outcome Variables by Wave**") |>
#     bold_labels()
#
#   # list of tables and additional information
#   return(list(
#     baseline_table = table_baseline,
#     exposure_table = table_exposures,
#     outcome_table = table_outcomes,
#     n_participants = n_participants,
#     exposure_summary = exposure_summary
#   ))
# }
