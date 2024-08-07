#' Create Summary Tables for Longitudinal Data
#'
#' This function creates three summary tables (baseline, exposure, and outcome)
#' for longitudinal data using gtsummary and janitor, and provides additional summary statistics.
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
#' @return A list containing:
#'   \item{baseline_table}{Summary of baseline variables}
#'   \item{exposure_table}{Summary of exposure variables by wave}
#'   \item{outcome_table}{Summary of outcome variables by wave}
#'   \item{n_participants}{Total number of unique participants}
#'   \item{exposure_summary}{A list of exposure summaries for each wave}
#'
#' @import dplyr
#' @import gtsummary
#' @import janitor
#' @import labelled
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

  # Handle exposure_waves input
  if (length(exposure_waves) == 1 && grepl(",", exposure_waves)) {
    exposure_waves <- unlist(strsplit(exposure_waves, ","))
  }
  exposure_waves <- trimws(exposure_waves)  # Remove any whitespace

  # Sort variables
  baseline_vars <- sort(baseline_vars)
  outcome_vars <- sort(outcome_vars)

  # Calculate number of unique participants
  n_participants <- length(unique(data$id))
  n_participants <- format(n_participants, big.mark = ",")

  # Baseline table
  dt_baseline <- data %>%
    filter(wave == baseline_wave) %>%
    select(all_of(baseline_vars)) %>%
    droplevels()

  table_baseline <- dt_baseline %>%
    clean_names(case = "title") %>%
    tbl_summary(
      missing = "ifany",
      percent = "column",
      statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
      type = list(all_continuous() ~ "continuous2"),
      label = baseline_labels
    ) %>%
    modify_header(label = "**Exposure + Demographic Variables**") %>%
    bold_labels()

  # Exposure table and summary
  exposure_vars <- c(name_exposure)
  if (!is.null(name_exposure_cat)) {
    exposure_vars <- c(exposure_vars, name_exposure_cat)
  }

  dt_exposure <- data %>%
    filter(wave %in% c(baseline_wave, exposure_waves)) %>%
    select(all_of(c(exposure_vars, "wave"))) %>%
    droplevels()

  table_exposures <- dt_exposure %>%
    clean_names(case = "title") %>%
    to_factor() %>%
    tbl_summary(
      by = "Wave",
      missing = "always",
      percent = "column",
      label = exposure_labels
    ) %>%
    modify_header(label = "**Exposure Variables by Wave**") %>%
    bold_labels()

  # Calculate exposure summary for each wave
  exposure_summary <- lapply(c(baseline_wave, exposure_waves), function(wave) {
    wave_data <- dt_exposure %>% filter(wave == !!wave)
    list(
      wave = wave,
      mean_exposure = mean(wave_data[[name_exposure]], na.rm = TRUE),
      sd_exposure = sd(wave_data[[name_exposure]], na.rm = TRUE),
      median_exposure = median(wave_data[[name_exposure]], na.rm = TRUE)
    )
  })

  # Outcome table
  dt_outcome <- data %>%
    filter(wave %in% c(baseline_wave, outcome_wave)) %>%
    select(all_of(c(outcome_vars, "wave"))) %>%
    droplevels()

  table_outcomes <- dt_outcome %>%
    clean_names(case = "title") %>%
    to_factor() %>%
    tbl_summary(
      by = "Wave",
      missing = "always",
      percent = "column",
      label = outcome_labels
    ) %>%
    modify_header(label = "**Outcome Variables by Wave**") %>%
    bold_labels()

  # Return list of tables and additional information
  return(list(
    baseline_table = table_baseline,
    exposure_table = table_exposures,
    outcome_table = table_outcomes,
    n_participants = n_participants,
    exposure_summary = exposure_summary
  ))
}
