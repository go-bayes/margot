#' Generate Summary Panel for Margot Study
#'
#' This function creates summary tables for a panel study, including unique IDs by wave,
#' participant wave summary, and grouped participant wave summary.
#'
#' @param data A data frame containing the panel study data.
#' @param output_format Character string specifying the output format: "markdown" (default) or "kable".
#' @param group_waves_at Numeric value specifying at which number of waves to start grouping (default is 3).
#' @param id_col Character string specifying the name of the ID column (default is "id").
#' @param wave_col Character string specifying the name of the wave column (default is "wave").
#' @param year_measured_col Character string specifying the name of the year measured column (default is "year_measured").
#'
#' @return A list containing three elements: unique_ids_by_wave, participant_wave_summary, and participant_wave_summary_grouped.
#'
#' @importFrom dplyr filter group_by summarize ungroup mutate n_distinct
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @import cli
#' @importFrom kableExtra kable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat' is your dataset
#' results <- margot_summary_panel(dat)
#'
#' # Custom settings
#' custom_results <- margot_summary_panel(
#'   data = dat,
#'   output_format = "markdown",
#'   group_waves_at = 4,
#'   id_col = "participant_id",
#'   wave_col = "survey_wave",
#'   year_measured_col = "measurement_year"
#' )
#'
#' # View results
#' results$unique_ids_by_wave
#' results$participant_wave_summary
#' results$participant_wave_summary_grouped
#'
#' # For markdown output
#' cat(custom_results$unique_ids_by_wave)
#' cat(custom_results$participant_wave_summary)
#' cat(custom_results$participant_wave_summary_grouped)
#' }
margot_summary_panel <- function(data,
                                 output_format = "markdown",
                                 group_waves_at = 3,
                                 id_col = "id",
                                 wave_col = "wave",
                                 year_measured_col = "year_measured") {
  cli::cli_alert_info("Starting margot_summary_panel function")

  required_cols <- c(id_col, wave_col, year_measured_col)
  if (!all(required_cols %in% colnames(data))) {
    cli::cli_alert_danger("Missing required columns!")
    stop(paste("Data must contain columns:", paste(required_cols, collapse = ", ")))
  }

  create_kable_table <- function(df, caption) {
    kable_table <- df %>%
      kable("markdown", caption = caption) %>%
      return(kable_table)
  }

  cli::cli_alert_success("Calculating unique IDs by wave")

  df_unique_ids <- data %>%
    filter(!!sym(year_measured_col) == 1) %>%
    group_by(!!sym(wave_col)) %>%
    summarize(number = n_distinct(!!sym(id_col)))

  table1 <- create_kable_table(df_unique_ids, "Unique IDs by Wave\nYear given starts in October and runs to October the following year")

  cli::cli_alert_success("Calculating participant wave summary")

  id_wave_counts <- data %>%
    filter(!!sym(year_measured_col) == 1) %>%
    group_by(!!sym(id_col)) %>%
    summarize(wave_count = n_distinct(!!sym(wave_col))) %>%
    ungroup()

  wave_summary <- id_wave_counts %>%
    group_by(wave_count) %>%
    summarize(number_of_participants = n())

  table2 <- create_kable_table(wave_summary, "Participant Wave Summary\nNumber of participants by the number of waves they participated in")

  cli::cli_alert_success("Calculating grouped participant wave summary")

  wave_summary_grouped <- id_wave_counts %>%
    mutate(wave_count = if_else(wave_count >= group_waves_at,
      paste(group_waves_at, "or more"),
      as.character(wave_count)
    )) %>%
    group_by(wave_count) %>%
    summarize(number_of_participants = n()) %>%
    ungroup()

  table3 <- create_kable_table(wave_summary_grouped, paste("Participant Wave Summary (Grouped at", group_waves_at, "or more)\nNumber of participants by the number of waves they participated in"))

  cli::cli_alert_success("Function completed successfully!")
  cli::cli_alert_info(paste("Total unique participants:", nrow(id_wave_counts)))
  cli::cli_alert_info(paste("Number of waves:", n_distinct(data[[wave_col]])))

  cli::cli_alert("Analysis complete \U0001F44D")

  return(list(
    unique_ids_by_wave = table1,
    participant_wave_summary = table2,
    participant_wave_summary_grouped = table3
  ))
}
