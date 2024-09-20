#' Count Dyads in Longitudinal Data
#'
#' This function computes dyad counts within each wave of a longitudinal data set.
#' It can process either a single wave or multiple waves of data.
#'
#' @param dat A data frame containing the longitudinal data.
#' @param start_wave Integer. The first wave to process (default: 2009).
#' @param end_wave Integer. The last wave to process (default: 2022).
#' @param year_measured_val Integer. The value of 'year_measured' to filter on (default: 1).
#'
#' @return A tibble with columns for wave, total_dyads, singletons, complete_dyads, and unique_dyads.
#'
#' @import dplyr
#' @import tibble
#' @import cli
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a longitudinal dataset called 'my_data'
#' # with columns: wave, year_measured, rel_num_l
#'
#' # Process all waves from 2009 to 2022
#' results <- margot_count_dyads(my_data, start_wave = 2009, end_wave = 2022)
#'
#' # Process a single wave (e.g., 2015)
#' single_wave_results <- margot_count_dyads(my_data, start_wave = 2015, end_wave = 2015)
#'
#' # Process waves from 2010 to 2020 with a different year_measured_val
#' custom_results <- margot_count_dyads(my_data, start_wave = 2010, end_wave = 2020, year_measured_val = 2)
#'
#' # View the results
#' print(results)
#' }
margot_count_dyads <- function(dat, start_wave = 2009, end_wave = 2022, year_measured_val = 1) {
  cli::cli_alert_info("Starting dyad count calculation")

  results <- tibble()

  for(wave in seq(start_wave, end_wave)) {
    cli::cli_alert_info(paste("Processing wave:", wave))

    rel_count_test <- dat %>%
      filter(wave == !!wave, year_measured == year_measured_val, !is.na(rel_num_l)) %>%
      group_by(rel_num_l) %>%
      summarise(n_in_couple = n(), .groups = 'drop') %>%
      ungroup()

    rel_count_1a <- rel_count_test %>%
      filter(n_in_couple == 1)

    rel_count_2a <- rel_count_test %>%
      filter(n_in_couple == 2)

    dyad_counts <- list(
      total_dyads = nrow(rel_count_2a) + nrow(rel_count_1a),
      singletons = nrow(rel_count_1a),
      complete_dyads = nrow(rel_count_2a),
      unique_dyads = n_distinct(rel_count_2a$rel_num_l)
    )

    temp_tibble <- tibble(
      wave = wave,
      total_dyads = dyad_counts$total_dyads,
      singletons = dyad_counts$singletons,
      complete_dyads = dyad_counts$complete_dyads,
      unique_dyads = dyad_counts$unique_dyads
    )

    results <- bind_rows(results, temp_tibble)

    cli::cli_alert_success(paste("Completed processing for wave:", wave))
  }

  cli::cli_alert_success("Dyad count calculation completed \U0001F44D")
  return(results)
}
