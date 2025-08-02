#' count dyads in longitudinal data
#'
#' @param dat a data frame containing the longitudinal data.
#' @param start_wave integer. the first wave to process (default: 2009).
#' @param end_wave integer. the last wave to process (default: 2022).
#' @param year_measured_val integer. the value of 'year_measured' to filter on (default: 1).
#' @param rel_id_var character. the name of the variable indicating relationship dyad (default: "rel_num_l").
#' @param complete_var character. the name of the variable indicating complete dyad status (default: "rel_complete").
#' @param prev_wave_counts integer vector. previous wave thresholds to count (default: c(1,2,3,4)).
#'
#' @return a tibble with columns for:
#'   - wave: survey wave number
#'   - n_total: total number of dyads (complete + singleton)
#'   - n_single: number of singleton dyads
#'   - n_complete: number of complete dyads
#'   - n_wave_1plus: dyads in 1+ previous waves
#'   - n_wave_2plus: dyads in 2+ previous waves
#'   - n_wave_3plus: dyads in 3+ previous waves
#'   - n_wave_4plus: dyads in 4+ previous waves
#'
#' @importFrom dplyr filter group_by summarise ungroup bind_rows
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom rlang sym !!
#'
#' @export
margot_count_dyads <- function(dat,
                               start_wave = 2009,
                               end_wave = 2022,
                               year_measured_val = 1,
                               rel_id_var = "rel_num_l",
                               complete_var = "rel_complete",
                               prev_wave_counts = c(1, 2, 3, 4)) {
  cli::cli_alert_info("Starting dyad count calculation")
  results <- tibble::tibble()

  # init wave history matrix
  all_waves <- seq(start_wave, end_wave)
  wave_history <- matrix(0, nrow = 0, ncol = length(all_waves))
  colnames(wave_history) <- as.character(all_waves)
  dyad_ids <- character(0)

  # validate prev_wave_counts
  if (!is.numeric(prev_wave_counts) || any(prev_wave_counts < 1)) {
    stop("prev_wave_counts must be a numeric vector with values >= 1")
  }
  prev_wave_counts <- sort(unique(prev_wave_counts)) # remove duplicates and sort

  for (wave in all_waves) {
    cli::cli_alert_info(paste("Processing wave:", wave))

    # current wave processing
    wave_data <- dat %>%
      dplyr::filter(wave == !!wave, year_measured == year_measured_val, !is.na(!!rlang::sym(rel_id_var)))

    # count dyads
    rel_count_test <- wave_data %>%
      dplyr::group_by(!!rlang::sym(rel_id_var)) %>%
      dplyr::summarise(
        n_in_couple = n(),
        is_complete = sum(!!rlang::sym(complete_var), na.rm = TRUE) == 2,
        .groups = "drop"
      ) %>%
      dplyr::ungroup()

    # basic counts
    rel_count_1a <- rel_count_test %>% dplyr::filter(n_in_couple == 1)
    rel_count_2a <- rel_count_test %>% dplyr::filter(n_in_couple == 2)
    complete_dyads <- rel_count_2a %>% dplyr::filter(is_complete)
    current_complete_dyads <- complete_dyads[[rel_id_var]]

    # update wave history matrix
    new_dyads <- setdiff(current_complete_dyads, dyad_ids)
    if (length(new_dyads) > 0) {
      new_rows <- matrix(0, nrow = length(new_dyads), ncol = length(all_waves))
      colnames(new_rows) <- colnames(wave_history)
      wave_history <- rbind(wave_history, new_rows)
      dyad_ids <- c(dyad_ids, new_dyads)
    }

    # mark current wave in history
    wave_idx <- which(all_waves == wave)
    dyad_rows <- match(current_complete_dyads, dyad_ids)
    wave_history[dyad_rows, wave_idx] <- 1

    # compute previous wave appearances
    dyads_in_prev_waves <- list()
    if (wave > start_wave) {
      previous_waves <- wave_history[dyad_rows, 1:(wave_idx - 1), drop = FALSE]
      wave_counts <- rowSums(previous_waves)

      # compute counts for each specified number of previous waves
      for (count in prev_wave_counts) {
        dyads_in_prev_waves[[paste0("prev_", count)]] <-
          sum(wave_counts >= count)
      }
    } else {
      # init with zeros for first wave
      for (count in prev_wave_counts) {
        dyads_in_prev_waves[[paste0("prev_", count)]] <- 0
      }
    }

    # results for current wave with new naming convention
    temp_tibble <- tibble::tibble(
      wave = wave,
      n_total = nrow(rel_count_2a) + nrow(rel_count_1a),
      n_single = nrow(rel_count_1a),
      n_complete = nrow(complete_dyads)
    )

    # add prev wave counts with new naming convention
    for (count in prev_wave_counts) {
      temp_tibble[[paste0("n_wave_", count, "plus")]] <-
        dyads_in_prev_waves[[paste0("prev_", count)]]
    }

    results <- dplyr::bind_rows(results, temp_tibble)
    cli::cli_alert_success(paste("Completed processing for wave:", wave))
  }

  # add variable labels using attributes
  attr(results, "variable.labels") <- c(
    wave = "Survey Wave",
    n_total = "Total Dyads (Complete + Singleton)",
    n_single = "Singleton Dyads",
    n_complete = "Complete Dyads",
    n_wave_1plus = "In 1+ Previous Waves",
    n_wave_2plus = "In 2+ Previous Waves",
    n_wave_3plus = "In 3+ Previous Waves",
    n_wave_4plus = "In 4+ Previous Waves"
  )

  cli::cli_alert_success("Dyad count calculation completed \U0001F44D")
  return(results)
}
