#' count individual participants in longitudinal data
#'
#' @param dat a data frame containing the longitudinal data
#' @param start_wave integer. the first wave to process (default: 2009)
#' @param end_wave integer. the last wave to process (default: 2022)
#' @param prev_wave_counts integer vector. previous wave thresholds to count (default: c(1,2,3,4))
#' @param opt_in_var character. name of the opt-in variable to track (default: "sample_frame_opt_in")
#' @param opt_in_true value indicating opted-in status (default: 1)
#' @param opt_in_false value indicating not opted-in status (default: 0)
#'
#' @return a tibble with columns for:
#'   - wave: survey wave number
#'   - n_total: cumulative unique participants through current wave
#'   - n_active: active participants in current wave
#'   - n_deceased: newly deceased in current wave
#'   - n_deceased_total: total deceased through current wave
#'   - n_returned: participants absent in previous wave but present in earlier waves
#'   - n_returned_total: total returnees through current wave
#'   - n_opt_in: newly opted-in participants in current wave
#'   - n_opt_in_total: total opted-in participants through current wave
#'   - n_wave_1plus: participants in 1+ previous waves
#'   - n_wave_2plus: participants in 2+ previous waves
#'   - n_wave_3plus: participants in 3+ previous waves
#'   - n_wave_4plus: participants in 4+ previous waves
#'
#' @importFrom dplyr filter mutate pull anti_join bind_rows %>%
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom rlang sym !!
#'
#' @export
margot_count_ids <- function(dat,
                             start_wave = 2009,
                             end_wave = 2022,
                             prev_wave_counts = c(1,2,3,4),
                             opt_in_var = "sample_frame_opt_in",
                             opt_in_true = 1,
                             opt_in_false = 0) {

  cli::cli_alert_info("Starting participant count calculation")
  results <- tibble::tibble()

  # validate opt_in variable exists if needed
  if(opt_in_var %in% names(dat)) {
    track_opt_ins <- TRUE
  } else {
    track_opt_ins <- FALSE
    cli::cli_alert_warning(sprintf("opt_in variable '%s' not found in data - opt-in tracking disabled", opt_in_var))
  }

  # ensure wave is numeric
  dat <- dat %>%
    dplyr::mutate(wave = as.numeric(as.character(wave)))

  # initialize tracking
  all_waves <- seq(start_wave, end_wave)
  wave_history <- matrix(0, nrow = 0, ncol = length(all_waves))
  colnames(wave_history) <- as.character(all_waves)
  participant_ids <- character(0)
  deceased_ids <- character(0)
  returned_ids <- character(0)
  opted_in_ids <- character(0)

  # validate prev_wave_counts
  if(!is.numeric(prev_wave_counts) || any(prev_wave_counts < 1)) {
    stop("prev_wave_counts must be a numeric vector with values >= 1")
  }
  prev_wave_counts <- sort(unique(prev_wave_counts))

  for(wave in all_waves) {
    cli::cli_alert_info(paste("Processing wave:", wave))

    # get current wave data
    wave_data <- dat %>%
      dplyr::filter(wave == !!wave)

    # count participants by status
    active_ids <- wave_data %>%
      dplyr::filter(year_measured == 1) %>%
      dplyr::pull(id) %>%
      unique()

    newly_deceased <- wave_data %>%
      dplyr::filter(year_measured == -1) %>%
      dplyr::pull(id) %>%
      unique()

    # track opt-ins for current wave if variable exists
    if(track_opt_ins) {
      new_opt_ins <- wave_data %>%
        dplyr::filter(year_measured == 1,
                      !!rlang::sym(opt_in_var) == opt_in_true) %>%
        dplyr::anti_join(tibble::tibble(id = opted_in_ids), by = "id") %>%
        dplyr::pull(id) %>%
        unique()

      opted_in_ids <- union(opted_in_ids, new_opt_ins)
    } else {
      new_opt_ins <- character(0)
    }

    # update tracking with current wave participants
    participant_ids <- union(participant_ids, c(active_ids, newly_deceased))
    deceased_ids <- union(deceased_ids, newly_deceased)

    # calculate cumulative total through this wave
    cumulative_total <- dat %>%
      dplyr::filter(wave <= !!wave, year_measured %in% c(1, -1)) %>%
      dplyr::pull(id) %>%
      unique() %>%
      length()

    # identify returnees (present in earlier waves but not previous wave)
    if(wave > start_wave) {
      # get previous wave participants
      prev_wave_actives <- dat %>%
        dplyr::filter(wave == (!!wave - 1), year_measured == 1) %>%
        dplyr::pull(id) %>%
        unique()

      # get participants from waves before the previous wave
      earlier_participants <- dat %>%
        dplyr::filter(wave < (!!wave - 1), year_measured == 1) %>%
        dplyr::pull(id) %>%
        unique()

      # find returnees: active now, not in previous wave, but in earlier waves
      new_returnees <- intersect(
        setdiff(active_ids, prev_wave_actives), # not in previous wave
        earlier_participants  # but were in earlier waves
      )

      returned_ids <- union(returned_ids, new_returnees)
    } else {
      new_returnees <- character(0)
    }

    # update wave history matrix
    new_ids <- setdiff(active_ids, rownames(wave_history))
    if(length(new_ids) > 0) {
      new_rows <- matrix(0, nrow = length(new_ids), ncol = length(all_waves))
      colnames(new_rows) <- colnames(wave_history)
      rownames(new_rows) <- new_ids
      wave_history <- rbind(wave_history, new_rows)
    }

    # mark current wave in history
    wave_idx <- which(all_waves == wave)
    active_rows <- match(active_ids, rownames(wave_history))
    wave_history[active_rows, wave_idx] <- 1

    # compute previous wave appearances
    participants_in_prev_waves <- list()
    if(wave > start_wave) {
      active_rows <- match(active_ids, rownames(wave_history))
      previous_waves <- wave_history[active_rows, 1:(wave_idx-1), drop = FALSE]
      wave_counts <- rowSums(previous_waves)

      for(count in prev_wave_counts) {
        participants_in_prev_waves[[paste0("prev_", count)]] <-
          sum(wave_counts >= count)
      }
    } else {
      for(count in prev_wave_counts) {
        participants_in_prev_waves[[paste0("prev_", count)]] <- 0
      }
    }

    # create results for current wave with new naming convention
    temp_tibble <- tibble::tibble(
      wave = wave,
      n_total = cumulative_total,
      n_active = length(active_ids),
      n_deceased = length(newly_deceased),
      n_deceased_total = length(deceased_ids),
      n_returned = length(new_returnees),
      n_returned_total = length(returned_ids),
      n_opt_in = length(new_opt_ins),
      n_opt_in_total = length(opted_in_ids)
    )

    # add previous wave counts with new naming convention
    for(count in prev_wave_counts) {
      temp_tibble[[paste0("n_wave_", count, "plus")]] <-
        participants_in_prev_waves[[paste0("prev_", count)]]
    }

    results <- dplyr::bind_rows(results, temp_tibble)
    cli::cli_alert_success(paste("Completed processing for wave:", wave))
  }

  # add variable labels using attributes
  attr(results, "variable.labels") <- c(
    wave = "Survey Wave",
    n_total = "Total Unique Participants (Cumulative)",
    n_active = "Active Participants",
    n_deceased = "Newly Deceased",
    n_deceased_total = "Total Deceased",
    n_returned = "Returned After Absence (Present in Earlier Waves)",
    n_returned_total = "Total Historical Returns",
    n_opt_in = "New Opt-ins",
    n_opt_in_total = "Total Opt-ins",
    n_wave_1plus = "In 1+ Previous Waves",
    n_wave_2plus = "In 2+ Previous Waves",
    n_wave_3plus = "In 3+ Previous Waves",
    n_wave_4plus = "In 4+ Previous Waves"
  )

  cli::cli_alert_success("Participant count calculation completed \U0001F44D")
  return(results)
}

#' debugging function to include opt-in checking
#'
#' @keywords internal
#' @noRd
# debug_participant_counts <- function(dat, results_df, opt_in_var = "sample_frame_opt_in", opt_in_true = 1) {
#   # get all unique ids from raw data
#   all_unique_ids <- unique(dat$id)
#
#   # get final cumulative count from results
#   final_cumulative <- tail(results_df$total_participants_cum, 1)
#   final_opt_ins <- tail(results_df$opt_ins_cum, 1)
#
#   # basic comparison
#   comparison <- tibble::tibble(
#     total_unique_ids = length(all_unique_ids),
#     final_cumulative = final_cumulative,
#     difference = length(all_unique_ids) - final_cumulative,
#     total_opt_ins = sum(dat[[opt_in_var]] == opt_in_true, na.rm = TRUE),
#     final_opt_ins_cum = final_opt_ins
#   )
#
#   # find ids that are counted vs not counted
#   ids_in_measured <- dat %>%
#     dplyr::filter(year_measured %in% c(1, -1)) %>%
#     dplyr::pull(id) %>%
#     unique()
#
#   ids_only_zero <- setdiff(all_unique_ids, ids_in_measured)
#
#   # detailed analysis of potentially problematic ids
#   problem_cases <- dat %>%
#     dplyr::filter(id %in% ids_only_zero) %>%
#     dplyr::group_by(id) %>%
#     dplyr::summarize(
#       all_waves = paste(sort(unique(wave)), collapse = ", "),
#       all_measured_values = paste(sort(unique(year_measured)), collapse = ", "),
#       all_opt_in_values = paste(sort(unique(!!rlang::sym(opt_in_var))), collapse = ", "),
#       n_waves = dplyr::n_distinct(wave),
#       .groups = 'drop'
#     )
#
#   return(list(
#     summary = comparison,
#     n_zero_only_ids = length(ids_only_zero),
#     problem_cases = problem_cases
#   ))
# }
