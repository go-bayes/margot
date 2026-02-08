#' Prepare Panel Data for Timeline Visualization
#'
#' This function prepares panel data for timeline visualization across multiple waves.
#'
#' @param dat A data frame containing the panel data. Must include columns for wave, time score, and participant ID.
#' @param wave_col The name of the column containing wave information. Default is "wave".
#' @param tscore_col The name of the column containing time score information. Default is "tscore".
#' @param id_col The name of the column containing participant IDs. Default is "id".
#' @param base_date The base date for calculating the timeline. Default is "1970-01-01".
#' @param wave_breaks A named list of date ranges for each wave. If NULL, waves will not be categorized.
#'
#' @return A list containing two elements:
#'   \item{df_timeline}{A data frame with one row per response, containing columns `day` (Date) and optionally `wave` (factor)}
#'   \item{n_total_participants}{The total number of unique participants in the dataset}
#'
#' @examples
#' \dontrun{
#' dat <- read.csv("panel_data.csv")
#' wave_breaks <- list(
#'   "wave 1" = c(as.Date("2010-01-01"), as.Date("2010-12-31")),
#'   "wave 2" = c(as.Date("2011-01-01"), as.Date("2011-12-31"))
#' )
#' prepared_data <- prepare_panel_data(dat,
#'   wave_col = "Wave", tscore_col = "TimeScore",
#'   id_col = "ParticipantID", base_date = "2010-01-01",
#'   wave_breaks = wave_breaks
#' )
#' }
#'
#' @importFrom dplyr mutate count case_when arrange
#' @importFrom lubridate make_date floor_date
#' @importFrom cli cli_alert_info cli_alert_success
#'
#' @export
prepare_panel_data <- function(dat, wave_col = "wave", tscore_col = "tscore", id_col = "id",
                               base_date = as.Date("1970-01-01"), wave_breaks = NULL) {
  cli::cli_alert_info("Starting to prepare panel data...")

  df_timeline <- dat %>%
    dplyr::mutate(
      timeline = lubridate::make_date(
        year = lubridate::year(base_date),
        month = lubridate::month(base_date),
        day = lubridate::day(base_date)
      ) + !!sym(tscore_col),
      day = lubridate::floor_date(timeline, "day")
    ) %>%
    dplyr::select(day)

  if (!is.null(wave_breaks)) {
    # assign wave labels using direct date comparison (avoids !! date coercion issues)
    wave_labels <- rep(NA_character_, nrow(df_timeline))
    for (wn in names(wave_breaks)) {
      r <- wave_breaks[[wn]]
      idx <- df_timeline$day >= r[1] & df_timeline$day <= r[2]
      wave_labels[idx] <- wn
    }
    df_timeline$wave <- factor(wave_labels, levels = names(wave_breaks))
  }

  df_timeline <- df_timeline %>% dplyr::arrange(day)

  cli::cli_alert_success("Timeline data prepared successfully.")

  n_total_participants <- length(unique(dat[[id_col]]))
  cli::cli_alert_info(paste("Total number of unique participants:", n_total_participants))

  result <- list(df_timeline = df_timeline, n_total_participants = n_total_participants)

  cli::cli_alert_success("Panel data preparation complete")

  return(result)
}
