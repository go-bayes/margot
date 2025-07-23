#' @title Proportion of missing data at baseline
#' @description This function calculates the proportion of missing data at a baseline wave.
#' If a wave column is present, it uses the lowest number or lowest factor level as the baseline.
#' If no wave column is found, it issues a warning and uses the entire dataset.
#'
#' @param data A data frame containing the dataset.
#' @param wave_col A character string. Name of the column that indicates the wave.
#' The default is "wave".
#'
#' @return A numeric value representing the proportion of missing data at the baseline wave.
#' @export
#'
#' @examples
#' # Example using a dataset with a wave column
#' # assume dat_long has a column called wave
#' margot_prop_missing(dat_long)
#'
#' # Example using a dataset without a wave column
#' # assume some_data is a dataset with no wave column
#' margot_prop_missing(some_data)
margot_prop_missing <- function(data, wave_col = "wave") {

  if (!wave_col %in% names(data)) {
    warning("No wave column found. Using entire dataset.")
    data_subset <- data
  } else {
    wave_vals <- data[[wave_col]]
    if (is.factor(wave_vals)) {
      baseline_wave <- levels(wave_vals)[1]
    } else if (is.numeric(wave_vals)) {
      baseline_wave <- min(wave_vals, na.rm = TRUE)
    } else {
      baseline_wave <- sort(unique(wave_vals))[1]
    }
    data_subset <- data[data[[wave_col]] == baseline_wave, ]
  }

  # $ latex here $ proportion = \frac{\text{number missing}}{\text{total observations}}
  prop_miss_case <- round(naniar::prop_miss(data_subset), 3)

  # $$ latex here $$ \text{Proportion missing} = \frac{\sum \text{missing}}{\text{N}}

  return(prop_miss_case)
}
