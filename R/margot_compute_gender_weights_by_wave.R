#' Compute Gender-Based Sample Weights Using Baseline Wave Proportions
#'
#' Compute sample weights for gender adjustment based on the baseline wave proportions.
#'
#' The function calculates the gender proportions in the baseline wave and computes weights so that
#' the overall sample aligns with the target gender distribution. The same weights are then applied to all rows.
#'
#' @param data A data frame containing gender and wave information.
#' @param male_col A character string specifying the column that indicates male gender (1 for male, 0 for female). Default is \code{"male"}.
#' @param wave_col A character string specifying the column indicating the wave. Default is \code{"wave"}.
#' @param target_wave The value in \code{wave_col} that identifies the baseline wave.
#' @param target_male_prop A numeric value between 0 and 1 representing the target proportion of males. Default is 0.5.
#'
#' @return A numeric vector of sample weights for all rows.
#'
#' @details
#' The function computes the sample proportions in the baseline wave and calculates weights by comparing these
#' proportions with the target proportions. It upweights the underrepresented gender and downweights the overrepresented gender.
#' The resulting weights are applied to the full dataset.
#'
#' @examples
#' dat <- data.frame(
#'   id = 1:100,
#'   male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
#'   wave = rep(1:2, each = 50)
#' )
#' weights <- margot_compute_gender_weights_by_wave(dat, male_col = "male",
#'                                                  wave_col = "wave",
#'                                                  target_wave = 1,
#'                                                  target_male_prop = 0.52)
#' head(weights)
#'
#' @export
margot_compute_gender_weights_by_wave <- function(data, male_col = "male", wave_col = "wave", target_wave, target_male_prop = 0.5) {
  if (!male_col %in% names(data)) {
    stop("The specified male column does not exist in the data.")
  }
  if (!wave_col %in% names(data)) {
    stop("The specified wave column does not exist in the data.")
  }

  # Coerce wave values to character if necessary
  if (is.factor(data[[wave_col]]) || is.character(data[[wave_col]])) {
    wave_values <- as.character(data[[wave_col]])
    target_wave <- as.character(target_wave)
  } else {
    wave_values <- data[[wave_col]]
  }

  baseline_idx <- wave_values == target_wave
  if (sum(baseline_idx, na.rm = TRUE) == 0) {
    stop("No data found for the specified baseline wave.")
  }

  baseline_data <- data[baseline_idx, ]

  prop_male_sample <- mean(baseline_data[[male_col]], na.rm = TRUE)
  prop_female_sample <- 1 - prop_male_sample

  weight_male <- target_male_prop / prop_male_sample
  weight_female <- (1 - target_male_prop) / prop_female_sample

  weights <- ifelse(data[[male_col]] == 1, weight_male, weight_female)

  return(weights)
}
