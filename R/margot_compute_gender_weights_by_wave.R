#' Compute Gender-Based Sample Weights by Wave
#'
#' @param data A data frame containing the gender and wave information.
#' @param male_col Character string specifying the name of the column in `data` that indicates male gender (1 for male, 0 for female). Default is "male".
#' @param wave_col Character string specifying the name of the column in `data` that indicates the wave or group for weighting. Default is "wave".
#' @param target_male_prop Numeric value between 0 and 1 specifying the target proportion of males in the population. Default is 0.5 (50\% male).
#'
#' @return A numeric vector of sample weights. Each weight corresponds to a row in the input data frame.
#'
#' @description
#' This function computes sample weights based on gender for each wave to achieve a target gender balance in the population.
#' It assumes a binary gender classification where 1 represents male and 0 represents female.
#'
#' @details
#' The function requires the `dplyr` package and calculates weights that, when applied, will adjust the sample within each wave to match the specified target gender proportion.
#' It upweights the underrepresented gender and downweights the overrepresented gender within each wave.
#' The function will return an error if `target_male_prop` is not between 0 and 1 or if the gender column does not contain binary values.
#' Missing values in the gender or wave column will be excluded from weight calculations.
#'
#' @examples
#' # Load the dplyr package
#' library(dplyr)
#'
#' # Create a sample dataset
#' dat <- data.frame(
#'   id = 1:100,
#'   male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
#'   wave = rep(1:2, each = 50)
#' )
#'
#' # Compute weights by wave
#' weights <- margot_compute_gender_weights_by_wave(
#'   dat,
#'   male_col = "male",
#'   wave_col = "wave",
#'   target_male_prop = 0.5
#' )
#'
#' # Check weight distribution by wave
#' table(round(weights, 3))
#'
#' @export
margot_compute_gender_weights_by_wave <- function(data, male_col = "male", wave_col = "wave", target_male_prop = 0.5) {
  # check if target_male_prop is between 0 and 1
  if (target_male_prop < 0 || target_male_prop > 1) {
    stop("target_male_prop must be between 0 and 1.")
  }

  # check if the required columns exist in the data
  if (!all(c(male_col, wave_col) %in% names(data))) {
    stop("one or more specified columns do not exist in the data.")
  }

  # check if the male column contains only binary values
  if (!all(data[[male_col]] %in% c(0, 1, NA))) {
    stop("the male column must contain only binary values (0 or 1).")
  }

  # group by wave and compute weights
  data |>
    dplyr::filter(!is.na(!!rlang::ensym(male_col))) |>
    dplyr::group_by(!!rlang::ensym(wave_col)) |>
    dplyr::mutate(weight = {
      prop_male_sample <- mean(!!rlang::ensym(male_col), na.rm = TRUE)
      prop_female_sample <- 1 - prop_male_sample
      prop_male_population <- target_male_prop
      prop_female_population <- 1 - target_male_prop

      gender_weight_male <- prop_male_population / prop_male_sample
      gender_weight_female <- prop_female_population / prop_female_sample

      ifelse(!!rlang::ensym(male_col) == 1, gender_weight_male, gender_weight_female)
    }) |>
    dplyr::ungroup() |>
    dplyr::pull(weight)
}
