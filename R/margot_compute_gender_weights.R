#' Compute Gender-Based Sample Weights
#'
#' @param data A data frame containing the gender information.
#' @param male_col Character string specifying the name of the column in `data` that indicates male gender (1 for male, 0 for female). Default is "male".
#' @param target_male_prop Numeric value between 0 and 1 specifying the target proportion of males in the population. Default is 0.5 (50\% male).
#'
#' @return A numeric vector of sample weights. Each weight corresponds to a row in the input data frame.
#'
#' @description
#' This function computes sample weights based on gender to achieve a target gender balance in the population.
#' It assumes a binary gender classification where 1 represents male and 0 represents female.
#'
#' @details
#' The function calculates weights that, when applied, will adjust the sample to match the specified target gender proportion.
#' It upweights the underrepresented gender and downweights the overrepresented gender.
#'
#' @examples
#' # Create a sample dataset
#' dat <- data.frame(id = 1:100, male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)))
#'
#' # Compute weights
#' weights <- margot_compute_gender_weights(dat, male_col = "male", target_male_prop = 0.5)
#'
#' # Check weight distribution
#' table(round(weights, 3))
#'
#' @export
margot_compute_gender_weights <- function(data, male_col = "male", target_male_prop = 0.5) {
  # Check if the male column exists in the data
  if (!male_col %in% names(data)) {
    stop("The specified male column does not exist in the data.")
  }

  # Set target proportions
  prop_male_population <- target_male_prop
  prop_female_population <- 1 - target_male_prop

  # Calculate sample proportions
  prop_male_sample <- mean(data[[male_col]])
  prop_female_sample <- 1 - prop_male_sample

  # Calculate weights
  gender_weight_male <- prop_male_population / prop_male_sample
  gender_weight_female <- prop_female_population / prop_female_sample

  # Assign weights
  weights <- ifelse(data[[male_col]] == 1, gender_weight_male, gender_weight_female)

  return(weights)
}
