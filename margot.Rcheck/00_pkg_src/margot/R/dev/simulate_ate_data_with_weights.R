#' Simulate Data for Average Treatment Effect (ATE) with Sample Weights
#'
#' Generates simulated data for a sample and a population to study average treatment effects (ATE),
#' considering the presence of an effect modifier. The function simulates differences in the
#' distribution of effect modifiers between the sample and the population without altering the treatment
#' effect or the effect-modification. Weights are calculated to adjust for the distribution differences
#' of the effect modifier between the sample and the population.
#'
#' @param n_sample Integer, the size of the sample. Default is 10000.
#' @param n_population Integer, the size of the population. Default is 100000.
#' @param p_z_sample Numeric, the probability of the effect modifier in the sample. Default is 0.1.
#' @param p_z_population Numeric, the probability of the effect modifier in the population. Default is 0.5.
#' @param beta_a Numeric, the treatment effect. Default is 1.
#' @param beta_z Numeric, the coefficient of the effect modifier. Default is 2.5.
#' @param beta_az Numeric, the interaction term representing the effect modification of treatment by the effect modifier. Default is 0.5.
#' @param noise_sd Numeric, the standard deviation of the noise in the outcome variable. Default is 0.5.
#' @param seed Optional. An integer value for setting the seed to ensure reproducibility. Default is `NULL`, which does not set a seed.
#'
#' @return A list containing two data frames: `sample_data` and `population_data`, each comprising the outcome variable `y`, the treatment variable `a`, and the effect modifier `z`. The `sample_data` data frame also includes the calculated weights for each observation.
#' @examples
#' data <- simulate_ate_data_with_weights(
#'   n_sample = 10000,
#'   n_population = 100000,
#'   p_z_sample = 0.1,
#'   p_z_population = 0.5,
#'   beta_a = 1,
#'   beta_z = 2.5,
#'   noise_sd = 0.5
#' )
#' @keywords internal
simulate_ate_data_with_weights <- function(n_sample = 10000, n_population = 100000,
                                           p_z_sample = 0.1, p_z_population = 0.5,
                                           beta_a = 1, beta_z = 2.5, beta_az = 0.5,
                                           noise_sd = 0.5, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # create sample data
  z_sample <- rbinom(n_sample, 1, p_z_sample)
  a_sample <- rbinom(n_sample, 1, 0.5)

  # simulate outcome
  y_sample <- beta_a * a_sample + beta_z * z_sample + beta_az * (a_sample * z_sample) +
    rnorm(n_sample, mean = 0, sd = noise_sd)

  # put sample data into data frame
  sample_data <- data.frame(y_sample, a_sample, z_sample)

  # simulate population data, where the distribution of effect modifiers differs, but the treatment effect is the same
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * z_population +
    beta_az * (a_population * z_population) + rnorm(n_population, mean = 0, sd = noise_sd)

  # put population data in dataframe
  population_data <- data.frame(y_population, a_population, z_population)

  # simulate weighting based on z distribution difference
  weight_z_1 = p_z_population / p_z_sample # adjust weight for Z=1
  weight_z_0 = (1 - p_z_population) / (1 - p_z_sample) # adjust weight for Z=0
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)

  # add weights to sample_data
  sample_data$weights = weights

  # return list of data frames and weights
  list(sample_data = sample_data, population_data = population_data)
}
