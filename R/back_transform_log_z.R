#' Back Transform Z-Score to Original Log-Transformed Scale
#'
#' This function takes z-scores and transforms them back to their original values
#' where the data was originally log-transformed. It performs a two-step transformation:
#' first converting z-scores back to log values using the log-scale mean and standard deviation,
#' then exponentiating to return to the original scale.
#'
#' @param z_scores A numeric value or vector of z-scores to be transformed back to the original scale.
#' @param log_mean The mean of the log-transformed dataset from which the z-scores were calculated.
#' @param log_sd The standard deviation of the log-transformed dataset from which the z-scores were calculated.
#'
#' @return Returns a numeric value or vector of the original scale values corresponding to the input z-scores.
#'
#' @examples
#' # Given log-transformed data with log_mean = 1.5 and log_sd = 0.5
#' original_value <- back_transform_log_z(z_scores = 1.2, log_mean = 1.5, log_sd = 0.5)
#' print(original_value)
#'
#' # Multiple z-scores can be transformed at once
#' z_scores <- c(-1, 0, 1, 2)
#' original_values <- back_transform_log_z(z_scores = z_scores, log_mean = 1.5, log_sd = 0.5)
#' print(original_values)
#'
#' # Real-world example: back-transforming household income z-scores
#' # Get mean and sd from original log-transformed data
#' log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
#' log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)
#'
#' # Back-transform all z-scores in the dataset
#' original_data_scale <- back_transform_log_z(
#'   df_grf$t0_log_household_inc_z,
#'   log_mean = log_mean_inc,
#'   log_sd = log_sd_inc
#' )
#' head(original_data_scale)
#'
#' # Interpret key points on the distribution (-1 SD, mean, +1 SD)
#' z_scores <- c(-1, 0, 1)
#' scale_values <- back_transform_log_z(
#'   z_scores,
#'   log_mean = log_mean_inc,
#'   log_sd = log_sd_inc
#' )
#'
#' # Create a data frame to display the relationship between z-scores and original values
#' results_df <- data.frame(
#'   z_score = z_scores,
#'   data_scale = scale_values
#' )
#' print(results_df) # Shows what values on the original scale correspond to each z-score
#'
#' @export
back_transform_log_z <- function(z_scores, log_mean, log_sd) {
  # Validate input types and values
  if (!is.numeric(z_scores) || !is.numeric(log_mean) || !is.numeric(log_sd)) {
    stop("All inputs must be numeric.")
  }
  if (log_sd <= 0) {
    stop("'log_sd' must be positive.")
  }

  # step 1: z-score to log value
  log_values <- z_scores * log_sd + log_mean

  # step 2: log to original value
  original_values <- exp(log_values)

  return(original_values)
}
