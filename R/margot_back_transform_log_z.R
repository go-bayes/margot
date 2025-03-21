#' Create Z-score to Original Scale Mapping for Log-Transformed Data
#'
#' This function creates a data frame that maps standard z-scores to their
#' corresponding values on the original data scale for log-transformed data.
#' It uses the `back_transform_log_z` function to perform the back-transformation
#' and presents the results in a tidy data frame.
#'
#' @param log_mean The mean of the log-transformed dataset from which the z-scores were calculated.
#' @param log_sd The standard deviation of the log-transformed dataset from which the z-scores were calculated.
#' @param z_scores Optional vector of z-scores to transform. Defaults to c(-2, -1, -0.5, 0, 0.5, 1, 2)
#'   representing common points in a normal distribution.
#' @param label Optional string to label the data scale column. Defaults to "data_scale".
#'
#' @return A data frame with two columns: z_score and the original data scale values.
#'   The name of the second column will be the value of the `label` parameter.
#'
#' @examples
#' # Get mean and sd from original log-transformed income data
#' log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
#' log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)
#'
#' # Create mapping table with default z-scores
#' income_mapping <- margot_back_transform_log_z(
#'   log_mean = log_mean_inc,
#'   log_sd = log_sd_inc,
#'   label = "household_income"
#' )
#' print(income_mapping)
#'
#' # Create mapping with custom z-scores
#' custom_mapping <- margot_back_transform_log_z(
#'   log_mean = log_mean_inc,
#'   log_sd = log_sd_inc,
#'   z_scores = c(-1, 0, 1),
#'   label = "household_income"
#' )
#' print(custom_mapping)
#'
#' @export
margot_back_transform_log_z <- function(log_mean, log_sd,
                                        z_scores = c(-2, -1, -0.5, 0, 0.5, 1, 2),
                                        label = "data_scale") {
  # validate inputs
  if (!is.numeric(log_mean) || !is.numeric(log_sd)) {
    stop("log_mean and log_sd must be numeric.")
  }
  if (!is.numeric(z_scores)) {
    stop("z_scores must be numeric.")
  }
  if (!is.character(label)) {
    stop("label must be a character string.")
  }

  # back transform the z-scores
  scale_values <- back_transform_log_z(
    z_scores = z_scores,
    log_mean = log_mean,
    log_sd = log_sd
  )

  # create the data frame
  result_df <- data.frame(
    z_score = z_scores
  )

  # dynamically name the second column using the label
  result_df[[label]] <- scale_values

  return(result_df)
}
