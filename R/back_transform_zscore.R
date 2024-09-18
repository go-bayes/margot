#' Back Transform Z-Score to Original Scale
#'
#' This function takes a z-score and transforms it back to its original scale
#' using the specified mean and standard deviation of the original data. Often,
#' standardization has been applied and the original scale values are needed for
#' interpretation.
#'
#' @param z A numeric value or vector of z-scores to be transformed back to the original scale.
#' @param mean The mean of the original dataset from which the z-score was calculated.
#' @param sd The standard deviation of the original dataset from which the z-score was calculated.
#'
#' @return Returns a numeric value or vector of the original scale values corresponding to the input z-scores.
#'
#' @examples
#' # Given a dataset with mean = 100 and sd = 15
#' original_value <- back_transform_zscore(z = 1.5, mean = 100, sd = 15)
#' print(original_value)
#'
#' # Multiple z-scores can be transformed at once
#' z_scores <- c(-1, 0, 1, 2)
#' original_values <- back_transform_zscore(z = z_scores, mean = 50, sd = 10)
#' print(original_values)
#'
#' @export
back_transform_zscore <- function(z, mean, sd) {
  # Validate input types and values
  if (!is.numeric(z) || !is.numeric(mean) || !is.numeric(sd)) {
    stop("All inputs must be numeric.")
  }
  if (sd <= 0) {
    stop("'sd' must be positive.")
  }

  # Perform back-transformation
  x <- (z * sd) + mean
  return(x)
}
