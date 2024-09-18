#' Back-transform Log-transformed Mean
#'
#' Back-transforms a log-transformed mean (using log(x + 1) transformation) to its original scale.
#' This utility function is useful for interpreting results when the original data were transformed using log(x + 1)
#' to handle zero values or to normalize the distribution of the data.
#'
#' @param log_mean The mean on the log scale, where the original data were transformed using log(x + 1).
#'
#' @return A list containing the mean on the original scale (`mean_original`).
#' The standard deviation is not back-transformed by this function due to the complexity introduced by the log(x + 1) transformation.
#'
#' @examples
#' log_mean <- 1.098612 # true mean is 2. We add + 1 to the log to handle zero: log(2+1) = log(3)
#' back_transformed_result <- back_transform_logmean(log_mean)
#' print(back_transformed_result)
#'
#' @export
back_transform_logmean <- function(log_mean) {
  if (!is.numeric(log_mean)) {
    stop("Input 'log_mean' must be numeric.")
  }

  # Back-transform the mean
  mean_original <- exp(log_mean) - 1

  # Create a list to store the results
  result <- list(
    mean_original = mean_original
  )

  return(result)
}
