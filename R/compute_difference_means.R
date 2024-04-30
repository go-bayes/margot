#' Compute Difference in Means, Standard Error, and Confidence Intervals Between Two Groups
#'
#' This function calculates the difference in means, the standard error of the difference,
#' and the 95% confidence intervals between two independent groups. Each group is represented
#' as a list that includes the mean (theta) and the standard error (std.error) of the group.
#' The result is formatted for easy use with `glue::glue` in Quarto documents etc
#'
#' @param group1 A list containing the mean and standard error of group 1.
#'               Expected structure: list(vals = list(theta = x, std.error = y)).
#' @param group2 A list containing the mean and standard error of group 2.
#'               Expected structure: list(vals = list(theta = x, std.error = y)).
#'
#' @return A data frame with columns `mean_difference`, `std_error`, `conf_low`, and `conf_high`,
#'         each rounded to 4 decimal places. Suitable for direct use in reporting.
#'
#' @examples
#' group1 <- list(vals = list(theta = 100, std.error = 10))
#' group2 <- list(vals = list(theta = 90, std.error = 5))
#' result <- compute_difference_means(group1, group2)
#' glue::glue("The difference in means is {result$mean_difference} with a 95% CI of [{result$conf_low}, {result$conf_high}].")
#'
#' @export
compute_difference_means <- function(group1, group2) {
  # extract means and standard errors from each group
  mean_A <- group1$vals$theta
  mean_B <- group2$vals$theta
  se_A <- group1$vals$std.error
  se_B <- group2$vals$std.error

  #compute difference in means and standard error of the difference
  mean_difference <- mean_A - mean_B
  se_diff <- sqrt(se_A^2 + se_B^2)

  # compute 95% confidence intervals (using 1.96 for Z-value)
  conf_low <- mean_difference - (1.96 * se_diff)
  conf_high <- mean_difference + (1.96 * se_diff)

  # create output data frame and round the results
  out <- data.frame(
    mean_difference = round(mean_difference, 4),
    std_error = round(se_diff, 4),
    conf_low = round(conf_low, 4),
    conf_high = round(conf_high, 4)
  )

  return(out)
}
