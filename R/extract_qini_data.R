#' Extract Qini Data for Plotting
#'
#' @description
#' Extracts Qini curve data from a Qini object and prepares it for plotting.
#'
#' @param qini_obj A Qini object.
#' @param arm_name Name of the treatment arm.
#' @param max_index Maximum index to extend the curve to.
#'
#' @return A data frame with extracted Qini data.
#'
#' @keywords internal
extract_qini_data <- function(qini_obj, arm_name, max_index) {
  gain <- qini_obj[["_path"]]$gain
  index <- seq_along(gain)
  # extend the gain to the max_index with the last value
  extended_gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
  extended_index <- seq_len(max_index)

  data.frame(
    index = extended_index,
    gain = extended_gain,
    arm = arm_name
  )
}
