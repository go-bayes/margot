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
  # Ensure qini_obj has a '_path' and 'gain' attribute
  if (is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
    warning(paste("Qini object is missing '_path' or 'gain' for arm:", arm_name))
    return(data.frame(index = integer(0), gain = numeric(0), arm = character(0)))
  }

  gain <- qini_obj[["_path"]]$gain
  index <- seq_along(gain)

  # extend the gain to the max_index with the last value
  extended_gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
  extended_index <- seq_len(max_index)

  # Print statements for debugging
  cat(paste("Extracting Qini data for:", arm_name, "\n"))
  cat(paste("Gain length:", length(gain), "\n"))
  cat(paste("Extended gain length:", length(extended_gain), "\n"))

  data.frame(
    index = extended_index,
    gain = extended_gain,
    arm = arm_name
  )
}
