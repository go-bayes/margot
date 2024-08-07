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
<<<<<<< HEAD
extract_qini_data <- function(qini_obj, arm_name, max_index) {
  # Ensure qini_obj has a '_path' and 'gain' attribute
  if (is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
    warning("Qini object is missing '_path' or 'gain'")
    return(data.frame(index = integer(0), gain = numeric(0), arm = character(0)))
=======
extract_qini_data <- function(qini_obj, name, max_index) {
  # Ensure qini_obj has a '_path' and 'gain' attribute
  if (is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
    warning("Qini object is missing '_path' or 'gain'")
    return(data.frame(index = integer(0), gain = numeric(0), name = character(0)))
>>>>>>> bd6bff32b0490ef0ada4a71d7408f12a808aab88
  }

  gain <- qini_obj[["_path"]]$gain

  # If gain is empty, fill it with zeros up to max_index
  if (length(gain) == 0) {
    gain <- rep(0, max_index)
  } else {
    # Fill in missing values to reach max_index
    gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
  }

  # Print statements for debugging
<<<<<<< HEAD
  print(paste("Extracting Qini data for:", arm_name))
=======
  print(paste("Extracting Qini data for:", name))
>>>>>>> bd6bff32b0490ef0ada4a71d7408f12a808aab88
  print(paste("Gain length:", length(gain)))

  data.frame(
    index = seq_len(max_index),
    gain = gain,
<<<<<<< HEAD
    arm = arm_name  # Update column name from 'name' to 'arm'
=======
    name = name
>>>>>>> bd6bff32b0490ef0ada4a71d7408f12a808aab88
  )
}
