#' #' Extract Qini Data for Plotting
#' #'
#' #' @description
#' #' Extracts Qini curve data from a Qini object and prepares it for plotting.
#' #'
#' #' @param qini_obj A Qini object.
#' #' @param name Name of the treatment arm.
#' #' @param max_index Maximum index to extend the curve to.
#' #' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#' #'
#' #' @return A data frame with extracted Qini data.
#' #'
#' #' @keywords internal
extract_qini_data <- function(qini_obj, name, max_index, verbose = TRUE) {
  gain <- if (!is.null(qini_obj[["_path"]]$gain)) {
    qini_obj[["_path"]]$gain
  } else {
    if (verbose) cli::cli_alert_warning(paste("Qini object", name, "is NULL or missing required components. Extending with zeros."))
    rep(0, max_index)
  }

  gain_length <- length(gain)
  if (gain_length < max_index) {
    # Extend gain vector
    gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
  } else if (gain_length > max_index) {
    # Truncate gain vector
    gain <- gain[1:max_index]
  }

  proportion <- seq_len(max_index) / max_index

  data.frame(
    proportion = proportion,
    gain = gain,
    curve = name
  )
}
# working for binary
# extract_qini_data <- function(qini_obj, arm_name, max_index) {
#   # Ensure qini_obj has a '_path' and 'gain' attribute
#   if (is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
#     warning(paste("Qini object is missing '_path' or 'gain' for arm:", arm_name))
#     return(data.frame(index = integer(0), gain = numeric(0), arm = character(0)))
#   }
#
#   gain <- qini_obj[["_path"]]$gain
#
#   # If gain is empty, fill it with zeros
#   if (length(gain) == 0) {
#     gain <- rep(0, max_index)
#   } else if (length(gain) < max_index) {
#     # Extend gain to max_index by repeating the last value
#     gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
#   } else if (length(gain) > max_index) {
#     # Truncate gain if it's longer than max_index
#     gain <- gain[1:max_index]
#   }
#
#   # Print statements for debugging
#   cat(paste("Extracting Qini data for:", arm_name, "\n"))
#   cat(paste("Original gain length:", length(qini_obj[["_path"]]$gain), "\n"))
#   cat(paste("Extended gain length:", length(gain), "\n"))
#
#   data.frame(
#     index = seq_len(max_index),
#     gain = gain,
#     arm = arm_name  #
#   )
# }
# WORKING FOR MULTI-ARM TREATMENTS
# extract_qini_data <- function(qini_obj, arm_name, max_index) {
#   # Ensure qini_obj has a '_path' and 'gain' attribute
#   if (is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
#     warning(paste("Qini object is missing '_path' or 'gain' for arm:", arm_name))
#     return(data.frame(index = integer(0), gain = numeric(0), arm = character(0)))
#   }
#
#   gain <- qini_obj[["_path"]]$gain
#   index <- seq_along(gain)
#
#   # extend the gain to the max_index with the last value
#   extended_gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
#   extended_index <- seq_len(max_index)
#
#   # Print statements for debugging
#   cat(paste("Extracting Qini data for:", arm_name, "\n"))
#   cat(paste("Gain length:", length(gain), "\n"))
#   cat(paste("Extended gain length:", length(extended_gain), "\n"))
#
#   data.frame(
#     index = extended_index,
#     gain = extended_gain,
#     arm = arm_name
#   )
#}
