#' Calculate the size of an R object in megabytes
#'
#' This function takes an R object and returns its size in megabytes (MB).
#' It's useful for understanding the memory footprint of large data structures
#' or complex objects in your R environment.
#'
#' @param obj An R object whose size you want to measure
#'
#' @return A character string representing the size of the object in MB,
#'         formatted to two decimal places
#'
#' @examples
#' big_matrix <- matrix(rnorm(1e6), nrow = 1000)
#' size_in_mb(big_matrix)
#'
#' summary_tables <- list(table1 = data.frame(a = 1:1000, b = rnorm(1000)))
#' margot_size(summary_tables)
#'
#' @export
#'
#' @importFrom utils object.size
#'
#' @note KEY MESSAGE: Monitoring object sizes is crucial for efficient memory
#'       management, especially when working with large datasets or complex
#'       analyses. Use this function to keep track of memory usage and optimize
#'       your R code for better performance.
margot_size <- function(obj) {
  size_bytes <- object.size(obj)
  size_mb <- size_bytes / (1024 * 1024)
  sprintf("%.2f MB", size_mb)
}
