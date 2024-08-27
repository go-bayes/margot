#' Save Data Frame or Object to qs File in a Specified Directory with Enhanced Compression
#'
#' Saves the provided data frame or object as a `.qs` file using the specified name, within a directory defined by `dir_path`.
#' This function leverages the `qs` package to write data to `.qs` format with enhanced compression for efficient storage and quick access in R.
#'
#' @param obj Data frame or object to be saved. This is the object you want to persist to disk in `.qs` format.
#' @param name Character string specifying the base name of the file (without the ".qs" extension).
#' @param dir_path Character string specifying the directory path where the file will be saved.
#' @param preset Character string specifying the compression preset. Default is "high" for better compression.
#' @param nthreads Integer specifying the number of threads to use for compression. Default is 1.
#'
#' @details
#' The `dir_path` argument must point to an existing directory. The function does not create directories; it assumes that the specified directory already exists.
#' The function uses enhanced compression settings by default to minimize file size.
#'
#' @return Invisible NULL. The primary effect of this function is the side effect of writing a `.qs` file to disk.
#'
#' @examples
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save_qs(my_df, "my_saved_dataframe", "~/mydata")
#'
#' @export
#' @importFrom qs qsave
#' @importFrom here here
here_save_qs <- function(obj, name, dir_path, preset = "high", nthreads = 1) {
  qs::qsave(obj, here::here(dir_path, paste0(name, ".qs")), preset = preset, nthreads = nthreads)
}
# old
# here_save_qs <- function(df, name, dir_path) {
#   qs::qsave(df, here::here(dir_path, paste0(name, ".qs")))
# }
