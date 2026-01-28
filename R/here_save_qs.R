#' Save Data Frame or Object to qs File in a Specified Directory with Enhanced Compression
#'
#' Saves the provided data frame or object as a `.qs` file using the specified name, within a directory defined by `dir_path`.
#' This function leverages the `qs` package to write data to `.qs` format with enhanced compression for efficient storage and quick access in R. The `.qs` format is retained for compatibility; for new workflows, prefer `here_save_arrow()`.
#'
#' @param obj Data frame or object to be saved. This is the object you want to persist to disk in `.qs` format.
#' @param name Character string specifying the base name of the file (without the ".qs" extension).
#' @param dir_path Character string specifying the directory path where the file will be saved.
#' @param preset Character string specifying the compression preset. Default is "high" for better compression.
#' @param nthreads Integer specifying the number of threads to use for compression. Default is 1.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
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
here_save_qs <- function(obj, name, dir_path, preset = "high", nthreads = 1, quiet = FALSE) {
  file_path <- here::here(dir_path, paste0(name, ".qs"))
  qs::qsave(obj, file_path, preset = preset, nthreads = nthreads)

  if (!quiet) {
    cli::cli_alert_warning("The .qs format is retained for compatibility. For new workflows, prefer here_save_arrow().")

    # Get file size
    file_size <- margot_size(obj)

    # Print CLI message
    cat(sprintf("Object saved to: %s\n", file_path))
    cat(sprintf("Object size: %s\n", file_size))
    cli::cli_alert_success("Save operation completed successfully")
  }

  invisible(NULL)
}
