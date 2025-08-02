#' Read Data Frame or Object from qs File in a Specified Directory
#'
#' Reads a `.qs` file specified by `name` from a directory defined by `dir_path`, returning the data frame or object stored within.
#' This function uses the `qs` package to efficiently read `.qs` files and the `here` package to construct the file path in a consistent, platform-independent manner.
#'
#' @param name Character string specifying the name of the `.qs` file to be read (without the ".qs" extension).
#' @param dir_path Character string specifying the directory path from which the file will be read. If NULL (default), uses `push_mods`.
#' @param nthreads Integer specifying the number of threads to use for decompression. Default is 1.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#'
#' @details
#' If `dir_path` is NULL, the `push_mods` variable must be defined in the user's environment or within the package, pointing to the directory from where files are to be read.
#' This function will throw an error if the specified file does not exist or cannot be read as a `.qs` file.
#'
#' @return A data frame or object representing the data stored in the specified `.qs` file.
#'
#' @examples
#' # Assuming `push_mods` is set in your environment to "~/mydata"
#' # and you have previously saved a `.qs` file named "my_dataset.qs" in that directory
#' my_df <- here_read_qs("my_dataset")
#'
#' # Reading from a custom directory with multiple threads
#' my_df <- here_read_qs("my_dataset", dir_path = "~/custom_dir", nthreads = 4)
#'
#' @export
#' @importFrom qs qread
#' @importFrom here here
here_read_qs <- function(name, dir_path = NULL, nthreads = 1, quiet = FALSE) {
  # Use push_mods if dir_path is NULL, maintaining backward compatibility
  read_dir <- if (is.null(dir_path)) push_mods else dir_path

  file_path <- here::here(read_dir, paste0(name, ".qs"))

  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }

  obj <- qs::qread(file_path, nthreads = nthreads)

  if (!quiet) {
    # Get file size
    file_size <- margot_size(obj)

    # Print CLI message
    cat(sprintf("Object read from: %s\n", file_path))
    cat(sprintf("Object size: %s\n", file_size))
    cat("👍 Read operation completed successfully!\n")
  }

  return(obj)
}
