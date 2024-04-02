#' Read Data Frame from qs File in a Specified Directory
#'
#' Reads a `.qs` file specified by `name` from a directory defined by `dir_path`, returning the data frame stored within.
#' This function uses the `qs` package to efficiently read `.qs` files and the `here` package to construct the file path in a consistent, platform-independent manner.
#'
#' @param name Character string specifying the name of the `.qs` file to be read (without the ".qs" extension).
#' @param dir_path Character string specifying the directory path from which the file will be read.
#'
#' @details
#' The `dir_path` argument must point to an existing directory. The function will throw an error if the specified file does not exist or cannot be read as a `.qs` file.
#'
#' @return A data frame representing the data stored in the specified `.qs` file.
#'
#' @examples
#' # Assuming you have a directory path "~/mydata"
#' # and you have previously saved a `.qs` file named "my_dataset.qs" in that directory
#' my_df <- here_read_qs("my_dataset", "~/mydata")
#'
#' @export
#' @importFrom qs qread
#' @importFrom here here
here_read_qs <- function(name, dir_path) {
  df <- qs::qread(here::here(dir_path, paste0(name, ".qs")))
  return(df)
}
