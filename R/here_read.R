#' Read Data Frame from RDS File in a Specified Directory
#'
#' Reads an RDS file specified by `name` from a directory defined by `push_mods`, returning the data frame stored within.
#' This function uses the `here` package to resolve the path, ensuring that file paths are built in a consistent and platform-independent manner.
#'
#' @param name Character string specifying the name of the RDS file to be read (without the ".rds" extension).
#'
#' @details
#' The `push_mods` variable must be defined in the user's environment or within the package, pointing to the directory from where files are to be read.
#' It is presumed that `push_mods` is correctly set up to point to an existing directory path. This function does not validate the existence of the file but will throw an error if the file does not exist.
#'
#' @return A data frame or the specific R object stored in the RDS file.
#'
#' @examples
#' # Assuming `push_mods` is set in your environment to "~/mydata"
#' # and you have previously saved an RDS file named "my_saved_dataframe.rds" in that directory
#' my_df <- here_read("my_df")
#'
#' @export
#' @importFrom here here
#' @importFrom base readRDS
here_read <- function(name) {
  df <- readRDS(here::here(push_mods, paste0(name, "")))
  return(df)
}
