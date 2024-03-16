#' Read Data Frame from Parquet File in a Specified Directory
#'
#' Reads a parquet file specified by `name` from a directory defined by `push_mods`, returning the data frame stored within.
#' This function uses the `arrow` and `here` packages to efficiently read parquet files and to construct the file path in a consistent, platform-independent manner.
#'
#' @param name Character string specifying the name of the Parquet file to be read (without the ".parquet" extension).
#'
#' @details
#' `push_mods` must be defined in the user's environment or within the package, indicating the directory from which files are to be read.
#' It is assumed that `push_mods` is properly set to point to an existing directory path. The function will throw an error if the specified file does not exist or cannot be read as a Parquet file.
#'
#' @return A data frame representing the data stored in the specified Parquet file.
#'
#' @examples
#' # Assuming `push_mods` is set in your environment to "~/mydata"
#' # and you have previously saved a Parquet file named "my_dataset.parquet" in that directory
#' my_df <- here_read_arrow("my_df")
#'
#' @export
#' @importFrom arrow read_parquet
#' @importFrom here here
here_read_arrow <- function(name) {
  df <- arrow::read_parquet(here::here(push_mods, paste0(name, ".parquet")))
  return(df)
}
