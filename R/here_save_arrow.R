#' Save Data Frame to Parquet File in a Specified Directory
#'
#' Saves the provided data frame as a parquet file using the specified name, within a directory defined by `push_mods`.
#' This function leverages the `arrow` and `here` packages to write data frames to parquet format and construct the file path in a consistent, platform-independent manner.
#'
#' @param df Data frame to be saved. This is the object you want to persist to disk in Parquet format.
#' @param name Character string specifying the base name of the file (without the ".parquet" extension).
#'
#' @details
#' The `push_mods` variable must be defined in the user's environment or within the package, pointing to the directory where files will be saved.
#' It is presumed that `push_mods` is properly set up to point to an existing directory path. The function does not create directories; it assumes that the specified directory already exists.
#'
#' @return Invisible NULL. The primary effect of this function is the side effect of writing a parquet file to disk.
#'
#' @examples
#' # Assuming `push_mods` is set in your environment to "~/mydata"
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save_arrow(my_df, "my_saved_dataframe")
#'
#' @export
#' @importFrom arrow write_parquet
#' @importFrom here here
here_save_arrow <- function(df, name) {
  arrow::write_parquet(df, here::here(push_mods, paste0(name, ".parquet")))
}
