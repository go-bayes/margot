#' Save Data Frame to qs File in a Specified Directory
#'
#' Saves the provided data frame as a `.qs` file using the specified name, within a directory defined by `dir_path`.
#' This function leverages the `qs` package to write data frames to `.qs` format for efficient storage and quick access in R.
#'
#' @param df Data frame to be saved. This is the object you want to persist to disk in `.qs` format.
#' @param name Character string specifying the base name of the file (without the ".qs" extension).
#' @param dir_path Character string specifying the directory path where the file will be saved.
#'
#' @details
#' The `dir_path` argument must point to an existing directory. The function does not create directories; it assumes that the specified directory already exists.
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
here_save_qs <- function(df, name, dir_path) {
  qs::qsave(df, here::here(dir_path, paste0(name, ".qs")))
}
