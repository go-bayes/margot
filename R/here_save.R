#' Save Data Frame as RDS File in a Specified Directory
#'
#' Saves the provided data frame as an RDS file using the specified name, within a directory defined by `push_mods`.
#' This function uses the `here` package to construct the path, ensuring that file paths are built in a consistent and platform-independent manner.
#'
#' @param df Data frame to be saved. This is the object you want to persist on disk.
#' @param name Character string specifying the base name of the file. The ".rds" extension will be automatically appended to this name.
#'
#' @details
#' The `push_mods` variable should be defined in the user's environment or within the package and should point to the directory where files will be saved.
#' It is assumed that `push_mods` is correctly set up to point to a valid directory path. This function does not create directories; it assumes that the specified directory exists.
#'
#' @return Invisible NULL. The primary effect of this function is the side effect of saving an RDS file to disk.
#'
#' @examples
#' # assuming `push_mods` is set in your environment to "~/mydata"
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save(my_df, "my_df")
#'
#' @export
#' @importFrom here here
#' @importFrom base saveRDS
here_save <- function(df, name) {
  saveRDS(df, here::here(push_mods, paste0(name, ".rds")))
}
