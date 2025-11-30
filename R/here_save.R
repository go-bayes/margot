#' Save Data Frame as RDS File in a Specified Directory
#'
#' Saves the provided data frame as an RDS file using the specified name, within a directory defined by `push_mods`
#' This function uses the `here` package to construct the path, ensuring that file paths are built in a consistent and platform-independent manner.
#'
#' @param df Data frame or object to be saved. This is the object you want to persist on disk.
#' @param name Character string specifying the base name of the file. The ".rds" extension will be automatically appended to this name.
#' @param dir_path Character string specifying the directory path where the file will be saved. If NULL (default), uses `push_mods`.
#' @param compress Logical or character string specifying the type of compression to use. See `?saveRDS` for details. Default is TRUE.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#'
#' @details
#' If `dir_path` is NULL, the `push_mods` variable should be defined in the user's environment or within the package and should point to the directory where files will be saved.
#' It is assumed that the specified directory exists. This function does not create directories.
#'
#' @return Invisibly returns the full path to the saved file.
#'
#' @examples
#' # assuming `push_mods` is set in your environment to "~/mydata"
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save(my_df, "my_df")
#'
#' # specifying a custom directory
#' here_save(my_df, "my_df", dir_path = "~/custom_dir", compress = "xz")
#'
#' @importFrom here here
#' @export
here_save <- function(df, name, dir_path = NULL, compress = TRUE, quiet = FALSE) {
  # Use push_mods if dir_path is NULL, maintaining backward compatibility
  save_dir <- if (is.null(dir_path)) push_mods else dir_path

  file_path <- here::here(save_dir, paste0(name, ".rds"))
  saveRDS(df, file = file_path, compress = compress)

  if (!quiet) {
    # Get file size
    file_size <- margot_size(df)

    # Print CLI message
    cat(sprintf("Object saved to: %s\n", file_path))
    cat(sprintf("Object size: %s\n", file_size))
    cli::cli_alert_success("Save operation completed successfully")
  }

  invisible(file_path)
}
