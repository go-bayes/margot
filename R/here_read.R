
#' Read Data Frame or Object from RDS File in a Specified Directory
#'
#' Reads an RDS file specified by `name` from a directory defined by `dir_path` or `push_mods`, returning the data frame or object stored within.
#' If no .rds file is found, it searches for the file without the .rds extension.
#' This function uses the `here` package to resolve the path, ensuring that file paths are built in a consistent and platform-independent manner.
#'
#' @param name Character string specifying the name of the file to be read (with or without the ".rds" extension).
#' @param dir_path Character string specifying the directory path from which the file will be read. If NULL (default), uses `push_mods`.
#' @param quiet Logical. If TRUE, suppresses console output. Default is FALSE.
#'
#' @details
#' If `dir_path` is NULL, the `push_mods` variable must be defined in the user's environment or within the package, pointing to the directory from where files are to be read.
#' This function will first try to read an .rds file. If not found, it will attempt to read the file without the .rds extension.
#'
#' @return The data frame or R object stored in the file.
#'
#' @examples
#' # Assuming `push_mods` is set in your environment to "~/mydata"
#' # and you have previously saved an RDS file named "my_df.rds" in that directory
#' my_df <- here_read("my_df")
#'
#' # Reading from a custom directory
#' my_df <- here_read("my_df", dir_path = "~/custom_dir")
#'
#' @export
#' @importFrom here here
#' @importFrom base readRDS
here_read <- function(name, dir_path = NULL, quiet = FALSE) {
  # use push_mods if dir_path is NULL, maintaining backward compatibility
  read_dir <- if (is.null(dir_path)) push_mods else dir_path

  # remove .rds extension if present in the name
  name <- sub("\\.rds$", "", name)

  # try to read .rds file first
  rds_path <- here::here(read_dir, paste0(name, ".rds"))
  if (file.exists(rds_path)) {
    file_path <- rds_path
    obj <- readRDS(file_path)
  } else {
    # If .rds file not found, try reading without extension
    non_rds_path <- here::here(read_dir, name)
    if (file.exists(non_rds_path)) {
      file_path <- non_rds_path
      obj <- readRDS(file_path)
    } else {
      stop(sprintf("File not found: %s or %s", rds_path, non_rds_path))
    }
  }

  if (!quiet) {
    # Get object size using margot_size function
    obj_size <- margot_size(obj, "Object")
    # Print CLI message
    cat(sprintf("Object read from: %s\n", file_path))
    cat(sprintf("Object size: %s\n", obj_size))
    cat("👍 Read operation completed successfully!\n")
  }
  return(obj)
}
# old
# here_read <- function(name) {
#   df <- readRDS(here::here(push_mods, paste0(name, "")))
#   return(df)
# }
