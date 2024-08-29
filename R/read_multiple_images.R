#' Read Multiple Images from a Folder
#'
#' This function reads multiple images from a specified folder and returns them as a list of magick image objects.
#' It uses the `here` package for path handling, `magick` for image reading, and `stringr` for string manipulation.
#'
#' @param path_multi_plots Character string specifying the path to the folder containing the images, relative to the project root.
#' @param pattern Character string specifying the pattern to match in the filenames. Default is "policy_tree_plot\\.png$".
#' @return A named list of magick image objects.
#'
#' @importFrom here here
#' @importFrom magick image_read
#' @importFrom stringr str_subset
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path_multi_plots <- "plots"
#' policy_tree_plots <- read_multiple_images(path_multi_plots, "policy_tree_plot\\.png$")
#' qini_plots <- read_multiple_images(path_multi_plots, "qini_plot\\.png$")
#' }
read_multiple_images <- function(path_multi_plots, pattern = "policy_tree_plot\\.png$") {
  # Get all PNG files in the specified path
  all_files <- list.files(here::here(path_multi_plots), pattern = "\\.png$", full.names = TRUE)

  # Filter files based on the specified pattern
  matching_files <- stringr::str_subset(all_files, pattern)

  # Read images and create a named list
  image_list <- lapply(matching_files, function(file) {
    name <- sub("\\.png$", "", basename(file))
    image <- magick::image_read(file)
    return(image)
  })

  # Set names for the list elements
  names(image_list) <- sub("\\.png$", "", basename(matching_files))

  # Add CLI message with emojis
  message(sprintf("\U1F44D\U1F60A Successfully loaded %d images matching the pattern '%s'",
                  length(image_list), pattern))

  return(image_list)
}
