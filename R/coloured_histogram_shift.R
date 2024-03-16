#' Visualize Shifts in Data Distributions with Highlighted Ranges
#'
#' This function creates a histogram that highlights a specified range of values to visualize shifts in data distributions. The highlighted range can indicate areas of interest, such as shifts up or down in the distribution. The fill colour of the histogram is dynamically adjusted based on the specified direction of the shift.
#'
#' @param df A dataframe containing the variable of interest.
#' @param col_name The name of the column in `df` to be visualized in the histogram. This should be a numeric variable.
#' @param binwidth The width of the bins for the histogram. Default is 1. Adjust this based on the distribution and scale of your data to create a meaningful visualization.
#' @param range_highlight A numeric vector of length 2 specifying the start and end of the range to highlight. If `NULL`, no range is highlighted.
#' @param shift A character string indicating the direction of the shift, with "up" highlighting in gold and "down" highlighting in dodger blue. The default is "up".
#' @param show_avg_line A logical value indicating whether to display a vertical line representing the average value of the specified column using a red dashed line. Default is `TRUE`.
#' @return A `ggplot` object representing the histogram with specified highlights. This object can be printed or further modified using `ggplot2` functions.
#'
#' @examples
#' # Assuming df_nz is your dataframe and it includes a numeric variable 'forgiveness'
#' # Filter to a specific subset, for example, wave 2019
#' df_19 <- dplyr::filter(df_nz, wave == 2019)
#'
#' # Create and print the histogram
#' graph_density_of_exposure <- coloured_histogram_shift(
#'   df = df_19,
#'   shift = "down",
#'   col_name = "forgiveness",
#'   binwidth = .5, # Adjust binwidth for your data
#'   range_highlight = c(3.9, 10)
#' )
#' print(graph_density_of_exposure)
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @export
coloured_histogram_shift <- function(df, col_name, binwidth = 1, range_highlight = NULL, shift = "up", show_avg_line = TRUE) {

  # validate data
  if(!col_name %in% names(df)) stop("col_name does not exist in the dataframe.")
  if(all(is.na(df[[col_name]]))) stop("The specified column contains only NA values.")

  # Ensure col_name is a symbol for aes()
  col_name_sym <- rlang::sym(col_name)

  # calculate average value for the vertical line
  avg_val <- mean(df[[col_name]], na.rm = TRUE)

  # determine the fill colour based on the shift direction
  highlight_color <- if(shift == "up") "gold" else "dodgerblue"

  # create a new column for fill colour based on range_highlight
  if (!is.null(range_highlight) && length(range_highlight) == 2) {
    df$fill_color <- ifelse(df[[col_name]] >= range_highlight[1] & df[[col_name]] <= range_highlight[2], highlight_color, "grey60")
  } else {
    df$fill_color <- "grey60" # Default color if no range_highlight is provided
  }

  # define subtitle based on the shift direction
  subtitle_text <- if(shift == "up") {
    "Gold region denotes population shifted up to grey"
  } else {
    "Blue region denotes population shifted down to grey"
  }

  # Optionally add average line description to the subtitle if the line is to be shown
  if(show_avg_line) {
    subtitle_text <- paste(subtitle_text, "\nRed dashed line shows the average value")
  }

  # Convert col_name to title case for the title
  col_name_title_case <- tools::toTitleCase(col_name)

  # create the histogram with the new fill_colour column for colouring
  p <- ggplot(df, aes(x = !!col_name_sym, fill = fill_color)) +
    geom_histogram(binwidth = binwidth, color = "black", alpha = 0.7) +
    scale_fill_identity() +
    labs(title = paste("Histogram of", col_name_title_case, "Shift Intervention"),
         subtitle = subtitle_text,
         x = col_name_title_case,
         y = "Count") +
    theme_minimal()

  # Conditionally add the average value line
  if(show_avg_line) {
    p <- p + geom_vline(xintercept = avg_val, color = "red", linetype = "dashed", size = .75)
  }

  return(p)
}
