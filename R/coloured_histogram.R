#' Create a Coloured Histogram Highlighting Specific Ranges
#'
#' This function generates a histogram with specific ranges highlighted to indicate
#' the highest and/or lowest values within a unit of the specified limits. It allows
#' customization of bin width and the range to be highlighted.
#'
#' @param df The dataframe containing the data to be plotted.
#' @param col_name The name of the column for which the histogram will be generated.
#' @param binwidth The width of the bins for the histogram; defaults to 1.
#' @param scale_min The minimum value to be used for scaling the histogram. If `NULL`,
#' the minimum value of `col_name` is used.
#' @param scale_max The maximum value to be used for scaling the histogram. If `NULL`,
#' the maximum value of `col_name` is used.
#' @param highlight_range Specifies which range to highlight: "lowest", "highest", or
#' "both". Defaults to "highest".
#'
#' @return A ggplot object of the histogram with highlighted ranges as specified.
#'
#' @examples
#' # Assuming df_19 is your dataframe and contains the column 'forgiveness'
#' graph <- coloured_histogram(
#'   df = df_19,
#'   col_name = "forgiveness",
#'   scale_min = 1,
#'   scale_max = 7,
#'   highlight_range = "highest",
#'   binwidth = .1 # Adjust binwidth as needed
#' )
#' print(graph)
#'
#' @import ggplot2
#' @import dplyr
#' @import tools
#' @export
coloured_histogram <- function(df, col_name, binwidth = 1, scale_min = NULL, scale_max = NULL, highlight_range = "highest") {

  # validate input
  if(!col_name %in% names(df)) stop("col_name does not exist in the dataframe.")
  if(all(is.na(df[[col_name]]))) stop("The specified column contains only NA values.")

  # automatically determine scale_min and scale_max if not provided
  if(is.null(scale_min)) scale_min <- min(df[[col_name]], na.rm = TRUE)
  if(is.null(scale_max)) scale_max <- max(df[[col_name]], na.rm = TRUE)

  # adjust scale_min and scale_max to create thresholds for highlighting
  adjusted_min <- scale_min + .99
  adjusted_max <- scale_max - .99

  # Title and subtitle using Title Case for the column name
  library(tools)
  dynamic_title <- paste("Density of Responses for", tools::toTitleCase(gsub("_", " ", col_name)))
  fixed_sub_title <- paste(
    "Highlights",
    ifelse(highlight_range == "both", "both the lowest and highest", highlight_range),
    "ranges within 1 unit of limit."
  )

  # Categorize data based on proximity to min/max and chosen highlight range
  df_copy <- df %>%
    mutate(fill_category = case_when(
      .data[[col_name]] <= adjusted_min & (highlight_range %in% c("lowest", "both")) ~ "Lowest",
      .data[[col_name]] >= adjusted_max & (highlight_range %in% c("highest", "both")) ~ "Highest",
      TRUE ~ "Within Range"
    ))

  # Create the plot
  p <- ggplot(df_copy, aes_string(x = col_name, fill = "fill_category")) +
    geom_histogram(binwidth = binwidth, alpha = 1, position = "identity") +
    scale_fill_manual(
      values = c("Lowest" = "dodgerblue", "Highest" = "gold2", "Within Range" = "lightgray"),
      name = "Response Category"
    ) +
    labs(title = dynamic_title, subtitle = fixed_sub_title, x = tools::toTitleCase(gsub("_", " ", col_name)), y = "Count") +
    theme_minimal()

  return(p)
}


