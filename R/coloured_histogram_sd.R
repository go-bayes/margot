#' Visualize Distribution with Mean and Standard Deviation Highlights
#'
#' This function creates a histogram for a specified column in a dataframe,
#' highlighting the mean and one standard deviation above and below the mean.
#' It draws vertical lines for the mean (in black) and for plus/minus one standard
#' deviation (in blue and gold, respectively), with arrows from the mean to each
#' standard deviation marker. The title of the plot includes the capitalized column name,
#' achieved using \code{tools::toTitleCase()}.
#'
#' @param df Dataframe containing the data to be visualized.
#' @param col_name Name of the column to create a histogram for. This column should
#'   contain numeric data.
#' @param binwidth Width of the bins for the histogram. Can be adjusted for finer or
#'   coarser resolution of the distribution. Default is 1.
#'
#' @return A ggplot object representing the histogram with highlights for the mean and
#'   standard deviations. The plot can be printed or modified further.
#'
#' @examples
#'  \dontrun{
#' # Assuming `df_nz` is a dataframe with a numeric column 'forgiveness'
#' # and a factor or integer column 'wave' for subsetting:
#' df_19 <- dplyr::filter(df_nz, wave == 2019)
#'
#' graph_density_of_exposure <- coloured_histogram_sd(
#'   df = df_19,
#'   col_name = "forgiveness",
#'   binwidth = 0.5 # Adjust binwidth as needed
#' )
#' }
#' print(graph_density_of_exposure)
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom tools toTitleCase
#' @keywords internal
coloured_histogram_sd <- function(df, col_name, binwidth = 1) {

  # validate data
  if(!col_name %in% names(df)) stop("col_name does not exist in the dataframe.")
  if(all(is.na(df[[col_name]]))) stop("The specified column contains only NA values.")

  # Compute statistics
  avg_val <- mean(df[[col_name]], na.rm = TRUE)
  std_val <- sd(df[[col_name]], na.rm = TRUE)

  # create data frame for v-lines and arrows
  line_data <- data.frame(
    value = c(avg_val - std_val, avg_val, avg_val + std_val),
    description = c("Mean - 1 SD", "Mean", "Mean + 1 SD"),
    color = c("dodgerblue", "darkgray", "gold2")
  )

  # create arrow data specifically for mean to +/- 1 SD
  arrow_data <- data.frame(
    x = avg_val,
    xend = c(avg_val - std_val, avg_val + std_val),
    y = 0,
    yend = 0,
    color = c("Mean to Mean - 1 SD", "Mean to Mean + 1 SD")
  )

  # dynamically construct the title, capitalizing the first letter of the column name
  dynamic_title <- paste(tools::toTitleCase(col_name), "Histogram with Mean and Standard Deviation Intervals")

  # create the plot
  p <- ggplot(df, aes(x = !!rlang::sym(col_name))) +
    geom_histogram(aes(y = ..count..), binwidth = binwidth, fill = "lightgray") +
    geom_vline(data = line_data, aes(xintercept = value, color = description), linewidth = 1.5) +
    geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend, color = color),
                 arrow = arrow(type = "closed", ends = "last", length = unit(0.1, "inches")),
                 linewidth = 1) +
    scale_color_manual(values = c(
      "Mean - 1 SD" = "dodgerblue",
      "Mean" = "black",
      "Mean + 1 SD" = "gold2",
      "Mean to Mean - 1 SD" = "dodgerblue",  # Use the same colors for arrows as the lines they connect
      "Mean to Mean + 1 SD" = "gold2"
    )) +
    labs(title = dynamic_title,
         subtitle = "Vertical lines indicate the mean and one standard deviation from the mean. Arrows highlight the direction of deviation.",
         color = "Legend",
         x = tools::toTitleCase(col_name),  # Optionally set the x-axis label similarly
         y = "Count") +
    theme_minimal()

  return(p)
}
