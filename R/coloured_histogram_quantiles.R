#' Visualize Distribution with Automatically Calculated Quantile Highlights
#'
#' This function creates a histogram for a specified column in a dataframe,
#' highlighting quantiles that are automatically calculated based on the number
#' of quantiles specified by the user. It draws vertical lines for each quantile
#' in specified colors, with the title of the plot incorporating the capitalized
#' column name using \code{tools::toTitleCase()}.
#'
#' @param df Dataframe containing the data to be visualized.
#' @param col_name Name of the column to create a histogram for. This column should
#'   contain numeric data.
#' @param n_quantiles The number of quantiles to calculate. If n_quantiles is 4, this will
#'   produce quartiles; if 5, quintiles; etc. This should be a positive integer greater than 1.
#' @param binwidth Width of the bins for the histogram. Can be adjusted for finer or
#'   coarser resolution of the distribution. Default is 1.
#'
#' @return A ggplot object representing the histogram with highlighted quantiles.
#'   The plot can be printed or modified further.
#'
#' @examples
#'  \dontrun{
#' # `df_nz` is the included dataframe with a numeric column 'forgiveness':
#' df_19 <- dplyr::filter(df_nz, wave == 2019)
#'
#' quantile_plot <- coloured_histogram_quantiles(
#'   df = df_19,
#'   col_name = "forgiveness",
#'   n_quantiles = 4, # producing quartiles
#'   binwidth = 0.5 # adjust binwidth as needed
#' )
#' }
#' print(quantile_plot)
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom tools toTitleCase
#' @importFrom stats quantile
#' @export
coloured_histogram_quantiles <- function(df, col_name, n_quantiles, binwidth = 1) {
  if (!col_name %in% names(df)) {
    stop("col_name does not exist in the dataframe.")
  }
  if (all(is.na(df[[col_name]]))) {
    stop("The specified column contains only NA values.")
  }
  if (n_quantiles <= 1) {
    stop("n_quantiles must be greater than 1.")
  }

  # Compute quantile values based on the number of quantiles; exclude the maximum value
  quantile_proportions <- seq(0, 1, length.out = n_quantiles + 1)[-c(1, n_quantiles + 1)]
  quantile_values <- quantile(df[[col_name]], probs = quantile_proportions, na.rm = TRUE)
  quantile_labels <- paste(quantile_proportions * 100, "% quantile", sep="")

  # Create data frame for v-lines
  line_data <- data.frame(
    value = quantile_values,
    label = quantile_labels,
    color = rainbow(length(quantile_proportions))
  )

  # Dynamically construct the title
  dynamic_title <- paste(tools::toTitleCase(col_name), "Histogram with Quantile Highlights")

  # Create the plot
  p <- ggplot(df, aes(x = !!rlang::sym(col_name))) +
    geom_histogram(aes(y = ..count..), binwidth = binwidth, fill = "lightgray") +
    geom_vline(data = line_data, aes(xintercept = value, color = I(color)), linewidth = 1.5) +
    scale_color_manual(values = setNames(line_data$color, line_data$label)) +
    labs(title = dynamic_title,
         subtitle = paste("Vertical lines indicate the", paste(quantile_proportions * 100, "%", collapse=", "), "quantiles."),
         color = "Quantile",
         x = tools::toTitleCase(col_name),
         y = "Count") +
    theme_classic()

  return(p)
}
