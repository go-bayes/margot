#' Create a Coloured Histogram Highlighting Specific Ranges (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `margot_plot_shift()` instead.
#'
#' This function generates a histogram with specific ranges highlighted to indicate
#' the highest and/or lowest values within a unit of the specified limits. It allows
#' customization of bin width, the unit of change for highlighting, and the range to be highlighted. This is useful in the settings of modified treatment policies for
#' clarifying which part of a distribution is shifted.
#'
#' @param df The dataframe containing the data to be plotted.
#' @param col_name The name of the column for which the histogram will be generated.
#' @param binwidth The width of the bins for the histogram; defaults to 1.
#' @param unit_of_change The unit of change used to define the highlight range.
#' The subtitle will mention this unit. It also adjusts the calculation of the highlight thresholds
#' to be slightly less than this unit so that it does not go over the range of the data. Defaults to 1.
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
#'  \dontrun{
#' # assuming df_19 is your dataframe and contains the column 'forgiveness'
#' graph <- coloured_histogram(
#'   df = df_19,
#'   col_name = "forgiveness",
#'   scale_min = 1,
#'   scale_max = 7,
#'   highlight_range = "highest",
#'   binwidth = .1, # adjust binwidth as needed
#'   unit_of_change = 1 # specify the unit of change
#' )
#' print(graph)
#' }
#' @importFrom ggplot2 ggplot aes_string geom_histogram scale_fill_manual labs theme_classic
#' @importFrom dplyr mutate case_when
#' @importFrom tools toTitleCase
#' @importFrom lifecycle deprecate_warn
#' @keywords internal
coloured_histogram <- function(df, col_name, binwidth = 1, unit_of_change = 1, scale_min = NULL, scale_max = NULL, highlight_range = "highest") {
  # deprecation warning
  lifecycle::deprecate_warn(
    when = "0.2.1.39",
    what = "coloured_histogram()",
    with = "margot_plot_shift()",
    details = "Please use `margot_plot_shift()` for future development."
  )

  # validate input
  if(!col_name %in% names(df)) stop("col_name does not exist in the dataframe.")
  if(all(is.na(df[[col_name]]))) stop("The specified column contains only NA values.")

  # automatically determine scale_min and scale_max if not provided
  if(is.null(scale_min)) scale_min <- min(df[[col_name]], na.rm = TRUE)
  if(is.null(scale_max)) scale_max <- max(df[[col_name]], na.rm = TRUE)

  # adjust scale_min and scale_max to create thresholds for highlighting
  # adjust by slightly less than the specified unit to avoid exceeding the data range
  adjusted_min <- scale_min + (unit_of_change * 0.99)
  adjusted_max <- scale_max - (unit_of_change * 0.99)

  # title and subtitle using title case for the column name
  dynamic_title <- paste("Density of Responses for", tools::toTitleCase(gsub("_", " ", col_name)))
  dynamic_sub_title <- paste(
    "Highlights",
    ifelse(highlight_range == "both", "both the lowest and highest", highlight_range),
    "range(s) within", unit_of_change, "unit(s) of limit."
  )

  # categorize data based on proximity to min/max and chosen highlight range
  df_copy <- df |>
    dplyr::mutate(fill_category = dplyr::case_when(
      .data[[col_name]] <= adjusted_min & (highlight_range %in% c("lowest", "both")) ~ "Lowest",
      .data[[col_name]] >= adjusted_max & (highlight_range %in% c("highest", "both")) ~ "Highest",
      TRUE ~ "Within Range"
    ))

  # create the plot
  p <- ggplot2::ggplot(df_copy, ggplot2::aes_string(x = col_name, fill = "fill_category")) +
    ggplot2::geom_histogram(binwidth = binwidth, alpha = 1, position = "identity") +
    ggplot2::scale_fill_manual(
      values = c("Lowest" = "dodgerblue", "Highest" = "gold2", "Within Range" = "lightgray"),
      name = "Response Category"
    ) +
    ggplot2::labs(title = dynamic_title, subtitle = dynamic_sub_title, x = tools::toTitleCase(gsub("_", " ", col_name)), y = "Count") +
    ggplot2::theme_classic()

  return(p)
}
