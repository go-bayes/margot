% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_plot_categorical.R
\name{margot_plot_categorical}
\alias{margot_plot_categorical}
\title{Create a Coloured Histogram with Quantile or Custom Breaks}
\usage{
margot_plot_categorical(
  df,
  col_name,
  n_divisions = NULL,
  custom_breaks = NULL,
  cutpoint_inclusive = "upper",
  ties.method = NULL,
  colour_palette = NULL,
  hist_colour = "black",
  line_type = "solid",
  line_width = 0.75,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = "Count",
  theme_choice = theme_classic(),
  text_size = 12,
  axis_text_angle = 45,
  x_scale_transform = NULL,
  y_scale_transform = NULL,
  additional_layers = NULL,
  binwidth = NULL,
  save_path = NULL,
  width = 16,
  height = 10,
  legend_position = "right",
  include_timestamp = FALSE,
  file_prefix = ""
)
}
\arguments{
\item{df}{A data frame containing the variable to be plotted.}

\item{col_name}{The name of the column in the data frame to be plotted.}

\item{n_divisions}{The number of divisions for quantile breaks. Ignored if custom_breaks is provided.}

\item{custom_breaks}{A numeric vector of custom break points.}

\item{cutpoint_inclusive}{Character. Either "lower" or "upper", specifying whether the cutpoint should be included in the lower or upper interval.}

\item{ties.method}{A character string specifying how ties should be handled. See ?quantile for details.}

\item{colour_palette}{A vector of colors to use for the intervals. If NULL, uses the Okabe-Ito palette.}

\item{hist_colour}{The color of the histogram borders.}

\item{line_type}{The type of line to use for the histogram borders.}

\item{line_width}{The width of the lines for the histogram borders.}

\item{title}{The title of the plot. If NULL, a default title is used.}

\item{subtitle}{The subtitle of the plot. If NULL, a default subtitle is used.}

\item{x_lab}{The label for the x-axis. If NULL, the formatted column name is used.}

\item{y_lab}{The label for the y-axis. Default is "Count".}

\item{theme_choice}{The ggplot2 theme to use. Default is theme_classic().}

\item{text_size}{The base text size for the plot.}

\item{axis_text_angle}{The angle of the x-axis text.}

\item{x_scale_transform}{Optional. A transformation for the x-axis (e.g., "log10").}

\item{y_scale_transform}{Optional. A transformation for the y-axis (e.g., "log10").}

\item{additional_layers}{A list of additional ggplot2 layers to add to the plot.}

\item{binwidth}{The width of the bins for the histogram. If NULL, calculated automatically.}

\item{save_path}{An optional path to save the plot. If NULL, the plot will not be saved.}

\item{width}{The width of the saved plot in inches. Default is 16.}

\item{height}{The height of the saved plot in inches. Default is 10.}

\item{legend_position}{The position of the legend. Can be "right", "left", "bottom", "top", or a two-element numeric vector.}

\item{include_timestamp}{A logical value indicating whether to include a timestamp in the saved filename. Default is FALSE.}

\item{file_prefix}{An optional prefix to add to the beginning of the saved filename.}
}
\value{
A ggplot2 object representing the colored histogram.
}
\description{
This function creates a histogram with coloured regions based on quantile breaks or custom breaks.
It uses the `create_ordered_variable` function to categorise the data and then plots the histogram
with different colours for each category.
}
