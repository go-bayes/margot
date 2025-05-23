% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_plot_hist.R
\name{margot_plot_hist}
\alias{margot_plot_hist}
\title{Create a Coloured Histogram with Quantile or Custom Breaks (DEPRECATED)}
\usage{
margot_plot_hist(
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
  add_density = FALSE,
  add_rug = FALSE,
  facet_var = NULL,
  x_scale_transform = NULL,
  y_scale_transform = NULL,
  additional_layers = NULL,
  binwidth = NULL
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

\item{x_lab}{The label for the x-axis. If NULL, the column name is used.}

\item{y_lab}{The label for the y-axis. Default is "Count".}

\item{theme_choice}{The ggplot2 theme to use. Default is theme_classic().}

\item{text_size}{The base text size for the plot.}

\item{axis_text_angle}{The angle of the x-axis text.}

\item{add_density}{Logical. If TRUE, adds a density curve to the plot.}

\item{add_rug}{Logical. If TRUE, adds a rug plot to the x-axis.}

\item{facet_var}{Optional. The name of a variable to use for faceting.}

\item{x_scale_transform}{Optional. A transformation for the x-axis (e.g., "log10").}

\item{y_scale_transform}{Optional. A transformation for the y-axis (e.g., "log10").}

\item{additional_layers}{A list of additional ggplot2 layers to add to the plot.}

\item{binwidth}{The width of the bins for the histogram. If NULL, calculated automatically.}
}
\value{
A ggplot2 object representing the colored histogram.
}
\description{
`r lifecycle::badge("deprecated")`
This function is deprecated. Please use `margot_plot_histogram()` instead.
}
\examples{
df <- data.frame(value = rnorm(1000))
coloured_histogram_quantiles(df, "value", n_divisions = 4)

# With custom breaks
coloured_histogram_quantiles(df, "value", custom_breaks = c(-2, -1, 0, 1, 2))

}
\keyword{internal}
