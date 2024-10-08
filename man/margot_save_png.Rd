% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_plot_save_png.R, R/margot_save_png.R
\name{margot_save_png}
\alias{margot_save_png}
\title{Save Margot Plot as PNG}
\usage{
margot_save_png(
  plot_output,
  prefix = NULL,
  base_filename = "margot_plot",
  save_path = here::here("push_mods"),
  width = 16,
  height = 8,
  dpi = 500
)

margot_save_png(
  plot_output,
  prefix = NULL,
  base_filename = "margot_plot",
  save_path = here::here("push_mods"),
  width = 16,
  height = 8,
  dpi = 500
)
}
\arguments{
\item{plot_output}{A list containing the output of `margot_plot()` or `margot_plot_multi_arm()`.
This list should have a `plot` element that contains the ggplot object.}

\item{prefix}{Character string. A prefix to add to the filename. Default is NULL.}

\item{base_filename}{Character string. The base name for the saved file. Default is "margot_plot".}

\item{save_path}{Character string. The directory path where the image will be saved.
Default is here::here("push_mods").}

\item{width}{Numeric. The width of the saved image in inches. Default is 16.}

\item{height}{Numeric. The height of the saved image in inches. Default is 8.}

\item{dpi}{Numeric. The resolution of the saved image in dots per inch. Default is 500.}
}
\value{
Invisibly returns the path of the saved file.

Invisibly returns the path of the saved file.
}
\description{
This function takes the output of either `margot_plot()` or `margot_plot_multi_arm()`
and saves the plot as a PNG image using `ggsave()`.

This function takes the output of either `margot_plot()` or `margot_plot_multi_arm()`
and saves the plot as a PNG image using `ggsave()`.
}
\details{
This function uses `ggsave()` to save the Margot plot as a PNG image.
If the save_path directory doesn't exist, it will be created.
The final filename will be constructed as: `{prefix}_{base_filename}.png`.

This function uses `ggsave()` to save the Margot plot as a PNG image.
If the save_path directory doesn't exist, it will be created.
The final filename will be constructed as: `{prefix}_{base_filename}.png`.
}
\examples{
\dontrun{
# Assuming you have already run margot_plot() or margot_plot_multi_arm()
plot_result <- margot_plot(your_data, your_options)

# Save the plot as PNG
margot_save_png(
  plot_result,
  prefix = "study1",
  base_filename = "treatment_effects",
  save_path = here::here("output", "plots")
)
}

\dontrun{
# Assuming you have already run margot_plot() or margot_plot_multi_arm()
plot_result <- margot_plot(your_data, your_options)

# Save the plot as PNG
margot_save_png(
  plot_result,
  prefix = "study1",
  base_filename = "treatment_effects",
  save_path = here::here("output", "plots")
)
}

}
