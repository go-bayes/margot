# Save Margot Plot as PNG

This function takes the output of either \`margot_plot()\` or
\`margot_plot_multi_arm()\` and saves the plot as a PNG image using
\`ggsave()\`.

## Usage

``` r
margot_plot_save_png(
  plot_output,
  prefix = NULL,
  base_filename = "margot_plot",
  save_path = here::here("push_mods"),
  width = 16,
  height = 8,
  dpi = 500
)
```

## Arguments

- plot_output:

  A list containing the output of \`margot_plot()\` or
  \`margot_plot_multi_arm()\`. This list should have a \`plot\` element
  that contains the ggplot object.

- prefix:

  Character string. A prefix to add to the filename. Default is NULL.

- base_filename:

  Character string. The base name for the saved file. Default is
  "margot_plot".

- save_path:

  Character string. The directory path where the image will be saved.
  Default is here::here("push_mods").

- width:

  Numeric. The width of the saved image in inches. Default is 16.

- height:

  Numeric. The height of the saved image in inches. Default is 8.

- dpi:

  Numeric. The resolution of the saved image in dots per inch. Default
  is 500.

## Value

Invisibly returns the path of the saved file.

## Details

This function uses \`ggsave()\` to save the Margot plot as a PNG image.
If the save_path directory doesn't exist, it will be created. The final
filename will be constructed as: \`prefix_base_filename.png\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming you have already run margot_plot() or margot_plot_multi_arm()
plot_result <- margot_plot(your_data, your_options)

# Save the plot as PNG
margot_plot_save_png(
  plot_result,
  prefix = "study1",
  base_filename = "treatment_effects",
  save_path = here::here("output", "plots")
)
} # }
```
