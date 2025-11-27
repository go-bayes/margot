# Plot Rank Average Treatment Effect

This function creates a ggplot visualisation of the Rank Average
Treatment Effect. It displays the estimate with a confidence interval,
using a simple black line and light gray shading by default.

## Usage

``` r
plot_rank_average_treatment_effect(
  x,
  title = "Targeting Operator Characteristic",
  subtitle = "(95% confidence interval shown as shaded area)",
  x_lab = "Treated fraction (q)",
  y_lab = "Estimate",
  ...
)
```

## Arguments

- x:

  An object of class "rank_average_treatment_effect", typically the
  output of the rank_average_treatment_effect() function.

- title:

  Character string for the plot title. Default is "Targeting Operator
  Characteristic".

- subtitle:

  Character string for the plot subtitle. Default explains the
  confidence interval.

- x_lab:

  Character string for the x-axis label. Default is "Treated fraction
  (q)".

- y_lab:

  Character string for the y-axis label. Default is "Estimate".

- ...:

  Additional arguments passed to ggplot.

## Value

A ggplot object that can be further customised or printed.

## Examples

``` r
if (FALSE) {
# Assuming rate_eval is your rank_average_treatment_effect object
p <- plot_rank_average_treatment_effect(rate_eval)
print(p)

# Customise colors using ggokabeito
p_colored <- p +
  ggokabeito::scale_fill_okabe_ito() +
  ggokabeito::scale_color_okabe_ito()
print(p_colored)
}
```
