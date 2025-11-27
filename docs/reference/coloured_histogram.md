# Create a Coloured Histogram Highlighting Specific Ranges (DEPRECATED)

\`r lifecycle::badge("deprecated")\` This function is deprecated. Please
use \`margot_plot_shift()\` instead.

This function generates a histogram with specific ranges highlighted to
indicate the highest and/or lowest values within a unit of the specified
limits. It allows customization of bin width, the unit of change for
highlighting, and the range to be highlighted. This is useful in the
settings of modified treatment policies for clarifying which part of a
distribution is shifted.

## Usage

``` r
coloured_histogram(
  df,
  col_name,
  binwidth = 1,
  unit_of_change = 1,
  scale_min = NULL,
  scale_max = NULL,
  highlight_range = "highest"
)
```

## Arguments

- df:

  The dataframe containing the data to be plotted.

- col_name:

  The name of the column for which the histogram will be generated.

- binwidth:

  The width of the bins for the histogram; defaults to 1.

- unit_of_change:

  The unit of change used to define the highlight range. The subtitle
  will mention this unit. It also adjusts the calculation of the
  highlight thresholds to be slightly less than this unit so that it
  does not go over the range of the data. Defaults to 1.

- scale_min:

  The minimum value to be used for scaling the histogram. If \`NULL\`,
  the minimum value of \`col_name\` is used.

- scale_max:

  The maximum value to be used for scaling the histogram. If \`NULL\`,
  the maximum value of \`col_name\` is used.

- highlight_range:

  Specifies which range to highlight: "lowest", "highest", or "both".
  Defaults to "highest".

## Value

A ggplot object of the histogram with highlighted ranges as specified.

## Examples

``` r
 if (FALSE) { # \dontrun{
# assuming df_19 is your dataframe and contains the column 'forgiveness'
graph <- coloured_histogram(
  df = df_19,
  col_name = "forgiveness",
  scale_min = 1,
  scale_max = 7,
  highlight_range = "highest",
  binwidth = .1, # adjust binwidth as needed
  unit_of_change = 1 # specify the unit of change
)
print(graph)
} # }
```
