# Visualize Distribution with Mean and Standard Deviation Highlights

This function creates a histogram for a specified column in a dataframe,
highlighting the mean and one standard deviation above and below the
mean. It draws vertical lines for the mean (in black) and for plus/minus
one standard deviation (in blue and gold, respectively), with arrows
from the mean to each standard deviation marker. The title of the plot
includes the capitalized column name, achieved using
[`tools::toTitleCase()`](https://rdrr.io/r/tools/toTitleCase.html).

## Usage

``` r
coloured_histogram_sd(df, col_name, binwidth = 1)
```

## Arguments

- df:

  Dataframe containing the data to be visualized.

- col_name:

  Name of the column to create a histogram for. This column should
  contain numeric data.

- binwidth:

  Width of the bins for the histogram. Can be adjusted for finer or
  coarser resolution of the distribution. Default is 1.

## Value

A ggplot object representing the histogram with highlights for the mean
and standard deviations. The plot can be printed or modified further.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Assuming `df_margot_example` is a dataframe with a numeric column 'forgiveness'
# and a factor or integer column 'wave' for subsetting:
df_19 <- dplyr::filter(df_margot_example, wave == 1)

graph_density_of_exposure <- coloured_histogram_sd(
  df = df_19,
  col_name = "forgiveness",
  binwidth = 0.5 # Adjust binwidth as needed
)
} # }
print(graph_density_of_exposure)
#> Error: object 'graph_density_of_exposure' not found
```
