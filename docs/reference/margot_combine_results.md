# Combine Multiple Results Tables from margot_plot into a Single Formatted Table

Takes multiple results tables from \`margot_plot\` objects and combines
them into a single formatted table using kableExtra, with optional group
headers for each section.

## Usage

``` r
margot_combine_results(
  results,
  options = NULL,
  format = "latex",
  digits = 2,
  ...
)
```

## Arguments

- results:

  A named list of data frames, typically extracted from \`margot_plot\`
  objects using \`\$transformed_table\`. Names will be used as section
  headers if no options are provided.

- options:

  Optional list of options created by \`margot_plot_create_options()\`.
  Should have the same names as the results list. Each option object can
  include a 'subtitle' that will be used as the section header.

- format:

  Output format for kable. Default is "latex".

- digits:

  Number of decimal places for rounding numeric values. Default is 2.

- ...:

  Additional arguments passed to kable().

## Value

A kable object that can be further customized using kableExtra
functions.

## Note

The group_rows function exists in both dplyr and kableExtra packages.
This function uses kableExtra::group_rows, which causes a harmless
namespace warning when loading the package.

## Examples

``` r
if (FALSE) { # \dontrun{
# Suppose we have domain-based results:
results_list <- list(
  Health = list(
    transformed_table = health_religious_vs_secular$transformed_table,
    interpretation = health_religious_vs_secular$interpretation
  ),
  Psychological = list(
    transformed_table = psych_religious_vs_secular$transformed_table,
    interpretation = psych_religious_vs_secular$interpretation
  )
)

# And corresponding options:
options_list <- list(
  Health = margot_plot_create_options(
    subtitle = "Health: Religious vs Secular (baseline)",
  ),
  Psychological = margot_plot_create_options(
    subtitle = "Psychological: Religious vs Secular (baseline)",
  )
)

# Combine the results and print:
combined_table <- margot_combine_results(
  results = results_list,
  options = options_list,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  digits = 2
)
} # }
```
