# Filter Data Based on Exposure Variables

This function filters a dataframe based on the levels of a single factor
variable or arranges the dataframe by identifier if only continuous
variables are present.

## Usage

``` r
margot_filter(dat_wide, exposure_vars, sort_var = "id")
```

## Arguments

- dat_wide:

  Dataframe to filter.

- exposure_vars:

  Vector of names of exposure variables to consider.

- sort_var:

  Optional; the variable by which to sort the dataframes.

## Value

A list of dataframes filtered by the levels of the factor variable or
arranged by identifier.
