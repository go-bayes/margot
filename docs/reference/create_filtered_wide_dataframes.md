# Create Filtered Wide Dataframes Based on Exposure Variables

This function processes a wide format dataframe to filter and create a
list of dataframes based on the levels of a specified factor exposure
variable. It separates the dataframes based on each level of the factor
variable, if one is present. If only continuous variables are specified,
it returns the dataframe arranged by an identifier.

## Usage

``` r
create_filtered_wide_dataframes(dat_wide, exposure_vars)
```

## Arguments

- dat_wide:

  A wide format dataframe that contains the exposure variables.

- exposure_vars:

  A character vector listing the names of the exposure variables. This
  vector must include at least one valid variable name in the dataframe.

## Value

A list of dataframes, each corresponding to a level of the factor
exposure variable if a factor is present; otherwise, a single dataframe
arranged by identifier.

## Examples

``` r
# Assuming wide_data is a dataframe and "exposure_var" includes factor or continuous variables:
list_filtered_df <- create_filtered_wide_dataframes(wide_data, c("exposure_var1", "exposure_var2"))
#> Error in eval(expr, envir, enclos): object 'wide_data' not found
# Access individual filtered dataframe if factor variables present:
q1_df <- list_filtered_df[["tile_1"]]  # For factor level "tile_1"
#> Error in eval(expr, envir, enclos): object 'list_filtered_df' not found
```
