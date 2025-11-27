# Transform Z-Score Results to Original Scale

This function takes a dataframe of z-score results and transforms them
back to the original scale using the standard deviations from the
original dataset. It is particularly useful for interpreting causal
contrasts that were calculated on z-transformed data.

## Usage

``` r
transform_to_original_scale(results_df, original_df, label_mapping = NULL)
```

## Arguments

- results_df:

  A dataframe containing the z-score results.

- original_df:

  A dataframe containing the original (non-transformed) data. Column
  names should match those in results_df or be mappable via
  label_mapping.

- label_mapping:

  An optional named list mapping row names in results_df to column names
  in original_df. Use this if the names do not match exactly.

## Value

A dataframe similar to results_df, with additional columns for the
back-transformed estimates and confidence intervals (suffixed with
"\_original").
