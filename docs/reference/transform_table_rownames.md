# Transform Table Row Names with CLI Feedback

This function transforms the row names of a data frame based on
specified criteria and provides CLI feedback on the changes made.

## Usage

``` r
transform_table_rownames(df, label_mapping = NULL, options = list())
```

## Arguments

- df:

  A data frame whose row names are to be transformed.

- remove_tx_prefix:

  Logical. If TRUE, removes 't' followed by numbers and underscore from
  the start of row names.

- remove_z_suffix:

  Logical. If TRUE, removes '\_z' from the end of row names.

- use_title_case:

  Logical. If TRUE, converts row names to title case.

- remove_underscores:

  Logical. If TRUE, replaces underscores with spaces in row names.

## Value

A data frame with transformed row names.

## Details

The function applies the following transformations in order: 1. Removes
't' followed by numbers and underscore from the start (if
remove_tx_prefix is TRUE) 2. Removes '\_z' from the end (if
remove_z_suffix is TRUE) 3. Replaces underscores with spaces (if
remove_underscores is TRUE) 4. Converts to title case (if use_title_case
is TRUE)

The function provides CLI feedback for each change made and a summary of
the transformation process.

## Examples

``` r
df <- data.frame(x = 1:3, row.names = c("t1_variable_z", "t2_another_var", "last_variable"))
transformed_df <- transform_table_rownames(df)
#> Error in transform_table_rownames(df): could not find function "transform_table_rownames"
```
