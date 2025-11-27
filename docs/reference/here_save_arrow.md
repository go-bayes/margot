# Save Data Frame to Parquet File in a Specified Directory (Deprecated)

This function is deprecated and will be removed in future releases. For
saving data frames, consider using the \`here_save_qs\` function.

## Usage

``` r
here_save_arrow(df, name)
```

## Arguments

- df:

  Data frame to be saved.

- name:

  Character string specifying the base name of the file.

## Examples

``` r
if (FALSE) { # \dontrun{
my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save_arrow(my_df, "my_saved_dataframe")
} # }
```
