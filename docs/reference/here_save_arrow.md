# Save Data Frame to Parquet File in a Specified Directory

Saves the provided data frame or object as a \`.parquet\` file using the
specified name, within a directory defined by \`dir_path\`. This
function uses \`arrow::write_parquet()\`.

## Usage

``` r
here_save_arrow(
  df,
  name,
  dir_path = NULL,
  compression = "zstd",
  compression_level = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  Data frame to be saved.

- name:

  Character string specifying the base name of the file.

- dir_path:

  Character string specifying the directory path where the file will be
  saved. If NULL (default), uses \`push_mods\`.

- compression:

  Character string specifying the compression codec. Default is "zstd".

- compression_level:

  Optional integer for the compression level. Default is NULL.

- quiet:

  Logical. If TRUE, suppresses console output. Default is FALSE.

- ...:

  Additional arguments passed to \`arrow::write_parquet()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save_arrow(my_df, "my_saved_dataframe")
} # }
```
