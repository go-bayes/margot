# Save Tabular Data to Parquet in a Specified Directory

Saves a data frame or Arrow table as a \`.parquet\` file under \`name\`
in \`dir_path\`. Use \[here_save()\] for lists, fitted models, plots,
and other non-tabular R objects.

## Usage

``` r
here_save_arrow(
  obj,
  name,
  dir_path = NULL,
  compression = "zstd",
  compression_level = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- obj:

  Data frame or Arrow table to be saved.

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
