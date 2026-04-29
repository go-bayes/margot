# Save Object to Parquet File in a Specified Directory

Saves the provided object as a \`.parquet\` file under \`name\`, in
\`dir_path\`. Data frames (and arrow tables) are written natively. Other
R objects (lists, fitted models, ggplots, ...) are serialised with
\`qs2::qs_serialize()\` and embedded in a single-row parquet envelope so
the round-trip is lossless via \`here_read_arrow()\`.

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

  Object to be saved.

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

fit <- lm(mpg ~ wt, data = mtcars)
here_save_arrow(fit, "fit_arrow")  # wrapped via qs2 envelope
} # }
```
