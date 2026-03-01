# Read Data Frame from Parquet File in a Specified Directory

Reads a \`.parquet\` file specified by \`name\` from a directory defined
by \`dir_path\`, returning the data frame or object stored within. This
function uses \`arrow::read_parquet()\` and \`here::here()\` to
construct portable file paths.

## Usage

``` r
here_read_arrow(name, dir_path = NULL, quiet = FALSE, ...)
```

## Arguments

- name:

  Character string specifying the name of the Parquet file to be read
  (without the ".parquet" extension).

- dir_path:

  Character string specifying the directory path from which the file
  will be read. If NULL (default), uses \`push_mods\`.

- quiet:

  Logical. If TRUE, suppresses console output. Default is FALSE.

- ...:

  Additional arguments passed to \`arrow::read_parquet()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
my_df <- here_read_arrow("my_dataset")
} # }
```
