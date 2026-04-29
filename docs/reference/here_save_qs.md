# Save Object to qs2 File in a Specified Directory

Saves the provided object as a \`.qs2\` file using the specified
\`name\`, within a directory defined by \`dir_path\`. Internally uses
the \`qs2\` package, which supersedes the original \`qs\` package.
Existing \`.qs\` files written by earlier versions of this function
remain readable via \`here_read_qs()\`.

## Usage

``` r
here_save_qs(
  obj,
  name,
  dir_path,
  compress_level = 4,
  nthreads = 1,
  preset = lifecycle::deprecated(),
  quiet = FALSE
)
```

## Arguments

- obj:

  Object to be saved.

- name:

  Character string specifying the base name of the file (no extension).

- dir_path:

  Character string specifying the directory path.

- compress_level:

  Integer between 1 and 22 controlling zstd compression. Defaults to
  \`4\` (the \`qs2\` default). Higher values compress more but more
  slowly.

- nthreads:

  Integer; number of threads for compression. Default \`1\`.

- preset:

  Deprecated. Was used by the old \`qs\` backend; now ignored.

- quiet:

  Logical. If TRUE, suppresses console output. Default FALSE.

## Value

Invisible NULL.

## Details

\`dir_path\` must point to an existing directory. The function does not
create directories.

## Examples

``` r
if (FALSE) { # \dontrun{
  my_df <- data.frame(x = 1:5, y = letters[1:5])
  here_save_qs(my_df, "my_saved_dataframe", "~/mydata")
} # }
```
