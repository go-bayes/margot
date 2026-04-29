# Read Object from qs2 (or legacy qs) File in a Specified Directory

Reads a serialised R object stored under \`name\` in \`dir_path\`. The
function looks for \`\<name\>.qs2\` first; if absent, it falls back to
\`\<name\>.qs\` (legacy format). Reading legacy \`.qs\` files requires
the \`qs\` package to be installed; if it is not, the function points at
\`margot_convert_qs_dir()\` or \`install.packages("qs")\`.

## Usage

``` r
here_read_qs(name, dir_path = NULL, nthreads = 1, quiet = FALSE)
```

## Arguments

- name:

  Character string; base file name (no extension).

- dir_path:

  Character string; directory to read from. If NULL (default), uses
  \`push_mods\`.

- nthreads:

  Integer; threads for decompression. Default \`1\`.

- quiet:

  Logical. If TRUE, suppresses console output. Default FALSE.

## Value

The object stored in the file.

## Examples

``` r
if (FALSE) { # \dontrun{
  my_df <- here_read_qs("my_dataset")
  my_df <- here_read_qs("my_dataset", dir_path = "~/custom_dir")
} # }
```
