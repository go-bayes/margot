# Read Object from a Deprecated qs2 or Legacy qs File

Reads a serialised R object stored under \`name\` in \`dir_path\`. The
function looks for \`\<name\>.qs2\` first. If no \`.qs2\` file exists,
it falls back to a legacy \`\<name\>.qs\` file when the optional \`qs\`
package is installed. This helper is deprecated for new storage, but
remains useful while converting old project archives.

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
