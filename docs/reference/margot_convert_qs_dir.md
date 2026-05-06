# Convert legacy \`.qs\` files in a directory to \`.qs2\`

Walks \`dir_path\`, reads every \`.qs\` file with the optional legacy
\`qs\` package, and writes a sibling \`.qs2\` file using
\`qs2::qs_save()\`. Originals are kept by default; pass \`delete_qs =
TRUE\` to remove them once the \`.qs2\` sibling is written and
read-verified.

## Usage

``` r
margot_convert_qs_dir(
  dir_path,
  recursive = TRUE,
  compress_level = 4,
  delete_qs = FALSE,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dir_path:

  Character; directory to scan.

- recursive:

  Logical; recurse into subdirectories. Default \`TRUE\`.

- compress_level:

  Integer; zstd compression level for the new \`.qs2\` files. Default
  \`4\`.

- delete_qs:

  Logical; if TRUE, delete each \`.qs\` file after a successful \`.qs2\`
  round-trip read. Default \`FALSE\`.

- overwrite:

  Logical; if a \`.qs2\` sibling already exists, overwrite it. Default
  \`FALSE\` (skip).

- quiet:

  Logical; if TRUE, suppress per-file output. Default \`FALSE\`.

## Value

Invisibly, a tibble of \`path\`, \`status\` (\`"converted"\`,
\`"skipped"\`, \`"failed"\`), and \`message\` for every file processed.

## Details

Use this direct converter when you have an R environment where \`qs\` is
available. If local \`qs\` is not available, use
\[margot_convert_qs_dir_docker()\] as a fallback.

## See also

\[margot_convert_qs_dir_docker()\] for users on R 4.6+ who can run
Docker.

## Examples

``` r
if (FALSE) { # \dontrun{
  margot_convert_qs_dir("~/Library/CloudStorage/.../lab-09-cache")
} # }
```
