# Convert legacy \`.qs\` files via a Docker R 4.5 container

Recursively converts every \`.qs\` file under \`dir_path\` to a \`.qs2\`
sibling by running the conversion inside a \`rocker/r-ver:4.5\`
container, where the legacy \`qs\` package still builds. Designed for
users on R 4.6 (or later) for whom \`qs\` is no longer installable.
Requires Docker on \`PATH\`; no other setup.

## Usage

``` r
margot_convert_qs_dir_docker(
  dir_path,
  recursive = TRUE,
  compress_level = 4,
  delete_qs = FALSE,
  overwrite = FALSE,
  image = "rocker/r-ver:4.5",
  qs_source_url = "https://cran.r-project.org/src/contrib/Archive/qs/qs_0.27.3.tar.gz",
  ppm_snapshot = "https://packagemanager.posit.co/cran/2024-12-01",
  quiet = FALSE
)
```

## Arguments

- dir_path:

  Character; directory to scan. Mounted into the container at \`/data\`.

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

- image:

  Character; Docker image to use. Default \`"rocker/r-ver:4.5"\`.

- qs_source_url:

  Character; CRAN archive URL for the \`qs\` source tarball. Pinned to
  \`qs_0.27.3.tar.gz\` by default. Override only if a future version
  supersedes this.

- ppm_snapshot:

  Character; Posit Package Manager snapshot URL pinned to a date when
  \`qs\` 0.27.3 still built against the contemporaneous \`stringfish\`.
  Default \`2024-12-01\`. Override only if that snapshot is ever
  retired.

- quiet:

  Logical; if TRUE, suppress per-file output. Default \`FALSE\`.

## Value

Invisible integer, the docker process exit status (0 on success).

## Details

On the first run Docker pulls the R 4.5 image (~600 MB) and compiles
\`qs\` from CRAN's archive. Subsequent runs reuse the local image and
the cached library, so they are fast.

Originals are kept by default. Re-run with \`delete_qs = TRUE\` once you
have read-verified some \`.qs2\` files.

This function is the user-friendly counterpart to
\[margot_convert_qs_dir()\]. Use \[margot_convert_qs_dir()\] directly if
you already have an R session with \`qs\` installed; use this wrapper if
you are on a host where \`qs\` will not install.

For prerequisites (installing Docker / Colima, verifying the daemon,
Linux/Windows setup), see the package vignette
[`vignette("migrating-qs-to-qs2", package = "margot")`](https://go-bayes.github.io/margot/articles/migrating-qs-to-qs2.md).

Steps:

1.  Confirms \`docker\` is on \`PATH\`.

2.  Writes a small R script to a temp file containing the conversion
    logic.

3.  Invokes \`docker run –rm\` mounting \`dir_path\` at \`/data\` and
    the script at \`/migrate.R\`.

4.  Inside the container, installs \`qs2\` (CRAN) and \`qs\` (CRAN
    archive source), then walks \`/data\` recursively and converts every
    \`.qs\`.

5.  Returns when the container exits. The status code is propagated.

## See also

\[margot_convert_qs_dir()\] for direct (non-Docker) use.

## Examples

``` r
if (FALSE) { # \dontrun{
  margot_convert_qs_dir_docker("~/Library/CloudStorage/.../caches")
  # spot-check a converted file in your normal R session, then:
  margot_convert_qs_dir_docker("~/Library/CloudStorage/.../caches",
                               delete_qs = TRUE)
} # }
```
