# Save an R Object as RDS in a Specified Directory

Saves an R object as an RDS file using the specified name, within a
directory defined by \`push_mods\`. This helper accepts any R object,
including a data frame. Use \[here_save_arrow()\] when Parquet is the
appropriate representation for rectangular tabular data. This function
uses the \`here\` package to construct the path, ensuring that file
paths are built in a consistent and platform-independent manner.

## Usage

``` r
here_save(df, name, dir_path = NULL, compress = TRUE, quiet = FALSE)
```

## Arguments

- df:

  R object to be saved.

- name:

  Character string specifying the base name of the file. The ".rds"
  extension will be automatically appended to this name.

- dir_path:

  Character string specifying the directory path where the file will be
  saved. If NULL (default), uses \`push_mods\`.

- compress:

  Logical or character string specifying the type of compression to use.
  See \`?saveRDS\` for details. Default is TRUE.

- quiet:

  Logical. If TRUE, suppresses console output. Default is FALSE.

## Value

Invisibly returns the full path to the saved file.

## Details

If \`dir_path\` is NULL, the \`push_mods\` variable should be defined in
the user's environment or within the package and should point to the
directory where files will be saved. It is assumed that the specified
directory exists. This function does not create directories.

## Examples

``` r
if (FALSE) { # \dontrun{
# assuming `push_mods` is set in your environment to "~/mydata"
result <- list(estimate = 0.3, interval = c(0.2, 0.4))
here_save(result, "result")

# specifying a custom directory
here_save(result, "result", dir_path = "~/custom_dir", compress = "xz")
} # }
```
