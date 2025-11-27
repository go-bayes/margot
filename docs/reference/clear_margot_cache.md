# Clear Margot Data Cache

Removes cached margot datasets to free disk space or force
re-downloading

## Usage

``` r
clear_margot_cache(version = "all", confirm = TRUE)
```

## Arguments

- version:

  Character string specifying which version to clear, or "all" to clear
  entire cache

- confirm:

  Logical. If TRUE (default), asks for confirmation before deleting.

## Value

NULL (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# clear specific version
clear_margot_cache("v1")

# clear all cached data
clear_margot_cache("all")
} # }
```
