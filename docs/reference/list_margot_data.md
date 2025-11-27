# List Available Margot Datasets

Shows information about all available margot example datasets

## Usage

``` r
list_margot_data(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. If TRUE (default), prints a formatted table. If FALSE,
  returns the data silently.

## Value

A tibble with information about available datasets (invisibly if verbose
= TRUE)

## Examples

``` r
# show available datasets
list_margot_data()
#> 
#> ── Available Margot Datasets ──
#> 
#> 
#> ℹ example: Small example dataset (built-in, ~1MB)
#> ℹ v1: Original simulated NZAVS-style data (27MB) (27.2 MB)
#> ℹ v2: Updated simulated data with additional waves (35 MB)
#> 
#> Use `fetch_margot_data(version = '...')` to load a dataset

# get as tibble
datasets <- list_margot_data(verbose = FALSE)
```
