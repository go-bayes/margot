# Fetch Margot Example Data

Downloads or loads margot example datasets. By default, returns a small
simulated dataset suitable for examples. For the full dataset used in
publications and teaching, specify version = "full".

## Usage

``` r
fetch_margot_data(version = "example", cache = TRUE, quiet = FALSE)
```

## Arguments

- version:

  Character string specifying which dataset version to fetch. Options
  are: - "example" (default): Small simulated dataset (~1MB) included in
  package - "v1": Original full simulated NZAVS-style data (~27MB) -
  "v2": Updated simulated data with additional waves (~35MB) - "latest":
  Most recent version available

- cache:

  Logical. If TRUE (default), caches downloaded data locally to avoid
  repeated downloads.

- quiet:

  Logical. If FALSE (default), shows download progress.

## Value

A data frame containing the requested margot example data

## Details

The example dataset is a small simulated dataset included with the
package, suitable for running examples and tests. The full datasets (v1,
v2) are larger simulated datasets that mirror the structure of the New
Zealand Attitudes and Values Study (NZAVS) but contain no real
participant data.

Full datasets are hosted on the Open Science Framework (OSF) and are
downloaded on first use. Subsequent calls use the cached version unless
cache = FALSE.

## Examples

``` r
# load small example dataset (included in package)
df_example <- fetch_margot_data()
#> ℹ Loading built-in example dataset

if (FALSE) { # \dontrun{
# fetch full simulated dataset from OSF
df_full <- fetch_margot_data(version = "v1")

# fetch latest version
df_latest <- fetch_margot_data(version = "latest")

# force re-download
df_fresh <- fetch_margot_data(version = "v1", cache = FALSE)
} # }
```
