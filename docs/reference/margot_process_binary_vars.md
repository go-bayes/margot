# Process Binary Variables in a Data Frame

This function identifies binary variables (both factors and numeric),
converts them to 0/1 format, and renames them. It allows for exceptions
to be specified, and ignores variables whose names already end with the
specified suffix.

## Usage

``` r
margot_process_binary_vars(data, exceptions = character(0), suffix = "_binary")
```

## Arguments

- data:

  A data frame to process.

- exceptions:

  A character vector of column names to exclude from processing.

- suffix:

  A string to append to renamed binary variables. Default is "\_binary".

## Value

A data frame with processed binary variables.

## Examples

``` r
df <- data.frame(
  a = factor(c("yes", "no", "yes")),
  b = c(1, 0, 1),
  c = c("apple", "banana", "apple"),
  d = factor(c("true", "false", "true")),
  e_binary = c(0, 1, 0)
)
processed_df <- margot_process_binary_vars(df, exceptions = "c")
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: cli
#> Loading required package: knitr
#> 
#> ── Processing Binary Variables ─────────────────────────────────────────────────
#> ℹ Ignoring variables with suffix '_binary': e_binary
#> 
#> ── Initial Data Summary ──
#> 
#> Total variables: 5
#> Total observations: 3
#> Exceptions specified: 1
#> 
#> ── Binary Variables Identified ──
#> 
#> Total binary variables: 3
#> List of binary variables:
#> 
#> 
#> |Variable |
#> |:--------|
#> |a        |
#> |b        |
#> |d        |
#> 
#> ── Variable Renaming ──
#> 
#> 
#> 
#> |Original |New      |
#> |:--------|:--------|
#> |a        |a_binary |
#> |b        |b_binary |
#> |d        |d_binary |
#> ── Final Data Summary ──
#> 
#> Total variables: 5
#> Variables processed: 3
#> Variables excluded (exceptions and ignored): 2
#> ✔ Binary variable processing completed successfully!
#> ────────────────────────────────────────────────────────────────────────────────
```
