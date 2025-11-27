# Format Numbers with Commas

This function takes a numeric vector and formats it by inserting commas
as thousands separators, making large numbers easier to read.

## Usage

``` r
pretty_number(x)
```

## Arguments

- x:

  A numeric vector that you want to format.

## Value

A character vector where each number is formatted with commas as
thousands separators.

## Examples

``` r
numbers <- c(1000, 50000, 1234567)
pretty_number(numbers)
#> [1] "1,000"     "50,000"    "1,234,567"
```
