# Calculate the size of an R object in megabytes

This function takes an R object and returns its size in megabytes (MB).
It's useful for understanding the memory footprint of large data
structures or complex objects in your R environment. It now includes cli
alerts for improved user feedback.

## Usage

``` r
margot_size(obj, name = "Object")
```

## Arguments

- obj:

  An R object whose size you want to measure

- name:

  An optional name for the object (default is "Object")

## Value

A character string representing the size of the object in MB, formatted
to two decimal places

## Note

KEY MESSAGE: Monitoring object sizes is crucial for efficient memory
management, especially when working with large datasets or complex
analyses. Use this function to keep track of memory usage and optimize
your R code for better performance.

## Examples

``` r
big_matrix <- matrix(rnorm(1e6), nrow = 1000)
margot_size(big_matrix, "Big Matrix")
#> ℹ Big Matrix size: 7.63 MB
#> [1] "7.63 MB"

summary_tables <- list(table1 = data.frame(a = 1:1000, b = rnorm(1000)))
margot_size(summary_tables, "Summary Tables")
#> ℹ Summary Tables size: 0.01 MB
#> [1] "0.01 MB"
```
