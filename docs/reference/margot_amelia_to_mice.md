# convert an amelia object to a mice object

convert an amelia object to a mice object

## Usage

``` r
margot_amelia_to_mice(amelia_obj)
```

## Arguments

- amelia_output:

  an object of class \`amelia\`, containing imputed datasets from the
  Amelia package

## Value

a \`mids\` object compatible with the \`mice\` package, structured with
the original dataset and imputed values

## Examples

``` r
# load Amelia package and perform imputation
library(Amelia)
#> Loading required package: Rcpp
#> ## 
#> ## Amelia II: Multiple Imputation
#> ## (Version 1.8.3, built: 2024-11-07)
#> ## Copyright (C) 2005-2026 James Honaker, Gary King and Matthew Blackwell
#> ## Refer to http://gking.harvard.edu/amelia/ for more information
#> ## 
data(africa) # example dataset from Amelia package
amelia_output <- amelia(x = africa, m = 5, idvars = "country") # impute data
#> -- Imputation 1 --
#> 
#>   1  2  3
#> 
#> -- Imputation 2 --
#> 
#>   1  2  3
#> 
#> -- Imputation 3 --
#> 
#>   1  2  3  4
#> 
#> -- Imputation 4 --
#> 
#>   1  2
#> 
#> -- Imputation 5 --
#> 
#>   1  2
#> 

# convert amelia object to mice object
mids_obj <- margot_amelia_to_mice(amelia_output)
#> Error in margot_amelia_to_mice(amelia_output): could not find function "margot_amelia_to_mice"

# verify mids object
print(mids_obj)
#> Error: object 'mids_obj' not found
```
