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
#> Error in library(Amelia): there is no package called ‘Amelia’
data(africa) # example dataset from Amelia package
#> Warning: data set ‘africa’ not found
amelia_output <- amelia(x = africa, m = 5, idvars = "country") # impute data
#> Error in amelia(x = africa, m = 5, idvars = "country"): could not find function "amelia"

# convert amelia object to mice object
mids_obj <- margot_amelia_to_mice(amelia_output)
#> Error in margot_amelia_to_mice(amelia_output): could not find function "margot_amelia_to_mice"

# verify mids object
print(mids_obj)
#> Error: object 'mids_obj' not found
```
