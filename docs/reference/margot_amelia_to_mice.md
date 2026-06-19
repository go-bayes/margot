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
if (FALSE) { # \dontrun{
# load Amelia package and perform imputation
library(Amelia)
data(africa) # example dataset from Amelia package
amelia_output <- amelia(x = africa, m = 5, idvars = "country") # impute data

# convert amelia object to mice object
mids_obj <- margot_amelia_to_mice(amelia_output)

# verify mids object
print(mids_obj)
} # }
```
