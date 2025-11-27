# Remove Attributes from Numeric Columns in a Data Frame

Iterates over each column in the provided data frame. If a column is
numeric and has attributes, this function removes those attributes by
converting the column to a basic numeric vector. This is particularly
useful for cleaning data frames after operations that may add undesired
attributes to numeric columns, such as aggregations or merges.

## Usage

``` r
remove_numeric_attributes(df)
```

## Arguments

- df:

  A \`data.frame\` object from which attributes of numeric columns will
  be removed.

## Value

A \`data.frame\` with attributes removed from all numeric columns.

## Examples

``` r
df <- data.frame(a = I(1:3), b = c("x", "y", "z"), c = I(rnorm(3)))
cleaned_df <- remove_numeric_attributes(df)
str(cleaned_df)
#> 'data.frame':    3 obs. of  3 variables:
#>  $ a: 'AsIs' int  1 2 3
#>  $ b: chr  "x" "y" "z"
#>  $ c: 'AsIs' num  1.200965.... 1.044751.... -1.00320....
```
