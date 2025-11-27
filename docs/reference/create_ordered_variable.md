# Create Ordered Variable Based on Quantile Breaks or Custom Breaks with Informative Labels

This function divides a numeric variable into categories based on either
quantile breaks or custom-specified breaks, and creates an ordered
factor variable with informative labels. It now includes rich CLI
reporting for better user feedback.

## Usage

``` r
create_ordered_variable(
  df,
  var_name,
  n_divisions = NULL,
  cutpoint_inclusive = "upper",
  ties.method = NULL,
  custom_breaks = NULL
)
```

## Arguments

- df:

  A data frame containing the variable to be divided into categories.

- var_name:

  The name of the variable within the data frame to divide into
  categories.

- n_divisions:

  The number of quantile divisions to create. Required if custom_breaks
  is not provided.

- cutpoint_inclusive:

  A character string specifying whether cutpoints should be included in
  the lower or upper category. Must be either "lower" or "upper".
  Default is "upper".

- ties.method:

  A character string specifying how ties should be handled when
  calculating quantiles. Must be one of "first", "last", "random",
  "ordered", or "average". If NULL (default), it will be set to "last"
  if cutpoint_inclusive is "upper", and "first" if cutpoint_inclusive is
  "lower".

- custom_breaks:

  A numeric vector of break points to use for categorisation. If
  provided, this overrides the quantile-based division specified by
  n_divisions.

## Value

The input data frame with an additional column representing the ordered
factor variable. The new column name will be the original variable name
with "\_binary" appended if there are 2 divisions, or "\_cat" otherwise.
