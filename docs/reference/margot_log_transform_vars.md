# Log-transform Variables in a Data Frame

This function applies a log(x + 1) transformation to specified variables
in a data frame. It handles NA values, warns and drops non-numeric
selections, allows for exceptions, and can be applied to variables with
specific prefixes.

## Usage

``` r
margot_log_transform_vars(
  data,
  vars,
  exceptions = character(0),
  prefix = "log_",
  keep_original = TRUE
)
```

## Arguments

- data:

  a data frame to process.

- vars:

  a character vector of variable names or a tidyselect helper (e.g.,
  starts_with("hours\_")).

- exceptions:

  a character vector of variable names to exclude from transformation.

- prefix:

  a string to prepend to the names of transformed variables. default is
  "log\_."

- keep_original:

  logical; if true, keeps both original and transformed variables. if
  false, replaces original. default is true.

## Value

a data frame with log-transformed variables.
