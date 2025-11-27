# Select and Rename Columns Based on Criteria

Selects columns from a base set that match specified baseline variables
and renames an outcome variable by changing its prefix. Useful for
longitudinal data where time-point prefixes need to be standardised or
adjusted.

## Usage

``` r
select_and_rename_cols(
  names_base,
  baseline_vars,
  outcome,
  from_prefix = "t2",
  to_prefix = "t0"
)
```

## Arguments

- names_base:

  a character vector of column names from which to select.

- baseline_vars:

  a character vector of baseline variables to match in \`names_base\`.

- outcome:

  the name of the outcome variable whose prefix should be replaced.

- from_prefix:

  the original prefix of the outcome variable to be replaced, defaulting
  to "t2". the prefix should include any character immediately preceding
  the numeric value and underscore, e.g., "t2\_".

- to_prefix:

  the new prefix to replace the original prefix in the outcome variable,
  defaulting to "t0". the prefix should be in the same format as
  \`from_prefix\`, including the character immediately preceding the
  numeric value and underscore, e.g., "t0\_".

## Value

a character vector of selected column names with the outcome variable
name modified to reflect the new prefix.

## Examples

``` r
names_base <- c("t0_age", "t0_weight", "t0_height", "t0_outcome")
baseline_vars <- c("age", "weight")
outcome_var <- "t2_outcome"

final_columns <- select_and_rename_cols(names_base, baseline_vars, outcome_var, "t2", "t0")
print(final_columns)
#> [1] "t0_age"     "t0_weight"  "t0_outcome"
```
