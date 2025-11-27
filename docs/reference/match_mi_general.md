# General Matching Function for Multiple Imputation Data

This function facilitates propensity score matching on datasets,
including those generated through multiple imputation, to assess
covariate balance across treatment groups. It leverages the
[`WeightIt`](https://ngreifer.github.io/WeightIt/reference/WeightIt-package.html)
and `MatchThem` packages to calculate propensity scores and apply
matching techniques. Users are referred to these packages for detailed
methodological specifications and additional functionality.

## Usage

``` r
match_mi_general(
  data,
  X,
  baseline_vars,
  estimand,
  method,
  subgroup = NULL,
  focal = NULL,
  sample_weights = NULL
)
```

## Arguments

- data:

  A data frame or a 'mids' object containing the data for matching.

- X:

  A string naming the treatment or exposure variable within \`data\`.

- baseline_vars:

  A character vector naming the covariates to include in the propensity
  score model.

- estimand:

  A string specifying the target estimand ('ATE', 'ATT', or 'ATC').

- method:

  The method for estimating propensity scores, per the
  [`WeightIt`](https://ngreifer.github.io/WeightIt/reference/WeightIt-package.html)
  package's documentation.

- subgroup:

  (Optional) A string specifying a variable by which to subgroup the
  data for within-group matching.

- focal:

  (Optional) Specifies the focal treatment group, useful for 'ATT' or
  'ATC' estimands.

- sample_weights:

  (Optional) A string indicating the variable in \`data\` representing
  sample weights.

- stabilize:

  Logical. Indicates whether to stabilize weights, defaulting to TRUE.

## Value

A list of matched datasets for each subgroup level (if \`subgroup\` is
used) or a single matched dataset. Each element is typically a data
frame or an object reflecting the matched data structure, depending on
the matching method applied.

## References

Detailed methodology and additional options can be found in: -
[`WeightIt`](https://ngreifer.github.io/WeightIt/reference/WeightIt-package.html)
package for propensity score estimation. - `MatchThem` package for
matching within imputed datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume `df` is a data frame with treatment, covariates, and sample weights
matched_data <- match_mi_general(data = df,
                                 X = "treatment_var",
                                 baseline_vars = c("covariate1", "covariate2"),
                                 estimand = "ATE",
                                 method = "nearest",
                                 stabilize = TRUE)
} # }
```
