# Assess and propagate one discrete exposure-support prefix

Evaluate the current conditional moment and, after a pass, optionally
carry realised exposure ratios to the next assignment.

## Usage

``` r
margot_lmtp_support_prefix(
  natural_mass,
  policy_mass,
  observed,
  preceding_weight,
  reached,
  row_id,
  minimum,
  observation_weight = NULL,
  excluded = character(),
  propagate = TRUE
)
```

## Arguments

- natural_mass, policy_mass:

  Numeric probability matrices. Rows have names equal to row_id;
  matching column names identify discrete values or regions with a
  common ratio within each region. Each reached row sums to one, within
  a tolerance of 1e-12. Each entry must lie in \[0, 1\] exactly; the
  tolerance applies only to the row sum.

- observed:

  Named character vector of observed discrete values or region labels,
  matching the probability-matrix column names on reached rows.

- preceding_weight:

  Named numeric vector of products of exposure ratios before this
  assignment. Use ones for the first assignment.

- reached:

  Named logical vector marking the current observation risk set.

- row_id:

  Unique nonempty character identifiers in input order.

- minimum:

  Study-selected concentration threshold in (0, 1\]. Equality passes.
  This is a decision convention, not a validity guarantee.

- observation_weight:

  Optional positive, named inverse-observation weights. They are
  normalised over all reached records, including records with zero
  preceding exposure weight. NULL uses equal averaging weights.

- excluded:

  Character vector of regions excluded by the policy by definition.
  Their policy mass must be zero on reached rows.

- propagate:

  Whether to calculate realised ratios and next weights after a pass.
  Set FALSE at the last assigned wave when no propagation is needed.

## Value

A list containing state, reason, concentration, diagnostics,
realised_ratio and next_weight, with the supplied probability laws and
observed categories. Invalid inputs return a reason and empty results.
Failures after concentration retain the assessment and its diagnostics.

## Details

A required zero natural mass fails the conditional-moment check. During
propagation, an observed category with zero natural mass is allowed only
when it is explicitly excluded by the policy. Otherwise propagation is
unassessable. Rows with zero prior weight stay zero without division.
With propagate = FALSE, callers must still report other_zero_mass in
diagnostics, even if the conditional-moment assessment passes. This
function does not choose or shorten a study's assignment schedule.

## Examples

``` r
ids <- c("a", "b")
g <- matrix(c(.2, .8, .2, .8), 2, byrow = TRUE,
  dimnames = list(ids, c("zero", "positive")))
q <- g
q[, ] <- rep(c(1, 0), each = 2)
margot_lmtp_support_prefix(g, q, setNames(c("zero", "positive"), ids),
  setNames(c(1, 1), ids), setNames(c(TRUE, TRUE), ids), ids,
  minimum = .05, excluded = "positive")
#> $state
#> [1] "pass"
#>
#> $reason
#> NULL
#>
#> $concentration
#> $concentration$fraction
#> [1] 0.2
#>
#> $concentration$equivalent
#> [1] 0.4
#>
#> $concentration$n
#> [1] 2
#>
#> $concentration$state
#> [1] "pass"
#>
#> $concentration$boundary_records
#> [1] 0
#>
#> $concentration$sole_failures
#> [1] 0
#>
#>
#> $diagnostics
#> $diagnostics$boundary_records
#> [1] 0
#>
#> $diagnostics$sole_failures
#> [1] 0
#>
#> $diagnostics$excluded_zero_mass
#> [1] 0
#>
#> $diagnostics$other_zero_mass
#> [1] 0
#>
#>
#> $realised_ratio
#> a b
#> 5 0
#>
#> $next_weight
#> a b
#> 5 0
#>
#> $natural_mass
#>   zero positive
#> a  0.2      0.8
#> b  0.2      0.8
#>
#> $policy_mass
#>   zero positive
#> a    1        0
#> b    1        0
#>
#> $observed
#>          a          b
#>     "zero" "positive"
#>
```
