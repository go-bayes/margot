# Conditional-moment concentration for a discrete intervention

Assess the concentration of uncapped exposure-only ratios for one arm
and assigned prefix. Observation probabilities define a separate
averaging measure. Supply the study's threshold explicitly.

## Usage

``` r
margot_lmtp_support_concentration(
  natural_mass,
  policy_mass,
  preceding_weight,
  reached,
  row_id,
  minimum,
  observation_weight = NULL
)
```

## Arguments

- natural_mass, policy_mass:

  Numeric probability matrices. Rows have names equal to row_id;
  matching column names identify discrete values or regions with a
  common ratio within each region. Each reached row sums to one, within
  a tolerance of 1e-12. Each entry must lie in \[0, 1\] exactly; the
  tolerance applies only to the row sum.

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

## Value

A list with fraction, equivalent (fraction times reached count), n, and
state (pass, fail, boundary_fail, or unassessable). Assessed inputs also
give boundary_records and sole_failures. A required positive policy mass
with zero natural mass on a positive-weight row gives boundary_fail and
no fraction. Invalid or numerically unrepresentable inputs give
unassessable.

## Details

With normalised observation weights b, preceding exposure ratios W and
m2 = sum(q^2/g), the fraction is (sum(b \* W))^2 / sum(b \* W^2 \* m2).
The observation weights enter once. Zero-W records remain in the
averaging population; their current exposure divisions are skipped.
Inputs are never trimmed. The fraction assesses the supplied fitted
laws; it cannot establish positivity, identification or coverage.

## Examples

``` r
ids <- c("a", "b")
g <- matrix(c(.2, .8, .2, .8), 2, byrow = TRUE,
  dimnames = list(ids, c("zero", "positive")))
q <- g
q[, ] <- rep(c(1, 0), each = 2)
margot_lmtp_support_concentration(g, q, setNames(c(1, 1), ids),
  setNames(c(TRUE, TRUE), ids), ids, minimum = .05)
#> $fraction
#> [1] 0.2
#> 
#> $equivalent
#> [1] 0.4
#> 
#> $n
#> [1] 2
#> 
#> $state
#> [1] "pass"
#> 
#> $boundary_records
#> [1] 0
#> 
#> $sole_failures
#> [1] 0
#> 
```
