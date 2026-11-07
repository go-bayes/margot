# Normalised inverse-observation weights for support assessment

Construct the averaging measure at one exposure position from the
conditional probabilities of its preceding observation transitions.

## Usage

``` r
margot_lmtp_support_weights(probability, reached, row_id)
```

## Arguments

- probability:

  Numeric matrix of conditional observation probabilities, with named
  rows equal to row_id and one named column per required preceding
  transition. Values outside reached are ignored.

- reached:

  Named logical vector identifying records observed at the current
  exposure.

- row_id:

  Unique nonempty character identifiers in input order.

## Value

A list with state, normalised weight, cumulative
log_inverse_observation, observation_effective_n, and reached count n.
Unreached vector entries are NA. Invalid probabilities, identities, an
empty risk set or numerical underflow return state unassessable with a
reason.

## Details

Products are accumulated in log space and rescaled by one common
constant. No probability floor or weight cap is applied. All reached
records enter normalisation, regardless of their exposure ratios.

## Examples

``` r
ids <- c("a", "b")
p <- matrix(c(1, .25), dimnames = list(ids, "observation"))
margot_lmtp_support_weights(p, setNames(c(TRUE, TRUE), ids), ids)
#> $state
#> [1] "assessable"
#>
#> $weight
#>   a   b
#> 0.2 0.8
#>
#> $log_inverse_observation
#>        a        b
#> 0.000000 1.386294
#>
#> $observation_effective_n
#> [1] 1.470588
#>
#> $n
#> [1] 2
#>
```
