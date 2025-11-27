# Generate Test Data with Flipped Outcomes

Convenience function that generates test data and creates flipped
versions of specified outcomes for testing flip forest functionality.

## Usage

``` r
margot_simulate_test_data_flip(flip_outcomes = "Y1", ...)
```

## Arguments

- flip_outcomes:

  Character vector of outcome names to flip (default: "Y1")

- ...:

  Additional arguments passed to margot_simulate_test_data()

## Value

List with same structure as margot_simulate_test_data() plus flipped
outcome columns (with "\_r" suffix)

## Examples

``` r
# Generate test data with Y1 flipped
test_data_flip <- margot_simulate_test_data_flip()

# Generate test data with Y1 and Y4 flipped
test_data_flip_multi <- margot_simulate_test_data_flip(
  flip_outcomes = c("Y1", "Y4")
)
```
