# Sequential Cross-Validation for RATE

Internal function implementing the uncorrelated sequential
cross-validation approach from Wager (2024). This function is exported
for technical reasons (parallel processing) but should not be called
directly by users.

## Usage

``` r
rate_sequential_cv(
  X,
  Y,
  W,
  weights = NULL,
  num_folds = 5,
  target = "AUTOC",
  grf_defaults = list(),
  seed = 12345,
  verbose = FALSE,
  model_name = NULL,
  ...
)
```
