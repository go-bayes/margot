# Compute QINI Curves Following GRF/MAQ Methodology

Computes QINI curves for causal forest models using the exact
methodology from GRF/MAQ packages. Uses test set data when train/test
splits were used in margot_causal_forest().

## Usage

``` r
margot_qini_new(
  margot_result,
  model_names = NULL,
  seed = 12345,
  n_bootstrap = 200,
  verbose = TRUE,
  spend_levels = NULL,
  label_mapping = NULL
)
```

## Arguments

- margot_result:

  Output from margot_causal_forest()

- model_names:

  Character vector of model names to process. If NULL (default),
  processes all models.

- seed:

  Integer. Random seed for reproducibility (default 12345)

- n_bootstrap:

  Integer. Number of bootstrap replicates for confidence intervals
  (default 200)

- verbose:

  Logical. Print progress messages (default TRUE)

## Value

The input margot_result object with added qini_results component
containing:

- cate:

  MAQ object for CATE-based targeting

- baseline:

  MAQ object for no-priority baseline

- test_indices:

  Indices used for QINI computation

- n_test:

  Number of observations used

## Details

This function implements the standard QINI curve methodology from the
GRF and MAQ packages. It computes two curves:

1\. CATE curve: Prioritizes treatment based on estimated individual
treatment effects 2. Baseline curve: No-priority assignment (random
allocation)

When margot_causal_forest() was run with use_train_test_split = TRUE,
QINI curves are computed on the test set only, following best practices
for avoiding overfitting in policy evaluation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run causal forest with train/test split
cf_results <- margot_causal_forest(
  data = mydata,
  outcome_vars = c("outcome1", "outcome2"),
  covariates = covariates,
  W = treatment,
  use_train_test_split = TRUE
)

# Compute QINI curves
cf_results <- margot_qini(cf_results)

# Plot results
margot_plot_qini(cf_results, model_name = "model_outcome1")
} # }
```
