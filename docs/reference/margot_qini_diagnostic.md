# Diagnose QINI Gain Discrepancies

Compare QINI gains from plotted data, direct maq calculations, and diff
summaries to help identify discrepancies between different calculation
methods.

## Usage

``` r
margot_qini_diagnostic(
  mc_result,
  model_names = NULL,
  spend_levels = 0.1,
  tolerance = 0.01,
  verbose = TRUE
)
```

## Arguments

- mc_result:

  Results from margot_causal_forest or similar with qini_data and
  qini_objects

- model_names:

  Character vector of models to check (NULL = all models)

- spend_levels:

  Numeric vector of spend levels to check (default: 0.1)

- tolerance:

  Numeric tolerance for flagging discrepancies (default: 0.01)

- verbose:

  Logical for detailed output (default: TRUE)

## Value

A tibble comparing gain values from different sources with columns: -
model: Model name - spend: Spend level - source: Source of the gain
value ("plot_data", "maq_direct", "diff_summary") - cate_gain: CATE gain
value - ate_gain: ATE/baseline gain value - difference: CATE - ATE
difference - se: Standard error (where available) - discrepancy: Logical
flag if values differ by more than tolerance

## Examples

``` r
if (FALSE) { # \dontrun{
# Run diagnostic on all models
diag_results <- margot_qini_diagnostic(mc_result)

# Check specific models at custom spend levels
diag_results <- margot_qini_diagnostic(
  mc_result,
  model_names = c("model_anxiety", "model_depression"),
  spend_levels = c(0.1, 0.2, 0.5)
)
} # }
```
