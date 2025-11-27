# Assemble RATE tables (AUTOC and QINI)

Convenience wrapper around margot_rate_batch(). Returns two data frames,
both sorted by descending RATE Estimate, with reliable results
highlighted in bold.

## Usage

``` r
margot_rate(
  models,
  model_names = NULL,
  policy = c("treat_best", "withhold_best"),
  round_digits = 3,
  highlight_significant = TRUE,
  label_mapping = NULL,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  adjust = "none",
  alpha = 0.05,
  apply_adjustment = adjust != "none",
  seed = 12345,
  use_evaluation_subset = TRUE,
  target = c("AUTOC", "QINI"),
  q = seq(0.1, 1, by = 0.1),
  ...
)
```

## Arguments

- models:

  List from margot_causal_forest().

- model_names:

  Optional character vector specifying which models to process. Default
  NULL (all models).

- policy:

  Character; "treat_best" (default) or "withhold_best".

- round_digits:

  Integer; decimal places (default 3).

- highlight_significant:

  Logical; bold outcomes whose 95 percent CI excludes 0 (default TRUE).
  Note: Only positive significant effects are bolded, negative effects
  are not bolded.

- label_mapping:

  Named character vector for converting variable names to readable
  labels.

- remove_tx_prefix:

  Logical; remove treatment prefix from variable names (default TRUE).

- remove_z_suffix:

  Logical; remove z-score suffix from variable names (default TRUE).

- use_title_case:

  Logical; convert variable names to title case (default TRUE).

- remove_underscores:

  Logical; replace underscores with spaces (default TRUE).

- adjust:

  Character; method for adjusting p-values. Only "bonferroni" or "none"
  are recommended. While other methods are technically available through
  stats::p.adjust(), they may not be appropriate for all contexts (e.g.,
  cross-validation). Default is "none". When using Bonferroni, consider
  alpha = 0.2 due to its conservative nature.

- alpha:

  Numeric; significance threshold (default 0.05). When using Bonferroni
  correction with noisy heterogeneous treatment effect models, alpha =
  0.2 may be more appropriate to maintain reasonable statistical power.

- apply_adjustment:

  Logical; if TRUE, compute p-values and apply adjustment method. If
  FALSE, just document the adjustment method without recomputing.
  Default is TRUE when adjust parameter is provided, FALSE otherwise.

- seed:

  Integer; base seed for reproducible RATE computations (default 12345).

- use_evaluation_subset:

  Logical; if TRUE, use test indices from qini_metadata when available
  for proper out-of-sample evaluation (default TRUE).

- target:

  Character vector; weighting schemes to compute: "AUTOC", "QINI", or
  c("AUTOC", "QINI") for both (default). When a single target is
  specified, only that table is returned.

- q:

  Numeric vector of quantiles at which to evaluate. Default is seq(0.1,
  1, by = 0.1) which matches the GRF default.

- ...:

  Additional arguments passed to grf::rank_average_treatment_effect().

## Value

When target includes both "AUTOC" and "QINI", returns a list with
elements: \* rate_autoc: AUTOC RATE table \* rate_qini: QINI RATE table
When a single target is specified, returns just that table as a data
frame.
