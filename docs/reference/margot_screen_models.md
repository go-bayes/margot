# Screen margot models by p-value tests with robust handling of missing components

Screen margot models by p-value tests with robust handling of missing
components

## Usage

``` r
margot_screen_models(
  model_results,
  rule = c("ate_or_rate", "ate", "rate"),
  target = c("AUTOC", "QINI", "either", "both"),
  alpha = 0.05,
  adjust = c("none", "bonferroni", "holm", "BH", "fdr", "BY"),
  use_boot = FALSE
)
```

## Arguments

- model_results:

  list. output of \`margot_causal_forest()\`, containing \$results

- rule:

  character; choose among "ate_or_rate", "ate", "rate"

- target:

  character; one of "AUTOC", "QINI", "either", "both"

- alpha:

  numeric; significance threshold after adjustment

- adjust:

  character; p.adjust method: "none", "bonferroni", "holm", "BH", "fdr",
  "BY"

- use_boot:

  logical; if TRUE, use bootstrap tests when available. Default = FALSE

## Value

tibble with outcomes, raw and adjusted p-values, and a 'keep' flag
