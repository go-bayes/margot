# Boilerplate provider helpers (JSON/list/function)

Allows external, JSON-based or function-provided text to override or
augment internal explanations. Configure via options: -
\`options(margot.boilerplate = \<list\|function\|filepath\>)\` -
\`options(margot.boilerplate.labels = \<named list of default
labels\>)\` - \`options(margot.boilerplate.acronyms = \<named list of
acronym expansions\>)\`

## Usage

``` r
margot_load_boilerplate()
```

## Details

Slot examples: - policy_value_explainer: character or list(policy=...,
research=...) - methods_long / methods_short / methods_prereg: glue
templates
