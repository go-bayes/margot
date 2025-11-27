# Batch process heterogeneity analyses across multiple outcome domains

runs the same planned subgroup contrasts (e.g., wealth, ethnicity,
political orientation) over multiple outcome domains, with optional
correction for multiple comparisons.

## Usage

``` r
margot_planned_subgroups_batch(
  domain_models,
  X,
  base_defaults,
  label_mapping = NULL,
  subset_types,
  original_df,
  domain_names,
  subtitles,
  adjust = c("none", "bonferroni", "holm"),
  alpha = 0.05,
  ...
)
```

## Arguments

- domain_models:

  list of model sets; one element per outcome domain

- X:

  model matrix (or data frame) of predictors used when the models were
  fitted

- base_defaults:

  named list of default arguments passed to the downstream plotting /
  helper functions

- label_mapping:

  named list of variable-to-label mappings; passed down to
  \`margot_plot()\`

- subset_types:

  named list of subset specifications (e.g., list(wealth =
  subsets_standard_wealth))

- original_df:

  the raw data frame containing all variables (needed for label
  recovery, plotting on the original scale, etc.)

- domain_names:

  character vector naming each element in \`domain_models\`

- subtitles:

  character vector of subtitles used in plot annotations; must be the
  same length as \`domain_names\`

- adjust:

  character; correction method for multiple comparisons in plots. one of
  "none", "bonferroni", or "holm". default: "none".

- alpha:

  numeric; significance threshold for correction. default: 0.05.

- ...:

  any additional arguments forwarded directly to
  \[\`margot_subset_batch()\`\]

## Value

a nested list. the first level is the domain name; the second level is
the subset type. each leaf contains the full list returned by
\`margot_subset_batch()\`.
