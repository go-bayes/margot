# Best linear projection of conditional average treatment effects

Computes \`grf::best_linear_projection()\` for every causal forest
retained by \[margot_causal_forest()\], projecting each forest's
estimated conditional average treatment effects onto the \*\*full\*\*
covariate matrix the forest was fitted on. This is the registered
reporting surface: it replaces the internal \`blp_top\` field, which
projected onto a top-15 variable-importance screen and is retired from
reporting.

## Usage

``` r
margot_blp(
  models,
  covariates = NULL,
  target_sample = c("all", "overlap"),
  model_names = NULL,
  ...
)
```

## Arguments

- models:

  A list returned by \[margot_causal_forest()\] fitted with
  \`save_models = TRUE\`. The covariate matrix is taken from the same
  object when \`save_data = TRUE\`, otherwise from each forest's stored
  design matrix.

- covariates:

  Optional numeric matrix of covariates to project onto. Defaults to
  \`NULL\`, which uses the matrix the forests were fitted on. When
  supplied, its dimensions and column names must match that matrix.

- target_sample:

  Character; passed to \`grf::best_linear_projection()\` as
  \`target.sample\`. One of \`"all"\` (default) or \`"overlap"\`.

- model_names:

  Optional character vector of outcome names (with or without the
  \`model\_\` prefix) restricting which forests are projected. Defaults
  to \`NULL\`, meaning every retained forest.

- ...:

  Further arguments passed to \`grf::best_linear_projection()\`.

## Value

A data frame of class \`margot_blp\` with one row per outcome and
coefficient, carrying the columns \`outcome\`, \`term\`, \`estimate\`,
\`std_error\`, \`conf_low\`, \`conf_high\`, \`target_sample\`, \`n\`,
\`ess\`, \`matrix_fingerprint\`, and \`status\`. Confidence intervals
are 95 approximations from the coefficient table returned by \`grf\`.
\`ess\` is the Kish effective sample size of the forest's sample
weights, or \`NA\` when the forest carries no weights. \`status\` is
\`"ok"\` or \`"failed: \<message\>"\`.

## Details

Results are reported as estimates with 95 significance stars, p-values,
or multiplicity corrections are produced, by design. Studies fitted with
\`use_train_test_split = FALSE\` project on the same sample used for the
average treatment effect.

Every projection is isolated: an outcome whose projection fails
contributes a single structured failure row and never aborts the batch.

## See also

\[margot_table_blp()\], \[margot_plot_blp()\]

## Examples

``` r
if (FALSE) { # \dontrun{
cf <- margot_causal_forest(
  data = df, outcome_vars = outcomes, covariates = X, W = W,
  weights = w, save_models = TRUE, save_data = TRUE
)
blp <- margot_blp(cf)
margot_table_blp(blp)
margot_plot_blp(blp)
} # }
```
