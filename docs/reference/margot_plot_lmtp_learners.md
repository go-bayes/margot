# Plot Super Learner weights for LMTP nuisance fits

Produces a heatmap of average Super Learner weights by wave, learner,
shift, and nuisance component (outcome regression \`m\` and
density-ratio regression \`r\`). Colours encode the mean Super Learner
weight averaged across cross-fitting folds.

## Usage

``` r
margot_plot_lmtp_learners(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  title = NULL,
  component = c("both", "outcome", "treatment")
)
```

## Arguments

- x:

  LMTP run output (e.g., the result of \[margot_lmtp()\]) or any object
  that exposes \`\$density_ratios\` in the same structure as the plot
  helpers.

- outcome:

  Character scalar giving the outcome name to summarise.

- shifts:

  Optional character vector of shifts to include (either full names such
  as \`t5_pwi_z_shift_up\` or cleaned suffixes such as \`shift_up\`). If
  \`NULL\`, all available shifts for the outcome are used.

- label_mapping:

  Optional named list passed to \[transform_label()\] for readable
  outcome/shift labels.

- waves:

  Optional integer vector of waves to keep (matching the wave index used
  by the LMTP fits).

- remove_waves:

  Optional integer vector of waves to drop after subsetting.

- title:

  Optional plot title. Defaults to the pretty outcome label of the
  requested contrast when \`NULL\`.

- component:

  Which nuisance models to include: \`"both"\` (default), \`"outcome"\`
  (only \`m\`), or \`"treatment"\` (only \`r\`).

## Value

A \`ggplot2\` object.
