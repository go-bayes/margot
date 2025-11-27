# Interpret Super Learner weights for LMTP nuisance fits

Generates concise prose describing which Super Learner components
dominate the outcome (\`m\`) and density-ratio (\`r\`) nuisance
regressions across waves and shifts. Highlights waves where a single
learner receives (approximately) all the weight, which can signal
limited information (e.g., after LOCF imputation).

## Usage

``` r
margot_interpret_lmtp_learners(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  component = c("both", "outcome", "treatment"),
  digits = 0,
  return = c("text", "list")
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

- component:

  Which nuisance models to include: \`"both"\` (default), \`"outcome"\`
  (only \`m\`), or \`"treatment"\` (only \`r\`).

- digits:

  Integer number of decimal places to use when reporting percentages.

- return:

  Either \`"text"\` (default) for a single character string or
  \`"list"\` for structured components.

## Value

Either a character string or a list (depending on \`return\`).
