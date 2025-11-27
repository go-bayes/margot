# Summarise Super Learner weights for LMTP nuisance models

Internal helper that collapses the cross-fit Super Learner weights
stored in an LMTP model (\`fits_m\` for the outcome regression and
\`fits_r\` for the density-ratio regression) to per-wave summaries.
Returns a tidy data frame used by downstream plotting and interpretation
utilities.

## Usage

``` r
summarise_lmtp_learners(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL
)
```
