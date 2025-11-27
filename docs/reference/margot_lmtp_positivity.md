# Summarize positivity via density ratios for LMTP fits

Computes by-wave and overall summaries of density ratios, including
zeros, extreme quantiles, tail mass above thresholds, and effective
sample size (ESS).

## Usage

``` r
margot_lmtp_positivity(
  x,
  thresholds = c(10, 25, 50, 100),
  probs = c(0.001, 0.01, 0.05, 0.5, 0.95, 0.999),
  ess_warn = 0.5,
  zero_warn = 0.01,
  tail_warn = c(`10` = 0.05, `25` = 0.02, `50` = 0.01, `100` = 0.005),
  include_overall = TRUE,
  digits = NULL,
  verbose = TRUE
)
```

## Arguments

- x:

  Either: - the full result of \`margot_lmtp()\` (list with \$models), -
  a single LMTP model fit (must have \$density_ratios), - or a numeric
  vector/matrix of density ratios.

- thresholds:

  Numeric vector of tail thresholds to report (P(ratio \> threshold)).

- probs:

  Quantiles to report (must include 0.5 if you want the median).

- ess_warn:

  Flag when ESS/N is below this fraction (per wave).

- zero_warn:

  Flag when proportion of exact zeros exceeds this fraction.

- tail_warn:

  Named numeric vector giving tail-mass flag thresholds (names must
  match thresholds). If length 1, recycled to all thresholds.

- include_overall:

  If TRUE, also compute an overall summary pooling all waves.

- digits:

  Optional integer for rounding in the returned data frames (NULL = no
  rounding).

- verbose:

  If TRUE, prints concise CLI messages when flags are raised.

## Value

A list with:

- by_wave:

  data.frame of per-wave summaries (one row per outcome/shift/wave).
  Columns include both all-observations metrics and uncensored-only
  (\`\*\_pos\`) metrics.

- overall:

  data.frame of pooled summaries across waves (one row per
  outcome/shift). Also includes \`\*\_pos\` columns for uncensored
  diagnostics.

- flags:

  data.frame of flagged issues (subset of rows from by_wave/overall with
  reasons).

## Details

\*\*Censoring vs. Treatment Positivity:\*\* In longitudinal LMTP, zeros
(\\r_t = 0\\) in density ratios primarily reflect \*\*censoring\*\*
(dropout) rather than treatment positivity violations. When an
individual is censored at time \\t\\, they have no observed treatment at
subsequent waves, yielding \\r_t = 0\\ in the numerator regardless of
the policy. These censoring-induced zeros appear identically across all
policies for the same individual.

In contrast, true \*\*treatment positivity violations\*\* are
policy-specific: an observed treatment trajectory may be incompatible
with one policy but not another. To distinguish these cases, this
function computes metrics for both all observations (including zeros)
and \*\*uncensored observations\*\* (\\r \> 0\\), with the latter
denoted by \`\*\_pos\` suffixes in column names. The \`prop_zero\`
column reports the censoring rate per wave.

## Examples

``` r
# Single model:
# pos <- margot_lmtp_positivity(fit$models$outcome$shift_model)
#
# Entire run from margot_lmtp():
# pos <- margot_lmtp_positivity(fit)
# head(pos$by_wave); head(pos$flags)
```
