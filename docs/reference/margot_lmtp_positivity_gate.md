# Registered trim-ladder positivity gate for LMTP fits

Applies a pre-registered positivity decision rule to one or more LMTP
fits across a descending ladder of trim rungs (default 0.99, 0.98,
0.96), returning a mechanical pass/fail per policy and rung plus the
selected rung. The gate exists so that trim selection can be cited in a
registration and reproduced from code rather than exercised as analyst
judgement.

## Usage

``` r
margot_lmtp_positivity_gate(
  fit,
  outcome = NULL,
  shifts = NULL,
  rungs = c(0.99, 0.98, 0.96),
  ess_floor = 0.5,
  trim_mass_budget = 0.05,
  test_thresholds = NULL,
  label_mapping = NULL,
  verbose = TRUE
)
```

## Arguments

- fit:

  A \`margot_lmtp()\` result, a single LMTP model exposing
  \`\$density_ratios\`, or a list of such models (the same inputs as
  \[margot_lmtp_weight_diag_from_fit()\]).

- outcome:

  Optional outcome name when \`fit\` is a full run.

- shifts:

  Optional character vector of shifts to gate; defaults to all shifts
  stored for the outcome.

- rungs:

  Numeric vector of trim rungs, evaluated in order; the first passing
  rung is selected (default \`c(0.99, 0.98, 0.96)\`).

- ess_floor:

  Minimum acceptable ESS fraction of uncensored observations at every
  wave (default 0.5, matching \[margot_lmtp_positivity()\]'s
  \`ess_warn\`).

- trim_mass_budget:

  Maximum acceptable share of total cumulative ratio mass removed by
  winsorising at the rung (default 0.05).

- test_thresholds:

  Optional list of product-band thresholds (defaults: \`prod_log10 =
  -1\`, \`prod_frac_ok = 0.05\`, \`prod_frac_warn = 0.20\`), matching
  \`margot_ipsi_summary()\`.

- label_mapping:

  Optional label mapping forwarded to
  \[margot_lmtp_weight_diag_from_fit()\].

- verbose:

  If TRUE, prints one verdict line per policy.

## Value

A list with:

- by_rung:

  data.frame with one row per outcome/shift/rung: \`min_ess_frac\`,
  \`trim_mass_share\`, \`prod_frac_outside\`, \`support_status\`, the
  three criterion flags, and \`pass\`.

- selection:

  data.frame with one row per outcome/shift: \`selected_rung\` (NA when
  no rung passes) and a \`verdict\` string.

- criteria:

  list recording the thresholds the gate applied, for citation in
  registration documents.

## Details

Three criteria must hold at a rung for a policy to pass:

1.  **Effective sample size (variance side).** The Kish ESS of the
    rung-winsorised cumulative weights (\`ess_cum_trim\` from
    \[margot_lmtp_weight_diag_from_fit()\]) must be at least
    \`ess_floor\` of the uncensored observations at every wave.
    Winsorising harder homogenises weights, so this criterion can be
    rescued by descending the ladder.

2.  **Trimmed mass (bias side).** Winsorising at the rung must remove no
    more than \`trim_mass_budget\` of total cumulative ratio mass at the
    final wave (\`trim_mass_share\`). Capping harder always removes more
    mass, so descending the ladder can only worsen this criterion: it is
    the pre-registered budget on how much distortion a study will buy
    for stability.

3.  **Product support band.** The fraction of uncensored cumulative
    ratio products outside the support band (default \\\[0.1, 10\]\\)
    must not reach "Limited" status under the shared support-status rule
    (\`Adequate\` at 5 percent outside or less, \`Caution\` to 20
    percent, \`Limited\` beyond). This criterion is computed on the raw
    ratios and does not vary with the rung.

The two rung-dependent criteria move in opposite directions, which gives
the ladder its logic: descend while the ESS criterion fails and the mass
budget still holds; once the budget is breached (or the rungs are
exhausted), the policy fails the gate and the registered contingency
applies.

## See also

\[margot_lmtp_weight_diag_from_fit()\], \[margot_lmtp_positivity()\]

## Examples

``` r
set.seed(2026)
tame <- matrix(stats::runif(300, 0.8, 1.25), ncol = 3)
fit <- list(density_ratios = tame)
gate <- margot_lmtp_positivity_gate(fit, verbose = FALSE)
gate$selection
#>     outcome   shift selected_rung support_status
#> 1 (outcome) (shift)          0.99       Adequate
#>                                    verdict
#> 1 estimate with lmtp_control(.trim = 0.99)
```
