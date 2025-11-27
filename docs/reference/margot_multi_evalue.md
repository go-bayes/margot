# Multi-bias E-value table (v1: unmeasured confounding)

Create a table-first sensitivity summary that reports baseline E-values
(after optional Bonferroni CI adjustment) and, when specified, computes
multi-bias E-values using the EValue package (confounding, selection,
misclassification). For RD/continuous outcomes, estimates are mapped to
the RR scale via the OLS-to-RR approximation (delta/sd) before
multi-bias evaluation.

## Usage

``` r
margot_multi_evalue(
  results,
  scale = c("RD", "RR"),
  intervention_type = c("exposure_shift", "ipsi"),
  delta_exposure = 1,
  sd_outcome = 1,
  biases = NULL,
  apply_bonferroni_first = TRUE,
  alpha = 0.05,
  m = NULL,
  notes = TRUE,
  include_baseline = TRUE,
  rename_multi = c("none", "friendly"),
  bound_params = NULL
)
```

## Arguments

- results:

  A data frame containing at least one of \`E\[Y(1)\]-E\[Y(0)\]\` (RD)
  or \`E\[Y(1)\]/E\[Y(0)\]\` (RR), plus columns \`2.5 combined table row
  or small set of rows from a margot workflow.

- scale:

  Character. "RD" (difference) or "RR" (ratio). If omitted, the function
  attempts to infer from the presence of \`E\[Y(1)\]-E\[Y(0)\]\` vs
  \`E\[Y(1)\]/E\[Y(0)\]\`.

- intervention_type:

  Character. One of "exposure_shift" or "ipsi". Used only for
  interpretation notes.

- delta_exposure:

  Numeric. Exposure contrast size for OLS-type E-values (RD). Defaults
  to 1. For IPSI, interpret as one policy contrast (α1 vs α0).

- sd_outcome:

  Numeric. Outcome standard deviation used for OLS-type E-values (RD).
  Defaults to 1 (standardized outcomes).

- biases:

  Either an EValue bias object created with \`EValue::multi_bias()\`
  (preferred), a single EValue \`bias\` object (e.g.,
  \`EValue::confounding()\`), or a character vector of bias names to
  build with sensible defaults: accepted tokens are \`"confounding"\`,
  \`"selection"\`, \`"misclassification_outcome"\`, and
  \`"misclassification_exposure"\`.

- apply_bonferroni_first:

  Logical; if TRUE (default), widen CIs using a Bonferroni correction at
  FWER \`alpha\` before computing E-values.

- alpha:

  Numeric FWER level for Bonferroni. Default 0.05.

- m:

  Optional integer for multiplicity (number of tests). If NULL, inferred
  from \`nrow(results)\`.

- notes:

  Logical; if TRUE include a LaTeX-ready interpretation note in the
  output list.

## Value

A list with elements: - \`table\`: data frame with adjusted CIs (if
requested), baseline E-values (\`E_Value\`, \`E_Val_bound\`), and audit
columns \`alpha_fwer\`, \`m\`, \`scale\`, \`intervention_type\`,
\`delta_exposure\`, \`sd_outcome\`, and \`bias_order\`. For convenience,
mirrored columns \`E_value_point\` and \`E_value_bound\` are also
included. - \`notes\`: character vector (length 1) with an
interpretation message when \`notes = TRUE\`.
