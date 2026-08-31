# Combine Model Summary and E-Value Calculation for Various Causal Models

This function creates a summary table from the output of various causal
models, including \`lmtp::lmtp_contrast()\`, \`grf::causal_forest()\`,
and \`grf::multi_arm_causal_forest()\`. It calculates E-values for the
estimates to assess the potential impact of unmeasured confounding,
appending these values to the summary table.

## Usage

``` r
margot_model_evalue(
  model_output,
  scale = c("RD", "RR"),
  new_name = "character_string",
  delta = 1,
  sd = 1,
  subset = NULL
)
```

## Arguments

- model_output:

  The output from a supported causal model. Supported types include:

  - Output from \`lmtp::lmtp_contrast()\`

  - Output from \`grf::causal_forest()\`

  - Output from \`grf::multi_arm_causal_forest()\`

  - A data frame with columns 'estimate' and 'std.err'

- scale:

  Character string specifying the E-value calculation. The legacy
  \`"RD"\` option applies the standardised-continuous-outcome
  approximation to an outcome-mean difference; \`"RR"\` treats the
  estimate as a risk ratio. Default is \`"RD"\`. Causal forest models
  use the \`"RD"\` calculation.

- new_name:

  Character string to name the row(s) in the output summary table,
  representing the treatment contrast(s). For multi-arm causal forests,
  this will be combined with the contrast information.

- delta:

  The exposure contrast represented by the outcome-mean difference, used
  only when \`scale = "RD"\`. Default is 1.

- sd:

  The outcome standard deviation used to standardise the outcome-mean
  difference, used only when \`scale = "RD"\`. Default is 1.

- subset:

  An optional logical vector for subsetting the data when the model is a
  \`grf\` model. Default is \`NULL\`.

## Value

A data frame with the original estimates and their E-values. The table
includes columns for the estimate (either RD or RR), its confidence
interval, E-Value, and the E-Value lower bound. For multi-arm causal
forests, multiple rows will be returned, one for each contrast. Numeric
columns retain their computational precision; round only when formatting
the table for presentation.

## Examples

``` r
if (FALSE) { # \dontrun{
# For lmtp_contrast output
summary_evalues <- margot_model_evalue(
  model_output = lmtp_contrast_output,
  scale = "RD",
  new_name = "Treatment Effect"
)

# For causal_forest output
cf_summary <- margot_model_evalue(
  model_output = causal_forest_output,
  new_name = "Causal Forest Effect"
)

# For multi_arm_causal_forest output
macf_summary <- margot_model_evalue(
  model_output = multi_arm_cf_output,
  new_name = "Multi-Arm Effect"
)

# For direct input of estimate and standard error
direct_summary <- margot_model_evalue(
  model_output = data.frame(estimate = 0.5, std.err = 0.1),
  new_name = "Direct Effect"
)
} # }
```
