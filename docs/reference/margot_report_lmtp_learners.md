# Assemble LMTP learner diagnostics for reporting

Bundles the learner heatmap, concise narrative, and a compact summary
table of dominant learners for a selected outcome and set of shifts.
Designed to keep the visual and textual learner diagnostics aligned in
Quarto and manuscript workflows.

## Usage

``` r
margot_report_lmtp_learners(
  x,
  outcome = NULL,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  title = NULL,
  component = c("both", "outcome", "treatment"),
  digits = 1,
  include_plot = TRUE,
  plot_args = list(),
  interpret_args = list()
)
```

## Arguments

- x:

  Result of \`margot_lmtp()\` (with \`\$models\`) or another object
  accepted by the learner helpers.

- outcome:

  Optional character outcome name. When \`NULL\`, the first stored
  outcome is used.

- shifts:

  Optional character vector of shifts to include (full or cleaned
  names). If \`NULL\`, all available shifts are used.

- label_mapping:

  Optional label map passed through to downstream helpers.

- waves:

  Optional integer vector selecting waves.

- remove_waves:

  Optional integer vector of waves to drop after subsetting.

- title:

  Optional plot title passed to \[margot_plot_lmtp_learners()\].

- component:

  Which nuisance models to include: \`"both"\` (default), \`"outcome"\`
  (only \`m\`), or \`"treatment"\` (only \`r\`).

- digits:

  Integer rounding applied to percentage columns in the summary table
  and to the narrative produced by \[margot_interpret_lmtp_learners()\].

- include_plot:

  Logical; if \`TRUE\`, returns a \`ggplot2\` object from
  \[margot_plot_lmtp_learners()\].

- plot_args:

  Optional named list overriding defaults passed to
  \[margot_plot_lmtp_learners()\].

- interpret_args:

  Optional named list overriding defaults passed to
  \[margot_interpret_lmtp_learners()\] (for example, \`digits = 1\`).

## Value

A named list with elements: - \`summary_table\`: compact
dominant-learner table by shift, component, and wave. -
\`learner_data\`: tidy learner-weight data from
\`summarise_lmtp_learners()\`. - \`plot\`: learner heatmap (or \`NULL\`
when \`include_plot = FALSE\`). - \`narrative\`: structured list from
\`margot_interpret_lmtp_learners(return = "list")\`. -
\`method_statement\`: single character string describing the learner
diagnostics. - \`metadata\`: list of context (outcome, shifts, waves,
component).
