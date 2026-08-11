# Assemble the mandatory nonbinding LMTP evidence reports

\`margot_lmtp_evidence_report()\` combines the censoring,
target-population projection, and full-analysis-weight reports under
stable names. A structurally unavailable component requires an explicit
reason. The manifest records availability without assigning a scientific
or routing verdict.

## Usage

``` r
margot_lmtp_evidence_report(
  censoring_report = NULL,
  projection_report = NULL,
  analysis_weight_reports = NULL,
  missing_reasons = character()
)
```

## Arguments

- censoring_report:

  A \`margot_lmtp_censoring_report\` object or \`NULL\`.

- projection_report:

  A \`margot_target_projection_report\` object or \`NULL\`.

- analysis_weight_reports:

  One \`margot_lmtp_analysis_weight_report\` object, a named list of
  such objects, or \`NULL\`.

- missing_reasons:

  Named character vector giving the structural reason for each missing
  component. Permitted names are \`censoring_report\`,
  \`projection_report\`, and \`analysis_weight_reports\`.

## Value

An object of class \`margot_lmtp_evidence_report\` containing the three
report families and an availability manifest.
