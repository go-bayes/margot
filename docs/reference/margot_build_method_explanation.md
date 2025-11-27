# Build Methods Text From Workflow Context

Constructs detailed, parameter-aware methods text suitable for
scientific reporting and preregistration, based on the actual objects
and settings used in \`margot_policy_workflow()\`. Returns long, short,
and prereg variants.

## Usage

``` r
margot_build_method_explanation(
  stability,
  best,
  summary,
  context,
  citations = TRUE
)
```

## Arguments

- stability:

  A \`margot_stability_policy_tree\` object.

- best:

  The result of \`margot_policy_summary_compare_depths()\`.

- summary:

  The result of \`margot_policy_summary_report()\`.

- context:

  List of key settings used by the workflow: \`se_method\`, \`R\`,
  \`audience\`, \`min_gain_for_depth_switch\`, \`prefer_stability\`,
  \`dominance_threshold\`, \`strict_branch\`, \`restricted_scope_1\`,
  \`restricted_scope_2\`, \`show_neutral\`, \`expand_acronyms\`.

- citations:

  Logical; include inline citations (default TRUE).

## Value

A named list with \`long\`, \`short\`, and \`prereg\` character strings.
