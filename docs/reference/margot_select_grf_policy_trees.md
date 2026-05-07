# Select GRF policy trees for manuscript graphing

Applies a transparent reporting rule to the policy-brief table returned
by \`margot_policy_workflow()\`. The default rule graphs a policy tree
only when the selected policy value and the uplift among treated units
both have lower confidence limits above the specified thresholds.

## Usage

``` r
margot_select_grf_policy_trees(
  policy_brief,
  model_names = NULL,
  outcome_labels = NULL,
  policy_value_lower_threshold = 0,
  treated_uplift_lower_threshold = 0,
  require_policy_value = TRUE,
  require_treated_uplift = TRUE
)
```

## Arguments

- policy_brief:

  Data frame with \`Outcome\`, \`Policy Value (95 \`Uplift in Treated
  (95 \`policy_workflow\$policy_brief_df\`.

- model_names:

  Optional character vector of model names. If supplied with
  \`outcome_labels\`, the returned table includes model names aligned by
  outcome label.

- outcome_labels:

  Optional character vector of outcome labels aligned with
  \`model_names\`.

- policy_value_lower_threshold:

  Numeric threshold for the policy-value lower confidence limit. Default
  is 0.

- treated_uplift_lower_threshold:

  Numeric threshold for the treated-uplift lower confidence limit.
  Default is 0.

- require_policy_value:

  Logical. Require policy-value lower confidence limit to exceed
  \`policy_value_lower_threshold\`. Default TRUE.

- require_treated_uplift:

  Logical. Require treated-uplift lower confidence limit to exceed
  \`treated_uplift_lower_threshold\`. Default TRUE.

## Value

A data frame containing parsed estimates, lower confidence limits, and
\`graph_policy_tree\`.
