# Assemble policy-tree plots, table, and standard text

Convenience wrapper for the standard policy-tree reporting artefacts.
The returned components are ordinary objects that can be edited,
replaced, or omitted in manuscript workflows.

## Usage

``` r
margot_report_policy_tree(
  result_object,
  model_name,
  policy_cv = NULL,
  depth = NULL,
  original_df = NULL,
  weights = NULL,
  digits = 3L,
  ci_level = 0.95,
  label_mapping = NULL,
  include_plots = TRUE,
  include_table = TRUE,
  include_text = TRUE,
  include_policy_value = !is.null(policy_cv),
  layout = list(heights = c(1, 2)),
  annotation = list(tag_levels = "A"),
  projection_args = list(),
  decision_tree_args = list()
)
```

## Arguments

- result_object:

  A
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)-style
  object.

- model_name:

  Character scalar naming the model to report.

- policy_cv:

  Optional `margot_policy_tree_cv` object with held-out policy-tree
  diagnostics.

- depth:

  Optional integer tree depth. If `NULL` and `policy_cv` is supplied,
  the selected depth from `policy_cv$depth_map` is used when available;
  otherwise depth one is used.

- original_df:

  Optional data frame with original-scale variables.

- weights:

  Optional evaluation weights.

- digits:

  Integer; rounding used in formatted table columns.

- ci_level:

  Confidence level for leaf score intervals.

- label_mapping:

  Optional named list used for display labels.

- include_plots:

  Logical. Include plot components.

- include_table:

  Logical. Include the leaf table.

- include_text:

  Logical. Include standard interpretation text.

- include_policy_value:

  Logical. Include held-out value summaries when `policy_cv` is
  supplied.

- layout:

  List passed to
  [`margot_plot_policy_tree_panels()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_tree_panels.md).

- annotation:

  List passed to
  [`margot_plot_policy_tree_panels()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_tree_panels.md).

- projection_args:

  Optional list of arguments for the projection plot.

- decision_tree_args:

  Optional list of arguments for the decision tree.

## Value

A list with `table`, `text`, `plots`, and `metadata`.

## Details

This helper reports display-tree artefacts and can also attach held-out
policy-value summaries when supplied a `margot_policy_tree_cv` object.
Leaf tables use the signed evaluation-sample `T-C` contrast \$\$
\Delta_L = \frac{\sum\_{j \in E_L} w_j\\\Gamma\_{jT} - \Gamma\_{jC}\\}
{\sum\_{j \in E_L} w_j} \$\$ where \\\Gamma\_{ja}\\ is the action score
for observation \\j\\ under action \\a\\ and \\E_L\\ are evaluation
observations in leaf \\L\\. The selected action is reported separately
as the fitted tree's stored action, learned on training observations
\\S_L\\: \$\$ \pi(L) = \arg\max\_{a \in \\C,T\\} \frac{\sum\_{j \in S_L}
w_j \Gamma\_{ja}}{\sum\_{j \in S_L} w_j}. \$\$ Tree-level value
summaries compare the learned rule with all-control, all-treatment, and
best-constant baselines. Between-leaf differences in \\\Delta_L\\
describe variation in magnitude, not the policy-tree decision rule.
