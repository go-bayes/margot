# Create a policy-tree leaf reporting table

Returns a tidy leaf table using the package reporting convention: the
selected action is shown separately from the signed treatment-control
contrast. Positive `T-C` values favour treatment and negative values
favour control.

## Usage

``` r
margot_table_policy_tree(
  object,
  model_name = NULL,
  depth = NULL,
  weights = NULL,
  digits = 3L,
  ci_level = 0.95,
  label_mapping = NULL,
  source = c("auto", "display_tree", "heldout_cv"),
  include_selected_action_difference = FALSE,
  include_value_contribution = FALSE,
  baseline = c("control_all", "treat_all")
)
```

## Arguments

- object:

  A
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)-style
  object, or a `margot_policy_tree_cv` object.

- model_name:

  Optional model name, with or without the `model_` prefix. Required for
  display-tree tables.

- depth:

  Integer tree depth. If `NULL` for a CV object, the selected depth map
  is used when available.

- weights:

  Optional evaluation weights for display-tree tables.

- digits:

  Integer; rounding used in formatted columns.

- ci_level:

  Confidence level for display-tree score intervals.

- label_mapping:

  Optional named list used for display labels.

- source:

  Character. `"auto"` chooses `"heldout_cv"` for `margot_policy_tree_cv`
  objects and `"display_tree"` otherwise.

- include_selected_action_difference:

  Logical. Include the selected action minus alternative action score
  contrast. Defaults to `FALSE`.

- include_value_contribution:

  Logical. Include baseline-dependent value contribution columns.
  Defaults to `FALSE`.

- baseline:

  Character. Baseline for optional value contribution columns.

## Value

A tibble with one row per reported leaf or held-out leaf-action summary.

## Details

Let \\\Gamma\_{ja}\\ denote the action score for observation \\j\\ under
action \\a\\, let \\L\\ denote a policy-tree leaf, and let \\E_L\\
denote the evaluation observations routed to that leaf. For binary
actions \\C\\ and \\T\\, the reported evaluation-sample contrast is \$\$
\Delta_L = \frac{\sum\_{j \in E_L} w_j\\\Gamma\_{jT} - \Gamma\_{jC}\\}
{\sum\_{j \in E_L} w_j}. \$\$ The selected action is reported
separately. For the fitted tree, it is the action stored in the terminal
node, learned on the training observations \\S_L\\ routed to that node:
\$\$ \pi(L) = \arg\max\_{a \in \\C,T\\} \frac{\sum\_{j \in S_L} w_j
\Gamma\_{ja}}{\sum\_{j \in S_L} w_j}. \$\$ Held-out CV tables therefore
report actions learned on training folds and signed contrasts computed
on held-out evaluation rows; they do not reselect actions from held-out
means. Between-leaf differences in \\\Delta_L\\ describe variation in
the magnitude of the score contrast; they are not the decision rule used
by the policy tree.
