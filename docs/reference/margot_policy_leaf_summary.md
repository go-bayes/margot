# Summarise policy-tree leaves with treatment-control contrasts and sample shares

Computes leaf-level summaries for a stored policy tree. Leaf contrasts
are estimated from doubly robust action scores and use a fixed signed
comparison: treatment minus control. Positive values favour treatment
and negative values favour control. The legacy action-conditional
advantage columns are retained as compatibility aliases.

## Usage

``` r
margot_policy_leaf_summary(
  object,
  model_name,
  depth = 1L,
  weights = NULL,
  digits = 3L,
  ci_level = 0.95,
  label_mapping = NULL
)
```

## Arguments

- object:

  A
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)-style
  object containing `results`, `covariates`, and optionally `weights`.

- model_name:

  Outcome/model name, with or without the `model_` prefix.

- depth:

  Integer policy-tree depth, usually `1` or `2`.

- weights:

  Optional evaluation weights. Defaults to `object$weights`.

- digits:

  Integer; rounding used in formatted labels.

- ci_level:

  Numeric confidence level for approximate row-level score intervals.
  Defaults to `0.95`.

- label_mapping:

  Optional named list used to label actions.

## Value

A tibble with one row per leaf and columns for node id, action,
unweighted count, weighted sample share, signed treatment-control
contrast, approximate interval columns, legacy action-conditional
advantage aliases, selected-action metadata, and policy-value
contributions.

## Details

Let \\\Gamma\_{ja}\\ denote the action score for observation \\j\\ under
action \\a\\, let \\L\\ denote a policy-tree leaf, and let \\E_L\\
denote the evaluation observations routed to that leaf. For binary
actions \\C\\ and \\T\\, this helper reports the signed
evaluation-sample contrast \$\$ \Delta_L = \frac{\sum\_{j \in E_L}
w_j\\\Gamma\_{jT} - \Gamma\_{jC}\\} {\sum\_{j \in E_L} w_j}. \$\$ The
selected action is reported separately. For the fitted tree, the
selected action is the action stored in the terminal node, learned on
the training observations \\S_L\\ routed to that node: \$\$ \pi(L) =
\arg\max\_{a \in \\C,T\\} \frac{\sum\_{j \in S_L} w_j
\Gamma\_{ja}}{\sum\_{j \in S_L} w_j}. \$\$ The reported \\\Delta_L\\ is
computed on the evaluation rows and does not reselect the action from
evaluation-row means. Between-leaf differences in \\\Delta_L\\ describe
variation in magnitude. They are not the decision rule used by the
policy tree.
