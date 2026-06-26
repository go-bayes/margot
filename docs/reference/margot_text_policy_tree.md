# Generate standard policy-tree reporting text

Produces cautious stock text for policy-tree reports. The text describes
the reporting convention without making substantive claims about
moderators.

## Usage

``` r
margot_text_policy_tree(
  source = c("generic", "heldout_cv", "display_tree"),
  include_ci = TRUE,
  include_plot_convention = TRUE,
  collapse = TRUE
)
```

## Arguments

- source:

  Character. Reporting source to describe.

- include_ci:

  Logical. Include the interval-interpretation sentence.

- include_plot_convention:

  Logical. Include the two-panel plot sentence.

- collapse:

  Logical. If `TRUE`, return one character string; otherwise return a
  character vector of sentences.

## Value

A character string or character vector.

## Details

Policy-tree reporting separates the selected action learned by the
fitted tree from the signed treatment-control score contrast computed on
evaluation rows. Let \\\Gamma\_{ja}\\ denote the action score for
observation \\j\\ under action \\a\\, let \\L\\ denote a policy-tree
leaf, and let \\E_L\\ denote the evaluation observations routed to that
leaf. For binary actions \\C\\ and \\T\\, the reported contrast is \$\$
\Delta_L = \frac{\sum\_{j \in E_L} w_j\\\Gamma\_{jT} - \Gamma\_{jC}\\}
{\sum\_{j \in E_L} w_j}. \$\$ The stored selected action is learned on
training observations \\S_L\\: \$\$ \pi(L) = \arg\max\_{a \in \\C,T\\}
\frac{\sum\_{j \in S_L} w_j \Gamma\_{ja}}{\sum\_{j \in S_L} w_j}. \$\$
If every displayed leaf has the same selected action, the tree describes
variation in score-contrast magnitude rather than a selective rule that
changes actions across leaves.
