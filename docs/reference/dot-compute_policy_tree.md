# Internal wrapper for policy tree computation

Internal function that wraps either policytree::policy_tree() or
fastpolicytree::fastpolicytree() depending on user preference and
package availability.

## Usage

``` r
.compute_policy_tree(
  X,
  Gamma,
  depth,
  tree_method = "policytree",
  min_node_size = getOption("margot.policy_tree.min_node_size", 1L)
)
```

## Arguments

- X:

  Covariate matrix

- Gamma:

  Matrix of doubly robust scores

- depth:

  Integer depth of tree (1 or 2)

- tree_method:

  Character string: "policytree" or "fastpolicytree"

## Value

Policy tree object
