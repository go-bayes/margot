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
  min_node_size = NULL
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

- min_node_size:

  Integer or `NULL`. Smallest permitted terminal node size.

## Value

Policy tree object

## Details

Margot pins `fastpolicytree`'s `strategy.datatype` to `1`, which uses
unsorted data sets that are sorted on demand. The upstream automatic
strategy (`2`) can select a different, lower-value rule than exact
`policytree` for wide covariate matrices. The pinned strategy is
therefore part of Margot's fast-engine contract rather than an
optimisation option selected from the input data.
