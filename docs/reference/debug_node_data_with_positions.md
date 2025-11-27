# Calculate Node Data and Positions for Policy Tree Visualisation

This internal function processes a policy tree object and calculates the
positions of nodes for visualisation purposes. It extracts relevant
information from the policy tree and computes x and y coordinates for
each node.

## Usage

``` r
debug_node_data_with_positions(policy_tree, node_distance_adjustment = -1.5)
```

## Arguments

- policy_tree:

  A policy tree object, typically obtained from a causal forest model.

- node_distance_adjustment:

  The adjustment for spacing between nodes. Default is -1.5.

## Value

A data frame containing information about each node in the policy tree,
including its ID, whether it's a leaf node, split variable, split value,
action, left and right child node IDs, and x and y coordinates for
visualization.
