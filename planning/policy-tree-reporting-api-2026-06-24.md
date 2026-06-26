# Policy-tree reporting API

## Decision

`margot` should expose specialised policy-tree reporting functions by artefact:
one function for the branching decision-tree plot, one for the projection plot,
one for the combined panel figure, one for the leaf table, one for standard
interpretive text, and one light report assembler.

## Rationale

Policy-tree reporting combines model evaluation, graphing, tabulation, and
interpretation. A single broad function such as `margot_plot()` would make the
return type unclear and would couple unrelated maintenance concerns. Separate
functions keep tests small, allow manuscript workflows to use only the artefacts
they need, and make it easier to revise one convention without changing the
whole reporting stack.

## Plot convention

Policy-tree workflows use two complementary plots:

- the branching decision tree, which shows the assignment rule;
- the projection plot, which shows evaluation points relative to the selected
  split variables and predicted actions.

When both plots are shown, the decision tree should appear in panel A above the
projection plot in panel B. This ordering gives readers the rule before asking
them to inspect the empirical geometry of the rule.

## Leaf summaries

Leaf labels and tables should use a fixed signed treatment-control contrast:
`T-C`. Positive values favour treatment; negative values favour control. The
selected action remains the leaf title. This convention avoids changing the
comparison across leaves and remains coherent when treatment is advantageous in
nearly all leaves.

Confidence intervals may be reported for leaf score summaries, preferably in
tables rather than inside the tree graphic. These intervals quantify
uncertainty in row-level score summaries, not formal post-selection subgroup
tests.
