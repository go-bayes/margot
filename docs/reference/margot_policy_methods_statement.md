# Policy Learning Methods Statement

Generates a boilerplate methods paragraph describing the policy learning
workflow used for policy trees in \`margot\` (DR scores, tree fitting,
honest evaluation, policy value contrasts with bootstrap CIs,
treated-only uplift, and optional stability analysis).

## Usage

``` r
margot_policy_methods_statement(
  object,
  depth = 2L,
  include_stability = TRUE,
  include_policy_value = TRUE,
  include_treated_only = FALSE,
  citations = TRUE,
  style = c("short", "long")
)
```

## Arguments

- object:

  A \`margot_stability_policy_tree\` (preferred) or a list with
  \`\$results\` created by \`margot_policy_tree()\`.

- depth:

  Integer; tree depth described in text (default 2).

- include_stability:

  Logical; add details on the stability analysis if available (default
  TRUE).

- include_policy_value:

  Logical; mention policy value contrasts and CIs (default TRUE).

- include_treated_only:

  Logical; mention treated-only uplift and coverage (default FALSE).

- citations:

  Logical; include inline citations (author-year) (default TRUE).

- style:

  Character; one of "short" (1 paragraph) or "long" (multi-line)
  (default "short").

## Value

A character scalar with the methods paragraph(s).
