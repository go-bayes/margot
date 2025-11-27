# Compare subgroups from a causal forest model.

Compare two sets of treatment-effect estimates and report their
differences.

## Usage

``` r
margot_compare_groups(
  group,
  subgroup,
  type = c("RD", "RR"),
  label_mapping = NULL,
  decimal_places = 3,
  group1_name = "Group 1",
  group2_name = "Group 2"
)
```

## Arguments

- group:

  data frame with treatment effects and confidence intervals for primary
  group

- subgroup:

  data frame with treatment effects and confidence intervals for
  contrast group

- type:

  Character; one of \`"RD"\` or \`"RR"\` (default: both allowed)

- label_mapping:

  Optional named vector for nicer row labels

- decimal_places:

  Number of digits for CI formatting (default: 3)

- group1_name:

  Character; name to use for primary group (default: "Group 1")

- group2_name:

  Character; name to use for contrast group (default: "Group 2")

## Value

A list with: - \`results\`: a data frame with columns \`Outcomes\` and
\`Group Differences\` - \`interpretation\`: a character string
summarising reliable differences
