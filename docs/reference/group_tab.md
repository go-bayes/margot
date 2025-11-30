# Group and Annotate Treatment Effect Estimates

This function arranges and annotates a data frame based on specified
types of treatment effect estimates (RR or RD). It supports a variety of
sorting options including alphabetical, magnitude (ascending or
descending), E-value bound (ascending or descending), custom order, and
a default alias for backward compatibility. It also handles original
scale estimates when available.

## Usage

``` r
group_tab(
  df,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  custom_order = NULL
)
```

## Arguments

- df:

  Data frame containing the variables of interest, or a list containing
  the results data frame and label mapping from
  transform_to_original_scale().

- type:

  Type of treatment effect to analyze. One of 'RR' (Risk Ratio) or 'RD'
  (Risk Difference). Defaults to 'RD'.

- order:

  Sorting option for outcomes. Options are:

  - 'alphabetical': sort by outcome name (A-Z)

  - 'magnitude_desc': sort by absolute effect size, descending (default
    for 'magnitude')

  - 'magnitude_asc': sort by absolute effect size, ascending

  - 'evaluebound_desc': sort by E-value bound, descending

  - 'evaluebound_asc': sort by E-value bound, ascending

  - 'custom': user-defined order (requires custom_order)

  - 'default': alias for 'magnitude_desc' (deprecated)

  Default is 'default'.

- custom_order:

  Character vector specifying a custom outcome ordering, used when order
  = 'custom'. Must contain all outcomes exactly once.

## Value

A data frame arranged according to \`order\`, annotated with:

- Estimate category (positive, negative, not reliable)

- Formatted label for the effect and confidence interval

- Optional original-scale label if \_original columns are present

## Details

The function detects whether \`df\` is a list output from
transform_to_original_scale() and extracts \`results_df\` and
\`label_mapping\` accordingly. It then ensures an \`outcome\` column,
applies any label mapping, and sorts based on the chosen \`order\`. New
options 'magnitude_desc' and 'magnitude_asc' sort by absolute effect
size; 'evaluebound_desc' and 'evaluebound_asc' sort by the E-Value
bound; 'alphabetical' sorts by outcome name; 'custom' respects a
user-provided vector; 'default' is an alias for 'magnitude_desc'.

## Examples

``` r
# descending magnitude (default for 'default')
result_df <- group_tab(df = analysis_df, order = "default")
#> Error in group_tab(df = analysis_df, order = "default"): could not find function "group_tab"

# ascending magnitude
result_df <- group_tab(df = analysis_df, order = "magnitude_asc")
#> Error in group_tab(df = analysis_df, order = "magnitude_asc"): could not find function "group_tab"

# strongest E-value bound first
result_df <- group_tab(df = analysis_df, order = "evaluebound_desc")
#> Error in group_tab(df = analysis_df, order = "evaluebound_desc"): could not find function "group_tab"

# alphabetical
result_df <- group_tab(df = analysis_df, order = "alphabetical")
#> Error in group_tab(df = analysis_df, order = "alphabetical"): could not find function "group_tab"

# custom ordering
custom_order <- c("Outcome3", "Outcome1", "Outcome2")
result_df <- group_tab(df = analysis_df, order = "custom", custom_order = custom_order)
#> Error in group_tab(df = analysis_df, order = "custom", custom_order = custom_order): could not find function "group_tab"
```
