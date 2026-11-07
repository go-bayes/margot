# Batch Process Subset Models for Causal Forests

We compared treatment effects across multiple subsets derived from
causal forest models (using the grf package). For each subset, the
number of participants, the total sample size, and the corresponding
percentage are computed, describing the composition of each targeted
subpopulation. In addition, for each subset the plot, interpretation,
and transformed table are generated to summarise the causal effect
estimates. Note that exploratory comparisons in causal forest analyses
should be interpreted with caution, as subsetting targeted
subpopulations may yield unstable treatment effect estimates, typically
indicated by large standard errors and wide confidence intervals.

## Usage

``` r
margot_subset_batch(
  model_results,
  X,
  base_defaults = list(),
  title = "",
  label_mapping = NULL,
  subsets,
  debug = FALSE,
  original_df = NULL,
  ...
)
```

## Arguments

- model_results:

  Results from the causal forest model.

- X:

  Covariate matrix.

- base_defaults:

  A list of default plotting options.

- title:

  A character string for the main title of the plots.

- label_mapping:

  A named vector mapping variable names to labels.

- subsets:

  A named list of subset definitions. Each element should be a list
  containing:

  var

  :   A character string specifying the variable to subset on.

  value

  :   The value used to define the subset.

  operator

  :   (Optional) A comparison operator (default is `"=="`).

  subset_condition

  :   (Optional) A pre-computed logical vector defining the subset.

  description

  :   (Optional) A character string describing the subset.

  label

  :   (Optional) A user-friendly label for the subset. If missing, the
      list name is used.

- debug:

  Logical; if `TRUE`, prints debugging information.

- original_df:

  Optional data frame containing the original (non-transformed) data for
  back-transformation.

- ...:

  Additional arguments to be passed to margot_plot().

## Value

A list with the following elements:

- results:

  A list of subset results, each containing the plot, interpretation,
  and transformed table along with sample statistics.

- summary:

  A data frame summarising the sample size, total sample size, and
  percentage for each subset.

- explanation:

  A character string providing a comprehensive explanation of the
  subsetting process and the results for each model.
