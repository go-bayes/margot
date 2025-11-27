# Assess Covariate Overlap from Causal Forest Models

Wrapper around the original GRF overlap diagnostics. This function
exists to provide a stable, descriptive name. It delegates to
\`margot_assess_overlap()\` for the implementation.

## Usage

``` r
margot_grf_overlap(
  model_results,
  model_names = NULL,
  exposure_name = NULL,
  label_mapping = NULL,
  plot = TRUE,
  save_plots = FALSE,
  output_dir = NULL,
  theme = "classic",
  verbose = TRUE
)
```

## Arguments

- model_results:

  Output from margot_causal_forest() or margot_flip_forests()

- model_names:

  Character vector of model names to assess. If NULL, all models.

- exposure_name:

  Character string naming the exposure/treatment variable. If NULL,
  defaults to "Treatment".

- label_mapping:

  Named list mapping variable names to custom labels for the exposure.

- plot:

  Logical indicating whether to create overlap plots. Default is TRUE.

- save_plots:

  Logical indicating whether to save plots. Default is FALSE.

- output_dir:

  Directory to save plots if save_plots is TRUE.

- theme:

  Character string specifying the ggplot2 theme. Default is "classic".
  Options include "classic", "minimal", "bw", "gray", "light", "dark",
  "void".

- verbose:

  Logical for detailed messages. Default is TRUE.

## Value

A list with overlap summaries, plots, and text summary.
