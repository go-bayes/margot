# Apply Proper Multiplicity Correction Using multcomp

Apply Proper Multiplicity Correction Using multcomp

## Usage

``` r
margot_correct_multcomp(
  model_fit,
  contrast_matrix = NULL,
  raw_table_df,
  adjust = "holm",
  alpha = 0.05,
  scale = "RD"
)
```

## Arguments

- model_fit:

  fitted model object (lm, glm, etc.)

- contrast_matrix:

  contrast matrix for glht (if NULL, uses identity matrix)

- raw_table_df:

  original results data frame

- adjust:

  correction method ("holm", "bonferroni")

- alpha:

  significance level

- scale:

  type of effect ("RD" or "RR")

## Value

corrected data frame with simultaneous confidence intervals
