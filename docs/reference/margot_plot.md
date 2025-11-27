# Create a Margot Plot with Proper Multiplicity Correction

Create a margot plot for visualising causal effects with proper
simultaneous confidence intervals using multcomp for family-wise error
rate control.

## Usage

``` r
margot_plot(
  .data,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  custom_order = NULL,
  title_binary = NULL,
  include_coefficients = TRUE,
  standardize_label = c("NZ", "US", "none"),
  e_val_bound_threshold = 1.2,
  adjust = c("none", "bonferroni"),
  alpha = 0.05,
  ...,
  options = list(),
  label_mapping = NULL,
  save_output = FALSE,
  use_timestamp = FALSE,
  base_filename = "margot_plot_output",
  prefix = NULL,
  save_path = here::here("push_mods"),
  original_df = NULL,
  bold_rows = FALSE,
  rename_cols = FALSE,
  col_renames = list(`E-Value` = "E_Value", `E-Value bound` = "E_Val_bound"),
  rename_ate = FALSE,
  rename_evalue = FALSE
)
```

## Arguments

- .data:

  data frame containing causal effect estimates with columns for effect
  sizes, confidence intervals, E-values and E-value bounds

- type:

  character. type of effect estimate: "RD" (risk difference) or "RR"
  (risk ratio)

- adjust:

  character. multiplicity correction method: "none", "bonferroni"

- alpha:

  numeric. significance level for corrections

- ...:

  other parameters as in original function
