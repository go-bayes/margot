# Experimental plot: advanced bias/adjustment features (DEV)

margot_plot_dev() is an experimental, feature‑rich plotting helper that
keeps all the recent bias analysis improvements (e.g., optional
multiplicity correction, flexible E‑value handling, label mapping, table
renaming) while leaving the stable margot_plot() API unchanged. Expect
breaking changes in a future release as we continue to refine RR/E‑value
workflows.

## Usage

``` r
margot_plot_dev(
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

  Data frame of effect estimates and 95 upstream helpers, e.g.,
  group_tab()).

- type:

  Character: "RD" (difference) or "RR" (ratio).

- order:

  Character ordering mode; see stable margot_plot() for options.

- custom_order:

  Optional character vector for custom ordering.

- title_binary:

  Optional title for binary outcomes.

- include_coefficients:

  Logical; if TRUE, prints numeric effect at left margin.

- standardize_label:

  One of "NZ", "US", "none"; controls x‑axis label text.

- e_val_bound_threshold:

  Numeric threshold for E‑value bound labelling.

- adjust:

  Multiplicity correction: "none" or "bonferroni".

- alpha:

  Numeric significance level used by multiplicity correction.

- ...:

  Additional named options merged into \`options\` (used by label
  mapping, theming, etc.).

- options:

  Named list of plotting/label options (e.g., colours, sizes,
  transforms).

- label_mapping:

  Optional named list for transform_label().

- save_output:

  Logical; if TRUE, saves the result list via here_save_qs().

- use_timestamp:

  Logical; append timestamp to saved filename.

- base_filename, prefix, save_path:

  Save controls passed to here_save_qs().

- original_df:

  Optional original data for back‑transform helpers.

- bold_rows:

  Logical; bold rows above the E‑value bound threshold.

- rename_cols:

  Logical; if TRUE, rename E‑value columns per \`col_renames\`.

- col_renames:

  Named list of column renames.

- rename_ate:

  Logical or character; if TRUE/character, rename the effect column.

- rename_evalue:

  Logical; if TRUE, rename E‑value column headers to display form.

## Value

A list with \`plot\`, \`interpretation\`, and \`transformed_table\`.
