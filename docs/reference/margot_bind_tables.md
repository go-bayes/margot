# Bind and format domain-specific tables or a single table

This function binds a named list of domain-specific tables, or handles a
single data frame. It optionally sorts by the E-value bound column,
renames columns, highlights rows above a threshold, and groups rows by
domain when provided. Works for latex, html, or markdown output. It
accepts both \`transformed_table\` layouts of \[margot_plot()\]: the
default reporting layout, whose bound column is "E-value bound" and
whose outcomes are in an "Outcome" column, and the legacy layout, whose
bound column is "E_Val_bound" and whose outcomes are row names. A table
that already has an "Outcome" column keeps it, and row names are not
printed.

## Usage

``` r
margot_bind_tables(
  tables_list,
  sort_E_val_bound = c("none", "asc", "desc"),
  e_val_bound_threshold = 1.1,
  highlight_color = "yellow",
  bold = TRUE,
  output_format = "markdown",
  rename_cols = TRUE,
  col_renames = list(`E-Value` = "E_Value", `E-Value bound` = "E_Val_bound"),
  rename_ate = FALSE,
  threshold_col = "E_Val_bound",
  kbl_args = list(booktabs = TRUE, caption = "Combined Results", align = NULL)
)
```

## Arguments

- tables_list:

  a named list of data frames, each a domain table, or a single data
  frame. For lists, row names are captured as an "Outcome" column; for
  data frames, same applies.

- sort_E_val_bound:

  character: one of "none", "asc", or "desc". default "none". For a list
  of domain tables, rows are sorted within each domain and the domains
  keep their order. Every table in a list must have the same columns
  once its outcome and E-value bound columns take the first table's
  spelling and any difference in interval level is resolved; otherwise
  binding would split values across columns, and the function stops with
  an error naming the differing columns.

  Tables that differ only in their interval level, for example domains
  each Bonferroni-corrected over their own outcomes ("99.58% CI lower"
  in one, "99.17% CI lower" in another), bind with level-free headers
  ("CI lower", "CI upper", "CI relative to 0" and the original-unit
  equivalents) and a per-row "CI level" column (for example "99.58%")
  after the last interval limit. Legacy tables with different quantile
  headers (for example "0.21 %" and "0.42 %") take "CI lower" and "CI
  upper", and their \`confidence_level\` column keeps each row's level.
  Tables that share one level keep their headers unchanged.

  Tables from the report and legacy layouts of margot_plot(), or with
  different estimate columns, renames or E-value thresholds, cannot be
  bound together.

- e_val_bound_threshold:

  numeric; threshold for highlighting rows. default 1.1.

- highlight_color:

  character or NULL; background colour for highlighted rows. default
  "yellow".

- bold:

  logical; whether to bold highlighted rows. default TRUE.

- output_format:

  character; one of "latex", "html", or "markdown". default "markdown".

- rename_cols:

  logical; whether to apply column renaming. default TRUE.

- col_renames:

  named list; mapping new_name = old_name for renaming. default
  list("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound").

- rename_ate:

  logical; if TRUE, renames the effect column to "ATE". default FALSE.

- threshold_col:

  character; name of the (pre-rename) column to test and sort by.
  default "E_Val_bound". When that column is absent, the first of
  "E_Val_bound", "E-value bound", "E-Value bound" and "E-Value Bound"
  that is present is used.

- kbl_args:

  list; extra arguments for kableExtra::kbl(). default
  list(booktabs=TRUE, caption="Combined Results", align=NULL).
