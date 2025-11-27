# Create Summary Tables Using table1 with Custom Formatting

`margot_make_tables` is a wrapper for
[`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html) which
simplifies the creation of summary tables. It provides custom variable
labelling, formatting, factor conversion, and additional table options.
This function is optimized for `"markdown"`, `"latex"`, and
`"flextable"` outputs, with special support for Quarto documents.

## Usage

``` r
margot_make_tables(
  data,
  vars,
  by,
  labels = NULL,
  factor_vars = NULL,
  ordinal_vars = NULL,
  ordinal_levels = list(),
  binary_to_yesno = FALSE,
  binary_labels = c("No", "Yes"),
  auto_integer_ordinals = FALSE,
  integer_ordinal_range = 0:10,
  table1_opts = list(),
  format = c("markdown", "latex", "flextable"),
  kable_opts = list(),
  flex_opts = list(),
  quarto_label = NULL
)
```

## Arguments

- data:

  A `data.frame` containing the dataset.

- vars:

  A character vector of variable names to include on the left-hand side
  of the table.

- by:

  A character vector of variable names to stratify the table by.
  Supports multiple variables for interactions.

- labels:

  A named character vector for custom variable labels. Names should
  correspond to variable names in `vars`.

- factor_vars:

  An optional character vector of variable names in `vars` to convert to
  factors for frequency tables.

- ordinal_vars:

  An optional character vector of variable names in `vars` to convert to
  ordered factors for ordinal frequency displays.

- ordinal_levels:

  An optional named list that specifies custom level orderings/labels
  for ordinal variables. Each entry can be either a vector of desired
  levels, a named vector where names identify the raw values, or a list
  with `levels` and optional `labels` components.

- binary_to_yesno:

  Logical toggle; when `TRUE`, numeric 0/1 indicators in `vars` are
  converted to factors that show frequencies.

- binary_labels:

  Character vector of length two that labels binary indicators when
  `binary_to_yesno = TRUE`. Defaults to `c("No", "Yes")`.

- auto_integer_ordinals:

  Logical toggle; when `TRUE`, numeric variables whose unique values
  fall inside `integer_ordinal_range` are treated as ordered factors in
  the table output.

- integer_ordinal_range:

  Integer vector that defines the permissible values for automatic
  ordinal detection (default: `0:10`).

- table1_opts:

  A list of additional options to pass to
  [`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html). For
  example, `list(overall = FALSE, transpose = TRUE)`.

- format:

  A character string specifying the output format. Options are
  `"markdown"` (default), `"latex"`, or `"flextable"`.

- kable_opts:

  A list of additional options controlling table styling:

  - For `format = "latex"`, these are passed to
    [`kableExtra::kable_styling()`](https://rdrr.io/pkg/kableExtra/man/kable_styling.html).

  - For `format = "markdown"`, currently only for documentation
    purposes.

- flex_opts:

  A list of additional options for flextable formatting:

  - `font_size`: Font size for the table (default: 9)

  - `font_size_header`: Font size for headers (default: 10)

  - `theme`: Theme function to apply (default: "theme_vanilla")

  - `autofit`: Whether to autofit columns (default: TRUE)

  - `width`: Table width (0-1 for proportion of page width, default: 1)

- quarto_label:

  An optional label for Quarto cross-references (e.g.,
  "tbl-demographics"). When specified for LaTeX output, this adds a
  `\label{}` command to enable Quarto's cross-referencing system.

## Value

A table object formatted for the specified output:

- For `format = "latex"`, a kableExtra-formatted LaTeX table with
  optional Quarto label

- For `format = "markdown"`, a markdown-formatted kable table with bold
  variable names

- For `format = "flextable"`, a flextable object optimized for Word
  output

## Details

Ordinal/binary controls:

- `binary_to_yesno` detects numeric/logical 0/1 columns in `vars` and
  converts them to two-level factors (defaults to `"No"/"Yes"` labels
  via `binary_labels`).

- Explicit ordinal variables can be supplied via `ordinal_vars`; these
  are coerced to ordered factors before table construction.

- Use `ordinal_levels` (a named list) to specify per-variable level
  order and, optionally, display labels. Each entry can be a simple
  vector of desired levels, a named vector (names = raw values, values =
  labels), or a list with `levels`/`labels`.

- When `auto_integer_ordinals = TRUE`, any numeric variable whose unique
  values fall fully inside `integer_ordinal_range` (default `0:10`) is
  automatically treated as ordered, unless it was already converted.
  This is useful for Likert-style items stored as integers.

## Examples

``` r
if (FALSE) { # \dontrun{
# Flextable output for Word
flex_table <- margot_make_tables(
  data = mydata,
  vars = c("age", "gender", "income"),
  by = "group",
  labels = c("age" = "Age", "gender" = "Gender", "income" = "Income"),
  factor_vars = "gender",
  table1_opts = list(overall = FALSE, transpose = TRUE),
  format = "flextable",
  flex_opts = list(font_size = 8)
)
} # }
```
