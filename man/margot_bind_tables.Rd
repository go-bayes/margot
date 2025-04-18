% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_bind_tables.R
\name{margot_bind_tables}
\alias{margot_bind_tables}
\title{Bind and Format Domain‑Specific Tables}
\usage{
margot_bind_tables(
  tables_list,
  sort_E_val_bound = c("none", "asc", "desc"),
  e_val_bound_threshold = 1,
  highlight_color = "yellow",
  bold = TRUE,
  output_format = "latex",
  rename_cols = TRUE,
  col_renames = list(`E-Value` = "E_Value", `E-Value bound` = "E_Val_bound"),
  threshold_col = "E_Val_bound",
  kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")
)
}
\arguments{
\item{tables_list}{A named list of data frames; each element is a domain table.}

\item{sort_E_val_bound}{Character: one of "none", "asc", or "desc".
If "asc", sorts by raw E_Val_bound ascending;
if "desc", sorts descending. Default "none".}

\item{e_val_bound_threshold}{Numeric; rows with E_Val_bound above this value will be highlighted. Default 1.}

\item{highlight_color}{Character or NULL; background colour for highlighted rows. Default "yellow".}

\item{bold}{Logical; whether to bold highlighted rows. Default TRUE.}

\item{output_format}{Character; one of "latex", "html", or "markdown". Default "latex".}

\item{rename_cols}{Logical; whether to apply column renaming. Default TRUE.}

\item{col_renames}{Named list; mapping new_name = old_name for renaming.
Default list("E-Value" = "E_Value",
              "E-Value bound" = "E_Val_bound").}

\item{threshold_col}{Character; name of the column (pre‑rename) to test. Default "E_Val_bound".}

\item{kbl_args}{List; extra arguments for kbl(), e.g. list(booktabs = TRUE, caption = "Combined Results by Domain").}
}
\value{
A knitr_kable object with rows sorted, highlighted, and grouped by domain.
}
\description{
This function binds a named list of domain‑specific tables, optionally
sorts by the raw E_Val_bound column (ascending or descending), highlights
rows where E_Val_bound exceeds a threshold, and groups rows by domain.
Works for LaTeX, HTML, or Markdown output.
}
