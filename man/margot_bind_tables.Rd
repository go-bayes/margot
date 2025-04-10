% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_bind_tables.R
\name{margot_bind_tables}
\alias{margot_bind_tables}
\title{Bind and Format Domain-Specific Tables}
\usage{
margot_bind_tables(
  tables_list,
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
\item{tables_list}{A named list of data frames. Each element should represent a domain-specific table.
The names of the list are used as domain labels.}

\item{e_val_bound_threshold}{Numeric. Rows with \code{E_Val_bound} exceeding this threshold will be highlighted.
Default is \code{1}.}

\item{highlight_color}{Character. The background highlight colour for output formats. Default is \code{"yellow"}.
Set to \code{NULL} to disable highlighting color.}

\item{bold}{Logical. Whether to bold highlighted rows. For LaTeX this uses the \code{bold} parameter; for HTML,
this is achieved with inline CSS. For Markdown, this adds asterisks around the entire row. Default is \code{TRUE}.}

\item{output_format}{Character. The output format passed to \code{kbl()}. Either \code{"latex"}, \code{"html"},
or \code{"markdown"}. Default is \code{"latex"}.}

\item{rename_cols}{Logical. Whether to rename columns using default renaming. Default is \code{TRUE}.}

\item{col_renames}{Named list. Custom column renamings to apply. Format should be list("new_name" = "old_name").
Default renames "E_Value" to "E-Value" and "E_Val_bound" to "E-Value bound". Supply custom named list to override.}

\item{threshold_col}{Character. The name of the column used for determining which rows to highlight.
Default is "E_Val_bound". This should be the name of the column *before* any renaming is applied.}

\item{kbl_args}{A list of additional arguments to pass to \code{kbl()}. For example, you might supply
\code{list(booktabs = TRUE, caption = "Combined Results by Domain")}.}
}
\value{
A formatted table (of class \code{knitr_kable}) with rows grouped by domain and
  highlighted where \code{E_Val_bound} exceeds the threshold.
}
\description{
This function binds a named list of domain-specific tables, removes the domain column,
highlights rows where \eqn{E_{\text{Val_bound}}} exceeds a specified threshold, and groups
rows by domain. Works with LaTeX, HTML, or Markdown output.
}
