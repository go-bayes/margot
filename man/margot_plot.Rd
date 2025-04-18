% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_plot.R
\name{margot_plot}
\alias{margot_plot}
\title{Create a Margot Plot with Interpretation}
\usage{
margot_plot(
  .data,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  custom_order = NULL,
  title_binary = NULL,
  include_coefficients = TRUE,
  standardize_label = c("NZ", "US", "none"),
  interpret_all_E_gt1 = FALSE,
  ...,
  options = list(),
  label_mapping = NULL,
  save_output = FALSE,
  use_timestamp = FALSE,
  base_filename = "margot_plot_output",
  prefix = NULL,
  save_path = here::here("push_mods"),
  original_df = NULL
)
}
\arguments{
\item{.data}{A data frame with:
- `outcome` column,
- effect columns: `E[Y(1)]-E[Y(0)]` or `E[Y(1)]/E[Y(0)]`,
- confidence limits: `2.5 %`, `97.5 %`,
- `E_Value`, `E_Val_bound`, and optional `unit`.}

\item{type}{One of `"RD"` (Risk Difference) or `"RR"` (Risk Ratio). Default: `"RD"`.}

\item{order}{Outcome sorting. Options:
\describe{
  \item{alphabetical}{A–Z}
  \item{magnitude_desc}{|effect| descending (alias for old `"magnitude"`/`"default"`) }
  \item{magnitude_asc}{|effect| ascending}
  \item{evaluebound_desc}{E‑value bound descending}
  \item{evaluebound_asc}{E‑value bound ascending}
  \item{custom}{use `custom_order`}
  \item{default}{alias for `"magnitude_desc"` (deprecated)}
}
Default: `"alphabetical"`.}

\item{custom_order}{Character vector for `order = "custom"`.}

\item{title_binary}{Optional title for binary plots.}

\item{include_coefficients}{Logical; include effect values on plot. Default: `TRUE`.}

\item{standardize_label}{One of `"NZ"`, `"US"`, or `"none"`. Controls x-axis label. Default: `"NZ"`.}

\item{interpret_all_E_gt1}{Logical; if `TRUE`, interprets all effects with `E_Value>1` & `E_Val_bound>1`. Default: `FALSE`.}

\item{...}{Additional arguments passed to `options`.}

\item{options}{List of plot & label options (see details).}

\item{label_mapping}{Named vector mapping original to display labels.}

\item{save_output}{Logical; if `TRUE`, saves output via `here_save_qs()`. Default: `FALSE`.}

\item{use_timestamp}{Logical; append timestamp. Default: `FALSE`.}

\item{base_filename}{Base of output filename. Default: `"margot_plot_output"`.}

\item{prefix}{Optional filename prefix.}

\item{save_path}{Directory for saving. Default: `here::here("push_mods")`.}

\item{original_df}{Optional data frame for back-transformation to original scale.}
}
\value{
A list with:
  \itemize{
    \item `plot`: a `ggplot2` object
    \item `interpretation`: markdown string
    \item `transformed_table`: input `.data` with transformed rownames
  }
}
\description{
This function creates a Margot plot for visualising causal effects with flexible sorting,
embeds a compact interpretation, and returns a transformed table.
}
\details{
Sorting is implemented via `group_tab()`. The `options` list can include:
  title, subtitle, base_size, text_size, point_size, colors, theme, facet_var,
  show_evalues, evalue_digits, remove_tx_prefix, remove_z_suffix,
  use_title_case, remove_underscores.
}
