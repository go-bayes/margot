\name{margot_positivity_report}
\alias{margot_positivity_report}
\title{Assemble a full LMTP positivity report}
\usage{
margot_positivity_report(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.1, near_zero_median = 0.001,
    near_zero_cv = 0.05),
  include_policy_rates = TRUE,
  effect_table = NULL,
  digits = 3,
  trim_right = 0.999,
  thresholds = c(5, 10, 25, 50, 100),
  summary_compact = TRUE,
  include_plot = TRUE,
  plot_args = list(),
  interpret_args = list()
)
}
\arguments{
\item{x}{Result of `margot_lmtp()` (with `$models`) or another object accepted by the positivity helpers.}

\item{outcome}{Character outcome name.}

\item{shifts}{Optional character vector of shifts to include (full or cleaned names). If `NULL`, all shifts are used.}

\item{label_mapping}{Optional label map passed to downstream helpers.}

\item{waves}{Optional integer vector selecting waves.}

\item{remove_waves}{Optional integer vector of waves to drop after subsetting.}

\item{test_thresholds}{Named list of thresholds forwarded to the summary and narrative functions.}

\item{include_policy_rates}{Logical; whether to compute policy-rate columns and include the policy-rate narrative.}

\item{effect_table}{Optional effect table merged into the summary.}

\item{digits}{Integer rounding applied to numeric outputs.}

\item{trim_right}{Numeric in `(0, 1]`; passed to `margot_lmtp_weight_diag_from_fit()`.}

\item{thresholds}{Numeric vector of ratio thresholds for the diagnostics.}

\item{summary_compact}{Logical; request the compact summary table.}

\item{include_plot}{Logical; whether to build the overlap plot.}

\item{plot_args}{Named list overriding defaults in `margot_plot_lmtp_overlap_grid()`.}

\item{interpret_args}{Named list overriding defaults in `margot_interpret_lmtp_positivity()`.}
}
\value{
A named list containing the summary table, diagnostics list, optional plot,
structured narrative, methods paragraph, per-wave summary table,
and metadata (including a censoring summary) describing the request.
}
\description{
Bundles the LMTP positivity summary table, density-ratio diagnostics,
interpretive text, overlap plot, and a ready-to-drop-in methods paragraph
into a single object for Quarto or reporting workflows.
}
