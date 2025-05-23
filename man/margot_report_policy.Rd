% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_compute_policy_value_helpers.R
\name{margot_report_policy}
\alias{margot_report_policy}
\title{summarise policy tests (add if absent, adjust, return tidy table)}
\usage{
margot_report_policy(
  cf_out,
  keep = NULL,
  depth = 2L,
  adjust = "bonferroni",
  alpha = 0.05,
  R = 999L,
  seed = 2025L
)
}
\arguments{
\item{cf_out}{**margot** result list.}

\item{keep}{character. which outcomes to include; `NULL` keeps all.}

\item{depth}{integer. policy-tree depth. default 2.}

\item{adjust}{character. multiplicity adjustment method for *p*-values.
default "bonferroni".}

\item{alpha}{numeric. significance level for the pass/fail flag. default 0.05.}

\item{R, seed}{bootstrap settings if tests need to be (re)run.}
}
\value{
tibble. one row per outcome with adjusted *p*-values and pass flag.
}
\description{
this convenience wrapper makes a complete, end-to-end policy report in one
line. it:
1. optionally runs the bootstrap test for the selected outcomes (via
   `margot_add_policy_batch()`),
2. pulls the depth‑specific results with `margot_policy_summary()`, and
3. returns the tidy summary.
}
\details{
nothing is printed; you decide what to do with the tibble (view, knit, etc.).
}
