% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_lmtp_evalue.R
\name{margot_lmtp_evalue}
\alias{margot_lmtp_evalue}
\title{Combine LMTP Summary and E-Value Calculation}
\usage{
margot_lmtp_evalue(
  lmtp_output,
  scale = c("RD", "RR"),
  new_name = "character_string",
  delta = 1,
  sd = 1
)
}
\arguments{
\item{lmtp_output}{The output from `lmtp::lmtp_contrast()`, to be summarized and analyzed for E-values.}

\item{scale}{Character string specifying the scale of the estimate to be used in the summary table and
E-value calculation. Valid options are "RD" (risk difference) or "RR" (risk ratio). Default is "RD".}

\item{new_name}{Character string to name the row in the output summary table, representing the treatment
contrast. This name will be applied to the first row of the summary table.}

\item{delta}{The hypothesized increase in outcome for RD scale calculations. Used only when `scale` is "RD".
Default value is 1.}

\item{sd}{The standard deviation of the outcome for RD scale calculations. Used only when `scale` is "RD".
Default value is 1.}
}
\value{
A data frame with the original estimates and their E-values. The table includes columns for the
estimate (either RD or RR), its E-Value, and the E-Value lower bound, excluding the 'standard_error' column.
}
\description{
This function first creates a summary table from the output of `lmtp::lmtp_contrast()` using
`margot_tab_lmtp`, specifying the desired scale (RD or RR) and a new name for the row. It then calculates
E-values for the estimates in the table to assess the potential impact of unmeasured confounding,
appending these values to the summary table.
}
\examples{
\dontrun{
# assuming `contrast_output` is the result from `lmtp::lmtp_contrast()`
summary_evalues <- margot_lmtp_evalue(
  lmtp_output = contrast_output,
  scale = "RD",
  new_name = "Treatment Effect"
)
print(summary_evalues)
}

}
\seealso{
\code{\link{margot_tab_lmtp}}, \code{\link{lmtp_evalue_tab}} for the underlying functions used.
}
