# Calculate E-values for LMTP Output

This function takes the output from \`margot_tab_lmtp()\`, which
contains estimates of treatment effects, and calculates E-values to
assess the robustness of the estimates to potential unmeasured
confounding. E-values quantify the minimum strength of association, on
the risk ratio scale, that an unmeasured confounder would need to have
with both the treatment and the outcome, to fully explain away the
observed association. The legacy \`"RD"\` option applies the
standardised-continuous-outcome approximation to an outcome-mean
difference; \`"RR"\` treats the estimate as a risk ratio.

## Usage

``` r
lmtp_evalue_tab(x, delta = 1, sd = 1, scale = c("RD", "RR"))
```

## Arguments

- x:

  A data frame output from \`margot_tab_lmtp()\` containing the
  estimates of interest.

- delta:

  The exposure contrast represented by the outcome-mean difference, used
  only when \`scale = "RD"\`. Default is 1.

- sd:

  The outcome standard deviation used to standardise the outcome-mean
  difference, used only when \`scale = "RD"\`. Default is 1.

- scale:

  A character string indicating the calculation: \`"RD"\` for the
  standardised-continuous-outcome approximation from an outcome-mean
  difference, or \`"RR"\` for a risk ratio. Default is \`"RD"\`.

## Value

A data frame similar to \`x\`, with additional columns for E-Value and
its lower bound, excluding the 'standard_error' column. Numeric columns
retain their computational precision; round only when formatting the
table for presentation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'tab_contrast_hours_charity_z_null' is a data frame output from `margot_lmtp_tab()`
lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RD")
lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RR")
} # }
```
