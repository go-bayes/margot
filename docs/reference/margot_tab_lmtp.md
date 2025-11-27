# Summarise LMTP Output into a Data Frame

This function takes the output from \`lmtp::lmtp_contrast()\` and
creates a data frame summarising the estimates. It allows for scaling
the estimates as either risk differences (RD) or risk ratios (RR). The
resulting data frame includes the estimate, standard error, and 95

## Usage

``` r
margot_tab_lmtp(
  lmtp_output,
  scale = c("RD", "RR"),
  new_name = "character_string"
)
```

## Arguments

- lmtp_output:

  The output object from \`lmtp::lmtp_contrast()\`.

- scale:

  A character string specifying the scale of the estimate. Valid options
  are "RD" for risk difference and "RR" for risk ratio. Default is "RD".

- new_name:

  A character string to name the row of the output data frame,
  representing the treatment contrast being summarised.

## Value

A data frame with four columns: the estimate under the specified scale,
its standard error, and the lower and upper bounds of the 95

## Examples

``` r
# Assuming `contrast_hours_charity_z_null` is output from `lmtp::lmtp_contrast()`
tab_contrast_hours_charity_z_null <- margot_tab_lmtp(
  contrast_hours_charity_z_null,
  scale = "RD",
  new_name = "relig service: hours volunteer"
)
#> Error in eval(expr, envir, enclos): object 'contrast_hours_charity_z_null' not found
```
