# Summarise LMTP or Causal Forest Output into a Data Frame

This function takes the output from \`lmtp::lmtp_contrast()\` or a
causal forest model and creates a data frame summarising the estimates.
It allows for scaling the estimates as either risk differences (RD) or
risk ratios (RR) for LMTP models. For causal forest models, the scale is
always "RD". The resulting data frame includes the estimate, standard
error, and 95

## Usage

``` r
margot_model_tab(
  model_output,
  scale = c("RD", "RR"),
  new_name = "character_string"
)
```

## Arguments

- model_output:

  The output object from \`lmtp::lmtp_contrast()\` or a causal forest
  model.

- scale:

  A character string specifying the scale of the estimate. Valid options
  are "RD" for risk difference and "RR" for risk ratio. Default is "RD".
  This parameter is ignored for causal forest models.

- new_name:

  A character string to name the row of the output data frame,
  representing the treatment contrast being summarised.

## Value

A data frame with four columns: the estimate under the specified scale,
its standard error, and the lower and upper bounds of the 95

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming `contrast_hours_charity_z_null` is output from `lmtp::lmtp_contrast()`
tab_contrast_hours_charity_z_null <- margot_model_tab(
  contrast_hours_charity_z_null,
  scale = "RD",
  new_name = "relig service: hours volunteer"
)
print(tab_contrast_hours_charity_z_null)
} # }
```
