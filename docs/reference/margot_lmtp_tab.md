# Summarise LMTP Output into a Data Frame

This function takes the output from \`lmtp::lmtp_contrast()\` and
creates a data frame summarising the estimates. It allows for scaling
the estimates as either risk differences (RD) or risk ratios (RR). The
resulting data frame includes the estimate, standard error, and 95

## Usage

``` r
margot_lmtp_tab(lmtp_output, scale = c("RD", "RR"), new_name = "")
```

## Arguments

- lmtp_output:

  The output object from \`lmtp::lmtp_contrast()\`.

- scale:

  A character string specifying the scale of the estimate: "RD" or "RR".
  Default is "RD".

- new_name:

  A character string to name the row of the output data frame,
  representing the treatment contrast being summarised.

## Value

A data frame with four columns: the estimate under the specified scale,
its standard error, and the lower and upper bounds of the 95
