# Resort/Reorder LMTP Contrasts by Recomputing from Models

This function recomputes LMTP contrasts with the desired ordering by
calling lmtp::lmtp_contrast() with models in the specified order, then
regenerating evaluation tables.

## Usage

``` r
margot_lmtp_resort_contrast(
  lmtp_output,
  contrast_order = NULL,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  specific_contrasts = NULL
)

margot_resort_contrast_lmtp(
  lmtp_output,
  contrast_order = NULL,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  specific_contrasts = NULL
)
```

## Arguments

- lmtp_output:

  Output from margot_lmtp() function containing models, contrasts, and
  tables

- contrast_order:

  A character vector specifying the desired order of shift names (e.g.,
  c("convert", "religious", "done", "secular", "null"))

- contrast_type:

  Type of contrasts to compute: "pairwise" or "null". Default is
  "pairwise".

- contrast_scale:

  Scale for contrasts: "additive", "rr", or "or". Default is "additive".

- specific_contrasts:

  Optional list of specific contrasts to compute. Each element should be
  a character vector of length 2 with shift names (e.g.,
  list(c("convert", "religious")))

## Value

A modified version of the lmtp_output with reordered contrasts and
regenerated tables

## Examples

``` r
if (FALSE) { # \dontrun{
# reorder all contrasts with new preference order
result_reordered <- margot_lmtp_resort_contrast(
  models_6_cp,
  contrast_order = c("convert", "religious", "done", "secular", "null")
)

# compute specific contrasts only
result_specific <- margot_lmtp_resort_contrast(
  models_6_cp,
  specific_contrasts = list(
    c("convert", "religious"),
    c("done", "religious"),
    c("secular", "religious")
  )
)
} # }
```
