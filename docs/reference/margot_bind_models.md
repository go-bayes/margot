# Combine multiple batched model outputs (with covariates & metadata)

This function combines either "causal forest" or "lmtp" batched model
outputs into a single object, provided they share compatible structures.
It now also preserves \`covariates\`, \`data\`, \`weights\`, and any
flip or rescue metadata when binding causal forest outputs.

## Usage

``` r
margot_bind_models(..., quiet = FALSE)
```

## Arguments

- ...:

  One or more batched model output objects.

- quiet:

  Logical; if TRUE, suppresses CLI feedback messages. Default is FALSE.

## Value

A single combined model output object.
