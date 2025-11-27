# Retrieve a named color palette for plotting

Provides centralised, named palettes used across margot plotting
functions. The "lab" palette is intended for LMTP positivity/overlap
plots and includes explicit colours for common shift names (cleaned
suffixes), including \`null\`, \`shift_zero\`, and \`ipsi\_\` variants
(e.g., \`ipsi_02\`, \`ipsi_05\`, \`ipsi_10\`).

## Usage

``` r
margot_palette(name = c("lab", "classic"))
```

## Arguments

- name:

  Character palette name. Currently supported: "lab", "classic".

## Value

A named character vector of hex colours.
