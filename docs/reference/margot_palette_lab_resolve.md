# Resolve lab palette colours for arbitrary shift labels

Resolve lab palette colours for arbitrary shift labels

## Usage

``` r
margot_palette_lab_resolve(
  labels,
  palette = margot_palette("lab"),
  default = NA_character_
)
```

## Arguments

- labels:

  Character vector of shift/policy names.

- palette:

  Character vector returned by \`margot_palette("lab")\`.

- default:

  Hex colour used when no match is found.

## Value

Character vector of colours aligned with \`labels\`.
