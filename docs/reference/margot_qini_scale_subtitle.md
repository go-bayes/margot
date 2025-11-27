# Generate Brief Scale Description for Plot Subtitles

Creates a brief description of the QINI scale for use in plot subtitles

## Usage

``` r
margot_qini_scale_subtitle(scale = "average", n_units = NULL)
```

## Arguments

- scale:

  Character string specifying the scale: "average", "cumulative", or
  "population"

- n_units:

  Optional integer specifying the number of units (for population scale)

## Value

A character string containing the brief description

## Examples

``` r
margot_qini_scale_subtitle("average")
#> [1] "Showing expected policy effects (maq default)"
margot_qini_scale_subtitle("cumulative")
#> [1] "Showing cumulative gains"
margot_qini_scale_subtitle("population", n_units = 1000)
#> [1] "Showing total impact for 1000 units"
```
