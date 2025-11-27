# Generate Explanatory Note for QINI Scale

Creates a descriptive note explaining what the QINI curve scale
represents based on the selected scale type (average, cumulative, or
population).

## Usage

``` r
margot_qini_scale_note(scale = "average", n_units = NULL)
```

## Arguments

- scale:

  Character string specifying the scale: "average", "cumulative", or
  "population"

- n_units:

  Optional integer specifying the number of units (for population scale)

## Value

A character string containing the explanatory note

## Examples

``` r
margot_qini_scale_note("average")
#> [1] "The y-axis shows expected policy effects per unit (Q(B) = E[⟨πB(Xi), τ(Xi)⟩]), representing the expected gain from treating units according to the policy. This is the standard maq implementation where gains converge to the ATE at 100% spend."
margot_qini_scale_note("cumulative")
#> [1] "The y-axis shows cumulative gains, where the gain at each point represents the total accumulated benefit from treating all units up to that proportion. This is the traditional uplift modeling approach where curves show total impact."
margot_qini_scale_note("population", n_units = 1000)
#> [1] "The y-axis shows total population impact for 1000 units. Values represent the absolute gain in the outcome from treating units according to the prioritization policy."
```
