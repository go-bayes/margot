# Post-process models to recover Qini curves via propensity trimming

for any model in a results list whose qini_objects are null or empty,
this function applies an overlap restriction on the estimated\\ n#'
propensity scores and recomputes in‑sample Qini curves on the trimmed
data, without touching the original ATE or forest objects.

## Usage

``` r
margot_rescue_qini(
  model_results,
  propensity_bounds = c(0.05, 0.95),
  verbose = TRUE
)
```

## Arguments

- model_results:

  a list from margot_causal_forest() with save_models = TRUE and
  save_data = TRUE

- propensity_bounds:

  numeric length‑2 vector of lower/upper bounds for forest\$W.hat
  (default c(0.05, 0.95))

- verbose:

  logical; if TRUE prints progress messages (default TRUE)

## Value

modified model_results with rescued qini_data and qini_objects for any
models that initially had empty gain
