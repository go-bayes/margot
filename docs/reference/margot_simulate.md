# Simulate longitudinal exposures, outcomes, and covariates

\`margot_simulate()\` draws baseline covariates (\`B\`), time-varying
covariates (\`L\`), exposures (\`A\`), and lead outcomes (\`Y\`) for a
synthetic panel study. Monotone attrition can depend on past exposure
and/or a latent shared frailty, and an optional indicator column records
whether each unit remains uncensored at every wave. The simulator
supports heterogeneous treatment effects, feedback from previous
outcomes into future exposures, and marginal item missingness.

## Usage

``` r
margot_simulate(
  n,
  waves,
  exposures = NULL,
  outcomes = NULL,
  p_covars = 20,
  censoring = list(rate = 0.25),
  item_missing_rate = 0,
  exposure_outcome = 0.5,
  y_feedback = 0.4,
  positivity = "good",
  outcome_type = NULL,
  wide = TRUE,
  seed = NULL,
  params = list(),
  ...
)
```

## Arguments

- n:

  Number of individuals.

- waves:

  Number of follow-up waves (outcomes are produced for wave
  `waves + 1`).

- exposures:

  Named list describing each exposure. Every element must contain a
  `type` field (\`"binary"\` or \`"normal"\`). Optional elements: `het`
  (baseline modifiers) and `lag_Y = TRUE` to enable outcome-to-exposure
  feedback.

- outcomes:

  Named list describing outcomes. Defaults to a single normal outcome
  called \`"Y"\`.

- p_covars:

  Number of baseline (\`B\`) covariates.

- censoring:

  List controlling attrition. Must include `rate`; optional logical
  flags `exposure_dependence`, `latent_dependence`, numeric
  `latent_rho`, and logical `indicator` to append `tX_not_censored`
  columns.

- item_missing_rate:

  MCAR probability an observed value is replaced by `NA`.

- exposure_outcome:

  Coefficient for the exposure → outcome path.

- y_feedback:

  Coefficient for lagged outcome feedback when an exposure lists
  `lag_Y = TRUE`.

- positivity:

  \`"good"\`, \`"poor"\`, or a numeric probability in (0, 1) governing
  baseline exposure prevalence.

- outcome_type:

  Shortcut for a single outcome: \`"continuous"\` (default) or
  \`"binary"\`. Ignored when `outcomes` is supplied.

- wide:

  If `TRUE` (default) return a wide data set; otherwise long format.

- seed:

  Integer seed for reproducibility.

- params:

  Named list of scalar coefficients (see above).

- ...:

  Deprecated arguments; ignored with a warning.

## Value

A \`tibble\` in wide or long form containing baseline \`B\` variables,
time-varying \`L\` covariates, \`A\` exposures, optional censoring
indicators, and lead outcomes \`Y\`. The object carries an attribute
`"margot_meta"` with the matched call and a timestamp.

## Details

The default parameter set is


     .default_sim_params()
     ## $cens_exp_coef   0.4
     ## $cens_latent_rho 0.5
     ## $exp_intercept   -0.2
     ## $exp_L1_coef     0.2
     ## $out_B1_coef     0.1

## The \`params\` argument

Supply a named list to override the internal defaults given by
`.default_sim_params()`. Typical entries include `cens_exp_coef`,
`exp_L1_coef`, and `out_B1_coef`.

## See also

`.default_sim_params`

## Examples

``` r
## basic usage
dat <- margot_simulate(n = 200, waves = 3, seed = 1)
dplyr::glimpse(dat)
#> Rows: 200
#> Columns: 35
#> $ id    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 1…
#> $ B1    <dbl> 1.38717622, 0.30733801, 1.52385587, -1.45786586, -1.31218682, 0.…
#> $ B2    <dbl> 0.56867050, 0.32317764, -0.79634791, -0.84417680, -0.02831455, -…
#> $ B3    <dbl> 0.359327067, -0.498545661, 0.645835598, -1.284425798, 0.62591385…
#> $ B4    <dbl> 1.13120902, 2.15236607, 0.50850055, -1.05442367, -0.39867018, 0.…
#> $ B5    <dbl> -0.09360758, -0.65509314, -0.25503349, 0.02718249, 2.37682411, -…
#> $ B6    <dbl> 1.3358744996, -0.9704755209, 0.3616893118, -1.4063023965, -0.931…
#> $ B7    <dbl> -1.576558708, -0.088479569, 0.487995724, -2.264870579, 0.1313669…
#> $ B8    <dbl> 1.20071079, -0.59614574, -1.04053816, 0.47034678, 0.56500777, 0.…
#> $ B9    <dbl> 0.97922287, 0.42597734, -0.01367560, 0.58258505, -0.26852743, 2.…
#> $ B10   <dbl> 1.19511520, 0.23398869, -0.57308037, -0.95525308, -0.34721398, 1…
#> $ B11   <dbl> -0.61221854, -2.04242795, 1.02058737, -1.07656682, -1.29516511, …
#> $ B12   <dbl> 0.53291378, 0.08565599, 0.03440012, -2.29850687, -1.24441193, 1.…
#> $ B13   <dbl> -0.176066882, 0.742283929, 0.333243297, -1.440735602, 0.05931179…
#> $ B14   <dbl> 1.93000559, 0.05657933, 0.40769632, -1.33184913, 0.05518891, -1.…
#> $ B15   <dbl> -0.319607922, -0.445111940, 0.453195957, -2.113141884, 0.1863665…
#> $ B16   <dbl> -0.51130370, -1.25585264, 1.82817341, 1.61178682, -0.04679639, -…
#> $ B17   <dbl> 0.89312064, -0.01132154, 1.52172495, -0.22530871, -0.86537188, 0…
#> $ B18   <dbl> -0.073517558, -1.502434951, 1.923492831, -0.785495001, -0.452210…
#> $ B19   <dbl> -0.5040000, 1.3892949, 0.5235887, -1.2756173, -0.3453183, 0.9109…
#> $ B20   <dbl> -0.3947381, 0.2234016, 0.7777960, -1.3440720, -0.2789659, 1.1170…
#> $ t0_A1 <int> 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0…
#> $ t1_L1 <dbl> -1.47983426, 1.02834657, -2.22105108, NA, -1.63855763, NA, 0.357…
#> $ t1_L2 <dbl> -0.302092925, 0.517340829, -1.164086557, NA, 1.274679847, NA, -0…
#> $ t1_L3 <dbl> -0.107179673, -0.298918555, 0.309770555, NA, 0.254681415, NA, 0.…
#> $ t1_A1 <dbl> 0, 1, 0, NA, 1, NA, 1, 0, 1, 0, 0, NA, 1, NA, 1, NA, 1, 1, 1, 0,…
#> $ t2_L1 <dbl> NA, NA, -0.7394305, NA, -1.5425303, NA, 0.3851541, -1.4544780, -…
#> $ t2_L2 <dbl> NA, NA, 1.56739714, NA, -1.34127895, NA, -0.93517145, -0.0457230…
#> $ t2_L3 <dbl> NA, NA, 0.97422723, NA, 1.33379026, NA, 0.27279618, 1.96492237, …
#> $ t2_A1 <dbl> NA, NA, 0, NA, 1, NA, 0, 0, 0, 0, NA, NA, 1, NA, NA, NA, 0, 1, 1…
#> $ t3_L1 <dbl> NA, NA, 1.28184341, NA, -1.36408357, NA, -0.03276689, -1.1094694…
#> $ t3_L2 <dbl> NA, NA, 0.16440270, NA, 0.34927903, NA, 0.95314451, -2.22791423,…
#> $ t3_L3 <dbl> NA, NA, 1.9731395, NA, -0.6404094, NA, -0.5885797, 1.2987237, NA…
#> $ t3_A1 <dbl> NA, NA, 0, NA, 0, NA, 0, 1, NA, NA, NA, NA, 1, NA, NA, NA, NA, 0…
#> $ t4_Y  <dbl> NA, NA, 0.7151895, NA, NA, NA, 0.3641293, -1.8084884, NA, NA, NA…

## heterogeneous treatment effect with censoring indicator
dat2 <- margot_simulate(
  n = 800,
  waves = 4,
  exposures = list(
    A1 = list(
      type = "binary",
      het  = list(modifier = "B2", coef = 0.6)
    )
  ),
  censoring = list(
    rate = 0.25, exposure_dependence = TRUE,
    indicator = TRUE
  ),
  seed = 42
)
```
