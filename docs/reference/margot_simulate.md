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
#> $ B1    <dbl> 1.09222869, 0.03732879, 0.33325180, -1.04099358, 0.93674047, -0.…
#> $ B2    <dbl> 0.98609482, 0.74762549, 1.08877775, -0.69113630, 0.64981762, -1.…
#> $ B3    <dbl> 0.77706593, -0.34995553, 1.30185226, 0.44531602, -0.25445307, 1.…
#> $ B4    <dbl> 0.044700071, -0.173093988, 0.424377646, -1.437486888, 0.08399902…
#> $ B5    <dbl> 0.3773246, -0.1806900, 1.4705661, -1.3590267, -1.3091550, 2.9373…
#> $ B6    <dbl> -0.82315045, 0.50480193, -0.95997714, -1.82435320, 1.47625713, 0…
#> $ B7    <dbl> 1.74749521, 0.34876753, -1.06408121, -0.88525605, -0.53675489, 0…
#> $ B8    <dbl> 0.2865951, -2.0161866, 0.6032415, 0.2090040, 0.9570337, -1.14702…
#> $ B9    <dbl> -1.0129410, -1.3887169, 0.3829138, -1.4931690, -0.2246240, 1.251…
#> $ B10   <dbl> -1.17120415, -1.28786692, 1.11064443, -0.11304329, -0.03904302, …
#> $ B11   <dbl> 1.8429192, -0.7108524, -0.6796990, 0.3312494, 0.1083900, 0.43791…
#> $ B12   <dbl> 0.026092250, -0.223060270, 1.102441126, -2.250833422, -1.8882708…
#> $ B13   <dbl> 1.27065399, -1.34333573, 0.87174743, -0.25277756, -1.13278114, 0…
#> $ B14   <dbl> 1.08488110, 0.10673698, 1.07301138, -2.55406904, -1.04843084, -0…
#> $ B15   <dbl> 1.00281979, 1.82798521, -0.66489836, -1.67947921, -0.29287152, 0…
#> $ B16   <dbl> 0.13380063, 0.81528145, -0.42156464, 0.15382208, 0.22484808, -0.…
#> $ B17   <dbl> 0.13483380, -0.39110209, 0.35962137, -2.49798044, -0.47972844, 0…
#> $ B18   <dbl> -0.97574176, -0.46776654, 1.26494753, -0.02409793, 0.21305225, 0…
#> $ B19   <dbl> -0.2088290, 0.6651854, 1.1247664, -1.6427421, -0.4638031, -0.591…
#> $ B20   <dbl> 0.63608838, 1.35308899, 0.95116036, 0.14034279, -0.79455062, 1.8…
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
#> $ t4_Y  <dbl> NA, NA, 0.5961291, NA, NA, NA, 0.2836739, -1.8521649, NA, NA, NA…

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
