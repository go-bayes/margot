# Compute Gender-Based Sample Weights Using Baseline Wave Proportions

Compute sample weights for gender adjustment based on the baseline wave
proportions.

Compute sample weights for gender adjustment based on the baseline wave
proportions.

## Usage

``` r
margot_compute_gender_weights_by_wave(
  data,
  male_col = "male",
  wave_col = "wave",
  target_wave,
  target_male_prop = 0.5
)

margot_compute_gender_weights_by_wave(
  data,
  male_col = "male",
  wave_col = "wave",
  target_wave,
  target_male_prop = 0.5
)
```

## Arguments

- data:

  A data frame containing gender and wave information.

- male_col:

  A character string specifying the column that indicates male gender (1
  for male, 0 for female). Default is `"male"`.

- wave_col:

  A character string specifying the column indicating the wave. Default
  is `"wave"`.

- target_wave:

  The value in `wave_col` that identifies the baseline wave.

- target_male_prop:

  A numeric value between 0 and 1 representing the target proportion of
  males. Default is 0.5.

## Value

A numeric vector of sample weights for all rows.

A numeric vector of sample weights for all rows.

## Details

The function calculates the gender proportions in the baseline wave and
computes weights so that the overall sample aligns with the target
gender distribution. The same weights are then applied to all rows.

The function computes the sample proportions in the baseline wave and
calculates weights by comparing these proportions with the target
proportions. It upweights the underrepresented gender and downweights
the overrepresented gender. The resulting weights are applied to the
full dataset.

The function calculates the gender proportions in the baseline wave and
computes weights so that the overall sample aligns with the target
gender distribution. The same weights are then applied to all rows.

The function computes the sample proportions in the baseline wave and
calculates weights by comparing these proportions with the target
proportions. It upweights the underrepresented gender and downweights
the overrepresented gender. The resulting weights are applied to the
full dataset.

## Examples

``` r
dat <- data.frame(
  id = 1:100,
  male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
  wave = rep(1:2, each = 50)
)
weights <- margot_compute_gender_weights_by_wave(dat,
  male_col = "male",
  wave_col = "wave",
  target_wave = 1,
  target_male_prop = 0.52
)
head(weights)
#> [1] 0.6153846 2.3636364 0.6153846 0.6153846 2.3636364 0.6153846

dat <- data.frame(
  id = 1:100,
  male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
  wave = rep(1:2, each = 50)
)
weights <- margot_compute_gender_weights_by_wave(dat,
  male_col = "male",
  wave_col = "wave",
  target_wave = 1,
  target_male_prop = 0.52
)
head(weights)
#> [1] 0.8571429 0.8571429 0.8571429 1.1818182 1.1818182 1.1818182
```
