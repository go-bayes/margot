# Compute Gender-Based Sample Weights (deprecated, use \`margot_compute_gender_weights_by_wave\`)

This function computes sample weights based on gender to achieve a
target gender balance in the population. It assumes a binary gender
classification where 1 represents male and 0 represents female.

## Usage

``` r
margot_compute_gender_weights(data, male_col = "male", target_male_prop = 0.5)
```

## Arguments

- data:

  A data frame containing the gender information.

- male_col:

  Character string specifying the name of the column in \`data\` that
  indicates male gender (1 for male, 0 for female). Default is "male".

- target_male_prop:

  Numeric value between 0 and 1 specifying the target proportion of
  males in the population. Default is 0.5 (50% male).

## Value

A numeric vector of sample weights. Each weight corresponds to a row in
the input data frame.

## Details

The function calculates weights that, when applied, will adjust the
sample to match the specified target gender proportion. It upweights the
underrepresented gender and downweights the overrepresented gender.

## Examples

``` r
# Create a sample dataset
dat <- data.frame(id = 1:100, male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)))

# Compute weights
weights <- margot_compute_gender_weights(dat, male_col = "male", target_male_prop = 0.5)
#> Warning: `margot_compute_gender_weights()` was deprecated in margot 1.0.0.
#> ℹ Please use `margot_compute_gender_weights_by_wave()` instead.

# Check weight distribution
table(round(weights, 3))
#> 
#> 0.725 1.613 
#>    69    31 
```
