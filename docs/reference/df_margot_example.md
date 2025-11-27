# Margot Example Dataset

A small simulated dataset for demonstrating margot functionality. This
dataset mimics the structure of longitudinal panel data but contains
entirely simulated data with no real participant information.

## Usage

``` r
df_margot_example
```

## Format

A data frame with 300 rows (100 participants × 3 waves) and 15
variables:

- id:

  Participant identifier

- wave:

  Measurement wave (0, 1, 2)

- year:

  Year of measurement (2018, 2019, 2020)

- male:

  Binary gender indicator (0 = not male, 1 = male)

- age:

  Age in years

- partner:

  Relationship status (0 = no partner, 1 = has partner)

- eth_cat:

  Ethnicity category (euro, maori, pacific, asian)

- sample_weights:

  Survey sampling weights

- forgiveness:

  Forgiveness scale score (1-7)

- gratitude:

  Gratitude scale score (1-7)

- modesty:

  Modesty scale score (1-7)

- alcohol_frequency:

  Frequency of alcohol consumption (0-7)

- alcohol_intensity:

  Intensity of alcohol consumption (0-7)

- hours_exercise:

  Hours of exercise per week

- ego_rubin:

  Ego resilience scale (1-7)

## Details

This is a lightweight example dataset suitable for package examples and
testing. For larger, more comprehensive simulated datasets that better
reflect real longitudinal studies, use \`fetch_margot_data()\` with
version = "v1" or "v2".

## See also

[`fetch_margot_data`](https://go-bayes.github.io/margot/reference/fetch_margot_data.md)
for accessing larger datasets
[`list_margot_data`](https://go-bayes.github.io/margot/reference/list_margot_data.md)
to see all available datasets

## Examples

``` r
# load the example data
data(df_margot_example)

# check structure
str(df_margot_example)
#> tibble [300 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ id               : int [1:300] 1 1 1 2 2 2 3 3 3 4 ...
#>  $ wave             : int [1:300] 0 1 2 0 1 2 0 1 2 0 ...
#>  $ year             : int [1:300] 2018 2019 2020 2018 2019 2020 2018 2019 2020 2018 ...
#>  $ male             : int [1:300] 1 1 1 0 0 0 1 1 1 0 ...
#>  $ age              : num [1:300] 51 52 53 27 28 29 43 44 45 61 ...
#>  $ partner          : num [1:300] 1 1 0 0 0 1 0 1 0 1 ...
#>  $ eth_cat          : chr [1:300] "euro" "euro" "euro" "euro" ...
#>  $ sample_weights   : num [1:300] 0.971 0.982 0.723 1.055 0.917 ...
#>  $ forgiveness      : num [1:300] 3.93 4.12 4.54 1.55 1.41 ...
#>  $ gratitude        : num [1:300] 4.48 3.88 4.19 4.39 5.04 ...
#>  $ modesty          : num [1:300] 3.76 4 3.93 2.77 3.11 ...
#>  $ alcohol_frequency: num [1:300] 7 6 6 4 2 7 7 5 5 6 ...
#>  $ alcohol_intensity: num [1:300] 4 1 0 6 2 3 3 0 NA 2 ...
#>  $ hours_exercise   : num [1:300] 9.6 1 0.2 3.3 4.1 NA 3.6 9.6 NA 1.6 ...
#>  $ ego_rubin        : num [1:300] 5.44 5.22 5.43 4.56 4.75 ...

# basic summary by wave
table(df_margot_example$wave)
#> 
#>   0   1   2 
#> 100 100 100 
```
