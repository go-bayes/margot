# Create Z-score to Original Scale Mapping for Log-Transformed Data

This function creates a data frame that maps standard z-scores to their
corresponding values on the original data scale for log-transformed
data. It uses the \`back_transform_log_z\` function to perform the
back-transformation and presents the results in a tidy data frame.

## Usage

``` r
margot_back_transform_log_z(
  log_mean,
  log_sd,
  z_scores = c(-2, -1, -0.5, 0, 0.5, 1, 2),
  label = "data_scale"
)
```

## Arguments

- log_mean:

  The mean of the log-transformed dataset from which the z-scores were
  calculated.

- log_sd:

  The standard deviation of the log-transformed dataset from which the
  z-scores were calculated.

- z_scores:

  Optional vector of z-scores to transform. Defaults to c(-2, -1, -0.5,
  0, 0.5, 1, 2) representing common points in a normal distribution.

- label:

  Optional string to label the data scale column. Defaults to
  "data_scale".

## Value

A data frame with two columns: z_score and the original data scale
values. The name of the second column will be the value of the \`label\`
parameter.

## Examples

``` r
# Get mean and sd from original log-transformed income data
log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
#> Error: object 'original_df' not found
log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)
#> Error: object 'original_df' not found

# Create mapping table with default z-scores
income_mapping <- margot_back_transform_log_z(
  log_mean = log_mean_inc,
  log_sd = log_sd_inc,
  label = "household_income"
)
#> Error: object 'log_mean_inc' not found
print(income_mapping)
#> Error: object 'income_mapping' not found

# Create mapping with custom z-scores
custom_mapping <- margot_back_transform_log_z(
  log_mean = log_mean_inc,
  log_sd = log_sd_inc,
  z_scores = c(-1, 0, 1),
  label = "household_income"
)
#> Error: object 'log_mean_inc' not found
print(custom_mapping)
#> Error: object 'custom_mapping' not found
```
