# Compute Difference in Means, Standard Error, and Confidence Intervals Between Two Groups

This function calculates the difference in means, the standard error of
the difference, and the 95 as a list that includes the mean (theta) and
the standard error (std.error) of the group. The result includes both a
data frame and an interpretation string formatted for easy use with
\`glue::glue\` in Quarto documents etc

## Usage

``` r
compute_difference_means(group1, group2)
```

## Arguments

- group1:

  A list containing the mean and standard error of group 1. Expected
  structure: list(vals = list(theta = x, std.error = y)).

- group2:

  A list containing the mean and standard error of group 2. Expected
  structure: list(vals = list(theta = x, std.error = y)).

## Value

A list containing: - \`results\`: A data frame with columns
\`mean_difference\`, \`std_error\`, \`conf_low\`, and \`conf_high\`,
each rounded to 4 decimal places. Suitable for direct use in
reporting. - \`interpretation\`: A string providing a formatted
interpretation of the results.

## Examples

``` r
group1 <- list(vals = list(theta = 100, std.error = 10))
group2 <- list(vals = list(theta = 90, std.error = 5))
output <- compute_difference_means(group1, group2)
cat(output$interpretation)  # Print the interpretation
#> The difference in means is 10 with a standard error of 11.1803 and a 95% CI of [-11.9135, 31.9135].
```
