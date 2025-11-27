# Create Tau Hat Plot

Creates a histogram plot of tau hat values for each treatment
comparison.

## Usage

``` r
create_tau_hat_plot(tau_hat, outcome)
```

## Arguments

- tau_hat:

  A matrix of estimated treatment effects.

- outcome:

  A character string specifying the name of the outcome variable.

## Value

A ggplot object representing the distribution of tau hat values.
