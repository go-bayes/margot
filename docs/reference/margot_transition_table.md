# Format a Transition Table with observed‑indicator filtering

Reshapes a long‑format transition frequency data frame into a
wide‑format table with totals, and formats it using markdown. Diagonal
elements are bolded to highlight state stability.

## Usage

``` r
margot_transition_table(
  data,
  state_var,
  id_var,
  wave_var,
  waves = NULL,
  state_names = NULL,
  observed_var = NULL,
  observed_val = 1,
  table_name = "transition_table"
)
```

## Arguments

- data:

  a data frame containing your id, state, wave, and (optionally) an
  observed indicator.

- state_var:

  name of the column indicating the state at each wave.

- id_var:

  name of the column identifying each participant.

- wave_var:

  name of the column indicating the wave (numeric or factor).

- waves:

  optional numeric vector of waves to include (defaults to all present
  in data).

- state_names:

  optional character vector of labels for each state.

- observed_var:

  optional name of a column marking participants still observed.

- observed_val:

  value in `observed_var` that denotes “observed” (default 1).

- table_name:

  name for the output object (default "transition_table").

## Value

an object of class `margot_transitions` with `tables`
(markdown-formatted matrices), paired `tables_data` (the underlying
numeric data frames), `explanation`, `wave_info`, and a `quarto_code`
helper. The returned object also includes convenience functions
`$get_table_data()` and `$compute_ipsi_probabilities()` to retrieve the
raw counts or to run
[`margot_compute_ipsi_probability()`](https://go-bayes.github.io/margot/reference/margot_compute_ipsi_probability.md)
directly from each table.

## Examples

``` r
dt <- data.frame(
  id = rep(1:3, each = 3),
  wave = rep(c(2018, 2019, 2022), times = 3),
  religion_church_binary = c(0, 0, 1,
                             0, 1, 1,
                             1, 1, 1),
  year_measured = 1
)

transitions <- margot_transition_table(
  dt,
  state_var = "religion_church_binary",
  id_var = "id",
  wave_var = "wave",
  observed_var = "year_measured",
  observed_val = 1,
  waves = c(2018, 2019, 2022)
)

# Extract machine-readable counts for the first transition
transitions$get_table_data(which = 1)
#> # A tibble: 2 × 4
#>   `From / To` `State 0` `State 1` Total
#>   <chr>           <int>     <int> <dbl>
#> 1 State 0             1         1     2
#> 2 State 1             0         1     1

# Compute IPSI counterfactual initiation probabilities for the first transition
ipsi_wave1 <- transitions$compute_ipsi_probabilities(which = 1)
ipsi_wave1$probabilities
#>   delta delta_inverse natural_p natural_p_l natural_p_u counterfactual_p
#> 1     2           0.5       0.5  0.01257912   0.9874209             0.75
#> 2     5           0.2       0.5  0.01257912   0.9874209             0.90
#> 3    10           0.1       0.5  0.01257912   0.9874209             0.95
#>   fold_increase
#> 1           1.5
#> 2           1.8
#> 3           1.9
```
