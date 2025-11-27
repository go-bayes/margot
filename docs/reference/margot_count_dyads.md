# count dyads in longitudinal data

count dyads in longitudinal data

## Usage

``` r
margot_count_dyads(
  dat,
  start_wave = 2009,
  end_wave = 2022,
  year_measured_val = 1,
  rel_id_var = "rel_num_l",
  complete_var = "rel_complete",
  prev_wave_counts = c(1, 2, 3, 4)
)
```

## Arguments

- dat:

  a data frame containing the longitudinal data.

- start_wave:

  integer. the first wave to process (default: 2009).

- end_wave:

  integer. the last wave to process (default: 2022).

- year_measured_val:

  integer. the value of 'year_measured' to filter on (default: 1).

- rel_id_var:

  character. the name of the variable indicating relationship dyad
  (default: "rel_num_l").

- complete_var:

  character. the name of the variable indicating complete dyad status
  (default: "rel_complete").

- prev_wave_counts:

  integer vector. previous wave thresholds to count (default:
  c(1,2,3,4)).

## Value

a tibble with columns for: - wave: survey wave number - n_total: total
number of dyads (complete + singleton) - n_single: number of singleton
dyads - n_complete: number of complete dyads - n_wave_1plus: dyads in 1+
previous waves - n_wave_2plus: dyads in 2+ previous waves -
n_wave_3plus: dyads in 3+ previous waves - n_wave_4plus: dyads in 4+
previous waves
