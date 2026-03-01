# count individual participants in longitudinal data

count individual participants in longitudinal data

## Usage

``` r
margot_count_ids(
  dat,
  start_wave = 2009,
  end_wave = 2022,
  prev_wave_counts = c(1, 2, 3, 4),
  opt_in_var = "sample_frame_opt_in",
  track_opt_ins = NULL,
  opt_in_true = 1,
  opt_in_false = 0,
  strata_var = NULL,
  strata_levels = NULL,
  strata_filter = NULL,
  strata_filter_scope = c("auto", "all_counts", "strata_only")
)
```

## Arguments

- dat:

  a data frame containing the longitudinal data

- start_wave:

  integer. the first wave to process (default: 2009)

- end_wave:

  integer. the last wave to process (default: 2022)

- prev_wave_counts:

  integer vector. previous wave thresholds to count (default:
  c(1,2,3,4))

- opt_in_var:

  character. name of the opt-in variable to track (default:
  "sample_frame_opt_in")

- track_opt_ins:

  logical. whether to track opt-in counts (default: NULL uses opt_in_var
  presence)

- opt_in_true:

  value indicating opted-in status (default: 1)

- opt_in_false:

  value indicating not opted-in status (default: 0)

- strata_var:

  character. optional name of a variable used to stratify active counts
  (default: NULL)

- strata_levels:

  character. optional levels to include for stratification (default:
  NULL uses all levels in data)

- strata_filter:

  character. optional levels used to focus stratified output (default:
  NULL)

- strata_filter_scope:

  character. whether strata_filter applies to all counts ("all_counts"),
  only stratified columns ("strata_only"), or automatic ("auto",
  default). in auto mode, strata_filter applies to all counts when
  supplied, and leaves totals unchanged when strata_filter is NULL.

## Value

a tibble with columns for: - wave: survey wave number - n_total:
cumulative unique participants through current wave - n_active: active
participants in current wave - n_deceased: newly deceased in current
wave - n_deceased_total: total deceased through current wave -
n_returned: participants absent in previous wave but present in earlier
waves - n_returned_total: total returnees through current wave -
n_opt_in: newly opted-in participants in current wave - n_opt_in_total:
total opted-in participants through current wave - n_wave_1plus:
participants in 1+ previous waves - n_wave_2plus: participants in 2+
previous waves - n_wave_3plus: participants in 3+ previous waves -
n_wave_4plus: participants in 4+ previous waves -
n_active_strata_var_level: active participants in current wave for each
stratum (if stratified)
