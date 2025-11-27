# Generate summary tables and plots for longitudinal data

Generate summary tables and plots for longitudinal data

## Usage

``` r
margot_summary_tables(
  data,
  baseline_wave,
  exposure_waves,
  outcome_wave,
  name_exposure,
  name_exposure_cat = NULL,
  baseline_vars = NULL,
  outcome_vars = NULL,
  extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
  baseline_labels = NULL,
  exposure_labels = NULL,
  outcome_labels = NULL,
  create_plots = FALSE,
  plot_type = "boxplot",
  show_progress = TRUE
)
```

## Arguments

- data:

  A data frame containing the longitudinal data

- baseline_wave:

  The wave number for baseline measurements

- exposure_waves:

  A vector of wave numbers for exposure measurements

- outcome_wave:

  The wave number for outcome measurements

- name_exposure:

  The name of the exposure variable

- name_exposure_cat:

  The name of the categorical exposure variable (optional)

- baseline_vars:

  A vector of baseline variable names

- outcome_vars:

  A vector of outcome variable names

- extra_vars:

  A vector of additional variable names (default: c("id", "wave",
  "year_measured", "not_lost", "sample_weights"))

- baseline_labels:

  A named vector of labels for baseline variables (optional)

- exposure_labels:

  A named vector of labels for exposure variables (optional)

- outcome_labels:

  A named vector of labels for outcome variables (optional)

- create_plots:

  Logical, whether to create plots (default: FALSE)

- plot_type:

  The type of plot to create ("boxplot" or "density", default:
  "boxplot")

- show_progress:

  Logical, whether to show a progress bar (default: TRUE)

## Value

A list containing summary tables, information, and optional plots
