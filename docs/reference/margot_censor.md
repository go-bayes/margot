# Transform year_measured Variable Based on Clustered Conditions Within Waves

Transform year_measured Variable Based on Clustered Conditions Within
Waves

## Usage

``` r
margot_censor(
  dt,
  cluster_id = "rel_num",
  id_var = "id",
  wave_var = "wave",
  condition_var = "rel_complete",
  condition_value = 0,
  year_measured_var = "year_measured",
  censor_final_wave = FALSE
)
```

## Arguments

- dt:

  A data.frame or data.table in long format containing repeated
  measures.

- cluster_id:

  A string specifying the cluster identifier variable. Default is
  "rel_num".

- id_var:

  A string specifying the individual identifier variable. Default is
  "id".

- wave_var:

  A string specifying the variable indicating the wave. Default is
  "wave".

- condition_var:

  A string specifying the condition variable. Default is "rel_complete".

- condition_value:

  The value of condition_var that triggers transformation. Default is 0.

- year_measured_var:

  A string specifying the year_measured variable. Default is
  "year_measured".

- censor_final_wave:

  Logical. If TRUE, censoring is applied to the final wave; if FALSE
  (default), final wave is exempt.

## Value

A modified data.table with updated year_measured values based on the
condition.
