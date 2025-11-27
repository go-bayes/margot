# Apply Lead-Based Censoring to Longitudinal Data

Apply Lead-Based Censoring to Longitudinal Data

## Usage

``` r
margot_censor_lead(
  dt,
  cluster_id = NULL,
  id_var = "id",
  wave_var = "wave",
  condition_var,
  condition_value = 0,
  year_measured_var = "year_measured",
  cluster_condition = "ANY"
)
```

## Arguments

- dt:

  A data.frame or data.table in long format containing repeated
  measures.

- cluster_id:

  A string specifying the cluster identifier variable. If NULL, defaults
  to id_var.

- id_var:

  A string specifying the individual identifier variable. Default is
  "id".

- wave_var:

  A string specifying the variable indicating the wave. Default is
  "wave".

- condition_var:

  A string specifying the condition variable to create lead for.

- condition_value:

  The value of lead condition_var that triggers transformation. Default
  is 0.

- year_measured_var:

  A string specifying the year_measured variable. Default is
  "year_measured".

- cluster_condition:

  String specifying cluster censoring logic: "ANY" (default) or "ALL".

## Value

A modified data.table with updated year_measured values based on lead
conditions.
