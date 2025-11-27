# Create Simple QINI Baseline Object

Creates a simple baseline object that represents the expected gain under
random treatment allocation. This serves as a robust fallback when maq
fails to generate a baseline.

## Usage

``` r
margot_qini_simple_baseline(
  mean_tau,
  n_points = 100,
  n_units = NULL,
  treatment_cost = 1,
  x_axis = "proportion"
)
```

## Arguments

- mean_tau:

  The average treatment effect (mean of tau_hat)

- n_points:

  Number of points for the gain curve (default 100)

- n_units:

  Total number of units (for metadata)

- treatment_cost:

  Scalar treatment cost per unit (default 1)

- x_axis:

  Type of x-axis: "proportion" (default) or "budget"

## Value

A list mimicking maq output structure with: - \_path: list with gain,
spend, and std.err - mean_tau: the average treatment effect -
treatment_cost: the treatment cost used - baseline_type: "simple" -
class: "qini_simple_baseline"
