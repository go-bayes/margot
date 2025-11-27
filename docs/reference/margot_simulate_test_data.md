# Simulate Test Data for margot Development Functions

Creates a consistent synthetic dataset for testing the development
versions of margot functions. Generates panel data with known treatment
effects to enable validation of causal inference methods.

## Usage

``` r
margot_simulate_test_data(
  n = 5000,
  k = 3,
  p_covars = 20,
  treatment_effects = list(Y1 = "positive", Y2 = "negative", Y3 = "null", Y4 =
    "heterogeneous"),
  missing_prop = 0.1,
  binary_outcomes = c(FALSE, FALSE, FALSE, TRUE),
  censoring_rate = 0.1,
  seed = 2025
)
```

## Arguments

- n:

  Integer. Number of individuals (default: 5000)

- k:

  Integer. Number of time points including baseline (default: 3)

- p_covars:

  Integer. Number of baseline covariates (default: 20)

- treatment_effects:

  Named list specifying treatment effect patterns for each outcome.
  Options: "positive", "negative", "null", "heterogeneous". Default
  creates 4 outcomes with different patterns.

- missing_prop:

  Numeric. Proportion of missing data to introduce (default: 0.1)

- binary_outcomes:

  Logical vector indicating which outcomes should be binary (default:
  c(FALSE, FALSE, FALSE, TRUE))

- censoring_rate:

  Numeric. Rate of censoring/attrition (default: 0.1)

- seed:

  Integer. Random seed for reproducibility (default: 2025)

## Value

A list containing:

- data:

  Data frame with all variables in wide format

- true_effects:

  List of true treatment effect functions

- metadata:

  List with generation parameters and variable information

## Details

The function creates: - Baseline covariates (B1-Bp) from multivariate
normal - Binary treatment (A) with propensity depending on covariates -
Multiple outcomes (Y1-Y4) with different treatment effect patterns: -
Y1: Positive homogeneous effect (ATE = 0.3) - Y2: Negative homogeneous
effect (ATE = -0.2) - Y3: Null effect (ATE = 0) - Y4: Strong
heterogeneous effect based on B1 and B2

Treatment assignment depends on B1 and B2 to create confounding. Missing
data is introduced randomly (MCAR) for testing robustness.

## Examples

``` r
# Generate default test dataset
test_data <- margot_simulate_test_data()

# Generate with more observations and covariates
test_data_large <- margot_simulate_test_data(n = 10000, p_covars = 30)

# Generate with custom treatment effects
test_data_custom <- margot_simulate_test_data(
  treatment_effects = list(
    Y1 = "heterogeneous",
    Y2 = "positive",
    Y3 = "heterogeneous"
  )
)
```
