# Run Multiple Generalised Random Forest (GRF) Causal Forest Models in Parallel

Parallelised, diagnostic‑rich variant of \`margot_causal_forest()\`.
Each outcome‑specific forest is estimated in its own R worker via
\*\*future\*\*. All the \`cli\` messages and checks from the sequential
original are preserved, so you still get the same granular reporting
(dimension checks, Qini status, warnings, etc.). Live progress bars are
emitted with \*\*progressr\*\* using a \`cli\` handler.

## Usage

``` r
margot_causal_forest_parallel(
  data,
  outcome_vars,
  covariates,
  W,
  weights,
  grf_defaults = list(),
  save_data = FALSE,
  compute_rate = TRUE,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.5,
  qini_split = TRUE,
  train_prop = 0.5,
  qini_train_prop = NULL,
  compute_conditional_means = TRUE,
  n_cores = future::availableCores() - 1,
  verbose = TRUE,
  qini_treatment_cost = 1,
  seed = 12345
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modelled.

- covariates:

  A matrix of covariates to be used in the GRF models.

- W:

  A vector of binary treatment assignments.

- weights:

  A vector of weights for the observations.

- grf_defaults:

  A list of default parameters for the GRF models.

- save_data:

  Logical indicating whether to save data, covariates, and weights.
  Default is FALSE.

- compute_rate:

  Logical indicating whether to compute RATE for each model. Default is
  TRUE. Note: Direct computation of RATE, QINI, and policy trees within
  this function may be deprecated in future versions. Use margot_rate(),
  margot_qini(), and margot_policy_tree() instead.

- top_n_vars:

  Integer specifying the number of top variables to use for additional
  computations. Default is 15.

- save_models:

  Logical indicating whether to save the full GRF model objects. Default
  is TRUE.

- train_proportion:

  Numeric value between 0 and 1 indicating the proportion of non-missing
  data to use for training policy trees. Default is 0.5.

- qini_split:

  Logical indicating whether to do a separate train/test split
  exclusively for the Qini calculation. Default is TRUE (i.e., Qini is
  computed out-of-sample).

- train_prop:

  Proportion of data to use for the training set when qini_split=TRUE.
  Default is 0.5.

- qini_train_prop:

  Deprecated. Use train_prop instead. If provided, will override
  train_prop with a warning.

- compute_conditional_means:

  Logical indicating whether to compute conditional means using
  [`policytree::conditional_means()`](https://rdrr.io/pkg/policytree/man/conditional_means.html).
  These represent expected outcomes under each treatment arm. Default is
  TRUE.

- n_cores:

  integer. number of parallel workers (default = all cores − 1).

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

- qini_treatment_cost:

  Scalar treatment cost per unit for QINI calculations. Default 1. Lower
  values (e.g., 0.2) represent cheap treatments creating steeper QINI
  curves; higher values (e.g., 5) represent expensive treatments
  creating shallower curves.

- seed:

  Integer. Random seed for reproducibility of train/test splits for
  policy trees and QINI evaluation. Default is 12345.

## Value

list with elements: \* \`results\` – per‑outcome diagnostics and objects
\* \`combined_table\` – rbind‑ed e‑value table across outcomes \*
\`outcome_vars\` – vector of (successful) outcome names \*
\`not_missing\` – indices of complete‑case rows \* (\`data\`,
\`covariates\`, \`weights\`, \`W\`) when \`save_data = TRUE\` \*
\`full_models\` when \`save_models = TRUE\`

## Details

Messages produced inside workers are captured by \*\*future\*\* and
dispatched to the master session. Progress bars update in real time. To
silence progress, call \`progressr::handlers("off")\` before running.
