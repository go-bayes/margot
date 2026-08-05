# Batch Process LMTP Models

This function runs multiple Longitudinal Modified Treatment Policy
(LMTP) models for specified outcome variables, calculates contrasts,
creates evaluation tables, and optionally saves checkpoints and the
complete output as \`.rds\` files.

## Usage

``` r
margot_lmtp(
  data,
  outcome_vars = NULL,
  trt = NULL,
  shift_functions = list(),
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  lmtp_defaults = list(),
  n_cores = parallel::detectCores() - 1,
  models_in_parallel = NULL,
  cv_workers = NULL,
  save_output = FALSE,
  save_path = here::here("push_mods"),
  base_filename = "lmtp_output",
  use_timestamp = FALSE,
  prefix = NULL,
  manage_future_plan = FALSE,
  progress = c("cli", "progressr", "none"),
  seed = NULL,
  reuse_density_ratios = FALSE,
  estimator_spec = NULL
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modelled. Optional
  when \`estimator_spec\` is supplied, which locks it.

- trt:

  A character string specifying the treatment variable. Optional when
  \`estimator_spec\` is supplied, which locks it.

- shift_functions:

  A list of shift functions to be applied. Each function should take
  \`data\` and \`trt\` as arguments.

- include_null_shift:

  Logical, whether to include a null shift. Default is TRUE.

- lmtp_model_type:

  The LMTP model function to use. Default is lmtp_tmle.

- contrast_type:

  Type of contrasts to compute: "pairwise" or "null". Default is
  "pairwise".

- contrast_scale:

  Scale for contrasts: "additive", "rr", or "or". Default is "additive".

- lmtp_defaults:

  A list of default parameters for the LMTP models. Must be empty when
  \`estimator_spec\` is supplied, which builds the whole list from the
  specification.

- n_cores:

  Total number of CPU cores to budget for the batch run. Default is
  detectCores() - 1 (includes efficiency cores on Apple Silicon, so set
  manually if you want to cap at performance cores).

- models_in_parallel:

  Optional cap on how many LMTP models to run at once. Defaults to
  floor(n_cores / cv_workers).

- cv_workers:

  Number of workers consumed internally by each LMTP fit (usually the
  cross-validation folds). Defaults to future::nbrOfWorkers().

- save_output:

  Logical, whether to save per-model checkpoints and the complete
  output. Saved artefacts are written as \`.rds\` files. Default is
  FALSE.

- save_path:

  The directory path to save the output. Default is "push_mods" in the
  current working directory.

- base_filename:

  The base filename for saving the output. Default is "lmtp_output".

- use_timestamp:

  Logical, whether to include a timestamp in the filename. Default is
  FALSE.

- prefix:

  Optional prefix to add to the saved output filename. Default is NULL.

- manage_future_plan:

  Logical, whether to manage the future plan internally for nested
  parallelization. Default is FALSE. When TRUE, margot_lmtp sets up
  nested futures (outer loop for models, inner loop for CV) and
  automatically cleans up workers on exit. When FALSE, models run
  sequentially but can use the user's external future::plan() for
  parallel CV.

- progress:

  Progress reporting method: "cli" (default CLI progress bar),
  "progressr" (use progressr package handlers), or "none" (no progress
  reporting).

- seed:

  Optional single whole number seeding every stochastic step: the RNG at
  entry, each model fit, and the parallel streams. Default NULL leaves
  the RNG untouched. When \`estimator_spec\` is supplied the seed comes
  from the locked specification, and supplying a different one errors.

- reuse_density_ratios:

  Logical. When \`TRUE\`, sequentially doubly robust fits sharing one
  policy-specific nuisance identity fit the treatment and censoring
  density ratios once and reuse them across \`outcome_vars\`. The
  returned models, contrasts, and tables retain the existing Margot
  structure. Default is \`FALSE\` while the opt-in path is validated.

- estimator_spec:

  Optional locked \`margot_lmtp_estimator_spec\` object from
  \[margot_lmtp_estimator_spec()\]. When supplied, the \`lmtp\` call is
  built from the specification and every conflicting user argument
  errors.

## Value

A list containing:

- models:

  A list of all LMTP models for each outcome and shift function.

- contrasts:

  A list of contrasts computed for each outcome.

- individual_tables:

  A list of individual tables for each contrast and outcome.

- combined_tables:

  A list of combined tables for each contrast type across all outcomes.

## Details

For very large datasets or models with many time points, parallel
processing may not improve performance as much as expected. This is
because LMTP models can be memory-bound rather than CPU-bound when
working with large data. In such cases, memory pressure and data copying
between workers may offset the benefits of parallelization. Consider
using fewer cores or sequential processing for very large models if you
experience performance degradation.

## Design and execution

\`margot_lmtp()\` executes an LMTP analysis. A study's causal question,
causal estimand, identification assumptions, policy rationale, and
decision rules belong in its protocol rather than this software call.
Keeping those design commitments outside the estimator prevents a later
computational improvement from changing the scientific workflow.

Supplying \`estimator_spec\` locks the execution settings. The \`lmtp\`
call is then built from the specification's \`call_arguments\` — the
exposure at each node, the baseline and time-varying covariates, the
censoring and competing-event indicators, the outcome and its model, the
identifier, the folds, the bounds, the registered learner library, the
analysis-weight column, and the cap — and any conflicting user argument
errors with a condition of class
\`margot_error_estimator_spec_conflict\` that names the conflict. The
specification supplies the whole \`lmtp_defaults\` list, so any entry
supplied alongside it — one the specification fixes, or one the derived
list would drop — raises that condition rather than passing in silence.
Margot re-verifies the specification's content hash on entry, so an
object edited after creation is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders

# Define shift functions
gain_function <- function(data, trt) {
  data[[trt]] + 1
}

loss_function <- function(data, trt) {
  pmax(data[[trt]] - 1, 0)
}

# Run LMTP analysis
result <- margot_lmtp(
  data = my_data,
  outcome_vars = c("outcome1", "outcome2"),
  trt = "treatment",
  shift_functions = list(gain = gain_function, loss = loss_function),
  lmtp_defaults = list(
    baseline = c("confounder1", "confounder2"),
    time_vary = c("time_var1", "time_var2"),
    outcome_type = "continuous"
  ),
  save_output = TRUE,
  save_path = here::here("output", "lmtp_results"),
  prefix = "my_study"
)
} # }
```
