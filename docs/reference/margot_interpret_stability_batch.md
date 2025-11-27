# Batch Interpret Policy Tree Stability Results

Processes multiple models from a stability analysis and provides
narrative interpretations for each. Returns a named list of
interpretations suitable for batch reporting.

## Usage

``` r
margot_interpret_stability_batch(
  object,
  model_names = NULL,
  depth = 2,
  stability_threshold = 0.7,
  format = c("text", "technical"),
  decimal_places = 1,
  include_theory = TRUE,
  label_mapping = NULL,
  verbose = TRUE,
  combine = FALSE,
  save_to_file = NULL,
  include_ci = FALSE,
  ci_level = 0.95
)
```

## Arguments

- object:

  Object of class "margot_stability_policy_tree"

- model_names:

  Optional character vector of model names to interpret. If NULL
  (default), interprets all models in the results.

- depth:

  Tree depth to interpret (1, 2, or "both")

- stability_threshold:

  Minimum frequency to consider a split "stable" (default 0.7)

- format:

  Output format: "text" for narrative prose or "technical" for detailed
  statistics

- decimal_places:

  Number of decimal places for statistics (default 1)

- include_theory:

  Logical: Include theoretical context about tree instability (default
  TRUE). Note: theory is only included for the first model to avoid
  repetition.

- label_mapping:

  Optional named list mapping variable names to labels

- verbose:

  Logical: Print progress messages (default TRUE)

- combine:

  Logical: If TRUE, combines all interpretations into a single text
  (default FALSE)

- save_to_file:

  Optional file path to save combined interpretations as text file

- include_ci:

  Logical: If TRUE and format = "technical", include simple CIs in
  output

- ci_level:

  Numeric: Confidence level for intervals (default 0.95)

## Value

Named list where each element contains the interpretation for a model.
If combine = TRUE, returns a single character string with all
interpretations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interpret all models
all_interpretations <- margot_interpret_stability_batch(
  stability_results,
  format = "technical",
  stability_threshold = 0.5,
  label_mapping = label_mapping_all
)

# Interpret specific models only
selected_interpretations <- margot_interpret_stability_batch(
  stability_results,
  model_names = c("model_depression", "model_anxiety"),
  format = "text"
)

# Get combined output for reporting
combined_text <- margot_interpret_stability_batch(
  stability_results,
  combine = TRUE,
  save_to_file = "stability_interpretations.txt"
)
} # }
```
