# Generate QINI Curves and Difference Gain Summaries

Computes QINI curves and difference gain summaries for causal forest
models. This function provides a direct way to generate QINI results
without running full policy analysis, paralleling the functionality of
margot_rate().

## Usage

``` r
margot_qini(
  models,
  model_names = NULL,
  spend_levels = 0.1,
  baseline_method = "maq_no_covariates",
  label_mapping = NULL,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  verbose = TRUE,
  seed = 12345,
  treatment_cost = 1
)
```

## Arguments

- models:

  List returned by margot_causal_forest(), containing results and
  optionally full_models.

- model_names:

  Optional character vector specifying which models to process. Default
  NULL (all models).

- spend_levels:

  Numeric vector of spend levels for difference gain summaries. Default
  is 0.1 (10 percent spend captures early heterogeneity patterns
  effectively).

- baseline_method:

  Method for generating baseline: "maq_no_covariates" (default), "auto",
  "simple", "maq_only", or "none". See margot_generate_qini_data() for
  details.

- label_mapping:

  Named character vector for converting variable names to readable
  labels.

- remove_tx_prefix:

  Logical; remove treatment prefix from variable names (default TRUE).

- remove_z_suffix:

  Logical; remove z-score suffix from variable names (default TRUE).

- use_title_case:

  Logical; convert variable names to title case (default TRUE).

- remove_underscores:

  Logical; replace underscores with spaces (default TRUE).

- verbose:

  Logical; print progress messages (default TRUE).

- seed:

  Integer; base seed for reproducible computations (default 12345).

- treatment_cost:

  Scalar treatment cost per unit. Default 1. Lower values (e.g., 0.2)
  represent cheap treatments; higher values (e.g., 5) represent
  expensive treatments. Affects QINI curve shape - lower costs create
  steeper curves, higher costs create shallower curves.

## Value

A list where each element corresponds to a model and contains:
qini_objects (maq objects for CATE and baseline curves), qini_data
(data.frame with proportion, gain, and curve columns for plotting),
diff_gain_summaries (list of difference gain summaries at each spend
level), model_name (the processed model name).

## Details

This function generates QINI curves on-demand using
margot_generate_qini_data(). For binary treatments, it creates both CATE
and baseline (e.g., ATE) curves. The difference gain summaries quantify
how much better CATE-based targeting performs compared to the baseline
at specified spend levels.

The treatment_cost parameter allows for cost sensitivity analysis
without rerunning the causal forest models. Lower costs (e.g., 0.2)
create steeper QINI curves indicating more people can be treated
cost-effectively, while higher costs (e.g., 5) create shallower curves
indicating only the highest-effect individuals justify treatment.

Cost Invariance Property: When treatment costs are uniform across
individuals, the relative benefit of CATE-based targeting over uniform
allocation remains constant regardless of cost level. While absolute
gains scale inversely with cost (gain at cost c = gain at cost 1 / c),
the difference between CATE and ATE curves scales proportionally,
preserving the relative advantage of personalized treatment. This means
the value of heterogeneous treatment effects for targeting decisions is
independent of the uniform cost level.

The output is structured to be compatible with margot_interpret_qini()
and other QINI visualization functions.
