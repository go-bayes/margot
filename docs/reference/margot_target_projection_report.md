# Report target-population projection evidence

\`margot_target_projection_report()\` compares the realised source
sample with the registered target population before and after projection
weighting. It reports aggregate balance, projection-weight
concentration, and source representation across registered strata. The
function returns evidence and provenance without an accept-or-reject
classification.

## Usage

``` r
margot_target_projection_report(
  source,
  target,
  variables,
  projection_weights = NULL,
  target_weights = NULL,
  strata = NULL,
  harmonisation = NULL,
  projection_model = NULL,
  uncertainty = NULL,
  top_shares = c(0.01, 0.05, 0.1)
)
```

## Arguments

- source:

  A source-sample data frame.

- target:

  A target-population data frame or aggregate target microdata.

- variables:

  Character vector naming the registered projection variables present in
  both data frames.

- projection_weights:

  Optional non-negative source-sample projection weights. Equal weights
  represent an unweighted projection.

- target_weights:

  Optional non-negative target-population weights. Equal weights are
  used when this argument is \`NULL\`.

- strata:

  Optional subset of \`variables\` defining the registered overlap
  strata. The report describes target strata that lack source
  representation.

- harmonisation:

  Optional aggregate record of the source-to-target harmonisation rules.

- projection_model:

  Optional aggregate description of the projection model or
  supplied-weight construction.

- uncertainty:

  Optional aggregate description of projection-weight or target-margin
  uncertainty.

- top_shares:

  Fractions used for projection-weight concentration summaries.

## Value

An object of class \`margot_target_projection_report\` containing a
variable-level balance table, stratum representation table,
projection-weight summary, concentration table, and provenance. The
return value contains no participant-level rows.
