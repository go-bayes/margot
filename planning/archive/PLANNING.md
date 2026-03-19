# PLANNING.md

1. This document consolidates the current architecture plan and the log-transformation back-calculation work, with a focus on robust, modular causal workflows and clear output on the original scale.

2. The development architecture aims to replace the monolithic workflow with modular functions that preserve proper train and test separation. The completed pieces include the dev simulators, the dev causal forest workflow, and the dev QINI pipeline, with plotting integrated into the dev QINI outputs. The pending core pieces are the heterogeneity, policy tree, RATE, and flipped forest components, followed by the related plotting and subgroup utilities, and then the remaining binding and diagnostic helpers. The key principles remain modularity, integration of related steps, consistent interfaces, honest evaluation, and backwards compatibility.

3. The transformation metadata plan will embed outcome transformation details directly in model objects when save_data is enabled, so interpretation functions no longer rely on external original data. The detection routine should identify z and log transformations, find the original variable, and store original and transformed means and standard deviations, including the log offset. The stability pipeline should preserve this metadata so downstream interpretation has access to the same parameters, and the interpretation functions should prefer embedded metadata when present and fall back to the original_df interface for older models.

4. The log-scale back-calculation should be updated to treat log effects as additive on the log scale and multiplicative on the original scale. The calculation should use the standardised effect and the log-scale standard deviation, then report both the multiplicative change and the implied absolute change using the mean on the original scale. A simple reference implementation is shown below.

```r
delta_log <- treatment_effect * sd_log
ratio <- exp(delta_log)
pct_change <- (ratio - 1) * 100
abs_change <- mean_dollars * (ratio - 1)
```

5. The implementation should proceed in phases, starting with an update to the leaf-mean calculations to apply the multiplicative interpretation. The next phase should store population statistics as attributes at transformation time so that subset data do not distort the back-transformation, and it should warn when subset statistics differ materially from the stored population values. The final phase should add weighted variants of the transformation statistics to account for sample weights and IPTWs, with a user-facing choice between weighted and unweighted summaries.

6. The output should prioritise clarity by reporting the CATE on the standardised scale alongside a multiplicative change and an approximate absolute change on the original scale. This presentation aligns with common econometric practice and avoids the bias that comes from separately transforming treatment and control means.
