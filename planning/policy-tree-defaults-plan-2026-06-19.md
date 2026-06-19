# Policy tree defaults and stability plan

## Summary

The GRF policy-tree workflow should default to descriptive, stable summaries rather than implied welfare-maximising policy claims. The standard path fits the causal forest and displayed policy tree on all complete cases, prefers depth-1 unless depth-2 is materially better, reports bootstrap tree stability, and keeps inferential heterogeneity evidence in opt-in RATE cross-validation or explicit held-out diagnostics.

## Workflow schematic

```text
Data
  |
  v
GRF fit on complete cases
  |-- OOB CATE / DR scores -> descriptive full-data policy tree
  |                         -> depth-1 default display
  |                         -> depth-2 only if materially better + stable
  |
  |-- bootstrap rows -> displayed-tree stability
  |                    variable frequency / threshold spread / action agreement
  |
  |-- repeated train/test splits -> policy-learning diagnostic
  |                                train tree on train rows
  |                                evaluate summaries on test rows
  |
  `-- sequential RATE CV -> inferential heterogeneity evidence
```

## Implementation choices

- `margot_causal_forest()` defaults to descriptive mode: `use_train_test_split = FALSE`, `compute_rate = FALSE`, no default QINI, and no in-sample RATE claim.
- GRF regularisation defaults are `num.trees = 5000` and `min.node.size = 50`. For typical `n > 30000` applications, `min.node.size = 100` is a recommended sensitivity analysis rather than the default.
- `margot_policy_tree_stability()` defaults to row bootstrap. In bootstrap mode the policy tree is fit on the full bootstrap sample; train/test splitting is reserved for `split_only` and `both`.
- Depth-2 must improve policy value by at least `0.01` outcome-standard-deviation units and must not reduce bootstrap consensus strength by more than `0.05`. Otherwise depth-1 is selected.
- QINI curves remain exploratory and require explicit opt-in when the input object came from descriptive mode.
- Methods and report text must distinguish descriptive evaluation slices from held-out policy evaluation.

## Tests and acceptance criteria

- Descriptive defaults use all complete cases for policy-tree fitting and evaluation metadata, while RATE and QINI are recorded as not computed.
- Bootstrap stability resamples full rows without an implicit half-sample split.
- Depth selection keeps depth-1 below the `0.01` material-gain threshold and when depth-2 loses more than `0.05` consensus strength.
- `margot_qini()` refuses descriptive in-sample regeneration unless `allow_in_sample = TRUE`.
- Flip workflows inherit `use_train_test_split` from saved computation parameters and do not revive placeholder RATE objects.
- Documentation is regenerated and `NEWS.md` records the behavioural change.

## Tutorial seed

The tutorial should teach three distinct claims rather than treating every policy-tree output as a held-out welfare estimate:

1. **Descriptive default:** Fit the GRF and the displayed policy tree on all complete cases. Interpret the tree as a compact description of estimated treatment-effect heterogeneity, not as proof that deploying the tree would improve welfare.
2. **Stability diagnostic:** Use `margot_policy_tree_stability()` to ask whether the displayed splits survive row perturbations. Treat low consensus strength, frequent split-variable changes, or wide threshold spread as evidence to prefer a simpler tree or to report the policy tree cautiously.
3. **Inferential heterogeneity evidence:** Use `margot_rate_cv()` when the manuscript needs a formal statement about prioritisation, because RATE/AUTOC has a worked-out cross-validation protocol in GRF.

A natural tutorial flow is:

```r
fit <- margot_causal_forest(
  data = analysis_data,
  outcome_var = "outcome",
  treatment_var = "treatment",
  covariates = covariates,
  policy_tree_vars = policy_vars
)

stability <- margot_policy_tree_stability(fit)
workflow <- margot_policy_workflow(stability)
split_diag <- margot_policy_split_diagnostic(fit, n_splits = 50)
rate <- margot_rate_cv(fit)
```

The worked example should show how the default report chooses depth-1 unless depth-2 clears the material-gain threshold (`0.01`) and does not lose too much consensus strength (`0.05`). It should then show when to escalate from the descriptive tree to a repeated split diagnostic or RATE: unstable split variables, depth-2 only marginally better than depth-1, or a manuscript claim that depends on prioritising one group over another.

The tutorial should avoid saying "out-of-sample policy value" for the default workflow. Use "complete-case descriptive policy tree", "bootstrap stability", "repeated held-out split diagnostic", and "RATE cross-validation" as the four user-facing labels.

## Graphing and reporting workflow

Graphing and reporting should enter after the fitted GRF object and the bootstrap stability object exist. The analysis layer estimates the forest and the descriptive tree; the reporting layer decides what, if anything, is worth showing in a manuscript.

```text
margot_causal_forest()
  |
  v
margot_policy_tree_stability(vary_type = "bootstrap")
  |
  |-- margot_policy_workflow()
  |     |-- margot_policy_summary_compare_depths()
  |     |-- margot_policy_summary_report()
  |     |-- margot_policy_methods_statement() / method_explanation
  |     `-- optional plots via margot_policy()
  |
  |-- direct figures:
  |     |-- margot_plot_policy_combo()
  |     |-- margot_plot_decision_tree()
  |     `-- margot_plot_policy_tree()
  |
  `-- optional diagnostics:
        |-- margot_policy_split_diagnostic()
        `-- margot_rate_cv()
```

For tutorials, use `margot_policy_workflow()` as the high-level reporting route. It returns the selected depth, a depth-comparison table, prose/report text, methods text, a policy-brief table, and optional plot objects. Use direct plotting functions only when the manuscript needs one named outcome/wave figure, as in the ARA earthquake manuscript.

The graphing gate should be descriptive-first. A tree is appropriate for the main text when it helps readers inspect the estimated heterogeneity surface and its bootstrap stability is acceptable. A tree should move to the supplement, or be omitted, when split variables change often, the depth-2 gain is below `0.01`, depth-2 loses more than `0.05` consensus strength, or the substantive claim would require deployment-like policy value. `margot_select_grf_policy_trees()` can still help triage manuscript figures, but its criteria should be revised later so graphing depends on selected depth plus stability, not only policy-value confidence limits.

Manuscript setup files should load saved reporting objects rather than rerunning estimation. The target saved objects are:

- `forest_results`: fitted GRF objects and descriptive policy-tree metadata.
- `policy_stability`: bootstrap stability output.
- `policy_workflow`: depth choice, report text, method text, policy-brief table, and optional plots.
- `policy_split_diagnostic`: optional repeated held-out split diagnostic.
- `rate_cv`: optional RATE/AUTOC inferential heterogeneity evidence.

The ARA earthquake study (`epic-pubs/2026/26-ara-eq`) is the stale reference to revise. Its current manuscript and SI already separate main-text interpretation from SI diagnostics, but they still use held-out policy-value language. The tutorial should use ARA as a "before and after" example: keep the cautious conclusion, replace held-out-default language with complete-case descriptive tree language, and pair any displayed tree with bootstrap stability.
