# Policy reporting and independent fixed-rule evaluation

Status: development specification. The function names below are proposed interfaces. This PR contains the development specification only.

## Purpose

Simple policy rules can communicate useful differences in exposure assignment and outcome benefit. Their reports should show the rule, the people assigned to each action, the outcome contrasts within its leaves and its value relative to a constant assignment. Each quantity needs an explicit population, scale and inferential scope.

The implementation has two separable parts: reporting from compatible stored results, and estimation of independently evaluated policy-value differences. Reporting can support an explicit unavailable-interval state while the independent-evaluation estimator is developed.

## Reuse the policy combo

Use `margot_plot_policy_combo()` through the existing `margot_plot_policy_tree_panels()` interface for the decision tree and participant projection. Retain the vertical arrangement. Both panels print the formatted outcome label, including any registered outcome reversal. Use separate, bold, left-aligned panel letters and headings.

The complete display places the taller tree and projection panels in full-width rows A and B, with C and D side by side below. Heights remain configurable. Preserve the individual components for other arrangements. A stability panel is outside this default display.

Represent the supplied display weight by point area. Retain exact horizontal predictor values, the tree's branch operators and original-scale cut points. Jitter only the nuisance vertical coordinate for a one-dimensional projection. Use the same resolved weight and reference records for the printed leaf shares. Separately report unweighted counts. Each reference participant appears once unless an explicitly labelled display-sampling option is used; printed shares always use all reference records. More complex trees require explicit correspondence between the branch conditions and each projected panel.

## Add two plots and complementary interpretations

| Proposed interface | Purpose |
|---|---|
| `margot_plot_policy_leaf_effects()` | Plot stored action contrasts within the identified leaves, with compatible intervals where available. |
| `margot_plot_policy_value_gain()` | Plot the stored rule-minus-comparator value difference, its compatible interval and resolved practical gain margin. |
| `margot_text_policy_leaf_effects()` | Interpret the same contrasts, leaf identities, scale, reference population and interval types. |
| `margot_text_policy_value_gain()` | Interpret the same comparator, gain, uncertainty and practical margin. |

Prefer extending `margot_report_policy_tree()` to compose these outputs with the existing combo. Add a thin plotting wrapper only where it improves the existing interface. Complement `margot_text_policy_tree()`, `margot_interpret_policy_tree()` and the batch interpreter by sharing validated tables and formatting helpers.

Reporting functions consume stored estimates. Model fitting, score reconstruction, interval estimation and repeated learning belong in explicit estimation functions, invoked before reporting.

## Shared result identity and interpretation

The reporting object records the outcome and formatted label, rule and leaf identities, comparator, evaluation mode, target or reference population, outcome scale and orientation, weight version, estimate, interval method and level, inferential scope, practical margin and any unavailable-result reason. Distinguish display records from evaluation records when they differ. Treat weights and outcome-scale metadata as explicit scientific inputs.

Validate these identities before combining panels. An independently evaluated fixed-rule report uses the same rule in A–D. A legacy report can combine a full-sample illustrative tree and its selected leaves in A–C with a repeated-learning procedure gain in D only when the separate targets are explicitly represented and described. Nominal intervals that treat selected full-sample leaves as fixed must retain that qualification. Independent evaluation requires a separate record of the learning and evaluation samples.

An interval belongs to its evaluated rule or procedure. Require agreement between the interval target and the displayed rule or procedure before attaching an interval. Missing or incompatible uncertainty remains explicitly unavailable. Partition variability is a separate descriptive result; variance estimation must account for participant reuse across folds and repeats.

Interpretations distinguish the point estimate, sampling interval and practical gain margin. For example, a constructed gain of 0.024 SD exceeds a margin of 0.01 SD, while an illustrative interval from 0.003 to 0.045 SD includes smaller gains. These stipulated fixtures illustrate reporting; coverage requires a separate simulation assessment. Describe a selected splitting variable as a component of an assignment rule; an effect of intervening on that variable requires a separate causal question and identification argument. A claim about differences between leaf effects requires a corresponding contrast and uncertainty. Constant assignment can remain optimal when benefit magnitudes differ across groups.

The depth and tree-versus-constant margins default to 0.01 standardised-outcome units, with explicit prospective overrides. Plots, tables and interpretations consume the resolved values. These margins are distinct from intervention costs and confidence bounds.

## Advantage of independent evaluation

A rule and its comparator can be learned using development participants and evaluated unchanged on separate participants. The interval then concerns the population value difference for that particular realised rule, conditional on its development sample. One development fit and one independent evaluation can support that comparison. Repeated tree learning is unnecessary for estimating its sampling uncertainty; it addresses the separate question of how the learning procedure changes under alternative development samples.

This advantage depends on an appropriate evaluation design and score. The estimator needs a justified influence representation for its target population and weighting, treatment of nuisance estimation, an appropriate independent sampling unit, adequate overlap and sufficiently small remaining bias. Cross-fitting and orthogonal scores can support asymptotic inference under their regularity conditions. Justify these conditions in addition to the sample split.

The tree, comparator, action orientation, margin and learning choices must be settled before evaluation outcomes are used. Selection of a replacement rule using evaluation outcomes requires an inference method that accounts for that selection. Full-data ATE estimation may remain separate; the independently evaluated learner uses development-only selection inputs.

The development/evaluation proportion, preprocessing and outcome-scale reference, observation-weight construction, exact interval estimator and handling of a selected policy remain implementation decisions requiring resolution. A 70/30 split remains a candidate. Preserve existing registered stored-score modes and defaults until a compatible prospective alternative is explicitly implemented and selected.

## Implementation sequence

1. Define a validated reporting object and adapt existing stored results while preserving their statistical meaning.
2. Implement the two standalone plots, their complementary text functions and the shared report assembly.
3. Add weighted projection support to the maintained combo, with consistent titles and panel tags at every supported depth.
4. Resolve the independent-evaluation score, weighting and variance specification before implementing the new estimator.
5. Validate that estimator against known policy values in focused simulations before recommending routine use.

Additional stability computation requires a separate purpose and justification. Existing saved summaries may inform qualified reporting. The prospective repeat count remains open.

## Acceptance checks

Reporting checks cover constant and nonconstant rules, reversed outcomes, unequal weights, exact split membership, unavailable and incompatible intervals, default and overridden margins, multiple outcomes and long labels. Verify numerical agreement among graph layers, tables and interpretations. Reject mismatched rule, population, scale and weight identities. Configure fitting and resampling functions to raise an error if called, and verify that reporting succeeds. Visually inspect a stump, a depth-two tree and the complete four-panel layout.

Independent-evaluation checks additionally cover participant or cluster separation, training-only transformations, invariant learned rules when evaluation outcomes are perturbed, exactly-once weighting, agreement of paired policy differences, identical-policy zero differences, covariance preservation and explicit treatment of selection. Assess coverage against the true value of each realised learned rule, bias, interval width and runtime in simulations with realistic overlap and weight concentration. Assess confidence-interval coverage separately from plotting and numerical reconstruction.

## Method references

- [GRF policy-learning guidance](https://grf-labs.github.io/grf/articles/policy_learning.html) explains policy trees and held-out estimation of leaf summaries.
- [Athey and Wager, Policy Learning With Observational Data](https://doi.org/10.3982/ECTA15732) develops policy learning using doubly robust scores.
- [Chernozhukov and colleagues, Double/Debiased Machine Learning](https://arxiv.org/abs/1608.00060) develops orthogonal-score inference and cross-fitting under regularity conditions.
