# End-to-end Policy Workflow (depth selection → report → interpretation)

Runs depth comparison, produces a mixed-depth policy summary report, and
(optionally) generates per-model interpretations. Designed as a one-call
wrapper for policymaker-facing reporting, with safe defaults that prefer
simple trees unless depth-2 gains are material.

## Usage

``` r
margot_policy_workflow(
  stability,
  original_df = NULL,
  label_mapping = NULL,
  se_method = c("plugin", "bootstrap"),
  R = 499L,
  dominance_threshold = 0.8,
  strict_branch = TRUE,
  min_gain_for_depth_switch = 0.005,
  include_interpretation = TRUE,
  audience = c("policy", "research"),
  use_coherent_in_interpret = TRUE,
  brief_include_group = FALSE,
  expand_acronyms = FALSE,
  show_neutral = NULL,
  prefer_stability = FALSE,
  signal_score = c("none", "pv_snr", "uplift_snr", "hybrid"),
  signals_k = 3,
  ...
)
```

## Arguments

- stability:

  A \`margot_stability_policy_tree\` object produced by
  \[margot_policy_tree_stability()\].

- original_df:

  Optional original-scale data (for scale annotations).

- label_mapping:

  Optional named list mapping outcome and variable names.

- se_method:

  Character; "plugin" (default) or "bootstrap".

- R:

  Integer; bootstrap reps when \`se_method = "bootstrap"\` (default
  499).

- dominance_threshold:

  Numeric in (0,1); required dominance share for a restricted policy
  recommendation (default 0.8 for policy mode).

- strict_branch:

  Logical; require positive uplift CI for dominant branch before
  recommending restricted deployment (default TRUE).

- min_gain_for_depth_switch:

  Numeric; minimum PV gain required to switch from depth-1 to depth-2
  (default 0.005 on standardized outcomes).

- include_interpretation:

  Logical; also run \[margot_interpret_policy_batch()\] with the
  selected depths (default TRUE).

- audience:

  Character; one of "policy" or "research" (default "policy").

- use_coherent_in_interpret:

  Logical; if TRUE, reuse PV rows from the coherent summary in
  interpretations instead of recomputing (default TRUE).

- brief_include_group:

  Logical; if TRUE, \`policy_brief_df\` includes the Group column
  (Wins/Neutral/Caution). If FALSE, returns a simplified brief without
  Group. Default FALSE for policy audiences.

- expand_acronyms:

  Logical; if TRUE, expand common acronyms in labels (e.g., RWA →
  Right-Wing Authoritarianism (RWA)). You may also supply custom
  expansions via \`options(margot.boilerplate.acronyms = list(SES =
  "Socioeconomic Status"))\`.

- show_neutral:

  Logical or NULL; controls inclusion of the Neutral group. Default NULL
  shows Neutral for research audiences and hides it for policy
  audiences. Set TRUE/FALSE to override explicitly.

- prefer_stability:

  Logical; if TRUE, raise the parsimony threshold for switching to
  depth-2 (min_gain_for_depth_switch ≥ 0.01) to prefer depth-1 unless
  depth-2 gains are clearly larger.

- signal_score:

  Character; one of "none", "pv_snr", "uplift_snr", or "hybrid". When
  not "none", the summary text includes a "Signals Worth Monitoring"
  section that ranks Neutral models by the selected score (magnitude
  relative to uncertainty, optionally weighted by coverage and
  stability). See the package NEWS for details.

- signals_k:

  Integer; number of top signals to display when \`signal_score !=
  "none"\`

- ...:

  Additional pass-through args to \[margot_policy_summary_report()\],
  e.g., \`split_compact\`, \`split_top_only\`, etc.

## Value

A list with components from depth selection (best), summary (summary),
and optional interpretations (interpret). Includes \`policy_brief_df\`
shortcut drawn from \`summary\$group_table_df\`. Also returns
\`method_explanation\` (long/short/prereg), \`depth_comparison_report\`
(a list with \`text\` and \`data\` showing both depth-1 and depth-2
policy values for all outcomes with selection rationale),
\`summary\$signals_df\` when \`signal_score != "none"\`, and top-level
\`report\`, \`report_prose\`, and \`report_detail\` for convenient
access to narrative summaries (bullets, prose, or detailed per-model
interpretations respectively).
