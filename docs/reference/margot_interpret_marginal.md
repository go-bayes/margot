# Make Interpretation of ATE Results

Helper that assembles a concise Markdown interpretation of the results.
By default it lists every outcome whose confidence interval excludes the
null, in plot order from top to bottom, with its E-value bound and
E-value flag, and states the correction and confidence level. When
\`include_adjust_note = FALSE\` (the default for a single-outcome call
from \`margot_plot()\`), the correction is not mentioned; the confidence
level is still stated.

## Usage

``` r
margot_interpret_marginal(
  df,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  original_df = NULL,
  e_val_bound_threshold = 1,
  adjust = c("none", "bonferroni", "holm", "BH"),
  alpha = 0.05,
  include_adjust_note = TRUE,
  effect_type = "ATE",
  scale_info = NULL,
  custom_order = NULL,
  category_rule = c("interval", "interval_and_evalue"),
  e_val_bound_comparison = c(">", ">="),
  order_direction = c("top_to_bottom", "bottom_to_top"),
  effect_label = NULL,
  listed_in_plot_order = FALSE
)
```

## Arguments

- df:

  Estimate table accepted by \[margot_plot()\], already corrected for
  multiplicity when \`adjust\` is not \`"none"\`.

- type:

  character. type of effect estimate: "RD" (risk difference) or "RR"
  (risk ratio)

- order:

  Outcome ordering rule. The first outcome in the ordering is drawn at
  the top of the plot (see \`order_direction\`), and the table and text
  follow the graph from top to bottom. \`"magnitude_desc"\` puts the
  largest absolute effect at the top; \`"magnitude_asc"\` the smallest;
  \`"evaluebound_desc"\` the largest E-value bound; \`"alphabetical"\`
  runs A to Z down the plot; \`"custom"\` follows \`custom_order\` from
  the top.

- original_df:

  Legacy unstandardised source data for inferred scale metadata.
  Explicit saved metadata is preferred.

- e_val_bound_threshold:

  Reporting threshold for the E-value bound. By default it sets only the
  E-value flag reported beside each estimate (table column and prose);
  it does not change the plotted category. Under \`category_rule =
  "interval_and_evalue"\` it also gates the category, as in earlier
  versions.

- adjust:

  character. multiplicity correction method: "none", "bonferroni",
  "holm" or "BH". With \`"none"\`, the supplied intervals and E-values
  are used unchanged. \`"holm"\` and \`"BH"\` stop under the default
  interval rule: margot builds their intervals at coverage 1 - adjusted
  p, so the intervals exclude the null whatever the adjusted p-value and
  cannot carry an interval decision. \`category_rule =
  "interval_and_evalue"\` still runs them, with a warning, to reproduce
  earlier output; it carries the same limitation.

- alpha:

  numeric. significance level for corrections

- include_adjust_note:

  logical; if \`FALSE\`, any reference to adjustment methods is omitted.
  default \`TRUE\`.

- effect_type:

  Label identifying ATE, ATT, ATC, ATO or an association.

- scale_info:

  Optional data frame keyed by the original outcome name, with
  transformation (\`identity\`, \`log\`, or \`log1p\`), saved \`center\`
  and positive \`scale\`, \`orientation\` (1 or -1), \`unit\`, and
  positive \`unit_multiplier\`. Constants describe the model outcome as
  orientation \* (g(Y) - center) / scale. Explicit metadata overrides
  inference from \`original_df\`.

- custom_order:

  Outcome display labels in the requested order, first at the top of the
  plot (with the default \`order_direction\`).

- category_rule:

  How estimates are classified for colour, legend, table and prose.
  \`"interval"\` (default) uses the (adjusted) confidence interval
  alone: above the null, below the null, or including the null.
  \`"interval_and_evalue"\` reproduces margot 1.3.2 and earlier, for
  reproducing earlier results only: an estimate is "positive" or
  "negative" only when its interval excludes the null and its E-value
  bound passes the threshold, and is otherwise labelled "not reliable";
  the prose then lists only estimates whose bound passes the threshold.

- e_val_bound_comparison:

  Comparison between the E-value bound and \`e_val_bound_threshold\`:
  \`"\>"\` (default) flags a bound that exceeds the threshold; \`"\>="\`
  reproduces margot 1.3.2 and earlier, which also flagged a bound equal
  to it. The comparison uses full precision.

- order_direction:

  Where the ordering starts. \`"top_to_bottom"\` (default) draws the
  first outcome of \`order\` at the top. \`"bottom_to_top"\` reproduces
  margot 1.3.2 and earlier, for reproducing earlier results only: the
  first outcome was drawn at the bottom, so \`"magnitude_desc"\` put the
  largest effect at the bottom. The table and prose follow the plot from
  top to bottom under either value.

- effect_label:

  Name for the model-scale estimate in each bullet, for example \`"ATE
  (standardised)"\`; \`NULL\` uses the effect type.

- listed_in_plot_order:

  logical; \`TRUE\` when the prose accompanies a plot, so that it says
  the outcomes are listed in plot order.

## Value

list with one element, \`interpretation\` (a character string).
