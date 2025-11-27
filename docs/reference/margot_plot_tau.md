# Create Faceted Tau Hat Distribution Plots

creates a faceted grid of histograms showing the distribution of tau hat
(individual treatment effects) for multiple models. the range is
standardised across all facets to facilitate comparison. automatically
detects and handles models_binary structures by extracting the \$results
component.

## Usage

``` r
margot_plot_tau(
  models_list,
  label_mapping = NULL,
  binwidth = 0.01,
  base_size = 14,
  ncol = NULL,
  title = NULL,
  subtitle = NULL,
  x_label = expression(tau[i]),
  show_zero_line = TRUE,
  fill_colour = "white",
  border_colour = NA,
  colour_by_sign = TRUE,
  colour_below = "#4f88c6",
  colour_above = "#d8a739",
  zero_line_colour = "red",
  zero_line_alpha = 0.5,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  free_scales = FALSE,
  theme = "void"
)
```

## Arguments

- models_list:

  list of models or results object containing models with tau_hat
  values. can be a nested list structure like
  \`models_binary\$results\`, or the parent \`models_binary\` object
  itself (in which case \`\$results\` will be extracted automatically).

- label_mapping:

  optional named list for transforming model names to display labels. if
  null, uses automatic label transformation.

- binwidth:

  numeric; width of histogram bins. default 0.01.

- base_size:

  numeric; base font size for the plot. default 14.

- ncol:

  integer; number of columns in facet grid. if null, automatically
  determined based on number of models.

- title:

  character; main title for the plot. default null.

- subtitle:

  character; subtitle for the plot. default null.

- x_label:

  character; label for x-axis. default uses expression for tau.

- show_zero_line:

  logical; whether to show vertical line at zero. default true.

- fill_colour:

  character; fill colour for histogram bars when colour_by_sign is
  false. default "white".

- border_colour:

  character; border colour for histogram bars. default NA (no border).

- colour_by_sign:

  logical; whether to colour bars differently above/below zero. default
  true.

- colour_below:

  character; colour for bars below zero when colour_by_sign is true.
  default "#4f88c6".

- colour_above:

  character; colour for bars above zero when colour_by_sign is true.
  default "#d8a739".

- zero_line_colour:

  character; colour for zero line. default "red".

- zero_line_alpha:

  numeric; transparency for zero line. default 0.5.

- remove_tx_prefix:

  logical; remove time prefixes from model names. default true.

- remove_z_suffix:

  logical; remove \_z suffix from model names. default true.

- use_title_case:

  logical; convert labels to title case. default true.

- remove_underscores:

  logical; replace underscores with spaces. default true.

- free_scales:

  logical; whether to allow free scales in facets. default false to
  maintain fixed range across all facets.

- theme:

  character; ggplot2 theme to use. options include "classic", "minimal",
  "bw", "light", "dark", "void", "grey", "linedraw". default "void".

## Value

a ggplot object with faceted tau hat distributions

## Examples

``` r
if (FALSE) { # \dontrun{
# with label mapping - pass models_binary directly
label_map <- list(
  "model_t2_belong_z" = "Social Belonging",
  "model_t2_trust_z" = "Trust in Others",
  "model_t2_log_charity_donate_z" = "Charitable Donations",
  "model_t2_log_hours_charity_z" = "Volunteer Hours"
)

# method 1: pass the parent object (auto-extracts $results)
tau_plot <- margot_plot_tau(
  models_binary,
  label_mapping = label_map,
  title = "Individual Treatment Effects"
)

# method 2: pass $results directly (also works)
tau_plot <- margot_plot_tau(
  models_binary$results,
  label_mapping = label_map,
  title = "Individual Treatment Effects"
)

# with different theme
tau_plot <- margot_plot_tau(
  models_binary,
  label_mapping = label_map,
  title = "Individual Treatment Effects",
  theme = "minimal"
)

# without conditional colouring
tau_plot <- margot_plot_tau(
  models_binary,
  label_mapping = label_map,
  title = "Individual Treatment Effects",
  colour_by_sign = FALSE,
  fill_colour = "lightblue"
)

# with custom colours for above/below zero
tau_plot <- margot_plot_tau(
  models_binary,
  label_mapping = label_map,
  title = "Individual Treatment Effects",
  colour_below = "darkred",
  colour_above = "darkgreen"
)

# add borders if desired
tau_plot <- margot_plot_tau(
  models_binary,
  label_mapping = label_map,
  border_colour = "grey50"
)

# without label mapping (auto transform)
tau_plot <- margot_plot_tau(models_binary)
} # }
```
