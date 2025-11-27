# Restore LMTP Output from Saved Checkpoints

Loads the checkpoint files produced by \`margot_lmtp()\` and rebuilds
the downstream contrasts and evaluation tables so the returned object
mimics a successful call to \`margot_lmtp()\`. This is useful when a
long-running batch completed all model fits but failed before combining
results.

## Usage

``` r
margot_lmtp_restore_checkpoints(
  checkpoint_dir,
  outcome_vars = NULL,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  quiet = FALSE
)
```

## Arguments

- checkpoint_dir:

  Path to the directory that contains the \`.qs\` checkpoint files saved
  by \`margot_lmtp()\` (e.g.,
  \`.../checkpoints/prefix_YYYYMMDD_HHMMSS\`).

- outcome_vars:

  Optional character vector giving the desired ordering of outcomes.
  When omitted, the order is inferred from the checkpoints.

- contrast_type:

  Type of contrasts to compute: \`"pairwise"\` or \`"null"\`. Defaults
  to \`"pairwise"\`.

- contrast_scale:

  Scale for contrasts: \`"additive"\`, \`"rr"\`, or \`"or"\`. Defaults
  to \`"additive"\`, matching the default used in \`margot_lmtp()\`.

- quiet:

  Logical; if \`TRUE\`, suppresses CLI messages. Defaults to \`FALSE\`.

## Value

A list with the same structure as \`margot_lmtp()\` output: \`models\`,
\`contrasts\`, \`individual_tables\`, and \`combined_tables\`. The
normalised checkpoint directory is attached via the \`"checkpoint_dir"\`
attribute for reference.

## Examples

``` r
if (FALSE) { # \dontrun{
restored <- margot_lmtp_restore_checkpoints(
  checkpoint_dir = "/path/to/checkpoints/ipsi_fixed_20251006_140122",
  contrast_type = "pairwise",
  contrast_scale = "additive"
)
} # }
```
