# margot NEWS

## Changes in version 1.0.263

### New
- IPSI context block in `margot_interpret_lmtp_positivity()` (odds‑free, LaTeX) explaining $q_t(H_t) = \frac{\delta g_t(H_t)}{(1-g_t(H_t)) + \delta g_t(H_t)}$ and small probability illustrations. Auto‑labels treatment as “exposure” (domain‑agnostic).
- Deterministic policy context in `margot_interpret_lmtp_positivity()` describing $A_t^{\bar d} := d_t(A_t,\mathcal H_t)$ and listing included policies.
- Policy‑implied exposure rates (optional) reported by wave and overall using $\hat p_t = \sum_i r_{i,t} A_{i,t}/\sum_i r_{i,t}$ on uncensored rows. If exposures are not binary, an indicator $\mathbb{1}(A_t \;{op}\; \tau)$ is used (defaults: $op$ is $>$ and $\tau=0$).

### Improvements
- Wave labels in positivity text now respect user `label_mapping` (e.g., “Baseline (2018/19)”, “Year 1 Follow‑up”) and support skipped panels.
- Clarified positivity vs. target: diagnostics labelled “positivity; uncensored rows” and text notes that estimation reweights to the baseline cohort via censoring adjustment.
- Per‑wave bullets now include both `ESS+/(N+)` and `ESS+/(N_pt)`.
- Detailed diagnostics clarify censoring semantics: “censoring to next wave” for intermediate waves; “censoring end of study” for the final exposure.
- LaTeX sanitisation of common glyphs in text ($\to$, $\pm$, $\ge$, $\le$, $\approx$, $\times$).

### Fixes
- `margot_plot_lmtp_overlap_grid()`: stop showing the deprecated `layout` warning unless the user explicitly supplies `layout`.

## Changes in version 1.0.261

### Improvements
- `margot_lmtp_overlap()` now sources its `text_summary` from `margot_interpret_lmtp_positivity()`, so overlap summaries mirror the enhanced prose and optional diagnostics provided by the helper.

## Changes in version 1.0.260

### Fixes
- Restored per-wave facets in LMTP overlap grids by coercing `density_ratios` from `Matrix`/`data.frame` to base matrices before iterating waves.
- Avoided dropped bars when harmonising axes by switching to `coord_cartesian()`.
- Ensured the `shifts` order is respected without losing wave panels.

### Improvements
- `margot_plot_lmtp_overlap_grid()` now defaults to `layout = "shifts_by_waves"` and `color_by = "shift"` (with a CLI note when overriding user settings) for robust mapping and consistent colouring.
- `margot_interpret_lmtp_positivity()` prints per-wave uncensored diagnostics; ESS labels clarified to `ESS+/(N+)` and, when helpful, `ESS+/(N_pt)`.
- Methods text clarifies that `null` includes censoring adjustment, so ratios may not centre at 1.

### Internal
- New internal palette helper `margot_palette("lab")` expands colours for `null`, `shift_zero`, and `ipsi_*` variants.

### Backward compatibility
- No exported function changes; `_pkgdown.yml` unchanged.

## Changes in version 1.0.150

### New features
* Added `compute_rate_on_demand()` helper function for flexible RATE computation
* Enhanced `margot_plot_rate()` to support on-demand RATE computation from causal forests
* Added `target` parameter to `margot_plot_rate_batch()` to choose between AUTOC and QINI
* Added `q` parameter to `margot_rate()` for custom quantile grids
* Added `use_evaluation_subset` parameter to properly use test indices for validation

### Parameter changes
* **BREAKING**: Changed `qini_train_prop` to `train_prop` in `margot_causal_forest()` 
  - Default changed from 0.7 to 0.5 for better train/test balance
  - `qini_train_prop` is now deprecated but still works with a warning
* Added validation warnings when RATE is computed without proper test/train splits

### Improvements
* RATE functions now automatically use test indices from `qini_metadata` when available
* Better validation messages to help users understand when results may be optimistic
* Consistent handling of out-of-sample evaluation across the pipeline

### Backward compatibility
* Existing results from `margot_causal_forest()` work with all new features
* The deprecated `qini_train_prop` parameter still works but shows a deprecation warning
* Users only need to re-run `margot_causal_forest()` if they want proper validation and previously used `qini_split = FALSE`
