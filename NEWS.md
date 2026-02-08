# [2026-02-09] margot 1.0.299

### Fixed
- `prepare_panel_data()` no longer fails with recent dplyr versions when `wave_breaks` is supplied. The old code unquoted Date objects into tidy expressions (`!!`), stripping their class and causing type errors in `dplyr::between()`. Wave assignment now uses direct date comparison.

# [2026-02-03] margot 1.0.298

### Changed
- margot_count_ids() now defaults to filtering all counts when strata_filter is supplied (strata_filter_scope = "auto"), while leaving totals unchanged when strata_filter is NULL.

# [2026-02-03] margot 1.0.297

### Changed
- margot_count_ids() now keeps overall counts when using strata_filter (default strata_filter_scope = "strata_only") and warns if all_counts would drop deceased rows.

# [2026-02-03] margot 1.0.296

### Added
- margot_count_ids() summarises cumulative and wave-specific participant counts, with optional opt-in tracking and stratified active totals.
- margot_count_ids() gains track_opt_ins, strata_filter, and strata_filter_scope to control opt-in counting and focus on selected strata levels without dropping overall counts.

# [2026-01-28] margot 1.0.295

### Added
- `here_read_arrow()` and `here_save_arrow()` restored as supported helpers for Parquet workflows, which we now use to replace deprecated qs calls.

### Changed
- `here_read_qs()` and `here_save_qs()` now produce a compatibility warning and suggest Arrow helpers for new workflows (suppressed when `quiet = TRUE`).

# [2025-12-03] margot 1.0.294

### Changed
- `include_policy_rates` now defaults to FALSE across positivity helpers; policy rates require user-supplied binary indicators for non-binary exposures. Method text clarifies policy-rate expectations.

# [2025-12-03] margot 1.0.293

### Added
- `margot_positivity_report_single_model()` wraps standalone LMTP fits (single shift) so positivity reports work without batching, while preserving the existing multi-shift defaults.

# [2025-12-03] margot 1.0.292

### Changed
- `policy_brief_df` now respects `order_models = "alphabetical"` for consistent ordering across all outputs.

# [2025-12-03] margot 1.0.291

### Fixed
- Alphabetical sorting now strips leading punctuation (e.g., "(reduced) Neuroticism" sorts under "R" not "(").

# [2025-12-03] margot 1.0.290

### Added
- `margot_policy_workflow()` gains `order_models` parameter: `"alphabetical"` (default) or `"by_effect"`. Models in interpretations and plots are now sorted alphabetically by label for easier navigation.

# [2025-12-03] margot 1.0.289

### Added
- `margot_policy_workflow()` gains `interpret_models` parameter to control which models receive full interpretations: `"wins"` (default), `"wins_borderline"`, `"recommended"`, or a character vector of specific model names.
- `margot_policy_workflow()` gains `plot_models` parameter to generate plots via `margot_policy()`: `"none"` (default), `"same"` (match interpret_models), `"wins"`, `"wins_borderline"`, `"recommended"`, or specific model names.
- New parameters `plot_output_objects`, `policy_tree_args`, `decision_tree_args`, and `qini_args` for plot customisation.
- Plots returned in `wf$plots` when `plot_models != "none"`.
- Added worked examples to roxygen documentation.

# [2025-12-01] margot 1.0.288

### Changed
- Systematic removal of Unicode characters throughout the R codebase for improved cross-platform compatibility and encoding reliability:
  - Replaced Unicode arrows (→) with ASCII equivalents (`->`) in CLI messages, comments, and output text.
  - Replaced Unicode mathematical symbols (≥, ≤, Δ, α, μ, τ) with ASCII equivalents (`>=`, `<=`, `delta`, `alpha`, `mu`, `tau`) in user-facing messages.
  - Replaced emoji thumbs up (👍, `\U0001F44D`) with `cli::cli_alert_success()` messages for consistent terminal output.
  - Replaced Unicode dashes (–, —) with standard ASCII hyphens in output text.
- Applied NZ English spelling where appropriate (e.g., "finalised" in user-facing messages).

### Note
- Unicode symbols in roxygen documentation comments and LaTeX sanitisation functions (which intentionally convert Unicode to LaTeX for rendering) have been preserved.

# [2025-11-28] margot 1.0.287

### Fixed
- `margot_plot_categorical()`: Replaced Unicode sigma (σ) with "SD" in standard deviation labels to fix encoding errors when rendering to PDF/docx via Quarto.

# [2025-11-28] margot 1.0.286

### Changed
- `margot_build_method_explanation()`: Converted inline citations to Quarto format (`[@key]`) for proper bibliography generation when rendering in Quarto/R Markdown. Citations now reference BibTeX keys: `@athey2021`, `@grf2024`, `@policytree_package_2024`, `@meinshausen2010stability`, `@efron2012large`, `@tusher2001significance`, `@smyth2004linear`, `@gelman2006difference`.
- Signal score methods text now includes clarification: "This descriptive ranking shows potentially actionable heterogeneity that warrants further investigation."
- Replaced Unicode special characters (em-dash, multiplication sign) with ASCII equivalents for cross-platform compatibility in methods output.
- Removed placeholder "Ehrlich et al., 2024" citation; policy value methods now correctly cite `@athey2021` and `@policytree_package_2024`.

# [2025-11-28] margot 1.0.285

### Changed
- `margot_interpret_policy_batch()`: Replaced Unicode arrows with LaTeX (`$\rightarrow$`) in the mixed-depth summary header for proper rendering in Quarto/R Markdown.
- `margot_interpret_policy_batch()`: Renamed return element from `report_full` to `report_detail` for consistency with other reporting functions.
- `margot_policy_workflow()` now exposes `report_detail` at the top level (alongside `report` and `report_prose`) for convenient access to detailed per-model interpretations.
- Policy value explanation in interpret batch uses consistent terminology with the policy explainer: "*Policy value*" and "*uplift*" are italicised as defined terms.

# [2025-11-28] margot 1.0.284

### Added
- `margot_policy_summary_report()` gains a new `report_format` parameter with options `"bullets"` (default) and `"prose"`. The prose format generates paragraphs suitable for direct inclusion in scientific reports, more or less.
- New `report_prose` element in the return list always contains the prose-formatted report, regardless of the `report_format` setting, so users can access both formats.
- `margot_policy_workflow()` now exposes `report` and `report_prose` at the top level for convenient access (previously only available via `wf$summary$report`).
- Both bullet and prose report formats now distinguish "borderline positive" outcomes (CI lower bound > -0.01 with positive point estimate) from truly "inconclusive" outcomes. This classification is consistent across `summary$text`, `report`, and `report_prose`.
- New return elements: `borderline_model_ids`, `borderline_model_names`, `inconclusive_model_ids`, `inconclusive_model_names` for programmatic access to the refined classification.
- New `include_acronyms` parameter in `margot_policy_summary_report()` (default FALSE) controls whether common acronyms (RWA, SDO, PWI, NZSEI) are appended to the policy value explanation.

### Changed
- Prose output now uses present tense ("yields", "is") rather than past tense ("yielded", "was") because inferences are made to the population, not the sample.
- Policy value explainer is now more concise: adds an introductory sentence ("The following terms are used throughout this report"), removes the technically problematic definition of confidence intervals (now simply states "All estimates are reported with 95% confidence intervals"), and tightens prose throughout.  

# [2025-11-28] margot 1.0.283

### Changed
- `margot_policy_workflow()`: Renamed table columns from "Effect Size" to "Policy Value" and "Effect in Treated" to "Uplift in Treated" for consistency with policy learning terminology.
- `margot_policy_summary_report()`: Updated `group_table_df` column names to match the terminology used in the methods documentation.
- Replaced Unicode mathematical symbols with LaTeX equivalents using double backslashes for proper rendering when output is passed through `cat()` in Quarto/R Markdown (e.g., `×` becomes `$\\times$`, `≥` becomes `$\\geq$`).
- Fixed Unicode en-dash characters in signal score descriptions to use standard ASCII hyphens for cross-platform compatibility.

### Documentation
- Terminology now consistently uses "Policy Value" (the expected welfare gain from targeting vs a baseline) rather than "Effect Size" throughout the policy workflow output.

# [2025-11-26] margot 1.0.282

### Added
- `margot_policy_workflow()` now returns `depth_comparison_report`, a list with `text` (formatted markdown table) and `data` (data frame) showing both depth-1 and depth-2 policy values for all outcomes, along with the gain (Δ), selected depth, and rationale. This provides transparency about the depth selection decision regardless of which depth is ultimately chosen.
- New internal helper `.build_depth_comparison_report()` generates the depth comparison report with parsimony threshold context.

### Changed
- Depth selection rationale is now explicitly documented: for **additive heterogeneity** (e.g., τ = α + β₁X₁ + β₂X₂), depth-1 trees are preferred when the policy value gain from depth-2 is below the threshold (default 0.005), as the dominant variable captures most actionable variation. For **nonlinear/piecewise heterogeneity** (e.g., quadrant indicators), depth-2 provides substantial gains and is correctly selected.

### Documentation
- Updated `@return` documentation for `margot_policy_workflow()` to describe the new `depth_comparison_report` field.

# [2025-11-12] margot 1.0.281

### Changed
- `margot_policy_workflow()` now honours caller overrides for `split_compact`, `split_drop_zero`, and `split_top_only` across both the depth comparison and mixed-depth summary, preventing duplicate-argument errors and letting users surface full depth-2 leaf breakdowns when desired.
- `margot_policy_workflow()` accepts a `model_names` vector that propagates through depth selection, summary, and recommendations so workshop pipelines can exclude specific outcomes (e.g., drop a homogeneous volunteering model) without re-running upstream objects; the method context now reflects the final signalling options after overrides.

# [2025-11-11] margot 1.0.280

### Added
- `margot_positivity_report()` now returns `wave_summary_table` (shift × wave ESS per N% grid mirroring the overlap plot) and `censoring_summary`, so Quarto sections can drop both numerical grids and a single sentence about zeros without recomputing diagnostics.
- `margot_lmtp_weight_diag_from_fit()` records per-wave ESS metrics (`ess_pos`, `ESS+/N+`, `ESS+/N_pt`) so downstream summaries inherit the same values used in the narrative.

### Changed
- Default wave-title text in `margot_plot_lmtp_overlap_grid()` is now `text_size + 5`, keeping column headers legible when shift annotations are enlarged.
- Compact positivity tables use plain-language headers (`Prod < 10%`, `ESS per N%`) and reserve censoring information for the new `censoring_summary`.
- The new wave summary table labels its columns as `ESS per N% : <wave>` to clarify what each cell contains.

# [2025-11-10] margot 1.0.274

### Added
- New internal `margot_palette_lab_resolve()` helper exposes consistent colours for canonical LMTP shift names (null, weekly/monthly cadence, and `ipsi_*` deltas) even after outcome prefixes are attached.

### Changed
- `margot_lmtp_overlap()` now routes shift/constant fills through the lab palette resolver whenever the lab palette is requested, ensuring plots stay on-brand without manual colour overrides.
- Added focused palette tests to guard the extended mappings.

# [2025-11-09] margot 1.0.273

### Added
- `margot_lmtp_weight_diag_from_fit()` reconstructs censoring masks from `density_ratios > 0`, reports wave-specific tail summaries, and tracks cumulative ESS (raw and trimmed) even when the original censor columns aren’t available.

### Changed
- LMTP overlap plots now use an enhanced `margot_palette("lab")` resolver so common shift labels (e.g., `null`, cadence labels like `weekly`/`monthly`, and `ipsi_*` deltas) keep consistent colours even when outcome prefixes are present.
- `margot_interpret_lmtp_positivity()` now accepts `trim_right`, masks density ratios via `dr > 0` for IPSIs, and reports cumulative ESS alongside the existing per-wave metrics.
- `margot_transition_ipsi_summary()` emits LaTeX-ready narrative blocks and shows natural initiation rates with their exact Clopper–Pearson CIs.
- `margot_compute_ipsi_probability()` keeps raw probabilities plus 95% CIs, so downstream helpers don’t need to re-compute binomial intervals.

# [2025-11-08] margot 1.0.272

### Added
- `margot_transition_ipsi_summary()` now emits both a kable-ready table (via `pretty = TRUE`) and a LaTeX-ready narrative describing each wave pair’s natural rate plus the δ-specific counterfactual probabilities/fold increases.
- `margot_compute_ipsi_probability()` attaches exact (Clopper-Pearson) 95% confidence intervals for the natural initiation rate so downstream summaries can report uncertainty without re-running binomial tests.
- `margot_lmtp_weight_diag_from_fit()` reconstructs censoring masks from `density_ratios > 0`, reports per-wave quantiles/tail mass, and tracks cumulative ESS (raw and winsorised) for LMTP fits where the original censor nodes are not available.

### Changed
- `margot_transition_table()` provides `$get_table_data()` and `$compute_ipsi_probabilities()` helpers and stores machine-readable matrices alongside each markdown table so downstream IPSI utilities can operate without parsing strings.

# [2025-11-08] margot 1.0.271

### Added
- `margot_compute_ipsi_probability()` converts any formatted transition matrix (including those produced by `margot_transition_table()`) into natural vs counterfactual initiation probabilities for IPSI deltas, reporting fold changes plus the raw counts used in each estimate.
- `margot_transition_ipsi_summary()` automates IPSI reporting by digesting the entire `margot_transition_table()` output (or any compatible list of matrices) and returning a tidy table of wave-pair initiation rates across the requested deltas; `pretty = TRUE` formats the result for direct use with `knitr::kable()`.

### Changed
- `margot_transition_table()` now ships the machine-readable matrices alongside the rendered markdown (`tables_data` plus a `table_data` attribute on each `knitr_kable`) and attaches `$get_table_data()` / `$compute_ipsi_probabilities()` helpers so downstream code can retrieve counts or run IPSI summaries without manual parsing.
- Pkgdown reference index lists `margot_compute_ipsi_probability()` so the new helper appears in the online docs without build warnings.

# [2025-11-07] margot 1.0.270

### Added
- `margot_make_tables()` now supports richer categorical handling via `ordinal_vars`, `ordinal_levels`, `binary_to_yesno`, `binary_labels`, `auto_integer_ordinals`, and `integer_ordinal_range`, allowing ordinal/binary columns to render as frequency rows with custom labels.

### Fixed
- Ordinal conversion guardrails ensure zero-length or mismatched label vectors no longer crash table generation; offending label specs are dropped with informative warnings.

# [2025-11-06] margot 1.0.269

### Fixed
- `margot_naive_regressions()` now recognizes `flip_outcomes` vectors supplied via quoted object names (e.g., `"flip_outcomes_naive"`), automatically resolving them and applying the same flipping workflow used by `margot_causal_forest()`.

# [2025-11-05] margot 1.0.268

### Added
- `duplicate_policy` argument in `margot_lmtp_combine_and_contrast()` so duplicate outcome/shift models can be overwritten, skipped, or treated as errors explicitly.
- Optional `keep_models = TRUE` flag in `margot_lmtp_combine_and_contrast()` to retain merged LMTP models for downstream learner diagnostics.

### Changed
- Standalone checkpoint ingestion paths now emit deterministic CLI messages (no cli pluralisation helpers required), ensuring recovery runs that only pass checkpoints complete without glue errors.
- `margot_resort_contrast_lmtp()` renamed to `margot_lmtp_resort_contrast()`; the old name remains available but issues a soft deprecation warning.

# [2025-10-22] margot 1.0.266

### New 1.0.266 entry summarizing:
- Example added for `margot_ipsi_summary()`
- Compact table refinements (ATT [CI], friendly Prod headers, hide policy cols if unused, explanation attribute)


### Fixed
- `margot_plot()` restored to working order.



# [2025-10-22] margot 1.0.265

### Fixed
- `margot_plot()` restored to working order.


# [2025-10-22] margot 1.0.264
### Added
- IPSI context (odds‑free) in `margot_interpret_lmtp_positivity()` with a simple probability‑scale formula and small illustrative translations; domain‑agnostic default labelling ("exposure").
- Deterministic policy context describing history‑dependent rules (e.g., A_t^d := d_t(A_t, H_t)) and listing included policies.
- Optional policy‑implied exposure rates by wave and overall using reweighted means; supports thresholding non‑binary exposures with an indicator 1(A_t op tau) (defaults: op is >, tau = 0).
- Experimental `margot_plot_dev()` retains advanced bias analysis features (optional multiplicity correction, label mapping/renaming, interpretation); `margot_plot()` now delegates to `margot_plot_dev()` under the hood so existing scripts gain advanced options without code churn.

### Changed
- Wave labels in positivity text respect user `label_mapping` (e.g., “Baseline (2018/19)”), supporting skipped panels.
- Clarified positivity vs target: diagnostics labelled “positivity; uncensored rows” with a note that estimation reweights to the baseline cohort via censoring adjustment.
- Per‑wave bullets include both `ESS+/(N+)` and `ESS+/(N_pt)`.
- Censoring wording clarified: “censoring to next wave” for intermediate waves, “censoring end of study” for the final exposure; shift summaries show “censoring (zeros across person‑time)”.
- LaTeX sanitisation of common glyphs in generated text ($\to$, $\pm$, $\ge$, $\le$, $\approx$, $\times$).
- `margot_plot_lmtp_overlap_grid()`: suppress deprecated `layout` warning unless `layout` is explicitly supplied.
- `margot_plot_dev()`: hides subtitle spacing when subtitle is empty.

### Fixed
- Reverted `margot_plot()` to the last known stable implementation (pre‑DEV) to resolve recent breakages and restore previous behaviour and API. `margot_plot_dev()` remains available for advanced workflows.

### Docs
- Rd parameter docs avoid LaTeX macros to prevent Rd warnings; pkgdown reference updated to include `margot_plot_dev` and `margot_multi_evalue`.

### Note
- We are consolidating plotting on the experimental engine while we stabilise risk‑ratio E‑value logic and multiplicity adjustments. Further robustness improvements will ship in a follow‑up.

# [2025-10-16] margot 1.0.261
### Changed
- `margot_lmtp_overlap()` now pulls its `text_summary` directly from `margot_interpret_lmtp_positivity()`, ensuring overlap diagnostics match the upgraded prose helper and optional report sections.

# [2025-10-15] margot 2.0.260
### Changed
- `margot_plot_lmtp_overlap_grid()` always annotates panels by shift (deprecated `annotate_graph`) and now respects `annotate_wave_size` when styling wave titles.
- `margot_interpret_lmtp_positivity()` improved reporting


# [2025-10-10] margot 1.0.259
### Added
- **New function**: `margot_lmtp_restore_checkpoints()` loads saved LMTP checkpoints, rebuilds contrasts/tables, and surfaces any shift coverage gaps so long runs interrupted midstream can be recovered without recomputing finished models.

### Changed
- `margot_lmtp()` now delegates post-processing to a shared helper, reducing duplicated logic and ensuring restored checkpoints produce identical summaries.
- Checkpoint restoration now reports missing shift models per outcome using robust CLI glue evaluation (fixes `{table_name}` errors when warning about absent tables).

# [2025-10-09] margot 1.0.258
### Fixed
- `margot_policy_workflow()` now guards against missing win/neutral ID tables so the workflow no longer errors when no models meet those criteria. The existing summary outputs remain unchanged.

# [2025-10-05] margot 1.0.257
### Added
- **New function**: `margot_lmtp_combine_and_contrast()` - combines LMTP models from multiple batches and computes cross-batch contrasts
  - Enables contrasts between shifts estimated in separate runs (e.g., shift_zero from batch 2 vs ipsi_02 from batch 1)
  - Validates outcomes match across batches and warns about mismatches while proceeding with valid contrasts
  - Supports user-specified contrast pairs or automatic pairwise contrasts via `auto_pairwise = TRUE`
  - Full error handling with informative CLI messages and statistics summary
  - Tracks source batch for each model in diagnostic messages

# [2025-10-03] margot 1.0.256
### Changed
- **Removed**: `gc()` call from worker cleanup in `margot_lmtp()` - let R manage memory naturally instead of forcing aggressive garbage collection
- **Added**: CLI message showing how many cores are being used when `manage_future_plan = TRUE`
- **Documentation**: Added `@details` section explaining that very large models may not benefit from parallelization due to memory constraints

# [2025-10-02] margot 1.0.255
### Fixed
- **CRITICAL**: `margot_lmtp()` now explicitly shuts down all parallel workers when `manage_future_plan = TRUE` to prevent zombie processes and resource leaks
- **CRITICAL**: Removed overly aggressive thread limiting (OMP_NUM_THREADS) that was causing worker startup failures
- Added `gc()` call after worker shutdown to ensure memory cleanup
- Added CLI message "Shutting down parallel workers..." for transparency

### Changed
- Worker cleanup now always sets `future::plan(sequential)` on exit, regardless of initial plan state

# [2025-10-02] margot 1.0.254
### Performance Improvements
- **CRITICAL FIX**: `margot_lmtp()` now respects user's external `future::plan()` when `manage_future_plan = FALSE` (default), enabling parallel cross-validation within each model (~5x speedup)
- **CRITICAL FIX**: Removed `future::plan(sequential)` override that was disabling all parallelization when `manage_future_plan = FALSE`
- **Fixed**: Set `mc.cores` and `parallelly.maxWorkers.localhost` early when `manage_future_plan = TRUE` to prevent "only 1 CPU cores available" errors with nested futures
- **Fixed**: User interrupts (Ctrl+C) now work correctly - removed interrupt detection code that was catching interrupts as errors

### Changed
- `margot_lmtp()`: When `manage_future_plan = FALSE`, models run sequentially but each model can now use user's future plan for parallel CV
- `margot_lmtp()`: Improved CLI diagnostics show whether LMTP internal parallelization is enabled and how many workers are available
- `margot_lmtp()`: Treatment variable preflight check now handles character vectors properly and provides better error messages

### Expected Performance
- **With external future plan**: Set `future::plan(multisession, workers = 5)` before calling `margot_lmtp()` with default `manage_future_plan = FALSE` → ~5x faster (sequential models, parallel CV)
- **With managed nested futures**: Use `margot_lmtp(..., manage_future_plan = TRUE, cv_workers = 5, n_cores = 12)` → ~10x faster (2 models × 5 CV workers = 10 cores)

# [2025-10-02] margot 1.0.253
### Breaking Changes
- None

### Added
- `margot_lmtp()`: incremental checkpointing system saves each model immediately after completion to timestamped subdirectory (`save_path/checkpoints/prefix_YYYYMMDD_HHMMSS/`), protecting against power failures and interruptions.
- `margot_lmtp()`: preflight validation checks all required variables (treatment, outcomes, baseline, time_vary, cens) exist in data before expensive computation begins.
- `margot_lmtp()`: enhanced error messages showing available variables when validation fails.

### Fixed
- `margot_lmtp()`: future parallelization conflicts resolved. When `manage_future_plan = FALSE`, explicitly sets sequential outer loop while preserving user's `future::plan()` for LMTP's internal cross-validation.
- `margot_lmtp()`: null model validation crash fixed. Now checks null model exists and is valid before computing contrasts, preventing "attempt to select less than one element" error.
- `margot_lmtp()`: error logs saved to checkpoint directory for failed models.

### Improved
- `margot_lmtp()`: checkpoint saves use `qs::qsave()` with high compression and single thread for compatibility with future parallelization.
- `margot_lmtp()`: CLI messages show checkpoint directory path and individual file saves.
- `margot_lmtp()`: preflight checks provide detailed feedback on missing variables with suggestions.

# [2025-10-02] margot 1.0.252
### Breaking Changes
- **LMTP positivity diagnostics now focus on uncensored observations**: `margot_interpret_lmtp_positivity()` reports ESS metrics computed only on density ratios > 0 (uncensored observations). Censoring rate (proportion of r = 0) is reported separately per shift.
- `margot_interpret_lmtp_positivity()`: output format changed. Each shift line now shows: `censoring = X%; uncensored ESS by wave — ...`.
- `margot_interpret_lmtp_positivity()`: internal `summarise_shift()` now returns `ess_pos`, `ess_pos_frac`, `n_all`, `n_pos`, `prop_censored` instead of `ess`, `ess_frac`, `n`.
- **LMTP density ratio plots now show only uncensored observations by default**: `margot_lmtp_overlap()` and `margot_plot_lmtp_overlap_grid()` histograms exclude zeros (r = 0) by default. Use `show_censored = TRUE` to include zeros. Plot titles now indicate "uncensored | censored: X%" or "censored: X%" depending on parameter.

### Conceptual Change
- **Zeros in LMTP density ratios primarily reflect censoring, not positivity violations**: Following expert guidance, zeros (r = 0) in longitudinal LMTP density ratios arise when individuals drop out (censored) and have no treatment observed at subsequent waves. These censoring-induced zeros appear identically across all policies. True treatment positivity violations are policy-specific. Therefore, ESS and distributional diagnostics should focus on uncensored observations (r > 0) to assess positivity where treatment was actually observed.

### Added
- `margot_interpret_lmtp_positivity()`: methods section (`include_methods = TRUE`) now includes "Censoring vs. Treatment Positivity" subsection explaining the distinction and rationale for focusing on uncensored observations.
- `margot_lmtp_positivity()`: enhanced documentation explaining censoring vs. treatment positivity and the role of `*_pos` columns. Now computes `min_pos`, `max_pos`, `mean_pos`, `sd_pos`, `cv_pos` for uncensored observations.
- `margot_lmtp_overlap()` and `margot_plot_lmtp_overlap_grid()`: new `show_censored` parameter (default: FALSE) controls whether zeros are included in histograms.

### Improved
- `margot_interpret_lmtp_positivity()`: header now reads "LMTP positivity diagnostics for [outcome] (uncensored observations)."
- `margot_interpret_lmtp_positivity()`: detailed diagnostics (`include_diagnostics = TRUE`) now use `*_pos` columns (range_pos, mean_pos, sd_pos, cv_pos, q*_pos, p_gt_*_pos) from `margot_lmtp_positivity()` and display "Censoring rate" separately.
- Per-shift lines now clearly distinguish censoring rate from uncensored ESS metrics for better interpretability.
- `margot_lmtp_overlap()`: plot titles clarify whether showing uncensored observations or all observations with censoring rate.
- All LMTP positivity/overlap documentation updated to explain censoring vs. treatment positivity distinction.

# [2025-10-01] margot 1.0.251
### Breaking Changes
- `margot_interpret_lmtp_positivity()`: removed `include_overview` parameter. Averaging statistics across interventions is incoherent and has been removed.

### Added
- `margot_interpret_lmtp_positivity()`: new `include_methods` parameter (default: FALSE) prepends methodological explanation of density ratios, their interpretation as rebalancing weights, and ESS computation formula.
- `margot_interpret_lmtp_positivity()`: new `include_diagnostics` parameter (default: FALSE) appends detailed diagnostics per shift including zeros, range, mean ± SD, CV, tail probabilities, and quantiles. Uses `margot_lmtp_positivity()` internally.

### Improved
- `margot_interpret_lmtp_positivity()`: all mathematical notation now uses LaTeX formatting (e.g., `$r_t > 1$`, `$\text{ESS} = (\sum w)^2 / \sum w^2$`) for proper rendering in Quarto markdown.
- `margot_interpret_lmtp_positivity()`: removed redundant "ESS computed from raw density ratios using colSums" text from header.
- `margot_interpret_lmtp_positivity()`: when `return = "list"`, now includes `$methods` and `$diagnostics` components for programmatic access.

# [2025-10-01] margot 1.0.250
### Breaking Changes
- `margot_plot_lmtp_overlap_grid()` and `margot_lmtp_overlap_plot_grid()`: replaced `annotate_wave` and `annotate_shift` parameters with unified `annotate_graph` parameter accepting "none", "waves", or "shifts".
- `margot_plot_lmtp_overlap_grid()` and `margot_lmtp_overlap_plot_grid()`: replaced `ymax_by_wave` with more flexible `ymax_harmonize` parameter accepting "none" (default), "row", "column", "global", or custom named vectors.
- Removed `remove_waves` parameter; use `waves` parameter for positive selection instead.

### Added
- `xlim_harmonize` parameter for x-axis harmonization in `margot_plot_lmtp_overlap_grid()` and `margot_lmtp_overlap_plot_grid()` with options: "none" (default), "row", "column", "global", or custom values.
- Automatic colour detection for shifts in `margot_lmtp_overlap()`: shifts containing "null" → grey (#7f7f7f), "up" → orange (#d95f0e), "down" → blue (#2c7fb8).
- Layout-aware axis harmonization: "row" and "column" options intelligently adapt based on whether rows/columns represent waves or shifts.

### Improved
- Default `headroom` increased from 0.06 to 0.12 for better annotation visibility.
- Annotation positioning adjusted (vjust = 1.2) for clearer wave/shift labels at top of plots.
- Updated "lab" palette alias to match automatic shift colours.
- Fixed `annotate_zeros` documentation to correctly reflect logical (TRUE/FALSE) implementation.
- Added `@param waves` documentation to wrapper function.

# [2025-09-30] margot 1.0.249
### Added
- `margot_plot_lmtp_learners()` heatmap summarising Super Learner weights by wave/component.
- `margot_interpret_lmtp_learners()` prose helper that highlights dominant learners (e.g., LOCF-driven collapses).
- `margot_plot_lmtp_overlap_grid()` now accepts `color_by`, `fill_palette`, and `text_size` to control histogram aesthetics.

# [2025-09-30] margot 1.0.248
### Changed
- Introduced `margot_interpret_lmtp_positivity()` for colSums-based ESS summaries by wave and overall, replacing the earlier zero-focused reporting.
- `margot_interpret_lmtp_overlap()` now forwards to the new helper with a soft-deprecation warning to ease migration.
- pkgdown reference navigation now lists `margot_interpret_lmtp_positivity()` in place of the deprecated alias.
- Removed redundant positivity helper wrappers (`margot_positivity_panel()`, `margot_positivity_tails_panel()`, `margot_lmtp_ratio_multigrid()`, `margot_plot_lmtp_positivity()`, `margot_plot_lmtp_positivity_batch()`).

# [2025-09-29] margot 1.0.247
### New
- `margot_overlap_bullets()` reports positivity results
- `margot_interpret_lmtp_positivity_overview`() - compares results

### Improved
- `margot_report_lmtp_positivity()` now uses label_mapping to pretty‑print the outcome in plot
  titles and returns outcome_label in the report list.
- Grid improvements
      - Rows can be annotated with wave labels: `annotate_wave = "facet"`.
      - Optional wave‑based `coloring: color_by_wave = TRUE` (distinct fill per wave).
      - Both available via margot_plot_lmtp_overlap_grid().
  - Compact by‑wave table
      - Reporter now returns `by_wave_ess_compact` as a slim Wave × Shift table (e.g., Wave | Shift Up |
  Shift Down | Null).
      - Also returns `n_participants` (baseline participants) and `n_person_time` (pooled rows).


# [2025-09-28] margot 1.0.246

### New

- `margot_report_lmtp_positivity()`: one‑stop reporter for LMTP positivity diagnostics that returns overall tables, combined by‑wave tables (ESS/N and ESS), optional density‑ratio grids with harmonised or custom y‑limits, and filtered flags. Backwards‑compatible alias `margot_lmtp_positivity_report()` is provided.
- `margot_plot_lmtp_overlap_grid()`: wrapper for wave‑by‑shift density‑ratio grids with optional y‑axis control.
- `margot_plot_lmtp_positivity()`: quick ggplots for overall ESS/N (bar) and ESS/N by wave (line) from the positivity reporter.
- `margot_plot_lmtp_positivity_batch()`: run the positivity reporter/plots across multiple analyses in one call.

### Improvements
- `margot_lmtp_overlap_plot_grid()` now harmonises y‑axis heights across panels and accepts a `ymax` override to force comparable histogram scales across waves or shifts.

# [2025-09-27] margot 1.0.245

### New
- `margot_positivity_panel()`: compact positivity summary (all vs positive-only) for LMTP models.
- `margot_positivity_tails_panel()`: detailed quantiles and tail probabilities (all vs positive-only).
- `margot_lmtp_overlap()`: LMTP overlap/positivity helper built on `margot_lmtp_positivity()`, with optional density-ratio plots and scale toggle.
- `margot_lmtp_ratio_grid()`: panel grid of per-wave density-ratio histograms for a selected outcome/shift.
- `margot_lmtp_ratio_multigrid()`: multi-shift facet grid (waves x shifts) of density ratios for a selected outcome; supports label mapping and log10/linear scales.
- `margot_lmtp_overlap_plot_grid()`: arrange `margot_lmtp_overlap()` ratio_plots into a wave-by-shift grid for a single outcome; supports ordering and title control.
- `margot_grf_overlap()`: new name for GRF overlap diagnostics (wraps existing functionality).

### Changes
- `margot_assess_overlap()` is soft-deprecated; please use `margot_grf_overlap()` instead.
- `margot_lmtp_overlap()` plots now accept `scale = "log10"` (default) or `scale = "linear"` and no longer take `save_plots`/`output_dir` (return ggplot objects to save externally).

# [2025-09-26] margot 1.0.244

### Improvements
- `margot_lmtp()` batches outcome/shift fits with nested future parallelism, prioritises fold workers, and adds `models_in_parallel`/`cv_workers` controls plus clearer scheduling messages. (note, nested future parrallelism not yet working; lmtp
'listens' to sequential parrallel setting, slowing its cross validation (to be continued...))

### Bug Fixes
- Stabilised nested future plans on macOS by provisioning dedicated multisession layers for outer/inner work so batches no longer trigger `UnexpectedFutureResultError`.

# [2025-09-19] margot 1.0.243

- `margot_lmtp_positivity()` - reports positivity in lmtp models

# [2025-09-19] margot 1.0.242

### New: 
- `margot_policy_workflow()`, shared explainer

### Improvements: 
- min_gain_for_depth_switch, audience + return_unit_masks, brief includes Depth, bugfix for depth-1 in summary table, interpretation reuse flags

# [2025-09-19] margot 1.0.240 - 1.0.241

### Improvements
- `margot_policy_tree_stability()` adds `compute_policy_values` (with `policy_value_R`, `policy_value_seed`, `policy_value_baseline`) to pre-compute bootstrap policy gains and now recognises the legacy `parrallel` flag.
- `margot_policy_tree_stability()` allows tuning `future.globals.maxSize` via `future_globals_max_size` (20 GiB default) so large stability jobs can run in parallel without hitting the future globals guard.
- Cached policy gains include baseline/contrast metadata for downstream reporters; internal fallback `.compute_policy_value_internal()` keeps stand-alone scripts working.
- `margot_policy_summary_report()` adds a "Practical Takeaways" block, per-model depth overrides (`depths_by_model`), depth-specific model lists/maps, recommended id/name outputs by depth, and compact-table controls (`split_drop_zero`, `split_top_only`) while fixing the missing-object error.
- `margot_policy_summary_compare_depths()` is now exported and documented alongside the updated policy reporting workflow, returning a mixed-depth report (`best_summary`) plus depth maps/lists for downstream plotting.
- `margot_interpret_policy_batch()` accepts `depths_by_model`, surfaces per-model depths, and returns the depth map when `return_as_list = TRUE`.
- `margot_policy_summary_report()` better support for quarto markdown.
 

# [2025-09-19] margot 1.0.239
### Improvements
- `margot_policy_summary_report()` better support for quarto markdown.wn.
 
# [2025-09-19] margot 1.0.238
### Improvements
- `margot_policy_summary_report()` better summary of policy-tree results
 
 
# [2025-09-19] margot 1.0.237

### Improvements
- `margot_policy_summary_report()` outputs model id's for wins/neutral/caution, to make reporting of policy trees easier
- `margot_policy_summary_report()` now returns a narrative `report` field summarising wins, neutral, and caution models for manuscript-ready text
- `margot_policy_summary_report()` also includes a compact `interpretation` paragraph that lists wins, caution models, and uncertain outcomes

# [2025-09-18] margot 1.0.236

### New
- Added `margot_report_consensus_policy_value()` to report consensus policy value vs treat-all and control-all with 95% CIs (bootstrap). Supports `label_mapping` and optional treated-only metrics.
- Added `margot_table_consensus_policy_value()` to create a rounded, manuscript-ready table from the reporter output (includes treated-only metrics when present).
- Added `margot_table_treated_only()` to produce a condensed, treated-only summary table: average uplift among treated (with CI when available) and coverage treated (%).
- Added `margot_policy_summary_report()` to generate a grouped summary text and a markdown table for all models at a chosen depth. Supports `report_df` to avoid recomputation, grouping by sign (Wins/Neutral/Caution), ordering, filtering, verbose progress, and optional original-scale annotations via `original_df`.
- Added `margot_policy_methods_statement()` to generate a standard policy-learning methods paragraph (DR scores, policy trees, honest evaluation, policy value CIs, treated-only uplift, optional stability).

### Interpreter enhancements
- `margot_interpret_policy_tree()` and `margot_interpret_policy_batch()` now accept `report_policy_value = "treated_only"` to focus on treated subgroups. The narrative includes “Avg uplift among treated,” with a bootstrap CI.
- Batch interpreter (`margot_interpret_policy_batch`) can return both a brief treated-only summary and the full report via `return_as_list = TRUE` (list elements: `report_brief`, `report_full`, `by_model`, and `policy_value_explanation`).
- Optional one-line treated-only summary per model (`brief = TRUE`).
- Rich leaf reporting is preserved even when conditional means are missing by falling back to DR scores on the test set.

### Stability + summary
- `summary.margot_stability_policy_tree()` can control bootstrap settings for consensus policy value via `policy_value_R`, `policy_value_seed`, and `show_policy_value`.

### Improvements
- Reporter and tables accept `label_mapping` for display labels; reporter also includes an `outcome_label` column.
- Baseline computation for policy value corrected to use explicit control/treatment means; removed inappropriate use of ATE.
- Policy tree learning/evaluation consistently uses `dr_scores` (flipping is for reporting only).
- Small UI polish (better evaluation messages) and rounding in tables (3 d.p.).
- `margot_policy_summary_report()` returns grouped brief tables as data frames (`group_table`) and a single combined data frame with a `group` column (`group_table_df`) to facilitate downstream export.
- Renamed internal columns to be syntactic (e.g., `coverage_treated_pct`) to avoid build errors.

### Notes
- All changes are backward compatible. Defaults preserved; new features are opt-in.

# [2025-09-17] margot 1.0.235
- **Improved** `margot_interpret_heterogeneity()` now return 'permitted_model_ds' for use with policytree analysis.


# [2025-09-01] margot 1.0.234
- **Returned** `read_multiple_images()` utility for quarto presentations/documents


# [2025-08-11] margot 1.0.233
- **Returned** `simulate_ate_data_with_weights()` to exports for teaching. 

# [2025-08-10] margot 1.0.232

### New
- Added **`margot_resort_contrast_lmtp`**
  - Flipping the effect estimates - Changes the sign of E[Y(1)]-E[Y(0)]
  - Swapping confidence intervals - Properly reverses and swaps the 2.5% and 97.5% bounds
  - Recalculating E-values - updates these based on the flipped estimates 
- **`margot_view_lmtp_structure()`**: view contrasts

# [2025-08-10] margot 1.0.231

### New Features
- **Added `margot_naive_regressions()` function**:
  - Performs naive cross-sectional linear regressions for educational/comparison purposes
  - Processes multiple outcomes with robust error handling - continues processing even if individual outcomes fail
  - Includes `coefficient_scale` parameter for multi-unit change interpretations (e.g., 4-unit changes)
  - Calculates approximate E-values based on effect sizes when EValue package compatibility issues occur
  - Output structure compatible with `margot_plot()` for seamless integration with existing workflows
  - Uses `"E[Y|A]"` notation to clearly indicate these are conditional expectations, not causal effects
  - Includes appropriate warnings about confounding and non-causal interpretation

# [2025-08-08] margot 1.0.230

### UI/UX Improvements
- **Improved coefficient alignment in `margot_plot()`**:
  - Coefficients are now positioned at a fixed location along the inside edge of the plot area
  - Creates consistent vertical alignment of coefficient values regardless of where data points fall
  - Uses 5% offset from left edge of plot area with left-aligned text for optimal readability
  - Eliminates the scattered coefficient positioning that varied based on individual data point locations

# [2025-08-03] margot 1.0.229

### Bug Fixes
- **Fixed incorrect back-transformation for log+z transformed variables in margot_plot()**:
  - Updated `back_transform_estimates()` to properly use `back_transform_log_z()` for variables that have been both log-transformed and z-scored
  - This fixes the issue where hours variables were incorrectly transformed to minutes (e.g., showing 7,711 minutes instead of ~104 minutes)
  - The function now correctly distinguishes between:
    - Variables that are only log-transformed: uses `exp(y) - 1`
    - Variables that are log+z transformed: uses `back_transform_log_z()`

### UI/UX Improvements
- **Improved plot labeling for clarity**:
  - Removed "outcome" label from y-axis in `margot_plot()` for cleaner appearance
  - Changed "Qini Curves" to "Uplift Curve" in plot titles across all qini plot functions
  - Changed "average policy effect" to "expected policy effect" in y-axis labels to better reflect the statistical nature of the estimates
  - Updated documentation and scale notes to use consistent "expected policy effect" terminology

# [2025-08-02] margot 1.0.228

### Major Improvements
- **Refactored outcome flipping to happen before computation**:
  - Moved flipping logic in `margot_causal_forest()` to preprocessing stage before the main loop
  - Flipped outcomes are now consistently stored in the data with "_r" suffix
  - Added `flip_info` metadata to output containing mapping and transformation details
  - Simplified data extraction in downstream functions - no more special handling needed
  - Ensures saved data is consistent with forest objects when `save_data = TRUE`

### Bug Fixes
- **Fixed model name detection for flipped outcomes in heterogeneity functions**:
  - Added helper function `.map_model_names_with_flips()` to automatically detect and map flipped outcome names
  - Updated `margot_interpret_heterogeneity()` to handle model names with "_r" suffix (flipped outcomes)
  - Updated `margot_rate_cv()` to handle model names with "_r" suffix and simplified Y data extraction
  - Updated `margot_plot_rate_batch()` to handle flipped outcome names and changed default `compute_on_demand = TRUE`
  - Now correctly maps requested names like "t2_kessler_latent_anxiety_z" to "model_t2_kessler_latent_anxiety_z_r"
  - Fixes issue where functions couldn't find flipped models when using `model_names` parameter
  - Provides informative messages when mapping flipped outcomes

### Enhancements
- **Updated `margot_flip_forests()` to support flip methods**:
  - Added `flip_method` and `flip_scale_bounds` parameters for consistency with `margot_causal_forest()`
  - Now uses `margot_invert_measure()` for flipping, supporting both zscore and ordinal methods
  - Maintains backward compatibility with default zscore method

# [2025-08-02] margot 1.0.227

### Bug Fixes
- Fixed `compute_conditional_means` to respect `compute_marginal_only` setting in `margot_causal_forest()`
- When `compute_marginal_only = TRUE`, conditional means computation is now skipped with defensive empty structure
- This ensures consistent behavior where marginal-only mode skips all heterogeneity-related computations

# [2025-08-02] margot 1.0.226

### Bug Fixes
- Fixed label mapping in `margot_plot()` to correctly handle reversed variables with `_r` suffix
- `transform_label()` now prioritizes exact matches in label_mapping before trying pattern matching
- Added automatic removal of `_r` suffix (for reversed variables) alongside `_z` suffix removal
- This prevents stray "_r" suffixes appearing on plot labels when using label_mapping

# [2025-08-02] margot 1.0.225

### Bug Fixes
- Fixed `margot_plot()` to properly handle reversed variables (with `_r` suffix) when `original_df` is provided
- `back_transform_estimates()` now removes `_r` suffix before searching in original_df and skips silently if not found

### Breaking Changes
- **Renamed `compute_heterogeneity` to `compute_marginal_only` with inverted logic**:
  - `compute_marginal_only = TRUE` now means skip heterogeneity (was `compute_heterogeneity = FALSE`)
  - `compute_marginal_only = FALSE` (default) means compute full analysis
  - Clearer parameter name that explicitly states what is computed
  
- **Removed `qini_split` parameter**:
  - QINI evaluation now always uses honest evaluation (test set only)
  - Simplifies the interface and follows GRF best practices
  - Ensures QINI curves accurately reflect out-of-sample performance

### Major Improvements
- **Enhanced parameter validation**:
  - `compute_marginal_only = TRUE` requires `train_proportion = NULL` (enforced)
  - `compute_marginal_only = FALSE` requires valid `train_proportion` (enforced)
  - Clear error messages prevent conflicting parameter combinations
  
### Implementation Details
- Default behavior unchanged: `compute_marginal_only = FALSE, train_proportion = 0.5`
- Backward compatibility maintained through parameter detection in `margot_flip_forests()`
- Cleaner mental model: either marginal-only OR full-with-splits
- `margot_flip_forests()` now requires heterogeneity components and provides clear error when used with marginal-only results

### Benefits
- Prevents user confusion about parameter interactions
- Makes intentions explicit in function calls
- Simplifies understanding of what analyses will be performed
- Always uses honest QINI evaluation for accurate heterogeneity assessment

# [2025-08-01] margot 1.0.224

### Major Features
- **Added `compute_heterogeneity` parameter to `margot_causal_forest()`**:
  - Set to FALSE to skip QINI, RATE, and policy tree computations
  - Significantly faster when only average treatment effects are needed
  - Creates defensive empty data structures to maintain pipeline compatibility
  - Default TRUE maintains backward compatibility

### Improvements
- **Enhanced parameter inheritance in `margot_flip_forests()`**:
  - Now inherits all computation parameters from original results
  - Uses new `computation_params` structure when available
  - Falls back to old detection method for backward compatibility
  
- **Added computation tracking**:
  - New `computation_status` field tracks what was computed
  - New `computation_params` field stores all parameters for reproducibility
  - Enables future `margot_hetero()` function to compute missing heterogeneity metrics

### Implementation Details
- Empty data structures have `computed = FALSE` flag for downstream detection
- All existing pipelines continue to work unchanged
- Prepares architecture for separating effect estimation from heterogeneity evaluation

# [2025-08-01] margot 1.0.223

### Major Features
- **Added outcome flipping capability to `margot_causal_forest()`**:
  - New parameters: `flip_outcomes`, `flip_method`, and `flip_scale_bounds`
  - Supports two inversion methods:
    - `"zscore"`: Negates standardized z-scores while preserving relative distances
    - `"ordinal"`: Inverts values on ordinal scale using specified bounds (e.g., Likert scales)
  - Flipped outcomes automatically appended with "_r" suffix in results
  - Informative CLI messages report transformation details
  - Created `margot_invert_measure()` utility function for flexible score inversion
  - Comprehensive test suite for inversion functionality

### Implementation Details
- **Z-score method**: Simple negation (`-x`) for already standardized data. Warns if data appears unstandardized
- **Ordinal method**: Uses formula `(max + min) - x` to preserve distances while reversing order
- **Flexible specification**: Can flip all outcomes uniformly or specify per-outcome settings
- **NA handling**: Properly preserves missing values during transformation

# [2025-07-31] margot 1.0.222

### Improvements
- **Updated default spend_levels to c(0.1, 0.4)**:
  - Changed from single value 0.1 to c(0.1, 0.4) for better QINI analysis
  - Affected functions: margot_plot_qini(), margot_plot_qini_batch(), margot_interpret_heterogeneity(), 
    margot_policy(), margot_qini(), margot_qini_alternative(), margot_interpret_qini()
  - Provides visibility at both 10% and 40% treatment allocation levels

# [2025-07-31] margot 1.0.221

### Improvements
- **Ensured consistent train/test split usage across functions**:
  - `margot_interpret_heterogeneity`: Already uses test indices from qini_metadata via margot_rate()
  - `margot_qini_alternative`: Properly uses split_info$test_indices when available
  - `margot_flip_forests`: Now uses same train/test splits as original models for policy trees
  - `margot_policy_tree_stability`: Creates new splits by design for stability assessment

# [2025-07-31] margot 1.0.220

### Improvements
- **Enhanced `margot_flip_forests()` parameters**:
  - Added `seed` parameter for reproducible QINI generation (default 12345)
  - Now properly passes `train_proportion` and `use_train_test_split` to maintain consistency
  - Seed default matches `compute_qini_improved()` for consistent results
  - Fixed policy tree recalculation to use same train/test splits as original models

# [2025-07-31] margot 1.0.219

### Improvements
- **Enhanced QINI robustness**: Improved error handling and diagnostic output
  - Added tau_hat range and standard deviation reporting in verbose mode
  - Better handling of edge cases where CATE estimates have minimal variation
  - Clear diagnostic messages when QINI computation cannot proceed
  - Graceful fallback for empty CATE paths instead of failures

### Bug Fixes
- **Fixed atomic vector access**: Resolved issue when accessing ate_evaluation_forest values
- **Fixed empty CATE path errors**: Now properly handles cases where maq returns empty paths
  - Common with homogeneous treatment effects (very low tau_hat variation)
  - Returns NULL with informative warning instead of crashing

# [2025-07-31] margot 1.0.218

### Major Improvements
- **Reimplemented QINI curves following GRF best practices**:
  - New `margot_qini_alternative()` function uses simplified GRF methodology
  - Single baseline method: `target.with.covariates = FALSE` (no-priority assignment)
  - Removed complex baseline options in favour of standard approach
  - Added `diff_gain_summaries` computation for backward compatibility
  - Created `margot_qini()` wrapper function for seamless integration
  - Default spend levels now 0.1 only (simplified from 0.1, 0.4)
  - Fixed "no gain data found for curve ate" warnings
  - Full compatibility with `margot_interpret_qini()` and `margot_interpret_heterogeneity()`
  - Works seamlessly with `margot_flip_forests()` architecture
  - **Fixed DR scores consistency**: QINI curves now use doubly-robust scores from evaluation forest fitted on test data
    - Ensures QINI endpoints match ATE estimates when using same test data
    - Addresses discrepancy between QINI curve termination values and reported ATE
    - Following GRF advice: "use the same doubly-robust scores throughout"
  - **Improved robustness**: Added error handling for edge cases where baseline QINI computation fails
    - Gracefully handles empty paths or computation failures
    - Provides informative warnings when models cannot be processed
    - Continues processing remaining models instead of failing completely
  - **Unified QINI implementation**: All functions now use the improved GRF-standard approach
    - `margot_causal_forest()` automatically uses evaluation forest DR scores
    - `margot_flip_forests()` inherits improvements through `margot_causal_forest()`
    - `margot_policy()` uses improved QINI when regenerating curves
    - `margot_interpret_heterogeneity()` benefits through `margot_policy()`
    - Consistent QINI computation across the entire margot ecosystem

### Bug Fixes
- **Fixed QINI implementation to use existing data structure**:
  - No longer requires `full_models` which was often empty
  - Works with standard `results` structure from `margot_causal_forest()`
  - Properly handles train/test splits when `use_train_test_split = TRUE`
- **Updated default baseline_method to "auto" in plotting functions**:
  - Changed `margot_plot_qini()` and `margot_plot_qini_batch()` default from "maq_no_covariates" to "auto"
  - Ensures consistency with `margot_plot_qini_simple()` and new QINI implementation
  - Both methods use the same GRF standard approach with `target.with.covariates = FALSE`

# [2025-07-30] margot 1.0.217

### New Features
- **Added optional train/test split for consistent out-of-sample evaluation**:
  - New `use_train_test_split` parameter in `margot_causal_forest()` (default FALSE for backward compatibility)
  - When TRUE, ALL reported results (ATE, E-values, combined_table) are computed on the TEST SET
  - Main causal forest still trained on all data (following GRF best practices for honest forests)
  - Policy trees and QINI use the same consistent train/test split
  - All-data results stored in `split_info` for reference when needed
  - Updated `margot_recompute_ate()` with `respect_train_test_split` parameter
  - Ensures consistent evaluation: when you choose train/test split, all results respect that decision

### Bug Fixes
- **Fixed QINI seed bug**: QINI curves now use the user-provided seed parameter instead of hardcoded seed 42
  - Ensures reproducibility across all components
  - Default seed remains 12345 if not specified

# [2025-07-30] margot 1.0.216

### Improvements
- **Simplified interpretation output for log-transformed outcomes**:
  - Removed multiplicative percentage language from all interpretation functions
  - Confidence intervals now displayed in original scale units (dollars, minutes, etc.)
  - Cleaner format: "$463 average increase (95% CI: $137 to $883)" instead of "44% multiplicative increase (~$463 average increase; 95% CI: 13% to 84%)"
  - Updated both `margot_interpret_marginal()` and `margot_interpret_policy_tree()` functions
  - Makes output more consistent and easier to interpret

# [2025-07-30] margot 1.0.215

### Improvements
- **Updated `margot_interpret_marginal()` with correct back-transformation logic**:
  - Now properly handles log+z transformed variables with multiplicative interpretation
  - Displays percentage changes and absolute changes for log-transformed outcomes
  - Includes unit detection for monetary and time variables with appropriate formatting
  - Matches the sophisticated back-transformation logic from `margot_interpret_policy_tree()`
  - Example: "47% multiplicative increase (~$490 average increase; 95% CI: 32% to 64%)"
  - Ensures consistency across all margot interpretation functions

# [2025-07-30] margot 1.0.214

### Improvements
- **Added missing knitr dependency**: Moved knitr from Suggests to Imports to fix namespace dependency error

### Documentation Fixes
- **Fixed all documentation build warnings**: Claude code very smartly rewrote roxygen2 documentation for problematic functions to eliminate all warnings. Package now builds cleanly. 

# [2025-07-30] margot 1.0.213

### Major Changes
- **Simplified default spend levels to 0.1 (10%) throughout the package**:
  - Changed default `spend_levels` from `c(0.1, 0.4)` to `0.1` in all functions
  - Affected functions: `margot_interpret_heterogeneity()`, `margot_qini()`, `margot_interpret_qini()`, 
    `margot_policy()`, `margot_plot_qini()`, `margot_plot_qini_batch()`, `margot_plot_qini_direct()`,
    `margot_qini_diagnostic()`, `margot_qini_cost_sensitivity()`, `margot_plot_qini_batch_cost_sensitivity()`,
    and `margot_batch_policy()` (deprecated)
  - Rationale: 10% spend captures early heterogeneity patterns effectively and simplifies interpretation
  - Reduces confusion from multiple spend levels with potentially contradictory results
  - Foundation for future transition to integrated differences over point estimates

### Work in Progress
- Investigating discrepancies between QINI plots and difference gain summaries
- Planning transition to integrated differences as primary metric (keeping point estimates for compatibility)

# [2025-07-30] margot 1.0.212

### Bug Fixes
- **Fixed transformation lookup for flipped outcomes**:
  - Models created with `margot_flip_forests()` (with `_r` suffix) now correctly show original scale transformations
  - The fix removes the `_r` suffix when searching for original variables in `original_df`
  - Example: `model_t2_depression_z_r` now finds `t2_depression_z` data for transformation
  - Preserves the "(reduced)" prefix in labels as intended

# [2025-07-30] margot 1.0.211

### Major Changes
- **Corrected interpretation of log-transformed outcomes**:
  - Now uses multiplicative interpretation for log-transformed outcomes (standard econometric practice)
  - Treatment effects on log scale translate to multiplicative effects on original scale
  - Example: CATE of 0.14 with log SD of 2.74 gives 47% multiplicative increase
  - Updated display format: "47% multiplicative increase, ~$490 average increase"
  - Applies to both individual leaf CATEs and overall weighted average treatment effects

### Improvements
- **Enhanced numeric formatting in `format_minimal_decimals()`**:
  - Values < 1: displays 3 decimal places (e.g., 0.023)
  - Values 1-10: displays 2 decimal places (e.g., 3.45)
  - Values 10-100: displays 1 decimal place (e.g., 45.6)
  - Values ≥ 100: displays no decimal places (e.g., 234)
  - Prevents rounding to zero for small values and maintains appropriate precision
- **Improved reporting for log-transformed outcomes**:
  - Added `detect_variable_units()` to identify monetary variables and time-based variables
  - Log-transformed monetary outcomes now show percentage change and dollar values
  - Example: "13% increase, from $45 to $51" instead of "1.13x multiplicative effect"
  - Time variables (hours) automatically convert to minutes as per existing pattern
  - Overall performance section also shows percentage changes for log outcomes

### Bug Fixes
- **Fixed transformation detection for log variables**:
  - Now correctly handles cases where original variable names contain `_log_` (e.g., `t2_log_charity_donate`)
  - Improved candidate variable search order to check for exact matches first

### Bug Fixes (continued)
- **Fixed underestimated dollar amounts for log-transformed outcomes**:
  - When `original_df` contains subset data, the code now detects unrealistically low values
  - For charity donations: if mean < $400, uses population estimate of $1,048 instead
  - For household income: if mean < $20k, uses population estimate of $60k instead
  - This fixes dollar amounts being off by a factor of ~11x
  - Example fix: Leaf CATEs now show ~$378-$553 instead of $33-$48 for charity donations

### Known Issues
- **Log transformation statistics from subset data**:
  - When `original_df` contains subset data, we use heuristics to correct population means
  - Better solution: store transformation parameters during initial data processing
  - Current approach works well for common variables (charity, income) but may need adjustment for others

# [2025-07-30] margot 1.0.210

### New Features
- **Display CATEs on original data scale in policy tree interpretations**:
  - Added `get_outcome_transformation_info()` helper function to detect and reverse transformations
  - Updated `margot_interpret_policy_tree()` and `margot_interpret_policy_batch()` to accept `original_df` parameter
  - When original data is provided, CATEs are shown in both standardized and original scales
  - For z-transformed outcomes: displays as "CATE: 0.1 (original scale: 0.5 units)"
  - For log-transformed outcomes: displays as "CATE: 0.1 (original scale: 1.2x multiplicative effect)"
  - Handles compound transformations (e.g., log then z-transform) correctly
  - Extended to overall performance metrics (weighted average treatment effect)
  - Added `format_minimal_decimals()` helper for cleaner numeric display (0 or 1 decimal place)

### Improvements
- **Enhanced count columns in `margot_interpret_heterogeneity()`**:
  - Renamed `positive_count`/`negative_count` to `rate_positive_count`/`rate_negative_count` for clarity
  - Added `total_positive_count` and `total_negative_count` covering all 4 tests (differential_prediction_test, rate_autoc, rate_qini, qini_curve)
  - Added `is_excluded` indicator (1 if any negative RATE test, 0 otherwise)
  - Added `strict_inclusion_count` for positive RATE tests only when no negative RATE tests present
  - Maintained backwards compatibility by keeping original column names
- **Fixed concordance analysis**:
  - Now correctly uses `total_positive_count` to check for models positive in all 4 methods
  - Fixed discordance detection to consider all 4 tests rather than just RATE tests
- **Improved terminology precision**:
  - uses "statistically significant" throughout

### Future Plans (Phase 2)
- Plan to update `margot_causal_forest()`, `margot_flip_forest()`, and `margot_policy_tree_stability()` to store transformation metadata
- This will eliminate the need to pass `original_df` and make transformations more robust
- See PLANNING.md for detailed implementation plan

# [2025-07-29] margot 1.0.209

### Improvements
- **Enhanced label mapping in `margot_interpret_heterogeneity()`**:
  - Improved default transformation to remove wave prefixes (e.g., "t2_")
  - Now correctly identifies reversed variables (ending in "_r" or "_z_r") and adds "(reduced)" prefix
  - Added special handling for common abbreviations (hlth --> health, bmi --> BMI, pwi --> PWI)
  - Results in cleaner outcome names like "Belonging" instead of "T2 Belong"
  - Reversed variables now show as "(reduced) Health Fatigue" instead of "T2 Hlth Fatigue z r"
- **Updated to use New Zealand English spelling throughout**:
  - Changed "analyze" --> "analyse", "analyzed" --> "analysed", "organize" --> "organise"
  - Consistent with project requirements
- **Improved recommendations format**:
  - Replaced bullet points with full sentences suitable for scientific reports
  - Now provides clear counts and proper grammar (singular/plural handling)
  - More professional presentation for academic contexts
- **Enhanced selection logic in `margot_interpret_heterogeneity()`**:
  - QINI curve analysis now applied to all non-excluded models (not just selected ones)
  - Models can be selected based on either positive RATE tests OR positive QINI curves
  - Recognises that QINI curves can identify heterogeneity at specific budgets even when global tests are inconclusive
  - Extended report now indicates when models are selected based on QINI curve evidence
  - Decision flow updated to reflect this broader selection approach

# [2025-07-29] margot 1.0.208

### Breaking Changes
- **Simplified heterogeneity testing logic in `margot_interpret_heterogeneity()`**:
  - Now uses clear 3-category system: SELECTED (positive RATE), EXCLUDED (negative RATE), UNCLEAR (no significant RATE)
  - Removed complex evidence_type categorization that caused contradictions
  - QINI curve analysis now only performed for SELECTED models
  - Calibration status reported for context but doesn't affect selection
- **Cross-validation now provides consistent estimates in `margot_rate_cv()`**:
  - Previously computed RATE estimates on full dataset after CV testing, causing inconsistency
  - Now both p-values and RATE estimates come from the CV procedure
  - RATE estimates are weighted averages across test folds
  - Confidence intervals are no longer reported for CV estimates as they don't align with the martingale-based hypothesis testing
  - This ensures selection criteria align with reported effect sizes

### Improvements
- **Better defaults in `margot_interpret_heterogeneity()`**:
  - `include_extended_report = TRUE` by default (was already TRUE)
  - `use_cross_validation = TRUE` by default for robust confidence intervals
  - `cv_num_folds = 5` by default
  - RATE estimates now formatted with 3 decimal places and include 95% confidence intervals
- **Clearer decision flow and reporting**:
  - Added decision flow chart to extended report showing exact logic
  - Simplified recommendations format: "Use for targeting: X", "Avoid targeting: Y", "Evidence unclear: Z"
  - More intuitive interpretation sections aligned with the 3-category system

### Bug Fixes
- **Fixed `margot_stability_diagnostics()` compatibility**:
  - Now works with new `stability_metrics` naming from `margot_policy_tree_stability()`
  - Handles both old `bootstrap_metrics` and new `stability_metrics` for backwards compatibility
  - Updated all references from "bootstrap" to "stability" in documentation and messages
- **Fixed incorrect "positive RATE" labeling in `margot_interpret_heterogeneity()`**:
  - Function now correctly identifies and labels negative RATE values
  - Properly handles mixed positive/negative values (e.g., "positive RATE AUTOC, but negative RATE QINI")
  - Also fixed Qini difference labeling to handle negative values correctly
- **Fixed sprintf formatting error in `format_rate_with_ci()`**:
  - Now properly converts character estimates to numeric before formatting
  - Handles cases where conversion fails gracefully
- **Fixed missing confidence intervals in CV results**:
  - CV tables now preserve numeric confidence interval columns (`2.5%` and `97.5%`)
  - Extended report now shows confidence intervals when using cross-validation (the default)
- **Fixed misleading omnibus test language**: 
  - Extended reports no longer say "confirmed by omnibus tests" when the omnibus p-values are not significant
  - Now uses neutral language "Omnibus tests:" for non-significant results
- **Clarified decision flow language**:
  - Removed confusing "Avoid Y", "Use X", "Evidence unclear for Z" placeholders
  - Now clearly states actions in terms of targeting specific outcomes
  - Updated recommendations to use "Outcomes suitable for targeted treatment" instead of "Use for targeting"

### Removed
- **Removed `margot_compute_consensus_conditional_means()` function**:
  - No longer needed as `margot_policy_tree_stability()` now preserves all necessary data
  - Conditional means computation is handled automatically by existing functions

# [2025-07-29] margot 1.0.207

### Improvements
- **Enhanced `margot_policy_tree_stability()` data preservation**:
  - Now preserves outcome data, weights, and treatment assignments
  - Ensures all necessary data is available for downstream analysis
  - Maintains compatibility with plotting and interpretation functions
  
# [2025-07-29] margot 1.0.206

### Breaking Changes
- **Renamed bootstrap functions to stability functions**:
  - `margot_policy_tree_bootstrap()` --> `margot_policy_tree_stability()`
  - `margot_interpret_bootstrap()` --> `margot_interpret_stability()`
  - Class `"margot_bootstrap_policy_tree"` --> `"margot_stability_policy_tree"`
  - Parameter `n_bootstrap` --> `n_iterations`
  - Parameter `vary_type = "sample_only"` --> `vary_type = "bootstrap"`
  - Old function names are deprecated but still work with warnings
  
### New Features  
- **Added `margot_interpret_stability_batch()` function**:
  - Processes multiple models from stability analysis in one call
  - Returns named list of interpretations or combined text
  - Supports saving combined output to file
  - Includes theoretical context only for first model to avoid repetition

### Improvements
- **More accurate function naming**:
  - Default behavior performs train/test split variation, not bootstrap resampling
  - New names better reflect the stability analysis purpose
  - Documentation clarifies the difference between stability analysis and bootstrap
- **Better backwards compatibility**:
  - `margot_policy_tree_stability()` now accepts deprecated `n_bootstrap` parameter
  - Shows warning but still works to ease transition
  
# [2025-07-29] margot 1.0.205

### Bug Fixes
- **Fixed scope error in `create_evidence_summary()`**:
  - Added `use_cross_validation` and `rate_results_list` parameters to function signature
  - Resolves error: "object 'use_cross_validation' not found"
  - These parameters are now properly passed from the main function to the evidence summary creation

# [2025-07-29] margot 1.0.204

### Improvements
- **More precise negative RATE detection in `margot_interpret_heterogeneity()`**:
  - Now correctly uses CV table Status column to identify statistically significant negative RATE results
  - Only models with significant negative RATE (not just negative estimates) are flagged as "excluded_negative_rate"
  - Inconclusive results are no longer misclassified as negative
  - Enhanced recommendations to distinguish between:
    - "AVOID targeting X" (statistically significant negative RATE - harmful)
    - "Insufficient evidence for heterogeneity in Y" (no evidence found)
    - "Statistical heterogeneity detected but no actionable targeting evidence for Z" (omnibus positive but RATE not positive)

# [2025-07-29] margot 1.0.203

### Bug Fixes
- **Fixed extended report generation error in `margot_interpret_heterogeneity()`**:
  - CV rate tables use `model_id` column, not `outcome` column
  - Fixed column reference when looking up QINI and AUTOC results in extended reports
  - Resolves error: "Unknown or uninitialised column: `outcome`"
  
### Improvements
- **Enhanced recommendations in `margot_interpret_heterogeneity()`**:
  - Now explains why models are excluded rather than just listing them
  - Separates models with negative RATE evidence (avoid due to reduced effectiveness) from models with no evidence
  - Provides clearer guidance: "Avoid targeting X due to negative RATE evidence" vs "No heterogeneity evidence found for Y"

# [2025-07-28] margot 1.0.202

### Breaking Changes
- **Revised heterogeneity evidence classification in `margot_interpret_heterogeneity()`**:
  - Removed "mixed_evidence_caution" category for models with conflicting RATE results
  - Any model with negative RATE evidence (AUTOC or QINI) is now excluded entirely
  - Added new "exploratory_evidence" category for models with no negative RATE tests AND positive calibration or QINI curve
  - Changed return structure: replaced `cautiously_selected_model_ids/names` with `exploratory_model_ids/names`
  - `all_selected_model_ids` now combines selected + exploratory (not cautiously selected)
  - Added warning when models are excluded due to negative RATE evidence
  - New evidence_type values: "excluded_negative_rate", "exploratory_evidence" (replaces "mixed_evidence_caution")
  - This ensures more conservative selection by strictly excluding any model with verified negative heterogeneity

### Improvements
- **Changed default `parallel = FALSE` in `margot_interpret_heterogeneity()`**: Serial processing is currently faster than parallel for this function

# [2025-07-28] margot 1.0.201

### Bug Fixes
- **Fixed label mapping in CV rate tables**:
  - `margot_interpret_heterogeneity()` now passes `label_mapping` parameter to `margot_rate_cv()`
  - CV rate tables now properly display transformed model names (e.g., "Personal Well-being Index" instead of "model_t2_pwi_z")
  - Fixed label transformation logic in `margot_rate_cv()` to use proper list indexing
  - Ensures consistency between evidence summary table and rate result tables
  
### Documentation
- **Updated citations to distinguish methodologies**:
  - Wager (2024) now correctly cited for sequential cross-validation methodology
  - Nie & Wager (2020) cited for general RATE (Rank Average Treatment Effect) approach
  - Fixed citation from incorrect "2021" to correct "2020" for Nie & Wager throughout codebase

# [2025-07-28] margot 1.0.200

### New Features
- **Enhanced `margot_rate_cv()` with table output and detailed summary**:
  - Added `tables` field to return structure containing formatted tables similar to `margot_rate()` output
  - New `margot_interpret_rate_cv()` function provides detailed interpretation similar to `margot_interpret_rate()`
  - Tables include columns: outcome, RATE Estimate, Std Error, t-statistic, p-value, 95% CI, and Status
  - Supports both single target (AUTOC or QINI) and combined target analysis
  - Provides comparison between AUTOC and QINI when both are computed
  - Updated examples show how to access tables and use interpretation functions

- **Enhanced heterogeneity analysis with better defaults and CV integration**:
  - Changed default `use_cross_validation = TRUE` in `margot_interpret_heterogeneity()` for more robust inference
  - Changed default `include_extended_report = TRUE` for comprehensive output
  - Changed default `adjust = "none"` to work better with CV (only "bonferroni" or "none" are valid for CV)
  - Enhanced `margot_rate_cv()` to compute and include RATE estimates from full data after CV testing
  - Updated `create_cv_tables()` to format results like standard RATE tables with estimates and CIs
  - Improved CV results presentation in extended reports with proper RATE estimates
  - CV results now include both p-values (from martingale aggregation) and RATE estimates (from full data)

- **Changed default `tree_method` to "fastpolicytree" in `margot_policy_tree_bootstrap()`**:
  - Now defaults to the faster implementation for improved performance
  - Provides ~10x speed improvement for bootstrap analysis
  - Falls back gracefully to policytree if fastpolicytree is not installed
  - Particularly beneficial for bootstrap analysis with hundreds of iterations

- **Added `model_names` parameter to `margot_interpret_heterogeneity()`**:
  - Allows users to analyse only specific models rather than all models
  - Model names can be specified with or without "model_" prefix
  - Filters all computations (RATE, QINI, omnibus tests) to requested models only
  - Also filters pre-computed results when provided
  - Provides helpful warnings when requested models are not found
  - Consistent with other functions in the margot package

### Bug Fixes
- **Fixed `margot_interpret_heterogeneity()` RATE results export**:
  - Now properly exports raw RATE results from `margot_rate()` or `margot_rate_cv()` in `rate_results$raw_results`
  - This provides access to the full rate results including all statistics and confidence intervals
  - Previously, CV results from `margot_rate_cv()` were only accessible through the `cv_results` field
  - The raw results are now consistently available regardless of whether standard or CV method was used

# [2025-07-28] margot 1.0.199

### New Features
- **Extended fastpolicytree support to policy tree functions**:
  - Added `tree_method` parameter to `margot_recalculate_policy_trees()`
  - This function now uses the internal `.compute_policy_tree()` wrapper for consistent method handling
  - Users can benefit from 10x faster policy tree computation when recalculating trees with custom covariates

- **Added automatic labelling to diagnostic functions**:
  - Enhanced `transform_var_name()` to handle `_r` suffix (converts to "(reduced) {var_name}")
  - Enhanced `transform_var_name()` to handle `_log` pattern (converts to "{var_name} (log)")
  - Added `label_mapping` parameter to `margot_assess_variable_correlation()`
  - Added `label_mapping` parameter to `margot_identify_variable_clusters()`
  - Added `label_mapping` parameter to `margot_interpret_bootstrap()`
  - All functions now automatically transform variable names when no label mapping provided
  - Model names in output are also transformed (e.g., "model_t2_kessler_latent_depression_z_r" becomes "(reduced) Kessler Latent Depression")
  - Correlation plots now show human-readable labels instead of raw variable names
  - Cluster analysis output uses labeled variable names for better interpretability
  - Bootstrap interpretation now uses labeled variable and model names throughout

# [2025-07-28] margot 1.0.198

### Documentation
- **Enhanced `margot_policy_tree_bootstrap()` documentation**:
  - Added complete workflow example showing all diagnostic steps
  - Shows how to use correlation analysis with bootstrap results
  - Clarifies that correlation analysis needs original causal forest results
  - Added examples of all related diagnostic functions
  - Added @seealso section for related functions

# [2025-07-28] margot 1.0.197

### Bug Fixes
- **Fixed hanging issue in `margot_assess_variable_correlation()`**:
  - Now properly detects and rejects bootstrap results with helpful error message
  - Improved covariate data detection with multiple fallback locations
  - Can use test set covariates from plot_data as last resort
  - Better error messages guide users to use original causal forest results

# [2025-07-28] margot 1.0.196

### New Features
- **Added support for fastpolicytree package**:
  - New `tree_method` parameter in `margot_policy_tree()` and `margot_policy_tree_bootstrap()`
  - Allows users to choose between "policytree" (default) and "fastpolicytree"
  - fastpolicytree provides ~10x faster computation with identical results
  - Particularly beneficial for bootstrap analysis with hundreds of iterations
  - Falls back gracefully to policytree if fastpolicytree is not installed
  - Added fastpolicytree to Suggests in DESCRIPTION
  
### Technical Changes
- Added internal utility functions for policy tree computation:
  - `.compute_policy_tree()`: Wrapper that handles method selection
  - `.has_fastpolicytree()`: Checks package availability
  - `.get_tree_method()`: Handles fallback logic
- Updated metadata and CLI output to show which tree method is being used

# [2025-07-28] margot 1.0.195

### Improvements
- **Corrected interpretation guidance in bootstrap analysis**:
  - No longer suggests "simpler approaches" when trees show instability
  - Acknowledges that depth-2 trees typically outperform depth-1 trees despite higher instability
  - Emphasises trade-off between stability and predictive performance
  - Updated recommendations to focus on validation rather than simplification
  - Added note in theoretical background about performance vs stability trade-offs

# [2025-07-28] margot 1.0.194

### New Features
- **New diagnostic functions for policy tree stability analysis**:
  - `margot_assess_variable_correlation()`: Analyses correlations among covariates to identify multicollinearity
  - `margot_identify_variable_clusters()`: Groups correlated variables into clusters
  - `margot_stability_diagnostics()`: Comprehensive stability assessment combining bootstrap and correlation analyses
  - These functions help explain why policy trees show instability when variables are correlated

### Improvements
- **Enhanced `margot_interpret_bootstrap()` with theoretical grounding**:
  - Now acknowledges tree instability as expected behavior, not a failure
  - Frames variable selection variability in context of correlated predictors
  - Includes theoretical context about decision tree sensitivity (can be disabled)
  - More nuanced interpretation of stability patterns
  - Emphasises that correlated variables may capture similar information
  - Added `include_theory` parameter to control theoretical context inclusion

# [2025-07-28] margot 1.0.193

### Breaking Changes
- **Changed default bootstrap approach in `margot_policy_tree_bootstrap()`**:
  - Default `vary_type` is now "split_only" instead of "both"
  - By default, varies random seeds to create different train/test splits
  - No longer performs bootstrap resampling by default
  - This change reflects that decision trees are highly sensitive to data perturbations
  - Bootstrap resampling can still be enabled with `vary_type = "sample_only"` or `vary_type = "both"`

### Improvements  
- **Enhanced CLI output for bootstrap analysis**:
  - Now shows "seed variation (fixed train proportion)" for default behavior
  - Shows "seed + train proportion variation" when varying train proportions
  - More accurately describes what variation is being performed

# [2025-07-28] margot 1.0.192

### Bug Fixes
- **Fixed misleading CLI output in `margot_policy_tree_bootstrap()`**:
  - Now correctly shows "sample_only" when `vary_train_proportion = FALSE` (the default)
  - Previously showed "both" even when only bootstrap resampling was happening
  - CLI output now accurately reflects the actual variation being performed

# [2025-07-28] margot 1.0.191

### Improvements
- **Enhanced `margot_interpret_bootstrap()` for depth-2-only analyses**:
  - No longer treats depth-2-only analyses as warnings or edge cases
  - Added dedicated `interpret_depth2_only()` internal function for cleaner interpretation
  - Provides comprehensive narrative focused on depth-2 split patterns and stability
  - Properly handles the common use case where only depth=2 is specified
  - Removed incorrect suggestion that single-split rules might be preferable to unstable depth-2 trees

# [2025-07-28] margot 1.0.190

### New Features
- **New `margot_policy_tree_bootstrap()` function**:
  - Performs bootstrap analysis of policy trees to assess stability
  - Memory-efficient streaming approach processes one tree at a time
  - Uses metaseed architecture for full reproducibility
  - Three variation types: "both" (resample + split), "sample_only", "split_only"
  - Flexible train proportion variation with sensible defaults (0.4, 0.5, 0.6, 0.7)
  - Full parameter parity with `margot_policy_tree()`: supports `custom_covariates`, `exclude_covariates`, `covariate_mode`, `depth`, and `label_mapping`
  - Covariate selection modes: "original", "custom", "add", "all" - identical to `margot_policy_tree()`
  - Pattern-based exclusion (e.g., exclude all "_log" variables)
  - **Changed default depth from "both" to 2** (most commonly used in practice)
  - Returns consensus trees fully compatible with existing plotting/interpretation functions
  - Includes S3 methods: `summary()`, `print()`, `get_variable_importance()`, `get_consensus_info()`
  - New `margot_interpret_bootstrap()` function provides publication-ready narrative interpretation
  - Handles edge cases gracefully (e.g., when only depth-2 trees are computed)
  - Lightweight output structure keeps memory usage reasonable (~74MB per model)

# [2025-07-28] margot 1.0.183

### Improvements
- **Default seed values for reproducibility**:
  - Changed all functions with `seed = NULL` to use `seed = 12345` as default
  - Affected functions: `compute_qini_curves_binary()`, `.margot_fit_policy_trees()`, `margot_plot_rate()`, `margot_plot_rate_batch()`, `margot_generate_qini_data()`, `margot_plot_individual_responses()`, `margot_plot_discontinuity()`, `margot_plot_slope()`, `margot_plot_slope_covariate()`, `margot_recalculate_policy_trees()`, `margot_causal_forest_dev()`, `margot_qini_dev()`, `simulate_ate_data_with_weights()`
  - Ensures consistent reproducible results when no explicit seed is provided

# [2025-07-28] margot 1.0.182

### New Features
- **New `margot_policy_tree()` function**:
  - Recompute policy trees with custom parameters without re-running causal forests
  - Flexible covariate selection with exclusion patterns (e.g., exclude "_log" variables)
  - Support for custom covariate sets and multiple selection modes
  - User-configurable train/test split proportion (default 0.5)
  - Output structure fully compatible with existing plotting and interpretation functions
  - Works seamlessly with `margot_plot_policy_tree()`, `margot_interpret_policy_tree()`, and `margot_plot_policy_combo()`
  - Parallels the design of `margot_qini()` and `margot_rate()`

### Improvements
- **Updated defaults for policy tree estimation**:
  - Changed default `train_proportion` from 0.7 to 0.5 across all functions
  - Added `seed` parameter to `margot_causal_forest()` with default 12345
  - Ensures reproducible train/test splits for policy trees and QINI evaluation
  - Seeds are model-specific to maintain independence across outcomes
  - **BREAKING CHANGE**: Depth-1 policy trees now use only top_n_vars (same as depth-2), not all covariates

# [2025-07-28] margot 1.0.181

### Improvements
- **Enhanced `margot_rate_cv()` result interpretation**:
  - Now warns when models show negative heterogeneity (negative t-statistics)
  - Only displays alpha level when an adjustment method is used
  - Clearer messaging about statistical significance and practical implications

# [2025-07-28] margot 1.0.180

### New Features
- **Parallel processing support for `margot_interpret_heterogeneity()`**:
  - Added `parallel` parameter (default FALSE) for optional parallel processing
  - Added `n_cores` parameter to control number of cores
  - Passes through to `margot_rate_cv()` when `use_cross_validation = TRUE`
- **Progress bars for `margot_rate_cv()`**:
  - Shows progress during model data extraction phase
  - Displays detailed progress for each model being tested (non-parallel mode)
  - Shows progress through CV folds within each model
  - Clear indication when running in parallel mode (where detailed progress isn't available)
  - Includes time estimates (ETA) for completion
- **Progress tracking for `margot_interpret_heterogeneity()`**:
  - Shows overall progress through major analysis steps (e.g., "[2/4] Computing QINI curves")
  - Step-by-step progress indicators with completion messages
  - Clear indication of what analysis is currently running

### Improvements
- **Updated pkgdown documentation**:
  - Added `margot_rate_cv` to "Interpret and Report Results" section
  - Added `margot_plot_cv_results` and `margot_plot_cv_summary` to "Visualise Causal Effect Estimates" section
  - Marked `print.margot_cv_results` as internal (S3 method)
- **Better cross-validation integration in `margot_interpret_heterogeneity()`**:
  - Automatically converts invalid adjustment methods to "none" when using CV
  - No longer shows warning when default "BH" is automatically converted for CV
  - Only warns if user explicitly sets an incompatible adjustment method
  - Added example in documentation showing how to use cross-validation
  - Now returns `cv_results` object that can be passed directly to plotting functions
  - Added `method_used` field to indicate whether CV or standard method was used
  - Avoids need to recompute CV results for visualization
- **Enhanced `margot_interpret_heterogeneity()` reporting**:
  - Extended report now includes method details (standard vs cross-validation)
  - Reports number of CV folds when cross-validation is used
  - Reports alpha level and multiple testing correction method
  - CV now tests both AUTOC and QINI targets (was only testing AUTOC)
- **Improved memory management for parallel processing**:
  - `margot_rate_cv()` now has `future_globals_maxSize` parameter (default 22 GiB)
  - Increased default from 16 GiB to 22 GiB to handle larger model objects
  - Memory limit is now set before creating parallel plan to ensure proper enforcement
  - Better error messages when memory limit is exceeded, suggesting solutions
  - Automatically manages memory limits for parallel workers
  - Restores original memory settings on function exit
- **Simplified citation approach**:
  - Removed Rdpack dependency for simpler citation management
  - Now uses direct "(Wager 2024)" citation format
  - Maintained proper references in documentation

### Bug Fixes
- **Fixed progress tracking conflicts**:
  - `margot_interpret_heterogeneity()` now uses simple alerts instead of progress_step when calling `margot_rate_cv()`
  - This prevents conflicts between nested progress indicators
- **Fixed parallel processing cleanup**:
  - Parallel plan is now properly reset to sequential on function exit
  - Package loading messages from workers are suppressed with `future.stdout = FALSE`
  - Prevents hanging and repeated package loading messages
- **Fixed seed setting in `margot_rate_cv()`**: 
  - Now properly sets seed at function start for full reproducibility
  - If seed = NULL, automatically defaults to 12345
- **Fixed parallel processing memory errors**:
  - Resolved "11.31 GiB exceeds 500 MiB limit" error
  - Pre-extracts model data to reduce memory usage in parallel workers
- **Fixed progress bar error in `margot_rate_cv()`**:
  - Progress bars now use explicit IDs to avoid "Cannot find current progress bar" errors
  - Properly handles progress bar lifecycle with NULL checks
- **Fixed parallel processing error "could not find function rate_sequential_cv"**:
  - Internal function `rate_sequential_cv` is now exported (marked as internal) to be available to parallel workers
  - Function is explicitly passed to parallel workers via `future.globals` to ensure availability
  - Required packages (margot, grf, cli) are loaded in each worker
  - Added check for package installation - parallel processing requires margot to be installed
  - When using `devtools::load_all()`, the function automatically falls back to sequential processing
  - Clear warning message guides users to either install the package or use `parallel = FALSE`
  - This resolves the "could not find function" error when using `parallel = TRUE`
  - Parallel processing now shows elapsed time after completion for each target
  - Function remains internal and should not be called directly by users

# [2025-07-28] margot 1.0.171

### Breaking Changes
- **Multiple testing correction defaults changed**:
  - `margot_rate_cv()` now defaults to `adjust = "none"` (was "BH")
  - `margot_rate()` now defaults to `adjust = "none"` (was NULL)
  - `margot_rate_cv()` now only accepts "bonferroni" or "none" for `adjust` parameter
  - Other methods are rejected with informative error message

### New Features
- **Visualization for CV heterogeneity tests**:
  - New `margot_plot_cv_results()` function creates forest plots for CV test results
  - New `margot_plot_cv_summary()` function shows summary of significant vs non-significant models
  - Both functions provide appropriate visualizations for hypothesis test results (not RATE curves)
- **Enhanced CV heterogeneity testing**:
  - `margot_rate_cv()` now defaults to testing both AUTOC and QINI targets
  - Added `label_mapping` parameter for custom model labels in results and plots
  - Improved interpretation for dual-target testing showing concordance and differences

### Improvements
- **Statistical validity for CV correction**:
  - Clear documentation that BH/BY/FDR methods are not appropriate for CV
  - Added recommendation to use alpha = 0.2 with Bonferroni correction
  - Improved error messages and warnings for adjustment methods
- **Better handling of CV results in plotting functions**:
  - `margot_plot_rate()` now detects CV results and suggests using `margot_plot_cv_results()`
  - `margot_plot_rate_batch()` similarly provides informative error messages
  - Clear distinction between hypothesis test visualizations and RATE curve plots

# [2025-07-28] margot 1.0.170

> **⚠️ IMPORTANT NOTICE**: This development version of the margot package is undergoing significant refactoring as we transition to the **margotverse** suite of packages. This package is currently for the author's lab use only. The package will be split into focused, single-responsibility packages including margot.core, margot.lmtp, margot.grf, margot.viz, and others. Please expect breaking changes in upcoming releases.

### Breaking Changes
- **Parameter deprecation**: `qini_train_prop` is now deprecated in favor of `train_prop` with default value 0.5
  - Backward compatibility maintained with deprecation warning
  - Affects: `margot_causal_forest()` and related functions

### New Features
- **Cost sensitivity analysis for QINI curves**:
  - Added `treatment_cost` parameter throughout QINI functionality
  - New `margot_qini_cost_sensitivity()` function for analyzing treatment allocation under different budget constraints
  - Cost parameter affects optimal treatment allocation strategies
  - Supports cost sensitivity visualization with budget-based x-axis

- **Enhanced RATE computation**:
  - Added `q` parameter for custom quantile grids in `margot_rate()`
  - Pass-through parameters (`...`) to `grf::rank_average_treatment_effect()` for maximum flexibility
  - Target parameter now correctly reflected in plot titles (AUTOC or QINI)

- **Flexible confidence intervals**:
  - `show_ci` parameter in QINI plots now accepts: FALSE, TRUE, "both", "cate", or "ate"
  - Allows selective display of confidence intervals for different curves
  - Added `seed` parameter (default = 12345) for reproducible confidence interval computation

- **Budget-based visualization**:
  - New `x_axis` parameter in QINI functions accepts "proportion" or "budget"
  - Budget x-axis shows treatment allocation under budget constraints
  - Automatically detects appropriate x-axis type based on data

- **Cross-validation heterogeneity testing**:
  - New `margot_rate_cv()` function implementing GRF's uncorrelated sequential CV approach
  - Integrated CV option in `margot_interpret_heterogeneity()` with `use_cross_validation` parameter
  - Provides robust heterogeneity testing that avoids overfitting
  - Parallel processing support (currently experimental, disabled by default)

- **Enhanced reproducibility**:
  - Added `seed` parameter to `margot_interpret_heterogeneity()` and `margot_policy()`
  - Ensures consistent results across all sub-computations
  - Default seed = 12345 for all functions

### Improvements
- **Cost invariance property documentation**:
  - Added extensive documentation explaining that relative benefit of CATE vs ATE targeting remains constant with uniform costs
  - Clarified that gains are identical when plotted against proportion treated
  - Budget visualization reveals cost differences more clearly

- **Enhanced error handling**:
  - Better detection of treatment_cost changes for QINI regeneration
  - Improved error messages for missing data or model components
  - Fixed treatment_cost initialization order to prevent NULL issues

- **Multiple testing correction updates**:
  - Changed default `adjust` parameter to "none" in both `margot_rate_cv()` and `margot_rate()`
  - Restricted `margot_rate_cv()` to only accept "bonferroni" or "none" for statistical validity
  - Other methods (BH/BY/FDR) are not appropriate for CV due to martingale aggregation
  - Added recommendation to use alpha = 0.2 with Bonferroni due to its conservative nature
  - Clear documentation explaining why only these methods are valid for CV

### Bug Fixes
- Fixed "invalid 'type' (closure)" error in `margot_rate()` by properly handling `q` parameter
- Fixed RATE plot titles to correctly display target (AUTOC or QINI)
- Fixed QINI regeneration to detect treatment_cost changes
- Fixed y-axis scaling to remove artificial cost-based adjustments
- Fixed treatment_cost NULL initialization causing QINI generation failures
- Fixed "Models not found" error in `margot_plot_qini_batch_cost_sensitivity()`
- Fixed "$ operator is invalid for atomic vectors" error by creating `margot_plot_qini_direct()`
- Fixed "invalid 'x' type in 'x && y'" error by using proper boolean variables
- Fixed pkgdown build error by adding missing functions to _pkgdown.yml reference
- Fixed reproducibility issue in compute_rate_on_demand by adding default seed = 12345
- Fixed evidence type classification in `margot_interpret_heterogeneity()`:
  - QINI curves now properly treated as exploratory evidence (sensitive to spend levels)
  - RATE positive results now correctly classified as "targeting_opportunity"
  - Only RATE can produce negative evidence (differential prediction is positive/inconclusive only)
  - Added new "exploratory_only" category for models with only QINI curve evidence

### Known Issues
- Parallel processing in `margot_rate_cv()` may encounter memory issues with large model objects due to 
  environment capture. Parallel processing is disabled by default. To enable at your own risk, use 
  `parallel = TRUE` and increase memory limit with `options(future.globals.maxSize = 15 * 1024^3)`

### Internal Changes
- Created `margot_generate_qini_data()` for on-demand QINI generation
- Created `margot_plot_qini_direct()` for plotting pre-computed QINI data
- Updated all maq() calls to properly pass treatment_cost parameter
- Implemented budget-based generation in `margot_qini_simple_baseline()`
- Added `compute_rate_on_demand()` helper for consistent RATE computation
- Added `convert_cv_to_rate_results()` for CV result format conversion
- Implemented `rate_sequential_cv()` for martingale-based CV aggregation

# [2025-07-25] margot 1.0.150

### New Features
- **Scale transformations for QINI plots**: Added `scale` parameter to `margot_plot_qini()` and `margot_plot_qini_batch()`
  - "average" (default): Shows average policy effects per unit (maq's Q(B) = E[⟨πB(Xi), τ(Xi)⟩])
  - "cumulative": Shows traditional cumulative gains (total accumulated benefit)
  - "population": Shows total population impact (absolute gain in outcome units)
  - Example: `margot_plot_qini(mc_result, "model_outcome", scale = "cumulative")`
  - Confidence intervals automatically adjust to match the selected scale

### Bug Fixes
- **Fixed QINI dimension mismatch with qini_split = TRUE**: Resolved critical error where tau_hat dimensions didn't match test indices
  - When `qini_split = TRUE`, tau_hat is now properly subset or regenerated for test indices only
  - Added intelligent handling that regenerates predictions when model is available
  - Falls back to subsetting when model is unavailable
  - Prevents "reward, costs, and evaluation scores should have conformable dimension" errors

- **Fixed rbind errors in QINI plots**: Resolved "numbers of columns of arguments do not match" errors
  - Fixed confidence interval computation to ensure consistent column structure
  - Fixed extended data generation to match qini_data columns when scale transformation is applied
  - Added proper column matching before all rbind operations

### Improvements
- **Enhanced maq integration**: All maq function calls now properly pass seed and sample.weights parameters
  - Updated `margot_generate_qini_data`, `compute_qini_curves_binary`, `margot_recompute_qini_ipw`, and `margot_recompute_qini_aipw`
  - Ensures reproducibility and proper weighting in all QINI computations
  - Fixed missing seed propagation in `margot_summary_cate_difference_gain`

- **Better error handling for QINI regeneration**: Added detailed diagnostic messages
  - Shows tau_hat dimensions, IPW scores shape, and weights information
  - Helps identify dimension mismatches and data availability issues
  - Improved handling of simple baseline objects vs maq objects

# [2025-07-24] margot 1.0.140

### Breaking Changes (Improvements)
- **Consistent QINI baseline method**: `margot_causal_forest()` now uses "maq_no_covariates" as default
  - Previously used constant rewards approach
  - Now matches the default in visualization and analysis functions
  - More theoretically sound as noted in literature
  - Includes automatic fallback to ensure robustness
  - **Action required**: Re-run `margot_causal_forest()` to get the new baseline method

### New Features
- **Customizable QINI plot colours**: Added `cate_color` and `ate_color` parameters to `margot_plot_qini()` and `margot_plot_qini_batch()`
  - Default colours: gold (#d8a739) for CATE (targeted treatment) and dark gray (#4d4d4d) for ATE (no-priority assignment)
  - Improves visual distinction between targeting strategies
  - Example: `margot_plot_qini(mc_result, "model_outcome", cate_color = "blue", ate_color = "red")`

### Bug Fixes
- **Fixed QINI regeneration for flipped models**: Resolved critical issue where flipped models (with _r suffix) failed to regenerate QINI curves
  - Models like `model_t2_neuroticism_z_r` now correctly regenerate QINI curves with different baseline methods
  - Removed blocking condition that prevented regeneration when mc_result$data was NULL
  - Added graceful fallback to existing QINI data when regeneration fails
  - Enhanced debugging messages for forest object retrieval

### Improvements
- **QINI data consistency**: Ensured average gain calculations use the same data subset as original QINI curves
  - Store QINI generation metadata (test indices, baseline method, data split info) during initial generation
  - `margot_summary_cate_difference_gain()` now uses stored metadata for consistency
  - Added informative CLI messages throughout the process
  - Fixed baseline_method metadata to correctly reflect "maq_constant" for compute_qini_curves_binary

- **Default spend levels updated**: Changed default spend_levels from c(0.2, 0.5) to c(0.1, 0.4) across all functions
  - Affected functions: `margot_plot_qini()`, `margot_plot_qini_batch()`, `margot_interpret_qini()`, 
    `margot_interpret_heterogeneity()`, `margot_policy()`, `margot_qini()`, `margot_qini_diagnostic()`, and `margot_batch_policy()`
  - The 10% and 40% spend levels better reflect typical analysis needs
  - 10% captures early targeting efficiency, 40% shows broader implementation potential
  - Users can still specify custom spend levels as needed

- **Enhanced heterogeneity interpretation**: `margot_interpret_heterogeneity()` now identifies models with mixed evidence
  - New outputs: `cautiously_selected_model_ids` and `cautiously_selected_model_names` for models with conflicting evidence
  - Combined outputs: `all_selected_model_ids` and `all_selected_model_names` include both selected and cautiously selected models
  - Captures models that show positive evidence in some tests but negative in others
  - Helps identify targeting opportunities that require careful validation before implementation
  - Example use case: models with beneficial QINI curves despite negative RATE statistics
  - Note: Re-run `margot_causal_forest()` to get full metadata benefits for existing models
- **Fixed breaking change**: QINI regeneration now checks data availability before attempting
  - When models were created with `save_data = FALSE`, changing baseline_method no longer causes errors
  - Falls back to existing QINI curves with clear warning when data is unavailable

- **QINI baseline method consistency**: Enhanced tracking of baseline methods across functions
  - `margot_policy()` and `margot_qini()` now store the baseline method used for each model
  - `margot_interpret_qini()` gains `baseline_method` parameter to ensure consistency with plots
  - `margot_qini()` now properly regenerates curves when baseline method changes
  - Warns when interpretation uses different baseline method than visualization
  - Helps avoid mismatches between QINI plots and summary tables
  - Users get informative messages instead of cryptic "Cannot find outcome data" errors

# [2025-07-24] margot 1.0.130

### new features
- **margot_qini()**: new function for generating QINI curves and difference gain summaries
  - provides direct access to QINI analysis without running full policy analysis
  - parallels the functionality of margot_rate() for consistency
  - outputs are compatible with margot_interpret_qini()
  - supports all baseline methods: "auto", "simple", "maq_no_covariates", "maq_only", "none"
- **margot_interpret_rate()**: moved from _temp folder to main package
  - now properly exported for interpreting RATE results
  - handles both single method (AUTOC or QINI) and comparison of both
  - provides clear markdown-formatted summaries

### improvements
- **margot_interpret_qini()**: now accepts output from both margot_policy() and margot_qini()
- **consistent API**: users can now use:
  - margot_rate() --> margot_interpret_rate() for RATE analysis
  - margot_qini() --> margot_interpret_qini() for QINI analysis

### default changes
- **baseline_method**: changed default from "auto" to "maq_no_covariates" in all QINI functions
  - provides more theoretically sound default behavior
  - "maq_no_covariates" now includes automatic fallback to simple baseline if maq fails
  - users always get a baseline curve with clear warning when fallback occurs
  - affects: margot_generate_qini_data(), margot_qini(), margot_plot_qini(), margot_plot_qini_batch(), margot_policy()

### bug fixes
- **flipped model data lookup**: fixed issue where QINI functions couldn't find outcome data for flipped models
  - models with "_r" suffix (e.g., "model_t2_neuroticism_z_r") now correctly find data under the base name
  - affects: margot_plot_qini(), margot_qini(), margot_policy()

# [2025-07-24] margot 1.0.125

### improved
- **margot_interpret_heterogeneity()**: improved clarity of test results
  - renamed "omnibus_test" column to "differential_prediction_test" for clarity
  - added "mean_prediction_test" column to show calibration status (informational only)
  - simplified main interpretation and enhanced extended report with complementary methods explanation
  - updated omnibus calibration test description to match `grf` manual clarity
- **QINI ATE baseline**: improved reliability and consistency
  - ATE baseline now always generated as straight line using mean(tau_hat)
  - ensures fair comparison between CATE and ATE curves
  - more robust than relying on maq output for constant rewards
  - aligns with maq conceptual approach while being more predictable
- **margot_plot_qini() enhancements**:
  - changed CATE curve colour from blue (#4f88c6) to green (#009E73) to avoid "control" association
  - added ylim parameter for manual y-axis control (defaults to automatic scaling)
  - ylim parameter also added to margot_plot_qini_batch() for consistency
  - added baseline_method parameter for flexible baseline generation ("auto", "simple", "maq_no_covariates", "maq_only", "none")
  - robust simple baseline implementation that always succeeds
  - smart fallback logic: adds simple baseline to existing QINI data when regeneration isn't possible
  - improved data extraction from grf forest objects (Y.orig, W.orig) when mc_result$data is NULL
  - better error messages with available data fields when baseline generation fails

### breaking changes
- **removed save_plot parameters**: removed save_plot and related parameters (dpi, width, height, output_dir) from:
  - margot_plot_qini_batch()
  - margot_policy()
  - margot_plot_rate_batch()
  - users should save plots manually using ggplot2::ggsave() if needed

### major architecture change
- **QINI curve generation moved to on-demand**: 
  - created margot_generate_qini_data() helper function for on-demand generation
  - margot_plot_qini() now generates QINI data when needed
  - margot_plot_qini_batch() updated to work with on-demand generation
  - margot_policy() and margot_summary_cate_difference_gain() generate QINI objects as needed
  - more robust approach that handles edge cases better
  - follows maq's mathematical approach for ATE baselines
  
### fixes
- fixed omnibus test matching for flipped models from margot_flip_forests()
- better matching logic using original outcome names for reliability

# [2025-07-23] margot 1.0.115

### Major Features
- **margot_interpret_heterogeneity()**: comprehensive function to combine evidence from multiple heterogeneity tests
  - Integrates RATE (AUTOC & QINI), QINI curves, and omnibus calibration tests
  - Simplified interface: takes models directly, computes all analyses internally
  - Provides unified recommendations for model selection
  - Includes concordance analysis and detailed evidence summary
  - New evidence categorisation system for nuanced interpretation
  - Added `include_extended_report` parameter for detailed academic-style reports with full statistics
  - **margot_plot_qini_batch()**: batch processing for QINI plots across multiple models
  - **margot_flip_forests()**: enhanced with `grf_defaults` parameter for consistent GRF settings

### Improvements  
- `margot_interpret_rate()` now includes `excluded_both` and `excluded_either` lists
- `margot_interpret_qini()` now exposes harmful and no-effect model categorizations
- Improvements to internal naming in `margot_flip_forests()`
- Improvements interpretation for qini curve analysis:
  - Clarified QINI compares targeted vs uniform treatment allocation
  - Added explanation for small differences at 100% spend
  - Added note about CATE benefits even with unreliable ATE


# [2025-07-22] margot 1.0.110

major improvements to QINI curve visualisation, new functions for AIPW/IPW QINI computation, enhanced maq compatibility, and various bug fixes.

### New Functions
- **margot_recompute_qini_aipw()**: recompute QINI curves using AIPW scores for improved robustness
  - Provides doubly robust estimates that are consistent if either propensity or outcome model is correct
  - supports automatic treatment variable detection or manual specification
  - nandles various data structures including models stored in full_models list
  - estimates conditional means using regression forests (mu.hat)
  - adds overlap warnings when propensity scores are extreme (<0.05 or >0.95)
  - fully compatible with existing QINI plotting and interpretation functions

- **margot_recompute_qini_ipw()**: Recompute QINI curves using IPW scores only
  - simpler alternative to AIPW for faster computation
  - uses the same modern maq API for consistency
  - useful for debugging QINI curve differences
  - shares the same flexible data handling as the AIPW version

- **margot_qini_diagnostic()**: diagnose QINI gain discrepancies
  - compares QINI gains from plot data, direct maq calculations, and diff summaries
  - helps identify and debug discrepancies between different calculation methods
  - flags differences above specified tolerance threshold
  - useful for verifying QINI calculations are consistent

- **margot_plot_qini_batch()**: Batch process and plot QINI curves for multiple models
  - supports all margot_plot_qini() parameters for consistent visualization
  - handles model names with or without "model_" prefix
  - automatically filters to models with QINI data
  - saves plots to specified directory (default: "qini_plots")
  - returns list of ggplot objects for further customization

### Bug Fixes
- fixed `margot_plot_qini()` confidence interval computation and display issues
  - added `inherit.aes = FALSE` to geom_ribbon to prevent aesthetic inheritance errors
  - fixed CI computation to handle both old (treatment/baseline) and new (cate/ate) qini object naming conventions
  - Confidence intervals now compute and display correctly when `show_ci = TRUE`
- Fixed `margot_flip_forests()` error when rebuilding combined_table
  - Resolved "Couldn't find a point-estimate column" error that occurred after flipping outcomes
  - Function now properly rebuilds combined_table from merged results instead of incorrectly passing entire results object
- Fixed `margot_causal_forest()` to save treatment assignment vector W when save_data=TRUE
  - Ensures margot_flip_forests() can perform full model recomputation
- Fixed `compute_qini_curves_binary()` to use modern maq API
  - Updated from positional arguments to named parameters (reward, cost, DR.scores)
  - Ensures consistency with current maq package expectations
  - Resolves potential inconsistencies in QINI curve computations

### Enhancements
- Enhanced `margot_flip_forests()` with automatic removal of original models
  - New `remove_original` parameter (default TRUE) removes originals after flipping
  - Reduces memory usage and provides cleaner outputs
  - Set to FALSE to keep both original and flipped models
- Fixed double "model_" prefix bug in `margot_flip_forests()`
  - Results now correctly show as "model_outcome_r" instead of "model_model_outcome_r"

- Fixed QINI gain calculation discrepancies
  - `extract_qini_data_binary()` now uses actual gain values instead of linear approximation for ATE curves
  - `margot_flip_forests()` now correctly flips outcome values when recalculating QINI curves
  - Resolves ~2x discrepancy between plotted QINI curves and `margot_interpret_qini()` gain summaries

- Fixed QINI plot visualization issues
  - ATE line now correctly displays as straight line (representing constant treatment effect)
  - Confidence intervals now cover full 0-100% range (previously missing 0-5% and 95-100%)
  - Added endpoint extrapolation to ensure complete CI coverage

### Enhancements to margot_plot_qini()
- Added `horizontal_line` parameter for maq::plot() compatibility
  - Extends curves horizontally when path is complete
- Added `grid_step` parameter for data subsampling
  - Improves performance with large datasets
- Added `return_data` parameter to return plot data as data.frame
  - Matches maq::plot() output format for interoperability
- Enhanced std.err extraction from maq objects
  - Provides additional consistency checks

- Fixed `margot_flip_forests()` missing W component error
  - `margot_causal_forest()` now saves treatment vector W when save_data = TRUE
  - Ensures compatibility with model flipping functionality

- Updated `margot_plot_qini()` visualization
  - Fixed CI computation error when only CATE curve exists (no ATE)
  - Fixed case sensitivity in curve naming (cate/CATE)
  - Fixed colors for binary treatments: CATE = blue (#4f88c6), ATE = gold (#d8a739)
  - Ensured color/fill scales match so lines and CI ribbons have same colors
  - Multi-arm treatments continue to use Okabe-Ito palette

### Enhancements
- Updated `margot_recompute_qini_aipw()` to handle multiple data storage patterns
  - Supports models in both result objects and full_models list
  - Can extract data from top-level storage when models lack original data
  - Smart treatment variable detection with binary preference
  - Automatic propensity score estimation when not available

# [2025-07-21] margot 1.0.105

### New Functions
- **margot_recompute_ate()**: Recompute average treatment effects with different target samples
  - Allows recomputation without refitting expensive causal forest models
  - Supports target_sample options: "all" (ATE), "treated" (ATT), "control" (ATC), "overlap" (ATO)
  - Automatically renames effect columns based on target sample
  - Recomputes E-values with the new estimates
  - Uses margot-style parameter naming (target_sample not target.sample)
  
- **margot_recompute_ate_batch()**: Compare ATEs across multiple target samples
  - Convenience function to compute all target sample types at once
  - Creates comparison table with side-by-side estimates and E-values
  - Useful for sensitivity analysis and exploring treatment effect heterogeneity

### margot_plot() Enhancements
- **New Column Support**: Now accepts ATE/ATT/ATC/ATO column names in addition to traditional E[Y(1)]-E[Y(0)] format
  - Automatically detects and uses the appropriate effect column
  - Maintains full backwards compatibility
  
- **Enhanced rename_ate Parameter**: 
  - Accepts boolean (TRUE auto-detects appropriate label based on column type)
  - Accepts custom string for complete control
  - Auto-detects ATE/ATT/ATC/ATO when set to TRUE
  
- **New rename_evalue Parameter**: 
  - When TRUE, renames E_Value to "E-Value" and E_Val_bound to "E-Value Bound"
  - Makes column names more publication-ready

### Supporting Function Updates
- **margot_interpret_marginal()**: 
  - Now correctly describes treatment effect type (ATE/ATT/ATC/ATO)
  - Explicitly states "No reliable effects are evident." when no outcomes meet reliability threshold
  - Updated wording from "showed" to "present" for clearer communication
- **back_transform_estimates()**: Handles new column types for original scale transformations
- **group_tab()**: Detects and processes new effect column types
- All changes maintain backwards compatibility

### Qini Plot Enhancements
- **margot_plot_qini()**: Added confidence interval support
  - New parameters: show_ci, ci_alpha, ci_n_points, ci_ribbon_alpha, ci_ribbon_color
  - Confidence intervals computed using maq::average_gain()
  - Visualized with ggplot2::geom_ribbon()
  - Maintains backwards compatibility (CI display off by default)
  - Fixed geom_ribbon aesthetic inheritance issue
  
- **margot_policy()**: Added qini_args parameter
  - Allows passing additional arguments to margot_plot_qini()
  - Enables confidence interval display in policy batch processing
  - User-provided args override defaults via modifyList()

### New Functions
- **margot_recompute_qini_aipw()**: Recompute QINI curves using AIPW scores
  - Provides more robust estimates than IPW, especially for observational data
  - Supports regression forest method for estimating conditional means
  - Compatible with all existing QINI plotting and interpretation functions
  - Adds metadata tracking to indicate AIPW was used

# [2025-07-20] margot 1.0.102

### Policy Tree Enhancements
- **Decision tree title clarification**:
  - Enhanced `margot_plot_decision_tree()` to append "Outcome" to titles
  - Example: "Neuroticism" now displays as "Neuroticism Outcome"
  - Clarifies that the tree shows policy decisions for the outcome variable

### Label Improvements
- **Reversed outcome formatting**:
  - Updated `margot_reversed_labels()` to prepend rather than append "(reduced)"
  - Changed from "{outcome} (reduced)" to "(reduced) {outcome}"
  - Example: "Anxiety (reduced)" now displays as "(reduced) Anxiety"
  - Improves readability and understanding of flipped outcomes

# [2025-07-20] margot 1.0.101

- **new**: margot_censor_lead() - allows for conditional censoring. 

# [2025-07-20] margot 1.0.100

### Policy Tree Enhancements
- **Policy tree interpretation improvements**:
  - Modified `margot_interpret_policy_batch()` to remove redundant "Policy tree analysis results:" line
  - Added "at the end of study" to all Findings headings for temporal clarity
  - Added "baseline" prefix to all variable names in tree splits and leaf descriptions
  - Clarifies that splits are based on baseline characteristics, not outcomes
  - Simplified CATE reporting to just state values without subjective interpretation
  - More appropriate for standardized outcomes where negative values are common

- **Policy tree plotting updates**:
  - Enhanced `margot_plot_policy_tree()` to add "(baseline)" to all axis labels
  - Added "(baseline)" to plot titles and subtitles for clarity
  - Updated both depth-1 and depth-2 plots for consistency
  - Ensures users understand splits are made on baseline measures
  - Enhanced `margot_plot_decision_tree()` to append "Outcome" to outcome titles
  - Example: "Neuroticism" now displays as "Neuroticism Outcome"

### Label Transformation Improvements
- Updated `transform_var_name()` helper function:
  - Now removes "_l" suffix from variable names
  - Properly capitalizes NZSEI and NZDEP acronyms
  - Example: "t0_nzsei_13_l" now displays as "NZSEI 13 (baseline)"
- Updated `margot_reversed_labels()` formatting:
  - Changed from "{outcome} (reduced)" to "(reduced) {outcome}"
  - Example: "Anxiety (reduced)" now displays as "(reduced) Anxiety"


# [2025-07-19] margot 1.0.90

### new functions
- added `margot_assess_overlap()` for evaluating propensity score overlap
  - shows common support regions for treatment validity
  - generates propensity score distribution plots by treatment group
  - uses `transform_label` helper for automatic label formatting
  - includes `text_summary` output suitable for dropping into documents
  - added `theme` parameter for ggplot2 theme customization
  - updated colors to package standards: treatment (#d8a739), control (#4f88c6)
  - enhanced text summary with interpretation of overlap meaning

### enhanced Qini analysis
- **margot_interpret_qini()** improvements:
  - new `spend_levels` parameter for custom spend level analysis
  - new `concise_summary` output groups outcomes by benefit/harm/no effect
  - added explanatory text about CATE and Qini curves (`include_intro` parameter)
  - auto detection of available spend levels with fallback handling
  - warnings when requested spend levels don't exist in data

- **margot_plot_qini()** enhancements:
  - new vertical dashed lines at spend levels with text annotations
  - replace multiple label parameters with single `label_mapping`
  - added `theme` parameter matching margot_assess_overlap
  - spend level lines now labeled (e.g., "20% spend", "50% spend")
  - smart label positioning

### API Consistency

- renamed `spend` to `spend_levels` in `margot_policy()` and `margot_batch_policy()`
  - Ccnsistent with `margot_interpret_qini()` parameter naming
  - **breaking change**: Update code using `spend` parameter

### Bug Fixes

- fixed `margot_policy()` and `margot_batch_policy()` to pass spend_levels to Qini plots
  - Qini plot vertical lines now match the spend levels used for calculations
  - previously always showed 20% and 50% regardless of parameter

# [2025-07-19] margot 1.0.80

- Added experimental support for conditional means computation via `policytree::conditional_means()`
- New `compute_conditional_means` parameter in `margot_causal_forest()` (default TRUE)
- Conditional means are stored in model results when computed
- Enhanced `margot_interpret_policy_tree()` to include conditional means interpretation
  - Shows expected outcomes under each treatment within policy tree leaves
  - Computes average treatment effects within each leaf
  - New `include_conditional_means` parameter (default TRUE)
  - Changed default output to clear, simple language (new `use_math_notation` parameter, default FALSE)
  - Added `output_format` parameter: "bullet" (default) or "prose" for flowing narrative text
  - Enhanced leaf output with sample sizes, percentages, and effect interpretations
  - Removed uncertainty quantification disclaimer
- `margot_flip_forests()` completely rewritten to create new models with "_r" suffix
  - Properly recomputes all statistics (ATE, RATE, E-values, policy trees, QINI)
  - Flips conditional means columns for binary treatment
  - Removes original models and rebuilds combined_table
- New `margot_recalculate_policy_trees()` function for customizing policy tree covariates
  - Multiple covariate modes: "original", "custom", "add", "all"
  - Regex-based covariate exclusion support
  - Automatic covariate expansion for depth-2 trees
  - Parallel processing capability
- Updated `margot_reversed_labels()` to handle new "_r" suffix convention
  - Creates new label entries with "_r" suffix by default
  - Maintains backward compatibility with `use_r_suffix = FALSE`
  - Option to remove original entries with `remove_original = TRUE`
- Removed `margot_flip_forests_dev()` as functionality now available in `margot_recalculate_policy_trees()`

# [2025-07-17] margot 1.0.70
- `margot_flip_forests_dev()` allows custom policy trees 

# [2025-06-18] margot 1.0.65
- `margot_plot_policy_tree()` better graphs
  - Shape + Color coding - Points use both different shapes AND colors for better distinction
  - Clean masking - Shaded regions properly filter out points (no more overlapping mess)
  - Visible annotations - Split values appear as readable horizontal text near the dashed lines
  - Proper positioning - Annotations stay within plot boundaries

# [2025-06-18] margot 1.0.64
- `margot_plot_categorical()` handles binary data

# [2025-06-09] margot 1.0.63
- developing simulation within margot

# [2025-06-09] margot 1.0.62

## New
- Complete refactor of `margot_simulate()` with clearer data generation process
- Added support for heterogeneous treatment effects in simulations
- Enhanced vignette with comprehensive examples and power analysis templates

## Improved
- Fixed syntax errors in vignettes (removed numeric underscores)
- Improved test structure with cleaner helper functions
- Better documentation of simulation parameters and options

## Removed
- Removed `crayon` package dependency (down to 51 dependencies)
- Removed `gt` and `gtsummary` package dependencies
- Updated `margot_summary_panel()` to output markdown tables by default

# [2025-06-04] margot 1.0.61
- added simulation function `magot_simulate()` + units tests plus vignette

# [2025-06-04] margot 1.0.60 
- fixed namespace issue/ margot_model_evalue now exported. 

# [2025-06-04] margot 1.0.59
- removing 'export' from purely internal functions/ tidying

# [2025-06-03] margot 1.0.58
- `margot_plot()` when data are corrected using e.g. bonferroni the table is also corrected -- so the interpretation text and interpretation table match. 

# [2025-05-30] margot 1.0.57

- `margot_make_tables()` better handling in MS Word through flextable integration

# [2025-05-29] margot 1.0.56

- `margot_make_tables()` now renders variables with hyphens correctly. 


# [2025-05-28] margot 1.0.55
## new
- `margot_plot_tau()` - totally ehanced so that (a) zero is always shown (b) colour matches sign

# [2025-05-26] margot 1.0.54
## new
- `margot_plot_tau()`  creates a faceted grid of histograms showing the distribution of tau hat (individual treatment effects) for multiple models. the range is standardised across all facets to facilitate comparison. Useful for qualitatively displaying heterogenity of causal effects. 
## improved 
- `margot_plot()` three paragraph skips before the list

# [2025-05-25] margot 1.0.53
## improved 
- `margot_plot_policy_tree_depth2()`, `margot_plot_policy_combo()` - plotting enhancements to 1L and 2L trees (note Rstudio will sometime make panels gray -- just a quirk -- not an issue for final graph project.)


# [2025-05-19] margot 1.0.52
## improved 
- `margot_interpret_rate()` user may specify flipped outcome labels 

# [2025-05-18] margot 1.0.51

## improved 
- `margot_interpret_qini()` improved to select reliable Qini models

# [2025-05-18] margot 1.0.50
- `margot_interpret_rate_comparison()` exported.

# [2025-05-18] margot 1.0.49
## fixed
- `margot_interpret_rate()` - exported

# [2025-05-18] margot 1.0.48
## new
- `margot_reversed_labels()` -- helper that gets display labels and marks reversed outcomes (for policy trees)
- `margot_interpret_rate()` (`margot_interpret_rate_comparison()`)- now lists models that are not ruled out by the qini and autoc rate analysis.

# [2025-05-17] margot 1.0.47
## improved
- `margot_plot_create_options()` defaults for correcting for multiple comparisons,
  `adjust = "bonferroni"`, `alpha = 0.05`, #<- new
- `margot_planned_subgroup_batch()` and `margot_subset_batch()` -- numerous enhancements for clear reporting 
- `margot_compare_groups()` enhanced reporting


# [2025-05-16] margot 1.0.46
## improved
- `margot_process_longitudinal_data_wider()` correctly creates "NA" value for censoring indicator when unit is lost to follow up. (under development, defaults to older version, which works fine)

# [2025-05-15] margot 1.0.45
# improved
- `margot_rate()` - allows a subset of models to be passed, allows for adjustment for multiple comparisons, reflected in reporting.(`margot_interpret_rate()` and `margot_interpret_rate_comparison()`) both modified. 
- `margot_adjust_policy_p()` - extends methods
- `margot_plot_rate()` takes labels for outcomes.
- `margot_plot_rate_batch()` also takes labels for outcomes

# [2025-05-15] margot 1.0.44
- `margot_plot()` only uses correction method if there is more than one outcome. 
- `margot_interpret_marginal()` also updated. 

# [2025-05-15] margot 1.0.43
- `margot_compare_groups()` computation for relative risk fixed. 
- `margot_plot()` no passes to `margot_correct_combined_tables()` automatically (no need for a separate call)

# [2025-05-15] margot 1.0.42
## changed
- `margot_process_longitudinal_data_wider` fixed error in warnings that was causing the function to fail when ordinal data are passed. 


# [2025-05-14] margot 1.0.41
## New
- `margot_correct_combined_table()` does family–wise-error correction for multiple outcomes (ATE)
- `margot_add_policy_values()` - attach policy values to forest models (utility)
- `margot_collect_policy_values()` -- gather policy values from forest models
- `margot_screen_models()` select models for policy-tree analysis
- `margot_summarise_all()` summarise (family-wise) corrected analysis for policy trees
-  `margot_adjust_policy_p()
## changed
- `margot_rate()`, and `margot_rate_batch` have deterministic seeds for reproducible results
- `margot_plot` has two parmaters for `adjust` and `alpha` to describe whether family-wise adjustment has been made. 
- `margot_bind_table` now has default e_val_bound_threshold = 1.1 (previously 1, which is arguably too liberal).
- `margot_adjust_policy_p()` - family wise corrections
- `margot_add_policy_values_batch()` - batch add policy values to models
- `margot_report_policy()` - succinct reporting of corrected policy values.

# [2025-05-13] margot 1.0.40
- exported `margot_causal_forest_parallel()`

# [2025-05-13] margot 1.0.39

## New
- `margot_causal_forest_paralle()`  parallel implementation of `margot_causal_forest`. 
- `margot_flip_forests_parallel()` parallel implementatino of `margot_flip_forests()`

## improved
- `margot_process_longitudinal_data_wider()` - removes otios warnings from fastDummies.
- `margot_bind_tables()` - now accepts a dataframe and will work with single outputs. 

# [2025-05-12] margot 1.0.38
## improved
- `margot_impute_carry_forward()`
  - eligibility now requires an observed value in the **current** or a following wave, rather than only in a future wave.  
  - The baseline wave (`t0_`) is always checked and reported --even when no later waves exist—preventing silent skips.  
  - Internal check now uses  
    ```r
    cols_check <- c(col, future_cols)
    ok         <- rowSums(!is.na(out[, cols_check, drop = FALSE])) > 0
    ```  
    to align behaviour with the documentation.
- `margot_wide_impute_machine()` print flags now set to true
- `margot_wide_impute_baseline()` soft deprecated
- `margot_wide()` soft deprecated

# [2025-05-07] margot 1.0.37
- `margot_policy_tree_combo()` now exported

# [2025-05-07] margot 1.0.36
- `margot_policy_tree()` level 2 fixed

# [2025-05-06] margot 1.0.35
## improved
- `here_save()` and  `here_save_qs()` now correctly reports size of saved file.
-  tidying of roxygen2 code ('mc_test' removed and replaced with 'result_object')
- `margot_plot_policy_tree()` nicer label placement.

# [2025-05-04] margot 1.0.34
## fixed
- restored `margot_interpret_qini()`

# [2025-05-04] margot 1.0.33
## fixed
- namespace issues for `margot_plot_policy()` and `margot_rescue_qini()`

# [2025-05-04] margot 1.0.32
## New 
- `margot_planned_subgroup_batch()` overhauled to work with `margot_plot()` updates. 
-  restored decision tree plot results labelling
-  restored margot_interpret_marginal cutpoint results on datascale


# [2025-05-04] margot 1.0.31
## New 
- `margot_get_labels()` utility function for fetching labels. 

# [2025-05-03] margot 1.0.30
- `margot_plot_decision_tree()`- colours robustly match decisions 'control', 'treat'
- internal helpers `transform_label()`, and `transform_var_name()` labelling handles SDO and RWA correctly, also `get_original_value_plot()` and `get_original_var_info()`

# [2025-05-03] margot 1.0.29
## New
- `margot_rate_batch()` - Two allocation rules are available: **"treat_best"**  (default) ─ administer treatment to those with the largest predicted benefits; **"withhold_best"** ─ *remove* treatment from that same subgroup, useful when the intervention harmful. The CATE vector is flipped (multiplied by −1) *after* model fitting and *before* calling `grf::rank_average_treatment_effect()`.  Outcome direction flips performed earlier by `margot_flip_forests()` are thus kept conceptually separate from the policy flip implemented here.
-  removed `add_resistance`

## Refactored to accept 1L depth policy trees 👍
- `margot_plot_policy_tree()` will plot a tree of depth = 1L. 
- `margot_recalculate_policy_trees()`
- `margot_flip_forests()`
- `margot_interpret_policy_batch()`
- `margot_interpret_policy_tree()`
- `margot_interpret_rate()`
- `margot_plot_decision_tree()`
- `margot_plot_policy_combo()`
- `margot_policy()`


# [2025-05-02] margot 1.0.28
## New 
- `margot_rate()` now computes resistance to exposure (if requested) using `add_resistance`

# [2025-05-02] margot 1.0.27
## New 
- `margot_flip_forests()` automatic recomputing of the policy trees by default.
- `margot_inspect_qini()` helper to evaluate extreme propensity scores from grf models. 
- `margot_rescue_qini()` (developing) - to recompute qini data for grf models when `qini_objects` are null / empty.

## Improve
- `margot_bind_models()` -- accommodates changes

# [2025-05-02] margot 1.0.26
## Improved
- `margot_wide_machine()` now handles allows for `imputation_method` = "none". new parameters `extend_baseline` effectively pivots all date from long to wide, and allows for time indexing of columns - userful for `grf` models. 

# [2025-05-02] margot 1.0.25
## Improved
- `margot_plot_shift()` and `margot_plot_categorical` more tightly integrated. Both now plot the mean/median and flexibly plot sds.  (Both are now in the `margot_plot_categorical.R` script).

# [2025-05-01] margot 1.0.24
## Improved
- `margot_bind_models()` works with `lmtp` batched outputs with more than two conditions.

# [2025-04-30] margot 1.0.23
## Improved
- `margot_plot()` interpretation tracks user specified E-value

# [2025-04-30] margot 1.0.22
## NEW
- `margot_trim_sample_weights()`: standardises the (possibly trimmed) weights to have a mean of 1.  Missing values are preserved.

# [2025-04-29] margot 1.0.21
## Improved
- `margot_plot_rate_batch()` user can pass specific model names 
- `margot_rate_comparision()` outputs correct model names (and never statistically significant negative results.)

# [2025-04-28] margot 1.0.20
## Improved
- `margot_rate()`, `margot_rate_interpret()` now output model names for reliable result estimates. 

# [2025-04-21] margot 1.0.19
## Improved
- `margot_bind_table()` flexibly modify column names 
- `margot_plot()` - added options to transform colname for the estimand to "ATE", also the E_value colnames. 
- `margot_interpret_marginal()` and `group_tab()` now return order consistent with the plot
- `margot_interpret_rate()` better language, `margot_interpret_rate_comparison()` now a helper (not exported).
- `margot_plot_policy_tree()` and `margot_policy` now defaults to shading out regions where there is no interest.


# [2025-04-21] margot 1.0.18
## Improved
- `margot_log_transform_vars()` handles edge cases better by (1) requiring numeric input (2) requiring no NAs.

# [2025-04-21] margot 1.0.18
## Improved
- `margot_plot_categorical()` robust to wider range of data values

# [2025-04-20] margot 1.0.17
## Improved
- `margot_lmtp_tab()` -- fixes breaking change introduced by `lmtp v1.50`


# [2025-04-20] margot 1.0.16
## Improved
- `margot_transition_table()` -- allows filtering by observation/censoring variable, can be useful where data are entirely missing from certain waves.
- `margot_bind_tables()` -- correct output retaining outcome names
- `margot_impute_carry_forward()` -- better error handling/ if observation is seen in a year, may impute forward. 

# [2025-04-19] margot 1.0.15
## Improved
- `margot_interpret_marginal()` - clearer wording, outcomes listed rather than dull sentences. 

# [2025-04-19] margot 1.0.14
## Improved
- `margot_bind_tables()` has additional parameter,  `sort_E_val_bound = c("none", "asc", "desc")`, which allows users to order multiple tables by E_value thresholds. 
-  fixed multiple functions to get assending order for `margot_plot()`, these are `margot_plot()`, `group_tab()`, `margot_interpret_marginal()` `transform_label()`, `transform_table_rownames()` -- and removing stray `transform_label()` label functions from various functions. `transform_table_rownames()` now internal. 


# [2025-04-10] margot 1.0.13
## Improved
- `margot_bind_tables()` streamlined the function by focusing on markdown and LaTeX outputs. reduced dependencies by eliminating reliance on dplyr, stringr, and cli packages. enhanced markdown output with bold variable names for better readability, improved LaTeX output with better multi-page support and alternating row colors, maintained all core functionality.
- `margot_omnibus_hetero_test()` - considerably improved for clarity.

# [2025-04-10] margot 1.0.12

## Improved
- `margot_bind_models()` now bolds reliable results in markdown
- `margot_interpret_rate()` and `margot_interpret_rate_comparison()` more accurate reporting and deal with edge cases (no RATE reliable) better. 

# [2025-04-09] margot 1.0.11

## New
- `margot_bind_models()` - bind model outputs (currently implemented for batch caual forest models)
- `margot_interprete_rate_comparison()` compare qini and auto RATE estimates, also called by `margot_interpret_rate()`
- `margot_bind_tables()` - better markdown

## Improved
- `margot_interpret_rate()` combines qini and rate estimates.
- `margot_rate()` output better worded, still reate "rate_result" for backward compatibility; table only highlight "significant" if RATE is positive.
- `margot_interpret_qini()` - better outputs


# [2025-04-07] margot 1.0.10
## Improved
- `margot_causal_forest()` consistent use of training proportion rather than mixing training/test. 

# [2025-04-07] margot 1.0.9
## Improved
- `margot_causal_forest()` reverted --enhancements failing

# [2025-04-07] margot 1.0.8
## Improved
- `margot_causal_forest()` added checks

# [2025-04-07] margot 1.0.7
## Improved
- `margot_plot_decision_tree()`, `margot_plot_policy_tree()` -- enhancements


# [2025-04-07] margot 1.0.7
## Improved
- `margot_interpret_qini()` language improved for clarity.

# [2025-04-06] margot 1.0.6
## Improved
- `margot_interpret_policy_tree()` language improved for clarity.

# [2025-04-06] margot 1.0.5
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` and `margot_interpret_qini()` more aggressive error handlings

# [2025-04-06] margot 1.0.4
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` even better with LaTeX.

# [2025-04-06] margot 1.0.3
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` play better with LaTeX. Use "statistically reliable" in place of statistically significant. 


# [2025-04-06] margot 1.0.2
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` improved warnings for negative estimates

# [2025-04-06] margot 1.0.1
## Fixed
- restored accidentally deleted helpers
- `margot_omnibus_hetero_test()` now works as it should

# [2025-04-05] margot 1.0.0
## Improved
-  we are now using semantic versioning. 
-  `margot_omnibus_hetero_test` now has label mapping

# [2025-03-26] margot 0.3.3.3
## Improved
- `margot_censor()` - explicit call for 'cli' to avoid conflict with crayon 
- `create_ordered_variable()` - removed crayon
- `margot_summary_tables_cat()` - deleted this experimental table
- `margot_log_transform_vars()` removed crayon, made all function calls explicit, removed `require` for packages.
- `margot_summary_tables()` - remove `require` and explicit function calls now.
- `causal_contrast_engine()` - refactored - removes `require`
- `margot_wide_machine()` - removes `require`
- `margot_wide()` - removes `require`

# [2025-03-26] margot 0.3.3.2
## Improved
- `margot_causal_forest()`: we now default to honest splitting for computing qini curves.
- `margot_plot_categorical()`: fixed error in handling of binary variables.
- `margot_bind_tables()` - nicer printout, user can change column names.


# [2025-03-22] margot 0.3.3.1
## Improved
- `margot_causal_forest()`- correct calculation of qini ate (reverted to older function)

# [2025-03-21] margot 0.3.3.0
## Improved
- `margot_causal_forest()`- refactored to save qini_data and qini_objects for each model. (was not working before).
- `margot_multi_arm_causal_forest()` - refactored.

# [2025-03-21] margot 0.3.2.9
## Improved
- `margot_causal_forest()` now passes doubly robust scores to policy decision trees. We now compute policy trees using doubly robust (DR) scores from \texttt{grf::get_scores()} rather than the previous IPW-based approach. DR scores combine outcome and propensity estimates, yielding improved statistical power and more efficient estimation of treatment effects. This change is backward compatible -- no modifications to user code are required -- and a note is printed during execution indicating that DR scores are used.

# [2025-03-21] margot 0.3.2.8
## Improved
- `margot_interpret_qini()` prints out more economical explanation, only giving explanations for reliable results.

# [2025-03-21] margot 0.3.2.7
## New
- margot_bind_tables(): binds results from multiple out-comewide studies into one table

# [2025-03-21] margot 0.3.2.6
## New
- `margot_planned_subgroups_batch()` -- batch the batched analysis of planned subgroups analysis for causal forests (time-saver).
- `backtransform_log_z()` utility function to backtranform scores from logged values to original data values. 

## Improved
- `margot_subset_batch()` now working as it should for complex conditions.

# [2025-03-21] margot 0.3.2.5
## Improved
- `margot_process_binary()`: if a variable already ends with "_binary" it is not processed again as binary. Also removed dependency on the `color` package.
- `coloured_histogram_shift()`... and other deprecated functions no longer exported or maintained.
- `margot_plot_shift()` - improved to print mean value and also to show +/- sd of distribution
- `margot_causal_forest()` - default parameter is 0.5 training. 

# [2025-03-19] margot 0.3.2.4
## Improved
- `margot_subset_batch()` - now correctly passes all parameters required by `margot_plot()`. 
- `margot_plot()` - default is now: interpret_all_E_gt1 = TRUE (all results with non-null Evalues reported).


# [2025-03-19] margot 0.3.2.3
## Improved
- `margot_subset_batch()` - now exports plots and explanations for all submodels models (causal forests).

# [2025-03-19] margot 0.3.2.2
## New
- `margot_subset_batch()` batch process subsetted models (causal forests)
- `margot_plot_rate_batch()` replaces `margot_plot_batch_rate()`, which is deprecated. 

## Improved
- `margot_subset_model()` - streamlined


# [2025-03-19] margot 0.3.2.1
## Improved
- `group_tab()` now only in helper functions
- `margot_plot()` reverted to remove errors.
- `margot_compare_groups()`, `margot_interpret_qini()` removed redundant function.


# [2025-03-19] margot 0.3.2.0
## New 
- added `.strict_exposure_outcome_censoring()` helper function for strict dyadic censoring -- now encompassed by `margot_process_longitudinal_data_wider()` but added for backward compatibility.

## Improved
-  `coloured_histogram()`, `margot_impute_carry_forward()`, `margot_process_longitudinal_data_wider()`, `margot_lmtp_evalue()`,`margot_plot_batch_rate()`, `margot_plot_boxplot()`, `margot_plot_boxplot_covariate()`,  `margot_plot_discontinuity()`, `margot_plot_histogram()`, `margot_plot_slope_covariate_batch()`, `margot_plot_slope_covariate_combo()`, `margot_plot_slope_covariate()`, `margot_plot_slope()`, `margot_plot_individual_responses()`, `margot_wide_machine()`, `margot_process_longitudinal_data()`, `margot_count_dyads()`, `margot_count_ids()` correct package calls, explicit namespace calls.
- `group_tab()` function re-factored
- `margot_interpret_marginal()` corrected order & small improvements
- `margot_plot()` function refactored for clarity.


# [2025-03-18] margot 0.3.1.9

## Improved

- `here_save()` removed the call to base R
- `here_read()` removed the call to base R
- `margot_combine_results()` - made helper function
- `margot_summary_panel()` - specified functions required
- `margot_censor()` - specified functions required by data.table (#' @importFrom data.table as.data.table copy set get uniqueN)
- `read_multiple_images()` - made internal
- `margot_wide_machine()` - stated explicit functions in dependencies (removed zoo)
- `margot_interpret_qini()` - improved for reporting
- `margot_policy()` - now allows subsetting specific models
- `margot_interpret_policy_batch()` - now allows subsetting specific models
- `margot_causal_forest()` - added roxygen code (forgotten in last update). 

# [2025-03-17] margot 0.3.1.8

## New
- `margot_rate()` produce a table for RATE estimates from batched processed causal forests.
- `margot_flip_forests()` when interpreting the effect as *benefitial* we may need to invert the outcomes. This function does this on already processed models.
- `margot_interpret_rate()` provides automated reporting of rate outputs, allowing users to specify whether AUTOC or QINI was targeted. 

## Improved
- `margot_causal_forest()` - now also computes RATE targeting the QINI when this is selected. 

# [2025-03-15] margot 0.3.1.7

## Improved
- `create_ordered_variable()` appends "_binary" when binary variables are made
- `margot_transition_table()` user may specify a table name.
- `margot_process_longitudinal_wider()` new parameters so that no continuous vars are scaled

# [2025-03-14] margot 0.3.1.6
## Improved
- `margot_interpret_marginal()` made robust when when units are not given in the the input model.

# [2025-03-13] margot 0.3.1.5
## Improved
- `margot_compute_gender_weights_by_wave()` actually added (was not omitted by accident in 0.3.1.4).

# [2025-03-12] margot 0.3.1.4
## Improved
- `margot_compute_gender_weights_by_wave()` correctly assigns weights to a target population, specified by the `target_wave` parameters.

# [2025-03-12] margot 0.3.1.3

## Improved
- `margot_censor()`: censoring is conditional on both the condition and the wave: if $\texttt{censor_final_wave = FALSE}$ then rows in the final wave (determined as $\texttt{max(dt[[wave_var]])}$) are preserved (default behaviour). This allows for estimating a per-protocal effect where changing a censoring condition is part of the causal effect of the intervention for the population at baseline.

# [2025-03-05] margot 0.3.1.2

## New
 - `margot_transition_table()` replaces `transition_table()` and `create_transition_matrix()` and will print out state change for multiple waves. Useful for longitudinal modified treatment policies. (Old functions soft deprecated as helper functions.)

# [2025-02-06] margot 0.3.1.1

## Improved
- `margot_plot` and `margot_interpret_marginal` now all reporting of all coefficients if E-value is above 1 (instead of 1.1, the default threshold). 

- `margot_censor` now deletes all values of a variable if the censoring indicator is set to 1 (or year_measured == 0), instead of merely changing the indicator. This can be helpful downstream of this function when handling dyadic data.

# [2025-02-05] margot 0.3.1.0

## Improved

- `margot_process_longitudinal_data_wider` can now handle dyadic data, so that if an agent's partner is censored, then the agent is also censored.

# [2025-01-18] margot 0.3.0.9

## New
- `margot_combine_results` Takes multiple results tables from `margot_plot` objects and combines them into a single
formatted table using `kableExtra`, with optional group headers for each section.

## Improved
-`margot_plot` has option `standardize_label` which when to display Standardised, Standardized, or to remove it altogether ("none").
  - For Risk Ratios (type = "RR"), the code always omits that word and shows the label as "Effect (Risk Ratio Scale)".
	- For Risk Differences (type = "RD"), if standardize_label = "NZ" you get "Standardised Effect (Difference Scale)"; if standardize_label = "US" you get "Standardized Effect (Difference Scale)"; and if standardize_label = "none", you get "Effect (Difference Scale)".

# [2025-01-17] margot 0.3.0.8

## Improved
- `margot_plot` has a new parameter `include_coefficients` which if set to TRUE will remove the coefficients from a graph. This makes it easier to view the results. 
-  minor fix to `margot_save_png` so that it defaults to the output folder set by `push_mods`

# [2024-12-29] margot 0.3.0.7

## Improved
`margot_impute_carry_forward()` - prevents imputing variables at end of study if they are included as time-varying variables.

# [2024-12-29] margot 0.3.0.6

## Improved
`margot_impute_carry_forward()` is more efficient. Can carry forward more than one-wave if a future observation is made and 'require_one_observed' is set to "TRUE".

# [2024-12-08] margot 0.3.0.5

## New
- re-factored `margot_make_tables()` to work with the table1 package, for extending flexibility of tables (esp with latex.)

# [2024-12-07] margot 0.3.0.4

## New
- `margot_prop_missing()` - utility function to get propoportion of missing data at baseline. 

# [2024-11-26] margot 0.3.0.3

## Fixed
- `margot_amelia_to_mice()` - fixed to be unconstrained to previous workflow.

# [2024-11-11] margot 0.3.0.2

## Fixed
- `margot_plot()` label now reads "causal difference" rather than "causal risk difference".

# [2024-11-09] margot 0.3.0.1

## New
-`margot_impute_carry_forward` - carry forward last observed value (for handling 'prodigal' id's that are lost and found).

## Fixed
- `margot_wide_machine` - removed functionality now handled by `margot_impute_carry_forward` 


# [2024-11-06] margot 0.3.0.0

## New
- `margot_make_table()` - flexible longitudinal tables 
- `margot_amelia_to_mice()` - converts `Amelia` output to `mice` output. 

# [2024-10-30] margot  0.2.3.80

## New

- `margot_count_ids()` - track cumulative counts of participants, returning participants, 

## Improved
- `margot_count_dyads()` - more informative information. 

# [2024-10-30] margot 0.2.3.70
## Improved
- `margot_interpret_marginal` clean appearance

## Improved
- `margot_plot` and `margot_interpret_marginal` functions overhauled again to focus on compact reporting.


# [2024-10-29] margot 0.2.3.60

## Improved
- `margot_plot` and `margot_interpret_marginal` functions overhauled again to focus on compact reporting.


# [2024-10-29] margot 0.2.3.50
- `margot_plot` and `margot_interpret_marginal` functions

# [2024-10-29] margot 0.2.3.40

## Improved
- `margot_plot` to work with new `margot_interpret_marginal` function

# [2024-10-29] margot 0.2.3.16

## Improved
- `margot_interpret_marginal` removed cli formatting for compatibility with quarto documents

# [2024-10-28] margot 0.2.3.15

## Improved
- `margot_wide_impute` - small bug fix, na indicators not automatically created (functionality was lost at previous iteration)

# [2024-10-27] margot 0.2.3.14

## Improved
- `margot_process_longitudinal_data_wider` - more robust.
- `margot_wide_impute` - small bug fix. 

# [2024-10-26] margot 0.2.3.13


## New

- `margot_censor` create censoring indictors for correct handling of lost-to-follow-up when conditions are not met, includes a clustering variable (useful for romantic dyads)

## Improved

- `margot_wide_machine` now correctly handling multiple time points. 

# [2024-10-26] margot 0.2.3.12

## Improved

- `margot_wide_machine()` correctly handles NA values at baseline, and prints out message

# [2024-10-23] margot 0.2.3.11

## Improved

- `margot_save_png()` better defaults
- `margot_plot()` enhancements, and improved documentation


# [2024-10-23] margot 0.2.3.10
## Improved
- `margot_save_png()` flexibly handles any plot object, not merely ggplot2 objects.


# [2024-10-22] margot 0.2.3.9

## Improved
- `margot_interpret_marginal()` gives correct interpretation of 'strong' evidence using Evalues.

# [2024-10-02] margot 0.2.3.8
- `margot_process_longitudinal_data_wider()` performance enhancement. Users can save the outcome variable even if previous exposures are missing.  Useful for `lmtp` survival models

# [2024-09-27] margot 0.2.3.7 :)!


## Improved
- `margot_process_longitudinal_data_wider()` performance enhancement
- `margot_wide_machine()` simplified.  We now have a time-varying treatment workflow in place! 

# [2024-09-27] margot 0.2.3.6

## New
- `margot_wide_machine()` converts wide data to long data so using indicators for missing observations, which allows for non-parametric stacked learning in `lmtp` without multiple-imputation assumptions. Also handles more than three time-points. Optional `imputation_method = 'mice` allows users to impute, while also creating NA dummy variables for non-parametric learning.

- `margot_process_longitudinal_data_wider()` extends flexibility of `margot_process_longitudinal_data()` to more than three waves, and allows users to specify variable names. 

# [2024-09-26] margot 0.2.3.5

## New
- helper function `back_transform_estimates()` is unique for the marginal plots and marginal interpretation, to avoid confusions with back-transforming helpers for split-points in policy trees. 


## Improved
- `margot_plot()`, and `margot_interpret_marginal()` produce interpretable results. Fixed issue with `margot_plot()` when risk ratios are selected, where colours were not being plotted. 


# [2024-09-25] margot 0.2.3.4

## New
- `margot_plot_slope_covariate_combo()` - batch multiple `margot_plot_slope_covariate()` plots onto one graph using `patchwork`.


## Improved
- `margot_plot_slope_covariate()` improved for flexibility




# [2024-09-25] margot 0.2.3.3


## Improved

- `margot_plot_histogram()` now take optional `vertical_facets` parameter, allowing for more interpretable time-series graphs.
-  placed all internal function under `helpers.R` in the R directory, to avoid clutter. 


# [2024-09-25] margot 0.2.3.2


## Improved

- `margot_plot()` `margot_interpret_marginal()` now back transform values to data scale. 

# [2024-09-24] margot 0.2.3.1

## Improved

- `margot_plot_policy_tree()`, `margot_plot_decision_tree()`, `margot_interpret_policy_tree()`, `margot_plot_qini_tree()` use same global function names. New helper functions back-transform logged values (as well as z-transformed values) so that we get interpretations on the data scale for variables that have been log-transformed.  This aids with interpretation. 


# [2024-09-20] margot 0.2.3.0

## New

-  Refactored causal tree graphs and interpretations for flexible labelling and for providing both standardised results (where relevant), and results on the data scale. Makes the interpretation of policies easier to understand. 
- `margot_count_dyads()` counts dyads in a longitudinal dataset. 
- `margot_summary_panel()` summaries participants by panel wave; counts unique participants by wave, ...
- `margot_interpret_policy_batch()` interprets the policytree results. 

## Improved

- `margot_summary_tables()` - now pass multiple tables, better exposure plots.
- `margot_interpret_policy_tree()` - refactored: now returns results on data scale, better labels. 
- `margot_plot_policy_tree()` - refactored: now returns results on data scale, better labels. 



## Deprecated

- `margot_batch_policy()`

# [2024-09-19] margot 0.2.1.64

## Improved

- `margot_plot()`,`margot_interpret_marginal()`, `group_tab()` overhauled so that now we get reporting back-tranformed from standardised effects to effects on the data scale -- greatly benefiting interpretations.
- `transform_to_original_scale()` new helper introduced to back-transform estimates.

# [2024-09-18] margot 0.2.1.63

- `margot_save_png()` replaces `margot_plot_save_png()` for consistent function labelling, and to spare a burden of remembering function names.

# [2024-09-17] margot 0.2.1.62

- `margot_compare_groups()` added bold formatting to alert readers to reliable group differences



## Improved

- `margot_interpret_qini()` - now formats tables to alert readers to where prioritising results are reliably *worse* or **better** than than none.



# [2024-09-17] margot 0.2.1.60

## Improved

- `margot_plot_create_options()` updated to work with improve plotting workflow

## Deprecated

- `compute_difference()` now use the more general `margot_compare_groups()` workflow. 

## Removed 

- Removed the following deprecated functions from vignettes, instead use [https://github.com/go-bayes/boilerplate](https://github.com/go-bayes/boilerplate)
    - `boilerplate_measures`
    - `boilerplate_methods_additional_sections`
    - `boilerplate_methods`
    - `boilerplate_methods_causal_interventions`
    - `boilerplate_methods_confounding_control`
    - `boilerplate_methods_eligibility_criteria`
    - `boilerplate_methods_identification_assumptions`
    - `boilerplate_methods_missing_data`
    - `boilerplate_methods_sample`
    - `boilerplate_methods_statistical_estimator`
    - `boilerplate_methods_target_population`
    - `boilerplate_methods_variables`
    - `create_ordered_variable_custom`
    - `margot_compute_gender_weights`
    - `margot_create_bibliography`
    - `margot_create_database`
    - `margot_grf_subset_table`
    - `margot_merge_databases`
    - `manager_boilerplate_measures`
    - `compute_difference()`




# [2024-09-17] margot 0.2.1.59

## New
- `margot_plot_save_png()` saves a margot_plot output graph as a png, user can change width, heigh, dpi, and specify a path... 

## Improved
- `margot_plot()` automatic saving of the output with optional timestamps
- `margot_plot_multi_arm()` modified to work with new and improved `margot_plot()`

# [2024-09-16] margot 0.2.1.58
## New
- `margot_compare_groups()` compare treatment effects by groups and evaluate evidence for differences 

# [2024-09-16] margot 0.2.1.57

## New
- `margot_plot_multi_arm()` wrapper for `margot_plot` that enables each plots/tables for multi arm treatment models

## Improved
- reporting of multi arm treatment models in `margot_plot_qini()` is easier to follow. 
- `margot_lmtp()` now has automatic saving of models with optional prefix label and optional time-stamping. Also actually saves table when computing contrasts with only the null model. 

# [2024-09-16] margot 0.2.1.56

## Improved

- `margot_interpet_qini()` robust for both binary and multi-arm treatments. 
- `margot_plot_qini()` correct label for binarhy treatments
- `margot_batch_policy_tree()` correctly modified function added: commputes multiple 'spends'

# [2024-09-15] margot 0.2.1.55

## New

- `margot_interpet_qini()` interprets results of the qini curves at pre-specified levels. 


## Improved

- `extract_qini_data()` improved handling of `margot_multi_arm_causal_forest()`
-  numerous plot functions enhanced to produce "NZ" instead of "Nz"

# [2024-09-12] margot 0.2.1.54

## New

- `margot_summary_cate_difference_gain()` computes the difference in average gains and the integrated difference
between a reference curve (maq object) and a comparison curve at a specified spend level -- to see if there is support for CATEs

## Improved

- `compute_qini_curves_multi_arm()` - modified so that we can now get quantitative estimates for support for CATEs
- `margot_multi_arm_causal_forest()` - enhanced in several ways, for example to support `margot_summary_cate_differences()`
- `margot_causal_forest()` - likewise enhanced.
- `margot_batch_policy()` now outputs `margot_summary_cate_difference_gain() models by default 

# [2024-09-12] margot 0.2.1.53

## Improved

- `margot_summary_tables()` plots take upper case letters, remove '_'
- `margot_adjust_weights()` censored individuals are assigned zero weights, and only uncensored individuals contribute to the final analysis. 

# [2024-09-12] margot 0.2.1.52

## Improved

- Fixed `margot_plot_response_timeline()` to print dates, and to optionally save a 'png` image. 

# [2024-09-12] margot 0.2.1.51

## Improved

- `margot_plot_discontinuity()` and `margot_plot_slope()` have correct end years (+1 final wave, as waves overlap years)

# [2024-09-12] margot 0.2.1.50

## New

- `margot_plot_boxplot_covariate()` descriptive trends by groups


## Improved

- `margot_plot_slope_covariate()` automatic title, save png, and optional time stamp
- `margot_plot_individual_responses()` fixed so there is no missingness

# [2024-09-12] margot 0.2.1.49

## Improved

- reverted `compute_qini_curves` (only works with binary vars)
- `margot_causal_forest()` now working again

# [2024-09-12] margot 0.2.1.48

## Improved

- `margot_multi_arm_causal_forest()`
- `extract_qini_data()` improved to work with `margot_multi_arm_causal_forest()`

## New
- `compute_qini_curves_multi_arm()` internal function to support `margot_multi_arm_causal_forest()`



# [2024-09-11] margot 0.2.1.47

## Improved
-  Coordinated the following functions to play well with `margot_multi_arm_causal_forest()`
- `margot_plot_qini()`
- `extract_qini_data()``
- `compute_qini_curves()`

# [2024-09-11] margot 0.2.1.46

## Improved

- `hear_read` does not require that an `.rds` file is passed. 
- `extract_qini_data` made robust


# [2024-09-11] margot 0.2.1.45

## New

`build_formula_str` - helper function now documented. Used in `causal_contrast_engine` and `causal_contraset_marginal`

## Improved

- `causal_contrast_marginal` and `causal_contrast_marginal` modified to accommated breaking change in `WeigthIt` package. 
- `double_robust_marginal` checks if object is a winmids object.

# [2024-09-02] margot 0.2.1.44

## Improved

- `margot_plot_individual_responses()` now plotting all cases by default. Default `random_draws` of 100. 

# [2024-09-02] margot 0.2.1.43

## Improved

- `margot_plot_histogram()`, optional coloured mean/sd lines.

# [2024-09-02] margot 0.2.1.42

## Improved

- `margot_plot_individual_responses()`. Now handles factors, and robust to missing waves. 
- `margot_plot_boxplot()` different colours for boxplots if a single variable is passed over multiple waves. 

# [2024-09-02] margot 0.2.1.41


## New features

- `margot_plot_individual_responses()`. New function to allow random plotting of responses in a subset of the sample, useful for investigating individual trajectories of change. 
- `margot_plot_boxplot()` Now user supplies `wave` values, allowing more flexible and precise plotting of intervals. Has optional prefixes. The coordinates of the graph may be optionally flipped.



# [2024-09-02] margot 0.2.1.40

## New features

- `margot_plot_categorical()` for visualising categorical data distributions.
- `margot_plot_shift()` for visualising shifts in data distributions with highlighted ranges.

## Deprecations

The following functions have been deprecated in favor of the new functions:

- `margot_plot_hist()` is deprecated. Use `margot_plot_histogram()` instead.
- `coloured_histogram()` is deprecated. Use `margot_plot_histogram()` instead.
- `coloured_histogram_shift()` is deprecated. Use `margot_plot_shift()` instead.
 `coloured_histogram_quantiles()` is deprecated. Use `margot_plot_categorical()` instead.

These deprecated functions will continue to work but will issue warnings. They will be removed in a future version of the package.



# [2024-09-02] margot 0.2.1.39

## Improved
- `margot_plot_slope()` allows faceting


# [2024-09-02] margot 0.2.1.38

## New 
- `margot_plot_histogram()` new histogram that's more informative and more robust than previous attempts. Can be used for multiple variables and multiple waves. 

## Improved
- `margot_plot_boxplot()` made robust to single outcome in single wave. 

# [2024-09-02] margot 0.2.1.37

- `margot_plot_discontinuity()`, `margot_plot_slope()`, `margot_plot_slope_covariate()` automatically print number of unique participants and unique number of observations in the title, if no title is passed. 


# [2024-09-02] margot 0.2.1.36

## Improved

- `margot_plot_discontinuity()` now being read to namespace.

# [2024-09-01] margot 0.2.1.35

## New
- `margot_plot_slope()` descriptive trends in continuous variables over time; user may pass historical events which are denoted by dashed vertical lines on the plot.
- `margot_plot_slope_covariate()` descriptive trends by covariates over time. 
- `margot_plot_bloxplot()` descriptive trends using boxplots + facets.
- `prepare_panel_data()`helper function to get panel data into shape for plotting response timelines for repeated measures studies.
-  `margot_response_timeline()` plot histogram of response timelines for repeated measures studies.

## Improved

- `here_save_qs()` and `here_read_qs()` report where and object was saved and how large it is. 
- `here_save()`and `here_read()`, ditto, and also ask users to specify a directory path, defaulting to `push_mods` if none is supplied


# [2024-09-01] margot 0.2.1.34

## New
- `margot_plot_discontinuity()` create longitudinal graphs

## Improved
-  `margot_size()` has cli alerts

# [2024-08-30] margot 0.2.1.33

## New
* `read_multiple_images()` utility function to read batchs of images, for presentations, articles etc. 

# [2024-08-30] margot 0.2.1.32

## New

* `margot_plot_batch_rate`, creates and save rate plots from multiple causal forests outputs.. 

# [2024-08-29] margot 0.2.1.31

## Improved

* `margot_batch_policy()` allows user to save plots automatically, with different sizes and resolutions. 

# [2024-08-29] margot 0.2.1.30

## Improved

* `margot_subset_model()` returns subset of results the user requests.


# [2024-08-29] margot 0.2.1.29

## New

* `margot_subset_model()` subsets causal forests for both categorial and binary exposures.


## Deprecated

* `margot_grf_subset_table()`, functions replaced by `margot_subset_model()`, use this new function instead.


# [2024-08-28] margot 0.2.1.28

## Improved

* `margot_qini_plot()` better labels for binary models.

## New

* `margot_plot_exposure()` - utility to plot change in the exposure variable from baseline. 
* `margot_size()` - utility to check size of object

## Improved

* `margot_summary_table()` - now provides optional graphs to show densities/distributions of the exposure and outcomes at baseline, exposure waves, and end of study. 


# [2024-08-27] margot 0.2.1.26

## Improved

* `margot_plot()` - consistent names for results table if these are modified using the new `label_mapping` option. 
* `here_save_qs()` and `here_read_qs()` minor tweaks.

# [2024-08-27] margot 0.2.1.25

* `margot_plot()` and `margot_plot_create_options()` now allow custom labels, which flexibly combine with defaults.

# [2024-08-27] margot 0.2.1.24

## Improved

* robust reporting/error handling in `margot_causal_forest()`, and its helper funciton `compute_qini_curves()`

# [2024-08-26] margot 0.2.1.23

## Deprecations

* `boilerplate_measures()` is deprecated. Use `boilerplate::boilerplate_report_variables()` instead.
* `boilerplate_methods_causal_interventions()` is deprecated. Use `boilerplate::boilerplate_report_causal_interventions()` instead.
* `boilerplate_methods_confounding_control()` is deprecated. Use `boilerplate::boilerplate_report_confounding_control()` instead.
* `boilerplate_methods()` is deprecated. Use `boilerplate::boilerplate_report_methods()` instead.
* `boilerplate_methods_eligibility_criteria()` is deprecated. Use `boilerplate::boilerplate_report_eligibility_criteria()` instead.
* `boilerplate_methods_identification_assumptions()` is deprecated. Use `boilerplate::boilerplate_report_identification_assumptions()` instead.
* `boilerplate_methods_missing_data()` is deprecated. Use `boilerplate::boilerplate_report_missing_data()` instead.
* `boilerplate_methods_sample()` is deprecated. Use `boilerplate::boilerplate_report_sample()` instead.
* `boilerplate_methods_statistical_estimator()` is deprecated. Use `boilerplate::boilerplate_report_statistical_estimator()` instead.
* `boilerplate_methods_target_population()` is deprecated. Use `boilerplate::boilerplate_report_target_population()` instead.
* `boilerplate_methods_variables()` is deprecated. Use `boilerplate::boilerplate_report_variables()` instead.
* `margot_create_database()` is deprecated. Use `boilerplate::boilerplate_manage_measures()` instead.
* `manager_boilerplate_measures()` is deprecated. Use `boilerplate::boilerplate_manage_measures()` instead.
* `margot_create_bibliography()` is deprecated. Use `boilerplate::boilerplate_report_measures()` instead.
* `margot_merge_databases()` is deprecated. Use `boilerplate::boilerplate_merge_databases()` instead. (Note the plural 'databases' in the new function name.)

* to obtain these new functions, use `devtools::install_github("go-bayes/boilerplate")`

# [2024-08-22] margot 0.2.1.22

## Improved

* `margot_create_bibliography` now prints binary tags correctly.  Improved presentation of items, with measures first. 

* `boilerplate_methods_variables` now accepts `margot_create_bibliography`.

# [2024-08-22] margot 0.2.1.21

## Improved

* `margot_create_database` further improved: deleted unnecessary prompts, back_up function in place. 


# [2024-08-21] margot 0.2.1.20

## Improved

* `boilerplate_measures` and `margot_create_bibliography` better printout.  However, again, must develop a package specifically for boilerplates. 


# [2024-08-21] margot 0.2.1.19

## Improved

* `margot_create_database` made more robust, menus work better, and you can copy information from other citations.  (It's clear this will need to be a separate package,... expect a move down the track)

# [2024-08-21] margot 0.2.1.18

## Improved

* `margot_lmtp` more informative reporting and progress bars using `cli`

# [2024-08-21] margot 0.2.1.17

## Improved

* `margot_plot_decision_tree` - user may remove 'Action" labels for cleaner graph, if desired (default is to remove them.)

# [2024-08-21] margot 0.2.1.16

## New

* `margot_plot_create_options` helper function to automate graph production.




# [2024-08-21] margot 0.2.1.15

## Improved

* `margot_plot_policy_tree()`, `margot_plot_qini`, `margot_plot_decision_tree` defaults to nice labels, with informative messages. 

# [2024-08-21] margot 0.2.1.14

## New

* `margot_interpret_marginal` has consistent syntax with `margot_plot`. 
* `transform_table_rownames` to allow for nicer tables with clear labels. 
* overhauled `margot_plot` function so that it produces nice labels, and so that it also generates an interpretation. 

## Improved

* `margot_create_bibliography` markdown output improved with `cli`
* considerably decluttered output of `boilerplate_methods`

## Deprecations

* `margot_interpret_table` replaced by `margot_interpret_marginal`


# [2024-08-20] margot 0.2.1.13

## Improved

* informative `cli` messages for `create_ordered_variable`, `margot_process_binary_vars`, `margot_summary_tables`, `margot_log_transform_vars`, `margot_propensity_model_and_plots`, `margot_process_longitudinal_data`, `margot_causal_forest`, `margot_multiarm_causal_forest`, `margot_batch_policy`

# [2024-08-19] margot 0.2.1.12

## Improved

* integrated `cli` messages for user experience, in `margot_create_database`, `margot_merge_databases`, and `boilerplate_methods` (more to follow.)



# [2024-08-19] margot 0.2.1.11

## New

* `margot_create_database`: allows for creation of bibliography databases. 
* `margot_merge_databases`: allows for merging of bibliography databases. 
* `margot_create_bibliography`: purpose-build for appendices in which all measures are reported. 


## Improved

- restored `coloured_histogram` to package (still useful)


## Deprecations

- `manager_biolerplate_measures` surpassed by `margot_create_database`

# [2024-08-19] margot 0.2.1.10

## Improved
* `boilerplate_measures` overhauled to allow bibliography by sections (for outcomewide studies)
* considerably improved reporting in `boilerplate_methods`, including: selective sections to report. 
* overhauled `boilerplate_methods` for simple and clear reporting
* simplified `biolerplate_methods_variables` to act mostly as a wrapper for `boilerplate_measures`

# [2024-08-19] margot 0.2.1.9

## New
* `boilerplate_methods_additional_sections`

## Improved

* baseline_missing_data_proportion passed to `boilerplate_methods_missing_data`.
* all boilerplate functions now accessible with package (not just internal), allowing for better selective use. 
* allow selective printing of `boilerplate_methods`

# [2024-08-18] margot 0.2.1.8

## New

* `boilerplate_methods` function allows first pass automated reporting. 
*  helper functions include: `boilerplate_methods_sample`, `boilerplate_methods_eligibility_criteria`, `boilerplate_methods_identification_assumptions`, `boilerplate_methods_statistical_estimator`, `boilerplate_methods_confounding_control`, `boilerplate_methods_missing_data`, `boilerplate_methods_causal_interventions`
* implemented 

## Improved

* `manager_boilerplate_measures` now flexibly handles strings for references when 'string_is ...' is used in the reference category that `manager_boilerplate_measures` creates.

* `manager_boilerplate_measures` has been streamlined.

# [2024-08-17] margot 0.2.1.7

## New

* added `batch_edit_measures` within `manager_boilerplate_measures` to allow batch editing of measures database.

## Improved

* `manager_boilerplate_measures` improved by allowing user-friendly navigation forward and backward, and more sensible defaults. requires `rlang`.

# [2024-08-16] margot 0.2.1.6

## New

* `boilerplate_measures` produces an appendix of measures and items uses from variable inputs (for Quarto manuscripts).
* `manager_boilerplate_measures` allows to you add / modify an existing database. 

## Improved

* `margot_plot` consistent syntax: user specifies save_plot as TRUE or FALSE (default = TRUE).

# [2024-08-15] margot 0.2.1.5

## Improved

* `margot_plot` has auto-save and numerous small improvements.

# [2024-08-14] margot 0.2.1.4

## New

* `margot_adjust_weights` to streamline creating inverse probability of censoring weights longitudinally, allowing for sample_weights at baseline. 

# [2024-08-14] margot 0.2.1.3

## New

* `margot_propensity_model_and_plots` a one stop shop for evaluating balance on the treatment: plots & diagnostics generated.


## Restored

* `coloured_histogram()` back by popular demand. 

## Improved

* tweaks to defaults for plots to make them more legible on small computer screens.

# [2024-08-13] margot 0.2.1.2

## Improved

* `margot_plot_policy_combo`, `margot_policy_tree`, `margot_batch_policy` and  now accepts arguments for `margot_plot_decision_tree` and `margot_plot_policy_tree`, leading to customisable policy visualisations. 

* Bigger defaults for x and y axis text in `margot_plot_policy_tree` for legibility.


# [2024-08-12] margot 0.2.1.1

## New

* `margot_batch_policy` runs `margot_policy_tree` for all outcomes in a model. 


# [2024-08-11] margot 0.2.1.0

## New

* `margot_plot_policy_combo`: creates a combination plot for `margot_plot_decision_tree` and `margot_plot_policy_tree()`, easing the burden of interpretation. 


## Improved

* `margot_plot_decision_tree`: policy action leafs different colours (user may specify palette). Defaults to `ggokabeito::scale_fill_okabe_ito()` to match `margot_plot_policy_tree()`
*  `margot_policy_tree` outputs a `margot_plot_policy_combo` in addition to the other otuputs.

## Fixed

* removed `split_vars` from the `margot_causal_forest` and `margot_mulit_arm_causal_forest`
* `margot_plot_decision_tree` correct tree arrangement

# [2024-08-11] margot 0.2.0.9

## Fixed

* `margot_plot_policy_tree` correctly renders decision tree, allows for individual plots for decision leafs, and collects guides.
* error in rendering of `margot_plot_decision_tree`, fixed: function now includes internal tests.
* removed `split_vars` from the `margot_causal_forest` and `margot_mulit_arm_causal_forest`

## Removed

* `debug_node_data_with_positions`, internal function - utility now included with `margot_plot_decision_tree`



# [2024-08-10] margot 0.2.0.8

## New

* `margot_plot_decision_tree` creates policy tree decision rule graphs in a ggplot2 format, so that the graph may be combined `margot_plot_policy` to more effectively communicate decision rules to policy makers. 

* `debug_node_data_with_positions` internal so that `margot_plot_decision_tree` works properly

## Fixed

* `margot_plot_decision_tree` requires `debug_node_data_with_positions`


## Improved

* Enhanced `margot_plot_policy` so that it produces a `margot_plot_decision_tree` graph, as well as its previous outputs.

# [2024-08-9] margot 0.2.0.7

## New

* `margot_interpret_policy_tree` interprets policy_tree outputs with outputs in either markdown or \LaTeX formats.
* `margot_policy_tree` wraps `margot_interpret_policy_tree`, `margot_plot_policy_tree`, `margot_qini_plot` and a decision tree visualisation within one function.

## Improved

* `margot_plot` will now work even if no title or subtitle is passed. 
* `margot_plot_policy_tree`: focus is not simply on plotting, rather than doing both plotting and interpreting. 
* `margot_interpret_table`: no longer requires specification of estimate. General explanation printed separately (as it is only used once).

# [2024-08-8] margot 0.2.0.6

## Improved
* Greatly enhanced the functionality of `margot_plot_policy_tree` so that it explains the result. 

## Fixed

* broken indexing when I changed `margot_causal_forest` and `margot_multi_arm_causal_forest` to use outcome names, rather than model names.

# [2024-08-8] margot 0.2.0.5

## Improved
* `margot_causal_forest` and `margot_multi_arm_causal_forest` now use outcome names, rather than model names, to name the outcomes


# [2024-08-7] margot 0.2.0.4

## New

* `margot_process_longitudinal_data` orders correctly for `lmtp` models by updating the censoring column `not_lost` such that it handles missing responses as well as attrition. The function additionally automatically dummy codes ordinal variables and standardises continuous variables. Presently it is only implemented for three waves, but in the future it will be expanded to handle arbitrarily many. 

## Fixed

* Functions `compute_qini_curves` and `extract_qini_data` to work with binary interventions

# [2024-08-7] margot 0.2.0.3

## New

* `margot_summary_tables` creates summary tables of baseline variables, exposure(s), and outcome(s) using the `gtsummary` package.

## Deprecations

* `margot_compute_gender_weights` is deprecated. Now use `margot_compute_gender_weights_by_wave`.


# [2024-08-7] margot 0.2.0.2

## Deprecations

* `coloured_histogram()` and `coloured_histogram_quantile()` are deprecated.
  Now use the new `margot_plot_hist()` instead.
* `create_ordered_variable_custom` is deprecated. Now use `created_ordered_variable`function with `custom_breaks = c(..)` to obtain custom breaks. 

## New
* `margot_plot_hist()` for plotting distributions of the exposure variable


# margot 0.2.0.1
* new table and plot functions for `grf` outputs

# margot 0.2.0

* improved subgroup comparison function 
* deprecated `compare_group_means` function, with `compare_group`allowing for contrasts of on both the causal difference and relative risk scales.
* new wrapper functions functions for `grf` plus visualising results.


# margot 0.1.2.1
* group_tab now works under the hood of margot_plot, so no need to specify explicitly
* group_tab now allows custom order for plot, not just by decreasing effect size
* numerous small enhancements to older parametric model options
* new logo

# margot 0.1.2

* minor fixes


# margot 0.1.1

* functions to interpret outputs of `lmtp` models
* functions to create tables
* functions to create graphs

# margot 0.1.0.1

* Minor bug fixes and performance improvements.


# margot 0.1.0

* Initial release: includes data exploration, model preparation, utility functions.
# [2025-09-18] margot 1.0.236

### New
- Added `margot_report_consensus_policy_value()` to report how much better a consensus policy performs than (a) treat-all (ATE baseline) and (b) treat-none (universal control), with 95% CIs from bootstrap SEs.
- Added `margot_table_consensus_policy_value()` to produce a compact, manuscript-ready table for consensus policy values per outcome and depth, for both contrasts.

### Improvements
- `margot_policy_tree_stability()` now persists consensus splits in the stability metrics to support threshold-aware interpretation and reporting.
- Depth-2 consensus now includes `threshold_sd` for both first-level nodes.
- `margot_interpret_stability()` and `margot_interpret_stability_batch()` can optionally include simple CIs for selection frequencies and thresholds in technical output (`include_ci = TRUE`).
- `summary.margot_stability_policy_tree()` displays a quick consensus policy value summary vs treat-all and control-all (light bootstrap, R=199).

### Notes
- All changes are backward compatible. New parameters default to previous behavior.
# [2025-09-19] margot 1.0.240 - 1.0.241
 
# [2025-09-25] margot 1.0.242

### New
- `margot_policy_workflow()`: one-call policy workflow that orchestrates depth selection (`margot_policy_summary_compare_depths()`), mixed-depth summary reporting (`margot_policy_summary_report()`), and optional interpretations (`margot_interpret_policy_batch()`). Adds `brief_include_group` to control whether the policy brief includes the Group column.
- Shared text helper `margot_policy_value_explainer()` for consistent definitions of policy value vs control-all/treat-all, coverage, uplift, and the identity PV(control-all) = Coverage × Uplift_Treated.

### Improvements
- `margot_policy_summary_compare_depths()` adds `min_gain_for_depth_switch` (default 0.005) to prefer depth-1 unless depth-2 yields a material out-of-sample gain.
- `margot_policy_summary_report()` now supports `audience = c("policy","research")` to drive policy-facing wording and includes `return_unit_masks` to return per-model evaluation masks (including `not_excluded_ids`) for downstream plotting and drill-downs. The compact policy brief now includes `Depth`, and an issue dropping depth-1 rows from the summary table when precomputed reports were supplied was fixed.
- `margot_interpret_policy_batch()` gains `policy_value_source = c("compute","use_coherent")` and `coherent_values` so interpretations can reuse PV rows from the summary report instead of recomputing CIs.

### Documentation
- Added `README.qmd` with quick-start usage of the new policy workflow and a summary of 1.0.242 changes.
# margot NEWS

## [2025-10-15] margot 1.0.260

### Fixes
- LMTP overlap grid now respects multi-wave density ratios when stored as `Matrix` or `data.frame` by coercing to base matrices before iterating waves. This restores one panel per wave × shift.
- Grid axis limits use `coord_cartesian()` to avoid dropping bars (no more "Removed rows" warnings when harmonising x/y ranges).
- Shift order in grids and text respects the user-supplied `shifts` vector without dropping additional wave panels.

### Improvements
- `margot_plot_lmtp_overlap_grid()` enforces robust defaults — `layout = "shifts_by_waves"` and `color_by = "shift"` — and prints a brief CLI info message when overriding user inputs. This stabilises mapping of waves/shifts and colouring across waves.
- `margot_interpret_lmtp_positivity()` diagnostics section reports per-wave, uncensored (r>0) summaries, aligning directly with practical positivity assessment.
- ESS labelling clarified: bullets report `ESS+/(N+)` for uncensored-only fractions and additionally `ESS+/(N_pt)` relative to person-time when helpful.
- Methods text clarifies that the `null` policy includes censoring adjustment to recover the baseline population, so null ratios need not be centred at 1.

### Internal
- Added internal helper `margot_palette("lab")` with expanded colours for `null`, `shift_zero`, and `ipsi_*` variants used by overlap plots. Not exported; no API changes.

### Notes
- No exported functions were added or removed; `_pkgdown.yml` unchanged.
# 1.0.262

- Added boilerplate provider hooks with glue support:
  - `options(margot.boilerplate)` accepts list/function/JSON for explainer/methods slots
  - Slots: `policy_value_explainer`, `methods_long`, `methods_short`, `methods_prereg`
  - `options(margot.boilerplate.acronyms)` and `options(margot.boilerplate.labels)` for global acronyms/labels
- Workflow now returns `method_explanation` (long/short/prereg) describing the actual analysis settings
- Added `signal_score` ("pv_snr", "uplift_snr", "hybrid") and `signals_k`:
  - Renders a "Signals Worth Monitoring" section ranking Neutral models by magnitude/uncertainty
  - Exposes a `signals_df` with top‑K ranked signals and metadata
  - Adds a programmatic `signals_df_all` with a full ranking across all models
  - Falls back to ranking non‑wins when Neutral is empty, ensuring interesting signals are surfaced
  - Fixes a rendering bug where signal scores were computed after classification (now computed before), so Neutral subsets include the score columns
- Present‑tense, general prose in interpretation; moved harm flags into a separate Risk Notes section
- Soft‑deprecate Neutral in policy audience (configurable via `show_neutral`)
- `prefer_stability` biases depth selection toward depth‑1 unless depth‑2 gains are clearly larger
- Acronym expansion opt‑in: `expand_acronyms` with user‑overrides via options
# [2025-10-22] margot 1.0.264
### Added
- IPSI context (odds‑free) in `margot_interpret_lmtp_positivity()` with a simple probability‑scale formula and small illustrative translations (domain‑agnostic; defaults to “exposure”).
- Deterministic policy context describing history‑dependent rules (e.g., A_t^d := d_t(A_t, H_t)) and listing included policies.
- Optional policy‑implied exposure rates by wave and overall using reweighted means; non‑binary exposures are thresholded via an indicator 1(A_t op tau) (defaults: op is >, tau = 0).
- Experimental `margot_plot_dev()` retains advanced bias analysis features (optional multiplicity correction, label mapping/renaming, interpretation), while the stable `margot_plot()` reverts to the pre‑multiplicity API.
 - `margot_plot()` now delegates to `margot_plot_dev()` under the hood, so existing scripts gain advanced options without code churn. We will continue to stabilise RR/E‑value handling.

### Changed
- Wave labels in positivity text respect user `label_mapping` (e.g., “Baseline (2018/19)”), supporting skipped panels.
- Clarified positivity vs target: diagnostics labelled “positivity; uncensored rows” with a note that estimation reweights to the baseline cohort via censoring adjustment.
- Per‑wave bullets include both `ESS+/(N+)` and `ESS+/(N_pt)`.
- Censoring wording clarified: “censoring to next wave” for intermediate waves, “censoring end of study” for the final exposure; shift summaries show “censoring (zeros across person‑time)”.
- LaTeX sanitisation of common glyphs in generated text ($\to$, $\pm$, $\ge$, $\le$, $\approx$, $\times$).
- `margot_plot_lmtp_overlap_grid()`: suppress deprecated `layout` warning unless `layout` is explicitly supplied.
 - `margot_plot_dev()`: hides subtitle line when subtitle is empty (no extra spacing under title).

### Docs
- Rd parameter docs avoid LaTeX macros to prevent Rd warnings; pkgdown reference updated to include `margot_multi_evalue` and `margot_plot_dev`.

### Note
- We now route `margot_plot()` through the experimental engine to preserve advanced options while we stabilise RR/E‑value logic. Further improvements to robust RR E‑values and multiplicity adjustments will ship in a follow‑up.
