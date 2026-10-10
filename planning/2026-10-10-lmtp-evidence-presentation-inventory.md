# LMTP evidence presentation: inventory

Date: 10 October 2026. Status: inventory for the design pass; no code changed.

Joseph judged the print and plot methods for the LMTP evidence reports (`R/margot_lmtp_evidence_methods.R`, added in 1.3.1) hard to read and hard to act on, and judged that the DH model reports share both faults. The figures in John Mark's kava study (`arc/studies/2026/external_jm_lmtp_kava/analysis/05-render-report-figures.R`) present comparable evidence better. This note lists what the current methods show, which reader decision each report serves, the specific defects, and what the kava figures do differently. It is the brief for the design pass.

The renders here use synthetic data (`2026-10-10-lmtp-evidence-presentation/render-current.R`, seed 42, `margot` 1.3.1): 3,000 rows, six transitions, about 37% retained at the last transition, and two policies. The outputs are `current-evidence-plot.png` and `current-print.txt` in the same folder.

## Constraint carried forward

The 11 August 2026 ruling (`arc/planning-decisions/2026-08-11-lmtp-nonbinding-evidence-reporting-api.md`) states that the reports return no scientific verdict, support state, threshold classification or route action. The constructors and their stored tables are outside the scope of this change; the change concerns presentation. A reference value that defines the scale (a ratio of 1, a difference of 0, the share of the total weight under equal weights) is not a threshold. A registered reference band, such as the kava study's [0.1, 10] ratio band or its ±0.1 standardised-difference lines, is a threshold unless it is supplied by the caller from the registration and labelled as such. Question 1 below asks how to treat it.

## What each report is for

| Report | Reader's question | Action the answer informs |
| --- | --- | --- |
| Censoring | Where, and for whom, does follow-up thin out? Do the fitted observation probabilities approach zero? | Whether to state stronger reliance on the censoring model at a wave, or to inspect the learners. |
| Target projection | After weighting, does the sample match the target on the registered margins? Which cells are thin and carry heavy weight? | Whether to narrow the target or add an explicit statement. |
| Analysis weight | How large is the effective sample at each node for each policy? Is a low value caused by censoring or by weight variability? Does regularisation change much? | Whether a policy's estimate rests on a few participants; whether the registered regularisation does any work. |
| Evidence manifest | Which components exist, and what does each say in one line? | Where to look first. |

## Defects in the current methods

### Shared

1. **Composite figure.** `plot(<evidence report>)` stacks every component into one tall figure (four rows for two policies). Policies are drawn as separate, near-identical panels instead of being compared within one panel.
2. **No reference marks.** No ratio axis has a line at 1, and the Kish panel shows no equal-weight value. The role line ("carries no verdict, threshold or route action") is printed, but the scale references that make a display readable are omitted.
3. **Machine labels.** Cause codes (`zero_joint_ratio_with_zero_censoring_factor`), stage names (`raw`, `regularised`) and policy identifiers (`natural_course`) appear verbatim.
4. **Verbose transition labels.** "Time 10 to Time 11" overprints on the retention x-axis, so the labels cannot be read.

### Censoring report

5. **Censored rows mixed with supported rows.** The censoring-factor and joint-ratio summaries include the structural zeros of censored rows. At later transitions the median becomes 0 (synthetic transitions 5 and 6), so the panel repeats the retention panel and hides the distribution among participants who remain under observation, which is the distribution that bears on support. The kava study plots only positive ratios.
6. **Linear ratio axis.** Ratios are multiplicative. On a linear axis the upper tail dominates, and values below 1 are compressed into a sliver.
7. **"n missing" mislabelled.** For fitted probabilities, "n missing" counts rows that never reached the transition (structural), not failed predictions.
8. **Weighted–unweighted retention gap invisible.** The two retention lines overlap on a 0–100 axis. The gap is the quantity of interest, because it shows whether up-weighted groups are retained less, and the display does not show it.
9. **Zero-cause table breaks in print.** Long cause strings make `print()` wrap the table across column blocks (see `current-print.txt`), so the counts cannot be matched to their rows.

### Target-projection report

10. **Mixed units on one axis.** Percentage-point differences (categorical) and standardised differences (numeric) share an axis labelled "Source minus target". A standardised difference of 0.16 sits on an axis spanning ±10 percentage points and reads as zero.
11. **Movement under weighting not drawn.** Unweighted and weighted points are dodged rather than joined, so the reader cannot see the direction or size of the change.
12. **Small cells absent from the plot.** The stratum table holds counts and weight shares, but the plot omits it. Small cells that carry large weights are visible only in the printed table.

### Analysis-weight report

13. **Kish fraction conflates two causes.** Zero weights do not change the Kish effective sample size, $(\sum_i w_i)^2 / \sum_i w_i^2$; they change only its denominator, the row count. The reported Kish fraction therefore equals the share of rows with a positive weight times the Kish efficiency among those rows. In the synthetic run, the final-node fraction of 0.09 combines 37% retention with an efficiency of about 0.24 among retained rows. The report does not separate the two, although they call for different actions.
14. **Unstable summaries.** The median full weight is 0 once most rows are censored, and the maximum is a single participant's value, which moves erratically across nodes (130 at transition 5, 67 at transition 6). Neither shows concentration.
15. **Separate panel per policy.** The intervention and natural-course reports are drawn separately, although the reader's question is comparative.

### Evidence manifest (print)

16. **Fractional counts.** The one-line summaries give the Kish *n* with decimals ("1,492.36"), use "Kish/n" without saying what *n* is, and print a final-node maximum weight that gives no context for interpretation.

## What the kava figures do well

- **One question per figure.** Each figure answers one question, faceted by target group, with plain-language row labels and policy colours held constant across figures.
- **Ratio display.** Ratios are plotted on a log axis with a line at 1; only positive ratios are included, summarised by the mean and the 0.1%–99.9% interval.
- **Axes fitted to the data.** The retention axis runs from 88% to 100%, so the differences between outcome domains are visible.
- **Reference marks.** Dashed ±0.1 lines on the balance plot; a shaded [0.1, 10] band on the ratio plot.
- **One cumulative figure per policy and domain.** The cumulative heatmap gives the share of the cumulative ratio outside the band for each policy and domain, with the percentage printed in each tile.
- **Counts on the figure.** Transition tiles and response distributions carry the n and the percentage.

Limits of the kava figures:
- They are script code tied to one study, with hard-coded labels and limits.
- The band and the ±0.1 lines are thresholds in the sense of the 11 August ruling.
- Only the Wave 2 ratio has a support display.

## Not yet inspected

- The DH report cards: to confirm that the same defects appear in a fitted run, not only in synthetic renders.
- `margot_report_lmtp_positivity()` and `margot_lmtp_positivity_report()`: exposure-ratio displays, which may overlap with defect 5 and should share one ratio convention with the evidence methods.

## Questions for the design pass

1. **References.** Should the methods accept caller-supplied, registered reference values (a ratio band, a balance tolerance), drawn and labelled as "registered reference", with no classification returned? Or should they draw only the scale references: 1, 0, and the equal-weight share?
2. **Censoring.** Should support displays condition on the participants who reached a transition (positive ratios and their share), with censoring shown once, in retention?
3. **Kish decomposition.** Should the analysis-weight display be the decomposition (retention × efficiency among positive weights) for each policy on one panel, with regularisation shown as a change?
4. **Output form.** Should `plot()` return a single composite or a named list of figures, each sized for a report-card page? The kava figures are separate files.
5. **Labels.** Should callers supply transition, policy and variable labels through one argument shared by all methods, so that study code stops relabelling the outputs?
6. **Constructor changes.** Can the displays be built from the stored tables alone? The positive-only quantiles and the Kish decomposition need summaries that the constructors do not store now. Adding fields is a compatible extension (minor version), but it changes the constructors, which this change was not meant to touch.
