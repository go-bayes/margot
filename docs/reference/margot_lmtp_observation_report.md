# Report the calibration of predicted observation probabilities

`margot_lmtp_observation_report()` summarises, for each wave, how well
predicted probabilities of remaining observed describe who was in fact
observed among the records that reached that wave. Each column of the
three matrices is one wave and each row one participant. For wave \\j\\,
let \\R_j\\ be the records with `reached[, j]` true, \\y_i\\ the
indicator `observed[i, j]` and \\p_i\\ the predicted probability
`probability[i, j]`. The report gives the number reached, the number
that continued (\\\sum\_{i \in R_j} y_i\\), the Brier score
\\\|R_j\|^{-1} \sum\_{i \in R_j} (p_i - y_i)^2\\ and the log loss
\\\|R_j\|^{-1} \sum\_{i \in R_j} -\\y_i \log p_i + (1 - y_i) \log(1 -
p_i)\\\\, computed as `mean(-ifelse(y == 1, log(p), log1p(-p)))`.
Records that did not reach a wave do not enter it.

## Usage

``` r
margot_lmtp_observation_report(
  reached,
  observed,
  probability,
  wave_labels = NULL
)
```

## Arguments

- reached:

  Logical matrix, one row per participant and one column per wave, true
  where a record reached that wave and was at risk of being lost to
  observation. Missing values are not allowed.

- observed:

  Logical matrix of the same dimensions, true where a record that
  reached the wave remained observed. It must be non-missing wherever
  `reached` is true and may not be true where `reached` is false; its
  values elsewhere are ignored. Convert a 0/1 censoring indicator with
  `observed == 1`.

- probability:

  Numeric matrix of the same dimensions holding the predicted
  probability of remaining observed. It must lie between 0 and 1
  inclusive and be non-missing wherever `reached` is true; its values
  elsewhere are ignored.

- wave_labels:

  Optional labels, one per column. Column names of `reached` are used
  when available, then `Wave 1`, `Wave 2`, and so on.

## Value

A data frame of class `margot_lmtp_observation_report` with one row per
wave and columns `wave`, `reached`, `continued`, `brier`, `log_loss` and
`n_infinite_log_loss`. It contains no participant-level values.

## Details

Probabilities are not clipped. A probability of exactly 0 or 1 is valid:
when it matches the observed indicator the record contributes 0 to the
log loss, and when it contradicts it (a record observed with predicted
probability 0, or not observed with predicted probability 1) the
record's loss, and so the wave's log loss, is `Inf`. The column
`n_infinite_log_loss` counts those records, so an infinite log loss can
be traced to a confident wrong prediction rather than mistaken for a
numerical failure. A wave that no record reached has zero counts and
`NA` scores.

The report describes the observation model; it is descriptive and
attaches no positivity or estimator-stability judgement. Weight
concentration is reported separately by
[`margot_lmtp_analysis_weight_report()`](https://go-bayes.github.io/margot/reference/margot_lmtp_analysis_weight_report.md).

## See also

[`margot_lmtp_analysis_weight_report()`](https://go-bayes.github.io/margot/reference/margot_lmtp_analysis_weight_report.md),
[`margot_lmtp_censoring_report()`](https://go-bayes.github.io/margot/reference/margot_lmtp_censoring_report.md)

## Examples

``` r
reached <- cbind(rep(TRUE, 6), c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
probability <- matrix(c(0.9, 0.8, 0.7, 0.6, 0.9, 0.5, 0.8, 0.7, 0.6, 0.5, 0.9, 0.5), ncol = 2)
observed <- cbind(reached[, 2], c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
margot_lmtp_observation_report(reached, observed, probability, wave_labels = c("2019", "2020"))
#>   wave reached continued     brier  log_loss n_infinite_log_loss
#> 1 2019       6         4 0.1266667 0.3999962                   0
#> 2 2020       4         3 0.1750000 0.5108256                   0
```
