# Summarize IPSI probabilities from transition tables

Consumes the output of \`margot_transition_table()\` (or a compatible
list of transition matrices) and computes transition probabilities for
each wave pair under incremental propensity score interventions (IPSIs).
Internally it calls \`margot_compute_ipsi_probability()\` for every
matrix, returning a tidy data frame that includes the natural transition
rate, counterfactual probabilities, fold increases, and the raw counts
used in each estimate.

## Usage

``` r
margot_transition_ipsi_summary(
  transitions,
  deltas = c(2, 5, 10),
  direction = c("up", "down"),
  pretty = FALSE,
  digits_prob = 1,
  digits_fold = 1
)
```

## Arguments

- transitions:

  Either the object returned by \`margot_transition_table()\` or a list
  of transition matrices (data frames or \`knitr_kable\`s containing
  \`"From / To"\`, the state columns, and a \`"Total"\` column). When an
  object from \`margot_transition_table()\` is supplied, the stored wave
  labels are used in the summary.

- deltas:

  Numeric vector of IPSI scaling factors (greater than 1). Passed to
  \`margot_compute_ipsi_probability()\`; defaults to the standard set
  \`c(2, 5, 10)\`.

- direction:

  Either \`"up"\` (State 0 cohort moving to State 1; the default) or
  \`"down"\` (State 1 cohort moving to State 0). Passed to
  \`margot_compute_ipsi_probability()\`.

- pretty:

  Logical; if \`TRUE\`, the table component is formatted for
  \`knitr::kable()\` / \`kbl()\` while numeric values remain attached
  under \`attr(table, "raw")\`.

- digits_prob:

  Integer digits for percentage columns when \`pretty = TRUE\`.

- digits_fold:

  Integer digits for fold-increase columns when \`pretty = TRUE\`.

## Value

A list with two elements:

- \`table\`:

  Data frame with one row per wave pair × delta containing the natural
  transition rate (with 95 fold increases, and raw counts
  (string-formatted when \`pretty = TRUE\`).

- \`report\`:

  Character vector of NZ-English sentences, with LaTeX math markup
  (e.g., \`\$\to\$\`, \`\$\delta\$\`, \`\$p'\$\`), summarising each wave
  pair.

## Details

The deltas match \`lmtp::ipsi()\`: the intervention divides the natural
probability of \*not\* reaching the target state by delta (a risk-ratio
shift), so \`p' = 1 - (1 - p)/delta\`. Direction selects the transition
of interest: \`"up"\` summarises movement from State 0 into State 1
(initiation); \`"down"\` summarises movement from State 1 into State 0
(the natural reading for a contingency that raises the probability of
the lower state among the upper-state cohort).

## Examples

``` r
transitions <- margot_transition_table(
  data.frame(
    id = rep(1:3, each = 3),
    wave = rep(c(2018, 2019, 2022), 3),
    religion = c(0, 0, 1,
                 0, 1, 1,
                 1, 1, 1),
    observed = 1
  ),
  state_var = "religion",
  id_var = "id",
  wave_var = "wave",
  observed_var = "observed",
  observed_val = 1,
  waves = c(2018, 2019, 2022)
)

margot_transition_ipsi_summary(transitions)
#> $table
#>   table_index table_name wave_from wave_to events at_risk initiations
#> 1           1    table_1      2018    2019      1       2           1
#> 2           1    table_1      2018    2019      1       2           1
#> 3           1    table_1      2018    2019      1       2           1
#> 4           2    table_2      2019    2022      1       1           1
#> 5           2    table_2      2019    2022      1       1           1
#> 6           2    table_2      2019    2022      1       1           1
#>   non_attenders direction delta delta_inverse natural_p natural_p_l natural_p_u
#> 1             2        up     2           0.5       0.5  0.01257912   0.9874209
#> 2             2        up     5           0.2       0.5  0.01257912   0.9874209
#> 3             2        up    10           0.1       0.5  0.01257912   0.9874209
#> 4             1        up     2           0.5       1.0  0.02500000   1.0000000
#> 5             1        up     5           0.2       1.0  0.02500000   1.0000000
#> 6             1        up    10           0.1       1.0  0.02500000   1.0000000
#>   counterfactual_p fold_increase
#> 1             0.75           1.5
#> 2             0.90           1.8
#> 3             0.95           1.9
#> 4             1.00           1.0
#> 5             1.00           1.0
#> 6             1.00           1.0
#> 
#> $report
#>  [1] "Wave 2018 $\\to$ 2019: The natural rate of moving up (State 0 $\\to$ State 1) was approximately 50.0\\% (95\\% CI 1.3\\%--98.7\\%) based on 1 transitions out of 2 at-risk participants in State 0."                                                                                
#>  [2] "Counterfactual probabilities follow $p' = 1 - (1 - p)/\\delta$ for this transition (the risk-ratio incremental propensity score intervention of `lmtp::ipsi()`, which divides the probability of remaining in the natural state by $\\delta$; $\\delta$ is not an odds multiplier)."
#>  [3] "For $\\delta = 2$ (so $1/\\delta = 0.5$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 75.0\\% (natural $p$ 50.0\\%; 1.5-fold increase)."                                                                                                          
#>  [4] "For $\\delta = 5$ (so $1/\\delta = 0.2$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 90.0\\% (natural $p$ 50.0\\%; 1.8-fold increase)."                                                                                                          
#>  [5] "For $\\delta = 10$ (so $1/\\delta = 0.1$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 95.0\\% (natural $p$ 50.0\\%; 1.9-fold increase)."                                                                                                         
#>  [6] ""                                                                                                                                                                                                                                                                                   
#>  [7] "Wave 2019 $\\to$ 2022: The natural rate of moving up (State 0 $\\to$ State 1) was approximately 100.0\\% (95\\% CI 2.5\\%--100.0\\%) based on 1 transitions out of 1 at-risk participants in State 0."                                                                              
#>  [8] "Counterfactual probabilities follow $p' = 1 - (1 - p)/\\delta$ for this transition (the risk-ratio incremental propensity score intervention of `lmtp::ipsi()`, which divides the probability of remaining in the natural state by $\\delta$; $\\delta$ is not an odds multiplier)."
#>  [9] "For $\\delta = 2$ (so $1/\\delta = 0.5$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."                                                                                                        
#> [10] "For $\\delta = 5$ (so $1/\\delta = 0.2$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."                                                                                                        
#> [11] "For $\\delta = 10$ (so $1/\\delta = 0.1$), the counterfactual probability of moving up (State 0 $\\to$ State 1) is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."                                                                                                       
#> 
#> $report_block
#> function (format = c("latex", "markdown")) 
#> {
#>     format <- match.arg(format)
#>     if (!length(report)) 
#>         return("")
#>     if (format == "latex") {
#>         paste0("\\begin{quote}\n", paste(report, collapse = "\n"), 
#>             "\n\\end{quote}\n")
#>     }
#>     else {
#>         paste(report, collapse = "\n")
#>     }
#> }
#> <bytecode: 0xc660daac0>
#> <environment: 0xc660ddce8>
#> 
```
