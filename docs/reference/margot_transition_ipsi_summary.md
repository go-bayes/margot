# Summarize IPSI probabilities from transition tables

Consumes the output of \`margot_transition_table()\` (or a compatible
list of transition matrices) and computes initiation probabilities for
each wave pair under incremental propensity score interventions (IPSIs).
Internally it calls \`margot_compute_ipsi_probability()\` for every
matrix, returning a tidy data frame that includes the natural initiation
rate, counterfactual probabilities, fold increases, and the raw counts
used in each estimate.

## Usage

``` r
margot_transition_ipsi_summary(
  transitions,
  deltas = c(2, 5, 10),
  pretty = FALSE,
  digits_prob = 1,
  digits_fold = 1
)
```

## Arguments

- transitions:

  Either the object returned by \`margot_transition_table()\` or a list
  of transition matrices (data frames or \`knitr_kable\`s containing
  \`"From / To"\`, \`"State 1"\`, and \`"Total"\` columns). When an
  object from \`margot_transition_table()\` is supplied, the stored wave
  labels are used in the summary.

- deltas:

  Numeric vector of IPSI scaling factors (greater than 1). Passed to
  \`margot_compute_ipsi_probability()\`; defaults to the standard set
  \`c(2, 5, 10)\`.

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
  initiation rate (with 95 fold increases, and raw counts
  (string-formatted when \`pretty = TRUE\`).

- \`report\`:

  Character vector of NZ-English sentences, with LaTeX math markup
  (e.g., \`\$\to\$\`, \`\$\delta\$\`, \`\$p'\$\`), summarising each wave
  pair.

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
#>   table_index table_name wave_from wave_to initiations non_attenders delta
#> 1           1    table_1      2018    2019           1             2     2
#> 2           1    table_1      2018    2019           1             2     5
#> 3           1    table_1      2018    2019           1             2    10
#> 4           2    table_2      2019    2022           1             1     2
#> 5           2    table_2      2019    2022           1             1     5
#> 6           2    table_2      2019    2022           1             1    10
#>   delta_inverse natural_p natural_p_l natural_p_u counterfactual_p
#> 1           0.5       0.5  0.01257912   0.9874209             0.75
#> 2           0.2       0.5  0.01257912   0.9874209             0.90
#> 3           0.1       0.5  0.01257912   0.9874209             0.95
#> 4           0.5       1.0  0.02500000   1.0000000             1.00
#> 5           0.2       1.0  0.02500000   1.0000000             1.00
#> 6           0.1       1.0  0.02500000   1.0000000             1.00
#>   fold_increase
#> 1           1.5
#> 2           1.8
#> 3           1.9
#> 4           1.0
#> 5           1.0
#> 6           1.0
#> 
#> $report
#>  [1] "Wave 2018 $\\to$ 2019: The natural initiation rate was approximately 50.0\\% (95\\% CI 1.3\\%--98.7\\%) based on 1 initiations out of 2 non-attenders."  
#>  [2] "Counterfactual probabilities follow $p' = 1 - (1 - p)/\\delta$ for this transition."                                                                     
#>  [3] "For $\\delta = 2$ (so $1/\\delta = 0.5$), the counterfactual initiation probability is about 75.0\\% (natural $p$ 50.0\\%; 1.5-fold increase)."          
#>  [4] "For $\\delta = 5$ (so $1/\\delta = 0.2$), the counterfactual initiation probability is about 90.0\\% (natural $p$ 50.0\\%; 1.8-fold increase)."          
#>  [5] "For $\\delta = 10$ (so $1/\\delta = 0.1$), the counterfactual initiation probability is about 95.0\\% (natural $p$ 50.0\\%; 1.9-fold increase)."         
#>  [6] ""                                                                                                                                                        
#>  [7] "Wave 2019 $\\to$ 2022: The natural initiation rate was approximately 100.0\\% (95\\% CI 2.5\\%--100.0\\%) based on 1 initiations out of 1 non-attenders."
#>  [8] "Counterfactual probabilities follow $p' = 1 - (1 - p)/\\delta$ for this transition."                                                                     
#>  [9] "For $\\delta = 2$ (so $1/\\delta = 0.5$), the counterfactual initiation probability is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."        
#> [10] "For $\\delta = 5$ (so $1/\\delta = 0.2$), the counterfactual initiation probability is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."        
#> [11] "For $\\delta = 10$ (so $1/\\delta = 0.1$), the counterfactual initiation probability is about 100.0\\% (natural $p$ 100.0\\%; 1.0-fold increase)."       
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
#> <bytecode: 0x153cbbd00>
#> <environment: 0x153cb8bb8>
#> 
```
