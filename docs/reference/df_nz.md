# Legacy New Zealand Attitudes and Values Study (NZAVS) Simulated Data

\`r lifecycle::badge("deprecated")\`

This dataset is deprecated and will be removed in a future version.
Please use \`fetch_margot_data()\` instead.

This dataset contains simulated data from The New Zealand Attitudes and
Values Study.

## Usage

``` r
df_nz

data(df_nz)
```

## Format

A data frame (see \`?df_margot_example\` for a similar structure)

A data frame with X rows and Y variables:

- age:

  participant age in years

- agreeableness:

  Score on the agreeableness scale. Includes items such as:

  i\.

  :   I sympathize with others' feelings.

  ii\.

  :   I am not interested in other people's problems. (reversed)

  iii\.

  :   I feel others' emotions.

  iv\.

  :   I am not really interested in others. (reversed)

- ...:

  Other variables

## Source

Simulated data. Copyright (c) 2024 margot package contributors.

## Details

This large dataset (27MB) is being phased out to reduce package size.
Use \`fetch_margot_data(version = "v1")\` to download the equivalent
dataset from the Open Science Framework.

The dataset is licensed under a custom license. You may use the data for
non-commercial purposes with appropriate credit, but redistribution of
the data in any form, including modified versions, is not permitted to
protect privacy.

The code in the margot package is licensed under the GNU General Public
License (GPL) v3.0. You can redistribute it and/or modify it under the
terms of the GPL as published by the Free Software Foundation. See
\<http://www.gnu.org/licenses/\>.

## See also

[`fetch_margot_data`](https://go-bayes.github.io/margot/reference/fetch_margot_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# old approach (deprecated)
data(df_nz)

# new approach
df <- fetch_margot_data(version = "v1")
} # }

data(df_nz)
head(df_nz)
#>   id wave      age agreeableness alcohol_frequency alcohol_intensity
#> 1  1 2018 40.31341      6.235760          4.031045         6.0138428
#> 2  1 2019 41.33449      6.216993          3.964873        10.0320085
#> 3  1 2020 42.24450      6.539885          3.950068         9.9997619
#> 4  2 2018 43.85094      6.790531          4.024855         1.0030336
#> 5  2 2019       NA            NA                NA                NA
#> 6  2 2020 45.80632      5.484677          3.985040         0.9976499
#>   alert_level_combined_lead   belong belong_accept belong_beliefs
#> 1                  no_alert 4.958810      7.000000       4.964069
#> 2                  no_alert 6.039168      6.954222       5.985981
#> 3                  no_alert 5.382137      6.017268       2.950767
#> 4                      <NA> 6.311607      5.976047       6.018113
#> 5                  no_alert       NA            NA             NA
#> 6                      <NA> 5.991122      6.015296       5.985946
#>   belong_routside_reversed  bodysat born_nz charity_donate conscientiousness
#> 1                 2.978316 1.969601       1     0.00000000          5.781907
#> 2                 5.046594 3.039433       1     0.01373293          6.004211
#> 3                 7.000000 6.042476       1   399.96241829          4.719577
#> 4                 6.980109 5.958325       1   199.96546646          5.238591
#> 5                       NA       NA       1             NA                NA
#> 6                 5.972425 6.030989       1   200.03938746          6.467995
#>   edu emotion_regulation_change_thinking_to_calm
#> 1   8                                   6.999934
#> 2   8                                   6.979494
#> 3   8                                   6.981677
#> 4   7                                   4.963946
#> 5   7                                         NA
#> 6   7                                   2.003005
#>   emotion_regulation_hide_neg_emotions emotion_regulation_out_control
#> 1                             6.044651                       2.028738
#> 2                             6.984203                       1.984919
#> 3                             6.043920                       2.018791
#> 4                             5.024856                       1.000000
#> 5                                   NA                             NA
#> 6                             4.995746                       1.000000
#>   emp_job_sat emp_job_secure emp_job_valued employed eth_cat extraversion
#> 1    5.971694       7.000000       6.023997        1    euro     4.740896
#> 2    6.019789       6.967036       6.022545        1    euro     4.737648
#> 3    7.000000       6.974346       6.006828        1    euro     4.724690
#> 4    5.987723       6.976047       7.000000        1    euro     3.960051
#> 5          NA             NA             NA       NA    euro           NA
#> 6    6.032520       6.961107       5.986527        1    euro     2.764532
#>   forgiveness gen_cohort gratitude hlth_bmi hlth_fatigue hlth_height
#> 1    4.020353      gen_X  6.986113 34.61469    0.9535594    1.856696
#> 2    4.969327      gen_X  7.000000 34.05344    2.0103681    1.826995
#> 3    5.366853      gen_X  6.999204 35.19452    2.9981118    1.786924
#> 4    5.618090      gen_X  6.367532 19.84119    0.9943821    1.620493
#> 5          NA      gen_X        NA       NA           NA          NA
#> 6    5.315945      gen_X  6.683664 19.87911    0.9742263    1.614109
#>   hlth_sleep_hours hlth_weight home_owner honesty_humility hours_charity
#> 1         8.546112   115.99149          1         5.954770   0.008159268
#> 2         8.019429   113.99033         NA         3.970874   0.000000000
#> 3         7.995631   117.95184          1         4.988354   0.028743272
#> 4         7.004173    54.01207          1         6.969159   6.011909720
#> 5               NA          NA         NA               NA            NA
#> 6         8.018394    53.95457          1         7.000000   0.000000000
#>   hours_exercise household_inc impermeability_group kessler_depressed
#> 1     0.51959112        200000             2.012835        0.00000000
#> 2     0.01974342        200000             4.964448        0.00000000
#> 3     0.00000000        140000             2.030814        0.00000000
#> 4     1.98318873        200000             4.994408        0.00000000
#> 5             NA            NA                   NA                NA
#> 6     7.02405270        160000             2.008591        0.02017855
#>   kessler_effort kessler_hopeless kessler_latent_anxiety
#> 1      0.9546004       0.00000000              0.9685371
#> 2      0.9633998       0.02006575              0.9830208
#> 3      1.9808154       0.04983561              1.7149373
#> 4      1.0289347       1.04402955              0.9997136
#> 5             NA               NA                    NaN
#> 6      0.9598355       0.00000000              0.6297783
#>   kessler_latent_depression kessler_nervous kessler_restless kessler_worthless
#> 1               0.037011885      0.98849863        0.9815225       0.046843363
#> 2               0.000000000      0.95881162        0.9716739       0.021033094
#> 3               0.006337577      3.03015091        0.0000000       0.009060714
#> 4               0.377568407      0.99305306        0.9758989       0.026907402
#> 5                       NaN              NA               NA                NA
#> 6               0.003030027      0.02515866        1.0125775       0.000000000
#>   kessler6_sum lifemeaning  lifesat lifesat_ideal lifesat_satlife male
#> 1     3.044502    6.468932 6.004578      5.970138        6.018526    1
#> 2     2.960963    5.978561 5.996101      6.018253        5.978932    1
#> 3     5.005375    5.549235 5.969862      5.961114        6.015522    1
#> 4     4.008829    5.971437 6.472409      6.000709        7.000000    0
#> 5           NA          NA       NA            NA              NA    0
#> 6     2.007682    5.954844 6.045588      5.016755        7.000000    0
#>   meaning_purpose meaning_sense  modesty neighbourhood_community neuroticism
#> 1        6.012352      7.000000 6.786189                6.951882    2.543777
#> 2        5.975751      6.044783 6.002267                6.005449    3.049655
#> 3        4.956256      5.997665 5.702049                5.986452    2.960180
#> 4        6.024632      6.043560 6.955113                3.012109    4.224377
#> 5              NA            NA       NA                      NA          NA
#> 6        5.979999      5.952246 6.977462                2.010986    4.456901
#>        nwi nz_dep2018 nzsei_13_l openness parent partner perfectionism
#> 1 8.020139   5.953996         48 6.502722      1       1      4.303349
#> 2 6.668893   6.023145         65 6.469405      1       1      3.670650
#> 3 4.618780   6.024971         65 6.237941      1       1      3.337095
#> 4 7.627521   3.048911         69 2.031197      1       1      2.363609
#> 5       NA   2.967774         69       NA     NA      NA            NA
#> 6 5.640812   3.036816         72 3.473353      1       1      2.953873
#>   permeability_individual pers_agreeable_feel_others_emotions
#> 1                7.000000                            6.038855
#> 2                7.000000                            6.005890
#> 3                6.964188                            6.010749
#> 4                5.027991                            7.000000
#> 5                      NA                                  NA
#> 6                5.021921                            7.000000
#>   pers_agreeable_no_interest_others pers_agreeable_no_interest_others_probs
#> 1                          6.028096                                6.017042
#> 2                          5.956827                                6.025382
#> 3                          7.000000                                7.000000
#> 4                          6.983117                                6.037968
#> 5                                NA                                      NA
#> 6                          6.037822                                2.013026
#>   pers_agreeable_sympathise_others pers_conscientious_chores_done
#> 1                         7.000000                       5.003450
#> 2                         7.000000                       5.978509
#> 3                         5.951258                       2.976528
#> 4                         7.000000                       7.000000
#> 5                               NA                             NA
#> 6                         6.960945                       5.998471
#>   pers_conscientious_forget_putback pers_conscientious_like_order
#> 1                          6.035918                      5.972150
#> 2                          6.048918                      5.988758
#> 3                          2.951705                      5.991320
#> 4                          1.003034                      6.969415
#> 5                                NA                            NA
#> 6                          7.000000                      6.977218
#>   pers_conscientious_make_mess pers_extraversion_dont_talkalot
#> 1                     5.955233                        6.040438
#> 2                     6.045258                        3.985668
#> 3                     6.990426                        4.970881
#> 4                     5.961425                        6.012815
#> 5                           NA                              NA
#> 6                     6.045320                        3.036480
#>   pers_extraversion_keepbackground pers_extraversion_life_party
#> 1                         3.998360                     6.039570
#> 2                         5.020996                     4.998691
#> 3                         4.991396                     6.031292
#> 4                         4.995249                     2.963147
#> 5                               NA                           NA
#> 6                         2.957594                     3.014112
#>   pers_extraversion_talk_peopleparties pers_honestyhumility_deserve_more
#> 1                             3.006411                          5.986706
#> 2                             5.025187                          3.976311
#> 3                             2.966148                          5.973563
#> 4                             1.976786                          7.000000
#> 5                                   NA                                NA
#> 6                             2.009894                          7.000000
#>   pers_honestyhumility_feel_entitled
#> 1                           7.000000
#> 2                           6.950590
#> 3                           6.038719
#> 4                           7.000000
#> 5                                 NA
#> 6                           7.000000
#>   pers_honestyhumility_pleasure_expensivegoods
#> 1                                     5.022716
#> 2                                     1.984692
#> 3                                     2.023260
#> 4                                     6.993182
#> 5                                           NA
#> 6                                     6.952615
#>   pers_honestyhumility_seen_expensivecar pers_neuroticism_mood_swings
#> 1                               5.955832                     1.994415
#> 2                               3.020957                     6.033852
#> 3                               6.048881                     2.045891
#> 4                               7.000000                     5.032754
#> 5                                     NA                           NA
#> 6                               7.000000                     6.043157
#>   pers_neuroticism_mostly_relaxed pers_neuroticism_seldom_blue
#> 1                        2.027604                     4.953003
#> 2                        1.977769                     2.038987
#> 3                        2.993304                     4.964138
#> 4                        4.966090                     1.964646
#> 5                              NA                           NA
#> 6                        1.997013                     5.952084
#>   pers_neuroticism_upset_easily pers_openness_difficult_abstraction
#> 1                      1.000000                            6.022979
#> 2                      2.021009                            4.962520
#> 3                      2.031607                            4.954873
#> 4                      5.002216                            2.012435
#> 5                            NA                                  NA
#> 6                      3.993217                            4.987355
#>   pers_openness_good_imagination pers_openness_interested_abstraction
#> 1                       7.000000                             6.014076
#> 2                       6.978520                             7.000000
#> 3                       6.995509                             6.048047
#> 4                       2.955771                             1.000000
#> 5                             NA                                   NA
#> 6                       4.989866                             1.000000
#>   pers_openness_vivid_imagination pol_wing political_conservative
#> 1                        7.000000 1.020237               1.000000
#> 2                        6.989238 1.000000               1.022343
#> 3                        6.960658 1.000000               1.042282
#> 4                        2.033375 4.014205               4.960227
#> 5                              NA       NA                     NA
#> 6                        2.960342 3.969545               4.954690
#>   power_no_control_composite power_no_control_composite_reversed
#> 1                   1.519361                            6.537026
#> 2                   3.457300                            4.531562
#> 3                   1.501127                            6.545508
#> 4                   1.011948                            7.000000
#> 5                        NaN                                 NaN
#> 6                   1.000000                            7.000000
#>   power_others_control power_self_nocontrol pwb_standard_living
#> 1             1.000000             2.025217            7.042358
#> 2             4.954861             1.999267            8.981462
#> 3             1.976260             1.035171            6.960540
#> 4             1.002600             1.000000            9.993654
#> 5                   NA                   NA                  NA
#> 6             1.000000             1.037436            9.969491
#>   pwb_your_future_security pwb_your_health pwb_your_relationships regc_2022_l
#> 1                 8.973228        7.023662               9.961455           2
#> 2                 7.963623        1.970227               9.038338           2
#> 3                 8.993340        7.984213               9.964077           2
#> 4                10.000000        9.954387              10.000000          13
#> 5                       NA              NA                     NA          13
#> 6                10.000000        9.986748               9.032489          13
#>   religion_believe_god religion_believe_spirit religion_bigger_denominations
#> 1                    0                       1                       not_rel
#> 2                    0                       0                       not_rel
#> 3                    0                       1                       not_rel
#> 4                    0                       0                       not_rel
#> 5                   NA                      NA                          <NA>
#> 6                    0                       1                       not_rel
#>   religion_identification_level religion_perceive_religious_discrim
#> 1                             1                            1.000000
#> 2                             1                            1.035010
#> 3                             1                            1.000000
#> 4                             1                            1.020197
#> 5                            NA                                  NA
#> 6                             1                            1.000000
#>   religion_religious rumination rural_gch_2018_l sample_frame
#> 1                  0 0.03677838                1          1.1
#> 2                  0 0.01604958                1          1.1
#> 3                  0 0.00000000                1          1.1
#> 4                  0 0.04074371                1          1.1
#> 5                 NA         NA                1          1.1
#> 6                  0 0.00000000                1          1.1
#>   sample_frame_opt_in sample_weights self_control_have_lots
#> 1                   0      0.6495057               4.987928
#> 2                   0      0.7789180               3.001922
#> 3                   0      0.8119713               5.026771
#> 4                   0      0.4022481               5.033600
#> 5                   0             NA                     NA
#> 6                   0      0.4794723               7.000000
#>   self_control_wish_more_reversed self_esteem selfesteem_failure_reversed
#> 1                        2.024191    5.983263                    5.049517
#> 2                        2.035422    6.024637                    6.010825
#> 3                        2.027122    6.016682                    5.038861
#> 4                        2.962663    6.697360                    7.000000
#> 5                              NA          NA                          NA
#> 6                        2.983433    6.285690                    7.000000
#>   selfesteem_postiveself selfesteem_satself sexual_satisfaction sfhealth
#> 1               5.959428           6.976774            5.963785 2.370515
#> 2               5.995463           6.045321            5.031907 3.692035
#> 3               5.979329           6.956274            6.030893 5.708331
#> 4               5.959445           7.000000            6.047659 7.000000
#> 5                     NA                 NA                  NA       NA
#> 6               6.020617           5.990747            3.965026 6.035785
#>   sfhealth_expect_worse_health_reversed sfhealth_get_sick_easier_reversed
#> 1                              1.000000                          3.048580
#> 2                              1.000000                          7.000000
#> 3                              6.005024                          6.006699
#> 4                              7.000000                          6.961256
#> 5                                    NA                                NA
#> 6                              3.991105                          7.000000
#>   sfhealth_your_health smoker  support support_help support_noguidance_reversed
#> 1             3.047783      0 7.000000     6.972380                    6.999528
#> 2             3.014775      0 6.959343     7.000000                    6.997369
#> 3             5.022465      0 6.967418     7.000000                    7.000000
#> 4             6.977021      0 7.000000     7.000000                    7.000000
#> 5                   NA     NA       NA           NA                          NA
#> 6             7.000000      0 7.000000     6.984364                    6.975150
#>   support_turnto vengeful_rumin year_measured
#> 1       6.959850       3.975229             1
#> 2       7.000000       3.014252             1
#> 3       7.000000       2.626865             1
#> 4       6.994214       2.363377             1
#> 5             NA             NA             0
#> 6       7.000000       2.678659             1
```
