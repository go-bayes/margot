# /Users/joseph/GIT/margot/tests/test_margot_policy_summary_report.r
#load package
devtools::load_all("/Users/joseph/GIT/margot/")


# directory for pulling models
# push_mods <- here::wf$summary$signals_brief_df:here("/Users/joseph/v-project\ Dropbox/data/25-hannah-grf-church-personality")
#original data frame before z transform
# original_df <- margot::here_read("df_wide", push_mods)

e example
# pull results from
# policy_tree_result_stability <- here_read_qs("policy_tree_result_stability", push_mods)


# function
# read labels 
label_mapping_all_flipped  <- here_read("label_mapping_all_flipped")

# focus on this function 
wf <- margot_policy_workflow(
  policy_tree_result_stability,
  original_df = original_df,
  label_mapping = label_mapping_all_flipped,
  se_method = "plugin",
  dominance_threshold = 0.1,
  strict_branch = TRUE,
  include_interpretation = TRUE,
  # min_gain_for_depth_switch = 0.005,
  audience = "policy",
  prefer_stability = TRUE,
  show_neutral = TRUE,
  expand_acronyms = FALSE,
  signal_score = "pv_snr",
  signals_k = 3)

cat(wf$best$depth_takeaways_text)

# view model
cat(wf$summary$text)
wf$policy_brief_df
str(wf, max.level = 1)
str(wf$method_explanation, max.level = 1)
str(wf$method_explanation, max.level = 1)
cat(wf$method_explanation$long)
# short
cat(wf$interpret$report_full)
cat(wf$interpret$policy_value_explanation)
wf$models_wins_or_has_ids
str(wf$summary, max.level = 1)
wf$best

wf$interpret


wf$summary$signals_brief_df
wf$summary$signals_brief_df_all
wf$method_explanation$long
wf$summary$signals_brief_df
wf$summary$signals_brief_df_all
here_save(wf, "wf")


make_brief <- function(df_in) {
rnd <- function(x) ifelse(is.na(x), NA, round(as.numeric(x), 3))
covp <- function(x) ifelse(is.na(x), NA, round(100 * as.numeric(x), 1))
data.frame(
  Outcome = df_in$outcome_label,
  Depth = df_in$depth,
  Signal = rnd(df_in$signal_value),
  PV [CI] = paste0(rnd(df_in$estimate_ctrl), " [", rnd(df_in$ci_lo_ctrl), ", ", rnd(df_in$ci_hi_ctrl), "]"),
  Uplift (treated) [CI] = paste0(rnd(df_in$uplift), " [", rnd(df_in$uplift_lo), ", ", rnd(df_in$uplift_hi), "]"),
  Coverage % = covp(df_in$coverage),
  Stability = rnd(df_in$stab_strength),
  Dominant split = df_in$dominant_split,
  check.names = FALSE
)
}
  signals_brief_df_all <- make_brief(wf$summary$signals_df_all)

  - And top‑K for the Neutral list you saw in text:

  signals_brief_df <- make_brief(wf$summary$signals_df)


# tab_strength is the model’s stability strength from the policy‑tree stability analysis.
#
#   - What it is: A 0–1 consensus metric indicating how consistently the consensus split(s) appear across stability iterations.
#       - Depth‑1: proportion of iterations where the consensus first split is selected (frequency).
#       - Depth‑2: the average of the node‑1 and node‑2 inclusion frequencies (each 0–1) for the first two splits.
#   - Where it comes from: stability$results[[model]]$stability_metrics$consensus_strength
#       - depth_1 = d1_consensus$frequency
#       - depth_2 = mean(d2_consensus$node1$frequency, d2_consensus$node2$frequency)
#   - How to interpret:
#       - Closer to 1 → more robust/repeatable split structure across resamples/splits.
#       - Around 0.5 → borderline consistency.
#       - Near 0 → unstable/rare split structure.
#   - How we use it: As a descriptive weight in the signal score (0.5 + 0.5 × stab_strength) to modestly favor signals with more stable trees. It’s not a p‑value; it’s a repeatability
#     indicator.


df <- wf$summary$coherent_policy_values
one <- subset(df, model == "model_t2_openness_z" & contrast == "policy - control_all")
half_width <- (one$ci_hi - one$ci_lo)/2
pv_snr <- abs(one$estimate) / half_width
pv_snr


# check
length(policy_tree_result_stability$results$model_t2_agreeableness_z$plot_data$predictions)


# read
best <- here_read("best")
str(best, max.level = 1)
names(best$depth_summary_df)

# That means you can do:
summary <- margot_policy_summary_report(policy_tree_result_stability,
  depths_by_model = best$depth_map,
  auto_recommend = TRUE,
  split_compact = TRUE,
  split_drop_zero = TRUE,
  split_top_only = TRUE,
  se_method = "plugin",
  label_mapping = label_mapping_all_flipped,
  original_df = original_df
)


names(summary)

# use these results
summary$depth_map
summary$coherence_audit
summary$interpretation
cat(summary$report)
cat(summary$practical_takeaways_text)
cat(summary$method_overview)
# cat(summary$policy_value_audit) # not there
# summary$split_table_compact_md_combined  # only showing 1L split values
# summary$split_table_compact # not useful
cat(summary$text)
# save
here_save(summary, "summary")

str(summary, max.level = 2)
summary
# depth-aware interpretation
policy_interpret <- margot_interpret_policy_batch(
  policy_tree_result_stability,
  model_names = summary$recommended_model_ids,
  depths_by_model = best$depth_map,
  output_format = "prose",
  label_mapping = label_mapping_all_flipped,
  original_df = original_df,
  report_policy_value = "both",
  return_as_list = TRUE,
  policy_value_R = 1000L,
  policy_value_seed = 42L,
  policy_value_ci_level = 0.95
)

cat(policy_interpret$report_full)
summary$recommended_model_ids
summary$group_table_df
here_save(policy_interpret, "policy_interpret")

# view
names(policy_interpret)

# view
cat(policy_interpret$report_full)

str(best, max.level = 1)
best
# plotting with the same depth map
policy_plots <- margot_policy(
  policy_tree_result_stability,
  # decision_tree_args = decision_tree_defaults,
  # policy_tree_args   = policy_tree_defaults,
  model_names = summary$wins_model_ids,
  depths_by_model = best$depth_map,
  original_df = original_df,
  max_depth = 2L,
  label_mapping = label_mapping_all_flipped,
  output_objects = "combined_plot"
)

names(wf$summary$neutral_model_ids)

length(best$recommended_model_ids)
