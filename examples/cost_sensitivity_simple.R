# Simple Cost Sensitivity Analysis Example
# ========================================

library(margot)
library(ggplot2)

# assume you have causal forest results already
# cf_results <- margot_causal_forest(...)

# 1. Single Cost Analysis
# -----------------------
# analyze at a specific treatment cost
qini_results <- margot_qini(
  cf_results,
  treatment_cost = 2.5,  # treatment costs 2.5x baseline
  model_names = "anxiety"
)

# interpret - will show cost in explanation
interpretation <- margot_interpret_qini(qini_results)
print(interpretation$qini_explanation)
# Output will include: "Treatment cost is set to 2.5." or similar

# plot - subtitle shows cost
plot <- margot_plot_qini(cf_results, "model_anxiety", treatment_cost = 2.5)
print(plot)  # subtitle will show "Treatment cost = 2.5"


# 2. Multiple Cost Scenarios
# --------------------------
# systematically analyze different costs
cost_analysis <- margot_qini_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2, 5),  # cheap to expensive
  model_names = "anxiety"
)

# a) overlay all curves on one plot
overlay_plot <- margot_plot_qini_cost_sensitivity(
  cost_analysis,
  model_name = "model_anxiety",
  plot_type = "overlay"
)
print(overlay_plot)

# b) separate panels for each cost
facet_plot <- margot_plot_qini_cost_sensitivity(
  cost_analysis,
  model_name = "model_anxiety", 
  plot_type = "facet"
)
print(facet_plot)

# c) summary plot showing optimal gain vs cost
summary_plot <- margot_plot_qini_cost_summary(
  cost_analysis,
  spend_level = 0.2  # at 20% treatment capacity
)
print(summary_plot)


# 3. Interpretation Across Costs
# ------------------------------
# the summary table shows key metrics at each cost
print(cost_analysis$summary)

# example output:
#   model         cost spend_level reference_gain comparison_gain difference_gain
# 1 model_anxiety  0.5         0.1          0.05            0.15            0.10
# 2 model_anxiety  0.5         0.2          0.10            0.25            0.15
# 3 model_anxiety  1.0         0.1          0.05            0.12            0.07
# 4 model_anxiety  1.0         0.2          0.10            0.20            0.10
# 5 model_anxiety  2.0         0.1          0.05            0.08            0.03
# 6 model_anxiety  2.0         0.2          0.10            0.14            0.04
# 7 model_anxiety  5.0         0.1          0.05            0.06            0.01
# 8 model_anxiety  5.0         0.2          0.10            0.11            0.01

# interpretation: as cost increases, the benefit of targeted treatment decreases


# 4. Key Insights from Cost Sensitivity
# -------------------------------------
# - Lower costs → steeper QINI curves → more people worth treating
# - Higher costs → flatter QINI curves → only treat highest-effect individuals  
# - At very high costs, CATE-based targeting may provide minimal benefit
# - The "break-even" cost where targeting stops being beneficial varies by outcome

# 5. Making Policy Decisions
# --------------------------
# if your budget is $3 per person, you want the highest cost ≤ $3
affordable_cost <- max(cost_analysis$costs[cost_analysis$costs <= 3])
print(paste("With $3 budget, use treatment cost:", affordable_cost))

# get the QINI results for that cost
best_scenario <- cost_analysis$results[[paste0("cost_", affordable_cost)]]
best_interpretation <- margot_interpret_qini(best_scenario)
print(best_interpretation$concise_summary)