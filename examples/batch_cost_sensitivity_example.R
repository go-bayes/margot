# Batch Cost Sensitivity Analysis Example
# =======================================
# This example shows how to create and compare QINI curves across multiple
# models and treatment costs simultaneously

library(margot)
library(ggplot2)
library(patchwork)  # needed for grid plots

# assume you have causal forest results
# cf_results <- margot_causal_forest(...)

# 1. Basic Batch Cost Sensitivity Analysis
# ----------------------------------------

# analyze multiple models at different costs
batch_plots_grid <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2, 5),
  model_names = c("anxiety", "depression", "wellbeing"),
  plot_type = "grid"
)

# display the grid
print(batch_plots_grid)
# This creates a grid with:
# - Rows: different models
# - Columns: different costs
# Each panel shows how the QINI curve changes with cost


# 2. Individual Plots for Detailed Analysis
# -----------------------------------------

# get individual plots as a nested list
individual_plots <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2),
  model_names = c("anxiety", "depression"),
  plot_type = "list"
)

# access specific plots
anxiety_cheap <- individual_plots$model_anxiety$cost_0.5
anxiety_expensive <- individual_plots$model_anxiety$cost_2

# compare side by side
comparison <- anxiety_cheap + anxiety_expensive + 
  plot_annotation(
    title = "Anxiety Treatment: Cheap vs Expensive",
    subtitle = "QINI curves show steeper benefit with cheaper treatment"
  )
print(comparison)


# 3. Combined Faceted Plot
# ------------------------

# create a single plot with all models and costs
combined_plot <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2),
  model_names = c("anxiety", "depression", "wellbeing"),
  plot_type = "combined"
)
print(combined_plot)

# customize the combined plot
combined_custom <- combined_plot +
  theme(
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(face = "bold", size = 10)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    caption = "Lower costs enable treating more people effectively"
  )
print(combined_custom)


# 4. Using with margot_plot_qini_batch()
# --------------------------------------

# if you want to use the regular batch plotting with a specific cost:

# option A: regenerate on the fly (new feature)
plots_at_cost_2 <- list()
for (model in c("model_anxiety", "model_depression")) {
  plots_at_cost_2[[model]] <- margot_plot_qini(
    cf_results,
    model,
    treatment_cost = 2,
    regenerate_qini = TRUE  # this regenerates curves with new cost
  )
}

# option B: run margot_qini first with specific cost
qini_at_cost_2 <- margot_qini(
  cf_results,
  treatment_cost = 2
)

# then batch plot
batch_plots <- margot_plot_qini_batch(
  qini_at_cost_2,
  model_names = c("anxiety", "depression")
)


# 5. Analyzing the Summary Data
# -----------------------------

# run cost sensitivity analysis
cost_analysis <- margot_qini_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2, 5),
  model_names = c("anxiety", "depression", "wellbeing")
)

# check the improved summary table
print(cost_analysis$summary)
# now includes reference_gain, comparison_gain, difference_gain columns

# find which model is most sensitive to cost
library(dplyr)
cost_sensitivity_by_model <- cost_analysis$summary %>%
  filter(spend_level == 0.2) %>%
  group_by(model) %>%
  summarise(
    gain_range = max(comparison_gain, na.rm = TRUE) - 
                 min(comparison_gain, na.rm = TRUE),
    best_cost = cost[which.max(comparison_gain)]
  ) %>%
  arrange(desc(gain_range))

print(cost_sensitivity_by_model)
# shows which models are most affected by cost changes


# 6. Creating a Summary Visualization
# -----------------------------------

# plot gain vs cost for all models
library(tidyr)
summary_data <- cost_analysis$summary %>%
  filter(spend_level == 0.2) %>%
  select(model, cost, comparison_gain) %>%
  mutate(model = gsub("^model_", "", model))

ggplot(summary_data, aes(x = cost, y = comparison_gain, color = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5)) +
  labs(
    title = "Treatment Effectiveness vs Cost",
    subtitle = "At 20% treatment capacity",
    x = "Treatment Cost (log scale)",
    y = "CATE-based Gain",
    color = "Outcome"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 7. Policy Recommendations
# -------------------------

# function to recommend treatment strategy based on budget
recommend_treatment_strategy <- function(cost_analysis, budget_per_person) {
  recommendations <- cost_analysis$summary %>%
    filter(cost <= budget_per_person) %>%
    group_by(model) %>%
    filter(comparison_gain == max(comparison_gain, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(comparison_gain)) %>%
    select(model, cost, spend_level, comparison_gain)
  
  cat("=== Treatment Recommendations ===\n")
  cat(sprintf("Budget per person: $%.2f\n\n", budget_per_person))
  
  if (nrow(recommendations) == 0) {
    cat("No treatments are affordable at this budget level.\n")
  } else {
    cat("Recommended treatments (in order of effectiveness):\n")
    for (i in 1:nrow(recommendations)) {
      rec <- recommendations[i,]
      cat(sprintf(
        "%d. %s: Use cost $%.2f, treat %.0f%% of population, expected gain: %.3f\n",
        i, 
        gsub("^model_", "", rec$model),
        rec$cost,
        rec$spend_level * 100,
        rec$comparison_gain
      ))
    }
  }
}

# example: budget of $1.50 per person
recommend_treatment_strategy(cost_analysis, budget_per_person = 1.50)


# 8. Interactive Exploration
# --------------------------

# create a function to quickly explore different scenarios
explore_cost_scenario <- function(models, model_name, costs) {
  plots <- list()
  
  for (cost in costs) {
    p <- margot_plot_qini(
      models,
      paste0("model_", model_name),
      treatment_cost = cost,
      regenerate_qini = TRUE
    ) +
      labs(title = paste(model_name, "- Cost:", cost))
    
    plots[[length(plots) + 1]] <- p
  }
  
  # combine with patchwork
  patchwork::wrap_plots(plots, ncol = 2)
}

# explore anxiety at different costs
explore_cost_scenario(cf_results, "anxiety", c(0.5, 1, 2, 5))