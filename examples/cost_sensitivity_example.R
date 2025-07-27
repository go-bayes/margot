# Example: Cost Sensitivity Analysis with margot
# This example demonstrates how to analyze and visualize how treatment 
# allocation changes with different treatment costs

library(margot)
library(ggplot2)
library(patchwork)  # for combining plots

# assume you have causal forest results
# cf_results <- margot_causal_forest(...)

# 1. Basic cost sensitivity analysis for a single cost value
# -----------------------------------------------------------

# run qini with a specific treatment cost (e.g., expensive treatment)
qini_expensive <- margot_qini(
  cf_results,
  treatment_cost = 5,  # 5x more expensive than baseline
  model_names = "anxiety"
)

# interpret the results
interpret_expensive <- margot_interpret_qini(qini_expensive)
cat(interpret_expensive$qini_explanation)

# plot the qini curve - note the subtitle shows cost
plot_expensive <- margot_plot_qini(
  qini_expensive, 
  "model_anxiety",
  title = "QINI Curve - Expensive Treatment"
)


# 2. Systematic cost sensitivity analysis
# ---------------------------------------

# analyze multiple cost scenarios systematically
cost_sensitivity <- margot_qini_cost_sensitivity(
  cf_results,
  costs = c(0.2, 0.5, 1, 2, 5),  # from very cheap to very expensive
  model_names = c("anxiety", "depression"),
  spend_levels = c(0.1, 0.2, 0.4)  # check at multiple spend levels
)

# view summary of how optimal treatment changes with cost
print(cost_sensitivity$summary)


# 3. Visualizing cost sensitivity
# --------------------------------

# a) overlay plot - all costs on one graph for a single model
overlay_plot <- margot_plot_qini_cost_sensitivity(
  cost_sensitivity,
  model_name = "model_anxiety",
  plot_type = "overlay",
  title = "QINI Curves Across Treatment Costs: Anxiety",
  subtitle = "Lower costs enable treating more patients effectively"
)

# b) faceted plot - separate panel for each cost
facet_plot <- margot_plot_qini_cost_sensitivity(
  cost_sensitivity,
  model_name = "model_anxiety", 
  plot_type = "facet"
)

# c) summary plot - optimal treatment fraction vs cost
summary_plot <- margot_plot_qini_cost_summary(
  cost_sensitivity,
  model_names = c("model_anxiety", "model_depression"),
  spend_level = 0.2,
  metric = "comparison_gain"
) + 
  labs(
    title = "Treatment Effectiveness vs Cost",
    subtitle = "At 20% treatment capacity"
  )


# 4. Combining multiple visualizations
# ------------------------------------

# create a comprehensive visualization
combined_plot <- (
  overlay_plot + 
    theme(legend.position = "bottom") +
    labs(title = "A. QINI Curves by Cost")
) + (
  summary_plot + 
    theme(legend.position = "bottom") +
    labs(title = "B. Optimal Gain vs Cost")
) +
  plot_layout(ncol = 2, widths = c(1, 1))

print(combined_plot)


# 5. Interpreting results across costs
# -------------------------------------

# create custom interpretation comparing costs
interpret_cost_comparison <- function(cost_sens, model_name) {
  results_by_cost <- list()
  
  for (i in seq_along(cost_sens$costs)) {
    cost <- cost_sens$costs[i]
    cost_label <- names(cost_sens$results)[i]
    
    # get qini results for this cost
    qini_at_cost <- cost_sens$results[[cost_label]]
    
    # interpret
    interp <- margot_interpret_qini(qini_at_cost)
    
    # extract key info
    results_by_cost[[cost_label]] <- list(
      cost = cost,
      summary = interp$concise_summary,
      reliable_models = interp$reliable_model_names
    )
  }
  
  # create narrative
  cat("\n=== Cost Sensitivity Analysis Summary ===\n\n")
  
  for (cost_info in results_by_cost) {
    cat(sprintf("At treatment cost = %.1f:\n", cost_info$cost))
    cat(cost_info$summary, "\n\n")
  }
  
  # identify cost threshold where treatment becomes ineffective
  costs <- sapply(results_by_cost, function(x) x$cost)
  has_benefit <- sapply(results_by_cost, function(x) 
    length(x$reliable_models) > 0
  )
  
  if (any(!has_benefit)) {
    threshold_idx <- which(!has_benefit)[1]
    if (threshold_idx > 1) {
      cat(sprintf(
        "Note: Treatment becomes ineffective when cost exceeds %.1f\n",
        costs[threshold_idx - 1]
      ))
    }
  }
}

# run the interpretation
interpret_cost_comparison(cost_sensitivity, "model_anxiety")


# 6. Interactive exploration (if using Shiny)
# -------------------------------------------

# you could create an interactive app
library(shiny)

ui <- fluidPage(
  titlePanel("Treatment Cost Sensitivity Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("cost", 
                  "Treatment Cost:", 
                  min = 0.1, 
                  max = 10, 
                  value = 1,
                  step = 0.1),
      
      selectInput("model",
                  "Model:",
                  choices = names(cf_results$results),
                  selected = "model_anxiety")
    ),
    
    mainPanel(
      plotOutput("qiniPlot"),
      verbatimTextOutput("interpretation")
    )
  )
)

server <- function(input, output) {
  # reactive qini calculation
  qini_reactive <- reactive({
    margot_qini(
      cf_results,
      treatment_cost = input$cost,
      model_names = gsub("^model_", "", input$model)
    )
  })
  
  output$qiniPlot <- renderPlot({
    margot_plot_qini(qini_reactive(), input$model)
  })
  
  output$interpretation <- renderPrint({
    interp <- margot_interpret_qini(qini_reactive())
    cat(interp$qini_explanation)
  })
}

# run the app (uncomment to use)
# shinyApp(ui = ui, server = server)


# 7. Policy recommendations based on budget
# -----------------------------------------

recommend_policy <- function(cost_sensitivity, budget_per_person, model_name) {
  # find the highest cost we can afford
  affordable_costs <- cost_sensitivity$costs[cost_sensitivity$costs <= budget_per_person]
  
  if (length(affordable_costs) == 0) {
    return("Treatment is not affordable at any tested cost level")
  }
  
  # get results for affordable scenarios
  best_cost <- max(affordable_costs)
  cost_idx <- which(cost_sensitivity$costs == best_cost)
  
  # extract gains at different spend levels
  summary_subset <- cost_sensitivity$summary[
    cost_sensitivity$summary$model == model_name & 
    cost_sensitivity$summary$cost == best_cost,
  ]
  
  if (nrow(summary_subset) == 0) {
    return("No data available for this model")
  }
  
  # find spend level with highest gain
  best_spend_idx <- which.max(summary_subset$comparison_gain)
  best_spend <- summary_subset$spend_level[best_spend_idx]
  best_gain <- summary_subset$comparison_gain[best_spend_idx]
  
  sprintf(
    "With budget of $%.2f per person, recommend:\n" %s+%
    "- Treatment cost: $%.2f\n" %s+%
    "- Treat top %.0f%% of population\n" %s+%
    "- Expected gain: %.3f units",
    budget_per_person, best_cost, best_spend * 100, best_gain
  )
}

# example usage
cat(recommend_policy(cost_sensitivity, budget_per_person = 3, "model_anxiety"))