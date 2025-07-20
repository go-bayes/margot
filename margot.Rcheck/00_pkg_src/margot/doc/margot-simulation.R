## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(margot)   # core simulation + estimation tools
library(dplyr)    # tidy manipulation
library(tidyr)    # data reshaping
library(ggplot2)  # graphics for examples

## ----fig.cap = "Figure 1: data-generating process underlying `margot_simulate()`. Solid arrows are structural paths; dashed arrows indicate optional feedback loops and censoring."----
# build a toy DAG with DiagrammeR for illustration only (not evaluated when package is built)
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(
    "digraph {graph [rankdir = LR, bgcolor = none]
      node [shape = box, fontname = Helvetica]
      subgraph cluster0 {label = \"Wave 0\"; style = dashed; B; t0_L;}
      subgraph cluster1 {label = \"Wave 1\"; style = dashed; t1_A; t1_L; t1_Y;}
      subgraph cluster2 {label = \"Wave 2\"; style = dashed; t2_A; t2_L; t2_Y;}
      B -> t1_L -> t1_A -> t1_Y
      B -> t1_A
      t1_Y -> t2_A [style = dashed]  // optional y_feedback
      t1_A -> t2_L [style = dashed]  // covar_feedback
      t1_A -> censor1 [style = dashed]
      censor1 [shape = circle, label = \"C\"]
    }"
  )
}

## ----simple-example-----------------------------------------------------------
# generate data in wide format (default)
basic_dat <- margot_simulate(n = 500, waves = 3, seed = 2025)

# examine structure
str(basic_dat[, 1:10])

# summarize treatment and outcome
summary(basic_dat[c("t1_A1", "t2_A1", "t3_A1", "t4_Y")])

## ----het-sim------------------------------------------------------------------
het_dat <- margot_simulate(
  n        = 4000,
  waves    = 3,
  exposures = list(
    A1 = list(
      type = "binary",
      het  = list(modifier = "B2",  # effect modifier
                  coef     = 0.6)    # γ: interaction strength
    )
  ),
  exposure_outcome = 0.4,  # marginal main effect β
  seed = 2027
)

## ----het-check----------------------------------------------------------------
fit_het <- lm(t4_Y ~ t3_A1 * B2, data = het_dat)
summary(fit_het)$coefficients

## ----feedback-sim-------------------------------------------------------------
# Without feedback
no_fb_dat <- margot_simulate(
  n          = 2000,
  waves      = 5,
  y_feedback = 0,     # no Y → A dependency
  wide       = TRUE,
  seed       = 101
)

# With feedback
fb_dat <- margot_simulate(
  n           = 2000,
  waves       = 5,
  y_feedback  = 0.8,   # strong Y → A dependency
  params      = list(exp_L1_coef = 0.3), # optional A → L feedback
  wide        = TRUE,
  seed        = 101
)

## ----feedback-analysis--------------------------------------------------------
# Calculate treatment rates for both datasets
get_treatment_rates <- function(data, label) {
  data %>%
    select(starts_with("t") & ends_with("_A1")) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "wave_var",
      values_to = "prop_treated"
    ) %>%
    mutate(
      wave = as.numeric(sub("t([0-9]+)_A1", "\\1", wave_var)),
      feedback = label
    )
}

# Combine rates from both simulations
combined_rates <- bind_rows(
  get_treatment_rates(no_fb_dat, "No feedback"),
  get_treatment_rates(fb_dat, "With feedback (0.8)")
)

# Display the comparison
combined_rates %>%
  filter(wave > 0) %>%
  tidyr::pivot_wider(names_from = feedback, values_from = prop_treated) %>%
  arrange(wave)

## ----feedback-plot, fig.alt = "Comparison of treatment probability trends"----
# Plot comparison
ggplot(combined_rates %>% filter(wave > 0), 
       aes(x = wave, y = prop_treated, color = feedback, group = feedback)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Treatment Assignment Rates Across Waves",
    subtitle = "Comparing simulations with and without outcome feedback",
    x = "Wave",
    y = "Proportion Treated",
    color = "Simulation"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("No feedback" = "darkgray", 
                                "With feedback (0.8)" = "steelblue"))

## ----positivity-plot, fig.alt="Histogram showing propensity score distributions for treated and control groups with good overlap"----
# wave-3 propensity model: A1 ~ B2
# Filter to complete cases for this analysis
het_complete <- het_dat %>%
  filter(!is.na(t3_A1) & !is.na(B2))

ps_mod <- glm(t3_A1 ~ B2, family = binomial, data = het_complete)
het_complete$ps3 <- predict(ps_mod, type = "response")

ggplot(het_complete, aes(ps3, fill = factor(t3_A1))) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  labs(
    x    = "estimated propensity score",
    fill = "A1 at wave 3"
  ) +
  theme_minimal()

## ----power-template, eval = FALSE---------------------------------------------
# power_grid <- tibble::tibble(N = seq(500, 5000, by = 500))
# 
# power_grid$power <- vapply(power_grid$N, function(n) {
#   mean(replicate(300, {
#     dat <- margot_simulate(n = n, waves = 4,
#                            y_feedback = 0.7,
#                            exposures = list(A1 = list(type = "binary",
#                                                       het = list(modifier = "B1", coef = 0.5))),
#                            exposure_outcome = 0.3, wide = TRUE))
#     t.test(t5_Y ~ t3_A1, data = dat)$p.value < 0.05
#   }))
# }, numeric(1))
# 
# ggplot(power_grid, aes(N, power)) +
#   geom_line() +
#   geom_hline(yintercept = 0.8, linetype = "dashed") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(y = "Power", x = "Sample size")

