# test script to verify simplified output format
library(margot)

# create sample data with log-transformed outcome
test_data <- data.frame(
  outcome = c("t2_log_charity_donate_z", "t2_hours_volunteering_log_z"),
  `E[Y(1)]-E[Y(0)]` = c(0.133, 0.150),
  `2.5 %` = c(0.044, 0.052),
  `97.5 %` = c(0.222, 0.248),
  E_Value = c(1.8, 1.9),
  E_Val_bound = c(1.25, 1.27),
  original_var_name = c("t2_log_charity_donate_z", "t2_hours_volunteering_log_z"),
  check.names = FALSE
)

# add original scale columns (simulating back_transform_estimates output)
# For charity: log SD = 2.74, mean on log scale = 3.5
# For volunteering: log SD = 2.81, mean on log scale = 6.87 (log of ~960 minutes)
test_data$`E[Y(1)]-E[Y(0)]_original` <- c(0.133 * 2.74, 0.150 * 2.81)
test_data$`2.5 %_original` <- c(0.044 * 2.74, 0.052 * 2.81)
test_data$`97.5 %_original` <- c(0.222 * 2.74, 0.248 * 2.81)
test_data$unit <- c("", "")

# create mock original_df with log-transformed data
original_df <- data.frame(
  t2_log_charity_donate = rnorm(1000, mean = 3.5, sd = 2.74), # log($33) with SD 2.74
  t2_hours_volunteering_log = rnorm(1000, mean = 6.87, sd = 2.81) # log(960 minutes) with SD 2.81
)

# test the simplified interpretation
cat("Testing simplified output format:\n")
cat("================================\n\n")

result <- margot_interpret_marginal(
  df = test_data,
  type = "RD",
  order = "alphabetical",
  original_df = original_df,
  e_val_bound_threshold = 1.2,
  adjust = "bonferroni",
  alpha = 0.05,
  include_adjust_note = TRUE
)

cat(result$interpretation, "\n\n")
cat("Expected format examples:\n")
cat("- Charity: $XXX average increase (95% CI: $XX to $XXX)\n")
cat("- Volunteering: X,XXX minutes average increase (95% CI: X,XXX to X,XXX minutes)\n")
