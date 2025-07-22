# debug margot_interpret_marginal

library(margot)

# test data with ATE column
test_ate <- data.frame(
  ATE = c(0.5, -0.3, 0.2),
  `2.5 %` = c(0.2, -0.5, -0.1),
  `97.5 %` = c(0.8, -0.1, 0.5),
  E_Value = c(2.5, 1.8, 1.4),
  E_Val_bound = c(1.8, 1.3, 1.0),
  check.names = FALSE
)
rownames(test_ate) <- c("Outcome1", "Outcome2", "Outcome3")

# run group_tab first
cat("After group_tab:\n")
df <- margot:::group_tab(test_ate, type = "RD")
print(names(df))

# check what happens after filter
cat("\nAfter filter:\n")
df_f <- df %>% dplyr::filter(E_Value > 1, E_Val_bound > 1)
print(names(df_f))
print(df_f$outcome)