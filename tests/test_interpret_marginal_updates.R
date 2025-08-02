# test updated margot_interpret_marginal messaging

library(margot)

# test 1: no reliable effects
cat("Test 1: No reliable effects\n")
cat("============================\n")
test_no_effects <- data.frame(
  ATE = c(0.1, -0.05, 0.02),
  `2.5 %` = c(-0.1, -0.2, -0.1),
  `97.5 %` = c(0.3, 0.1, 0.15),
  E_Value = c(1.2, 1.1, 1.05),
  E_Val_bound = c(0.9, 0.8, 0.7), # all below threshold
  check.names = FALSE
)
rownames(test_no_effects) <- c("Outcome1", "Outcome2", "Outcome3")

result1 <- margot:::margot_interpret_marginal(test_no_effects, type = "RD")
cat("\nInterpretation:\n")
cat(result1$interpretation, "\n\n")

# test 2: some reliable effects with new wording
cat("\nTest 2: Reliable effects with new wording\n")
cat("==========================================\n")
test_reliable <- data.frame(
  ATE = c(0.5, -0.3, 0.2),
  `2.5 %` = c(0.2, -0.5, -0.1),
  `97.5 %` = c(0.8, -0.1, 0.5),
  E_Value = c(2.5, 1.8, 1.4),
  E_Val_bound = c(1.8, 1.3, 1.0),
  check.names = FALSE
)
rownames(test_reliable) <- c("Outcome1", "Outcome2", "Outcome3")

result2 <- margot:::margot_interpret_marginal(test_reliable, type = "RD")
cat("\nInterpretation:\n")
cat(result2$interpretation, "\n\n")

# test 3: with adjustment note
cat("\nTest 3: No effects with adjustment note\n")
cat("========================================\n")
result3 <- margot:::margot_interpret_marginal(
  test_no_effects,
  type = "RD",
  adjust = "bonferroni",
  alpha = 0.05
)
cat("\nInterpretation:\n")
cat(result3$interpretation, "\n")
