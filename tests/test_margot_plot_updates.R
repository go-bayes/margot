# test script for updated margot_plot functionality

library(margot)

# test data with traditional columns
test_data_traditional <- data.frame(
  `E[Y(1)]-E[Y(0)]` = c(0.5, -0.3, 0.2),
  `2.5 %` = c(0.2, -0.5, -0.1),
  `97.5 %` = c(0.8, -0.1, 0.5),
  E_Value = c(2.5, 1.8, 1.4),
  E_Val_bound = c(1.8, 1.3, 1.0),
  check.names = FALSE
)
rownames(test_data_traditional) <- c("Outcome1", "Outcome2", "Outcome3")

# test data with new ATE column
test_data_ate <- test_data_traditional
names(test_data_ate)[1] <- "ATE"

# test data with ATT column
test_data_att <- test_data_traditional
names(test_data_att)[1] <- "ATT"

cat("Test 1: Traditional column with default settings\n")
cat("================================================\n")
p1 <- margot_plot(test_data_traditional, type = "RD")
print(p1$transformed_table)

cat("\n\nTest 2: ATE column (should be detected automatically)\n")
cat("=====================================================\n")
p2 <- margot_plot(test_data_ate, type = "RD")
print(p2$transformed_table)

cat("\n\nTest 3: ATT column with rename_ate = TRUE\n")
cat("==========================================\n")
p3 <- margot_plot(test_data_att, type = "RD", rename_ate = TRUE)
print(p3$transformed_table)
cat("\nInterpretation should mention 'average treatment effects on the treated'\n")

cat("\n\nTest 4: Traditional column with rename_ate = TRUE (should rename to ATE)\n")
cat("=========================================================================\n")
p4 <- margot_plot(test_data_traditional, type = "RD", rename_ate = TRUE)
print(p4$transformed_table)

cat("\n\nTest 5: Test rename_evalue parameter\n")
cat("=====================================\n")
p5 <- margot_plot(test_data_traditional, type = "RD", rename_evalue = TRUE)
print(p5$transformed_table)
cat("\nColumns should now be named 'E-Value' and 'E-Value Bound'\n")

cat("\n\nTest 6: Custom rename_ate with string\n")
cat("======================================\n")
p6 <- margot_plot(test_data_traditional, type = "RD", rename_ate = "Custom Effect")
print(p6$transformed_table)

cat("\n\nTest 7: All renaming options together\n")
cat("======================================\n")
p7 <- margot_plot(test_data_traditional, type = "RD", 
                  rename_ate = TRUE, 
                  rename_evalue = TRUE,
                  rename_cols = TRUE)
print(p7$transformed_table)

cat("\n\nAll tests completed!\n")