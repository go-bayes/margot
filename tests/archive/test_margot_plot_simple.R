# simple test for margot_plot with new column types

library(margot)

# create test data that mimics margot_recompute_ate output
test_ate <- data.frame(
  ATE = c(0.5, -0.3, 0.2),
  `2.5 %` = c(0.2, -0.5, -0.1),
  `97.5 %` = c(0.8, -0.1, 0.5),
  E_Value = c(2.5, 1.8, 1.4),
  E_Val_bound = c(1.8, 1.3, 1.0),
  check.names = FALSE
)
rownames(test_ate) <- c("Outcome1", "Outcome2", "Outcome3")

cat("Testing margot_plot with ATE column...\n")
tryCatch(
  {
    result <- margot_plot(test_ate, type = "RD")
    cat("✓ margot_plot worked with ATE column\n")
    print(result$transformed_table)
  },
  error = function(e) {
    cat("✗ Error:", e$message, "\n")
  }
)

# test with ATT column
test_att <- test_ate
names(test_att)[1] <- "ATT"

cat("\nTesting margot_plot with ATT column...\n")
tryCatch(
  {
    result <- margot_plot(test_att, type = "RD")
    cat("✓ margot_plot worked with ATT column\n")
    print(result$transformed_table)
  },
  error = function(e) {
    cat("✗ Error:", e$message, "\n")
  }
)

# test rename_evalue
cat("\nTesting rename_evalue parameter...\n")
tryCatch(
  {
    result <- margot_plot(test_ate, type = "RD", rename_evalue = TRUE)
    cat("✓ rename_evalue worked\n")
    print(names(result$transformed_table))
  },
  error = function(e) {
    cat("✗ Error:", e$message, "\n")
  }
)
