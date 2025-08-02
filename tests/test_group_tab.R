# test the group_tab function directly

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

cat("Testing group_tab with ATE column...\n")
tryCatch(
  {
    result <- margot:::group_tab(test_ate, type = "RD")
    cat("✓ group_tab worked\n")
    print(result)
  },
  error = function(e) {
    cat("✗ Error:", e$message, "\n")
  }
)
