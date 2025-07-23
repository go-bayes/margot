# manual test of the updated margot_recompute_ate function
# to be run after loading a real causal forest output

# assuming you have models_binary loaded from your analysis
# this script tests the new parameter names and column headings

cat("Testing margot_recompute_ate with updated parameters\n")
cat("====================================================\n\n")

# test 1: check that target_sample parameter works
cat("Test 1: Using target_sample = 'overlap'\n")
test_overlap <- margot_recompute_ate(
  models_binary,
  target_sample = "overlap"  # new parameter name
)
cat("Success! Function accepts target_sample parameter.\n\n")

# test 2: check column names in combined table
cat("Test 2: Checking column names in overlap results\n")
cat("Column names:", paste(names(test_overlap$combined_table), collapse = ", "), "\n")
if ("ATO" %in% names(test_overlap$combined_table)) {
  cat("Success! Table contains 'ATO' column.\n\n")
} else {
  cat("ERROR: Table does not contain 'ATO' column.\n\n")
}

# test 3: test batch function
cat("Test 3: Testing batch function\n")
batch_results <- margot_recompute_ate_batch(
  models_binary,
  target_samples = c("all", "treated", "control", "overlap")
)

cat("\nComparison table column names:\n")
print(names(batch_results$comparison_table))

# test 4: show first few rows of comparison table
cat("\nFirst 3 rows of comparison table:\n")
print(head(batch_results$comparison_table, 3))

cat("\n\nAll tests completed!\n")