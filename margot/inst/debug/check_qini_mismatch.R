# Debug script to check mismatch between qini data and CI estimates

library(margot)

# Function to compare qini_data gains with average_gain estimates
check_qini_consistency <- function(model_results, outcome_var) {
  # Add model_ prefix if needed
  if (!outcome_var %in% names(model_results$results)) {
    outcome_var <- paste0("model_", outcome_var)
  }
  
  # Get qini data and objects
  qini_data <- model_results$results[[outcome_var]]$qini_data
  qini_objects <- model_results$results[[outcome_var]]$qini_objects
  
  if (is.null(qini_data) || is.null(qini_objects)) {
    cat("No qini data or objects found\n")
    return(NULL)
  }
  
  # For each curve, compare
  curves <- unique(qini_data$curve)
  
  for (crv in curves) {
    cat("\n=== Checking curve:", crv, "===\n")
    
    # Get qini object name
    qini_name <- if (crv == "cate") "cate" else if (crv == "ate") "ate" else NULL
    
    if (!is.null(qini_name) && qini_name %in% names(qini_objects)) {
      qini_obj <- qini_objects[[qini_name]]
      
      # Get gains from qini_data at specific proportions
      crv_data <- qini_data[qini_data$curve == crv, ]
      test_props <- c(0.1, 0.2, 0.5, 0.8)
      
      for (p in test_props) {
        # Find closest proportion in qini_data
        idx <- which.min(abs(crv_data$proportion - p))
        data_gain <- crv_data$gain[idx]
        actual_prop <- crv_data$proportion[idx]
        
        # Get estimate from average_gain
        avg_result <- tryCatch({
          maq::average_gain(qini_obj, spend = p)
        }, error = function(e) NULL)
        
        if (!is.null(avg_result)) {
          avg_estimate <- if (is.list(avg_result)) avg_result$estimate else avg_result[1]
          
          cat(sprintf("Proportion %.2f (actual %.3f):\n", p, actual_prop))
          cat(sprintf("  qini_data gain: %.6f\n", data_gain))
          cat(sprintf("  average_gain:   %.6f\n", avg_estimate))
          cat(sprintf("  Difference:     %.6f (%.1f%%)\n", 
                      avg_estimate - data_gain,
                      abs(avg_estimate - data_gain) / abs(data_gain) * 100))
        }
      }
    }
  }
}

# Example usage:
# check_qini_consistency(mod_use, "t2_neuroticism_z_r")