#' Debug Qini Curve Data
#'
#' @param model_results Output from margot_causal_forest
#' @param model_name Name of the model to debug
#' @export
margot_debug_qini <- function(model_results, model_name) {
  if (!grepl("^model_", model_name)) {
    model_name <- paste0("model_", model_name)
  }

  qini_data <- model_results$results[[model_name]]$qini_data
  qini_objects <- model_results$results[[model_name]]$qini_objects

  if (is.null(qini_data)) {
    cat("No qini data found for", model_name, "\n")
    return(invisible(NULL))
  }

  # Check ATE data
  ate_data <- qini_data[qini_data$curve %in% c("ate", "ATE"), ]
  if (nrow(ate_data) > 0) {
    cat("\n=== ATE Curve Analysis ===\n")
    cat("Number of points:", nrow(ate_data), "\n")

    # Check if it's straight
    ate_final <- tail(ate_data$gain, 1)
    expected_gain <- ate_data$proportion * ate_final
    deviation <- abs(ate_data$gain - expected_gain)
    max_deviation <- max(deviation)

    cat("Final ATE value:", ate_final, "\n")
    cat("Max deviation from straight line:", max_deviation, "\n")
    cat("Relative deviation:", round(max_deviation / abs(ate_final) * 100, 2), "%\n")

    # Show first and last few points
    cat("\nFirst 5 points:\n")
    print(head(ate_data[, c("proportion", "gain")], 5))
    cat("\nLast 5 points:\n")
    print(tail(ate_data[, c("proportion", "gain")], 5))

    # Check the raw qini object
    if (!is.null(qini_objects)) {
      ate_obj <- qini_objects$ate %||% qini_objects$baseline
      if (!is.null(ate_obj)) {
        cat("\n=== Raw maq object ===\n")
        cat("Path length:", length(ate_obj[["_path"]]$gain), "\n")
        cat("First 5 raw gains:", paste(round(head(ate_obj[["_path"]]$gain, 5), 4), collapse = ", "), "\n")
        cat("Last 5 raw gains:", paste(round(tail(ate_obj[["_path"]]$gain, 5), 4), collapse = ", "), "\n")
      }
    }

    # Plot
    plot(ate_data$proportion, ate_data$gain,
      type = "l", col = "red", lwd = 2,
      main = paste("ATE Curve for", model_name),
      xlab = "Proportion", ylab = "Gain"
    )
    lines(ate_data$proportion, expected_gain, col = "blue", lty = 2, lwd = 2)
    legend("topleft", c("Actual", "Expected (straight)"),
      col = c("red", "blue"), lty = c(1, 2), lwd = 2
    )

    # Add deviation plot
    plot(ate_data$proportion, deviation,
      type = "l", col = "darkgreen", lwd = 2,
      main = "Deviation from Straight Line",
      xlab = "Proportion", ylab = "Absolute Deviation"
    )
    abline(h = 0, lty = 2)
  }

  invisible(list(
    ate_data = ate_data,
    qini_objects = qini_objects
  ))
}
