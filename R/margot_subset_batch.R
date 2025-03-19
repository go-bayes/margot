#' Batch Process Subset Models for Causal Forests
#'
#' We compared treatment effects across multiple subsets derived from causal forest models (using the grf package).
#' For each subset, the number of participants, the total sample size, and the corresponding percentage are computed,
#' describing the composition of each targeted subpopulation. Note that exploratory comparisons in causal forest analyses
#' should be interpreted with caution, as subsetting targeted subpopulations may yield unstable treatment effect estimates,
#' typically indicated by large standard errors and wide confidence intervals.
#'
#' @param model_results Results from the causal forest model.
#' @param X Covariate matrix.
#' @param base_defaults A list of default plotting options.
#' @param title A character string for the main title of the plots.
#' @param label_mapping A named vector mapping variable names to labels.
#' @param subsets A named list of subset definitions. Each element should be a list containing:
#'   \describe{
#'     \item{var}{A character string specifying the variable to subset on.}
#'     \item{value}{The value used to define the subset.}
#'     \item{operator}{(Optional) A comparison operator (default is \code{"=="}).}
#'     \item{description}{A character string describing the subset.}
#'   }
#' @param debug Logical; if \code{TRUE}, prints debugging information.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{results}{A list of subset results, each containing the plot and sample statistics.}
#'     \item{summary}{A data frame summarising the sample size, total sample size, and percentage for each subset.}
#'     \item{interpretation}{A character string providing a narrative of the comparisons including the composition of the subsets.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Define subsets
#' subsets <- list(
#'   "males" = list(var = "gender", value = "male", description = "Males only"),
#'   "older" = list(var = "age", value = 30, operator = ">=", description = "Age 30 or above")
#' )
#'
#' # Run the batch processing function
#' results <- margot_subset_batch(model_results, X, base_defaults,
#'                                      "Example Title", label_mapping, subsets)
#' }
#'
#' @export
margot_subset_batch <- function(model_results,
                                      X,
                                      base_defaults = list(),
                                      title = "",
                                      label_mapping = NULL,
                                      subsets,
                                      debug = FALSE) {
  results <- list()

  for (subset_name in names(subsets)) {
    subset_info <- subsets[[subset_name]]

    # Use operator if provided; otherwise default to "=="
    subset_operator <- if (!is.null(subset_info$operator)) subset_info$operator else "=="

    # Run the subset model
    subset_result <- margot_subset_model(
      model_results = model_results,
      X = X,
      subset_var = subset_info$var,
      subset_value = subset_info$value,
      subset_operator = subset_operator,
      subset_description = subset_info$description,
      debug = debug
    )

    # Create plot options for this subset
    options <- margot_plot_create_options(
      subtitle = subset_result$subset_description,
      base_defaults = base_defaults,
      title = title,
      filename_prefix = paste0("subset_", subset_name)
    )

    # Generate the plot for the subset model
    plot_result <- margot_plot(
      subset_result$results,
      options = options,
      label_mapping = label_mapping,
      include_coefficients = FALSE
    )

    # Store results including sample size and percentage.
    results[[subset_name]] <- list(
      subset = subset_result,
      plot = plot_result$plot,
      n = subset_result$subset_info$subset_size,
      total = subset_result$subset_info$total_size,
      pct = 100 * subset_result$subset_info$subset_size / subset_result$subset_info$total_size
    )
  }

  # Create a summary data frame and remove extra row names
  summary_df <- data.frame(
    Subset = names(results),
    Sample_Size = sapply(results, function(x) x$n),
    Total_Size = sapply(results, function(x) x$total),
    Percentage = sapply(results, function(x) round(x$pct, 2)),
    stringsAsFactors = FALSE
  )
  rownames(summary_df) <- NULL

  # Build an interpretation narrative that incorporates results
  subset_lines <- apply(summary_df, 1, function(row) {
    sprintf("In the '%s' subset, %s participants were observed out of %s (%.2f%% of the full sample).",
            row["Subset"], row["Sample_Size"], row["Total_Size"], as.numeric(row["Percentage"]))
  })

  interpretation <- paste(
    "We compared treatment effects across multiple subsets derived from causal forest models (using the grf package).",
    paste(subset_lines, collapse = " "),
    "These comparisons help assess heterogeneity in treatment effects.",
    "Note that exploratory comparisons in causal forest analyses should be interpreted with caution, as subsetting targeted subpopulations may yield unstable treatment effect estimates, typically indicated by wider confidence intervals."
  )

  # Print interpretation to the console
  cat("\nInterpretation:\n", interpretation, "\n")

  # Return a list with results, summary, and interpretation
  return(list(results = results, summary = summary_df, interpretation = interpretation))
}
