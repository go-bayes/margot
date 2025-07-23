#' Batch Process Subset Models for Causal Forests
#'
#' We compared treatment effects across multiple subsets derived from causal forest models (using the grf package).
#' For each subset, the number of participants, the total sample size, and the corresponding percentage are computed,
#' describing the composition of each targeted subpopulation. In addition, for each subset the plot, interpretation, and
#' transformed table are generated to summarise the causal effect estimates. Note that exploratory comparisons in causal
#' forest analyses should be interpreted with caution, as subsetting targeted subpopulations may yield unstable treatment
#' effect estimates, typically indicated by large standard errors and wide confidence intervals.
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
#'     \item{subset_condition}{(Optional) A pre-computed logical vector defining the subset.}
#'     \item{description}{(Optional) A character string describing the subset.}
#'     \item{label}{(Optional) A user-friendly label for the subset. If missing, the list name is used.}
#'   }
#' @param debug Logical; if \code{TRUE}, prints debugging information.
#' @param original_df Optional data frame containing the original (non-transformed) data for back-transformation.
#' @param ... Additional arguments to be passed to margot_plot().
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{results}{A list of subset results, each containing the plot, interpretation, and transformed table along with sample statistics.}
#'     \item{summary}{A data frame summarising the sample size, total sample size, and percentage for each subset.}
#'     \item{explanation}{A character string providing a comprehensive explanation of the subsetting process and the results for each model.}
#'   }
#'
#' @export
margot_subset_batch <- function(model_results,
                                X,
                                base_defaults = list(),
                                title = "",
                                label_mapping = NULL,
                                subsets,
                                debug = FALSE,
                                original_df = NULL,
                                ...) {
  results <- list()

  for (subset_name in names(subsets)) {
    subset_info <- subsets[[subset_name]]

    # determine friendly label: label > name
    subset_label <- if (!is.null(subset_info$label)) subset_info$label else subset_name
    # determine friendly description: description > label
    subset_description <- if (!is.null(subset_info$description)) subset_info$description else subset_label

    # subset the data
    if (!is.null(subset_info$subset_condition)) {
      subset_result <- margot_subset_model(
        model_results     = model_results,
        subset_condition  = subset_info$subset_condition,
        subset_description = subset_description,
        debug             = debug
      )
    } else {
      subset_operator <- if (!is.null(subset_info$operator)) subset_info$operator else "=="
      subset_result <- margot_subset_model(
        model_results     = model_results,
        X                 = X,
        subset_var        = subset_info$var,
        subset_value      = subset_info$value,
        subset_operator   = subset_operator,
        subset_description = subset_description,
        debug             = debug
      )
    }

    # create plot options
    options <- margot_plot_create_options(
      subtitle       = subset_description,
      base_defaults  = base_defaults,
      title          = title,
      filename_prefix = paste0("subset_", subset_label)
    )

    # generate plot
    plot_result <- margot_plot(
      subset_result$results,
      options            = options,
      label_mapping      = label_mapping,
      include_coefficients = FALSE,
      original_df        = original_df,
      ...
    )

    # store outputs
    results[[subset_label]] <- list(
      subset            = subset_result,
      plot              = plot_result$plot,
      interpretation    = plot_result$interpretation,
      transformed_table = plot_result$transformed_table,
      n                 = subset_result$subset_info$subset_size,
      total             = subset_result$subset_info$total_size,
      pct               = 100 * subset_result$subset_info$subset_size / subset_result$subset_info$total_size
    )
  }

  # summary table
  summary_df <- data.frame(
    Subset      = names(results),
    Sample_Size = vapply(results, `[[`, numeric(1), "n"),
    Total_Size  = vapply(results, `[[`, numeric(1), "total"),
    Percentage  = vapply(results, function(x) round(x$pct, 2), numeric(1)),
    stringsAsFactors = FALSE
  )
  rownames(summary_df) <- NULL

  # explanation
  subset_lines <- sprintf(
    "In the '%s' subset, %s participants were observed out of %s (%.2f%% of the full sample).",
    summary_df$Subset, summary_df$Sample_Size, summary_df$Total_Size, summary_df$Percentage
  )
  subset_interpretations <- vapply(
    names(results),
    function(lbl) paste0("### ", lbl, "\n", results[[lbl]]$interpretation),
    character(1)
  )
  explanation <- paste(
    "We compared treatment effects across multiple subsets derived from causal forest models (using the grf package).",
    paste(subset_lines, collapse = "
"),
    paste(subset_interpretations, collapse = "

"),
    "These comparisons help assess heterogeneity in treatment effects.",
    "Note that exploratory comparisons in causal forest analyses should be interpreted with caution, as subsetting targeted subpopulations may yield unstable treatment effect estimates, typically indicated by wider confidence intervals.",
    sep = "

"
  )

  cat("\nExplanation:\n", explanation, "\n")

  return(list(results = results, summary = summary_df, explanation = explanation))
}
