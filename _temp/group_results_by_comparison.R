#' Group Results by Comparison
#'
#' @description
#' Groups the results of multi-arm causal forest analysis by comparison levels.
#'
#' @param results_list A list of results from margot_multi_arm_causal_forest.
#'
#' @return A list of grouped results by comparison levels.
#'
#' @keywords internal
group_results_by_comparison <- function(results_list) {
  # extract all custom tables
  custom_tables <- lapply(results_list, function(x) x$custom_table)

  # get all unique comparisons (exposure levels)
  all_comparisons <- unique(unlist(lapply(custom_tables, function(table) {
    if(!is.null(table) && nrow(table) > 0) {
      sub(".*? - ", "", rownames(table))
    } else {
      character(0)
    }
  })))

  # initialise a list to store grouped results
  grouped_results <- vector("list", length(all_comparisons))
  names(grouped_results) <- all_comparisons

  # for each comparison, combine results from all outcomes
  for (comparison in all_comparisons) {
    comparison_results <- lapply(names(custom_tables), function(outcome) {
      table <- custom_tables[[outcome]]
      if(!is.null(table) && nrow(table) > 0) {
        row_index <- which(sub(".*? - ", "", rownames(table)) == comparison)
        if (length(row_index) > 0) {
          result <- table[row_index, , drop = FALSE]
          rownames(result) <- sub(" - .*$", "", rownames(result))  # remove comparison from rowname
          result
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    # remove null entries and combine
    comparison_results <- do.call(rbind, comparison_results[!sapply(comparison_results, is.null)])
    if(nrow(comparison_results) > 0) {
      grouped_results[[comparison]] <- comparison_results
    }
  }

  # remove any null entries from grouped_results
  grouped_results <- grouped_results[!sapply(grouped_results, is.null)]

  return(grouped_results)
}
