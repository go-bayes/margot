#' Format RATE Results into Readable Tables
#'
#' @param models A list containing model results from which to extract RATE information.
#' @param label_mapping An optional named vector for mapping variable names to readable labels.
#' @param remove_tx_prefix Logical indicating whether to remove \sQuote{tx_} prefix from outcome names. Default is TRUE.
#' @param remove_z_suffix Logical indicating whether to remove \sQuote{_z} suffix from outcome names. Default is TRUE.
#' @param use_title_case Logical indicating whether to convert outcome names to title case. Default is TRUE.
#' @param remove_underscores Logical indicating whether to replace underscores with spaces. Default is TRUE.
#' @param round_digits Integer specifying the number of decimal places for rounding. Default is 3.
#' @param highlight_significant Logical indicating whether to add asterisks to outcomes with positive confidence intervals that don\sQuote{t} cross zero. Default is TRUE
#'
#' @return A list containing two tables: rate_autoc (also accessible as rate_result for backward compatibility) and rate_qini.
#'
#' @export
margot_rate <- function(models, label_mapping = NULL,
                        remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
                        use_title_case = TRUE, remove_underscores = TRUE,
                        round_digits = 3, highlight_significant = TRUE) {

  build_table <- function(target_field) {
    rate_list <- lapply(names(models$result), function(mod_name) {
      # extract the specified rate target
      rate_res <- models$result[[mod_name]][[target_field]]

      # exclude the TOC and target fields
      rate_fields <- rate_res[setdiff(names(rate_res), "TOC")]
      rate_fields <- rate_fields[setdiff(names(rate_fields), "target")]

      # round numeric values and extract the first element if needed
      rate_fields <- lapply(rate_fields, function(x) {
        if (is.numeric(x)) x <- round(x, round_digits)
        if (length(x) > 1) x[1] else x
      })

      # transform the model name
      outcome <- transform_var_name(
        mod_name,
        label_mapping,
        remove_tx_prefix = remove_tx_prefix,
        remove_z_suffix = remove_z_suffix,
        use_title_case = use_title_case,
        remove_underscores = remove_underscores
      )

      # build a one-row data frame
      df <- data.frame(outcome = outcome, stringsAsFactors = FALSE)
      for (nm in names(rate_fields)) {
        df[[nm]] <- rate_fields[[nm]]
      }
      df
    })

    result_table <- do.call(rbind, rate_list)
    rownames(result_table) <- NULL

    # rename columns if needed
    if ("estimate" %in% colnames(result_table)) {
      colnames(result_table)[colnames(result_table) == "estimate"] <- "RATE Estimate"
    }
    if ("std.err" %in% colnames(result_table)) {
      colnames(result_table)[colnames(result_table) == "std.err"] <- "Std Error"
    }

    # compute 95% confidence intervals if estimates and standard errors exist
    if ("RATE Estimate" %in% colnames(result_table) &&
        "Std Error" %in% colnames(result_table)) {
      result_table$`2.5%` <- round(result_table$`RATE Estimate` - 1.96 * result_table$`Std Error`, round_digits)
      result_table$`97.5%` <- round(result_table$`RATE Estimate` + 1.96 * result_table$`Std Error`, round_digits)
    }

    # MODIFIED: add ** only to positive significant outcomes where CI doesn't cross zero
    if (highlight_significant && all(c("2.5%", "97.5%") %in% colnames(result_table))) {
      # only highlight when both CI bounds are positive
      significant_positive <- (result_table$`2.5%` > 0 & result_table$`97.5%` > 0)
      result_table[, 1] <- ifelse(significant_positive,
                                  paste0("**", result_table[, 1], "**"),
                                  result_table[, 1])
    }

    # remove the column name for the outcome column
    colnames(result_table)[1] <- ""

    result_table
  }

  # build tables
  autoc_table <- build_table("rate_result")
  qini_table <- build_table("rate_qini")

  # return a list with both tables, providing rate_autoc as an alias for rate_result
  result <- list(
    rate_qini = qini_table
  )

  # add rate_autoc and rate_result (for backward compatibility)
  result$rate_autoc <- autoc_table
  result$rate_result <- result$rate_autoc  # Same object, different name for backward compatibility

  return(result)
}
