#' Bind and Format Domain-Specific Tables
#'
#' This function binds a named list of domain-specific tables, removes the domain column,
#' highlights rows where \eqn{E_{\text{Val_bound}}} exceeds a specified threshold, and groups
#' rows by domain using \code{pack_rows}. Advanced formatting features work with LaTeX or HTML output.
#'
#' @importFrom dplyr bind_rows select rename
#' @importFrom kableExtra kbl row_spec pack_rows
#' @importFrom rlang .data !!!
#'
#' @section Additional Notes:
#' This function requires the \code{kableExtra} package to format tables.
#' For LaTeX output, the \code{booktabs} package is recommended to be loaded in your LaTeX document.
#'
#' @seealso
#' \code{\link[kableExtra]{kbl}} for base table creation
#' \code{\link[kableExtra]{row_spec}} for row styling
#' \code{\link[kableExtra]{pack_rows}} for grouping rows
#' \code{\link[dplyr]{rename}} for column renaming operations
#'
#' @param tables_list A named list of data frames. Each element should represent a domain-specific table.
#'   The names of the list are used as domain labels.
#' @param e_val_bound_threshold Numeric. Rows with \code{E_Val_bound} exceeding this threshold will be highlighted.
#'   Default is \code{1}.
#' @param highlight_color Character. The background highlight colour for output formats. Default is \code{"yellow"}.
#'   Set to \code{NULL} to disable highlighting color.
#' @param bold Logical. Whether to bold highlighted rows. For LaTeX this uses the \code{bold} parameter; for HTML,
#'   this is achieved with inline CSS. Default is \code{TRUE}.
#' @param output_format Character. The output format passed to \code{kbl()}. Either \code{"latex"} or \code{"html"}.
#'   Default is \code{"latex"}.
#' @param rename_cols Logical. Whether to rename columns using default renaming. Default is \code{TRUE}.
#' @param col_renames Named list. Custom column renamings to apply. Format should be list("new_name" = "old_name").
#'   Default renames "E_Value" to "E-Value" and "E_Val_bound" to "E-Value bound". Supply custom named list to override.
#' @param threshold_col Character. The name of the column used for determining which rows to highlight.
#'   Default is "E_Val_bound". This should be the name of the column *before* any renaming is applied.
#' @param kbl_args A list of additional arguments to pass to \code{kbl()}. For example, you might supply
#'   \code{list(booktabs = TRUE, caption = "Combined Results by Domain")}.
#'
#' @return A formatted table (of class \code{knitr_kable}) with rows grouped by domain and
#'   highlighted where \code{E_Val_bound} exceeds the threshold.
#'
#' @examples
#' \dontrun{
#' tables_list <- list(
#'   Health = binary_results_health$transformed_table,
#'   Psych  = binary_results_psych$transformed_table,
#'   Life   = binary_results_life$transformed_table,
#'   Social = binary_results_social$transformed_table
#' )
#'
#' # For LaTeX output:
#' margot_bind_tables(tables_list,
#'   output_format = "latex",
#'   e_val_bound_threshold = 1,
#'   bold = TRUE,
#'   kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")
#' )
#'
#' # For HTML output:
#' margot_bind_tables(tables_list,
#'   output_format = "html",
#'   e_val_bound_threshold = 1,
#'   highlight_color = "yellow",
#'   bold = TRUE,
#'   kbl_args = list(caption = "Combined Results by Domain")
#' )
#'
#' # With custom column renaming:
#' margot_bind_tables(tables_list,
#'   output_format = "html",
#'   rename_cols = TRUE,
#'   col_renames = list("E-value (custom)" = "E_Value", "E-value bound (custom)" = "E_Val_bound")
#' )
#' }
#'
#' @export
margot_bind_tables <- function(tables_list,
                               e_val_bound_threshold = 1,
                               highlight_color = "yellow",
                               bold = TRUE,
                               output_format = "latex",
                               rename_cols = TRUE,
                               col_renames = list(
                                 "E-Value" = "E_Value",
                                 "E-Value bound" = "E_Val_bound"
                               ),
                               threshold_col = "E_Val_bound",
                               kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")) {
  # input validation
  if (!is.list(tables_list) || is.null(names(tables_list)) ||
      any(names(tables_list) == "") || length(tables_list) == 0) {
    stop("'tables_list' must be a non-empty named list of data frames")
  }

  if (!all(sapply(tables_list, is.data.frame))) {
    stop("All elements in 'tables_list' must be data frames")
  }

  # check that threshold column exists in the data
  if (!all(sapply(tables_list, function(df) threshold_col %in% names(df)))) {
    warning("Threshold column '", threshold_col, "' not found in all tables. Highlighting may not work correctly.")
  }

  # validate output format
  if (!output_format %in% c("latex", "html", "markdown")) {
    warning("Output format '", output_format, "' not fully supported. Output may be unpredictable.")
  }
  # combine tables and preserve domain labels
  combined_table <- dplyr::bind_rows(tables_list, .id = "Domain")

  # save domain information then remove the domain column for display
  domain_vec <- combined_table$Domain
  combined_table2 <- dplyr::select(combined_table, -Domain)

  # rename columns if requested
  # identify the e_val_bound column for highlighting later
  e_val_bound_col <- "E_Val_bound"

  if (rename_cols && !is.null(col_renames) && length(col_renames) > 0) {
    # for dplyr::rename, we need a named list where names are new column names
    # and values are existing column names

    # check which target columns exist in the dataset
    cols_exist <- names(col_renames) %in% names(combined_table2)
    valid_renames <- col_renames[cols_exist]

    if (length(valid_renames) > 0) {
      # apply renaming - for dplyr::rename format is: rename(new_name = old_name)
      combined_table2 <- dplyr::rename(combined_table2, !!!valid_renames)

      # if the threshold column was renamed, track the new name for highlighting
      for (new_name in names(valid_renames)) {
        if (valid_renames[[new_name]] == threshold_col) {
          e_val_bound_col <- new_name
          break
        }
      }
    }
  }

  # identify rows where e_val_bound exceeds the threshold
  highlight_rows <- which(combined_table2[[e_val_bound_col]] > e_val_bound_threshold)

  # create a kable object using additional kbl arguments
  tbl <- do.call(kableExtra::kbl, c(list(combined_table2, format = output_format), kbl_args))

  if (output_format %in% c("latex", "html")) {
    # apply formatting only if highlight_color is not NULL
    if (!is.null(highlight_color) && length(highlight_rows) > 0) {
      if (output_format == "html") {
        # for html output, add inline css if bold is true
        if (bold) {
          tbl <- tbl %>%
            kableExtra::row_spec(highlight_rows,
                                 background = highlight_color,
                                 extra_css = "font-weight: bold;")
        } else {
          tbl <- tbl %>% kableExtra::row_spec(highlight_rows, background = highlight_color)
        }
      } else if (output_format == "latex") {
        # for latex, use row_spec with bold parameter
        tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold)
      }
    } else if (bold && length(highlight_rows) > 0) {
      # if highlight_color is NULL but bold is TRUE, still apply bold
      if (output_format == "html") {
        tbl <- tbl %>% kableExtra::row_spec(highlight_rows, extra_css = "font-weight: bold;")
      } else if (output_format == "latex") {
        tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold)
      }
    }

    # compute starting and ending row indices for each domain group
    domains <- unique(domain_vec)

    # use a more efficient approach with match() to find row positions
    domain_indices <- data.frame(
      domain = domains,
      start_row = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
      end_row = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
    )

    # pack rows for each domain
    for (i in seq_len(nrow(domain_indices))) {
      # add tryCatch to handle potential errors in pack_rows
      tryCatch({
        tbl <- tbl |>
          kableExtra::pack_rows(
            group_label = domain_indices$domain[i],
            start_row = domain_indices$start_row[i],
            end_row = domain_indices$end_row[i]
          )
      }, error = function(e) {
        warning("Error in pack_rows for domain '", domain_indices$domain[i], "': ", e$message)
      })
    }
  }

  return(tbl)
}
