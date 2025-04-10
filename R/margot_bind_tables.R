#' Bind and Format Domain-Specific Tables
#'
#' This function binds a named list of domain-specific tables, removes the domain column,
#' highlights rows where \eqn{E_{\text{Val_bound}}} exceeds a specified threshold, and groups
#' rows by domain. Works with LaTeX, HTML, or Markdown output.
#'
#' @importFrom dplyr bind_rows select rename
#' @importFrom kableExtra kbl row_spec pack_rows
#' @importFrom rlang .data !!!
#'
#' @param tables_list A named list of data frames. Each element should represent a domain-specific table.
#'   The names of the list are used as domain labels.
#' @param e_val_bound_threshold Numeric. Rows with \code{E_Val_bound} exceeding this threshold will be highlighted.
#'   Default is \code{1}.
#' @param highlight_color Character. The background highlight colour for output formats. Default is \code{"yellow"}.
#'   Set to \code{NULL} to disable highlighting color.
#' @param bold Logical. Whether to bold highlighted rows. For LaTeX this uses the \code{bold} parameter; for HTML,
#'   this is achieved with inline CSS. For Markdown, this adds asterisks around the entire row. Default is \code{TRUE}.
#' @param output_format Character. The output format passed to \code{kbl()}. Either \code{"latex"}, \code{"html"},
#'   or \code{"markdown"}. Default is \code{"latex"}.
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
  valid_formats <- c("latex", "html", "markdown")
  if (!output_format %in% valid_formats) {
    warning("Output format '", output_format, "' not fully supported. Output may be unpredictable.")
  }

  # create a combined table with domain markers
  combined_table <- dplyr::bind_rows(tables_list, .id = "Domain")

  # rename columns if requested
  if (rename_cols && !is.null(col_renames) && length(col_renames) > 0) {
    for (new_name in names(col_renames)) {
      old_name <- col_renames[[new_name]]
      if (old_name %in% names(combined_table)) {
        names(combined_table)[names(combined_table) == old_name] <- new_name
      }
    }
  }

  # identify the threshold column (after potential renaming)
  threshold_col_name <- threshold_col
  for (new_name in names(col_renames)) {
    if (col_renames[[new_name]] == threshold_col) {
      threshold_col_name <- new_name
      break
    }
  }

  # Special handling for markdown format
  if (output_format == "markdown") {
    # Create a kable table
    caption <- if (!is.null(kbl_args$caption)) kbl_args$caption else NULL
    align <- if (!is.null(kbl_args$align)) kbl_args$align else NULL

    # For markdown, we need a different approach since we can't use row_spec
    # Instead, we'll modify the data before passing it to kable

    # Identify rows to be bolded based on the threshold
    if (bold && threshold_col_name %in% names(combined_table)) {
      rows_to_bold <- which(as.numeric(combined_table[[threshold_col_name]]) > e_val_bound_threshold)

      if (length(rows_to_bold) > 0) {
        # Convert data to character for consistent handling
        combined_table_md <- combined_table
        for (col in names(combined_table_md)) {
          combined_table_md[[col]] <- as.character(combined_table_md[[col]])
        }

        # Add markdown bold markers to each value in the bold rows
        for (row in rows_to_bold) {
          for (col in names(combined_table_md)) {
            value <- combined_table_md[row, col]
            # Only add bold if the value isn't already bold and isn't empty
            if (!grepl("^\\*\\*.*\\*\\*$", value) && trimws(value) != "") {
              combined_table_md[row, col] <- paste0("**", value, "**")
            }
          }
        }

        # Use the modified data for the markdown table
        markdown_table <- knitr::kable(combined_table_md, format = "markdown",
                                       caption = caption, align = align)
      } else {
        # No rows to bold, just create the table normally
        markdown_table <- knitr::kable(combined_table, format = "markdown",
                                       caption = caption, align = align)
      }
    } else {
      # No bolding requested, create the table normally
      markdown_table <- knitr::kable(combined_table, format = "markdown",
                                     caption = caption, align = align)
    }

    return(markdown_table)
  }

  # For latex and html formats
  # save domain information then remove domain column for display
  domain_vec <- combined_table$Domain
  combined_table2 <- dplyr::select(combined_table, -Domain)

  # identify the threshold column for highlighting
  e_val_bound_col <- threshold_col_name

  # identify rows where threshold exceeds the specified value
  highlight_rows <- which(combined_table2[[e_val_bound_col]] > e_val_bound_threshold)

  # create a kable object using additional kbl arguments
  tbl <- do.call(kableExtra::kbl, c(list(combined_table2, format = output_format), kbl_args))

  # apply formatting for latex and html
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
        tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold, background = highlight_color)
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
        tbl <- tbl %>%
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

# old
# margot_bind_tables <- function(tables_list,
#                                e_val_bound_threshold = 1,
#                                highlight_color = "yellow",
#                                bold = TRUE,
#                                output_format = "latex",
#                                rename_cols = TRUE,
#                                col_renames = list(
#                                  "E-Value" = "E_Value",
#                                  "E-Value bound" = "E_Val_bound"
#                                ),
#                                threshold_col = "E_Val_bound",
#                                kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")) {
#   # input validation
#   if (!is.list(tables_list) || is.null(names(tables_list)) ||
#       any(names(tables_list) == "") || length(tables_list) == 0) {
#     stop("'tables_list' must be a non-empty named list of data frames")
#   }
#
#   if (!all(sapply(tables_list, is.data.frame))) {
#     stop("All elements in 'tables_list' must be data frames")
#   }
#
#   # check that threshold column exists in the data
#   if (!all(sapply(tables_list, function(df) threshold_col %in% names(df)))) {
#     warning("Threshold column '", threshold_col, "' not found in all tables. Highlighting may not work correctly.")
#   }
#
#   # validate output format
#   valid_formats <- c("latex", "html", "markdown")
#   if (!output_format %in% valid_formats) {
#     warning("Output format '", output_format, "' not fully supported. Output may be unpredictable.")
#   }
#
#   # Special handling for markdown format
#   if (output_format == "markdown") {
#     # create a combined table with domain markers
#     combined_table <- dplyr::bind_rows(tables_list, .id = "Domain")
#
#     # rename columns if requested
#     if (rename_cols && !is.null(col_renames) && length(col_renames) > 0) {
#       for (new_name in names(col_renames)) {
#         old_name <- col_renames[[new_name]]
#         if (old_name %in% names(combined_table)) {
#           names(combined_table)[names(combined_table) == old_name] <- new_name
#         }
#       }
#     }
#
#     # For markdown with bold=TRUE, add asterisks around rows where threshold is exceeded
#     if (bold) {
#       # First identify the threshold column (before or after renaming)
#       threshold_col_name <- threshold_col
#       for (new_name in names(col_renames)) {
#         if (col_renames[[new_name]] == threshold_col) {
#           threshold_col_name <- new_name
#           break
#         }
#       }
#
#       # Ensure the threshold column exists
#       if (threshold_col_name %in% names(combined_table)) {
#         # Apply markdown bold formatting to rows exceeding the threshold
#         # Get a character matrix representation of the data
#         char_data <- as.matrix(combined_table)
#         mode(char_data) <- "character"
#
#         # Identify rows to be bolded
#         rows_to_bold <- which(as.numeric(combined_table[[threshold_col_name]]) > e_val_bound_threshold)
#
#         # Add asterisks to each cell in rows that need bolding
#         if (length(rows_to_bold) > 0) {
#           for (row in rows_to_bold) {
#             char_data[row, ] <- paste0("**", char_data[row, ], "**")
#           }
#         }
#
#         # Convert back to a data frame
#         combined_table <- as.data.frame(char_data, stringsAsFactors = FALSE)
#
#         # preserve original column types where possible
#         for (col in names(combined_table)) {
#           # try to convert to numeric if appropriate
#           if (!col %in% c("Domain")) {  # skip domain column
#             combined_table[[col]] <- tryCatch({
#               as.numeric(combined_table[[col]])
#             }, warning = function(w) {
#               combined_table[[col]]  # keep as character if conversion fails
#             })
#           }
#         }
#       }
#     }
#
#     # Create a basic knitr table without any kableExtra enhancements
#     caption <- if (!is.null(kbl_args$caption)) kbl_args$caption else NULL
#     markdown_table <- knitr::kable(combined_table, format = "markdown", caption = caption)
#
#     # Return as a plain string with knitr_kable class
#     return(markdown_table)
#   }
#
#   # Original code for latex and html formats
#   # combine tables and preserve domain labels
#   combined_table <- dplyr::bind_rows(tables_list, .id = "Domain")
#
#   # save domain information then remove the domain column for display
#   domain_vec <- combined_table$Domain
#   combined_table2 <- dplyr::select(combined_table, -Domain)
#
#   # rename columns if requested
#   # identify the e_val_bound column for highlighting later
#   e_val_bound_col <- threshold_col  # Changed from hardcoded "E_Val_bound"
#
#   if (rename_cols && !is.null(col_renames) && length(col_renames) > 0) {
#     for (new_name in names(col_renames)) {
#       old_name <- col_renames[[new_name]]
#       if (old_name %in% names(combined_table2)) {
#         names(combined_table2)[names(combined_table2) == old_name] <- new_name
#
#         # Update the threshold column name if it was renamed
#         if (old_name == threshold_col) {
#           e_val_bound_col <- new_name
#         }
#       }
#     }
#   }
#
#   # identify rows where e_val_bound exceeds the threshold
#   highlight_rows <- which(combined_table2[[e_val_bound_col]] > e_val_bound_threshold)
#
#   # create a kable object using additional kbl arguments
#   tbl <- do.call(kableExtra::kbl, c(list(combined_table2, format = output_format), kbl_args))
#
#   # apply formatting for latex and html
#   if (output_format %in% c("latex", "html")) {
#     # apply formatting only if highlight_color is not NULL
#     if (!is.null(highlight_color) && length(highlight_rows) > 0) {
#       if (output_format == "html") {
#         # for html output, add inline css if bold is true
#         if (bold) {
#           tbl <- tbl %>%
#             kableExtra::row_spec(highlight_rows,
#                                  background = highlight_color,
#                                  extra_css = "font-weight: bold;")
#         } else {
#           tbl <- tbl %>% kableExtra::row_spec(highlight_rows, background = highlight_color)
#         }
#       } else if (output_format == "latex") {
#         # for latex, use row_spec with bold parameter
#         tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold, background = highlight_color)
#       }
#     } else if (bold && length(highlight_rows) > 0) {
#       # if highlight_color is NULL but bold is TRUE, still apply bold
#       if (output_format == "html") {
#         tbl <- tbl %>% kableExtra::row_spec(highlight_rows, extra_css = "font-weight: bold;")
#       } else if (output_format == "latex") {
#         tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold)
#       }
#     }
#
#     # compute starting and ending row indices for each domain group
#     domains <- unique(domain_vec)
#
#     # use a more efficient approach with match() to find row positions
#     domain_indices <- data.frame(
#       domain = domains,
#       start_row = vapply(domains, function(x) min(which(domain_vec == x)), integer(1)),
#       end_row = vapply(domains, function(x) max(which(domain_vec == x)), integer(1))
#     )
#
#     # pack rows for each domain
#     for (i in seq_len(nrow(domain_indices))) {
#       # add tryCatch to handle potential errors in pack_rows
#       tryCatch({
#         tbl <- tbl %>%
#           kableExtra::pack_rows(
#             group_label = domain_indices$domain[i],
#             start_row = domain_indices$start_row[i],
#             end_row = domain_indices$end_row[i]
#           )
#       }, error = function(e) {
#         warning("Error in pack_rows for domain '", domain_indices$domain[i], "': ", e$message)
#       })
#     }
#   }
#
#   return(tbl)
# }

