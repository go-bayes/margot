#' Bind and Format Domain-Specific Tables
#'
#' This function binds a named list of domain-specific tables, removes the domain column,
#' highlights rows where \eqn{E_{\text{Val_bound}}} exceeds a specified threshold, and groups
#' rows by domain using \code{pack_rows}. Advanced formatting features work with LaTeX or HTML output.
#'
#' @param tables_list A named list of data frames. Each element should represent a domain-specific table.
#'   The names of the list are used as domain labels.
#' @param e_val_bound_threshold Numeric. Rows with \code{E_Val_bound} exceeding this threshold will be highlighted.
#'   Default is \code{1}.
#' @param highlight_color Character. The background highlight colour for output formats. Default is \code{"yellow"}.
#' @param bold Logical. Whether to bold highlighted rows. For LaTeX this uses the \code{bold} parameter; for HTML,
#'   this is achieved with inline CSS. Default is \code{TRUE}.
#' @param output_format Character. The output format passed to \code{kbl()}. Either \code{"latex"} or \code{"html"}.
#'   Default is \code{"latex"}.
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
#' }
#'
#' @export
margot_bind_tables <- function(tables_list,
                               e_val_bound_threshold = 1,
                               highlight_color = "yellow",
                               bold = TRUE,
                               output_format = "latex",
                               kbl_args = list(booktabs = TRUE, caption = "Combined Results by Domain")) {
  # Combine tables and preserve domain labels
  combined_table <- dplyr::bind_rows(tables_list, .id = "Domain")

  # Save domain information then remove the Domain column for display
  domain_vec <- combined_table$Domain
  combined_table2 <- dplyr::select(combined_table, -Domain)

  # Identify rows where E_Val_bound exceeds the threshold
  highlight_rows <- which(combined_table2$E_Val_bound > e_val_bound_threshold)

  # Create a kable object using additional kbl arguments
  tbl <- do.call(kableExtra::kbl, c(list(combined_table2, format = output_format), kbl_args))

  if (output_format %in% c("latex", "html")) {
    if (output_format == "html") {
      # For HTML output, add inline CSS if bold is TRUE
      if (bold) {
        tbl <- tbl %>%
          kableExtra::row_spec(highlight_rows,
                               background = highlight_color,
                               extra_css = "font-weight: bold;")
      } else {
        tbl <- tbl %>% kableExtra::row_spec(highlight_rows, background = highlight_color)
      }
    } else if (output_format == "latex") {
      tbl <- tbl %>% kableExtra::row_spec(highlight_rows, bold = bold)
    }

    # Compute starting and ending row indices for each domain group
    domains <- unique(domain_vec)
    start_rows <- sapply(domains, function(x) min(which(domain_vec == x)))
    end_rows <- sapply(domains, function(x) max(which(domain_vec == x)))

    # Pack rows for each domain
    for (i in seq_along(domains)) {
      tbl <- tbl %>% kableExtra::pack_rows(group_label = domains[i],
                                           start_row = start_rows[i],
                                           end_row = end_rows[i])
    }
  }

  return(tbl)
}
