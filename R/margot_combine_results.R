#' Combine Multiple Results Tables from margot_plot into a Single Formatted Table
#'
#' @description
#' Takes multiple results tables from `margot_plot` objects and combines them into a single
#' formatted table using kableExtra, with optional group headers for each section.
#'
#' @param results A named list of data frames, typically extracted from `margot_plot` objects
#'        using `$transformed_table`. Names will be used as section headers if no options
#'        are provided.
#' @param options Optional list of options created by `margot_plot_create_options()`. Should
#'        have the same names as the results list. Each option object can include a 'subtitle'
#'        that will be used as the section header.
#' @param format Output format for kable. Default is "latex".
#' @param digits Number of decimal places for rounding numeric values. Default is 2.
#' @param ... Additional arguments passed to kable().
#'
#' @return A kable object that can be further customized using kableExtra functions.
#'
#' @examples
#' \dontrun{
#' # Suppose we have domain-based results:
#' results_list <- list(
#'   Health = list(
#'     transformed_table = health_religious_vs_secular$transformed_table,
#'     interpretation = health_religious_vs_secular$interpretation
#'   ),
#'   Psychological = list(
#'     transformed_table = psych_religious_vs_secular$transformed_table,
#'     interpretation = psych_religious_vs_secular$interpretation
#'   )
#' )
#'
#' # And corresponding options:
#' options_list <- list(
#'   Health = margot_plot_create_options(
#'     subtitle = "Health: Religious vs Secular (baseline)",
#'   ),
#'   Psychological = margot_plot_create_options(
#'     subtitle = "Psychological: Religious vs Secular (baseline)",
#'   )
#' )
#'
#' # Combine the results and print:
#' combined_table <- margot_combine_results(
#'   results = results_list,
#'   options = options_list,
#'   format = "latex",
#'   booktabs = TRUE,
#'   longtable = TRUE,
#'   digits = 2
#' )
#' }
#' @importFrom dplyr mutate across where select bind_rows
#' @importFrom kableExtra kbl group_rows
#' @keywords internal
#' @note The group_rows function exists in both dplyr and kableExtra packages.
#'   This function uses kableExtra::group_rows, which causes a harmless namespace
#'   warning when loading the package.
margot_combine_results <- function(
    results,
    options = NULL,
    format = "latex",
    digits = 2,
    ...) {
  # 1. Ensure 'results' is a named list
  if (is.null(names(results)) || any(names(results) == "")) {
    stop("`results` must be a named list with non-empty names.")
  }

  domain_names <- names(results)
  recorded_layouts <- unlist(lapply(results, function(df) attr(df, "margot_table_layout", exact = TRUE)))

  # 2. Build a list of domain-specific data frames
  df_list <- lapply(domain_names, function(nm) {
    # Extract the domain's table
    df <- results[[nm]]

    # Add Variable column from the Outcome column of the default margot_plot()
    # table, or from row names in the legacy layout
    label_col <- attr(df, "margot_outcome_column", exact = TRUE)
    if (is.null(label_col) || !label_col %in% names(df)) label_col <- "Outcome"
    if (label_col %in% names(df)) {
      df <- df %>%
        dplyr::rename(Variable = dplyr::all_of(label_col)) %>%
        select(Variable, everything())
    } else {
      df <- df %>%
        mutate(Variable = rownames(df)) %>%
        select(Variable, everything())
    }
    df
  })

  # domains that differ only in their interval level share level-free headers
  # and a per-row "CI level" column, so that their limits bind into one column
  df_list <- margot_align_ci_levels(df_list)
  # tables whose columns still differ would bind into split, partly empty
  # columns, so refuse them as margot_bind_tables() does
  margot_check_shared_columns(df_list, recorded_layouts)

  df_list <- lapply(seq_along(domain_names), function(i) {
    nm <- domain_names[[i]]
    df <- df_list[[i]]

    # Round numeric columns, except interval coverage, which rounding would
    # misstate (0.9958 would read 1)
    df <- df %>%
      mutate(across(where(is.numeric) & !dplyr::any_of(c("confidence_level", "CI level")), ~ round(.x, digits)))

    # Add .domain_label for grouping
    df$.domain_label <- if (!is.null(options) && !is.null(options[[nm]]$subtitle)) {
      options[[nm]]$subtitle
    } else {
      nm
    }

    return(df)
  })

  # 3. Combine all data frames
  combined_df <- bind_rows(df_list)

  # 4. Get rows per domain for group_rows, in row order: grouping by label
  # would sort the labels alphabetically and attach them to the wrong rows
  domain_runs <- rle(as.character(combined_df$.domain_label))
  domain_info <- data.frame(
    .domain_label = domain_runs$values,
    rows = domain_runs$lengths,
    stringsAsFactors = FALSE
  )

  # 5. Create kable without .domain_label
  kbl_df <- combined_df %>%
    select(-.domain_label)

  # 6. Create kable and add group_rows
  kbl_out <- kbl(kbl_df, format = format, row.names = FALSE, ...)

  # 7. Add groups with subtitles
  start_row <- 1
  for (i in seq_len(nrow(domain_info))) {
    label_i <- domain_info$.domain_label[i]
    row_count <- domain_info$rows[i]
    end_row <- start_row + row_count - 1

    kbl_out <- kbl_out %>%
      kableExtra::group_rows(label_i, start_row, end_row)

    start_row <- end_row + 1
  }

  return(kbl_out)
}
