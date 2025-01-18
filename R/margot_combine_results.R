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
#'
#' @importFrom dplyr mutate across where select bind_rows
#' @importFrom kableExtra kbl group_rows
#' @export
margot_combine_results <- function(
    results,
    options = NULL,
    format = "latex",
    digits = 2,
    ...
) {
  # 1. Ensure 'results' is a named list
  if (is.null(names(results)) || any(names(results) == "")) {
    stop("`results` must be a named list with non-empty names.")
  }

  domain_names <- names(results)

  # 2. Build a list of domain-specific data frames
  df_list <- lapply(domain_names, function(nm) {
    # Extract the domain's table
    df <- results[[nm]]

    # Round numeric columns
    df <- df %>%
      mutate(across(where(is.numeric), ~ round(.x, digits)))

    # Add Variable column from row names
    df <- df %>%
      mutate(Variable = rownames(df)) %>%
      select(Variable, everything())

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

  # 4. Get rows per domain for group_rows
  domain_info <- combined_df %>%
    group_by(.domain_label) %>%
    summarise(rows = n(), .groups = 'drop')

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
