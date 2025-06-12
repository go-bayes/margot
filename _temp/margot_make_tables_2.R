#' Create Summary Tables Using table1 with Custom Formatting
#'
#' \code{margot_make_tables} is a wrapper for \code{table1::table1()} which simplifies the creation of summary tables.
#' It provides custom variable labelling, formatting, factor conversion, and additional table options.
#' The function supports output in four formats: \code{"html"} (default), \code{"latex"}, \code{"docx"} and \code{"markdown"}.
#'
#' When \code{format = "markdown"}, the table is converted to a data frame and then rendered with \code{knitr::kable()} to produce a markdown table.
#'
#' @param data A \code{data.frame} containing the dataset.
#' @param vars A character vector of variable names to include on the left-hand side of the table.
#' @param by A character vector of variable names to stratify the table by. Supports multiple variables for interactions.
#' @param labels A named character vector for custom variable labels. Names should correspond to variable names in \code{vars}.
#' @param factor_vars An optional character vector of variable names in \code{vars} to convert to factors for frequency tables.
#' @param table1_opts A list of additional options to pass to \code{table1::table1()}. For example, \code{list(overall = FALSE, transpose = TRUE)}.
#' @param format A character string specifying the output format. Options are \code{"html"} (default), \code{"latex"}, \code{"docx"} and \code{"markdown"}.
#' @param kable_opts A list of additional options controlling table styling:
#'  \itemize{
#'    \item For \code{format = "latex"}, these are passed to \code{kableExtra::kable_styling()}.
#'    \item For \code{format = "docx"}, they control \code{flextable} styling (e.g. \code{font_size}).
#'    \item For \code{format = "markdown"}, options are available for future extensions.
#'  }
#'
#' @return A table object formatted for the specified output.
#'
#' @examples
#' \dontrun{
#'   # markdown output example
#'   summary_tab_md <- margot_make_tables(
#'     data = mydata,
#'     vars = c("age", "gender", "income"),
#'     by = "group",
#'     labels = c("age" = "Age", "gender" = "Gender", "income" = "Income"),
#'     factor_vars = "gender",
#'     table1_opts = list(overall = FALSE, transpose = TRUE),
#'     format = "markdown",
#'     kable_opts = list(font_size = 10)
#'   )
#' }
#'
#' @importFrom table1 table1 setLabel t1kable
#' @importFrom dplyr mutate across all_of intersect setdiff
#' @importFrom stringr str_to_title
#' @importFrom cli cat_rule
#' @importFrom knitr kable
#' @keywords internal
#' Create Summary Tables Using table1 with Custom Formatting
#'
#' \code{margot_make_tables} is a wrapper for \code{table1::table1()} which simplifies the creation of summary tables.
#' It provides custom variable labelling, formatting, factor conversion, and additional table options.
#' This function is optimized for \code{"markdown"} and \code{"latex"} outputs.
#'
#' @param data A \code{data.frame} containing the dataset.
#' @param vars A character vector of variable names to include on the left-hand side of the table.
#' @param by A character vector of variable names to stratify the table by. Supports multiple variables for interactions.
#' @param labels A named character vector for custom variable labels. Names should correspond to variable names in \code{vars}.
#' @param factor_vars An optional character vector of variable names in \code{vars} to convert to factors for frequency tables.
#' @param table1_opts A list of additional options to pass to \code{table1::table1()}. For example, \code{list(overall = FALSE, transpose = TRUE)}.
#' @param format A character string specifying the output format. Options are \code{"markdown"} (default) or \code{"latex"}.
#' @param kable_opts A list of additional options controlling table styling:
#'  \itemize{
#'    \item For \code{format = "latex"}, these are passed to \code{kableExtra::kable_styling()}.
#'    \item For \code{format = "markdown"}, currently only for documentation purposes.
#'  }
#'
#' @return A table object formatted for the specified output:
#'  \itemize{
#'    \item For \code{format = "latex"}, a kableExtra-formatted LaTeX table
#'    \item For \code{format = "markdown"}, a markdown-formatted kable table with bold variable names
#'  }
#'
#' @examples
#' \dontrun{
#'   # markdown output example
#'   summary_tab_md <- margot_make_tables(
#'     data = mydata,
#'     vars = c("age", "gender", "income"),
#'     by = "group",
#'     labels = c("age" = "Age", "gender" = "Gender", "income" = "Income"),
#'     factor_vars = "gender",
#'     table1_opts = list(overall = FALSE, transpose = TRUE),
#'     format = "markdown"
#'   )
#' }
#'
#' @importFrom table1 table1 setLabel t1kable
#' @importFrom knitr kable
#' @keywords internal
margot_make_tables_2 <- function(data, vars, by, labels = NULL, factor_vars = NULL, table1_opts = list(),
                               format = c("markdown", "latex"), kable_opts = list()) {
  # match and validate format argument
  format <- match.arg(format)

  # validate input data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # check for missing variables
  missing_vars <- vars[!vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("the following variables are missing in the data:",
               paste(missing_vars, collapse = ", ")))
  }

  # data checks for 'by' variables and missingness
  missing_by_vars <- by[!by %in% names(data)]
  if (length(missing_by_vars) > 0) {
    stop(paste("the following 'by' variables are missing in the data:",
               paste(missing_by_vars, collapse = ", ")))
  }

  # check for missing values in stratification variables
  for (var in by) {
    if (any(is.na(data[[var]]))) {
      stop(paste("variable", var, "has missing values. please handle missingness before proceeding."))
    }
  }

  # define a helper function for formatting variable names
  format_var_name <- function(name) {
    # replace underscores with spaces and capitalize first letter of each word
    name <- gsub("_", " ", name)
    name <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", name, perl = TRUE)
    trimws(name)
  }

  # convert specified variables to factors if needed
  if (!is.null(factor_vars)) {
    missing_factor_vars <- factor_vars[!factor_vars %in% vars]
    if (length(missing_factor_vars) > 0) {
      stop(paste("the following 'factor_vars' are not in 'vars':",
                 paste(missing_factor_vars, collapse = ", ")))
    }

    # Convert to factors using base R
    for (var in factor_vars) {
      data[[var]] <- as.factor(data[[var]])
    }
  }

  # apply custom labels if provided
  if (!is.null(labels)) {
    if (is.null(names(labels))) {
      stop("'labels' must be a named character vector with names corresponding to variable names in 'vars'.")
    }

    # Apply labels using table1::setLabel
    for (var_name in names(labels)) {
      if (var_name %in% vars) {
        data[[var_name]] <- table1::setLabel(data[[var_name]], labels[[var_name]])
      }
    }
  }

  # assign formatted labels to variables without custom labels
  vars_without_labels <- setdiff(vars, names(labels))
  for (var_name in vars_without_labels) {
    data[[var_name]] <- table1::setLabel(data[[var_name]], format_var_name(var_name))
  }

  # create formula for table1 (using interactions on the 'by' variables)
  lhs <- paste(vars, collapse = " + ")
  rhs <- paste(by, collapse = " * ")
  form <- stats::as.formula(paste("~", lhs, "|", rhs))

  # prepare table1 arguments
  table1_args <- c(list(x = form, data = data), table1_opts)

  # generate the table1 object with error handling
  table_obj <- tryCatch({
    do.call(table1::table1, table1_args)
  }, error = function(e) {
    stop("error in generating table1: ", e$message)
  })

  # process output based on the chosen format
  if (format == "latex") {
    # ensure kableExtra is available
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("package 'kableExtra' is required for latex formatting but is not installed.")
    }

    # set default options if not provided
    booktabs <- if (!is.null(kable_opts$booktabs)) kable_opts$booktabs else TRUE
    longtable <- if (!is.null(kable_opts$longtable)) kable_opts$longtable else TRUE
    font_size <- if (!is.null(kable_opts$font_size)) kable_opts$font_size else 10
    latex_options <- if (!is.null(kable_opts$latex_options))
      kable_opts$latex_options else
        c("hold_position", "repeat_header", "striped", "longtable")

    # create latex table
    latex_table <- table1::t1kable(table_obj, format = "latex",
                                   booktabs = booktabs,
                                   longtable = longtable)
    result <- kableExtra::kable_styling(latex_table,
                                        font_size = font_size,
                                        latex_options = latex_options)
  } else { # markdown
    # convert to data frame
    table_df <- as.data.frame(table_obj)

    # bold the variable names in the first column

    # step 1: create a logical vector identifying the variable name rows
    # These are rows where:
    # - The text in the first column doesn't start with whitespace
    # - The text doesn't match statistics patterns (Mean, Median, Missing)
    # - The text isn't the N count row (starts with "(N=")
    # - The cell isn't empty
    is_variable <- !grepl("^\\s+", table_df[,1]) &
      !grepl("^(Mean|Median|Missing|\\(N=)", table_df[,1]) &
      table_df[,1] != ""

    # Apply bold formatting to identified variable rows
    table_df[is_variable, 1] <- paste0("**", table_df[is_variable, 1], "**")

    # create markdown table
    result <- knitr::kable(table_df, format = "markdown")
  }

  # provide feedback on successful completion
  message("summary table created successfully!")

  return(result)
}
