#' Create Summary Tables Using table1 with Custom Formatting
#'
#' \code{margot_make_tables} is a wrapper for \code{table1::table1()} which simplifies the creation of summary tables.
#' It provides custom variable labelling, formatting, factor conversion, and additional table options.
#' This function is optimized for \code{"markdown"}, \code{"latex"}, and \code{"flextable"} outputs, with special support for Quarto documents.
#'
#' @param data A \code{data.frame} containing the dataset.
#' @param vars A character vector of variable names to include on the left-hand side of the table.
#' @param by A character vector of variable names to stratify the table by. Supports multiple variables for interactions.
#' @param labels A named character vector for custom variable labels. Names should correspond to variable names in \code{vars}.
#' @param factor_vars An optional character vector of variable names in \code{vars} to convert to factors for frequency tables.
#' @param table1_opts A list of additional options to pass to \code{table1::table1()}. For example, \code{list(overall = FALSE, transpose = TRUE)}.
#' @param format A character string specifying the output format. Options are \code{"markdown"} (default), \code{"latex"}, or \code{"flextable"}.
#' @param kable_opts A list of additional options controlling table styling:
#'  \itemize{
#'    \item For \code{format = "latex"}, these are passed to \code{kableExtra::kable_styling()}.
#'    \item For \code{format = "markdown"}, currently only for documentation purposes.
#'  }
#' @param flex_opts A list of additional options for flextable formatting:
#'  \itemize{
#'    \item \code{font_size}: Font size for the table (default: 9)
#'    \item \code{font_size_header}: Font size for headers (default: 10)
#'    \item \code{theme}: Theme function to apply (default: "theme_vanilla")
#'    \item \code{autofit}: Whether to autofit columns (default: TRUE)
#'    \item \code{width}: Table width (0-1 for proportion of page width, default: 1)
#'  }
#' @param quarto_label An optional label for Quarto cross-references (e.g., "tbl-demographics"). When specified for LaTeX output,
#'   this adds a \code{\\label{}} command to enable Quarto's cross-referencing system.
#'
#' @return A table object formatted for the specified output:
#'  \itemize{
#'    \item For \code{format = "latex"}, a kableExtra-formatted LaTeX table with optional Quarto label
#'    \item For \code{format = "markdown"}, a markdown-formatted kable table with bold variable names
#'    \item For \code{format = "flextable"}, a flextable object optimized for Word output
#'  }
#'
#' @examples
#' \dontrun{
#'   # Flextable output for Word
#'   flex_table <- margot_make_tables(
#'     data = mydata,
#'     vars = c("age", "gender", "income"),
#'     by = "group",
#'     labels = c("age" = "Age", "gender" = "Gender", "income" = "Income"),
#'     factor_vars = "gender",
#'     table1_opts = list(overall = FALSE, transpose = TRUE),
#'     format = "flextable",
#'     flex_opts = list(font_size = 8)
#'   )
#' }
#'
#' @importFrom table1 table1 setLabel t1kable
#' @importFrom knitr kable
#' @importFrom flextable flextable fontsize theme_vanilla theme_box theme_booktabs theme_alafoli autofit set_table_properties align
#' @importFrom stats as.formula
#' @export
margot_make_tables <- function(data, vars, by, labels = NULL, factor_vars = NULL, table1_opts = list(),
                               format = c("markdown", "latex", "flextable"),
                               kable_opts = list(), flex_opts = list(), quarto_label = NULL) {
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

    # Add Quarto cross-reference label if provided
    if (!is.null(quarto_label)) {
      # Insert the label right after the beginning of the longtable environment
      label_pos <- regexpr("\\\\begin\\{longtable\\}\\[t\\]\\{[^}]*\\}", result)
      if (label_pos > 0) {
        label_end <- label_pos + attr(label_pos, "match.length")
        result <- paste0(
          substr(result, 1, label_end),
          "\n\\label{", quarto_label, "}",
          substr(result, label_end + 1, nchar(result))
        )
      } else {
        # Fallback if longtable pattern not found
        result <- paste0("\\label{", quarto_label, "}\n", result)
      }
    }

  } else if (format == "flextable") {
    # ensure flextable is available
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("package 'flextable' is required for flextable formatting but is not installed.")
    }

    # convert to data frame
    table_df <- as.data.frame(table_obj)

    # set default flex options
    font_size <- if (!is.null(flex_opts$font_size)) flex_opts$font_size else 9
    font_size_header <- if (!is.null(flex_opts$font_size_header)) flex_opts$font_size_header else 10
    theme_name <- if (!is.null(flex_opts$theme)) flex_opts$theme else "theme_vanilla"
    autofit <- if (!is.null(flex_opts$autofit)) flex_opts$autofit else TRUE
    width <- if (!is.null(flex_opts$width)) flex_opts$width else 1

    # create flextable
    result <- flextable::flextable(table_df)

    # apply font sizes
    result <- flextable::fontsize(result, size = font_size, part = "all")
    result <- flextable::fontsize(result, size = font_size_header, part = "header")

    # identify variable name rows (rows to bold)
    # these are rows where:
    # - the text in the first column doesn't start with whitespace
    # - the text doesn't match statistics patterns (Mean, Median, Missing)
    # - the text isn't the N count row (starts with "(N=")
    # - the cell isn't empty
    is_variable <- !grepl("^\\s+", table_df[,1]) &
      !grepl("^(Mean|Median|Missing|\\(N=)", table_df[,1]) &
      table_df[,1] != ""

    # additional check for custom labels
    provided_labels <- if (!is.null(labels)) labels else character(0)
    if (length(provided_labels) > 0) {
      row_texts <- trimws(table_df[,1])
      for (i in seq_along(row_texts)) {
        if (row_texts[i] %in% provided_labels) {
          is_variable[i] <- TRUE
        }
      }
    }

    # bold the variable name rows
    if (any(is_variable)) {
      var_rows <- which(is_variable)
      result <- flextable::bold(result, i = var_rows, j = 1, bold = TRUE)
    }

    # apply theme
    theme_func <- switch(theme_name,
                         "theme_vanilla" = flextable::theme_vanilla,
                         "theme_box" = flextable::theme_box,
                         "theme_booktabs" = flextable::theme_booktabs,
                         "theme_alafoli" = flextable::theme_alafoli,
                         flextable::theme_vanilla)  # default

    result <- theme_func(result)

    # autofit columns if requested
    if (autofit) {
      result <- flextable::autofit(result)
    }

    # set table width
    result <- flextable::set_table_properties(result,
                                              layout = "autofit",
                                              width = width)

    # align first column left, others center
    result <- flextable::align(result, j = 1, align = "left", part = "all")
    if (ncol(table_df) > 1) {
      result <- flextable::align(result, j = 2:ncol(table_df), align = "center", part = "all")
    }

  } else { # markdown
    # convert to data frame
    table_df <- as.data.frame(table_obj)

    # store the provided labels for reference
    provided_labels <- if (!is.null(labels)) labels else character(0)

    # bold the variable names in the first column
    # step 1: create a logical vector identifying the variable name rows
    # these are rows where:
    # - the text in the first column doesn't start with whitespace
    # - the text doesn't match statistics patterns (Mean, Median, Missing)
    # - the text isn't the N count row (starts with "(N=")
    # - the cell isn't empty
    # - IMPORTANT: check if the trimmed text matches any of our provided labels
    is_variable <- !grepl("^\\s+", table_df[,1]) &
      !grepl("^(Mean|Median|Missing|\\(N=)", table_df[,1]) &
      table_df[,1] != ""

    # additional check: see if the row text matches any of our custom labels
    # this handles cases where labels contain special characters like hyphens
    if (length(provided_labels) > 0) {
      row_texts <- trimws(table_df[,1])
      for (i in seq_along(row_texts)) {
        if (row_texts[i] %in% provided_labels) {
          is_variable[i] <- TRUE
        }
      }
    }

    # apply bold formatting to identified variable rows
    table_df[is_variable, 1] <- paste0("**", table_df[is_variable, 1], "**")

    # create markdown table
    result <- knitr::kable(table_df, format = "markdown")

    # for markdown, the quarto_label is handled by quarto itself when using code chunks with labels
  }

  # provide feedback on successful completion
  message("summary table created successfully!")

  return(result)
}
