#' Log-transform Variables in a Data Frame
#'
#' This function applies a log(x + 1) transformation to specified variables in a data frame.
#' It handles NA values, warns and drops non-numeric selections, allows for exceptions,
#' and can be applied to variables with specific prefixes.
#'
#' @param data a data frame to process.
#' @param vars a character vector of variable names or a tidyselect helper (e.g., starts_with("hours_")).
#' @param exceptions a character vector of variable names to exclude from transformation.
#' @param prefix a string to prepend to the names of transformed variables. default is "log_."
#' @param keep_original logical; if true, keeps both original and transformed variables. if false, replaces original. default is true.
#'
#' @return a data frame with log-transformed variables.
#' @importFrom dplyr mutate across select if_else all_of where
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_text cli_rule
#' @importFrom knitr kable
#' @export
margot_log_transform_vars <- function(data,
                                      vars,
                                      exceptions = character(0),
                                      prefix = "log_",
                                      keep_original = TRUE) {
  # input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("input must be a data frame.")
  }
  if (!is.character(exceptions)) {
    cli::cli_abort("exceptions must be a character vector.")
  }

  cli::cli_h1("log-transforming variables")
  cli::cli_h2("initial data summary:")
  cli::cli_text(paste("  total variables:", ncol(data)))
  cli::cli_text(paste("  total observations:", nrow(data)))
  cli::cli_text(paste("  exceptions specified:", length(exceptions)))

  # identify vars to transform
  all_sel <- dplyr::select(data, {{ vars }})
  vars_to_transform <- setdiff(names(all_sel), exceptions)

  # drop non-numeric and warn
  numeric_vars <- intersect(
    vars_to_transform,
    names(dplyr::select(data, where(is.numeric)))
  )
  non_numeric <- setdiff(vars_to_transform, numeric_vars)
  if (length(non_numeric) > 0) {
    cli::cli_alert_warning(
      "dropping non-numeric variables: {paste(non_numeric, collapse = ', ')}"
    )
  }

  cli::cli_h2("variables to be transformed:")
  cli::cli_text(paste("  total numeric vars to transform:", length(numeric_vars)))
  if (length(numeric_vars) > 0) {
    print(knitr::kable(
      data.frame(variable = numeric_vars),
      format = "pipe"
    ))
  } else {
    cli::cli_alert_warning("no numeric variables to transform.")
  }

  # apply log transformation
  data_transformed <- dplyr::mutate(
    data,
    dplyr::across(
      .cols = dplyr::all_of(numeric_vars),
      .fns = ~ dplyr::if_else(!is.na(.), log(. + 1), NA_real_),
      .names = "{prefix}{.col}"
    )
  )

  cli::cli_h2("transformed variables:")
  if (length(numeric_vars) > 0) {
    transform_df <- data.frame(
      original    = numeric_vars,
      transformed = paste0(prefix, numeric_vars)
    )
    print(knitr::kable(transform_df, format = "pipe"))
  } else {
    cli::cli_alert_warning("no variables were transformed.")
  }

  # optionally drop originals
  if (!keep_original) {
    data_transformed <- dplyr::select(
      data_transformed,
      -dplyr::all_of(numeric_vars)
    )
    cli::cli_alert_warning(
      "original variables removed as keep_original = FALSE"
    )
  } else {
    cli::cli_alert_success(
      "original variables kept as keep_original = TRUE"
    )
  }

  cli::cli_h2("final data summary:")
  cli::cli_text(paste("  total variables:", ncol(data_transformed)))
  cli::cli_text(paste("  variables transformed:", length(numeric_vars)))
  cli::cli_text(paste("  variables excluded:", length(exceptions) + length(non_numeric)))

  cli::cli_alert_success("log transformation completed successfully! 👍")
  cli::cli_rule()

  data_transformed
}


# margot_log_transform_vars <- function(data, vars, exceptions = character(0), prefix = "log_", keep_original = TRUE) {
#   # input validation
#   if (!is.data.frame(data)) {
#     cli::cli_abort("Input must be a data frame.")
#   }
#   if (!is.character(exceptions)) {
#     cli::cli_abort("Exceptions must be a character vector.")
#   }
#
#   # print function header
#   cli::cli_h1("Log-transforming Variables")
#
#   # report initial data summary
#   cli::cli_h2("Initial Data Summary:")
#   cli::cli_text(paste("  Total variables:", ncol(data)))
#   cli::cli_text(paste("  Total observations:", nrow(data)))
#   cli::cli_text(paste("  Exceptions specified:", length(exceptions)))
#
#   # identify vars to transform
#   vars_to_transform <- dplyr::select(data, {{ vars }}) %>%
#     names()
#
#   # remove exceptions from vars_to_transform
#   vars_to_transform <- setdiff(vars_to_transform, exceptions)
#
#   # report on variables to be transformed
#   cli::cli_h2("Variables to be Transformed:")
#   cli::cli_text(paste("  Total variables to transform:", length(vars_to_transform)))
#   if (length(vars_to_transform) > 0) {
#     cli::cli_text("  List of variables to transform:")
#     print(knitr::kable(data.frame(Variable = vars_to_transform), format = "pipe"))
#   } else {
#     cli::cli_alert_warning("  No variables to transform.")
#   }
#
#   # apply log transformation
#   data_transformed <- dplyr::mutate(data, dplyr::across(
#     .cols = dplyr::all_of(vars_to_transform),
#     .fns = ~ dplyr::if_else(!is.na(.), log(. + 1), NA_real_),
#     .names = "{prefix}{.col}"
#   ))
#
#   # report on transformed variables
#   cli::cli_h2("Transformed Variables:")
#   if (length(vars_to_transform) > 0) {
#     transform_df <- data.frame(
#       Original = vars_to_transform,
#       Transformed = paste0(prefix, vars_to_transform)
#     )
#     print(knitr::kable(transform_df, format = "pipe"))
#   } else {
#     cli::cli_alert_warning("  No variables were transformed.")
#   }
#
#   # remove original variables if keep_original is FALSE
#   if (!keep_original) {
#     data_transformed <- dplyr::select(data_transformed, -dplyr::all_of(vars_to_transform))
#     cli::cli_alert_warning("Original variables removed as keep_original = FALSE")
#   } else {
#     cli::cli_alert_success("Original variables kept as keep_original = TRUE")
#   }
#
#   # final summary
#   cli::cli_h2("Final Data Summary:")
#   cli::cli_text(paste("  Total variables:", ncol(data_transformed)))
#   cli::cli_text(paste("  Variables transformed:", length(vars_to_transform)))
#   cli::cli_text(paste("  Variables excluded:", length(exceptions)))
#
#   cli::cli_alert_success("Log transformation completed successfully! \U0001F44D")
#   cli::cli_rule()
#
#   return(data_transformed)
# }
