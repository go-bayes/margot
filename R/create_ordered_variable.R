#' Create Ordered Variable Based on Quantile Breaks or Custom Breaks with Informative Labels
#'
#' This function divides a numeric variable into categories based on either quantile breaks
#' or custom-specified breaks, and creates an ordered factor variable with informative labels.
#' It now includes rich CLI reporting for better user feedback.
#'
#' @param df A data frame containing the variable to be divided into categories.
#' @param var_name The name of the variable within the data frame to divide into categories.
#' @param n_divisions The number of quantile divisions to create. Required if custom_breaks is not provided.
#' @param cutpoint_inclusive A character string specifying whether cutpoints should be included
#'        in the lower or upper category. Must be either "lower" or "upper". Default is "upper".
#' @param ties.method A character string specifying how ties should be handled when calculating quantiles.
#'        Must be one of "first", "last", "random", "ordered", or "average".
#'        If NULL (default), it will be set to "last" if cutpoint_inclusive is "upper",
#'        and "first" if cutpoint_inclusive is "lower".
#' @param custom_breaks A numeric vector of break points to use for categorisation. If provided,
#'        this overrides the quantile-based division specified by n_divisions.
#'
#' @return The input data frame with an additional column representing the ordered factor variable.
#'         The new column name will be the original variable name with "_binary" appended if there
#'         are 2 divisions, or "_cat" otherwise.
#'
#' @importFrom stats quantile
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info cli_text cli_rule
#' @importFrom knitr kable
#' @export
create_ordered_variable <- function(df, var_name, n_divisions = NULL,
                                    cutpoint_inclusive = "upper",
                                    ties.method = NULL,
                                    custom_breaks = NULL) {
  # ensure required packages are loaded
  require(cli)
  require(knitr)

  # print function header
  cli::cli_h1("Creating Ordered Variable")

  # input validation
  if (!var_name %in% names(df)) {
    cli::cli_abort(paste("Error:", var_name, "not found in the dataframe."))
  }
  if (!cutpoint_inclusive %in% c("lower", "upper")) {
    cli::cli_abort("Error: Invalid cutpoint_inclusive. Must be either 'lower' or 'upper'.")
  }

  # set default ties.method based on cutpoint_inclusive if not specified
  if (is.null(ties.method)) {
    ties.method <- if(cutpoint_inclusive == "lower") "first" else "last"
    cli::cli_alert_info(paste("ties.method set to", ties.method, "based on cutpoint_inclusive."))
  }

  if (!ties.method %in% c("first", "last", "random", "ordered", "average")) {
    cli::cli_abort("Error: Invalid ties.method. Must be one of 'first', 'last', 'random', 'ordered', or 'average'.")
  }

  if (ties.method == "average") {
    cli::cli_alert_warning("Using 'average' for ties.method may lead to inconsistent results with cutpoint_inclusive setting.")
  }

  var <- df[[var_name]]
  var_clean <- var[!is.na(var)]
  min_val <- min(var_clean, na.rm = TRUE)
  max_val <- max(var_clean, na.rm = TRUE)

  cli::cli_h2("Variable Summary:")
  cli::cli_text(paste("  Min value:", min_val))
  cli::cli_text(paste("  Max value:", max_val))
  cli::cli_text(paste("  Number of NA values:", sum(is.na(var))))

  if (!is.null(custom_breaks)) {
    cli::cli_h2("Using custom breaks:")
    if (!is.numeric(custom_breaks) || length(custom_breaks) < 2) {
      cli::cli_abort("Error: custom_breaks must be a numeric vector with at least 2 elements.")
    }

    custom_breaks <- sort(unique(custom_breaks))

    if (min(custom_breaks) > min_val) {
      cli::cli_alert_warning("Lowest break is higher than the minimum value. Adding minimum value to breaks.")
      custom_breaks <- c(min_val, custom_breaks)
    }

    if (max(custom_breaks) < max_val) {
      cli::cli_alert_warning("Highest break is lower than the maximum value. Adding maximum value to breaks.")
      custom_breaks <- c(custom_breaks, max_val)
    }

    quantile_breaks <- custom_breaks
    n_divisions <- length(quantile_breaks) - 1
  } else {
    cli::cli_h2("Using quantile breaks:")
    if (is.null(n_divisions)) cli::cli_abort("Error: Please specify the number of divisions or provide custom breaks.")

    n_unique <- length(unique(var_clean))
    if (n_unique < n_divisions) {
      cli::cli_alert_warning(paste("The variable has fewer unique non-NA values than requested divisions.",
                                   "Adjusting number of divisions."))
      n_divisions <- n_unique
    }

    # calculate quantile breaks
    probs <- seq(0, 1, length.out = n_divisions + 1)
    quantile_breaks <- unique(quantile(var_clean, probs = probs, na.rm = TRUE, type = 1, ties.method = ties.method))

    # ensure minimum and maximum values are included
    quantile_breaks[1] <- min_val
    quantile_breaks[length(quantile_breaks)] <- max_val

    # ensure we have the correct number of unique breaks
    if (length(quantile_breaks) < n_divisions + 1) {
      epsilon <- diff(range(var_clean)) * .Machine$double.eps
      quantile_breaks <- unique(c(quantile_breaks, max_val + epsilon))
      quantile_breaks <- quantile_breaks[1:(n_divisions + 1)]  # ensure exactly n_divisions + 1 breaks
    }
  }

  cli::cli_text(paste("  Number of divisions:", n_divisions))
  cli::cli_text("  Breaks:")
  print(knitr::kable(data.frame(Break = quantile_breaks), format = "pipe"))

  # create informative labels based on cut points and cutpoint_inclusive
  labels <- vapply(seq_len(n_divisions), function(x) {
    if (cutpoint_inclusive == "lower") {
      if (x == n_divisions) {
        sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      } else {
        sprintf("[%.1f,%.1f)", quantile_breaks[x], quantile_breaks[x + 1])
      }
    } else {  # cutpoint_inclusive == "upper"
      if (x == 1) {
        sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      } else {
        sprintf("(%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      }
    }
  }, character(1))

  # determine column suffix based on binary condition
  suffix <- if (n_divisions == 2) "_binary" else "_cat"
  new_col_name <- paste0(var_name, suffix)

  # use cut for categorisation, adjusting for cutpoint inclusivity
  if (cutpoint_inclusive == "lower") {
    df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
                              labels = labels, include.lowest = TRUE, right = FALSE)
  } else {  # cutpoint_inclusive == "upper"
    df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
                              labels = labels, include.lowest = TRUE, right = TRUE)
  }

  # print summary of the new variable
  cli::cli_h2("New Variable Summary:")
  cli::cli_text(paste("  New column name:", new_col_name))
  cli::cli_text("  Category distribution:")
  summary_table <- table(df[[new_col_name]], useNA = "ifany")
  print(knitr::kable(data.frame(Category = names(summary_table), Count = as.vector(summary_table)), format = "pipe"))

  cli::cli_alert_success("Ordered variable created successfully \U0001F44D")
  cli::cli_rule()

  return(df)
}

#' #' Create Ordered Variable Based on Quantile Breaks or Custom Breaks with Informative Labels
#' #'
#' #' This function divides a numeric variable into categories based on either quantile breaks
#' #' or custom-specified breaks, and creates an ordered factor variable with informative labels.
#' #' It now includes rich CLI reporting for better user feedback.
#' #'
#' #' @param df A data frame containing the variable to be divided into categories.
#' #' @param var_name The name of the variable within the data frame to divide into categories.
#' #' @param n_divisions The number of quantile divisions to create. Required if custom_breaks is not provided.
#' #' @param cutpoint_inclusive A character string specifying whether cutpoints should be included
#' #'        in the lower or upper category. Must be either "lower" or "upper". Default is "upper".
#' #' @param ties.method A character string specifying how ties should be handled when calculating quantiles.
#' #'        Must be one of "first", "last", "random", "ordered", or "average".
#' #'        If NULL (default), it will be set to "last" if cutpoint_inclusive is "upper",
#' #'        and "first" if cutpoint_inclusive is "lower".
#' #' @param custom_breaks A numeric vector of break points to use for categorisation. If provided,
#' #'        this overrides the quantile-based division specified by n_divisions.
#' #'
#' #' @return The input data frame with an additional column representing the ordered factor variable.
#' #'         The new column name will be the original variable name with "_binary" appended if there
#' #'         are 2 divisions, or "_cat" otherwise.
#' #'
#' #' @importFrom stats quantile
#' #' @importFrom cli rule
#' #' @importFrom knitr kable
#' #' @export
#' create_ordered_variable <- function(df, var_name, n_divisions = NULL,
#'                                     cutpoint_inclusive = "upper",
#'                                     ties.method = NULL,
#'                                     custom_breaks = NULL) {
#'   # Ensure required packages are loaded
#'   require(crayon)
#'   require(cli)
#'   require(knitr)
#'
#'   # Print function header
#'   cat(crayon::blue$bold("\n=== Creating Ordered Variable ===\n"))
#'
#'   # Input validation
#'   if (!var_name %in% names(df)) {
#'     stop(crayon::red$bold(paste("Error:", var_name, "not found in the dataframe.")))
#'   }
#'   if (!cutpoint_inclusive %in% c("lower", "upper")) {
#'     stop(crayon::red$bold("Error: Invalid cutpoint_inclusive. Must be either 'lower' or 'upper'."))
#'   }
#'
#'   # Set default ties.method based on cutpoint_inclusive if not specified
#'   if (is.null(ties.method)) {
#'     ties.method <- if(cutpoint_inclusive == "lower") "first" else "last"
#'     cat(crayon::yellow$bold(paste("Note: ties.method set to", ties.method, "based on cutpoint_inclusive.\n")))
#'   }
#'
#'   if (!ties.method %in% c("first", "last", "random", "ordered", "average")) {
#'     stop(crayon::red$bold("Error: Invalid ties.method. Must be one of 'first', 'last', 'random', 'ordered', or 'average'."))
#'   }
#'
#'   if (ties.method == "average") {
#'     warning(crayon::yellow$bold("Warning: Using 'average' for ties.method may lead to inconsistent results with cutpoint_inclusive setting."))
#'   }
#'
#'   var <- df[[var_name]]
#'   var_clean <- var[!is.na(var)]
#'   min_val <- min(var_clean, na.rm = TRUE)
#'   max_val <- max(var_clean, na.rm = TRUE)
#'
#'   cat(crayon::blue$bold("\nVariable Summary:\n"))
#'   cat(paste("  Min value:", crayon::green(min_val), "\n"))
#'   cat(paste("  Max value:", crayon::green(max_val), "\n"))
#'   cat(paste("  Number of NA values:", crayon::yellow(sum(is.na(var))), "\n"))
#'
#'   if (!is.null(custom_breaks)) {
#'     cat(crayon::blue$bold("\nUsing custom breaks:\n"))
#'     if (!is.numeric(custom_breaks) || length(custom_breaks) < 2) {
#'       stop(crayon::red$bold("Error: custom_breaks must be a numeric vector with at least 2 elements."))
#'     }
#'
#'     custom_breaks <- sort(unique(custom_breaks))
#'
#'     if (min(custom_breaks) > min_val) {
#'       warning(crayon::yellow$bold("Warning: Lowest break is higher than the minimum value. Adding minimum value to breaks."))
#'       custom_breaks <- c(min_val, custom_breaks)
#'     }
#'
#'     if (max(custom_breaks) < max_val) {
#'       warning(crayon::yellow$bold("Warning: Highest break is lower than the maximum value. Adding maximum value to breaks."))
#'       custom_breaks <- c(custom_breaks, max_val)
#'     }
#'
#'     quantile_breaks <- custom_breaks
#'     n_divisions <- length(quantile_breaks) - 1
#'   } else {
#'     cat(crayon::blue$bold("\nUsing quantile breaks:\n"))
#'     if (is.null(n_divisions)) stop(crayon::red$bold("Error: Please specify the number of divisions or provide custom breaks."))
#'
#'     n_unique <- length(unique(var_clean))
#'     if (n_unique < n_divisions) {
#'       warning(crayon::yellow$bold(paste("Warning: The variable has fewer unique non-NA values than requested divisions.",
#'                                         "Adjusting number of divisions.")))
#'       n_divisions <- n_unique
#'     }
#'
#'     # calculate quantile breaks
#'     probs <- seq(0, 1, length.out = n_divisions + 1)
#'     quantile_breaks <- unique(quantile(var_clean, probs = probs, na.rm = TRUE, type = 1, ties.method = ties.method))
#'
#'     # Ensure minimum and maximum values are included
#'     quantile_breaks[1] <- min_val
#'     quantile_breaks[length(quantile_breaks)] <- max_val
#'
#'     # ensure we have the correct number of unique breaks
#'     if (length(quantile_breaks) < n_divisions + 1) {
#'       epsilon <- diff(range(var_clean)) * .Machine$double.eps
#'       quantile_breaks <- unique(c(quantile_breaks, max_val + epsilon))
#'       quantile_breaks <- quantile_breaks[1:(n_divisions + 1)]  # Ensure exactly n_divisions + 1 breaks
#'     }
#'   }
#'
#'   cat(paste("  Number of divisions:", crayon::green(n_divisions), "\n"))
#'   cat("  Breaks:\n")
#'   print(knitr::kable(data.frame(Break = quantile_breaks), format = "pipe"))
#'
#'   # create informative labels based on cut points and cutpoint_inclusive
#'   labels <- vapply(seq_len(n_divisions), function(x) {
#'     if (cutpoint_inclusive == "lower") {
#'       if (x == n_divisions) {
#'         sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
#'       } else {
#'         sprintf("[%.1f,%.1f)", quantile_breaks[x], quantile_breaks[x + 1])
#'       }
#'     } else {  # cutpoint_inclusive == "upper"
#'       if (x == 1) {
#'         sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
#'       } else {
#'         sprintf("(%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
#'       }
#'     }
#'   }, character(1))
#'
#'   # Determine column suffix based on binary condition
#'   suffix <- if (n_divisions == 2) "_binary" else "_cat"
#'   new_col_name <- paste0(var_name, suffix)
#'
#'   # use cut for categorisation, adjusting for cutpoint inclusivity
#'   if (cutpoint_inclusive == "lower") {
#'     df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
#'                               labels = labels, include.lowest = TRUE, right = FALSE)
#'   } else {  # cutpoint_inclusive == "upper"
#'     df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
#'                               labels = labels, include.lowest = TRUE, right = TRUE)
#'   }
#'
#'   # print summary of the new variable
#'   cat(crayon::blue$bold("\nNew Variable Summary:\n"))
#'   cat(paste("  New column name:", crayon::green(new_col_name), "\n"))
#'   cat("  Category distribution:\n")
#'   summary_table <- table(df[[new_col_name]], useNA = "ifany")
#'   print(knitr::kable(data.frame(Category = names(summary_table), Count = as.vector(summary_table)), format = "pipe"))
#'
#'   cat(crayon::green$bold("\nOrdered variable created successfully \U0001F44D \n"))
#'   cat(crayon::blue$bold(cli::rule(line = "=")))
#'
#'   return(df)
#' }
