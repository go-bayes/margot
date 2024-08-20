#' Transform Table Row Names with CLI Feedback
#'
#' This function transforms the row names of a data frame based on specified criteria
#' and provides CLI feedback on the changes made.
#'
#' @param df A data frame whose row names are to be transformed.
#' @param remove_tx_prefix Logical. If TRUE, removes 't' followed by numbers and underscore from the start of row names.
#' @param remove_z_suffix Logical. If TRUE, removes '_z' from the end of row names.
#' @param use_title_case Logical. If TRUE, converts row names to title case.
#' @param remove_underscores Logical. If TRUE, replaces underscores with spaces in row names.
#'
#' @return A data frame with transformed row names.
#'
#' @details
#' The function applies the following transformations in order:
#' 1. Removes 't' followed by numbers and underscore from the start (if remove_tx_prefix is TRUE)
#' 2. Removes '_z' from the end (if remove_z_suffix is TRUE)
#' 3. Replaces underscores with spaces (if remove_underscores is TRUE)
#' 4. Converts to title case (if use_title_case is TRUE)
#'
#' The function provides CLI feedback for each change made and a summary of the transformation process.
#'
#' @import cli
#'
#' @examples
#' df <- data.frame(x = 1:3, row.names = c("t1_variable_z", "t2_another_var", "last_variable"))
#' transformed_df <- transform_table_rownames(df)
#'
#' @export
transform_table_rownames <- function(df,
                                     remove_tx_prefix = TRUE,
                                     remove_z_suffix = TRUE,
                                     use_title_case = TRUE,
                                     remove_underscores = TRUE) {
  cli::cli_h1("Transforming Table Row Names")

  changes <- 0
  # Function to transform a single row name and report changes
  transform_rowname <- function(name) {
    original_name <- name
    if (remove_tx_prefix) {
      name <- sub("^t[0-9]+_", "", name)
    }
    if (remove_z_suffix) {
      name <- sub("_z$", "", name)
    }
    if (remove_underscores) {
      name <- gsub("_", " ", name)
    }
    if (use_title_case) {
      name <- tools::toTitleCase(name)
    }
    if (name != original_name) {
      cli::cli_alert_info("Changed: {.val {original_name}} -> {.val {name}}")
      changes <<- changes + 1
    }
    return(name)
  }

  # Apply the transformation to all row names
  rownames(df) <- sapply(rownames(df), transform_rowname)

  # Print summary
  cli::cli_h2("Transformation Summary")
  cli::cli_alert_info("Total rows processed: {.val {nrow(df)}}")
  cli::cli_alert_info("Total changes made: {.val {changes}}")

  # Print success message
  cli::cli_alert_success("Table row names have been successfully transformed.")

  return(df)
}
