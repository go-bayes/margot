#' Process Binary Variables in a Data Frame
#'
#' This function identifies binary variables (both factors and numeric),
#' converts them to 0/1 format, and renames them. It allows for exceptions
#' to be specified.
#'
#' @param data A data frame to process.
#' @param exceptions A character vector of column names to exclude from processing.
#' @param suffix A string to append to renamed binary variables. Default is "_binary".
#'
#' @return A data frame with processed binary variables.
#'
#' @examples
#' df <- data.frame(
#'   a = factor(c("yes", "no", "yes")),
#'   b = c(1, 0, 1),
#'   c = c("apple", "banana", "apple"),
#'   d = factor(c("true", "false", "true"))
#' )
#' processed_df <- process_binary_vars(df, exceptions = "c")
#'
#' @export
margot_process_binary_vars <- function(data, exceptions = character(0), suffix = "_binary") {
  # input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!is.character(exceptions)) {
    stop("Exceptions must be a character vector.")
  }

  # helper functions
  is_binary <- function(x) {
    unique_vals <- unique(x[!is.na(x)])
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  }

  is_binary_factor <- function(x) {
    is.factor(x) && length(levels(x)) == 2
  }

  # convert binary factors to 0/1
  data_converted <- data %>%
    mutate(across(where(is_binary_factor) & !any_of(exceptions),
                  ~ as.numeric(. == levels(.)[2])))

  # identify binary variables (now all numeric)
  binary_vars <- data_converted %>%
    select(where(is_binary) & !any_of(exceptions)) %>%
    names()

  # rename binary variables
  data_renamed <- data_converted %>%
    rename_with(~ paste0(., suffix), all_of(binary_vars))

  return(data_renamed)
}
