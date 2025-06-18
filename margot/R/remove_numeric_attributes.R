#' Remove Attributes from Numeric Columns in a Data Frame
#'
#' Iterates over each column in the provided data frame. If a column is numeric
#' and has attributes, this function removes those attributes by converting the column
#' to a basic numeric vector. This is particularly useful for cleaning data frames
#' after operations that may add undesired attributes to numeric columns, such as
#' aggregations or merges.
#'
#' @param df A `data.frame` object from which attributes of numeric columns will be removed.
#'
#' @return A `data.frame` with attributes removed from all numeric columns.
#'
#' @examples
#' df <- data.frame(a = I(1:3), b = c("x", "y", "z"), c = I(rnorm(3)))
#' cleaned_df <- remove_numeric_attributes(df)
#' str(cleaned_df)
#'
#' @export
remove_numeric_attributes <- function(df) {
  # Validate input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Apply attribute removal to each column
  cleaned_df <- data.frame(lapply(df, function(x) {
    if (is.numeric(x) && length(attributes(x)) > 1) {
      # Convert to vector to remove attributes
      x <- as.vector(x)
    }
    return(x)
  }))

  return(cleaned_df)
}
