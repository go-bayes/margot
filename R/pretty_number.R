#' Format Numbers with Commas
#'
#' This function takes a numeric vector and formats it by inserting commas as
#' thousands separators, making large numbers easier to read.
#'
#' @param x A numeric vector that you want to format.
#'
#' @return A character vector where each number is formatted with commas
#' as thousands separators.
#'
#' @examples
#' numbers <- c(1000, 50000, 1234567)
#' pretty_number(numbers)
#'
#' @export
pretty_number <- function(x) {
  out <- prettyNum(x, big.mark = ",")
  return(out)
}
