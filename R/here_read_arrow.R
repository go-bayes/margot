#' @title Read Data Frame from Parquet File in a Specified Directory (Deprecated)
#'
#' @description This function is deprecated and will be removed in future releases.
#' For reading data frames, consider using the `here_read_qs` function.
#'
#' @param name Character string specifying the name of the Parquet file to be read.
#'
#' @examples
#' \dontrun{
#' my_df <- here_read_arrow("my_dataset")
#' }
#'
#' @export
#' @keywords internal
here_read_arrow <- function(name) {
  .Deprecated("here_read_qs",
              message = "here_read_arrow is deprecated and will be removed in a future release. Please use here_read_qs instead.")
  # function
  df <- arrow::read_parquet(here::here(push_mods, paste0(name, ".parquet")))
  return(df)
}
