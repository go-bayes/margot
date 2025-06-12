#' @title Save Data Frame to Parquet File in a Specified Directory (Deprecated)
#'
#' @description This function is deprecated and will be removed in future releases.
#' For saving data frames, consider using the `here_save_qs` function.
#'
#' @param df Data frame to be saved.
#' @param name Character string specifying the base name of the file.
#'
#' @examples
#' \dontrun{
#' my_df <- data.frame(x = 1:5, y = letters[1:5])
#' here_save_arrow(my_df, "my_saved_dataframe")
#' }
#'
#' @keywords internal
here_save_arrow <- function(df, name) {
  .Deprecated("here_save_qs", package = "margot")
  message("here_save_arrow is deprecated and will be removed in a future release. Please use here_save_qs instead.")
  # function logic
  arrow::write_parquet(df, here::here(name, ".parquet"))
}
