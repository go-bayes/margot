#' Save Object to a Deprecated qs2 File
#'
#' Saves the provided object as a `.qs2` file using the specified `name`, within a
#' directory defined by `dir_path`. This helper is deprecated because its name
#' and file format belong to the old qs workflow. Prefer [here_save_arrow()] for
#' new checkpoints.
#'
#' @param obj Object to be saved.
#' @param name Character string specifying the base name of the file (no extension).
#' @param dir_path Character string specifying the directory path.
#' @param compress_level Integer between 1 and 22 controlling zstd compression.
#'   Defaults to `4` (the `qs2` default). Higher values compress more but more slowly.
#' @param nthreads Integer; number of threads for compression. Default `1`.
#' @param preset Deprecated. Was used by the old `qs` backend; now ignored.
#' @param quiet Logical. If TRUE, suppresses console output. Default FALSE.
#'
#' @details
#' `dir_path` must point to an existing directory. The function does not create
#' directories.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#'   my_df <- data.frame(x = 1:5, y = letters[1:5])
#'   here_save_qs(my_df, "my_saved_dataframe", "~/mydata")
#' }
#'
#' @export
#' @importFrom here here
here_save_qs <- function(obj, name, dir_path,
                         compress_level = 4, nthreads = 1,
                         preset = lifecycle::deprecated(),
                         quiet = FALSE) {
  lifecycle::deprecate_warn(
    when = "1.0.320",
    what = "here_save_qs()",
    with = "here_save_arrow()"
  )

  if (lifecycle::is_present(preset)) {
    lifecycle::deprecate_soft(
      when = "1.0.318",
      what = "here_save_qs(preset)",
      with = "here_save_qs(compress_level)"
    )
  }

  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("Package 'qs2' is required. Install with install.packages('qs2').")
  }

  file_path <- here::here(dir_path, paste0(name, ".qs2"))
  qs2::qs_save(obj, file_path, compress_level = compress_level, nthreads = nthreads)

  if (!quiet) {
    file_size <- margot_size(obj)
    cat(sprintf("Object saved to: %s\n", file_path))
    cat(sprintf("Object size: %s\n", file_size))
    cli::cli_alert_success("Save operation completed successfully")
  }

  invisible(NULL)
}
