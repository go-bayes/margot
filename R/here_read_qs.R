#' Read Object from a Deprecated qs2 or Legacy qs File
#'
#' Reads a serialised R object stored under `name` in `dir_path`. The function
#' looks for `<name>.qs2` first. If no `.qs2` file exists, it falls back to a
#' legacy `<name>.qs` file when the optional `qs` package is installed. This
#' helper is deprecated for new storage, but remains useful while converting old
#' project archives.
#'
#' @param name Character string; base file name (no extension).
#' @param dir_path Character string; directory to read from. If NULL (default), uses `push_mods`.
#' @param nthreads Integer; threads for decompression. Default `1`.
#' @param quiet Logical. If TRUE, suppresses console output. Default FALSE.
#'
#' @return The object stored in the file.
#'
#' @examples
#' \dontrun{
#'   my_df <- here_read_qs("my_dataset")
#'   my_df <- here_read_qs("my_dataset", dir_path = "~/custom_dir")
#' }
#'
#' @export
#' @importFrom here here
here_read_qs <- function(name, dir_path = NULL, nthreads = 1, quiet = FALSE) {
  lifecycle::deprecate_warn(
    when = "1.0.320",
    what = "here_read_qs()",
    with = "here_read_arrow()"
  )

  read_dir <- if (is.null(dir_path)) push_mods else dir_path

  qs2_path <- here::here(read_dir, paste0(name, ".qs2"))
  qs_path  <- here::here(read_dir, paste0(name, ".qs"))

  if (file.exists(qs2_path)) {
    if (!requireNamespace("qs2", quietly = TRUE)) {
      stop("Package 'qs2' is required to read .qs2 files. Install with install.packages('qs2').")
    }
    obj <- qs2::qs_read(qs2_path, nthreads = nthreads)
    file_path <- qs2_path
  } else if (file.exists(qs_path)) {
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop(
        "Found legacy file '", qs_path, "' but the optional 'qs' package is not installed. ",
        "Install or load margot in an R environment where 'qs' is available, then run ",
        "margot_convert_qs_dir('", read_dir, "') to migrate the directory to .qs2. ",
        "If local 'qs' is not available, use margot_convert_qs_dir_docker('", read_dir, "')."
      )
    }
    obj <- qs::qread(qs_path, nthreads = nthreads)
    file_path <- qs_path
    if (!quiet) {
      cli::cli_alert_warning(
        "Read legacy .qs file. Re-save as .qs2 with margot_convert_qs_dir() to remove the dependency on 'qs'."
      )
    }
  } else {
    stop(sprintf("File not found: %s (tried .qs2 and .qs)", qs2_path))
  }

  if (!quiet) {
    file_size <- margot_size(obj)
    cat(sprintf("Object read from: %s\n", file_path))
    cat(sprintf("Object size: %s\n", file_size))
    cli::cli_alert_success("Read operation completed successfully")
  }

  return(obj)
}
