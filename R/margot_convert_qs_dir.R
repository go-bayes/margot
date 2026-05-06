#' Convert legacy `.qs` files in a directory to `.qs2`
#'
#' Walks `dir_path`, reads every `.qs` file with the optional legacy `qs`
#' package, and writes a sibling `.qs2` file using `qs2::qs_save()`. Originals
#' are kept by default; pass `delete_qs = TRUE` to remove them once the `.qs2`
#' sibling is written and read-verified.
#'
#' Use this direct converter when you have an R environment where `qs` is
#' available. If local `qs` is not available, use
#' [margot_convert_qs_dir_docker()] as a fallback.
#'
#' @param dir_path Character; directory to scan.
#' @param recursive Logical; recurse into subdirectories. Default `TRUE`.
#' @param compress_level Integer; zstd compression level for the new `.qs2` files. Default `4`.
#' @param delete_qs Logical; if TRUE, delete each `.qs` file after a successful
#'   `.qs2` round-trip read. Default `FALSE`.
#' @param overwrite Logical; if a `.qs2` sibling already exists, overwrite it.
#'   Default `FALSE` (skip).
#' @param quiet Logical; if TRUE, suppress per-file output. Default `FALSE`.
#'
#' @return Invisibly, a tibble of `path`, `status` (`"converted"`,
#'   `"skipped"`, `"failed"`), and `message` for every file processed.
#'
#' @examples
#' \dontrun{
#'   margot_convert_qs_dir("~/Library/CloudStorage/.../lab-09-cache")
#' }
#'
#' @seealso [margot_convert_qs_dir_docker()] for users on R 4.6+ who can run Docker.
#' @export
margot_convert_qs_dir <- function(dir_path,
                                  recursive = TRUE,
                                  compress_level = 4,
                                  delete_qs = FALSE,
                                  overwrite = FALSE,
                                  quiet = FALSE) {
  if (!dir.exists(dir_path)) {
    stop(sprintf("Directory not found: %s", dir_path))
  }

  qs_files <- list.files(
    dir_path,
    pattern = "\\.qs$",
    recursive = recursive,
    full.names = TRUE
  )
  if (length(qs_files) == 0L) {
    if (!quiet) cli::cli_alert_info("No .qs files found in {dir_path}.")
    return(invisible(tibble::tibble(
      path = character(), status = character(), message = character()
    )))
  }

  if (!requireNamespace("qs", quietly = TRUE)) {
    stop(
      "Package 'qs' is required to read legacy .qs files. Install or load margot ",
      "in an R environment where 'qs' is available, then rerun margot_convert_qs_dir(). ",
      "If local 'qs' is not available, use margot_convert_qs_dir_docker()."
    )
  }
  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("Package 'qs2' is required to write .qs2 files. Install with install.packages('qs2').")
  }

  results <- vector("list", length(qs_files))

  for (i in seq_along(qs_files)) {
    src <- qs_files[[i]]
    dest <- sub("\\.qs$", ".qs2", src)

    if (file.exists(dest) && !overwrite) {
      if (!quiet) cli::cli_alert_info("skip (exists): {dest}")
      results[[i]] <- list(path = src, status = "skipped", message = ".qs2 sibling already exists")
      next
    }

    res <- tryCatch({
      obj <- qs::qread(src)
      qs2::qs_save(obj, dest, compress_level = compress_level)

      check <- qs2::qs_read(dest)
      if (!identical(obj, check)) {
        stop("read-verify failed: deserialised object differs from source")
      }

      if (delete_qs) file.remove(src)
      list(path = src, status = "converted",
           message = if (delete_qs) "converted; .qs removed" else "converted")
    }, error = function(e) {
      list(path = src, status = "failed", message = conditionMessage(e))
    })

    if (!quiet) {
      if (res$status == "converted") {
        cli::cli_alert_success("{src} -> {basename(dest)}")
      } else {
        cli::cli_alert_danger("{src}: {res$message}")
      }
    }
    results[[i]] <- res
  }

  out <- tibble::tibble(
    path = vapply(results, `[[`, "", "path"),
    status = vapply(results, `[[`, "", "status"),
    message = vapply(results, `[[`, "", "message")
  )

  if (!quiet) {
    n_ok <- sum(out$status == "converted")
    n_skip <- sum(out$status == "skipped")
    n_fail <- sum(out$status == "failed")
    cli::cli_alert_info("converted: {n_ok}, skipped: {n_skip}, failed: {n_fail}")
  }

  invisible(out)
}
