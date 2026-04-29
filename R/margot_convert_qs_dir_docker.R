#' Convert legacy `.qs` files via a Docker R 4.5 container
#'
#' Recursively converts every `.qs` file under `dir_path` to a `.qs2` sibling by
#' running the conversion inside a `rocker/r-ver:4.5` container, where the
#' legacy `qs` package still builds. Designed for users on R 4.6 (or later) for
#' whom `qs` is no longer installable. Requires Docker on `PATH`; no other
#' setup.
#'
#' On the first run Docker pulls the R 4.5 image (~600 MB) and compiles `qs`
#' from CRAN's archive. Subsequent runs reuse the local image and the cached
#' library, so they are fast.
#'
#' Originals are kept by default. Re-run with `delete_qs = TRUE` once you have
#' read-verified some `.qs2` files.
#'
#' @param dir_path Character; directory to scan. Mounted into the container at
#'   `/data`.
#' @param recursive Logical; recurse into subdirectories. Default `TRUE`.
#' @param compress_level Integer; zstd compression level for the new `.qs2`
#'   files. Default `4`.
#' @param delete_qs Logical; if TRUE, delete each `.qs` file after a successful
#'   `.qs2` round-trip read. Default `FALSE`.
#' @param overwrite Logical; if a `.qs2` sibling already exists, overwrite it.
#'   Default `FALSE` (skip).
#' @param image Character; Docker image to use. Default `"rocker/r-ver:4.5"`.
#' @param qs_source_url Character; CRAN archive URL for the `qs` source
#'   tarball. Pinned to `qs_0.27.3.tar.gz` by default. Override only if a
#'   future version supersedes this.
#' @param ppm_snapshot Character; Posit Package Manager snapshot URL pinned
#'   to a date when `qs` 0.27.3 still built against the contemporaneous
#'   `stringfish`. Default `2024-12-01`. Override only if that snapshot is
#'   ever retired.
#' @param quiet Logical; if TRUE, suppress per-file output. Default `FALSE`.
#'
#' @details
#' This function is the user-friendly counterpart to [margot_convert_qs_dir()].
#' Use [margot_convert_qs_dir()] directly if you already have an R session with
#' `qs` installed; use this wrapper if you are on a host where `qs` will not
#' install.
#'
#' For prerequisites (installing Docker / Colima, verifying the daemon,
#' Linux/Windows setup), see the package vignette
#' \code{vignette("migrating-qs-to-qs2", package = "margot")}.
#'
#' Steps:
#' \enumerate{
#'   \item Confirms `docker` is on `PATH`.
#'   \item Writes a small R script to a temp file containing the conversion logic.
#'   \item Invokes `docker run --rm` mounting `dir_path` at `/data` and the
#'     script at `/migrate.R`.
#'   \item Inside the container, installs `qs2` (CRAN) and `qs` (CRAN archive
#'     source), then walks `/data` recursively and converts every `.qs`.
#'   \item Returns when the container exits. The status code is propagated.
#' }
#'
#' @return Invisible integer, the docker process exit status (0 on success).
#'
#' @examples
#' \dontrun{
#'   margot_convert_qs_dir_docker("~/Library/CloudStorage/.../caches")
#'   # spot-check a converted file in your normal R session, then:
#'   margot_convert_qs_dir_docker("~/Library/CloudStorage/.../caches",
#'                                delete_qs = TRUE)
#' }
#'
#' @seealso [margot_convert_qs_dir()] for direct (non-Docker) use.
#' @export
margot_convert_qs_dir_docker <- function(dir_path,
                                         recursive = TRUE,
                                         compress_level = 4,
                                         delete_qs = FALSE,
                                         overwrite = FALSE,
                                         image = "rocker/r-ver:4.5",
                                         qs_source_url = "https://cran.r-project.org/src/contrib/Archive/qs/qs_0.27.3.tar.gz",
                                         ppm_snapshot = "https://packagemanager.posit.co/cran/2024-12-01",
                                         quiet = FALSE) {
  if (!dir.exists(dir_path)) {
    stop(sprintf("Directory not found: %s", dir_path))
  }
  if (Sys.which("docker") == "") {
    stop(
      "Docker not found on PATH. Install Docker Desktop (https://www.docker.com/products/docker-desktop) ",
      "or, if you have an R 4.5 environment available, call margot_convert_qs_dir() directly from inside it."
    )
  }

  abs_dir <- normalizePath(dir_path, mustWork = TRUE)

  # build the in-container R script
  script_lines <- c(
    "options(warn = 1, install.packages.compile.from.source = 'always')",
    sprintf("options(repos = c(CRAN = %s))", shQuote(ppm_snapshot)),
    "# install everything from the same dated snapshot so qs 0.27.3 sees the",
    "# stringfish and BH versions it was originally built against.",
    "if (!requireNamespace('qs2', quietly = TRUE)) install.packages('qs2')",
    "if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes')",
    sprintf(
      "if (!requireNamespace('qs', quietly = TRUE)) remotes::install_url(%s, dependencies = TRUE, upgrade = 'never')",
      shQuote(qs_source_url)
    ),
    sprintf("OVERWRITE      <- %s", as.character(overwrite)),
    sprintf("DELETE_QS      <- %s", as.character(delete_qs)),
    sprintf("RECURSIVE      <- %s", as.character(recursive)),
    sprintf("COMPRESS_LEVEL <- %d", as.integer(compress_level)),
    sprintf("QUIET          <- %s", as.character(quiet)),
    "",
    "files <- list.files('/data', pattern = '\\\\.qs$',",
    "                    recursive = RECURSIVE, full.names = TRUE)",
    "cat(sprintf('found %d .qs file(s) under /data\\n', length(files)))",
    "n_ok <- 0L; n_skip <- 0L; n_fail <- 0L",
    "",
    "for (src in files) {",
    "  dest <- sub('\\\\.qs$', '.qs2', src)",
    "  rel  <- sub('^/data/', '', src)",
    "",
    "  if (file.exists(dest) && !OVERWRITE) {",
    "    if (!QUIET) cat(sprintf('skip   %s (.qs2 exists)\\n', rel))",
    "    n_skip <- n_skip + 1L; next",
    "  }",
    "",
    "  ok <- tryCatch({",
    "    obj <- qs::qread(src)",
    "    qs2::qs_save(obj, dest, compress_level = COMPRESS_LEVEL)",
    "    if (!identical(obj, qs2::qs_read(dest))) {",
    "      stop('verify failed: deserialised object differs from source')",
    "    }",
    "    if (DELETE_QS) file.remove(src)",
    "    TRUE",
    "  }, error = function(e) { message('FAIL  ', rel, ': ', conditionMessage(e)); FALSE })",
    "",
    "  if (isTRUE(ok)) {",
    "    if (!QUIET) cat(sprintf('ok     %s\\n', rel))",
    "    n_ok <- n_ok + 1L",
    "  } else {",
    "    n_fail <- n_fail + 1L",
    "  }",
    "}",
    "",
    "cat(sprintf('\\nconverted: %d, skipped: %d, failed: %d\\n', n_ok, n_skip, n_fail))",
    "if (n_fail > 0L) quit(status = 1)"
  )

  # mount a *directory* containing the script rather than the file itself.
  # Colima/Lima on macOS treats single-file bind mounts as empty directories,
  # and only shares ~/ into the VM by default, so place the staging dir under
  # the user's home rather than /tmp.
  staging_root <- file.path(path.expand("~"), ".cache", "margot", "qs-migrate")
  dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
  script_dir <- tempfile("run_", tmpdir = staging_root)
  dir.create(script_dir)
  on.exit(unlink(script_dir, recursive = TRUE), add = TRUE)
  script_path <- file.path(script_dir, "migrate.R")
  writeLines(script_lines, script_path)

  if (!quiet) {
    cli::cli_alert_info("starting docker container ({image}); first run pulls the image and compiles qs.")
    cli::cli_alert_info("mounted: {abs_dir} -> /data")
  }

  docker_args <- c(
    "run", "--rm",
    "-v", sprintf("%s:/data", abs_dir),
    "-v", sprintf("%s:/migrate", script_dir),
    image,
    "Rscript", "/migrate/migrate.R"
  )

  status <- system2("docker", docker_args)
  if (status != 0L) {
    cli::cli_alert_danger("docker exited with status {status}; check the log above for details.")
  } else if (!quiet) {
    cli::cli_alert_success("conversion finished. spot-check a .qs2 file before re-running with delete_qs = TRUE.")
  }

  invisible(status)
}
