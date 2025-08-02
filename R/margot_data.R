#' Available Margot Datasets
#'
#' Internal tibble containing information about available margot datasets
#' @keywords internal
#' @noRd
available_margot_data <- function() {
  tibble::tibble(
    version = c("v1", "v2"),
    date = c("2025-05-10", "2025-12-01"),
    source = c("local", "https://osf.io/abcd1/"),
    description = c(
      "Original simulated NZAVS-style data (27MB)",
      "Updated simulated data with additional waves"
    ),
    size_mb = c(27.2, 35.0),
    waves = c(3, 5),
    n_participants = c(1000, 1500)
  )
}

#' Fetch Margot Example Data
#'
#' Downloads or loads margot example datasets. By default, returns a small
#' simulated dataset suitable for examples. For the full dataset used in
#' publications and teaching, specify version = "full".
#'
#' @param version Character string specifying which dataset version to fetch.
#'   Options are:
#'   - "example" (default): Small simulated dataset (~1MB) included in package
#'   - "v1": Original full simulated NZAVS-style data (~27MB)
#'   - "v2": Updated simulated data with additional waves (~35MB)
#'   - "latest": Most recent version available
#' @param cache Logical. If TRUE (default), caches downloaded data locally
#'   to avoid repeated downloads.
#' @param quiet Logical. If FALSE (default), shows download progress.
#'
#' @return A data frame containing the requested margot example data
#'
#' @details
#' The example dataset is a small simulated dataset included with the package,
#' suitable for running examples and tests. The full datasets (v1, v2) are
#' larger simulated datasets that mirror the structure of the New Zealand
#' Attitudes and Values Study (NZAVS) but contain no real participant data.
#'
#' Full datasets are hosted on the Open Science Framework (OSF) and are
#' downloaded on first use. Subsequent calls use the cached version unless
#' cache = FALSE.
#'
#' @examples
#' # load small example dataset (included in package)
#' df_example <- fetch_margot_data()
#'
#' \dontrun{
#' # fetch full simulated dataset from OSF
#' df_full <- fetch_margot_data(version = "v1")
#'
#' # fetch latest version
#' df_latest <- fetch_margot_data(version = "latest")
#'
#' # force re-download
#' df_fresh <- fetch_margot_data(version = "v1", cache = FALSE)
#' }
#'
#' @export
fetch_margot_data <- function(version = "example", cache = TRUE, quiet = FALSE) {
  # if requesting example data, return built-in dataset
  if (version == "example") {
    if (!quiet) cli::cli_alert_info("Loading built-in example dataset")
    return(df_margot_example) # we'll create this smaller dataset
  }

  # for full datasets, check if margotData package is available
  if (requireNamespace("margotData", quietly = TRUE)) {
    if (!quiet) cli::cli_alert_info("Loading data from margotData package")
    return(margotData::get_margot_data(version))
  }

  # otherwise, fetch from OSF
  available <- available_margot_data()

  # handle "latest" version
  if (version == "latest") {
    version <- available$version[nrow(available)]
  }

  # validate version
  if (!version %in% available$version) {
    cli::cli_abort(c(
      "Unknown version: {.val {version}}",
      "i" = "Available versions: {.val {available$version}}"
    ))
  }

  # get dataset info
  dataset_info <- available[available$version == version, ]

  # skip local source (that was the old df_nz approach)
  if (dataset_info$source == "local") {
    cli::cli_abort(c(
      "Version {.val {version}} used embedded data which is being phased out.",
      "i" = "Use {.code fetch_margot_data('example')} for a small example dataset",
      "i" = "Use {.code fetch_margot_data('v2')} for the full simulated dataset"
    ))
  }

  # set up cache directory
  cache_dir <- tools::R_user_dir("margot", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # cache file path
  cache_file <- file.path(cache_dir, paste0("margot_data_", version, ".rds"))

  # check cache
  if (cache && file.exists(cache_file)) {
    if (!quiet) {
      cli::cli_alert_info("Loading cached data from {.path {cache_file}}")
    }
    return(readRDS(cache_file))
  }

  # download data
  if (!quiet) {
    cli::cli_alert_info(
      "Downloading {.field {dataset_info$description}} ({dataset_info$size_mb} MB)"
    )
  }

  # check for internet connection
  if (!curl::has_internet()) {
    cli::cli_abort(c(
      "No internet connection detected.",
      "i" = "Install the margotData package for offline access:",
      "i" = "{.code remotes::install_github('go-bayes/margotData')}"
    ))
  }

  # download from OSF
  temp_file <- tempfile(fileext = ".rds")

  tryCatch(
    {
      utils::download.file(
        url = dataset_info$source,
        destfile = temp_file,
        mode = "wb",
        quiet = quiet
      )

      # load data
      data <- readRDS(temp_file)

      # save to cache
      if (cache) {
        saveRDS(data, cache_file)
        if (!quiet) {
          cli::cli_alert_success("Data cached at {.path {cache_file}}")
        }
      }

      # clean up
      unlink(temp_file)

      return(data)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download data from {.url {dataset_info$source}}",
        "x" = conditionMessage(e),
        "i" = "Try installing the margotData package instead:",
        "i" = "{.code remotes::install_github('go-bayes/margotData')}"
      ))
    }
  )
}

#' List Available Margot Datasets
#'
#' Shows information about all available margot example datasets
#'
#' @param verbose Logical. If TRUE (default), prints a formatted table.
#'   If FALSE, returns the data silently.
#'
#' @return A tibble with information about available datasets (invisibly if verbose = TRUE)
#'
#' @examples
#' # show available datasets
#' list_margot_data()
#'
#' # get as tibble
#' datasets <- list_margot_data(verbose = FALSE)
#'
#' @export
list_margot_data <- function(verbose = TRUE) {
  data_info <- available_margot_data()

  if (verbose) {
    cli::cli_h2("Available Margot Datasets")
    cli::cli_text("")

    # built-in example
    cli::cli_alert_info(
      "{.field example}: Small example dataset (built-in, ~1MB)"
    )

    # other versions
    for (i in seq_len(nrow(data_info))) {
      cli::cli_alert_info(
        "{.field {data_info$version[i]}}: {data_info$description[i]} ({data_info$size_mb[i]} MB)"
      )
    }

    cli::cli_text("")
    cli::cli_text("Use {.code fetch_margot_data(version = '...')} to load a dataset")

    invisible(data_info)
  } else {
    data_info
  }
}

#' Clear Margot Data Cache
#'
#' Removes cached margot datasets to free disk space or force re-downloading
#'
#' @param version Character string specifying which version to clear,
#'   or "all" to clear entire cache
#' @param confirm Logical. If TRUE (default), asks for confirmation before deleting.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' # clear specific version
#' clear_margot_cache("v1")
#'
#' # clear all cached data
#' clear_margot_cache("all")
#' }
#'
#' @export
clear_margot_cache <- function(version = "all", confirm = TRUE) {
  cache_dir <- tools::R_user_dir("margot", "cache")

  if (!dir.exists(cache_dir)) {
    cli::cli_alert_info("No cache directory found")
    return(invisible(NULL))
  }

  if (version == "all") {
    files <- list.files(cache_dir, pattern = "^margot_data_.*\\.rds$", full.names = TRUE)
  } else {
    files <- file.path(cache_dir, paste0("margot_data_", version, ".rds"))
    files <- files[file.exists(files)]
  }

  if (length(files) == 0) {
    cli::cli_alert_info("No cached files found")
    return(invisible(NULL))
  }

  # calculate total size
  total_size <- sum(file.size(files)) / 1024^2 # in MB

  if (confirm) {
    response <- utils::menu(
      choices = c("Yes", "No"),
      title = sprintf(
        "Delete %d cached file(s) (%.1f MB)?",
        length(files),
        total_size
      )
    )

    if (response != 1) {
      cli::cli_alert_info("Cancelled")
      return(invisible(NULL))
    }
  }

  # delete files
  unlink(files)
  cli::cli_alert_success(
    "Removed {.val {length(files)}} cached file{?s} ({round(total_size, 1)} MB)"
  )

  invisible(NULL)
}
