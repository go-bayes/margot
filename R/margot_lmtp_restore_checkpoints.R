#' Restore LMTP Output from Saved Checkpoints
#'
#' @description
#' Loads the checkpoint files produced by `margot_lmtp()` and rebuilds the
#' downstream contrasts and evaluation tables so the returned object mimics
#' a successful call to `margot_lmtp()`. This is useful when a long-running
#' batch completed all model fits but failed before combining results.
#'
#' @param checkpoint_dir Path to the directory that contains the checkpoint
#'   files saved by `margot_lmtp()` (e.g.,
#'   `.../checkpoints/prefix_YYYYMMDD_HHMMSS`). Checkpoints are read from
#'   `.rds` files. Legacy `.qs` checkpoints are also restored when the optional
#'   `qs` package is installed.
#' @param outcome_vars Optional character vector giving the desired ordering of
#'   outcomes. When omitted, the order is inferred from the checkpoints.
#' @param contrast_type Type of contrasts to compute: `"pairwise"` or `"null"`.
#'   Defaults to `"pairwise"`.
#' @param contrast_scale Scale for contrasts: `"additive"`, `"rr"`, or `"or"`.
#'   Defaults to `"additive"`, matching the default used in `margot_lmtp()`.
#' @param quiet Logical; if `TRUE`, suppresses CLI messages. Defaults to `FALSE`.
#'
#' @return A list with the same structure as `margot_lmtp()` output:
#'   `models`, `contrasts`, `individual_tables`, and `combined_tables`.
#'   The normalised checkpoint directory is attached via the
#'   `"checkpoint_dir"` attribute for reference.
#'
#' @examples
#' \dontrun{
#' restored <- margot_lmtp_restore_checkpoints(
#'   checkpoint_dir = "/path/to/checkpoints/ipsi_fixed_20251006_140122",
#'   contrast_type = "pairwise",
#'   contrast_scale = "additive"
#' )
#' }
#'
#' @export
margot_lmtp_restore_checkpoints <- function(
    checkpoint_dir,
    outcome_vars = NULL,
    contrast_type = c("pairwise", "null"),
    contrast_scale = c("additive", "rr", "or"),
    quiet = FALSE) {

  if (missing(checkpoint_dir) || !nzchar(checkpoint_dir)) {
    stop("`checkpoint_dir` must be a non-empty path", call. = FALSE)
  }

  if (!dir.exists(checkpoint_dir)) {
    stop(sprintf("Checkpoint directory does not exist: %s", checkpoint_dir), call. = FALSE)
  }

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  rds_files <- list.files(
    checkpoint_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )
  qs_files <- list.files(
    checkpoint_dir,
    pattern = "\\.qs$",
    full.names = TRUE
  )

  if (length(rds_files) == 0 && length(qs_files) == 0) {
    stop(
      sprintf(
        "No checkpoint files (*.rds or *.qs) found in directory: %s",
        checkpoint_dir
      ),
      call. = FALSE
    )
  }

  if (length(qs_files) > 0L && !requireNamespace("qs", quietly = TRUE)) {
    if (length(rds_files) == 0L) {
      stop(
        "Found legacy .qs checkpoints, but the optional 'qs' package is not installed. ",
        "Install or load margot in an R environment where 'qs' is available, then rerun ",
        "margot_lmtp_restore_checkpoints(). If local 'qs' is not available, convert the ",
        "checkpoint directory with margot_convert_qs_dir_docker() first.",
        call. = FALSE
      )
    }
    if (!quiet) {
      cli::cli_alert_warning(
        "Ignoring {length(qs_files)} legacy .qs checkpoint file{?s}; optional package 'qs' is not installed."
      )
    }
    qs_files <- character(0)
  }

  checkpoint_files <- sort(c(rds_files, qs_files))

  if (!quiet) {
    cli::cli_h1("Restoring LMTP Models from Checkpoints")
    cli::cli_alert_info("Loading checkpoints from {.path {checkpoint_dir}}")
    cli::cli_alert_info("Detected {length(checkpoint_files)} checkpoint file{?s}")
    if (length(qs_files) > 0L) {
      cli::cli_alert_warning(
        "Reading {length(qs_files)} legacy .qs checkpoint file{?s}; convert to .qs2 or .rds when convenient."
      )
    }
  }

  all_models <- list()
  shift_names <- character(0)
  loaded <- list()

  for (file_path in checkpoint_files) {
    file_name <- basename(file_path)
    file_ext <- tolower(tools::file_ext(file_name))
    checkpoint_obj <- tryCatch(
      {
        if (identical(file_ext, "rds")) {
          readRDS(file_path)
        } else if (identical(file_ext, "qs")) {
          qs::qread(file_path, nthreads = 1)
        } else {
          stop(sprintf("Unsupported checkpoint extension: %s", file_ext), call. = FALSE)
        }
      },
      error = function(e) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Skipping {.file {file_name}} (failed to read): {e$message}"
          )
        }
        NULL
      }
    )

    if (is.null(checkpoint_obj)) {
      next
    }

    if (!is.list(checkpoint_obj) || is.null(checkpoint_obj$model)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Skipping {.file {file_name}} (unexpected structure)"
        )
      }
      next
    }

    outcome <- checkpoint_obj$outcome
    shift_name <- checkpoint_obj$shift_name

    if (is.null(outcome) || !nzchar(outcome)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Skipping {.file {file_name}} (missing `outcome` metadata)"
        )
      }
      next
    }

    if (is.null(shift_name) || !nzchar(shift_name)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Skipping {.file {file_name}} (missing `shift_name` metadata)"
        )
      }
      next
    }

    model <- checkpoint_obj$model
    if (is.null(model)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Skipping {.file {file_name}} (`model` is NULL)"
        )
      }
      next
    }

    model_name <- paste0(outcome, "_", shift_name)
    if (is.null(all_models[[outcome]])) {
      all_models[[outcome]] <- list()
    }

    if (!quiet && model_name %in% names(all_models[[outcome]])) {
      cli::cli_alert_warning(
        "Replacing existing model for outcome {.val {outcome}}, shift {.val {shift_name}}"
      )
    }

    all_models[[outcome]][[model_name]] <- model
    shift_names <- unique(c(shift_names, shift_name))
    loaded[[model_name]] <- list(
      file = file_path,
      timestamp = checkpoint_obj$timestamp %||% NA_character_
    )

    if (!quiet) {
      cli::cli_alert_success(
        "Loaded {.file {file_name}} -> outcome {.val {outcome}}, shift {.val {shift_name}}"
      )
    }
  }

  if (length(all_models) == 0) {
    stop(
      "No valid models were loaded from the checkpoint directory.",
      call. = FALSE
    )
  }

  if (!quiet) {
    cli::cli_h2("Checking shift coverage across outcomes")
  }
  expected_shifts <- sort(unique(shift_names))
  if (length(expected_shifts) > 0) {
    complete_coverage <- TRUE
    for (outcome in names(all_models)) {
      available_shifts <- sub(paste0("^", outcome, "_"), "", names(all_models[[outcome]]))
      missing_shifts <- setdiff(expected_shifts, available_shifts)
      if (length(missing_shifts) > 0) {
        complete_coverage <- FALSE
        if (!quiet) {
          cli::cli_alert_warning(
            "Outcome {.val {outcome}} missing shift{?s}: {.val {missing_shifts}}"
          )
        }
      }
    }
    if (complete_coverage && !quiet) {
      cli::cli_alert_success("All outcomes include shift{?s}: {.val {expected_shifts}}")
    }
  }

  finalized <- margot_lmtp_finalize_outputs(
    all_models = all_models,
    outcome_vars = outcome_vars,
    shift_names = shift_names,
    contrast_type = contrast_type,
    contrast_scale = contrast_scale,
    quiet = quiet
  )

  complete_output <- list(
    models = finalized$models,
    contrasts = finalized$contrasts,
    individual_tables = finalized$individual_tables,
    combined_tables = finalized$combined_tables
  )

  attr(complete_output, "checkpoint_dir") <- normalizePath(checkpoint_dir, mustWork = FALSE)
  attr(complete_output, "checkpoint_files") <- loaded

  if (!quiet) {
    cli::cli_alert_success("Checkpoint restoration complete \U0001F44D")
  }

  complete_output
}
