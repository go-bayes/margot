#' Batch process policy tree interpretations
#'
#' This function now accepts a vector of model names to process and produces
#' a single combined output. The common description is printed once at the top,
#' followed by each model's specific findings. You can now control whether to
#' interpret the depth-1 or depth-2 tree via the `max_depth` argument.
#'
#' @param models A list containing the results from multi-arm causal forest models.
#' @param model_names A character vector of model names to interpret. If NULL, all models are processed.
#' @param max_depth Integer, 1 or 2; which saved policy tree to interpret (default 2).
#' @param save_path The path where the combined interpretation will be saved. If NULL, nothing is saved.
#' @param prefix An optional prefix for the filename.
#' @param include_timestamp Logical; whether to include a timestamp in the filename (if desired).
#' @param ... Additional arguments to pass to margot_interpret_policy_tree().
#'
#' @return A single character string containing the combined markdown output.
#' @export
margot_interpret_policy_batch <- function(models,
                                          model_names       = NULL,
                                          max_depth         = 2L,
                                          save_path         = NULL,
                                          prefix            = NULL,
                                          include_timestamp = FALSE,
                                          ...) {
  cli::cli_alert_info("Starting batch processing of policy tree interpretations (depth {max_depth})")

  # If model_names is not supplied, process all models
  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- names(models$results)
  }

  # Create save directory if needed
  if (!is.null(save_path) && !dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cli::cli_alert_success("Created output directory: {save_path}")
  }

  interpretations_list <- vector("list", length(model_names))
  names(interpretations_list) <- model_names

  pb <- cli::cli_progress_bar(total = length(model_names),
                              format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info("Processing model: {model_name}")
    tryCatch({
      ## pass max_depth plus any other args on to the interpreter
      interpretation <- margot_interpret_policy_tree(
        model       = models,
        model_name  = model_name,
        max_depth   = max_depth,
        ...
      )
      interpretations_list[[model_name]] <- interpretation
      cli::cli_alert_success("Successfully processed model: {model_name}")
    }, error = function(e) {
      cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
    })
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Batch processing completed (depth {max_depth})")

  # Extract common intro: everything up to the first "**Findings for"
  first_txt <- interpretations_list[[1]]
  split_at  <- strsplit(first_txt, "\\*\\*Findings for", perl = TRUE)[[1]]
  common_intro <- split_at[1]

  # Now pull out each model's specific section
  model_specifics <- lapply(interpretations_list, function(txt) {
    if (grepl("\\*\\*Findings for", txt)) {
      parts <- strsplit(txt, "\\*\\*Findings for", perl = TRUE)[[1]]
      paste0("**Findings for", parts[2])
    } else {
      # if no Findings header, just return the whole thing
      txt
    }
  })

  # Assemble final markdown
  final_output <- paste0(
    "### Policy Tree Interpretations (depth ", max_depth, ")\n\n",
    common_intro, "\n\n",
    paste(unlist(model_specifics), collapse = "\n\n")
  )

  # Optionally save to disk
  if (!is.null(save_path)) {
    fname <- "policy_tree_interpretation_combined"
    if (!is.null(prefix) && nzchar(prefix)) {
      fname <- paste0(prefix, "_", fname)
    }
    if (include_timestamp) {
      fname <- paste0(fname, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    fname <- paste0(fname, ".qs")
    full_path <- file.path(save_path, fname)

    qs::qsave(final_output, full_path)
    cli::cli_alert_success("Saved combined interpretation to {full_path}")
  }

  return(final_output)
}
