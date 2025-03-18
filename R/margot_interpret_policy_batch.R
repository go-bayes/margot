#' Batch process policy tree interpretations
#'
#' This function now accepts a vector of model names to process and produces
#' a single combined output. The common description is printed once at the top,
#' followed by each model's specific findings.
#'
#' @param models A list containing the results from multi-arm causal forest models.
#' @param model_names A character vector of model names to interpret. If NULL, all models are processed.
#' @param save_path The path where the combined interpretation will be saved. If NULL, nothing is saved.
#' @param prefix An optional prefix for the filename.
#' @param include_timestamp Logical; whether to include a timestamp in the filename (if desired).
#' @param ... Additional arguments to pass to margot_interpret_policy_tree().
#'
#' @return A single character string containing the combined markdown output.
#'
#' @export
margot_interpret_policy_batch <- function(models, model_names = NULL,
                                          save_path = NULL, prefix = NULL,
                                          include_timestamp = FALSE, ...) {
  cli::cli_alert_info("Starting batch processing of policy tree interpretations")

  # If model_names is not supplied, process all models
  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- names(models$results)
  }

  if (!is.null(save_path) && !dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cli::cli_alert_success(paste("Created output directory:", save_path))
  }

  interpretations_list <- list()
  pb <- cli::cli_progress_bar(total = length(model_names),
                              format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    tryCatch({
      interpretation <- margot_interpret_policy_tree(model = models, model_name = model_name, ...)
      interpretations_list[[model_name]] <- interpretation
      cli::cli_alert_success(paste("Successfully processed model:", model_name))
    }, error = function(e) {
      cli::cli_alert_danger(paste("Error processing model", model_name, ":", e$message))
    })
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Batch processing of policy tree interpretations completed successfully")

  # Extract the common introduction text from the first interpretation.
  # We assume that everything before the first occurrence of "**Findings for" is common.
  first_interp <- interpretations_list[[1]]
  split_common <- strsplit(first_interp, "\\*\\*Findings for")[[1]]
  common_intro <- split_common[1]

  # Extract the specific findings from each interpretation.
  # This removes the repeated common text.
  combined_specific <- sapply(interpretations_list, function(txt) {
    if (grepl("\\*\\*Findings for", txt)) {
      parts <- strsplit(txt, "\\*\\*Findings for")[[1]]
      # Reattach the header
      specific <- paste0("**Findings for", parts[2])
      return(specific)
    } else {
      return(txt)
    }
  })
  # Combine into a single output.
  final_output <- paste0("### Policy Tree Interpretations\n\n",
                         common_intro, "\n\n",
                         paste(combined_specific, collapse = "\n\n"))

  # Save the combined output if save_path is provided.
  if (!is.null(save_path)) {
    filename <- "policy_tree_interpretation_combined"
    if (!is.null(prefix) && nzchar(prefix)) {
      filename <- paste0(prefix, "_", filename)
    }
    if (include_timestamp) {
      filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    filename <- paste0(filename, ".qs")

    full_path <- here::here(save_path, filename)
    qs::qsave(final_output, file = full_path)
    cli::cli_alert_success(paste("Saved combined interpretation to", full_path))
  }

  return(final_output)
}
