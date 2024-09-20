#' Batch Process Policy Tree Interpretations
#'
#' This function batch processes outputs from margot_interpret_policy_tree() models
#' and optionally saves them using the here_save_qs method.
#'
#' @param models A list containing the results from multi-arm causal forest models.
#' @param save_path The path where the interpretations will be saved. If NULL, interpretations are not saved. Default is NULL.
#' @param prefix An optional prefix for the filenames. Default is NULL.
#' @param include_timestamp Logical, whether to include a timestamp in the filename. Default is FALSE.
#' @param ... Additional arguments to pass to margot_interpret_policy_tree().
#'
#' @return A list of the generated interpretations.
#'
#' @import cli
#' @import here
#' @import qs
#'
#' @export
margot_interpret_policy_batch <- function(models, save_path = NULL, prefix = NULL, include_timestamp = FALSE, ...) {
  cli::cli_alert_info("Starting batch processing of policy tree interpretations")

  if (!is.null(save_path) && !dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cli::cli_alert_success(paste("Created output directory:", save_path))
  }

  model_names <- names(models$results)
  cli::cli_alert_info(paste("Number of models to process:", length(model_names)))

  interpretations <- list()
  pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (model_name in model_names) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    tryCatch({
      interpretation <- margot_interpret_policy_tree(model = models, model_name = model_name, ...)

      interpretations[[model_name]] <- interpretation

      if (!is.null(save_path)) {
        # Generate filename
        filename <- "policy_tree_interpretation"
        if (!is.null(prefix) && nzchar(prefix)) {
          filename <- paste0(prefix, "_", filename)
        }
        filename <- paste0(filename, "_", model_name)
        if (include_timestamp) {
          filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        }
        filename <- paste0(filename, ".qs")

        # Save interpretation using here_save_qs
        full_path <- here::here(save_path, filename)
        qs::qsave(interpretation, file = full_path)
        cli::cli_alert_success(paste("Saved interpretation to", full_path))
      }

      cli::cli_alert_success(paste("Successfully processed model:", model_name))
    }, error = function(e) {
      cli::cli_alert_danger(paste("Error processing model", model_name, ":", e$message))
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Batch processing of policy tree interpretations completed successfully")

  return(interpretations)
}
