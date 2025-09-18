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
#' @param ... Additional arguments to pass to margot_interpret_policy_tree(), including
#'   include_conditional_means (default TRUE), use_math_notation (default FALSE),
#'   output_format ("bullet" or "prose"), original_df, label_mapping, and policy value options.
#' @param report_policy_value Character: one of "none" (default), "treat_all",
#'   "control_all", or "both". If not "none", each model interpretation will include
#'   a one-line policy value summary with 95% CIs based on bootstrap SEs.
#' @param policy_value_R Integer >= 199; bootstrap replicates (default 499).
#' @param policy_value_seed Integer or NULL; RNG seed (default 42).
#' @param policy_value_ci_level Numeric confidence level (default 0.95).
#' @param brief Logical; if TRUE, prepend a compact treated-only summary for each
#'   model (coverage treated and average uplift among treated) and optionally save it.
#' @param brief_save_to Optional path to save the brief treated-only summary as text.
#'
#' @return A single character string containing the combined markdown output.
#' @export
margot_interpret_policy_batch <- function(models,
                                          model_names = NULL,
                                          max_depth = 2L,
                                          save_path = NULL,
                                          prefix = NULL,
                                          include_timestamp = FALSE,
                                          report_policy_value = c("none", "treat_all", "control_all", "both", "treated_only"),
                                          policy_value_R = 499L,
                                          policy_value_seed = 42L,
                                          policy_value_ci_level = 0.95,
                                          brief = FALSE,
                                          brief_save_to = NULL,
                                          return_as_list = FALSE,
                                          ...) {
  report_policy_value <- match.arg(report_policy_value)
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

  pb <- cli::cli_progress_bar(
    total = length(model_names),
    format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )

  for (model_name in model_names) {
    cli::cli_alert_info("Processing model: {model_name}")
    tryCatch(
      {
        ## pass max_depth plus any other args on to the interpreter
        interpretation <- margot_interpret_policy_tree(
          model       = models,
          model_name  = model_name,
          max_depth   = max_depth,
          report_policy_value = report_policy_value,
          policy_value_R = policy_value_R,
          policy_value_seed = policy_value_seed,
          policy_value_ci_level = policy_value_ci_level,
          ...
        )
        interpretations_list[[model_name]] <- interpretation
        cli::cli_alert_success("Successfully processed model: {model_name}")
      },
      error = function(e) {
        cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
      }
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Batch processing completed (depth {max_depth})")

  # Filter out NULL interpretations
  valid_interpretations <- Filter(Negate(is.null), interpretations_list)

  if (length(valid_interpretations) == 0) {
    cli::cli_alert_warning("No valid interpretations were generated")
    return("")
  }

  # Build optional treated-only brief summary
  brief_lines <- character()
  if (isTRUE(brief)) {
    dots <- list(...)
    label_mapping <- if (!is.null(dots$label_mapping)) dots$label_mapping else NULL
    for (mn in model_names) {
      m <- models$results[[mn]]
      tag <- paste0("policy_tree_depth_", max_depth)
      pol <- m[[tag]]
      if (is.null(pol)) next
      pd <- m$plot_data
      dr <- m$dr_scores
      if (is.null(pd) || is.null(dr)) next
      full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
      if (is.null(full)) next
      X <- full[, pol$columns, drop = FALSE]
      keep <- stats::complete.cases(X)
      Xk <- X[keep, , drop = FALSE]
      drk <- dr[keep, , drop = FALSE]
      if (!nrow(Xk)) next
      preds <- predict(pol, Xk)
      treat_mask <- preds == 2L
      avg_uplift <- if (any(treat_mask)) mean(drk[treat_mask, 2] - drk[treat_mask, 1]) else NA_real_
      coverage <- mean(treat_mask)
      out <- sub("^model_", "", mn)
      out_disp <- .apply_label_stability(out, label_mapping)
      line <- sprintf("%s: Treated %.1f%%, Avg uplift among treated %.3f",
        out_disp, 100 * coverage, avg_uplift)
      brief_lines <- c(brief_lines, line)
    }
  }

  # Extract common intro: everything up to the first "**Findings for"
  first_txt <- valid_interpretations[[1]]
  if (is.null(first_txt) || !is.character(first_txt)) {
    cli::cli_alert_warning("First interpretation is not valid text")
    return("")
  }

  if (grepl("\\*\\*Findings for", first_txt)) {
    split_at <- strsplit(first_txt, "\\*\\*Findings for", perl = TRUE)[[1]]
    common_intro <- split_at[1]
  } else {
    common_intro <- "Policy tree analysis results:\n\n"
  }

  # Now pull out each model's specific section
  model_specifics <- lapply(valid_interpretations, function(txt) {
    if (!is.null(txt) && is.character(txt) && grepl("\\*\\*Findings for", txt)) {
      parts <- strsplit(txt, "\\*\\*Findings for", perl = TRUE)[[1]]
      if (length(parts) > 1) {
        paste0("**Findings for", parts[2])
      } else {
        txt
      }
    } else {
      # if no Findings header or invalid, just return the whole thing
      txt
    }
  })

  # Assemble final markdown
  # Remove "Policy tree analysis results:" from common_intro if present
  common_intro <- gsub("Policy tree analysis results:\n\n", "", common_intro, fixed = TRUE)

  header <- paste0("### Policy Tree Interpretations (depth ", max_depth, ")\n\n")
  if (isTRUE(brief) && length(brief_lines)) {
    brief_block <- paste0("#### Treated-only Summary\n\n", paste(brief_lines, collapse = "\n"), "\n\n")
  } else {
    brief_block <- ""
  }
  final_output <- paste0(header, brief_block, paste(unlist(model_specifics), collapse = "\n\n"))

  if (isTRUE(return_as_list)) {
    policy_value_explanation <- paste0(
      "Policy value vs control-all: mean benefit when treating only those recommended; ",
      "policy value vs treat-all: mean benefit when withholding treatment only where the policy ",
      "recommends control. Avg uplift among treated: average (DR_treat − DR_control) across units ",
      "the policy recommends to treat."
    )
    return(list(
      report_full = final_output,
      report_brief = if (length(brief_lines)) brief_lines else character(0),
      by_model = valid_interpretations,
      policy_value_explanation = policy_value_explanation
    ))
  } else {
    return(final_output)
  }
}
