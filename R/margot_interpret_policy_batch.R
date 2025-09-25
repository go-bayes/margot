#' Batch process policy tree interpretations
#'
#' This function now accepts a vector of model names to process and produces
#' a single combined output. The common description is printed once at the top,
#' followed by each model's specific findings. You can now control whether to
#' interpret depth-1 or depth-2 trees via the `max_depth` argument, or supply
#' per-model depth choices via `depths_by_model`.
#'
#' @param models A list containing the results from multi-arm causal forest models.
#' @param model_names A character vector of model names to interpret. If named, the values should be 1 or 2
#'   (depth assignments) and take precedence over `max_depth` and `depths_by_model`. If NULL, all models are processed.
#' @param max_depth Integer, 1 or 2; fallback depth used when no per-model depth mapping is provided
#'   via `model_names` or `depths_by_model` (default 2).
#' @param depths_by_model Optional named vector/list mapping models (with or without `model_` prefix) to
#'   depth 1 or 2; combined with any depth hints supplied through `model_names`.
#' @param save_path The path where the combined interpretation will be saved. If NULL, nothing is saved.
#' @param prefix An optional prefix for the filename.
#' @param include_timestamp Logical; whether to include a timestamp in the filename (if desired).
#' @param ... Additional arguments to pass to margot_interpret_policy_tree(), including
#'   include_conditional_means (default TRUE), use_math_notation (default FALSE),
#'   output_format ("bullet" or "prose"), original_df, label_mapping, and policy value options.
#' @param report_policy_value Character: one of "none" (default), "treat_all",
#'   "control_all", or "both". If not "none", each model interpretation will include
#'   a one-line policy value summary with 95\% CIs based on bootstrap SEs.
#' @param policy_value_R Integer >= 199; bootstrap replicates (default 499).
#' @param policy_value_seed Integer or NULL; RNG seed (default 42).
#' @param policy_value_ci_level Numeric confidence level (default 0.95).
#' @param brief Logical; if TRUE, prepend a compact treated-only summary for each
#'   model (coverage treated and average uplift among treated) and optionally save it.
#' @param brief_save_to Optional path to save the brief treated-only summary as text.
#'
#' @return A single character string containing the combined markdown output. When
#'   `return_as_list = TRUE`, returns a list with the full report, brief summary,
#'   per-model sections, policy value explanation, and `model_depths`/`depth_map`.
#' @export
margot_interpret_policy_batch <- function(models,
                                          model_names = NULL,
                                          max_depth = 2L,
                                          depths_by_model = NULL,
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
                                          policy_value_source = c("compute", "use_coherent"),
                                          coherent_values = NULL,
                                          ...) {
  report_policy_value <- match.arg(report_policy_value)
  policy_value_source <- match.arg(policy_value_source)

  available_models <- names(models$results)
  available_clean <- gsub("^model_", "", available_models)
  name_lookup <- setNames(available_models, available_models)
  name_lookup <- c(name_lookup, setNames(available_models, available_clean))
  name_lookup <- name_lookup[!duplicated(names(name_lookup))]

  resolve_model_ids <- function(x) {
    if (is.null(x) || length(x) == 0) return(character())
    x <- as.character(x)
    if (anyNA(x)) {
      stop("model names cannot contain NA values")
    }
    unknown <- setdiff(unique(x), names(name_lookup))
    if (length(unknown)) {
      stop("Unknown model name(s): ", paste(unknown, collapse = ", "))
    }
    unname(name_lookup[x])
  }

  if (!is.null(model_names) && length(model_names) > 0 && !is.null(names(model_names))) {
    candidate_names <- names(model_names)
    depth_vals <- as.character(model_names)
    if (!anyNA(candidate_names) && !anyNA(depth_vals) &&
        all(candidate_names %in% names(name_lookup)) &&
        all(depth_vals %in% c("1", "2"))) {
      inferred_depths <- as.integer(depth_vals)
      resolved_names <- resolve_model_ids(candidate_names)
      depth_hint <- setNames(inferred_depths, resolved_names)
      if (is.null(depths_by_model)) {
        depths_by_model <- depth_hint
      } else {
        depths_by_model[names(depth_hint)] <- depth_hint
      }
      model_names <- resolved_names
    }
  }

  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- available_models
  } else {
    model_names <- resolve_model_ids(model_names)
  }

  # Create save directory if needed
  if (!is.null(save_path) && !dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cli::cli_alert_success("Created output directory: {save_path}")
  }

  # resolve per-model depths
  if (!is.null(depths_by_model)) {
    if (is.null(names(depths_by_model))) {
      stop("depths_by_model must be a named vector or list")
    }
    resolved_names <- resolve_model_ids(names(depths_by_model))
    depth_vec <- setNames(as.integer(depths_by_model), resolved_names)
    if (!all(depth_vec %in% c(1L, 2L))) {
      stop("depths_by_model values must be 1 or 2")
    }
    relevant <- intersect(names(depth_vec), model_names)
    extras <- setdiff(names(depth_vec), relevant)
    if (length(extras)) {
      cli::cli_alert_info("Ignoring depth assignments for models not in `model_names`: {paste(extras, collapse = \", \")}")
    }
    depth_vec <- depth_vec[relevant]
    model_depths <- setNames(rep(as.integer(max_depth), length(model_names)), model_names)
    if (length(depth_vec)) {
      model_depths[names(depth_vec)] <- depth_vec
    }
  } else {
    model_depths <- setNames(rep(as.integer(max_depth), length(model_names)), model_names)
  }
  if (!all(model_depths %in% c(1L, 2L))) {
    stop("depth values must be 1 or 2")
  }
  unique_depths <- sort(unique(model_depths))

  dots <- list(...)
  label_mapping <- if (!is.null(dots$label_mapping)) dots$label_mapping else NULL

  if (length(unique_depths) == 1) {
    cli::cli_alert_info("Starting batch processing of policy tree interpretations (depth {unique_depths})")
  } else {
    cli::cli_alert_info("Starting batch processing of policy tree interpretations (mixed depths)")
  }

  interpretations_list <- vector("list", length(model_names))
  names(interpretations_list) <- model_names

  pb <- cli::cli_progress_bar(
    total = length(model_names),
    format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )

  for (model_name in model_names) {
    depth_cur <- model_depths[[model_name]]
    cli::cli_alert_info("Processing model: {model_name} (depth {depth_cur})")
    tryCatch(
      {
        ## pass max_depth plus any other args on to the interpreter
        # If we are asked to reuse coherent values, suppress internal PV computation
        rp <- if (identical(policy_value_source, "use_coherent")) "none" else report_policy_value
        interpretation <- margot_interpret_policy_tree(
          model       = models,
          model_name  = model_name,
          max_depth   = depth_cur,
          report_policy_value = rp,
          policy_value_R = policy_value_R,
          policy_value_seed = policy_value_seed,
          policy_value_ci_level = policy_value_ci_level,
          ...
        )
        # Optionally append PV lines from coherent_values
        if (identical(policy_value_source, "use_coherent") && !is.null(coherent_values)) {
          cv <- tryCatch({
            sub_ctrl <- coherent_values[coherent_values$model == model_name &
                                         coherent_values$depth == depth_cur &
                                         coherent_values$contrast == "policy - control_all", , drop = FALSE]
            sub_trt  <- coherent_values[coherent_values$model == model_name &
                                         coherent_values$depth == depth_cur &
                                         coherent_values$contrast == "policy - treat_all", , drop = FALSE]
            list(ctrl = sub_ctrl, trt = sub_trt)
          }, error = function(e) NULL)
          if (!is.null(cv) && nrow(cv$ctrl) > 0) {
            pv_ctrl <- sprintf("Policy value vs control-all: %.3f [% .3f, % .3f]",
                               cv$ctrl$estimate[1], cv$ctrl$ci_lo[1], cv$ctrl$ci_hi[1])
            pv_trt <- if (nrow(cv$trt) > 0) sprintf("Policy value vs treat-all: %.3f [% .3f, % .3f]",
                                                   cv$trt$estimate[1], cv$trt$ci_lo[1], cv$trt$ci_hi[1]) else NULL
            extra <- paste(c("\nIn out-of-sample evaluation,", pv_trt, pv_ctrl), collapse = " ")
            interpretation <- paste0(interpretation, "\n", extra, "\n")
          }
        }
        interpretations_list[[model_name]] <- interpretation
        cli::cli_alert_success("Successfully processed model: {model_name} (depth {depth_cur})")
      },
      error = function(e) {
        cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
      }
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  if (length(unique_depths) == 1) {
    cli::cli_alert_success("Batch processing completed (depth {unique_depths})")
  } else {
    cli::cli_alert_success("Batch processing completed (mixed depths)")
  }

  # Filter out NULL interpretations
  valid_interpretations <- Filter(Negate(is.null), interpretations_list)

  if (length(valid_interpretations) == 0) {
    cli::cli_alert_warning("No valid interpretations were generated")
    return("")
  }

  # Build optional treated-only brief summary
  brief_lines <- character()
  if (isTRUE(brief)) {
    for (mn in model_names) {
      m <- models$results[[mn]]
      depth_cur <- model_depths[[mn]]
      tag <- paste0("policy_tree_depth_", depth_cur)
      pol <- m[[tag]]
      if (is.null(pol)) next
      pd <- m$plot_data
      dr <- m$dr_scores
      if (is.null(dr)) dr <- m$dr_scores_flipped
      if (is.null(pd) || is.null(dr)) next
      full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
      if (is.null(full)) next
      full_df <- as.data.frame(full)
      test_idx <- pd$test_indices
      if (is.null(test_idx)) {
        test_idx <- suppressWarnings(as.integer(rownames(full_df)))
      }
      drm <- as.matrix(dr)
      if (!is.null(test_idx) && length(test_idx) == nrow(full_df) && all(!is.na(test_idx)) && max(test_idx) <= nrow(drm)) {
        dr_test <- drm[test_idx, , drop = FALSE]
      } else {
        take <- seq_len(min(nrow(full_df), nrow(drm)))
        dr_test <- drm[take, , drop = FALSE]
        full_df <- full_df[take, , drop = FALSE]
      }
      X <- full_df[, pol$columns, drop = FALSE]
      keep <- stats::complete.cases(X)
      if (!any(keep)) next
      Xk <- X[keep, , drop = FALSE]
      drk <- dr_test[keep, , drop = FALSE]
      if (!nrow(Xk)) next
      preds <- predict(pol, Xk)
      if (is.matrix(preds)) preds <- preds[, 1]
      preds <- .normalize_policy_actions(preds)
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

  if (length(unique_depths) == 1) {
    header <- paste0("### Policy Tree Interpretations (depth ", unique_depths, ")\n\n")
  } else {
    depth_summary <- paste(sprintf("%s → %s",
      if (!is.null(label_mapping)) vapply(model_names, function(mn) .apply_label_stability(gsub('^model_', '', mn), label_mapping), character(1)) else gsub('^model_', '', model_names),
      model_depths[model_names]
    ), collapse = ", ")
    header <- paste0("### Policy Tree Interpretations (mixed depths)\n\nDepth assignments: ", depth_summary, "\n\n")
  }
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
      policy_value_explanation = policy_value_explanation,
      model_depths = model_depths,
      depth_map = model_depths
    ))
  } else {
    return(final_output)
  }
}
