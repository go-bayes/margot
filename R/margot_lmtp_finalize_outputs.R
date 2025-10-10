# Internal helper to finalise LMTP outputs from a list of fitted models.
# This consolidates the downstream logic shared by margot_lmtp() and
# margot_lmtp_restore_checkpoints().
margot_lmtp_finalize_outputs <- function(
    all_models,
    outcome_vars,
    shift_names = NULL,
    contrast_type = c("pairwise", "null"),
    contrast_scale = c("additive", "rr", "or"),
    quiet = FALSE) {

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  if (is.null(outcome_vars) || length(outcome_vars) == 0) {
    outcome_vars <- names(all_models)
  }

  # Ensure we have named list elements for each outcome
  if (!is.null(outcome_vars)) {
    missing_outcomes <- setdiff(outcome_vars, names(all_models))
    if (length(missing_outcomes) > 0) {
      for (outcome in missing_outcomes) {
        all_models[[outcome]] <- list()
      }
      # Reorder to match requested outcome order if provided
      all_models <- all_models[outcome_vars]
    }
  }

  # Infer shift order if not supplied
  if (is.null(shift_names) || length(shift_names) == 0) {
    inferred_shifts <- unique(unlist(mapply(
      function(outcome, models) {
        if (is.null(models)) {
          return(character(0))
        }
        if (length(models) == 0) {
          return(character(0))
        }
        sub(paste0("^", outcome, "_"), "", names(models))
      },
      outcome = names(all_models),
      models = all_models,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )))
    shift_names <- inferred_shifts
  }

  # Guard against NULL
  outcome_vars <- outcome_vars %||% names(all_models)
  if (is.null(outcome_vars) || length(outcome_vars) == 0) {
    cli::cli_alert_warning("No outcomes detected in supplied models")
    return(list(
      models = all_models,
      contrasts = list(),
      individual_tables = list(),
      combined_tables = list()
    ))
  }

  # Local wrappers to honour quiet = TRUE while preserving existing messaging
  cli_h2 <- function(..., .envir = parent.frame()) {
    if (!quiet) cli::cli_h2(..., .envir = .envir)
  }
  cli_h3 <- function(..., .envir = parent.frame()) {
    if (!quiet) cli::cli_h3(..., .envir = .envir)
  }
  cli_alert_success <- function(..., .envir = parent.frame()) {
    if (!quiet) cli::cli_alert_success(..., .envir = .envir)
  }
  cli_alert_warning <- function(..., .envir = parent.frame()) {
    if (!quiet) cli::cli_alert_warning(..., .envir = .envir)
  }
  cli_alert_danger <- function(..., .envir = parent.frame()) {
    if (!quiet) cli::cli_alert_danger(..., .envir = .envir)
  }

  all_contrasts <- list()
  all_tables <- list()

  for (outcome in outcome_vars) {
    cli_h2("Processing outcome: {.val {outcome}}")

    outcome_models <- all_models[[outcome]]
    if (is.null(outcome_models)) {
      outcome_models <- list()
    }

    model_names <- names(outcome_models)
    if (length(model_names) == 0) {
      cli_alert_warning("No models available for contrasts for {.val {outcome}}")
      all_contrasts[[outcome]] <- list()
      all_tables[[outcome]] <- list()
      next
    }

    cli_h3("Calculating contrasts")
    contrasts <- list()

    if (contrast_type == "null") {
      null_model_name <- grep("null", model_names, value = TRUE)

      if (length(null_model_name) == 0) {
        cli_alert_danger(
          "No null model found for {.val {outcome}}. Cannot compute null contrasts."
        )
        cli_alert_warning("Available models: {paste(model_names, collapse = ', ')}")
        all_contrasts[[outcome]] <- list()
        all_tables[[outcome]] <- list()
        next
      }

      if (length(null_model_name) > 1) {
        cli_alert_warning(
          "Multiple null models found. Using first: {null_model_name[1]}"
        )
        null_model_name <- null_model_name[1]
      }

      null_model <- outcome_models[[null_model_name]]

      if (is.null(null_model)) {
        cli_alert_danger(
          "Null model {null_model_name} exists but is NULL. Cannot compute contrasts."
        )
        all_contrasts[[outcome]] <- list()
        all_tables[[outcome]] <- list()
        next
      }

      for (model_name in model_names) {
        if (!grepl("null", model_name, fixed = TRUE)) {
          contrast_name <- paste0(model_name, "_vs_null")
          tryCatch(
            {
              contrast <- lmtp::lmtp_contrast(
                outcome_models[[model_name]],
                ref = null_model,
                type = contrast_scale
              )
              contrasts[[contrast_name]] <- contrast
              cli_alert_success("Completed contrast: {.val {contrast_name}}")
            },
            error = function(e) {
              cli_alert_danger(
                "Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}"
              )
            }
          )
        }
      }
    } else {
      if (length(model_names) < 2) {
        cli_alert_warning(
          "Only one model available for {.val {outcome}}; pairwise contrasts skipped."
        )
      } else {
        for (i in 1:(length(model_names) - 1)) {
          for (j in (i + 1):length(model_names)) {
            contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
            tryCatch(
              {
                contrast <- lmtp::lmtp_contrast(
                  outcome_models[[model_names[i]]],
                  ref = outcome_models[[model_names[j]]],
                  type = contrast_scale
                )
                contrasts[[contrast_name]] <- contrast
                cli_alert_success("Completed contrast: {.val {contrast_name}}")
              },
              error = function(e) {
                cli_alert_danger(
                  "Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}"
                )
              }
            )
          }
        }
      }
    }

    all_contrasts[[outcome]] <- contrasts

    cli_h3("Creating tables")
    tables <- list()

    for (contrast_name in names(contrasts)) {
      tryCatch(
        {
          scale <- if (contrast_scale == "additive") "RD" else "RR"
          table <- margot::margot_lmtp_evalue(
            contrasts[[contrast_name]],
            scale = scale,
            new_name = outcome
          )
          tables[[contrast_name]] <- table
          cli_alert_success("Created table for contrast: {.val {contrast_name}}")
        },
        error = function(e) {
          cli_alert_danger(
            "Error in creating table for contrast {.val {contrast_name}}, outcome {.val {outcome}}: {e$message}"
          )
        }
      )
    }

    all_tables[[outcome]] <- tables
  }

  cli_h2("Combining tables across outcomes")
  combined_tables <- list()

  if (contrast_type == "null") {
    for (shift_name in shift_names) {
      if (identical(shift_name, "null")) {
        next
      }

      contrast_name <- paste0("combined_outcomes_", shift_name, "_vs_null")
      combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
        table_name <- paste0(outcome, "_", shift_name, "_vs_null")
        if (!is.null(all_tables[[outcome]][[table_name]])) {
          all_tables[[outcome]][[table_name]]
        } else {
          cli_alert_warning("Table not found for: {table_name}")
          NULL
        }
      }))

      if (!is.null(combined_table) && nrow(combined_table) > 0) {
        combined_tables[[contrast_name]] <- combined_table
        cli_alert_success("Created combined table: {.val {contrast_name}}")
      } else {
        cli_alert_warning("No data for combined table: {contrast_name}")
      }
    }
  } else if (length(shift_names) >= 2) {
    for (i in 1:(length(shift_names) - 1)) {
      for (j in (i + 1):length(shift_names)) {
        contrast_name <- paste0(
          "combined_outcomes_",
          shift_names[i],
          "_vs_",
          shift_names[j]
        )

        combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
          table_name <- paste0(
            outcome,
            "_",
            shift_names[i],
            "_vs_",
            outcome,
            "_",
            shift_names[j]
          )
          if (!is.null(all_tables[[outcome]][[table_name]])) {
            all_tables[[outcome]][[table_name]]
          } else {
            cli_alert_warning("Table not found for: {table_name}")
            NULL
          }
        }))

        if (!is.null(combined_table) && nrow(combined_table) > 0) {
          combined_tables[[contrast_name]] <- combined_table
          cli_alert_success("Created combined table: {.val {contrast_name}}")
        } else {
          cli_alert_warning("No data for combined table: {contrast_name}")
        }
      }
    }
  }

  list(
    models = all_models,
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  )
}
