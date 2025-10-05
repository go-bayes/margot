#' Combine LMTP Models from Multiple Batches and Compute Cross-Batch Contrasts
#'
#' @description
#' This function merges LMTP models from multiple batch runs and computes user-specified
#' contrasts across batches. This enables comparisons between shifts that were estimated
#' in separate runs (e.g., comparing shift_zero from batch 2 with ipsi_02 from batch 1).
#'
#' @param ... One or more LMTP output objects from margot_lmtp()
#' @param contrasts Optional list of character vectors (each length 2) specifying contrast pairs.
#'   Example: list(c("shift_zero", "ipsi_02"), c("ipsi_10", "ipsi_05"))
#' @param contrast_scale Scale for contrasts: "additive", "rr", or "or". Default is "additive".
#' @param auto_pairwise Logical, if TRUE compute all pairwise contrasts among available shifts.
#'   Default is FALSE. If both contrasts and auto_pairwise are specified, contrasts takes precedence.
#' @param include_null_contrasts Logical, if TRUE include contrasts against null shift when
#'   auto_pairwise = TRUE. Default is TRUE. Ignored when auto_pairwise = FALSE.
#' @param quiet Logical, if TRUE suppress informational CLI messages. Default is FALSE.
#'
#' @details
#' The function merges models by outcome and shift, then computes specified contrasts only
#' where both shifts exist for the same outcome. Original models are not retained in the output.
#' If an outcome is missing from one batch or a shift doesn't exist for a particular outcome,
#' the function warns but continues processing valid contrasts.
#'
#' @return A list with the standard LMTP output structure:
#'   \item{models}{NULL (not retained per design)}
#'   \item{contrasts}{List of contrasts by outcome}
#'   \item{individual_tables}{List of evaluation tables by outcome}
#'   \item{combined_tables}{List of combined tables across outcomes}
#'
#' @examples
#' \dontrun{
#' # Run two separate LMTP batches with non-overlapping shifts
#' fit_batch_1 <- margot_lmtp(
#'   data = df,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   trt = A,
#'   shift_functions = list(ipsi_02 = ipsi(2), ipsi_05 = ipsi(5)),
#'   include_null_shift = TRUE
#' )
#'
#' fit_batch_2 <- margot_lmtp(
#'   data = df,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   trt = A,
#'   shift_functions = list(ipsi_10 = ipsi(10), shift_zero = treatment_zero),
#'   include_null_shift = TRUE
#' )
#'
#' # Combine and compute cross-batch contrasts
#' combined <- margot_lmtp_combine_and_contrast(
#'   fit_batch_1,
#'   fit_batch_2,
#'   contrasts = list(
#'     c("shift_zero", "ipsi_02"),
#'     c("shift_zero", "ipsi_05"),
#'     c("ipsi_10", "ipsi_02")
#'   ),
#'   contrast_scale = "additive"
#' )
#'
#' # Or use automatic pairwise contrasts
#' combined_auto <- margot_lmtp_combine_and_contrast(
#'   fit_batch_1,
#'   fit_batch_2,
#'   auto_pairwise = TRUE,
#'   include_null_contrasts = TRUE,
#'   contrast_scale = "additive"
#' )
#' }
#'
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom purrr map_lgl
#' @importFrom dplyr bind_rows
#' @export
margot_lmtp_combine_and_contrast <- function(
    ...,
    contrasts = NULL,
    contrast_scale = c("additive", "rr", "or"),
    auto_pairwise = FALSE,
    include_null_contrasts = TRUE,
    quiet = FALSE) {

  contrast_scale <- match.arg(contrast_scale)
  lmtp_outputs <- list(...)
  n_batches <- length(lmtp_outputs)

  # validate inputs
  if (n_batches < 1) {
    cli::cli_alert_danger("No LMTP outputs provided")
    stop("No LMTP outputs provided", call. = FALSE)
  }

  # check all inputs are lmtp outputs
  lmtp_structure <- c("models", "contrasts", "individual_tables", "combined_tables")
  is_valid <- purrr::map_lgl(lmtp_outputs, ~ all(lmtp_structure %in% names(.x)))

  if (!all(is_valid)) {
    invalid_idx <- which(!is_valid)
    cli::cli_alert_danger("Invalid LMTP output structure at position{?s}: {invalid_idx}")
    stop("All inputs must be LMTP output objects from margot_lmtp()", call. = FALSE)
  }

  if (!quiet) {
    cli::cli_h1("Combining LMTP Models from Multiple Batches")
    cli::cli_alert_info("Processing {n_batches} LMTP batch{?es}...")
  }

  # extract and organize models by outcome and shift
  # structure: all_models[[outcome]][[shift]] <- model_object
  all_models <- list()
  model_sources <- list()  # track which batch each model came from

  for (batch_idx in seq_along(lmtp_outputs)) {
    batch_models <- lmtp_outputs[[batch_idx]]$models

    if (is.null(batch_models) || length(batch_models) == 0) {
      if (!quiet) {
        cli::cli_alert_warning("Batch {batch_idx} contains no models - skipping")
      }
      next
    }

    for (outcome in names(batch_models)) {
      if (!outcome %in% names(all_models)) {
        all_models[[outcome]] <- list()
        model_sources[[outcome]] <- list()
      }

      outcome_models <- batch_models[[outcome]]

      for (model_name in names(outcome_models)) {
        # extract shift name from model name (format: outcome_shift)
        shift_name <- gsub(paste0("^", outcome, "_"), "", model_name)

        # check for duplicates
        if (shift_name %in% names(all_models[[outcome]])) {
          if (!quiet) {
            cli::cli_alert_warning(
              "Duplicate model found: outcome={.val {outcome}}, shift={.val {shift_name}}. Using model from batch {batch_idx}"
            )
          }
        }

        all_models[[outcome]][[shift_name]] <- outcome_models[[model_name]]
        model_sources[[outcome]][[shift_name]] <- batch_idx
      }
    }
  }

  # get unique outcomes and shifts
  unique_outcomes <- names(all_models)
  unique_shifts <- unique(unlist(lapply(all_models, names)))

  if (!quiet) {
    cli::cli_alert_info("Found {length(unique_outcomes)} unique outcome{?s}: {.val {unique_outcomes}}")
    cli::cli_alert_info("Found {length(unique_shifts)} unique shift{?s}: {.val {unique_shifts}}")
  }

  # determine which contrasts to compute
  contrasts_to_compute <- NULL

  if (!is.null(contrasts) && auto_pairwise) {
    if (!quiet) {
      cli::cli_alert_warning("Both 'contrasts' and 'auto_pairwise=TRUE' specified. Using 'contrasts'.")
    }
  }

  if (!is.null(contrasts)) {
    # validate user-specified contrasts
    if (!is.list(contrasts)) {
      cli::cli_alert_danger("'contrasts' must be a list of character vectors")
      stop("'contrasts' must be a list of character vectors", call. = FALSE)
    }

    # validate each contrast pair
    valid_contrasts <- list()
    for (i in seq_along(contrasts)) {
      contrast_pair <- contrasts[[i]]

      if (!is.character(contrast_pair) || length(contrast_pair) != 2) {
        if (!quiet) {
          cli::cli_alert_warning("Contrast {i} is not a character vector of length 2 - skipping")
        }
        next
      }

      # check if both shifts exist in at least one outcome
      shift1_exists <- any(sapply(all_models, function(om) shift1 <- contrast_pair[1] %in% names(om)))
      shift2_exists <- any(sapply(all_models, function(om) shift2 <- contrast_pair[2] %in% names(om)))

      if (!shift1_exists) {
        if (!quiet) {
          cli::cli_alert_warning("Shift '{contrast_pair[1]}' not found in any outcome - skipping contrast")
        }
        next
      }
      if (!shift2_exists) {
        if (!quiet) {
          cli::cli_alert_warning("Shift '{contrast_pair[2]}' not found in any outcome - skipping contrast")
        }
        next
      }

      valid_contrasts[[length(valid_contrasts) + 1]] <- contrast_pair
    }

    contrasts_to_compute <- valid_contrasts

  } else if (auto_pairwise) {
    # generate all pairwise contrasts
    if (!quiet) {
      cli::cli_alert_info("Generating all pairwise contrasts automatically...")
    }

    contrasts_to_compute <- list()

    # filter shifts if excluding null
    shifts_for_contrasts <- unique_shifts
    if (!include_null_contrasts && "null" %in% unique_shifts) {
      shifts_for_contrasts <- setdiff(unique_shifts, "null")
    }

    # generate all pairs
    if (length(shifts_for_contrasts) >= 2) {
      for (i in 1:(length(shifts_for_contrasts) - 1)) {
        for (j in (i + 1):length(shifts_for_contrasts)) {
          contrasts_to_compute[[length(contrasts_to_compute) + 1]] <- c(
            shifts_for_contrasts[i],
            shifts_for_contrasts[j]
          )
        }
      }
    }

    # add null contrasts if requested
    if (include_null_contrasts && "null" %in% unique_shifts) {
      for (shift in setdiff(unique_shifts, "null")) {
        contrasts_to_compute[[length(contrasts_to_compute) + 1]] <- c(shift, "null")
      }
    }

  } else {
    cli::cli_alert_danger("Either 'contrasts' or 'auto_pairwise=TRUE' must be specified")
    stop("Either 'contrasts' or 'auto_pairwise=TRUE' must be specified", call. = FALSE)
  }

  if (length(contrasts_to_compute) == 0) {
    cli::cli_alert_danger("No valid contrasts to compute")
    stop("No valid contrasts to compute", call. = FALSE)
  }

  if (!quiet) {
    cli::cli_alert_info("Will attempt to compute {length(contrasts_to_compute)} contrast{?s} across {length(unique_outcomes)} outcome{?s}")
  }

  # compute contrasts
  all_contrasts <- list()
  all_tables <- list()
  contrast_stats <- list(
    total_attempts = 0,
    successful = 0,
    skipped_missing_model = 0,
    errors = 0
  )

  if (!quiet) {
    cli::cli_h2("Computing Contrasts")
  }

  for (outcome in unique_outcomes) {
    if (!quiet) {
      cli::cli_h3("Outcome: {.val {outcome}}")
    }

    outcome_contrasts <- list()
    outcome_tables <- list()

    for (contrast_pair in contrasts_to_compute) {
      shift1 <- contrast_pair[1]
      shift2 <- contrast_pair[2]

      contrast_name <- paste0(outcome, "_", shift1, "_vs_", outcome, "_", shift2)
      contrast_stats$total_attempts <- contrast_stats$total_attempts + 1

      # check if both models exist for this outcome
      if (!shift1 %in% names(all_models[[outcome]])) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Shift '{shift1}' not found for outcome '{outcome}' - skipping contrast"
          )
        }
        contrast_stats$skipped_missing_model <- contrast_stats$skipped_missing_model + 1
        next
      }

      if (!shift2 %in% names(all_models[[outcome]])) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Shift '{shift2}' not found for outcome '{outcome}' - skipping contrast"
          )
        }
        contrast_stats$skipped_missing_model <- contrast_stats$skipped_missing_model + 1
        next
      }

      # get models
      model1 <- all_models[[outcome]][[shift1]]
      model2 <- all_models[[outcome]][[shift2]]

      # check models are not NULL
      if (is.null(model1)) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Model for '{shift1}' (outcome '{outcome}') is NULL - skipping contrast"
          )
        }
        contrast_stats$skipped_missing_model <- contrast_stats$skipped_missing_model + 1
        next
      }

      if (is.null(model2)) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Model for '{shift2}' (outcome '{outcome}') is NULL - skipping contrast"
          )
        }
        contrast_stats$skipped_missing_model <- contrast_stats$skipped_missing_model + 1
        next
      }

      # compute contrast
      tryCatch(
        {
          contrast <- lmtp::lmtp_contrast(
            model1,
            ref = model2,
            type = contrast_scale
          )
          outcome_contrasts[[contrast_name]] <- contrast

          # create evaluation table
          scale <- if (contrast_scale == "additive") "RD" else "RR"
          table <- margot::margot_lmtp_evalue(
            contrast,
            scale = scale,
            new_name = outcome
          )
          outcome_tables[[contrast_name]] <- table

          contrast_stats$successful <- contrast_stats$successful + 1

          if (!quiet) {
            source1 <- model_sources[[outcome]][[shift1]]
            source2 <- model_sources[[outcome]][[shift2]]
            cli::cli_alert_success(
              "Contrast: {.val {shift1}} (batch {source1}) vs {.val {shift2}} (batch {source2})"
            )
          }
        },
        error = function(e) {
          if (!quiet) {
            cli::cli_alert_danger(
              "Error computing contrast {.val {shift1}} vs {.val {shift2}}: {e$message}"
            )
          }
          contrast_stats$errors <<- contrast_stats$errors + 1
        }
      )
    }

    if (length(outcome_contrasts) > 0) {
      all_contrasts[[outcome]] <- outcome_contrasts
      all_tables[[outcome]] <- outcome_tables
    }
  }

  # create combined tables across outcomes
  if (!quiet) {
    cli::cli_h2("Creating Combined Tables Across Outcomes")
  }

  combined_tables <- list()

  # get all unique contrast patterns (shift1_vs_shift2)
  all_contrast_patterns <- unique(unlist(lapply(all_tables, function(outcome_tables) {
    # extract shift pattern from contrast names
    sapply(names(outcome_tables), function(cn) {
      # remove outcome prefix to get shift pattern
      # format: outcome_shift1_vs_outcome_shift2
      # we need to extract: shift1_vs_shift2
      parts <- strsplit(cn, "_vs_")[[1]]
      if (length(parts) == 2) {
        # remove outcome from first part
        outcome_name <- names(all_tables)[which(sapply(names(all_tables), function(o) {
          grepl(paste0("^", o, "_"), cn)
        }))[1]]
        shift1 <- gsub(paste0("^", outcome_name, "_"), "", parts[1])
        shift2 <- gsub(paste0("^", outcome_name, "_"), "", parts[2])
        paste0(shift1, "_vs_", shift2)
      } else {
        NA_character_
      }
    })
  })))

  all_contrast_patterns <- all_contrast_patterns[!is.na(all_contrast_patterns)]

  for (pattern in all_contrast_patterns) {
    combined_name <- paste0("combined_outcomes_", pattern)

    # extract shift names from pattern
    pattern_parts <- strsplit(pattern, "_vs_")[[1]]
    if (length(pattern_parts) != 2) next

    shift1 <- pattern_parts[1]
    shift2 <- pattern_parts[2]

    # collect tables for this pattern across all outcomes
    combined_table <- do.call(rbind, lapply(unique_outcomes, function(outcome) {
      table_name <- paste0(outcome, "_", shift1, "_vs_", outcome, "_", shift2)
      if (!is.null(all_tables[[outcome]][[table_name]])) {
        all_tables[[outcome]][[table_name]]
      } else {
        NULL
      }
    }))

    if (!is.null(combined_table) && nrow(combined_table) > 0) {
      combined_tables[[combined_name]] <- combined_table
      if (!quiet) {
        cli::cli_alert_success("Created combined table: {.val {combined_name}}")
      }
    }
  }

  # summary
  if (!quiet) {
    cli::cli_h2("Summary")
    cli::cli_alert_info("Total contrast attempts: {contrast_stats$total_attempts}")
    cli::cli_alert_success("Successful contrasts: {contrast_stats$successful}")
    if (contrast_stats$skipped_missing_model > 0) {
      cli::cli_alert_warning("Skipped (missing model): {contrast_stats$skipped_missing_model}")
    }
    if (contrast_stats$errors > 0) {
      cli::cli_alert_danger("Errors: {contrast_stats$errors}")
    }
    cli::cli_alert_success("Analysis complete \U0001F44D")
  }

  # return standard lmtp output structure (without models)
  output <- list(
    models = NULL,  # not retained per design
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  )

  return(output)
}
