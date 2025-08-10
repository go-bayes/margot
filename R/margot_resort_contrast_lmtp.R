#' Resort/Reorder LMTP Contrasts by Recomputing from Models
#'
#' This function recomputes LMTP contrasts with the desired ordering by calling
#' lmtp::lmtp_contrast() with models in the specified order, then regenerating
#' evaluation tables.
#'
#' @param lmtp_output Output from margot_lmtp() function containing models, contrasts, and tables
#' @param contrast_order A character vector specifying the desired order of shift names
#'                       (e.g., c("convert", "religious", "done", "secular", "null"))
#' @param contrast_type Type of contrasts to compute: "pairwise" or "null". Default is "pairwise".
#' @param contrast_scale Scale for contrasts: "additive", "rr", or "or". Default is "additive".
#' @param specific_contrasts Optional list of specific contrasts to compute. Each element should be
#'                          a character vector of length 2 with shift names (e.g., list(c("convert", "religious")))
#'
#' @return A modified version of the lmtp_output with reordered contrasts and regenerated tables
#'
#' @examples
#' \dontrun{
#' # reorder all contrasts with new preference order
#' result_reordered <- margot_resort_contrast_lmtp(
#'   models_6_cp,
#'   contrast_order = c("convert", "religious", "done", "secular", "null")
#' )
#'
#' # compute specific contrasts only
#' result_specific <- margot_resort_contrast_lmtp(
#'   models_6_cp,
#'   specific_contrasts = list(
#'     c("convert", "religious"),
#'     c("done", "religious"),
#'     c("secular", "religious")
#'   )
#' )
#' }
#'
#' @import cli
#' @import lmtp
#' @import margot
#' @importFrom stringr str_detect str_remove
#'
#' @export
margot_resort_contrast_lmtp <- function(
    lmtp_output,
    contrast_order = NULL,
    contrast_type = c("pairwise", "null"),
    contrast_scale = c("additive", "rr", "or"),
    specific_contrasts = NULL) {

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  # check that we have models to work with
  if (is.null(lmtp_output$models)) {
    cli::cli_alert_danger("No models found in the lmtp_output")
    stop("No models found in the lmtp_output")
  }

  # get outcome names
  outcome_vars <- names(lmtp_output$models)

  # initialise output structure
  output <- lmtp_output
  output$contrasts <- list()
  output$individual_tables <- list()
  output$combined_tables <- list()

  cli::cli_h1("Recomputing LMTP Contrasts")

  # process each outcome
  for (outcome in outcome_vars) {
    cli::cli_h2("Processing outcome: {.val {outcome}}")

    # get models for this outcome
    outcome_models <- lmtp_output$models[[outcome]]

    # extract shift names from model names
    shift_names <- gsub(paste0("^", outcome, "_"), "", names(outcome_models))

    # determine which contrasts to compute
    if (!is.null(specific_contrasts)) {
      # use specific contrasts provided
      contrasts_to_compute <- specific_contrasts
    } else if (!is.null(contrast_order)) {
      # reorder shifts according to specified order
      ordered_shifts <- contrast_order[contrast_order %in% shift_names]

      if (contrast_type == "null") {
        # compute contrasts against null
        if (!"null" %in% ordered_shifts) {
          cli::cli_alert_warning("No null model found for {.val {outcome}}")
          next
        }
        contrasts_to_compute <- lapply(
          ordered_shifts[ordered_shifts != "null"],
          function(shift) c(shift, "null")
        )
      } else {
        # compute pairwise contrasts in the specified order
        contrasts_to_compute <- list()
        for (i in 1:(length(ordered_shifts) - 1)) {
          for (j in (i + 1):length(ordered_shifts)) {
            contrasts_to_compute <- append(
              contrasts_to_compute,
              list(c(ordered_shifts[i], ordered_shifts[j]))
            )
          }
        }
      }
    } else {
      # default: use original shift order but allow null contrasts
      if (contrast_type == "null") {
        if (!"null" %in% shift_names) {
          cli::cli_alert_warning("No null model found for {.val {outcome}}")
          next
        }
        contrasts_to_compute <- lapply(
          shift_names[shift_names != "null"],
          function(shift) c(shift, "null")
        )
      } else {
        # compute all pairwise contrasts
        contrasts_to_compute <- list()
        for (i in 1:(length(shift_names) - 1)) {
          for (j in (i + 1):length(shift_names)) {
            contrasts_to_compute <- append(
              contrasts_to_compute,
              list(c(shift_names[i], shift_names[j]))
            )
          }
        }
      }
    }

    # compute contrasts
    cli::cli_h3("Computing contrasts")
    contrasts <- list()
    tables <- list()

    for (contrast_pair in contrasts_to_compute) {
      shift1 <- contrast_pair[1]
      shift2 <- contrast_pair[2]

      # get model names
      model1_name <- paste0(outcome, "_", shift1)
      model2_name <- paste0(outcome, "_", shift2)

      # check models exist
      if (!model1_name %in% names(outcome_models)) {
        cli::cli_alert_warning("Model {.val {model1_name}} not found")
        next
      }
      if (!model2_name %in% names(outcome_models)) {
        cli::cli_alert_warning("Model {.val {model2_name}} not found")
        next
      }

      # get the models
      model1 <- outcome_models[[model1_name]]
      model2 <- outcome_models[[model2_name]]

      # compute contrast (model1 vs model2)
      contrast_name <- paste0(model1_name, "_vs_", model2_name)

      tryCatch(
        {
          contrast <- lmtp::lmtp_contrast(
            model1,
            ref = model2,
            type = contrast_scale
          )
          contrasts[[contrast_name]] <- contrast
          cli::cli_alert_success("Computed contrast: {.val {contrast_name}}")

          # create evaluation table
          scale <- if (contrast_scale == "additive") "RD" else "RR"
          table <- margot::margot_lmtp_evalue(
            contrast,
            scale = scale,
            new_name = outcome
          )
          tables[[contrast_name]] <- table
        },
        error = function(e) {
          cli::cli_alert_danger("Error in contrast {.val {contrast_name}}: {e$message}")
        }
      )
    }

    output$contrasts[[outcome]] <- contrasts
    output$individual_tables[[outcome]] <- tables
  }

  # create combined tables across outcomes
  cli::cli_h2("Creating combined tables")

  # get all unique contrast patterns across outcomes
  # we need to extract the shift pair pattern from each contrast name
  all_shift_pairs <- list()

  for (outcome in names(output$individual_tables)) {
    for (contrast_name in names(output$individual_tables[[outcome]])) {
      # extract the two shift names from the full contrast name
      # e.g., "t5_belong_z_convert_vs_t5_belong_z_religious" -> "convert" and "religious"
      # first, remove the outcome prefix
      temp <- gsub(paste0("^", outcome, "_"), "", contrast_name)
      # now we have "convert_vs_t5_belong_z_religious"
      # split by _vs_ and clean up
      parts <- strsplit(temp, "_vs_")[[1]]
      if (length(parts) == 2) {
        shift1 <- parts[1]
        # remove outcome from second part
        shift2 <- gsub(paste0("^", outcome, "_"), "", parts[2])
        shift_pair <- paste0(shift1, "_vs_", shift2)
        all_shift_pairs[[shift_pair]] <- TRUE
      }
    }
  }

  shift_pairs <- names(all_shift_pairs)

  for (shift_pair in shift_pairs) {
    contrast_name <- paste0("combined_outcomes_", shift_pair)

    # parse the shift pair to get individual shifts
    shifts <- strsplit(shift_pair, "_vs_")[[1]]
    if (length(shifts) != 2) next

    shift1 <- shifts[1]
    shift2 <- shifts[2]

    # collect tables for this shift pair across all outcomes
    # the table name format is: outcome_shift1_vs_outcome_shift2
    combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
      table_name <- paste0(outcome, "_", shift1, "_vs_", outcome, "_", shift2)
      if (!is.null(output$individual_tables[[outcome]][[table_name]])) {
        output$individual_tables[[outcome]][[table_name]]
      } else {
        NULL
      }
    }))

    if (!is.null(combined_table) && nrow(combined_table) > 0) {
      output$combined_tables[[contrast_name]] <- combined_table
      cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
    }
  }

  cli::cli_alert_success("Contrast recomputation complete")

  return(output)
}

#' Helper function to view available models and contrasts
#'
#' @param lmtp_output Output from margot_lmtp() or margot_resort_contrast_lmtp()
#' @param show What to show: "models", "contrasts", "tables", or "all"
#'
#' @import cli
#'
#' @export
margot_view_lmtp_structure <- function(lmtp_output, show = "all") {

  if (show %in% c("models", "all") && !is.null(lmtp_output$models)) {
    cli::cli_h2("Available Models")
    for (outcome in names(lmtp_output$models)) {
      cli::cli_h3(outcome)
      shifts <- gsub(paste0("^", outcome, "_"), "", names(lmtp_output$models[[outcome]]))
      cli::cli_text("Shifts: {.val {shifts}}")
    }
  }

  if (show %in% c("contrasts", "all") && !is.null(lmtp_output$contrasts)) {
    cli::cli_h2("Computed Contrasts")
    for (outcome in names(lmtp_output$contrasts)) {
      cli::cli_h3(outcome)
      print(names(lmtp_output$contrasts[[outcome]]))
    }
  }

  if (show %in% c("tables", "all")) {
    if (!is.null(lmtp_output$combined_tables)) {
      cli::cli_h2("Combined Tables")
      print(names(lmtp_output$combined_tables))
    }

    if (!is.null(lmtp_output$individual_tables)) {
      cli::cli_h2("Individual Tables")
      for (outcome in names(lmtp_output$individual_tables)) {
        cli::cli_h3(outcome)
        print(names(lmtp_output$individual_tables[[outcome]]))
      }
    }
  }
}
