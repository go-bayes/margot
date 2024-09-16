#' Batch Process LMTP Models
#'
#' This function runs multiple Longitudinal Modified Treatment Policy (LMTP) models for specified outcome variables,
#' calculates contrasts, creates evaluation tables, and optionally saves the complete output.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param trt A character string specifying the treatment variable.
#' @param shift_functions A list of shift functions to be applied. Each function should take `data` and `trt` as arguments.
#' @param include_null_shift Logical, whether to include a null shift. Default is TRUE.
#' @param lmtp_model_type The LMTP model function to use. Default is lmtp_tmle.
#' @param contrast_type Type of contrasts to compute: "pairwise" or "null". Default is "pairwise".
#' @param contrast_scale Scale for contrasts: "additive", "rr", or "or". Default is "additive".
#' @param lmtp_defaults A list of default parameters for the LMTP models.
#' @param n_cores Number of cores to use for parallel processing. Default is detectCores() - 1.
#' @param save_output Logical, whether to save the complete output. Default is FALSE.
#' @param save_path The directory path to save the output. Default is "push_mods" in the current working directory.
#' @param base_filename The base filename for saving the output. Default is "lmtp_output".
#' @param use_timestamp Logical, whether to include a timestamp in the filename. Default is FALSE.
#' @param prefix Optional prefix to add to the saved output filename. Default is NULL.
#'
#' @return A list containing:
#'   \item{models}{A list of all LMTP models for each outcome and shift function.}
#'   \item{contrasts}{A list of contrasts computed for each outcome.}
#'   \item{individual_tables}{A list of individual tables for each contrast and outcome.}
#'   \item{combined_tables}{A list of combined tables for each contrast type across all outcomes.}
#'
#' @examples
#' \dontrun{
#' # Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders
#'
#' # Define shift functions
#' gain_function <- function(data, trt) {
#'   data[[trt]] + 1
#' }
#'
#' loss_function <- function(data, trt) {
#'   pmax(data[[trt]] - 1, 0)
#' }
#'
#' # Run LMTP analysis
#' result <- margot_lmtp(
#'   data = my_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   trt = "treatment",
#'   shift_functions = list(gain = gain_function, loss = loss_function),
#'   lmtp_defaults = list(baseline = c("confounder1", "confounder2"),
#'                        time_vary = c("time_var1", "time_var2"),
#'                        outcome_type = "continuous"),
#'   save_output = TRUE,
#'   save_path = here::here("output", "lmtp_results"),
#'   prefix = "my_study"
#' )
#' }
#'
#' @import lmtp
#' @import margot
#' @import parallel
#' @import doParallel
#' @import SuperLearner
#' @import cli
#' @import progressr
#'
#' @export
margot_lmtp <- function(
    data,
    outcome_vars,
    trt,
    shift_functions = list(),
    include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_tmle,
    contrast_type = c("pairwise", "null"),
    contrast_scale = c("additive", "rr", "or"),
    lmtp_defaults = list(),
    n_cores = parallel::detectCores() - 1,
    save_output = FALSE,
    save_path = here::here("push_mods"),
    base_filename = "lmtp_output",
    use_timestamp = FALSE,
    prefix = NULL
) {
  # Load required packages
  library(cli)
  library(progressr)

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  # define shift_names
  shift_names <- names(shift_functions)
  if (include_null_shift && !("null" %in% shift_names)) {
    shift_names <- c(shift_names, "null")
  }

  # ensure outcome_vars is always a character vector
  if (!is.character(outcome_vars)) {
    cli::cli_alert_danger("outcome_vars must be a character vector")
    stop("outcome_vars must be a character vector")
  }

  # check if SuperLearner library is specified, if not, default to "SL.ranger"
  if (is.null(lmtp_defaults$learners_trt)) {
    lmtp_defaults$learners_trt <- "SL.ranger"
  }
  if (is.null(lmtp_defaults$learners_outcome)) {
    lmtp_defaults$learners_outcome <- "SL.ranger"
  }

  # parallel processing
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  # add null shift if requested
  if (include_null_shift) {
    shift_functions <- c(shift_functions, list(null = NULL))
  }

  # Initialise results lists
  all_models <- list()
  all_contrasts <- list()
  all_tables <- list()

  # Set up progressr
  handlers(global = TRUE)
  p <- progressor(steps = length(outcome_vars) * length(shift_functions))

  # CLI setup
  cli::cli_h1("Starting LMTP Analysis")

  # run models for each outcome and shift function
  for (outcome in outcome_vars) {
    cli::cli_h2("Processing outcome: {.val {outcome}}")
    outcome_models <- list()

    for (shift_name in names(shift_functions)) {
      shift <- shift_functions[[shift_name]]
      cli::cli_h3("Running model for shift: {.val {shift_name}}")

      tryCatch({
        # set up LMTP arguments
        lmtp_args <- c(
          list(data = data, trt = trt, outcome = outcome, shift = shift),
          lmtp_defaults
        )

        # run LMTP model with progress reporting
        with_progress({
          model <- do.call(lmtp_model_type, lmtp_args)
        })

        # Store model
        model_name <- paste0(outcome, "_", shift_name)
        outcome_models[[model_name]] <- model

        cli::cli_alert_success("Completed model for {.val {outcome}} with shift {.val {shift_name}}")
        p(sprintf("Completed %s - %s", outcome, shift_name))
      }, error = function(e) {
        cli::cli_alert_danger("Error in model for {.val {outcome}} with shift {.val {shift_name}}: {e$message}")
        p(sprintf("Error in %s - %s", outcome, shift_name))
      })
    }

    all_models[[outcome]] <- outcome_models

    # contrasts
    # contrasts
    cli::cli_h3("Calculating contrasts")
    contrasts <- list()
    model_names <- names(outcome_models)

    if (contrast_type == "null") {
      null_model <- outcome_models[[grep("null", model_names, value = TRUE)]]
      for (model_name in model_names) {
        if (!grepl("null", model_name)) {
          contrast_name <- paste0(model_name, "_vs_null")
          tryCatch({
            contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
            contrasts[[contrast_name]] <- contrast
            cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
          }, error = function(e) {
            cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
          })
        }
      }
    } else {
      # Original pairwise contrast code
      for (i in 1:(length(model_names) - 1)) {
        for (j in (i + 1):length(model_names)) {
          contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
          tryCatch({
            contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
            contrasts[[contrast_name]] <- contrast
            cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
          }, error = function(e) {
            cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
          })
        }
      }
    }

    all_contrasts[[outcome]] <- contrasts

    # tables
    cli::cli_h3("Creating tables")
    tables <- list()
    for (contrast_name in names(contrasts)) {
      tryCatch({
        scale <- if(contrast_scale == "additive") "RD" else "RR"
        table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
        tables[[contrast_name]] <- table
        cli::cli_alert_success("Created table for contrast: {.val {contrast_name}}")
      }, error = function(e) {
        cli::cli_alert_danger("Error in creating table for contrast {.val {contrast_name}}, outcome {.val {outcome}}: {e$message}")
      })
    }

    all_tables[[outcome]] <- tables
  }

  # combine tables across outcomes
  cli::cli_h2("Combining tables across outcomes")
  combined_tables <- list()

  if (contrast_type == "null") {
    for (shift_name in shift_names) {
      if (shift_name != "null") {
        contrast_name <- paste0("combined_outcomes_", shift_name, "_vs_null")
        combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
          table_name <- paste0(outcome, "_", shift_name, "_vs_null")
          if (!is.null(all_tables[[outcome]][[table_name]])) {
            all_tables[[outcome]][[table_name]]
          } else {
            cli::cli_alert_warning(paste("Table not found for:", table_name))
            NULL
          }
        }))
        if (!is.null(combined_table) && nrow(combined_table) > 0) {
          combined_tables[[contrast_name]] <- combined_table
          cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
        } else {
          cli::cli_alert_warning(paste("No data for combined table:", contrast_name))
        }
      }
    }
  } else {
    for (i in 1:(length(shift_names) - 1)) {
      for (j in (i + 1):length(shift_names)) {
        contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
        combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
          table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
          if (!is.null(all_tables[[outcome]][[table_name]])) {
            all_tables[[outcome]][[table_name]]
          } else {
            cli::cli_alert_warning(paste("Table not found for:", table_name))
            NULL
          }
        }))
        if (!is.null(combined_table) && nrow(combined_table) > 0) {
          combined_tables[[contrast_name]] <- combined_table
          cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
        } else {
          cli::cli_alert_warning(paste("No data for combined table:", contrast_name))
        }
      }
    }
  }

  # Prepare the complete output
  complete_output <- list(
    models = all_models,
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  )

  # Save complete output if save_output is TRUE
  if (save_output) {
    cli::cli_alert_info("Saving complete output...")
    tryCatch({
      if (use_timestamp) {
        output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
                                  base_filename, "_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"))
      } else {
        output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
                                  base_filename)
      }

      margot::here_save_qs(
        obj = complete_output,
        name = output_filename,
        dir_path = save_path,
        preset = "high",
        nthreads = 1
      )
      cli::cli_alert_success("Complete output saved successfully")
    }, error = function(e) {
      cli::cli_alert_danger(paste("Failed to save complete output:", e$message))
    })
  }

  cli::cli_alert_success("Analysis complete \U0001F44D")

  return(complete_output)
}
# margot_lmtp <- function(
#     data,
#     outcome_vars,
#     trt,
#     shift_functions = list(),
#     include_null_shift = TRUE,
#     lmtp_model_type = lmtp::lmtp_tmle,
#     contrast_type = c("pairwise", "null"),
#     contrast_scale = c("additive", "rr", "or"),
#     lmtp_defaults = list(),
#     n_cores = parallel::detectCores() - 1
# ) {
#   # Load required packages
#   library(cli)
#   library(progressr)
#
#   contrast_type <- match.arg(contrast_type)
#   contrast_scale <- match.arg(contrast_scale)
#
#   # ensure outcome_vars is always a character vector
#   if (!is.character(outcome_vars)) {
#     cli::cli_alert_danger("outcome_vars must be a character vector")
#     stop("outcome_vars must be a character vector")
#   }
#
#   # check if SuperLearner library is specified, if not, default to "SL.ranger"
#   if (is.null(lmtp_defaults$learners_trt)) {
#     lmtp_defaults$learners_trt <- "SL.ranger"
#   }
#   if (is.null(lmtp_defaults$learners_outcome)) {
#     lmtp_defaults$learners_outcome <- "SL.ranger"
#   }
#
#   # parallel processing
#   cl <- parallel::makeCluster(n_cores)
#   doParallel::registerDoParallel(cl)
#   on.exit(parallel::stopCluster(cl))
#
#   # add null shift if requested
#   if (include_null_shift) {
#     shift_functions <- c(shift_functions, list(null = NULL))
#   }
#
#   # Initialise results lists
#   all_models <- list()
#   all_contrasts <- list()
#   all_tables <- list()
#
#   # Set up progressr
#   handlers(global = TRUE)
#   p <- progressor(steps = length(outcome_vars) * length(shift_functions))
#
#   # CLI setup
#   cli::cli_h1("Starting LMTP Analysis")
#
#   # run models for each outcome and shift function
#   for (outcome in outcome_vars) {
#     cli::cli_h2("Processing outcome: {.val {outcome}}")
#     outcome_models <- list()
#
#     for (shift_name in names(shift_functions)) {
#       shift <- shift_functions[[shift_name]]
#       cli::cli_h3("Running model for shift: {.val {shift_name}}")
#
#       tryCatch({
#         # set up LMTP arguments
#         lmtp_args <- c(
#           list(data = data, trt = trt, outcome = outcome, shift = shift),
#           lmtp_defaults
#         )
#
#         # run LMTP model with progress reporting
#         with_progress({
#           model <- do.call(lmtp_model_type, lmtp_args)
#         })
#
#         # Store model
#         model_name <- paste0(outcome, "_", shift_name)
#         outcome_models[[model_name]] <- model
#
#         cli::cli_alert_success("Completed model for {.val {outcome}} with shift {.val {shift_name}}")
#         p(sprintf("Completed %s - %s", outcome, shift_name))
#       }, error = function(e) {
#         cli::cli_alert_danger("Error in model for {.val {outcome}} with shift {.val {shift_name}}: {e$message}")
#         p(sprintf("Error in %s - %s", outcome, shift_name))
#       })
#     }
#
#     all_models[[outcome]] <- outcome_models
#
#     # contrasts
#     cli::cli_h3("Calculating contrasts")
#     contrasts <- list()
#     model_names <- names(outcome_models)
#
#     if (contrast_type == "pairwise") {
#       for (i in 1:(length(model_names) - 1)) {
#         for (j in (i + 1):length(model_names)) {
#           contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#             cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
#           }, error = function(e) {
#             cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
#           })
#         }
#       }
#     } else if (contrast_type == "null") {
#       null_model <- outcome_models[[grep("null", model_names)]]
#       for (model_name in model_names) {
#         if (!grepl("null", model_name)) {
#           contrast_name <- paste0(model_name, "_vs_null")
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#             cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
#           }, error = function(e) {
#             cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
#           })
#         }
#       }
#     }
#
#     all_contrasts[[outcome]] <- contrasts
#
#     # tables
#     cli::cli_h3("Creating tables")
#     tables <- list()
#     for (contrast_name in names(contrasts)) {
#       tryCatch({
#         scale <- if(contrast_scale == "additive") "RD" else "RR"
#         table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
#         tables[[contrast_name]] <- table
#         cli::cli_alert_success("Created table for contrast: {.val {contrast_name}}")
#       }, error = function(e) {
#         cli::cli_alert_danger("Error in creating table for contrast {.val {contrast_name}}, outcome {.val {outcome}}: {e$message}")
#       })
#     }
#
#     all_tables[[outcome]] <- tables
#   }
#
#   # combine tables across outcomes
#   cli::cli_h2("Combining tables across outcomes")
#   combined_tables <- list()
#   shift_names <- names(shift_functions)
#
#   # create combined tables for each contrast type
#   for (i in 1:(length(shift_names) - 1)) {
#     for (j in (i + 1):length(shift_names)) {
#       contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
#       combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
#         table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
#         all_tables[[outcome]][[table_name]]
#       }))
#       combined_tables[[contrast_name]] <- combined_table
#       cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
#     }
#   }
#
#   cli::cli_alert_success("Analysis complete \U0001F44D")
#
#   return(list(
#     models = all_models,
#     contrasts = all_contrasts,
#     individual_tables = all_tables,
#     combined_tables = combined_tables
#   ))
# }
# no cli
# margot_lmtp <- function(
    #     data,
#     outcome_vars,
#     trt,
#     shift_functions = list(),
#     include_null_shift = TRUE,
#     lmtp_model_type = lmtp::lmtp_tmle,
#     contrast_type = c("pairwise", "null"),
#     contrast_scale = c("additive", "rr", "or"),
#     lmtp_defaults = list(),
#     n_cores = parallel::detectCores() - 1
# ) {
#   # Load required packages
#   library(cli)
#   library(progressr)
#
#   contrast_type <- match.arg(contrast_type)
#   contrast_scale <- match.arg(contrast_scale)
#
#   # ensure outcome_vars is always a character vector
#   if (!is.character(outcome_vars)) {
#     cli::cli_alert_danger("outcome_vars must be a character vector")
#     stop("outcome_vars must be a character vector")
#   }
#
#   # check if SuperLearner library is specified, if not, default to "SL.ranger"
#   if (is.null(lmtp_defaults$learners_trt)) {
#     lmtp_defaults$learners_trt <- "SL.ranger"
#   }
#   if (is.null(lmtp_defaults$learners_outcome)) {
#     lmtp_defaults$learners_outcome <- "SL.ranger"
#   }
#
#   # parallel processing
#   cl <- parallel::makeCluster(n_cores)
#   doParallel::registerDoParallel(cl)
#   on.exit(parallel::stopCluster(cl))
#
#   # add null shift if requested
#   if (include_null_shift) {
#     shift_functions <- c(shift_functions, list(null = NULL))
#   }
#
#   # Initialise results lists
#   all_models <- list()
#   all_contrasts <- list()
#   all_tables <- list()
#
#   # Set up progressr
#   handlers(global = TRUE)
#   p <- progressor(steps = length(outcome_vars) * length(shift_functions))
#
#   # CLI setup
#   cli::cli_h1("Starting LMTP Analysis")
#
#   # run models for each outcome and shift function
#   for (outcome in outcome_vars) {
#     cli::cli_h2("Processing outcome: {.val {outcome}}")
#     outcome_models <- list()
#
#     for (shift_name in names(shift_functions)) {
#       shift <- shift_functions[[shift_name]]
#       cli::cli_h3("Running model for shift: {.val {shift_name}}")
#
#       tryCatch({
#         # set up LMTP arguments
#         lmtp_args <- c(
#           list(data = data, trt = trt, outcome = outcome, shift = shift),
#           lmtp_defaults
#         )
#
#         # run LMTP model with progress reporting
#         with_progress({
#           model <- do.call(lmtp_model_type, lmtp_args)
#         })
#
#         # Store model
#         model_name <- paste0(outcome, "_", shift_name)
#         outcome_models[[model_name]] <- model
#
#         cli::cli_alert_success("Completed model for {.val {outcome}} with shift {.val {shift_name}}")
#         p(sprintf("Completed %s - %s", outcome, shift_name))
#       }, error = function(e) {
#         cli::cli_alert_danger("Error in model for {.val {outcome}} with shift {.val {shift_name}}: {e$message}")
#         p(sprintf("Error in %s - %s", outcome, shift_name))
#       })
#     }
#
#     all_models[[outcome]] <- outcome_models
#
#     # contrasts
#     cli::cli_h3("Calculating contrasts")
#     contrasts <- list()
#     model_names <- names(outcome_models)
#
#     if (contrast_type == "pairwise") {
#       for (i in 1:(length(model_names) - 1)) {
#         for (j in (i + 1):length(model_names)) {
#           contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#             cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
#           }, error = function(e) {
#             cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
#           })
#         }
#       }
#     } else if (contrast_type == "null") {
#       null_model <- outcome_models[[grep("null", model_names)]]
#       for (model_name in model_names) {
#         if (!grepl("null", model_name)) {
#           contrast_name <- paste0(model_name, "_vs_null")
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#             cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
#           }, error = function(e) {
#             cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
#           })
#         }
#       }
#     }
#
#     all_contrasts[[outcome]] <- contrasts
#
#     # tables
#     cli::cli_h3("Creating tables")
#     tables <- list()
#     for (contrast_name in names(contrasts)) {
#       tryCatch({
#         scale <- if(contrast_scale == "additive") "RD" else "RR"
#         table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
#         tables[[contrast_name]] <- table
#         cli::cli_alert_success("Created table for contrast: {.val {contrast_name}}")
#       }, error = function(e) {
#         cli::cli_alert_danger("Error in creating table for contrast {.val {contrast_name}}, outcome {.val {outcome}}: {e$message}")
#       })
#     }
#
#     all_tables[[outcome]] <- tables
#   }
#
#   # combine tables across outcomes
#   cli::cli_h2("Combining tables across outcomes")
#   combined_tables <- list()
#   shift_names <- names(shift_functions)
#
#   # create combined tables for each contrast type
#   for (i in 1:(length(shift_names) - 1)) {
#     for (j in (i + 1):length(shift_names)) {
#       contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
#       combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
#         table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
#         all_tables[[outcome]][[table_name]]
#       }))
#       combined_tables[[contrast_name]] <- combined_table
#       cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
#     }
#   }
#
#   cli::cli_alert_success("Analysis complete!")
#
#   return(list(
#     models = all_models,
#     contrasts = all_contrasts,
#     individual_tables = all_tables,
#     combined_tables = combined_tables
#   ))
# }
# margot_lmtp <- function(
#     data,
#     outcome_vars,
#     trt,
#     shift_functions = list(),
#     include_null_shift = TRUE,
#     lmtp_model_type = lmtp::lmtp_tmle,
#     contrast_type = c("pairwise", "null"),
#     contrast_scale = c("additive", "rr", "or"),
#     lmtp_defaults = list(),
#     n_cores = parallel::detectCores() - 1
# ) {
#   contrast_type <- match.arg(contrast_type)
#   contrast_scale <- match.arg(contrast_scale)
#
#   # ensure outcome_vars is always a character vector
#   if (!is.character(outcome_vars)) {
#     stop("outcome_vars must be a character vector")
#   }
#
#   # check if SuperLearner library is specified, if not, default to "SL.ranger"
#   if (is.null(lmtp_defaults$learners_trt)) {
#     lmtp_defaults$learners_trt <- "SL.ranger"
#   }
#   if (is.null(lmtp_defaults$learners_outcome)) {
#     lmtp_defaults$learners_outcome <- "SL.ranger"
#   }
#
#   #  parallel processing
#   cl <- parallel::makeCluster(n_cores)
#   doParallel::registerDoParallel(cl)
#   on.exit(parallel::stopCluster(cl))
#
#   # add null shift if requested
#   if (include_null_shift) {
#     shift_functions <- c(shift_functions, list(null = NULL))
#   }
#
#   # Initialise results lists
#   all_models <- list()
#   all_contrasts <- list()
#   all_tables <- list()
#
#   # run models for each outcome and shift function
#   for (outcome in outcome_vars) {
#     outcome_models <- list()
#
#     for (shift_name in names(shift_functions)) {
#       shift <- shift_functions[[shift_name]]
#
#       tryCatch({
#         # set up LMTP arguments
#         lmtp_args <- c(
#           list(data = data, trt = trt, outcome = outcome, shift = shift),
#           lmtp_defaults
#         )
#
#         # run LMTP model
#         model <- do.call(lmtp_model_type, lmtp_args)
#
#         # Store model
#         model_name <- paste0(outcome, "_", shift_name)
#         outcome_models[[model_name]] <- model
#
#         message(sprintf("Completed model for %s with shift %s", outcome, shift_name))
#       }, error = function(e) {
#         warning(sprintf("Error in model for %s with shift %s: %s", outcome, shift_name, e$message))
#       })
#     }
#
#     all_models[[outcome]] <- outcome_models
#
#     # contrasts
#     contrasts <- list()
#     model_names <- names(outcome_models)
#
#     if (contrast_type == "pairwise") {
#       for (i in 1:(length(model_names) - 1)) {
#         for (j in (i + 1):length(model_names)) {
#           contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#           }, error = function(e) {
#             warning(sprintf("Error in contrast %s for %s: %s", contrast_name, outcome, e$message))
#           })
#         }
#       }
#     } else if (contrast_type == "null") {
#       null_model <- outcome_models[[grep("null", model_names)]]
#       for (model_name in model_names) {
#         if (!grepl("null", model_name)) {
#           contrast_name <- paste0(model_name, "_vs_null")
#           tryCatch({
#             contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
#             contrasts[[contrast_name]] <- contrast
#           }, error = function(e) {
#             warning(sprintf("Error in contrast %s for %s: %s", contrast_name, outcome, e$message))
#           })
#         }
#       }
#     }
#
#     all_contrasts[[outcome]] <- contrasts
#
#     # tables
#     tables <- list()
#     for (contrast_name in names(contrasts)) {
#       tryCatch({
#         scale <- if(contrast_scale == "additive") "RD" else "RR"
#         table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
#         tables[[contrast_name]] <- table
#       }, error = function(e) {
#         warning(sprintf("Error in creating table for contrast %s, outcome %s: %s", contrast_name, outcome, e$message))
#       })
#     }
#
#     all_tables[[outcome]] <- tables
#   }
#
#   # combine tables across outcomes
#   combined_tables <- list()
#   shift_names <- names(shift_functions)
#
#   # create combined tables for each contrast type
#   for (i in 1:(length(shift_names) - 1)) {
#     for (j in (i + 1):length(shift_names)) {
#       contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
#       combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
#         table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
#         all_tables[[outcome]][[table_name]]
#       }))
#       combined_tables[[contrast_name]] <- combined_table
#     }
#   }
#
#   return(list(
#     models = all_models,
#     contrasts = all_contrasts,
#     individual_tables = all_tables,
#     combined_tables = combined_tables
#   ))
# }
