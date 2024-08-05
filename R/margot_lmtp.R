#' Batch Process LMTP Models
#'
#' This function runs multiple Longitudinal Modified Treatment Policy (LMTP) models for specified outcome variables,
#' calculates contrasts, and creates evaluation tables.
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
#'
#' @return A list containing:
#'   \item{models}{A list of all LMTP models for each outcome and shift function.}
#'   \item{contrasts}{A list of contrasts computed for each outcome.}
#'   \item{individual_tables}{A list of individual tables for each contrast and outcome.}
#'   \item{combined_tables}{A list of combined tables for each contrast type across all outcomes.}
#'
#' @examples
#' \dontrun{
#' fit <- margot_lmtp(
#'   data = my_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   trt = "treatment",
#'   shift_functions = list(gain = gain_function, loss = loss_function),
#'   lmtp_defaults = list(baseline = c("confounder1", "confounder2"),
#'                        mtp = TRUE,
#'                        folds = 10,
#'                        outcome_type = "continuous")
#' )
#' }
#'
#' @import lmtp
#' @import margot
#' @import parallel
#' @import doParallel
#' @import SuperLearner
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
    n_cores = parallel::detectCores() - 1
) {
  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  # ensure outcome_vars is always a character vector
  if (!is.character(outcome_vars)) {
    stop("outcome_vars must be a character vector")
  }

  # check if SuperLearner library is specified, if not, default to "SL.ranger"
  if (is.null(lmtp_defaults$learners_trt)) {
    lmtp_defaults$learners_trt <- "SL.ranger"
  }
  if (is.null(lmtp_defaults$learners_outcome)) {
    lmtp_defaults$learners_outcome <- "SL.ranger"
  }

  #  parallel processing
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

  # run models for each outcome and shift function
  for (outcome in outcome_vars) {
    outcome_models <- list()

    for (shift_name in names(shift_functions)) {
      shift <- shift_functions[[shift_name]]

      tryCatch({
        # set up LMTP arguments
        lmtp_args <- c(
          list(data = data, trt = trt, outcome = outcome, shift = shift),
          lmtp_defaults
        )

        # run LMTP model
        model <- do.call(lmtp_model_type, lmtp_args)

        # Store model
        model_name <- paste0(outcome, "_", shift_name)
        outcome_models[[model_name]] <- model

        message(sprintf("Completed model for %s with shift %s", outcome, shift_name))
      }, error = function(e) {
        warning(sprintf("Error in model for %s with shift %s: %s", outcome, shift_name, e$message))
      })
    }

    all_models[[outcome]] <- outcome_models

    # contrasts
    contrasts <- list()
    model_names <- names(outcome_models)

    if (contrast_type == "pairwise") {
      for (i in 1:(length(model_names) - 1)) {
        for (j in (i + 1):length(model_names)) {
          contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
          tryCatch({
            contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
            contrasts[[contrast_name]] <- contrast
          }, error = function(e) {
            warning(sprintf("Error in contrast %s for %s: %s", contrast_name, outcome, e$message))
          })
        }
      }
    } else if (contrast_type == "null") {
      null_model <- outcome_models[[grep("null", model_names)]]
      for (model_name in model_names) {
        if (!grepl("null", model_name)) {
          contrast_name <- paste0(model_name, "_vs_null")
          tryCatch({
            contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
            contrasts[[contrast_name]] <- contrast
          }, error = function(e) {
            warning(sprintf("Error in contrast %s for %s: %s", contrast_name, outcome, e$message))
          })
        }
      }
    }

    all_contrasts[[outcome]] <- contrasts

    # tables
    tables <- list()
    for (contrast_name in names(contrasts)) {
      tryCatch({
        scale <- if(contrast_scale == "additive") "RD" else "RR"
        table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
        tables[[contrast_name]] <- table
      }, error = function(e) {
        warning(sprintf("Error in creating table for contrast %s, outcome %s: %s", contrast_name, outcome, e$message))
      })
    }

    all_tables[[outcome]] <- tables
  }

  # combine tables across outcomes
  combined_tables <- list()
  shift_names <- names(shift_functions)

  # create combined tables for each contrast type
  for (i in 1:(length(shift_names) - 1)) {
    for (j in (i + 1):length(shift_names)) {
      contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
      combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
        table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
        all_tables[[outcome]][[table_name]]
      }))
      combined_tables[[contrast_name]] <- combined_table
    }
  }

  return(list(
    models = all_models,
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  ))
}
