#' Create Propensity Score Model and Associated Plots
#'
#' This function creates a propensity score model using the specified exposure variable
#' and baseline covariates. It also generates associated plots and diagnostics.
#'
#' @param df_propensity A data frame containing the variables for the propensity score model.
#' @param exposure_variable A character string specifying the name of the exposure variable in df_propensity.
#' @param baseline_vars A character vector specifying the names of the baseline covariates in df_propensity.
#' @param weights_var_name A character string specifying the name of the weights variable in df_propensity.
#' @param estimand A character string specifying the estimand. Default is "ATE" (Average Treatment Effect).
#' @param method A character string specifying the method for propensity score estimation. Default is "ebal".
#' @param focal For binary treatments, a value of the treatment to be considered "focal" (i.e., the intervention). Default is NULL.
#' @param love_plot_options A list of options to be passed to cobalt::love.plot(). Default is an empty list.
#' @param bal_tab_options A list of options to be passed to cobalt::bal.tab(). Default is an empty list.
#' @param verbose A logical value indicating whether to print progress messages. Default is TRUE.
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item match_propensity: The propensity score model object.
#'   \item love_plot: A love plot object created by cobalt::love.plot().
#'   \item summary: A summary of the propensity score model.
#'   \item summary_plot: A plot of the propensity score model summary.
#'   \item balance_table: A balance table created by cobalt::bal.tab().
#'   \item diagnostics: A list of additional diagnostic information.
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming df_propensity is your dataset with appropriate variables
#' results <- margot_propensity_model_and_plots(
#'   df_propensity = df_propensity,
#'   exposure_variable = "treatment",
#'   baseline_vars = c("age", "sex", "bmi"),
#'   weights_var_name = "sample_weights",
#'   love_plot_options = list(
#'     thresholds = c(m = .05),
#'     size = 4
#'   ),
#'   bal_tab_options = list(
#'     thresholds = c(m = .1, v = 2.5)
#'   ),
#'   verbose = TRUE
#' )
#'
#' # Access the results
#' print(results$summary)
#' plot(results$love_plot)
#' print(results$balance_table)
#' }
#'
#' @import WeightIt
#' @import MatchIt
#' @import cobalt
#' @import cli
#'
#' @export

margot_propensity_model_and_plots <- function(
    df_propensity,
    exposure_variable,
    baseline_vars,
    weights_var_name,
    estimand = "ATE",
    method = "ebal",
    focal = NULL,
    love_plot_options = list(),
    bal_tab_options = list(),
    verbose = TRUE
) {

  # Function to safely load or install packages
  load_or_install_package <- function(package_name) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      message(paste("Package", package_name, "is not installed. Attempting to install..."))
      install.packages(package_name, dependencies = TRUE, quiet = TRUE)
    }
    library(package_name, character.only = TRUE)
  }

  # Required packages
  required_packages <- c("WeightIt", "MatchIt", "cobalt", "cli")

  # Load or install required packages
  tryCatch({
    sapply(required_packages, load_or_install_package)
  }, error = function(e) {
    stop(paste("Error loading or installing packages:", e$message))
  })

  # Function to print messages if verbose is TRUE
  log_msg <- function(msg, level = "INFO") {
    if (verbose) {
      if (level == "INFO") {
        cli::cli_alert_info(msg)
      } else if (level == "SUCCESS") {
        cli::cli_alert_success(msg)
      } else if (level == "WARNING") {
        cli::cli_alert_warning(msg)
      } else if (level == "ERROR") {
        cli::cli_alert_danger(msg)
      }
    }
  }

  # Initialize result list
  result <- list(
    match_propensity = NULL,
    love_plot = NULL,
    summary = NULL,
    summary_plot = NULL,
    balance_table = NULL,
    diagnostics = NULL
  )

  # Input validation
  tryCatch({
    cli::cli_h1("Input Validation")
    cli::cli_progress_step("Checking input parameters")
    if (!is.data.frame(df_propensity)) stop("df_propensity must be a data frame")
    if (!is.character(exposure_variable) || length(exposure_variable) != 1) stop("exposure_variable must be a single character string")
    if (!is.character(baseline_vars) || length(baseline_vars) < 1) stop("baseline_vars must be a character vector of length >= 1")
    if (!is.character(weights_var_name) || length(weights_var_name) != 1) stop("weights_var_name must be a single character string")
    if (!all(c(exposure_variable, baseline_vars, weights_var_name) %in% names(df_propensity))) stop("All specified variables must be present in df_propensity")
    cli::cli_progress_done()
  }, error = function(e) {
    log_msg(paste("Input validation error:", e$message), "ERROR")
    stop(e)
  })

  log_msg("Starting propensity score modeling and analysis...")

  # Create the propensity score model
  cli::cli_h1("Propensity Score Modeling")
  cli::cli_progress_step("Creating propensity score model")
  result$match_propensity <- tryCatch({
    margot::match_mi_general(
      data = df_propensity,
      X = exposure_variable,
      baseline_vars = baseline_vars,
      estimand = estimand,
      method = method,
      sample_weights = weights_var_name,
      focal = focal
    )
  }, error = function(e) {
    log_msg(paste("Error in creating propensity score model:", e$message), "ERROR")
    return(result)
  })
  cli::cli_progress_done()
  log_msg("Propensity score model created successfully.", "SUCCESS")

  # Default love plot options
  default_love_plot_options <- list(
    binary = "std",
    thresholds = c(m = .1),
    wrap = 50,
    position = "bottom",
    size = 3,
    limits = list(m = c(-1, 2))
  )

  # Merge user-provided options with defaults
  love_plot_options <- modifyList(default_love_plot_options, love_plot_options)

  # Create love plot with merged options
  cli::cli_h1("Generating Plots")
  cli::cli_progress_step("Creating love plot")
  result$love_plot <- tryCatch({
    do.call(cobalt::love.plot,
            c(list(result$match_propensity), love_plot_options))
  }, error = function(e) {
    log_msg(paste("Error in generating love plot:", e$message), "ERROR")
    NULL
  })
  cli::cli_progress_done()
  if (!is.null(result$love_plot)) {
    log_msg("Love plot generated.", "SUCCESS")
  }

  # Create summary
  cli::cli_progress_step("Creating summary of propensity score model")
  result$summary <- tryCatch({
    summary(result$match_propensity)
  }, error = function(e) {
    log_msg(paste("Error in creating summary:", e$message), "ERROR")
    NULL
  })
  cli::cli_progress_done()
  if (!is.null(result$summary)) {
    log_msg("Summary created.", "SUCCESS")
  }

  # Create summary plot
  cli::cli_progress_step("Generating summary plot")
  result$summary_plot <- tryCatch({
    plot(result$summary)
  }, error = function(e) {
    log_msg(paste("Error in generating summary plot:", e$message), "ERROR")
    NULL
  })
  cli::cli_progress_done()
  if (!is.null(result$summary_plot)) {
    log_msg("Summary plot generated.", "SUCCESS")
  }

  # Default balance table options
  default_bal_tab_options <- list(
    un = TRUE,
    thresholds = c(m = .05, v = 2)
  )

  # Merge user-provided options with defaults
  bal_tab_options <- modifyList(default_bal_tab_options, bal_tab_options)

  # Create balance table with merged options
  cli::cli_h1("Balance Analysis")
  cli::cli_progress_step("Creating balance table")
  result$balance_table <- tryCatch({
    do.call(cobalt::bal.tab,
            c(list(result$match_propensity), bal_tab_options))
  }, error = function(e) {
    log_msg(paste("Error in creating balance table:", e$message), "ERROR")
    NULL
  })
  cli::cli_progress_done()
  if (!is.null(result$balance_table)) {
    log_msg("Balance table created.", "SUCCESS")
  }

  # Calculate additional diagnostics
  cli::cli_h1("Additional Diagnostics")
  cli::cli_progress_step("Calculating additional diagnostics")
  result$diagnostics <- tryCatch({
    diag_list <- list()

    if (!is.null(result$match_propensity)) {
      if ("weights" %in% names(result$match_propensity)) {
        diag_list$weight_summary <- summary(result$match_propensity$weights)
        diag_list$effective_sample_size <- sum(result$match_propensity$weights)^2 / sum(result$match_propensity$weights^2)
      } else {
        log_msg("Weights not found in match_propensity object", "WARNING")
      }

      if ("propensity" %in% names(result$match_propensity)) {
        diag_list$prop_score_summary <- summary(result$match_propensity$propensity)
      } else if ("ps" %in% names(result$match_propensity)) {
        diag_list$prop_score_summary <- summary(result$match_propensity$ps)
      } else {
        log_msg("Propensity scores not found in match_propensity object", "WARNING")
      }
    }

    diag_list$model_info <- list(
      estimand = estimand,
      method = method,
      focal = focal,
      exposure_variable = exposure_variable,
      n_baseline_vars = length(baseline_vars)
    )

    diag_list
  }, error = function(e) {
    log_msg(paste("Error in calculating diagnostics:", e$message), "ERROR")
    NULL
  })
  cli::cli_progress_done()
  log_msg("Additional diagnostics calculated.", "SUCCESS")

  log_msg("Propensity score modeling and analysis completed.", "SUCCESS")

  return(result)
}
#
# margot_propensity_model_and_plots <- function(
#     df_propensity,
#     exposure_variable,
#     baseline_vars,
#     weights_var_name,
#     estimand = "ATE",
#     method = "ebal",
#     focal = NULL,
#     love_plot_options = list(),
#     bal_tab_options = list(),
#     verbose = TRUE
# ) {
#
#   # Required libraries
#   library(WeightIt)
#   library(MatchIt)
#   library(cobalt)
#
#   # Function to print messages if verbose is TRUE
#   log_msg <- function(msg, level = "INFO") {
#     if (verbose) {
#       cat(paste0("[", level, "] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", msg, "\n"))
#     }
#   }
#
#   # Initialize result list
#   result <- list(
#     match_propensity = NULL,
#     love_plot = NULL,
#     summary = NULL,
#     summary_plot = NULL,
#     balance_table = NULL,
#     diagnostics = NULL
#   )
#
#   # Input validation
#   tryCatch({
#     if (!is.data.frame(df_propensity)) stop("df_propensity must be a data frame")
#     if (!is.character(exposure_variable) || length(exposure_variable) != 1) stop("exposure_variable must be a single character string")
#     if (!is.character(baseline_vars) || length(baseline_vars) < 1) stop("baseline_vars must be a character vector of length >= 1")
#     if (!is.character(weights_var_name) || length(weights_var_name) != 1) stop("weights_var_name must be a single character string")
#     if (!all(c(exposure_variable, baseline_vars, weights_var_name) %in% names(df_propensity))) stop("All specified variables must be present in df_propensity")
#   }, error = function(e) {
#     log_msg(paste("Input validation error:", e$message), "ERROR")
#     stop(e)
#   })
#
#   log_msg("Starting propensity score modeling and analysis...")
#
#   # Create the propensity score model
#   log_msg("Creating propensity score model...")
#   result$match_propensity <- tryCatch({
#     margot::match_mi_general(
#       data = df_propensity,
#       X = exposure_variable,
#       baseline_vars = baseline_vars,
#       estimand = estimand,
#       method = method,
#       sample_weights = weights_var_name,
#       focal = focal
#     )
#   }, error = function(e) {
#     log_msg(paste("Error in creating propensity score model:", e$message), "ERROR")
#     return(result)
#   })
#   log_msg("Propensity score model created successfully.")
#
#   # Default love plot options
#   default_love_plot_options <- list(
#     binary = "std",
#     thresholds = c(m = .1),
#     wrap = 50,
#     position = "bottom",
#     size = 3,
#     limits = list(m = c(-1, 2))
#   )
#
#   # Merge user-provided options with defaults
#   love_plot_options <- modifyList(default_love_plot_options, love_plot_options)
#
#   # Create love plot with merged options
#   log_msg("Generating love plot...")
#   result$love_plot <- tryCatch({
#     do.call(cobalt::love.plot,
#             c(list(result$match_propensity), love_plot_options))
#   }, error = function(e) {
#     log_msg(paste("Error in generating love plot:", e$message), "ERROR")
#     NULL
#   })
#   if (!is.null(result$love_plot)) {
#     log_msg("Love plot generated.")
#   }
#
#   # Create summary
#   log_msg("Creating summary of propensity score model...")
#   result$summary <- tryCatch({
#     summary(result$match_propensity)
#   }, error = function(e) {
#     log_msg(paste("Error in creating summary:", e$message), "ERROR")
#     NULL
#   })
#   if (!is.null(result$summary)) {
#     log_msg("Summary created.")
#   }
#
#   # Create summary plot
#   log_msg("Generating summary plot...")
#   result$summary_plot <- tryCatch({
#     plot(result$summary)
#   }, error = function(e) {
#     log_msg(paste("Error in generating summary plot:", e$message), "ERROR")
#     NULL
#   })
#   if (!is.null(result$summary_plot)) {
#     log_msg("Summary plot generated.")
#   }
#
#   # Default balance table options
#   default_bal_tab_options <- list(
#     un = TRUE,
#     thresholds = c(m = .05, v = 2)
#   )
#
#   # Merge user-provided options with defaults
#   bal_tab_options <- modifyList(default_bal_tab_options, bal_tab_options)
#
#   # Create balance table with merged options
#   log_msg("Creating balance table...")
#   result$balance_table <- tryCatch({
#     do.call(cobalt::bal.tab,
#             c(list(result$match_propensity), bal_tab_options))
#   }, error = function(e) {
#     log_msg(paste("Error in creating balance table:", e$message), "ERROR")
#     NULL
#   })
#   if (!is.null(result$balance_table)) {
#     log_msg("Balance table created.")
#   }
#
#   # Calculate additional diagnostics
#   log_msg("Calculating additional diagnostics...")
#   result$diagnostics <- tryCatch({
#     diag_list <- list()
#
#     if (!is.null(result$match_propensity)) {
#       if ("weights" %in% names(result$match_propensity)) {
#         diag_list$weight_summary <- summary(result$match_propensity$weights)
#         diag_list$effective_sample_size <- sum(result$match_propensity$weights)^2 / sum(result$match_propensity$weights^2)
#       } else {
#         log_msg("Weights not found in match_propensity object", "WARNING")
#       }
#
#       if ("propensity" %in% names(result$match_propensity)) {
#         diag_list$prop_score_summary <- summary(result$match_propensity$propensity)
#       } else if ("ps" %in% names(result$match_propensity)) {
#         diag_list$prop_score_summary <- summary(result$match_propensity$ps)
#       } else {
#         log_msg("Propensity scores not found in match_propensity object", "WARNING")
#       }
#     }
#
#     diag_list$model_info <- list(
#       estimand = estimand,
#       method = method,
#       focal = focal,
#       exposure_variable = exposure_variable,
#       n_baseline_vars = length(baseline_vars)
#     )
#
#     diag_list
#   }, error = function(e) {
#     log_msg(paste("Error in calculating diagnostics:", e$message), "ERROR")
#     NULL
#   })
#   log_msg("Additional diagnostics calculated.")
#
#   log_msg("Propensity score modeling and analysis completed.")
#
#   return(result)
# }
