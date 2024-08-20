#' Generate Methods Section for a Causal Inference Study
#'
#' This function generates a comprehensive methods section for a research paper,
#' including details about the sample, variables, causal interventions,
#' identification assumptions, target population, eligibility criteria,
#' confounding control, missing data handling, statistical estimators,
#' and additional sections like sensitivity analysis and scope of interventions.
#'
#' @param exposure_var Character string specifying the primary exposure variable.
#' @param outcome_vars A named list of character vectors specifying the outcome variables by domain.
#' @param measure_data A list containing information about each measure.
#' @param n_total Numeric value indicating the total number of participants in the study.
#' @param baseline_wave Character string specifying the baseline wave of the study.
#' @param exposure_wave Character string specifying the exposure wave of the study.
#' @param outcome_wave Character string specifying the outcome wave of the study.
#' @param baseline_missing_data_proportion Numeric value indicating the proportion of missing data at baseline.
#' @param sections_to_include Character vector specifying which sections to include in the output.
#'   Default is 'all', which includes all available sections. Use 'list' to see available sections.
#' @param appendices_measures Character string specifying the appendix where detailed variable descriptions can be found.
#' @param ... Additional arguments to be passed to helper functions.
#'
#' @return If sections_to_include is not 'list', returns a character string containing the generated methods
#'   section in markdown format. If sections_to_include is 'list', returns a character vector of available sections.
#'
#' @examples
#' \dontrun{
#' # Define outcomes by domain
#' outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
#' outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
#' outcomes_social <- c("belong", "neighbourhood_community")
#'
#' # Combine all outcomes into a single list
#' all_outcomes <- list(
#'   health = outcomes_health,
#'   psychological = outcomes_psychological,
#'   social = outcomes_social
#' )
#'
#' # Define the exposure variable
#' exposure_var <- "political_conservative"
#'
#' # Load your measure_data
#' measure_data <- readRDS(here::here("boilerplate", "data", "measure_data.rds"))
#'
#' # Generate methods text
#' methods_text <- boilerplate_methods(
#'   exposure_var = exposure_var,
#'   outcome_vars = all_outcomes,
#'   measure_data = measure_data,
#'   n_total = 47000,
#'   baseline_wave = "NZAVS time 10, years 2018-2019",
#'   exposure_wave = "NZAVS time 11, years 2019-2020",
#'   outcome_wave = "NZAVS time 12, years 2020-2021",
#'   baseline_missing_data_proportion = 0.15,
#'   appendices_measures = "C",
#'   causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
#'   contrasts = "null",
#'   null_intervention = "Do not change exposure_var",
#'   sample = list(appendices = "A-C"),
#'   statistical_estimator = list(estimators = c("lmtp", "grf")),
#'   inclusion_criteria = c(
#'     "Enrolled in the 2018 wave of the New Zealand Attitudes and Values Study (NZAVS time 10).",
#'     "Missing covariate data at baseline was permitted, and the data was subjected to imputation methods to reduce bias."
#'   ),
#'   exclusion_criteria = c(
#'     "Did not answer the political conservative question at NZAVS time 10 and time 11."
#'   ),
#'   n_participants = 32451,
#'   confounding_control = list(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/"),
#'   additional_sections = list(
#'     sensitivity_analysis = list(
#'       description = "We use the E-value method to assess sensitivity to unmeasured confounding."
#'     ),
#'     scope_interventions = list(figure_ref = "@fig-custom-hist"),
#'     evidence_change = list(table_ref = "@tbl-custom-transition")
#'   )
#' )
#'
#' # Print the result
#' cat(methods_text)
#' }
#'
#' @import cli
#'
#' @export
boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, ...) {
  all_sections <- c(
    "sample",
    "variables",
    "causal_interventions",
    "identification_assumptions",
    "target_population",
    "eligibility_criteria",
    "confounding_control",
    "missing_data",
    "statistical_estimator",
    "additional_sections"
  )

  if (identical(sections_to_include, 'list')) {
    return(all_sections)
  }

  cli::cli_alert_info("Starting boilerplate_methods function")

  # initialise an empty list to store all sections
  methods_sections <- list()

  # capture all additional arguments
  extra_args <- list(...)

  cli::cli_alert_info("Starting boilerplate_methods function")

  # Suppress detailed output for safe_get function
  safe_get <- function(name, default = NULL) {
    if (name %in% names(extra_args)) {
      return(extra_args[[name]])
    } else {
      return(default)
    }
  }

  # Modify safe_execute to reduce output
  safe_execute <- function(func_name, args) {
    cli::cli_alert_info("Executing {func_name}")
    tryCatch({
      if (exists(func_name, mode = "function")) {
        func <- get(func_name, mode = "function")
        valid_args <- args[names(args) %in% names(formals(func))]
        result <- do.call(func, valid_args)
        cli::cli_alert_success("Finished {func_name}")
        return(result)
      } else {
        cli::cli_alert_danger("Function {func_name} not found. Skipping this section.")
        return(NULL)
      }
    }, error = function(e) {
      cli::cli_alert_danger("Error in {func_name}: {e$message}")
      return(NULL)
    })
  }

  # get eligibility criteria
  inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
  exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
  n_participants <- safe_get("n_participants", "UNDEFINED")

  # get the statistical estimator
  statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))
  if (is.list(statistical_estimator) && "estimators" %in% names(statistical_estimator)) {
    statistical_estimator <- statistical_estimator$estimators  # Use all provided estimators
  }

  sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include

  # call sub-functions for each section
  for (section in sections) {
    cli::cli_h1("Processing section: {section}")
    section_name <- section
    func_name <- paste0("boilerplate_methods_", section)
    args <- list()

    # Add only necessary arguments for each section
    if (section_name == "variables") {
      args <- list(
        exposure_var = exposure_var,
        outcome_vars = outcome_vars,
        measure_data = safe_get("measure_data"),
        appendices_measures = appendices_measures
      )
    } else if (section_name == "causal_interventions") {
      args <- list(
        exposure_var = exposure_var,
        causal_interventions = safe_get("causal_interventions")$interventions,
        contrasts = safe_get("contrasts", "pairwise"),
        null_intervention = safe_get("null_intervention", NULL)
      )
    } else if (section_name == "target_population") {
      args <- list(
        statistical_estimator = if (is.list(statistical_estimator)) statistical_estimator[[1]] else statistical_estimator[1],
        baseline_wave = baseline_wave
      )
    } else if (section_name == "eligibility_criteria") {
      args <- list(
        inclusion_criteria = inclusion_criteria,
        exclusion_criteria = exclusion_criteria,
        n_participants = n_participants,
        baseline_wave = baseline_wave
      )
    } else if (section_name == "missing_data") {
      args <- list(
        estimators = statistical_estimator,
        baseline_wave = baseline_wave,
        exposure_wave = exposure_wave,
        outcome_wave = outcome_wave,
        baseline_missing_data_proportion = baseline_missing_data_proportion
      )
    } else if (section_name == "statistical_estimator") {
      args <- list(estimators = statistical_estimator)
    } else if (section_name == "additional_sections") {
      additional_sections_args <- safe_get("additional_sections", list())
      args <- list(
        sensitivity_analysis = additional_sections_args$sensitivity_analysis,
        scope_interventions = additional_sections_args$scope_interventions,
        evidence_change = additional_sections_args$evidence_change
      )
    } else {
      # For other sections, pass all common arguments
      args <- list(
        exposure_var = exposure_var,
        outcome_vars = outcome_vars,
        n_total = n_total,
        baseline_wave = baseline_wave,
        exposure_wave = exposure_wave,
        outcome_wave = outcome_wave
      )
    }

    result <- safe_execute(func_name, args)
    if (!is.null(result)) {
      methods_sections[[section_name]] <- result
      cli::cli_alert_success("Added result for {section_name} to methods_sections")
    } else {
      cli::cli_alert_warning("No result added for {section_name}")
    }
  }

  # Combine all sections
  cli::cli_alert_info("Combining all sections")
  markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")

  cli::cli_alert_success("Finished boilerplate_methods function \U0001F44D")

  return(markdown_output)
}
# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, ...) {
#   all_sections <- c(
#     "sample",
#     "variables",
#     "causal_interventions",
#     "identification_assumptions",
#     "target_population",
#     "eligibility_criteria",
#     "confounding_control",
#     "missing_data",
#     "statistical_estimator",
#     "additional_sections"
#   )
#
#   if (identical(sections_to_include, 'list')) {
#     return(all_sections)
#   }
#
#   cli::cli_alert_info("Starting boilerplate_methods function")
#
#   # initialise an empty list to store all sections
#   methods_sections <- list()
#
#   # capture all additional arguments
#   extra_args <- list(...)
#
#   # define the safe_get function inside boilerplate_methods
#   safe_get <- function(name, default = NULL) {
#     if (name %in% names(extra_args)) {
#       cli::cli_alert_success("Found {name} in extra_args")
#       return(extra_args[[name]])
#     } else {
#       cli::cli_alert_warning("Using default value for {name}")
#       return(default)
#     }
#   }
#
#   # define a helper function for error-tolerant execution
#   safe_execute <- function(func_name, args) {
#     cli::cli_h2("Executing {func_name}")
#     cli::cli_alert_info("Arguments passed to {func_name}:")
#     print(args)
#     tryCatch({
#       if (exists(func_name, mode = "function")) {
#         func <- get(func_name, mode = "function")
#         # Only pass arguments that the function can accept
#         func_args <- formals(func)
#         valid_args <- args[names(args) %in% names(func_args)]
#         cli::cli_alert_info("Valid arguments for {func_name}:")
#         print(valid_args)
#         result <- do.call(func, valid_args)
#         cli::cli_alert_success("Finished {func_name}")
#         cli::cli_alert_info("Result of {func_name}:")
#         print(result)
#         return(result)
#       } else {
#         cli::cli_alert_danger("Function {func_name} not found. Skipping this section.")
#         return(NULL)
#       }
#     }, error = function(e) {
#       cli::cli_alert_danger("Error in {func_name}: {e$message}")
#       return(NULL)
#     })
#   }
#
#   # get eligibility criteria
#   inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#   exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#   n_participants <- safe_get("n_participants", "UNDEFINED")
#
#   # get the statistical estimator
#   statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#   if (is.list(statistical_estimator) && "estimators" %in% names(statistical_estimator)) {
#     statistical_estimator <- statistical_estimator$estimators  # Use all provided estimators
#   }
#
#   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#
#   # call sub-functions for each section
#   for (section in sections) {
#     cli::cli_h1("Processing section: {section}")
#     section_name <- section
#     func_name <- paste0("boilerplate_methods_", section)
#     args <- list()
#
#     # Add only necessary arguments for each section
#     if (section_name == "variables") {
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         measure_data = safe_get("measure_data"),
#         appendices_measures = appendices_measures
#       )
#     } else if (section_name == "causal_interventions") {
#       args <- list(
#         exposure_var = exposure_var,
#         causal_interventions = safe_get("causal_interventions")$interventions,
#         contrasts = safe_get("contrasts", "pairwise"),
#         null_intervention = safe_get("null_intervention", NULL)
#       )
#     } else if (section_name == "target_population") {
#       args <- list(
#         statistical_estimator = if (is.list(statistical_estimator)) statistical_estimator[[1]] else statistical_estimator[1],
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "eligibility_criteria") {
#       args <- list(
#         inclusion_criteria = inclusion_criteria,
#         exclusion_criteria = exclusion_criteria,
#         n_participants = n_participants,
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "missing_data") {
#       args <- list(
#         estimators = statistical_estimator,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave,
#         baseline_missing_data_proportion = baseline_missing_data_proportion
#       )
#     } else if (section_name == "statistical_estimator") {
#       args <- list(estimators = statistical_estimator)
#     } else if (section_name == "additional_sections") {
#       additional_sections_args <- safe_get("additional_sections", list())
#       args <- list(
#         sensitivity_analysis = additional_sections_args$sensitivity_analysis,
#         scope_interventions = additional_sections_args$scope_interventions,
#         evidence_change = additional_sections_args$evidence_change
#       )
#     } else {
#       # For other sections, pass all common arguments
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         n_total = n_total,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave
#       )
#     }
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section_name]] <- result
#       cli::cli_alert_success("Added result for {section_name} to methods_sections")
#     } else {
#       cli::cli_alert_warning("No result added for {section_name}")
#     }
#   }
#
#   # combine all sections into a single markdown string
#   cli::cli_h1("Combining all sections")
#   cli::cli_alert_info("Contents of methods_sections:")
#   print(methods_sections)
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   cli::cli_alert_success("Finished boilerplate_methods function")
#   cli::cli_alert_info("Final markdown_output:")
#   cat(markdown_output)
#
#   return(markdown_output)
# }
# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, ...) {
#   all_sections <- c(
#     "sample",
#     "variables",
#     "causal_interventions",
#     "identification_assumptions",
#     "target_population",
#     "eligibility_criteria",
#     "confounding_control",
#     "missing_data",
#     "statistical_estimator",
#     "additional_sections"
#   )
#
#   if (identical(sections_to_include, 'list')) {
#     return(all_sections)
#   }
#
#   cat("Starting boilerplate_methods function\n")
#
#   # initialise an empty list to store all sections
#   methods_sections <- list()
#
#   # capture all additional arguments
#   extra_args <- list(...)
#
#   # define the safe_get function inside boilerplate_methods
#   safe_get <- function(name, default = NULL) {
#     if (name %in% names(extra_args)) {
#       cat(paste("Found", name, "in extra_args\n"))
#       return(extra_args[[name]])
#     } else {
#       cat(paste("Using default value for", name, "\n"))
#       return(default)
#     }
#   }
#
#   # define a helper function for error-tolerant execution
#   safe_execute <- function(func_name, args) {
#     cat(paste("Executing", func_name, "\n"))
#     cat("Arguments passed to", func_name, ":\n")
#     print(args)
#     tryCatch({
#       if (exists(func_name, mode = "function")) {
#         func <- get(func_name, mode = "function")
#         # Only pass arguments that the function can accept
#         func_args <- formals(func)
#         valid_args <- args[names(args) %in% names(func_args)]
#         cat("Valid arguments for", func_name, ":\n")
#         print(valid_args)
#         result <- do.call(func, valid_args)
#         cat(paste("Finished", func_name, "\n"))
#         cat("Result of", func_name, ":\n")
#         print(result)
#         return(result)
#       } else {
#         warning(paste("Function", func_name, "not found. Skipping this section."))
#         return(NULL)
#       }
#     }, error = function(e) {
#       warning(paste("Error in", func_name, ":", e$message))
#       return(NULL)
#     })
#   }
#
#   # get eligibility criteria
#   inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#   exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#   n_participants <- safe_get("n_participants", "UNDEFINED")
#
#   # get the statistical estimator
#   statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#   if (is.list(statistical_estimator) && "estimators" %in% names(statistical_estimator)) {
#     statistical_estimator <- statistical_estimator$estimators  # Use all provided estimators
#   }
#
#   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#
#   # call sub-functions for each section
#   for (section in sections) {
#     cat(paste("\nProcessing section:", section, "\n"))
#     section_name <- section
#     func_name <- paste0("boilerplate_methods_", section)
#     args <- list()
#
#     # Add only necessary arguments for each section
#     if (section_name == "variables") {
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         measure_data = safe_get("measure_data"),
#         appendices_measures = appendices_measures
#       )
#     } else if (section_name == "causal_interventions") {
#       args <- list(
#         exposure_var = exposure_var,
#         causal_interventions = safe_get("causal_interventions")$interventions,
#         contrasts = safe_get("contrasts", "pairwise"),
#         null_intervention = safe_get("null_intervention", NULL)
#       )
#     } else if (section_name == "target_population") {
#       args <- list(
#         statistical_estimator = if (is.list(statistical_estimator)) statistical_estimator[[1]] else statistical_estimator[1],
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "eligibility_criteria") {
#       args <- list(
#         inclusion_criteria = inclusion_criteria,
#         exclusion_criteria = exclusion_criteria,
#         n_participants = n_participants,
#         baseline_wave = baseline_wave
#       )
#     } else if (section_name == "missing_data") {
#       args <- list(
#         estimators = statistical_estimator,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave,
#         baseline_missing_data_proportion = baseline_missing_data_proportion
#       )
#     } else if (section_name == "statistical_estimator") {
#       args <- list(estimators = statistical_estimator)
#     } else if (section_name == "additional_sections") {
#       additional_sections_args <- safe_get("additional_sections", list())
#       args <- list(
#         sensitivity_analysis = additional_sections_args$sensitivity_analysis,
#         scope_interventions = additional_sections_args$scope_interventions,
#         evidence_change = additional_sections_args$evidence_change
#       )
#     } else {
#       # For other sections, pass all common arguments
#       args <- list(
#         exposure_var = exposure_var,
#         outcome_vars = outcome_vars,
#         n_total = n_total,
#         baseline_wave = baseline_wave,
#         exposure_wave = exposure_wave,
#         outcome_wave = outcome_wave
#       )
#     }
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section_name]] <- result
#       cat(paste("Added result for", section_name, "to methods_sections\n"))
#     } else {
#       cat(paste("No result added for", section_name, "\n"))
#     }
#   }
#
#   # combine all sections into a single markdown string
#   cat("\nCombining all sections\n")
#   cat("Contents of methods_sections:\n")
#   print(methods_sections)
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   cat("Finished boilerplate_methods function\n")
#   cat("Final markdown_output:\n")
#   cat(markdown_output)
#
#   return(markdown_output)
# }
# boilerplate_methods <- function(exposure_var, outcome_vars, measure_data, n_total, baseline_wave,
#                                 exposure_wave, outcome_wave, baseline_missing_data_proportion,
#                                 sections_to_include = 'all', appendices_measures = NULL, ...) {
#   all_sections <- c(
#     "sample",
#     "variables",
#     "causal_interventions",
#     "identification_assumptions",
#     "target_population",
#     "eligibility_criteria",
#     "confounding_control",
#     "missing_data",
#     "statistical_estimator",
#     "additional_sections"
#   )
#
#   if (identical(sections_to_include, 'list')) {
#     return(all_sections)
#   }
#
#   # Capture all additional arguments
#   extra_args <- list(...)
#
#   # Helper function to safely get arguments
#   safe_get <- function(name, default = NULL) {
#     if (name %in% names(extra_args)) {
#       return(extra_args[[name]])
#     } else {
#       return(default)
#     }
#   }
#
#   # Helper function for error-tolerant execution
#   safe_execute <- function(func_name, args) {
#     tryCatch({
#       if (exists(func_name, mode = "function")) {
#         func <- get(func_name, mode = "function")
#         valid_args <- args[names(args) %in% names(formals(func))]
#         return(do.call(func, valid_args))
#       } else {
#         warning(paste("Function", func_name, "not found. Skipping this section."))
#         return(NULL)
#       }
#     }, error = function(e) {
#       warning(paste("Error in", func_name, ":", e$message))
#       return(NULL)
#     })
#   }
#
#   # Initialize methods_sections
#   methods_sections <- list()
#
#   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#
#   for (section in sections) {
#     func_name <- paste0("boilerplate_methods_", section)
#     args <- list(
#       exposure_var = exposure_var,
#       outcome_vars = outcome_vars,
#       measure_data = measure_data,
#       n_total = n_total,
#       baseline_wave = baseline_wave,
#       exposure_wave = exposure_wave,
#       outcome_wave = outcome_wave,
#       baseline_missing_data_proportion = baseline_missing_data_proportion,
#       appendices_measures = appendices_measures
#     )
#
#     # Add section-specific arguments
#     args <- c(args, safe_get(section, list()))
#
#     # Special handling for certain sections
#     if (section == "causal_interventions") {
#       args$causal_interventions <- args$interventions
#       args$contrasts <- safe_get("contrasts", "pairwise")
#       args$null_intervention <- safe_get("null_intervention", NULL)
#       args$interventions <- NULL
#     } else if (section == "target_population") {
#       args$statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))$estimators[1]
#     } else if (section == "eligibility_criteria") {
#       args$inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#       args$exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#       args$n_participants <- safe_get("n_participants", "UNDEFINED")
#     } else if (section == "missing_data") {
#       args$estimators <- safe_get("statistical_estimator", list(estimators = "lmtp"))$estimators
#     } else if (section == "statistical_estimator") {
#       args$estimators <- safe_get("statistical_estimator", list(estimators = "lmtp"))$estimators
#     } else if (section == "additional_sections") {
#       args <- safe_get("additional_sections", list())
#     }
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section]] <- result
#     }
#   }
#
#   # Combine all sections into a single markdown string
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   return(markdown_output)
# }
# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, domains = NULL, ...) {
#   all_sections <- c(
#     "sample",
#     "variables",
#     "causal_interventions",
#     "identification_assumptions",
#     "target_population",
#     "eligibility_criteria",
#     "confounding_control",
#     "missing_data",
#     "statistical_estimator",
#     "additional_sections"
#   )
#
#   if (identical(sections_to_include, 'list')) {
#     return(all_sections)
#   }
#
#   cat("Starting boilerplate_methods function\n")
#
#   # initialise an empty list to store all sections
#   methods_sections <- list()
#
#   # capture all additional arguments
#   extra_args <- list(...)
#
#   # define the safe_get function inside boilerplate_methods
#   safe_get <- function(name, default = NULL) {
#     if (name %in% names(extra_args)) {
#       cat(paste("Found", name, "in extra_args\n"))
#       return(extra_args[[name]])
#     } else {
#       cat(paste("Using default value for", name, "\n"))
#       return(default)
#     }
#   }
#
#   # define a helper function for error-tolerant execution
#   safe_execute <- function(func_name, args) {
#     cat(paste("Executing", func_name, "\n"))
#     cat("Arguments passed to", func_name, ":\n")
#     print(args)
#     tryCatch({
#       if (exists(func_name, mode = "function")) {
#         func <- get(func_name, mode = "function")
#         # Only pass arguments that the function can accept
#         func_args <- formals(func)
#         valid_args <- args[names(args) %in% names(func_args)]
#         cat("Valid arguments for", func_name, ":\n")
#         print(valid_args)
#         result <- do.call(func, valid_args)
#         cat(paste("Finished", func_name, "\n"))
#         cat("Result of", func_name, ":\n")
#         print(result)
#         return(result)
#       } else {
#         warning(paste("Function", func_name, "not found. Skipping this section."))
#         return(NULL)
#       }
#     }, error = function(e) {
#       warning(paste("Error in", func_name, ":", e$message))
#       return(NULL)
#     })
#   }
#
#   # get eligibility criteria
#   inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#   exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#   n_participants <- safe_get("n_participants", "UNDEFINED")
#
#   # get the statistical estimator
#   statistical_estimator <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#   if (is.list(statistical_estimator) && "estimators" %in% names(statistical_estimator)) {
#     statistical_estimator <- statistical_estimator$estimators  # Use all provided estimators
#   }
#
#   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#
#   # call sub-functions for each section
#   for (section in sections) {
#     cat(paste("\nProcessing section:", section, "\n"))
#     section_name <- section
#     func_name <- paste0("boilerplate_methods_", section)
#     args <- safe_get(section_name, list())
#
#     # add exposure_var, outcome_vars, n_total, and wave information to args if they're needed
#     args$exposure_var <- exposure_var
#     args$n_total <- n_total
#     args$baseline_wave <- baseline_wave
#     args$exposure_wave <- exposure_wave
#     args$outcome_wave <- outcome_wave
#
#     if (section_name == "variables") {
#       args$outcome_vars <- outcome_vars
#       args$appendices_measures <- appendices_measures
#       args$domains <- domains
#     }
#
#     # Special handling for causal_interventions
#     if (section_name == "causal_interventions") {
#       args$causal_interventions <- args$interventions
#       args$contrasts <- safe_get("contrasts", "pairwise")
#       args$null_intervention <- safe_get("null_intervention", NULL)
#       args$interventions <- NULL
#     }
#
#     # special handling for target_population
#     if (section_name == "target_population") {
#       args$statistical_estimator <- if (is.list(statistical_estimator)) statistical_estimator$estimators[1] else statistical_estimator[1]
#       args$baseline_wave <- baseline_wave
#     }
#
#     # special handling for eligibility_criteria
#     if (section_name == "eligibility_criteria") {
#       args$inclusion_criteria <- inclusion_criteria
#       args$exclusion_criteria <- exclusion_criteria
#       args$n_participants <- n_participants
#     }
#
#     if (section_name == "missing_data") {
#       args$estimators <- statistical_estimator
#       args$baseline_wave <- baseline_wave
#       args$exposure_wave <- exposure_wave
#       args$outcome_wave <- outcome_wave
#       args$baseline_missing_data_proportion <- baseline_missing_data_proportion
#     }
#
#     # special handling for statistical_estimator
#     if (section_name == "statistical_estimator") {
#       args$estimators <- statistical_estimator
#     }
#
#     # special handling for additional_sections
#     if (section_name == "additional_sections") {
#       additional_sections_args <- safe_get("additional_sections", list())
#       args <- list(
#         sensitivity_analysis = additional_sections_args$sensitivity_analysis,
#         scope_interventions = additional_sections_args$scope_interventions,
#         evidence_change = additional_sections_args$evidence_change
#       )
#     }
#
#     # unlist nested lists if necessary
#     args <- lapply(args, function(arg) if (is.list(arg) && length(arg) == 1) unlist(arg) else arg)
#
#     result <- safe_execute(func_name, args)
#     if (!is.null(result)) {
#       methods_sections[[section_name]] <- result
#       cat(paste("Added result for", section_name, "to methods_sections\n"))
#     } else {
#       cat(paste("No result added for", section_name, "\n"))
#     }
#   }
#   # combine all sections into a single markdown string
#   cat("\nCombining all sections\n")
#   cat("Contents of methods_sections:\n")
#   print(methods_sections)
#   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#
#   cat("Finished boilerplate_methods function\n")
#   cat("Final markdown_output:\n")
#   print(markdown_output)
#   return(markdown_output)
# }
