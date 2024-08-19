#' Generate Methods Section for a Causal Inference Study
#'
#' This function generates a comprehensive methods section for a research paper,
#' including details about the sample, variables, causal interventions,
#' identification assumptions, target population, eligibility criteria,
#' confounding control, missing data handling, statistical estimators,
#' and additional sections like sensitivity analysis and scope of interventions.
#' Users can specify which sections to include in the output.
#'
#' @param exposure_var Character string specifying the primary exposure variable.
#' @param outcome_vars Character vector specifying the outcome variables.
#' @param n_total Numeric value indicating the total number of participants in the study.
#' @param baseline_wave Character string specifying the baseline wave of the study.
#' @param exposure_wave Character string specifying the exposure wave of the study.
#' @param outcome_wave Character string specifying the outcome wave of the study.
#' @param baseline_missing_data_proportion Numeric value indicating the proportion of missing data at baseline.
#' @param sections_to_include Character vector specifying which sections to include in the output.
#'   Default is 'all', which includes all available sections. Otherwise, specify a vector of section names.
#' @param ... Additional arguments to be passed to helper functions.
#'
#' @return A character string containing the generated methods section in markdown format.
#'
#' @examples
#' \dontrun{
#' # Generate methods text with all sections
#' methods_text_all <- boilerplate_methods(
#'   exposure_var = "political_conservative",
#'   outcome_vars = c("religion_identification_level", "rumination", "self_esteem"),
#'   n_total = 47000,
#'   baseline_wave = "NZAVS time 10, years 2018-2019",
#'   exposure_wave = "NZAVS time 11, years 2019-2020",
#'   outcome_wave = "NZAVS time 12, years 2020-2021",
#'   baseline_missing_data_proportion = 0.15,
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
#'     "Did not answer the religious service attendance question at NZAVS time 10 and time 11."
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
#' # Generate methods text with specific sections
#' methods_text_specific <- boilerplate_methods(
#'   exposure_var = "political_conservative",
#'   outcome_vars = c("religion_identification_level", "rumination", "self_esteem"),
#'   n_total = 47000,
#'   baseline_wave = "NZAVS time 10, years 2018-2019",
#'   exposure_wave = "NZAVS time 11, years 2019-2020",
#'   outcome_wave = "NZAVS time 12, years 2020-2021",
#'   baseline_missing_data_proportion = 0.15,
#'   sections_to_include = c("sample", "variables", "causal_interventions"),
#'   causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
#'   contrasts = "null",
#'   null_intervention = "Do not change exposure_var",
#'   sample = list(appendices = "A-C")
#' )
#' }
#'
#' @export
boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, sections_to_include = 'all', ...) {
  cat("Starting boilerplate_methods function\n")

  # initialise an empty list to store all sections
  methods_sections <- list()

  # capture all additional arguments
  extra_args <- list(...)

  # define the safe_get function inside boilerplate_methods
  safe_get <- function(name, default = NULL) {
    if (name %in% names(extra_args)) {
      cat(paste("Found", name, "in extra_args\n"))
      return(extra_args[[name]])
    } else {
      cat(paste("Using default value for", name, "\n"))
      return(default)
    }
  }

  # define a helper function for error-tolerant execution
  safe_execute <- function(func_name, args) {
    cat(paste("Executing", func_name, "\n"))
    cat("Arguments passed to", func_name, ":\n")
    print(args)
    tryCatch({
      if (exists(func_name, mode = "function")) {
        func <- get(func_name, mode = "function")
        # Only pass arguments that the function can accept
        func_args <- formals(func)
        valid_args <- args[names(args) %in% names(func_args)]
        cat("Valid arguments for", func_name, ":\n")
        print(valid_args)
        result <- do.call(func, valid_args)
        cat(paste("Finished", func_name, "\n"))
        cat("Result of", func_name, ":\n")
        print(result)
        return(result)
      } else {
        warning(paste("Function", func_name, "not found. Skipping this section."))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error in", func_name, ":", e$message))
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

  # If sections_to_include is 'all', use all sections, otherwise use the specified sections
  sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include

  # call sub-functions for each section
  for (section in sections) {
    cat(paste("\nProcessing section:", section, "\n"))
    section_name <- section
    func_name <- paste0("boilerplate_methods_", section)
    args <- safe_get(section_name, list())

    # add exposure_var, outcome_vars, n_total, and wave information to args if they're needed
    args$exposure_var <- exposure_var
    args$n_total <- n_total
    args$baseline_wave <- baseline_wave
    args$exposure_wave <- exposure_wave
    args$outcome_wave <- outcome_wave

    if (section_name == "variables") {
      args$outcome_vars <- outcome_vars
    }

    # Special handling for causal_interventions
    if (section_name == "causal_interventions") {
      args$causal_interventions <- args$interventions
      args$contrasts <- safe_get("contrasts", "pairwise")
      args$null_intervention <- safe_get("null_intervention", NULL)
      args$interventions <- NULL
    }

    # special handling for target_population
    if (section_name == "target_population") {
      args$statistical_estimator <- if (is.list(statistical_estimator)) statistical_estimator$estimators[1] else statistical_estimator[1]
      args$baseline_wave <- baseline_wave
    }

    # special handling for eligibility_criteria
    if (section_name == "eligibility_criteria") {
      args$inclusion_criteria <- inclusion_criteria
      args$exclusion_criteria <- exclusion_criteria
      args$n_participants <- n_participants
    }

    if (section_name == "missing_data") {
      args$estimators <- statistical_estimator
      args$baseline_wave <- baseline_wave
      args$exposure_wave <- exposure_wave
      args$outcome_wave <- outcome_wave
      args$baseline_missing_data_proportion <- baseline_missing_data_proportion
    }

    # special handling for statistical_estimator
    if (section_name == "statistical_estimator") {
      args$estimators <- statistical_estimator
    }

    # special handling for additional_sections
    if (section_name == "additional_sections") {
      additional_sections_args <- safe_get("additional_sections", list())
      args <- list(
        sensitivity_analysis = additional_sections_args$sensitivity_analysis,
        scope_interventions = additional_sections_args$scope_interventions,
        evidence_change = additional_sections_args$evidence_change
      )
    }

    # unlist nested lists if necessary
    args <- lapply(args, function(arg) if (is.list(arg) && length(arg) == 1) unlist(arg) else arg)

    result <- safe_execute(func_name, args)
    if (!is.null(result)) {
      methods_sections[[section_name]] <- result
      cat(paste("Added result for", section_name, "to methods_sections\n"))
    } else {
      cat(paste("No result added for", section_name, "\n"))
    }
  }
  # combine all sections into a single markdown string
  cat("\nCombining all sections\n")
  cat("Contents of methods_sections:\n")
  print(methods_sections)
  markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")

  cat("Finished boilerplate_methods function\n")
  cat("Final markdown_output:\n")
  print(markdown_output)
  return(markdown_output)
}

# boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion, ...) {
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
#   sections <- c(
#     "boilerplate_methods_sample",
#     "boilerplate_methods_variables",
#     "boilerplate_methods_causal_interventions",
#     "boilerplate_methods_identification_assumptions",
#     "boilerplate_methods_target_population",
#     "boilerplate_methods_eligibility_criteria",
#     "boilerplate_methods_confounding_control",
#     "boilerplate_methods_missing_data",
#     "boilerplate_methods_statistical_estimator",
#     "boilerplate_methods_additional_sections"
#   )
#
#
#   # call sub-functions for each section
#   for (section in sections) {
#     cat(paste("\nProcessing section:", section, "\n"))
#     section_name <- sub("boilerplate_methods_", "", section)
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
#     result <- safe_execute(section, args)
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
