#' Generate Methods Section for a Causal Inference Study (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `boilerplate::boilerplate_report_methods()` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @examples
#' \dontrun{
#' # This function is deprecated. Use instead:
#' # devtools::install_github("go-bayes/boilerplate")
#' # library(boilerplate)
#' # boilerplate::boilerplate_report_methods(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @export
boilerplate_methods <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "boilerplate_methods()",
    with = "boilerplate::boilerplate_report_methods()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_report_methods() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Methods Section for a Causal Inference Study
#' #'
#' #' This function generates a comprehensive methods section for a research paper,
#' #' including details about the sample, variables, causal interventions,
#' #' identification assumptions, target population, eligibility criteria,
#' #' confounding control, missing data handling, statistical estimators,
#' #' and additional sections like sensitivity analysis and scope of interventions.
#' #'
#' #' @param exposure_var Character string specifying the primary exposure variable.
#' #' @param outcome_vars A named list of character vectors specifying the outcome variables by domain.
#' #' @param measure_data A list containing information about each measure.
#' #' @param n_total Numeric value indicating the total number of participants in the study.
#' #' @param baseline_wave Character string specifying the baseline wave of the study.
#' #' @param exposure_wave Character string specifying the exposure wave of the study.
#' #' @param outcome_wave Character string specifying the outcome wave of the study.
#' #' @param baseline_missing_data_proportion Numeric value indicating the proportion of missing data at baseline.
#' #' @param sections_to_include Character vector specifying which sections to include in the output.
#' #'   Default is 'all', which includes all available sections. Use 'list' to see available sections.
#' #' @param appendices_measures Character string specifying the appendix where detailed variable descriptions can be found.
#' #' @param ... Additional arguments to be passed to helper functions.
#' #'
#' #' @return If sections_to_include is not 'list', returns a character string containing the generated methods
#' #'   section in markdown format. If sections_to_include is 'list', returns a character vector of available sections.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Define outcomes by domain
#' #' outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
#' #' outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
#' #' outcomes_social <- c("belong", "neighbourhood_community")
#' #'
#' #' # Combine all outcomes into a single list
#' #' all_outcomes <- list(
#' #'   health = outcomes_health,
#' #'   psychological = outcomes_psychological,
#' #'   social = outcomes_social
#' #' )
#' #'
#' #' # Define the exposure variable
#' #' exposure_var <- "political_conservative"
#' #'
#' #' # Load your measure_data
#' #' measure_data <- readRDS(here::here("boilerplate", "data", "measure_data.rds"))
#' #'
#' #' # Generate methods text
#' #' methods_text <- boilerplate_methods(
#' #'   exposure_var = exposure_var,
#' #'   outcome_vars = all_outcomes,
#' #'   measure_data = measure_data,
#' #'   n_total = 47000,
#' #'   baseline_wave = "NZAVS time 10, years 2018-2019",
#' #'   exposure_wave = "NZAVS time 11, years 2019-2020",
#' #'   outcome_wave = "NZAVS time 12, years 2020-2021",
#' #'   baseline_missing_data_proportion = 0.15,
#' #'   appendices_measures = "C",
#' #'   causal_interventions = list(interventions = c("Increase exposure_var", "Do not change exposure_var")),
#' #'   contrasts = "null",
#' #'   null_intervention = "Do not change exposure_var",
#' #'   sample = list(appendices = "A-C"),
#' #'   statistical_estimator = list(estimators = c("lmtp", "grf")),
#' #'   inclusion_criteria = c(
#' #'     "Enrolled in the 2018 wave of the New Zealand Attitudes and Values Study (NZAVS time 10).",
#' #'     "Missing covariate data at baseline was permitted, and the data was subjected to imputation methods to reduce bias."
#' #'   ),
#' #'   exclusion_criteria = c(
#' #'     "Did not answer the political conservative question at NZAVS time 10 and time 11."
#' #'   ),
#' #'   n_participants = 32451,
#' #'   confounding_control = list(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/"),
#' #'   additional_sections = list(
#' #'     sensitivity_analysis = list(
#' #'       description = "We use the E-value method to assess sensitivity to unmeasured confounding."
#' #'     ),
#' #'     scope_interventions = list(figure_ref = "@fig-custom-hist"),
#' #'     evidence_change = list(table_ref = "@tbl-custom-transition")
#' #'   )
#' #' )
#' #'
#' #' # Print the result
#' #' cat(methods_text)
#' #' }
#' #'
#' #' @import cli
#' #'
#' #' @export
#' boilerplate_methods <- function(exposure_var, outcome_vars, n_total, baseline_wave, exposure_wave, outcome_wave baseline_missing_data_proportion, sections_to_include = 'all', appendices_measures = NULL, ...) {
#'   all_sections <- c(
#'     "sample",
#'     "variables", # will need to be bibliography
#'     "causal_interventions",
#'     "identification_assumptions",
#'     "target_population",
#'     "eligibility_criteria",
#'     "confounding_control",
#'     "missing_data",
#'     "statistical_estimator",
#'     "additional_sections"
#'   )
#'
#'   if (identical(sections_to_include, 'list')) {
#'     return(all_sections)
#'   }
#'
#'   # Define sections based on sections_to_include parameter
#'   sections <- if(identical(sections_to_include, 'all')) all_sections else sections_to_include
#'
#'   cli::cli_alert_info("Starting boilerplate_methods function")
#'
#'   # initialise an empty list to store all sections
#'   methods_sections <- list()
#'
#'   # capture all additional arguments
#'   extra_args <- list(...)
#'
#'   # Safe get function to retrieve arguments with defaults
#'   safe_get <- function(name, default = NULL) {
#'     if (name %in% names(extra_args)) {
#'       return(extra_args[[name]])
#'     } else {
#'       return(default)
#'     }
#'   }
#'
#'   # Safe execute function
#'   safe_execute <- function(func_name, args) {
#'     cli::cli_alert_info("Executing {func_name}")
#'     tryCatch({
#'       if (exists(func_name, mode = "function")) {
#'         func <- get(func_name, mode = "function")
#'         valid_args <- args[names(args) %in% names(formals(func))]
#'         result <- do.call(func, valid_args)
#'         cli::cli_alert_success("Finished {func_name}")
#'         return(result)
#'       } else {
#'         cli::cli_alert_danger("Function {func_name} not found. Skipping this section.")
#'         return(NULL)
#'       }
#'     }, error = function(e) {
#'       cli::cli_alert_danger("Error in {func_name}: {e$message}")
#'       return(NULL)
#'     })
#'   }
#'
#'   for (section in sections) {
#'     cli::cli_h1("Processing section: {section}")
#'     section_name <- section
#'     func_name <- paste0("boilerplate_methods_", section)
#'
#'     # Prepare base arguments for all sections
#'     args <- list(
#'       exposure_var = exposure_var,
#'       outcome_vars = outcome_vars,
#'       measure_data = safe_get("measure_data"),
#'       appendices_measures = appendices_measures,
#'       n_total = n_total,
#'       baseline_wave = baseline_wave,
#'       exposure_wave = exposure_wave,
#'       outcome_wave = outcome_wave,
#'       baseline_missing_data_proportion = baseline_missing_data_proportion
#'     )
#'
#'     # Add section-specific arguments
#'     if (section_name == "causal_interventions") {
#'       args$causal_interventions <- safe_get("causal_interventions", list())
#'       args$contrasts <- safe_get("contrasts", "pairwise")
#'       args$null_intervention <- safe_get("null_intervention", NULL)
#'     } else if (section_name == "eligibility_criteria") {
#'       args$inclusion_criteria <- safe_get("inclusion_criteria", list("No inclusion criteria specified"))
#'       args$exclusion_criteria <- safe_get("exclusion_criteria", list("No exclusion criteria specified"))
#'       args$n_participants <- safe_get("n_participants", "UNDEFINED")
#'     } else if (section_name == "missing_data") {
#'       args$estimators <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#'     } else if (section_name == "statistical_estimator") {
#'       args$estimators <- safe_get("statistical_estimator", list(estimators = "lmtp"))
#'     } else if (section_name == "additional_sections") {
#'       args$sensitivity_analysis <- safe_get("sensitivity_analysis", NULL)
#'       args$scope_interventions <- safe_get("scope_interventions", NULL)
#'       args$evidence_change <- safe_get("evidence_change", NULL)
#'     }
#'
#'     # Execute the function for each section
#'     if (section_name == "variables") {
#'       result <- do.call(boilerplate_methods_variables, args)
#'     } else {
#'       result <- safe_execute(func_name, args)
#'     }
#'
#'     # Store the result
#'     if (!is.null(result)) {
#'       methods_sections[[section_name]] <- result
#'       cli::cli_alert_success("Added result for {section_name} to methods_sections")
#'     } else {
#'       cli::cli_alert_warning("No result added for {section_name}")
#'     }
#'   }
#'
#'   # Combine all sections
#'   cli::cli_alert_info("Combining all sections")
#'   markdown_output <- paste(unlist(methods_sections), collapse = "\n\n")
#'
#'   cli::cli_alert_success("Finished boilerplate_methods function \U0001F44D")
#'
#'   return(markdown_output)
#' }
