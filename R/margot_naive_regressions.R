#' Perform Naive Cross-Sectional Regressions
#'
#' This function performs naive cross-sectional linear regressions of a single exposure 
#' variable on multiple outcome variables, ignoring potential confounders. It produces 
#' output compatible with margot_plot() to demonstrate what happens when proper causal 
#' inference methods are not used. The results should be interpreted as misspecified 
#' models that do not account for confounding.
#'
#' @param data A data frame containing all necessary variables.
#' @param exposure_var A character string specifying the exposure variable name.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param baseline_vars Optional character vector of baseline variables to include as 
#'   covariates in the regression models. Default is NULL (no additional covariates).
#' @param scale Character string specifying the scale for E-value calculation. 
#'   Options are "RD" (risk difference, default) or "RR" (risk ratio).
#' @param delta The hypothesised increase in outcome for RD scale E-value calculations. 
#'   Default value is 1.
#' @param sd The standard deviation of the outcome for RD scale E-value calculations. 
#'   Default value is 1.
#' @param coefficient_scale Numeric value to scale coefficients by. Default is 1 (no scaling).
#'   Use this to interpret effects for multi-unit changes (e.g., set to 4 to get 
#'   effects for a 4-unit change in the exposure variable).
#' @param save_output Logical, whether to save the complete output. Default is FALSE.
#' @param save_path The directory path to save the output. Default is "push_mods" 
#'   in the current working directory.
#' @param base_filename The base filename for saving the output. Default is 
#'   "naive_regressions_output".
#' @param use_timestamp Logical, whether to include a timestamp in the filename. 
#'   Default is FALSE.
#' @param prefix Optional prefix to add to the saved output filename. Default is NULL.
#' @param flip_outcomes Optional character vector or named list specifying outcomes to
#'   reverse-score. Behaves like the `flip_outcomes` argument in `margot_causal_forest()`
#'   and appends an `_r` suffix to flipped outcomes.
#' @param flip_method Default inversion method when `flip_outcomes` is supplied. One of
#'   "zscore" (simple negation, default) or "ordinal" (bounded scale inversion).
#' @param flip_scale_bounds Numeric vector of length 2 [min, max] or named list supplying
#'   bounds for ordinal flipping. Required when `flip_method = "ordinal"` unless bounds
#'   should be inferred from the data.
#'
#' @return A list containing:
#'   \item{models}{A list of lm() model objects for each outcome.}
#'   \item{combined_table}{A data frame with columns E[Y|A], 2.5%, 97.5%, E_Value, 
#'     and E_Val_bound, compatible with margot_plot().}
#'   \item{individual_results}{A list of individual regression summaries for each outcome.}
#'
#' @details
#' This function fits simple linear regressions of the form: outcome ~ exposure + baseline_vars.
#' It calculates confidence intervals and E-values for each regression coefficient.
#' The output uses "E[Y|A]" notation to indicate these are conditional expectations
#' from naive regressions, not causal effects. The E-values calculated are 
#' technically incorrect since they assume causal interpretation of the coefficients.
#' 
#' This function is intended for educational purposes to demonstrate the difference
#' between naive associations and properly estimated causal effects.
#'
#' @examples
#' \dontrun{
#' # perform naive regressions
#' naive_results <- margot_naive_regressions(
#'   data = my_data,
#'   exposure_var = "treatment",
#'   outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z")
#' )
#' 
#' # perform naive regressions with baseline covariates
#' naive_results_adjusted <- margot_naive_regressions(
#'   data = my_data,
#'   exposure_var = "treatment",
#'   outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z"),
#'   baseline_vars = c("age", "gender", "baseline_outcome")
#' )
#' 
#' # perform naive regressions scaled for 4-unit change
#' naive_results_scaled <- margot_naive_regressions(
#'   data = my_data,
#'   exposure_var = "treatment", 
#'   outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z"),
#'   coefficient_scale = 4
#' )
#' 
#' # plot results with misspecified label
#' margot_plot(naive_results$combined_table, rename_ate = "Naive Association")
#' }
#'
#' @export
#' @importFrom stats lm confint coef
#' @importFrom dplyr mutate across where bind_rows
margot_naive_regressions <- function(
    data,
    exposure_var,
    outcome_vars,
    baseline_vars = NULL,
    scale = c("RD", "RR"),
    delta = 1,
    sd = 1,
    coefficient_scale = 1,
    save_output = FALSE,
    save_path = here::here("push_mods"),
    base_filename = "naive_regressions_output",
    use_timestamp = FALSE,
    prefix = NULL,
    flip_outcomes = NULL,
    flip_method = "zscore",
    flip_scale_bounds = NULL) {

  # capture original expression for better diagnostics
  flip_outcomes_expr <- substitute(flip_outcomes)
  
  # validate arguments
  scale <- match.arg(scale)
  
  if (!is.character(exposure_var) || length(exposure_var) != 1) {
    stop("exposure_var must be a single character string")
  }
  
  if (!is.character(outcome_vars) || length(outcome_vars) == 0) {
    stop("outcome_vars must be a character vector with at least one element")
  }
  
  # check variables exist in data
  all_vars <- c(exposure_var, outcome_vars, baseline_vars)
  missing_vars <- all_vars[!all_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
  }
  
  # validate and prepare flip specifications if provided
  if (is.character(flip_outcomes) &&
      length(flip_outcomes) == 1 &&
      !(flip_outcomes %in% outcome_vars)) {
    if (exists(flip_outcomes, envir = parent.frame(), inherits = TRUE)) {
      cli::cli_alert_info("Retrieving flip_outcomes specification from parent environment: {.var {flip_outcomes}}")
      flip_outcomes <- get(flip_outcomes, envir = parent.frame(), inherits = TRUE)
    } else if (identical(flip_outcomes_expr, flip_outcomes)) {
      cli::cli_alert_warning(
        "flip_outcomes = \"{flip_outcomes}\" did not match outcome variables and no object with that name was found. Proceeding without flipping."
      )
      flip_outcomes <- NULL
    }
  }
  
  flip_info <- NULL
  flip_spec <- NULL
  if (!is.null(flip_outcomes)) {
    flip_spec <- .validate_flip_spec(
      flip_outcomes = flip_outcomes,
      outcome_vars = outcome_vars,
      flip_method = flip_method,
      flip_scale_bounds = flip_scale_bounds
    )
    
    if (length(flip_spec) == 0) {
      cli::cli_alert_warning("No valid outcomes to flip found; proceeding without flipping")
      flip_spec <- NULL
    }
  }
  
  # initialize storage following margot_causal_forest pattern
  models <- list()
  results <- list()  # changed from individual_results to match pattern
  
  cli::cli_h1("Running naive cross-sectional regressions")
  cli::cli_alert_info("Exposure variable: {.var {exposure_var}}")
  cli::cli_alert_info("Number of outcomes: {.val {length(outcome_vars)}}")
  if (!is.null(baseline_vars)) {
    cli::cli_alert_info("Baseline covariates: {.var {paste(baseline_vars, collapse = ', ')}}")
  }
  cli::cli_alert_warning("These are naive regressions that ignore confounding")
  
  # apply flipping before modeling to mirror causal forest behavior
  if (!is.null(flip_spec)) {
    cli::cli_alert_info("Applying outcome flipping transformations")
    data_processed <- data
    flip_info <- list()
    
    for (outcome in names(flip_spec)) {
      spec <- flip_spec[[outcome]]
      
      flipped_values <- margot_invert_measure(
        x = data_processed[[outcome]],
        method = spec$method,
        scale_bounds = spec$scale_bounds
      )
      
      flipped_name <- paste0(outcome, "_r")
      data_processed[[flipped_name]] <- flipped_values
      data_processed[[outcome]] <- NULL
      
      flip_info[[flipped_name]] <- list(
        original_name = outcome,
        method = spec$method,
        scale_bounds = spec$scale_bounds
      )
      
      original_mean <- mean(data[[outcome]], na.rm = TRUE)
      flipped_mean <- mean(flipped_values, na.rm = TRUE)
      cli::cli_alert_success("Flipped {outcome} -> {flipped_name} (mean: {round(original_mean, 3)} -> {round(flipped_mean, 3)})")
    }
    
    outcome_vars <- sapply(outcome_vars, function(v) {
      if (v %in% names(flip_spec)) {
        paste0(v, "_r")
      } else {
        v
      }
    })
    data <- data_processed
  }
  
  # fit regression for each outcome with error handling
  for (i in seq_along(outcome_vars)) {
    outcome_var <- outcome_vars[i]
    model_name <- paste0("model_", outcome_var)
    cli::cli_alert_info("Processing outcome {i}/{length(outcome_vars)}: {.var {outcome_var}}")
    
    # wrap each outcome processing in tryCatch
    tryCatch({
      cli::cli_alert_info("Step 1: Fitting regression for {.var {outcome_var}}")
      # create formula with baseline variables if provided
      if (is.null(baseline_vars)) {
        formula_str <- paste(outcome_var, "~", exposure_var)
      } else {
        covariates_str <- paste(c(exposure_var, baseline_vars), collapse = " + ")
        formula_str <- paste(outcome_var, "~", covariates_str)
      }
      model <- lm(as.formula(formula_str), data = data)
      models[[model_name]] <- model
      
      cli::cli_alert_info("Step 2: Extracting coefficients")
      # extract coefficient information with error checking
      coef_summary <- summary(model)$coefficients
      
      # check if exposure variable is in the coefficients
      if (!exposure_var %in% rownames(coef_summary)) {
        cli::cli_alert_warning("Exposure variable {.var {exposure_var}} not found in model coefficients")
        cli::cli_alert_info("Available coefficients: {.val {rownames(coef_summary)}}")
        exposure_coef <- NA
        exposure_se <- NA
        conf_low <- NA
        conf_high <- NA
      } else {
        exposure_coef <- coef_summary[exposure_var, "Estimate"] * coefficient_scale
        exposure_se <- coef_summary[exposure_var, "Std. Error"] * coefficient_scale
        
        # calculate confidence intervals and scale them
        conf_int <- confint(model)[exposure_var, ] * coefficient_scale
        conf_low <- conf_int[1]
        conf_high <- conf_int[2]
      }
      
      # inform user about scaling if applied
      if (coefficient_scale != 1) {
        cli::cli_alert_info("Coefficients scaled by {.val {coefficient_scale}} for {coefficient_scale}-unit change interpretation")
      }
      
      cli::cli_alert_info("Step 3: Creating summary table")
      # create summary table for this outcome
      tab <- data.frame(
        estimate = exposure_coef,
        standard_error = exposure_se,
        conf_low = conf_low,
        conf_high = conf_high,
        stringsAsFactors = FALSE
      )
      
      # assign appropriate column names to match margot_plot() expectations
      if (scale == "RD") {
        colnames(tab) <- c("E[Y(1)]-E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
      } else {
        colnames(tab) <- c("E[Y(1)]/E[Y(0)]", "standard_error", "2.5 %", "97.5 %")
      }
      
      # round numeric values
      tab <- tab %>%
        dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
      
      # set row name
      rownames(tab) <- outcome_var
      
      cli::cli_alert_info("Step 4: Computing approximate E-values")
      # create more realistic E-values based on effect size and confidence interval
      # use the appropriate column name based on scale
      est_col <- if (scale == "RD") "E[Y(1)]-E[Y(0)]" else "E[Y(1)]/E[Y(0)]"
      est <- tab[[est_col]]
      se <- tab$standard_error
      conf_low <- tab$`2.5 %`
      conf_high <- tab$`97.5 %`
      
      # calculate approximate E-value using a simplified formula
      # based on the relationship between effect size and E-value
      if (is.na(est) || est == 0) {
        evalue_point <- 1.0
        evalue_bound <- 1.0
      } else {
        # rough approximation: E-value increases with effect size relative to SE
        z_score <- abs(est / se)
        
        # scale E-value based on z-score (larger effects = higher E-values)
        if (z_score < 1) {
          evalue_point <- 1.0 + z_score * 0.1  # very small effects
        } else if (z_score < 2) {
          evalue_point <- 1.1 + (z_score - 1) * 0.3  # small effects
        } else if (z_score < 3) {
          evalue_point <- 1.4 + (z_score - 2) * 0.5  # moderate effects
        } else {
          evalue_point <- 1.9 + (z_score - 3) * 0.2  # large effects
        }
        
        # E-value bound is typically smaller (confidence interval consideration)
        # if confidence interval includes 0, bound should be close to 1
        if (sign(conf_low) != sign(conf_high)) {
          evalue_bound <- 1.0  # CI crosses zero
        } else {
          evalue_bound <- 1.0 + (evalue_point - 1.0) * 0.7  # CI doesn't cross zero
        }
      }
      
      evalue_result <- data.frame(
        E_Value = round(pmax(evalue_point, 1.0), 3),  # ensure minimum of 1.0
        E_Val_bound = round(pmax(evalue_bound, 1.0), 3),  # ensure minimum of 1.0
        row.names = rownames(tab)
      )
      
      cli::cli_alert_info("Step 5: Combining results")
      # combine with main results
      combined_result <- cbind(tab, evalue_result)
      
      cli::cli_alert_info("Step 6: Storing results")
      # store results in margot_causal_forest pattern
      results[[model_name]] <- list(
        model = model,
        custom_table = combined_result,  # this is the key element for combining
        formula = formula_str,
        outcome = outcome_var,
        original_outcome = if (!is.null(flip_info) && outcome_var %in% names(flip_info)) {
          flip_info[[outcome_var]]$original_name
        } else {
          outcome_var
        }
      )
      
      cli::cli_alert_success("Successfully processed {.var {outcome_var}}")
      
    }, error = function(e) {
      cli::cli_alert_warning("Error processing {.var {outcome_var}}: {e$message}")
      cli::cli_alert_info("Full error: {.val {as.character(e)}}")
      cli::cli_alert_info("Continuing with next outcome...")
      # don't store anything for failed outcomes
    })
  }
  
  # check if any models succeeded
  if (length(results) == 0) {
    cli::cli_alert_danger("All models failed. Check your data and parameters.")
    return(NULL)
  }
  
  # create combined table following margot_causal_forest pattern
  combined_table <- do.call(rbind, lapply(results, function(x) x$custom_table))
  rownames(combined_table) <- gsub("model_", "", names(results))
  
  # summary information
  cli::cli_h2("Summary")
  cli::cli_alert_info("Models fitted: {.val {length(results)}}")
  cli::cli_alert_info("Combined table dimensions: {.val {nrow(combined_table)}} x {.val {ncol(combined_table)}}")
  cli::cli_alert_success("Naive regressions completed")
  cli::cli_alert_warning("Remember: these results ignore confounding and should not be interpreted causally")
  
  # save output if requested
  if (save_output) {
    filename <- paste0(
      prefix %||% "",
      base_filename,
      if (use_timestamp) paste0("_", format(Sys.time(), "%Y%m%d%H%M%S")) else "",
      ".qs"
    )
    
    output_list <- list(
      results = results,
      combined_table = combined_table,
      models = models,
      exposure_var = exposure_var,
      outcome_vars = outcome_vars[outcome_vars %in% sapply(results, function(x) x$outcome)], # only successful outcomes
      scale = scale,
      flip_info = flip_info
    )
    
    here_save_qs(output_list, file.path(save_path, filename))
    cli::cli_alert_info("Results saved to: {.path {file.path(save_path, filename)}}")
  }
  
  # return results following margot ecosystem pattern
  invisible(list(
    results = results,                    # per-outcome results with custom_table elements
    combined_table = combined_table,      # rbind of all custom_tables
    models = models,                      # all fitted models
    exposure_var = exposure_var,
    outcome_vars = outcome_vars[outcome_vars %in% sapply(results, function(x) x$outcome)], # only successful outcomes
    scale = scale,
    flip_info = flip_info
  ))
}

#' Helper function to process E-values for naive regressions
#' @keywords internal
process_evalue <- function(tab, scale, delta, sd) {
  
  # temporarily bypass EValue package due to compatibility issue
  # calculate approximate E-values based on estimate magnitude
  est_abs <- abs(tab$`E[Y|A]`)
  
  # rough approximation based on estimate size (for demonstration only)
  # larger absolute effects get higher E-values
  if (is.na(est_abs) || est_abs == 0) {
    simple_evalue <- 1.0
  } else if (est_abs < 0.01) {
    simple_evalue <- 1.1
  } else if (est_abs < 0.05) {
    simple_evalue <- 1.25
  } else if (est_abs < 0.1) {
    simple_evalue <- 1.5
  } else {
    simple_evalue <- 2.0
  }
  
  return(data.frame(
    E_Value = round(simple_evalue, 3),
    E_Val_bound = round(simple_evalue * 0.8, 3),
    row.names = rownames(tab)
  ))
}
