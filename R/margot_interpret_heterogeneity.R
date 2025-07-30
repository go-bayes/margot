#' Interpret Heterogeneity Evidence from Multiple Sources
#'
#' @description
#' Combines evidence from multiple heterogeneity tests (RATE AUTOC, RATE QINI, 
#' QINI curves, and omnibus calibration tests) to provide unified recommendations 
#' about which models show treatment effect heterogeneity.
#'
#' @param models Output from `margot_causal_forest()` containing model results
#' @param model_names Character vector of model names to analyse. If NULL (default), 
#'   analyses all models. Model names can be specified with or without "model_" prefix.
#' @param spend_levels Numeric vector of spend levels for QINI analysis. 
#'   Default is c(0.1, 0.4).
#' @param require_any_positive Logical. If TRUE (default), include models that 
#'   show positive evidence in ANY method. If FALSE, require positive evidence 
#'   in ALL methods.
#' @param exclude_negative_any Logical. If TRUE (default), exclude models that 
#'   show negative evidence in ANY RATE test (AUTOC or QINI). Models with any 
#'   negative RATE evidence are classified as "excluded_negative_rate" and will 
#'   not appear in selected or exploratory lists.
#' @param require_omnibus Logical. If TRUE, only include models that pass the 
#'   omnibus calibration test. Default is FALSE.
#' @param alpha Numeric. Significance level for RATE tests. Default is 0.05.
#'   Note: this controls which RATE estimates are considered statistically significant after
#'   multiple testing correction.
#' @param adjust Character. Multiple testing adjustment method for RATE estimates. 
#'   Options include "BH" (Benjamini-Hochberg), "BY" (Benjamini-Yekutieli), 
#'   "bonferroni", "holm", "fdr", or "none". Default is "none".
#'   Note: When use_cross_validation = TRUE (the default), only "bonferroni" or "none" are valid.
#'   Invalid methods will be automatically converted to "none" without warning.
#' @param flipped_outcomes Character vector of outcome names that were flipped 
#'   (reversed) in preprocessing. Used for interpretation text.
#' @param label_mapping Named list for mapping model names to human-readable labels.
#' @param verbose Logical. If TRUE, show progress messages. Default is TRUE.
#' @param include_extended_report Logical. If TRUE (default), generate detailed academic-style report
#'   with full statistics and confidence intervals.
#' @param rate_results Optional pre-computed RATE results to skip computation.
#' @param qini_results Optional pre-computed QINI results to skip computation.
#' @param omnibus_results Optional pre-computed omnibus test results to skip computation.
#' @param use_cross_validation Logical. If TRUE (default), use cross-validation for RATE tests
#'   instead of standard approach. This provides confidence intervals through robust inference.
#' @param cv_num_folds Integer. Number of CV folds when use_cross_validation = TRUE (default 5).
#' @param cv_results Optional pre-computed CV results to skip computation.
#' @param seed Integer. Random seed for reproducibility in all computations (default 12345).
#' @param parallel Logical. Use parallel processing for cross-validation when use_cross_validation = TRUE 
#'   (default FALSE). Note: Parallel processing is experimental and may encounter memory issues.
#' @param n_cores Integer. Number of cores for parallel processing when parallel = TRUE 
#'   (default all cores - 1). Only applies when use_cross_validation = TRUE.
#'
#' @return A list containing:
#'   \item{selected_model_ids}{Character vector of model IDs with heterogeneity evidence}
#'   \item{selected_model_names}{Character vector of human-readable model names}
#'   \item{exploratory_model_ids}{Character vector of model IDs with exploratory evidence (positive calibration or QINI curve, no negative RATE)}
#'   \item{exploratory_model_names}{Character vector of human-readable model names with exploratory evidence}
#'   \item{all_selected_model_ids}{Combined vector of selected_model_ids and exploratory_model_ids}
#'   \item{all_selected_model_names}{Combined vector of selected_model_names and exploratory_model_names}
#'   \item{excluded_model_ids}{Character vector of model IDs to exclude}
#'   \item{excluded_model_names}{Character vector of human-readable excluded model names}
#'   \item{evidence_summary}{Data frame with detailed evidence by source. Contains columns: model_id, model_name, category (selected/excluded/unclear), mean_prediction_test (calibration status), differential_prediction_test (heterogeneity test), rate_autoc, rate_qini, qini_curve, positive_count (backwards compatibility: same as rate_positive_count), negative_count (backwards compatibility: same as rate_negative_count), rate_positive_count (positive RATE tests only), rate_negative_count (negative RATE tests only), total_positive_count (across all 4 tests), total_negative_count (across all 4 tests), is_excluded (1 if any negative RATE test), strict_inclusion_count (positive RATE tests only if no negative RATE), selection_source (excluded/rate_only/qini_curve_only/both_rate_and_qini/none), has_negative_rate, has_positive_rate, has_positive_qini_curve. Note: mean_prediction_test indicates calibration quality but is not included in heterogeneity scoring}
#'   \item{interpretation}{Character string with main interpretation text organized by evidence categories}
#'   \item{summary}{Character string with brief summary}
#'   \item{recommendations}{Character string with actionable recommendations}
#'   \item{rate_results}{List containing AUTOC and QINI RATE results, interpretation, and raw_results from margot_rate() or margot_rate_cv()}
#'   \item{qini_results}{QINI curve interpretation results}
#'   \item{omnibus_results}{Omnibus calibration test results}
#'   \item{concordance}{List analysing agreement between methods}
#'   \item{extended_report}{Character string with detailed academic report (if include_extended_report = TRUE)}
#'   \item{cv_results}{Cross-validation results object (if use_cross_validation = TRUE) that can be passed to margot_plot_cv_results() and margot_plot_cv_summary()}
#'   \item{method_used}{Character string indicating whether "cross_validation" or "standard" method was used}
#'
#' @examples
#' \dontrun{
#' # Simple usage - let the function handle everything
#' het_evidence <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   spend_levels = c(0.1, 0.4),
#'   flipped_outcomes = c("anxiety", "depression")
#' )
#' 
#' # Analyze specific models only
#' het_evidence_subset <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   model_names = c("t2_depression_z", "t2_anxiety_z"),
#'   spend_levels = c(0.1, 0.4)
#' )
#' 
#' # View interpretation
#' cat(het_evidence$interpretation)
#' 
#' # Use selected models for targeting
#' policy_results <- margot_policy(
#'   causal_forest_results,
#'   model_names = het_evidence$selected_model_ids
#' )
#' 
#' # Use model_names parameter with selected models
#' het_focused <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   model_names = het_evidence$selected_model_ids,
#'   include_extended_report = TRUE
#' )
#' 
#' # Advanced usage with pre-computed results
#' het_evidence <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   rate_results = my_rate_results,
#'   require_omnibus = TRUE
#' )
#' 
#' # Using cross-validation instead of standard RATE
#' het_evidence_cv <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   use_cross_validation = TRUE,
#'   cv_num_folds = 5,
#'   alpha = 0.2,  # Higher alpha recommended for Bonferroni with CV
#'   adjust = "bonferroni",
#'   parallel = TRUE,  # Enable parallel processing for faster CV
#'   n_cores = 4
#' )
#' 
#' # Plot CV results without recomputing
#' if (!is.null(het_evidence_cv$cv_results)) {
#'   plot <- margot_plot_cv_results(het_evidence_cv$cv_results)
#'   summary_plot <- margot_plot_cv_summary(het_evidence_cv$cv_results)
#' }
#' 
#' # Use standard RATE plotting functions with the raw results
#' # This works for both standard and CV methods
#' rate_results <- het_evidence$rate_results$raw_results
#' 
#' # Plot using standard functions
#' plot_autoc <- margot_plot_rate(rate_results$rate_autoc, target = "AUTOC")
#' plot_qini <- margot_plot_rate(rate_results$rate_qini, target = "QINI")
#' plot_batch <- margot_plot_rate_batch(rate_results)
#' }
#'
#' @export
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_progress_step cli_progress_done
#' @importFrom dplyr bind_rows mutate case_when
#' @importFrom tibble tibble
#' @importFrom future availableCores
margot_interpret_heterogeneity <- function(
  models = NULL,
  model_names = NULL,
  spend_levels = c(0.1, 0.4),
  require_any_positive = TRUE,
  exclude_negative_any = TRUE,
  require_omnibus = FALSE,
  alpha = 0.05,
  adjust = "none",
  flipped_outcomes = NULL,
  label_mapping = NULL,
  verbose = TRUE,
  include_extended_report = TRUE,
  rate_results = NULL,
  qini_results = NULL,
  omnibus_results = NULL,
  use_cross_validation = TRUE,
  cv_num_folds = 5,
  cv_results = NULL,
  seed = 12345,
  parallel = FALSE,
  n_cores = future::availableCores() - 1
) {
  
  # validate inputs
  if (is.null(models) && (is.null(rate_results) || is.null(qini_results))) {
    stop("Either 'models' or pre-computed results must be provided")
  }
  
  # filter models if model_names is specified
  if (!is.null(model_names) && !is.null(models)) {
    # ensure model names have the "model_" prefix
    model_names_prefixed <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    
    # filter the models
    available_models <- names(models$results)
    requested_models <- intersect(model_names_prefixed, available_models)
    
    if (length(requested_models) == 0) {
      stop("None of the specified model names were found in the results. Available models: ", 
           paste(available_models, collapse = ", "))
    }
    
    if (length(requested_models) < length(model_names_prefixed)) {
      missing_models <- setdiff(model_names_prefixed, available_models)
      if (verbose) {
        cli::cli_alert_warning("Some requested models were not found: {missing_models}")
      }
    }
    
    # create filtered models object
    models_filtered <- models
    models_filtered$results <- models$results[requested_models]
    if (!is.null(models$full_models)) {
      models_filtered$full_models <- models$full_models[intersect(requested_models, names(models$full_models))]
    }
    models <- models_filtered
  }
  
  # filter pre-computed results if model_names is specified
  if (!is.null(model_names) && !is.null(rate_results)) {
    # ensure model names have the "model_" prefix
    model_names_prefixed <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    
    # filter rate results if they contain model lists
    if (!is.null(rate_results$autoc_model_names)) {
      rate_results$autoc_model_names <- intersect(rate_results$autoc_model_names, model_names_prefixed)
    }
    if (!is.null(rate_results$qini_model_names)) {
      rate_results$qini_model_names <- intersect(rate_results$qini_model_names, model_names_prefixed)
    }
    if (!is.null(rate_results$not_excluded_autoc_model_names)) {
      rate_results$not_excluded_autoc_model_names <- intersect(rate_results$not_excluded_autoc_model_names, model_names_prefixed)
    }
    if (!is.null(rate_results$not_excluded_qini_model_names)) {
      rate_results$not_excluded_qini_model_names <- intersect(rate_results$not_excluded_qini_model_names, model_names_prefixed)
    }
  }
  
  # filter qini results if model_names is specified
  if (!is.null(model_names) && !is.null(qini_results)) {
    # ensure model names have the "model_" prefix
    model_names_prefixed <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    
    # filter qini results if they contain model lists
    if (!is.null(qini_results$reliable_model_ids)) {
      qini_results$reliable_model_ids <- intersect(qini_results$reliable_model_ids, model_names_prefixed)
    }
    if (!is.null(qini_results$harmful_model_ids)) {
      qini_results$harmful_model_ids <- intersect(qini_results$harmful_model_ids, model_names_prefixed)
    }
    if (!is.null(qini_results$no_effect_model_ids)) {
      qini_results$no_effect_model_ids <- intersect(qini_results$no_effect_model_ids, model_names_prefixed)
    }
  }
  
  # header
  if (verbose) cli::cli_h2("Computing heterogeneity evidence")
  
  # Count tasks to perform
  n_tasks <- 0
  if (is.null(rate_results) && !is.null(models)) n_tasks <- n_tasks + 1
  if (is.null(qini_results) && !is.null(models)) n_tasks <- n_tasks + 1
  if (is.null(omnibus_results) && !is.null(models)) n_tasks <- n_tasks + 1
  n_tasks <- n_tasks + 1  # For combining evidence
  
  # Progress tracking
  current_task <- 0
  
  # initialize rate_results_list
  rate_results_list <- NULL
  
  # 1. compute rate results if not provided
  if (is.null(rate_results) && !is.null(models)) {
    current_task <- current_task + 1
    if (verbose) {
      if (use_cross_validation) {
        # Don't use progress_step for CV as margot_rate_cv has its own progress
        cli::cli_alert_info("[{current_task}/{n_tasks}] Computing cross-validation heterogeneity tests")
      } else {
        # Use progress_step for standard RATE
        cli::cli_progress_step(
          "[{current_task}/{n_tasks}] Computing RATE estimates",
          msg_done = "[{current_task}/{n_tasks}] Computing RATE estimates ... done"
        )
      }
    }
    
    if (use_cross_validation) {
      # Use cross-validation approach
      if (is.null(cv_results)) {
        # For CV, only "bonferroni" or "none" are valid
        cv_adjust <- adjust
        if (!adjust %in% c("bonferroni", "none")) {
          # Only warn if user explicitly set a non-default adjustment
          if (adjust != "BH" && verbose) {
            cli::cli_alert_warning(
              "Adjustment method '{adjust}' is not valid for cross-validation. Using 'none' instead."
            )
          }
          cv_adjust <- "none"
        }
        
        cv_results <- margot_rate_cv(
          model_results = models,
          num_folds = cv_num_folds,
          target = c("AUTOC", "QINI"),  # Test both targets
          alpha = alpha,
          adjust = cv_adjust,
          verbose = verbose,
          seed = seed,
          parallel = parallel,
          n_cores = n_cores,
          label_mapping = label_mapping
        )
      }
      
      # Convert CV results to rate_results format for compatibility
      rate_results <- convert_cv_to_rate_results(cv_results, flipped_outcomes)
      rate_results_list <- cv_results  # Store for later reference
      
    } else {
      # Standard RATE approach
      # compute rate - margot_rate() handles adjustment internally
      rate_results_list <- margot_rate(
        models, 
        adjust = adjust, 
        alpha = alpha,
        apply_adjustment = TRUE,
        seed = seed
      )
      
      # get interpretation
      rate_results <- margot_interpret_rate(
        rate_results_list,
        flipped_outcomes = flipped_outcomes,
        adjust_positives_only = TRUE
      )
    }
  }
  
  # 2. compute qini results if not provided
  if (is.null(qini_results) && !is.null(models)) {
    current_task <- current_task + 1
    if (verbose) {
      cli::cli_progress_step(
        "[{current_task}/{n_tasks}] Computing QINI curves",
        msg_done = "[{current_task}/{n_tasks}] Computing QINI curves ... done"
      )
    }
    
    # run margot_policy to get diff_gain_summaries
    qini_batch <- margot_policy(
      models,
      spend_levels = spend_levels,
      output_objects = c("diff_gain_summaries"),
      seed = seed
    )
    
    # get interpretation
    qini_results <- margot_interpret_qini(
      qini_batch,
      spend_levels = spend_levels,
      label_mapping = label_mapping
    )
  }
  
  # 3. compute omnibus test if not provided
  if (is.null(omnibus_results) && !is.null(models)) {
    current_task <- current_task + 1
    if (verbose) {
      cli::cli_progress_step(
        "[{current_task}/{n_tasks}] Computing omnibus calibration tests",
        msg_done = "[{current_task}/{n_tasks}] Computing omnibus calibration tests ... done"
      )
    }
    # extract all outcome names from the models, including flipped ones
    all_outcome_names <- gsub("^model_", "", names(models$results))
    omnibus_results <- margot_omnibus_hetero_test(
      models, 
      outcome_vars = all_outcome_names,
      label_mapping = label_mapping
    )
  }
  
  # 4. combine evidence
  current_task <- current_task + 1
  if (verbose) {
    cli::cli_progress_step(
      "[{current_task}/{n_tasks}] Combining heterogeneity evidence",
      msg_done = "[{current_task}/{n_tasks}] Combining heterogeneity evidence ... done"
    )
  }
  
  # extract all model ids - filter if model_names was specified
  all_model_ids <- unique(c(
    if (!is.null(models)) names(models$results) else character(0),
    rate_results$autoc_model_names,
    rate_results$qini_model_names,
    rate_results$not_excluded_autoc_model_names,
    rate_results$not_excluded_qini_model_names,
    qini_results$reliable_model_ids,
    qini_results$harmful_model_ids,
    qini_results$no_effect_model_ids
  ))
  
  # if model_names was specified, only include those models
  if (!is.null(model_names)) {
    model_names_prefixed <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    all_model_ids <- intersect(all_model_ids, model_names_prefixed)
  }
  
  # create evidence summary
  evidence_summary <- create_evidence_summary(
    all_model_ids,
    rate_results,
    qini_results,
    omnibus_results,
    label_mapping,
    use_cross_validation,
    rate_results_list
  )
  
  # select models based on criteria
  selection_results <- select_models(
    evidence_summary,
    require_any_positive,
    exclude_negative_any,
    require_omnibus,
    verbose
  )
  
  # use the updated evidence_summary from selection_results
  evidence_summary <- selection_results$evidence_summary
  
  # generate interpretation text
  interpretation_text <- generate_interpretation(
    evidence_summary,
    selection_results,
    rate_results,
    qini_results,
    omnibus_results,
    spend_levels
  )
  
  # create concordance analysis
  concordance <- analyse_concordance(evidence_summary)
  
  # generate extended report if requested
  extended_report <- NULL
  if (include_extended_report) {
    if (verbose) {
      cli::cli_progress_step(
        "Generating extended report",
        msg_done = "Generating extended report ... done"
      )
    }
    # use the evidence_summary which now includes model_id
    evidence_summary_with_id <- selection_results$evidence_summary
    extended_report <- generate_extended_report(
      evidence_summary_with_id,
      selection_results,
      rate_results_list,
      rate_results,
      qini_results,
      omnibus_results,
      models,
      spend_levels,
      label_mapping,
      alpha,
      adjust,
      use_cross_validation,
      cv_num_folds,
      cv_results
    )
  }
  
  # return results
  if (verbose) cli::cli_alert_success("Heterogeneity analysis complete")
  
  list(
    selected_model_ids = selection_results$selected_ids,
    selected_model_names = selection_results$selected_names,
    excluded_model_ids = selection_results$excluded_ids,
    excluded_model_names = selection_results$excluded_names,
    unclear_model_ids = selection_results$unclear_ids,
    unclear_model_names = selection_results$unclear_names,
    # for backwards compatibility
    exploratory_model_ids = character(),
    exploratory_model_names = character(),
    all_selected_model_ids = selection_results$selected_ids,
    all_selected_model_names = selection_results$selected_names,
    evidence_summary = evidence_summary,
    interpretation = interpretation_text$full,
    summary = interpretation_text$summary,
    recommendations = interpretation_text$recommendations,
    rate_results = list(
      autoc = if (use_cross_validation && !is.null(rate_results_list)) {
        # For CV results, use the formatted tables
        if (!is.null(rate_results_list$tables) && !is.null(rate_results_list$tables$rate_autoc)) {
          rate_results_list$tables$rate_autoc
        } else NULL
      } else if (!is.null(rate_results_list) && !is.null(rate_results_list$rate_autoc)) {
        rate_results_list$rate_autoc
      } else NULL,
      qini = if (use_cross_validation && !is.null(rate_results_list)) {
        # For CV results, use the formatted tables
        if (!is.null(rate_results_list$tables) && !is.null(rate_results_list$tables$rate_qini)) {
          rate_results_list$tables$rate_qini
        } else NULL
      } else if (!is.null(rate_results_list) && !is.null(rate_results_list$rate_qini)) {
        rate_results_list$rate_qini
      } else NULL,
      interpretation = rate_results,
      raw_results = rate_results_list  # export the full raw results from margot_rate() or margot_rate_cv()
    ),
    cv_results = if (use_cross_validation && !is.null(rate_results_list)) rate_results_list else NULL,
    qini_results = qini_results,
    omnibus_results = omnibus_results,
    concordance = concordance,
    positive_counts = selection_results$positive_counts,
    extended_report = extended_report,
    method_used = if (use_cross_validation) "cross_validation" else "standard"
  )
}


#' Create evidence summary table
#' @keywords internal
create_evidence_summary <- function(model_ids, rate_results, qini_results, 
                                    omnibus_results, label_mapping, 
                                    use_cross_validation = FALSE, rate_results_list = NULL) {
  
  # helper to get model name
  get_model_name <- function(id) {
    if (!is.null(label_mapping)) {
      # try exact match
      if (id %in% names(label_mapping)) {
        return(label_mapping[[id]])
      }
      # try without model_ prefix
      clean_id <- sub("^model_", "", id)
      if (clean_id %in% names(label_mapping)) {
        return(label_mapping[[clean_id]])
      }
    }
    # default transformation
    clean_id <- sub("^model_", "", id)
    
    # check if reversed (ends with _r or _z_r)
    is_reversed <- grepl("_r$|_z_r$", clean_id)
    
    # remove suffixes
    clean_id <- sub("_z_r$", "", clean_id)  # remove _z_r suffix
    clean_id <- sub("_r$", "", clean_id)    # remove _r suffix
    clean_id <- sub("_z$", "", clean_id)    # remove _z suffix
    clean_id <- sub("^t[0-9]_", "", clean_id)  # remove wave prefix like t2_
    clean_id <- gsub("_", " ", clean_id)   # then replace underscores
    
    # special cases for common abbreviations
    clean_id <- gsub("hlth", "health", clean_id, ignore.case = TRUE)
    clean_id <- gsub("bmi", "BMI", clean_id, ignore.case = TRUE)
    clean_id <- gsub("pwi", "PWI", clean_id, ignore.case = TRUE)
    
    # apply title case
    clean_id <- tools::toTitleCase(clean_id)
    
    # add (reduced) prefix if reversed
    if (is_reversed) {
      clean_id <- paste0("(reduced) ", clean_id)
    }
    
    clean_id
  }
  
  # build summary for each model
  summary_list <- lapply(model_ids, function(id) {
    
    # For CV results, we need to check the tables directly for statistically significant negative
    if (use_cross_validation && !is.null(rate_results_list$tables)) {
      # Check AUTOC status
      if (!is.null(rate_results_list$tables$rate_autoc)) {
        autoc_row <- rate_results_list$tables$rate_autoc[rate_results_list$tables$rate_autoc$model_id == id, ]
        if (nrow(autoc_row) > 0) {
          rate_autoc_status <- autoc_row$Status[1]
        } else {
          rate_autoc_status <- "not_tested"
        }
      } else {
        rate_autoc_status <- "not_tested"
      }
      
      # Check QINI status
      if (!is.null(rate_results_list$tables$rate_qini)) {
        qini_row <- rate_results_list$tables$rate_qini[rate_results_list$tables$rate_qini$model_id == id, ]
        if (nrow(qini_row) > 0) {
          rate_qini_status <- qini_row$Status[1]
        } else {
          rate_qini_status <- "not_tested"
        }
      } else {
        rate_qini_status <- "not_tested"
      }
    } else {
      # Standard RATE approach - original logic
      rate_autoc_status <- case_when(
        id %in% rate_results$autoc_model_names ~ "positive",
        id %in% rate_results$excluded_either ~ "negative", 
        id %in% rate_results$not_excluded_autoc_model_names ~ "inconclusive",
        TRUE ~ "not_tested"
      )
      
      rate_qini_status <- case_when(
        id %in% rate_results$qini_model_names ~ "positive",
        id %in% rate_results$excluded_either ~ "negative",
        id %in% rate_results$not_excluded_qini_model_names ~ "inconclusive",
        TRUE ~ "not_tested"
      )
    }
    
    # qini curve status
    qini_curve_status <- case_when(
      id %in% qini_results$reliable_model_ids ~ "positive",
      id %in% qini_results$harmful_model_ids ~ "negative",
      id %in% qini_results$no_effect_model_ids ~ "inconclusive",
      TRUE ~ "not_tested"
    )
    
    # omnibus test results - both mean and differential prediction
    mean_pred_status <- "not_tested"
    diff_pred_status <- "not_tested"
    
    if (!is.null(omnibus_results)) {
      # extract model name without prefix
      clean_id <- sub("^model_", "", id)
      
      # try to match using the results_table which has the original outcome names
      if (!is.null(omnibus_results$results_table) && "outcome" %in% names(omnibus_results$results_table)) {
        # direct match on outcome name
        omnibus_row <- omnibus_results$results_table[omnibus_results$results_table$outcome == clean_id, ]
        
        if (!is.null(omnibus_row) && nrow(omnibus_row) > 0) {
          # check mean prediction statistical significance for calibration
          if ("mean_prediction_significant" %in% names(omnibus_row)) {
            if (isTRUE(omnibus_row$mean_prediction_significant[1])) {
              mean_pred_status <- "calibrated"
            } else {
              mean_pred_status <- "not_calibrated"
            }
          }
          
          # check differential prediction statistical significance for heterogeneity
          if ("differential_prediction_significant" %in% names(omnibus_row)) {
            if (isTRUE(omnibus_row$differential_prediction_significant[1])) {
              diff_pred_status <- "positive"
            } else {
              diff_pred_status <- "inconclusive"
            }
          }
        }
      } else if (!is.null(omnibus_results$summary_table)) {
        # fallback to matching on display name in summary table
        model_display_name <- get_model_name(id)
        omnibus_row <- omnibus_results$summary_table[
          omnibus_results$summary_table$Outcome == model_display_name,
        ]
        
        if (!is.null(omnibus_row) && nrow(omnibus_row) > 0) {
          # check mean status column
          mean_col <- names(omnibus_row)[grep("Mean Status", names(omnibus_row))]
          if (length(mean_col) > 0 && !is.na(omnibus_row[[mean_col[1]]][1])) {
            if (grepl("Calibrated", omnibus_row[[mean_col[1]]][1], ignore.case = TRUE)) {
              mean_pred_status <- "calibrated"
            } else {
              mean_pred_status <- "not_calibrated"
            }
          }
          
          # check heterogeneity column
          hetero_col <- names(omnibus_row)[grep("Heterogeneity|heterogeneity", names(omnibus_row))]
          if (length(hetero_col) > 0 && !is.na(omnibus_row[[hetero_col[1]]][1])) {
            if (grepl("Heterogeneity present|present", omnibus_row[[hetero_col[1]]][1])) {
              diff_pred_status <- "positive"
            } else {
              diff_pred_status <- "inconclusive"
            }
          }
        }
      }
    }
    
    tibble::tibble(
      model_id = id,  # keep for internal use but will remove later
      model_name = get_model_name(id),
      mean_prediction_test = mean_pred_status,
      differential_prediction_test = diff_pred_status,
      rate_autoc = rate_autoc_status,
      rate_qini = rate_qini_status,
      qini_curve = qini_curve_status
    )
  })
  
  dplyr::bind_rows(summary_list)
}


#' Select models based on criteria
#' @keywords internal
select_models <- function(evidence_summary, require_any_positive, 
                          exclude_negative_any, require_omnibus, verbose = TRUE) {
  
  # add the enhanced selection system that includes QINI curves
  evidence_summary <- evidence_summary %>%
    dplyr::mutate(
      # Determine if significantly negative RATE
      has_negative_rate = (rate_autoc == "negative" | rate_qini == "negative"),
      
      # Determine if significantly positive RATE  
      has_positive_rate = (rate_autoc == "positive" | rate_qini == "positive"),
      
      # Check if QINI curves show positive evidence (only for non-excluded)
      has_positive_qini_curve = (!has_negative_rate & qini_curve == "positive"),
      
      # Enhanced selection system
      category = dplyr::case_when(
        # EXCLUDED: Any statistically significant negative RATE test
        has_negative_rate ~ "excluded",
        
        # SELECTED: Any statistically significant positive RATE test OR positive QINI curves
        has_positive_rate | has_positive_qini_curve ~ "selected",
        
        # UNCLEAR: Neither positive RATE nor positive QINI curves
        TRUE ~ "unclear"
      ),
      
      # Track selection source for reporting
      selection_source = dplyr::case_when(
        has_negative_rate ~ "excluded",
        has_positive_rate & has_positive_qini_curve ~ "both_rate_and_qini",
        has_positive_rate ~ "rate_only", 
        has_positive_qini_curve ~ "qini_curve_only",
        TRUE ~ "none"
      ),
      
      # Renamed counts for RATE tests only (backwards compatibility)
      rate_positive_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini) == "positive"
      ),
      rate_negative_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini) == "negative"
      ),
      
      # Comprehensive counts across all 4 tests
      total_positive_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini, qini_curve, differential_prediction_test) == "positive"
      ),
      total_negative_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini, qini_curve, differential_prediction_test) == "negative"
      ),
      
      # Exclusion/inclusion indicators
      is_excluded = as.numeric(has_negative_rate),
      
      # Strict inclusion: count positive RATE tests only if neither is negative
      strict_inclusion_count = dplyr::case_when(
        has_negative_rate ~ 0L,
        TRUE ~ as.integer(rowSums(dplyr::select(., rate_autoc, rate_qini) == "positive"))
      ),
      
      # Keep old names for backwards compatibility (same as rate counts)
      positive_count = rate_positive_count,
      negative_count = rate_negative_count
    )
  
  # simplified selection logic based on categories
  selected_rows <- evidence_summary$category == "selected"
  excluded_rows <- evidence_summary$category == "excluded"
  unclear_rows <- evidence_summary$category == "unclear"
  
  # extract results by category
  selected_ids <- evidence_summary$model_id[selected_rows]
  selected_names <- evidence_summary$model_name[selected_rows]
  
  excluded_ids <- evidence_summary$model_id[excluded_rows]
  excluded_names <- evidence_summary$model_name[excluded_rows]
  
  unclear_ids <- evidence_summary$model_id[unclear_rows]
  unclear_names <- evidence_summary$model_name[unclear_rows]
  
  # Issue warnings for excluded models
  if (length(excluded_ids) > 0 && verbose) {
    cli::cli_alert_warning(
      "Excluded {length(excluded_ids)} model{?s} due to negative RATE evidence: {paste(excluded_names, collapse = ', ')}"
    )
  }
  
  # count positives by method
  positive_counts <- list(
    rate_autoc = sum(evidence_summary$rate_autoc == "positive"),
    rate_qini = sum(evidence_summary$rate_qini == "positive"),
    selected = sum(evidence_summary$category == "selected"),
    excluded = sum(evidence_summary$category == "excluded"),
    unclear = sum(evidence_summary$category == "unclear")
  )
  
  # reorder columns with model_id first for internal matching reliability
  evidence_summary_final <- evidence_summary %>%
    dplyr::select(model_id, model_name, category, mean_prediction_test, differential_prediction_test, 
                  rate_autoc, rate_qini, qini_curve, 
                  positive_count, negative_count,  # backwards compatibility
                  rate_positive_count, rate_negative_count,  # renamed RATE-only counts
                  total_positive_count, total_negative_count,  # all 4 tests
                  is_excluded, strict_inclusion_count,  # new indicators
                  selection_source, has_negative_rate, has_positive_rate, has_positive_qini_curve)
  
  list(
    selected_ids = selected_ids,
    selected_names = selected_names,
    excluded_ids = excluded_ids,
    excluded_names = excluded_names,
    unclear_ids = unclear_ids,
    unclear_names = unclear_names,
    positive_counts = positive_counts,
    evidence_summary = evidence_summary_final  # includes model_id for reliable matching
  )
}


#' Analyze concordance between methods
#' @keywords internal
analyse_concordance <- function(evidence_summary) {
  
  # models positive in all methods (across all 4 tests)
  all_positive <- evidence_summary$model_name[evidence_summary$total_positive_count == 4]
  
  # models positive in majority (3+ methods across all 4 tests)
  majority_positive <- evidence_summary$model_name[evidence_summary$total_positive_count >= 3]
  
  # discordant models (mix of positive and negative across all 4 tests)
  discordant <- evidence_summary$model_name[
    evidence_summary$total_positive_count > 0 & evidence_summary$total_negative_count > 0
  ]
  
  # create concordance matrix
  methods <- c("rate_autoc", "rate_qini", "qini_curve", "differential_prediction_test")
  concordance_matrix <- matrix(0, nrow = 4, ncol = 4, 
                               dimnames = list(methods, methods))
  
  for (i in 1:3) {
    for (j in (i+1):4) {
      # count agreements
      agree_positive <- sum(
        evidence_summary[[methods[i]]] == "positive" & 
        evidence_summary[[methods[j]]] == "positive"
      )
      agree_negative <- sum(
        evidence_summary[[methods[i]]] == "negative" & 
        evidence_summary[[methods[j]]] == "negative"
      )
      total_tested <- sum(
        evidence_summary[[methods[i]]] != "not_tested" & 
        evidence_summary[[methods[j]]] != "not_tested"
      )
      
      if (total_tested > 0) {
        concordance_matrix[i, j] <- (agree_positive + agree_negative) / total_tested
        concordance_matrix[j, i] <- concordance_matrix[i, j]
      }
    }
  }
  
  list(
    all_positive = all_positive,
    majority_positive = majority_positive,
    discordant = discordant,
    concordance_matrix = concordance_matrix
  )
}


#' Generate interpretation text
#' @keywords internal
generate_interpretation <- function(evidence_summary, selection_results, 
                                    rate_results, qini_results, omnibus_results,
                                    spend_levels) {
  
  # simple header for main interpretation
  header <- "### Heterogeneity Evidence Summary\n\n"
  
  # organise outcomes by category
  selected <- evidence_summary[evidence_summary$category == "selected", ]
  excluded <- evidence_summary[evidence_summary$category == "excluded", ]
  unclear <- evidence_summary[evidence_summary$category == "unclear", ]
  
  # SELECTED: Models with positive RATE evidence
  selected_text <- if (nrow(selected) > 0) {
    models_text <- paste(selected$model_name, collapse = ", ")
    sprintf("**SELECTED for targeting**: %s\n\nThese models show statistically significant positive RATE evidence, indicating that personalized treatment allocation would improve outcomes compared to treating everyone equally.",
            models_text)
  } else {
    ""
  }
  
  # EXCLUDED: Models with negative RATE evidence  
  excluded_text <- if (nrow(excluded) > 0) {
    models_text <- paste(excluded$model_name, collapse = ", ")
    sprintf("\n\n**EXCLUDED from targeting**: %s\n\nThese models show statistically significant negative RATE evidence. Targeting based on predicted effects would reliably worsen outcomes compared to random assignment. Avoid personalized treatment for these outcomes.",
            models_text)
  } else {
    ""
  }
  
  # UNCLEAR: Models with no significant RATE evidence
  unclear_text <- if (nrow(unclear) > 0) {
    models_text <- paste(unclear$model_name, collapse = ", ")
    sprintf("\n\n**EVIDENCE UNCLEAR**: %s\n\nThese models show no statistically significant RATE evidence in either direction. There is insufficient evidence to support targeted treatment strategies.",
            models_text)
  } else {
    ""
  }
  
  # simplified recommendations
  rec_parts <- character()
  
  if (length(selection_results$selected_names) > 0) {
    n_selected <- length(selection_results$selected_names)
    rec_parts <- c(rec_parts, 
                   sprintf("The following %d outcome%s show%s statistically significant positive heterogeneous treatment effects and %s suitable for targeted intervention: %s.", 
                           n_selected,
                           ifelse(n_selected == 1, "", "s"),
                           ifelse(n_selected == 1, "s", ""),
                           ifelse(n_selected == 1, "is", "are"),
                           paste(selection_results$selected_names, collapse = ", ")))
  }
  
  if (length(selection_results$excluded_names) > 0) {
    n_excluded <- length(selection_results$excluded_names)
    rec_parts <- c(rec_parts,
                   sprintf("Targeted treatment should be avoided for %d outcome%s that show%s statistically significant negative heterogeneous effects: %s. Targeting based on these outcomes would worsen results compared to universal treatment.", 
                           n_excluded,
                           ifelse(n_excluded == 1, "", "s"),
                           ifelse(n_excluded == 1, "s", ""),
                           paste(selection_results$excluded_names, collapse = ", ")))
  }
  
  if (length(selection_results$unclear_names) > 0) {
    n_unclear <- length(selection_results$unclear_names)
    rec_parts <- c(rec_parts,
                   sprintf("There is insufficient evidence to support targeted treatment strategies for %d outcome%s: %s.", 
                           n_unclear,
                           ifelse(n_unclear == 1, "", "s"),
                           paste(selection_results$unclear_names, collapse = ", ")))
  }
  
  if (length(rec_parts) == 0) {
    recommendations <- "\n\n**Recommendations**: No outcomes show clear evidence of treatment effect heterogeneity suitable for targeted intervention strategies."
  } else {
    recommendations <- paste0("\n\n**Recommendations**: ", paste(rec_parts, collapse = " "))
  }
  
  # full interpretation
  full_text <- paste0(header, selected_text, excluded_text, unclear_text, recommendations)
  
  # summary
  summary_text <- sprintf(
    "Analysed %d outcome models. Suitable for targeting: %d. Should avoid targeting: %d. Insufficient evidence: %d.",
    nrow(evidence_summary),
    selection_results$positive_counts$selected,
    selection_results$positive_counts$excluded,
    selection_results$positive_counts$unclear
  )
  
  list(
    full = full_text,
    summary = summary_text,
    recommendations = gsub("^.*\\*\\*Recommendations\\*\\*: ", "", recommendations)
  )
}


#' Generate extended academic-style report
#' @keywords internal
generate_extended_report <- function(evidence_summary, selection_results,
                                   rate_results_list, rate_results,
                                   qini_results, omnibus_results, 
                                   models, spend_levels,
                                   label_mapping, alpha = 0.05, 
                                   adjust = "BH", 
                                   use_cross_validation = FALSE,
                                   cv_num_folds = NULL,
                                   cv_results = NULL) {
  
  # extract grf parameters from first model
  grf_params <- extract_grf_params(models)
  
  # format spend levels for text
  spend_text <- paste0(spend_levels * 100, "%")
  if (length(spend_levels) == 2) {
    spend_range <- paste0(spend_text[1], "–", spend_text[2])
  } else {
    spend_range <- paste(spend_text, collapse = ", ")
  }
  
  # format adjustment method name
  adjust_name <- switch(adjust,
    "BH" = "Benjamini-Hochberg",
    "BY" = "Benjamini-Yekutieli", 
    "bonferroni" = "Bonferroni",
    "holm" = "Holm",
    "fdr" = "false discovery rate",
    "none" = "no adjustment",
    adjust  # default to the provided name
  )
  
  # header with method details
  method_description <- if (use_cross_validation) {
    sprintf("We used %d-fold sequential cross-validation for heterogeneity testing, which provides robust statistical inference by avoiding overfitting. P-values are derived using the martingale property (Wager 2024) for aggregation across folds. RATE estimates shown are weighted averages across test folds. Note: Confidence intervals are not provided for CV estimates as they do not align with the martingale-based hypothesis testing framework. ", cv_num_folds)
  } else {
    "We used standard heterogeneity testing methods. "
  }
  
  # Construct statistical significance text based on adjustment method
  significance_text <- if (adjust == "none") {
    ""  # No mention of alpha when no adjustment is applied
  } else {
    sprintf("Statistical significance was assessed at α = %.3f with %s correction for multiple testing. ", 
            alpha, adjust_name)
  }
  
  header <- sprintf(
    "We evaluated treatment effect heterogeneity (HTE) using complementary methods to identify outcomes suitable for targeted interventions covering %s of the population. We applied causal forests (grf package) with min.node.size = %s to obtain reliable estimates of conditional average treatment effects (CATE). %s%s\n\nStatistical heterogeneity tests (RATE with AUTOC and QINI weighting, plus omnibus calibration) assess whether treatment effects vary across individuals. Practical targeting tests (QINI curves) assess whether targeting based on predicted effects improves outcomes at specific budget constraints. Each method provides different insights, and their agreement strengthens conclusions.\n\nOur methods include:\n\n",
    spend_range,
    grf_params$min.node.size,
    method_description,
    significance_text
  )
  
  # methods description
  methods_desc <- paste0(
    "#### Omnibus Calibration Tests\n",
    "Test calibration of the forest. Computes the best linear fit of the target estimand using the forest prediction (on held-out data) as well as the mean forest prediction as the sole two regressors.\n\n",
    
    "#### RATE AUTOC (Secondary Global Evidence)\n",
    "Focuses on top responders with logarithmic weighting. A positive AUTOC signals strong HTE in groups with high $\\\\hat{\\\\tau}(x)$, although it is sensitive to ranking noise.",
    if (use_cross_validation) " Tests were conducted using sequential cross-validation for robust inference." else "",
    "\n\n",
    
    "#### RATE QINI (Primary Global Evidence)\n", 
    "Measures overall targeting gains by ranking individuals by $\\\\hat{\\\\tau}(x)$, using linear weighting across treated proportions. ",
    "A positive RATE QINI indicates HTE that enhances outcomes beyond random assignment, ideal for policies targeting a segment of the population.",
    if (use_cross_validation) " Tests were conducted using sequential cross-validation for robust inference." else "",
    "\n\n",
    
    "#### Qini Curves (Budget Focus)\n",
    "Illustrate cumulative gains at specific budgets (e.g., ", paste(spend_text, collapse = ", "), 
    ") compared to a treat-all baseline, guiding resource allocation decisions.\n\n",
    
    "These methods offer complementary insights: QINI and AUTOC evaluate statistical HTE, while Qini curves quantify practical targeting benefits when budgets are limited to a portion of the population. ",
    "Consistency across methods increases confidence in personalised strategies.\n\n",
    
    "#### Heterogeneity Testing Decision Flow\n\n",
    "For each outcome model:\n\n",
    "#### 1. EXCLUSION CHECK\n",
    "Is RATE QINI < 0 (sig) OR RATE AUTOC < 0 (sig)?\n\n",
    "→ YES: EXCLUDED (Issue warning)\n",
    "       Action: Avoid targeting based on this outcome - targeting would worsen results\n",
    "       Stop analysis\n",
    "       \n",
    "→ NO:  Continue to Step 2\n\n",
    "#### 2. GLOBAL HETEROGENEITY CHECK\n",
    "Is RATE QINI > 0 (sig) OR RATE AUTOC > 0 (sig)?\n\n",
    "→ YES: Evidence of global heterogeneity\n",
    "→ NO:  No global heterogeneity detected\n",
    "Continue to Step 3 for all non-excluded models\n\n",
    "#### 3. QINI CURVE ANALYSIS (All non-excluded models)\n",
    "Analyse practical gains at specific budget levels (", paste(spend_text, collapse = ", "), ")\n",
    "Do QINI curves show positive gains at any budget level?\n\n",
    "→ YES: Evidence of targeted heterogeneity at specific budgets\n",
    "→ NO:  No evidence of practical gains\n\n",
    "#### 4. FINAL SELECTION\n",
    "Model is SELECTED FOR TARGETING if:\n",
    "- Positive RATE tests (Step 2) OR\n",
    "- Positive QINI curves (Step 3)\n\n",
    "Otherwise: EVIDENCE UNCLEAR\n\n",
    "#### 5. CALIBRATION CHECK (Selected models only)\n",
    "Check mean_prediction_test status\n",
    "→ Record calibration status (affects confidence in predictions)\n\n"
  )
  
  # selected models section
  selected_text <- format_evidence_section_by_category(
    evidence_summary,
    "selected",
    "#### Selected for Targeting\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping
  )
  
  # excluded models section
  excluded_text <- format_evidence_section_by_category(
    evidence_summary,
    "excluded",
    "#### Excluded from Targeting\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping
  )
  
  # unclear evidence section
  unclear_text <- format_evidence_section_by_category(
    evidence_summary,
    "unclear",
    "#### Evidence Unclear\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping,
    include_qini = FALSE  # Don't analyse QINI curves for unclear models
  )
  
  # combine all sections
  paste0(header, methods_desc, selected_text, excluded_text, unclear_text)
}


#' Extract GRF parameters from models
#' @keywords internal
extract_grf_params <- function(models) {
  # default parameters
  params <- list(
    min.node.size = 20,
    num.trees = 2000,
    honesty = TRUE
  )
  
  # try to extract from first model if available
  if (!is.null(models) && !is.null(models$results)) {
    first_model_name <- names(models$results)[1]
    if (!is.null(models$full_models) && 
        first_model_name %in% names(models$full_models)) {
      forest <- models$full_models[[first_model_name]]
      if (!is.null(forest$min.node.size)) params$min.node.size <- forest$min.node.size
      if (!is.null(forest$num.trees)) params$num.trees <- forest$num.trees
      if (!is.null(forest$honesty)) params$honesty <- forest$honesty
    }
  }
  
  params
}


#' Format evidence section by category
#' @keywords internal
format_evidence_section_by_category <- function(evidence_summary, category, header_text,
                                              rate_results_list, qini_results, omnibus_results,
                                              models, spend_levels, label_mapping,
                                              include_qini = TRUE) {
  
  # filter to relevant outcomes
  outcomes <- evidence_summary[evidence_summary$category == category, ]
  
  if (nrow(outcomes) == 0) return("")
  
  # start with header
  text <- header_text
  
  # format each outcome
  outcome_texts <- character()
  for (i in seq_len(nrow(outcomes))) {
    outcome_row <- outcomes[i, ]
    # evidence_summary should have model_id column since we're using the full version
    model_id <- outcome_row$model_id
    
    outcome_text <- format_outcome_details(
      model_id,
      outcome_row$model_name,
      outcome_row,
      rate_results_list,
      qini_results,
      omnibus_results,
      models,
      spend_levels,
      include_qini = include_qini
    )
    
    outcome_texts <- c(outcome_texts, outcome_text)
  }
  
  paste0(text, paste(outcome_texts, collapse = "\n\n"), "\n\n")
}


#' Format detailed statistics for a single outcome
#' @keywords internal
format_outcome_details <- function(model_id, model_name, evidence_row,
                                 rate_results_list, qini_results, omnibus_results,
                                 models, spend_levels, include_qini = TRUE) {
  
  details <- paste0("**", model_name, "**: ")
  
  # Check selection source if available
  selection_note <- ""
  if ("selection_source" %in% names(evidence_row)) {
    if (evidence_row$selection_source == "qini_curve_only") {
      selection_note <- " (selected based on QINI curve evidence)"
    } else if (evidence_row$selection_source == "both_rate_and_qini") {
      selection_note <- " (selected based on both RATE and QINI curve evidence)"
    }
  }
  
  # extract RATE statistics if available
  rate_text <- character()
  
  # Check if using CV results
  is_cv <- !is.null(rate_results_list) && inherits(rate_results_list, "margot_cv_results")
  
  if (is_cv) {
    # Handle CV results - get from tables
    if (!is.null(rate_results_list$tables)) {
      # RATE QINI from CV
      if (!is.null(rate_results_list$tables$rate_qini)) {
        qini_table <- rate_results_list$tables$rate_qini
        qini_row <- qini_table[qini_table$model_id == model_id, ]
        if (nrow(qini_row) > 0) {
          qini_est <- format_rate_with_ci(qini_row, "QINI")
          if (nchar(qini_est) > 0) rate_text <- c(rate_text, qini_est)
        }
      }
      
      # RATE AUTOC from CV
      if (!is.null(rate_results_list$tables$rate_autoc)) {
        autoc_table <- rate_results_list$tables$rate_autoc
        autoc_row <- autoc_table[autoc_table$model_id == model_id, ]
        if (nrow(autoc_row) > 0) {
          autoc_est <- format_rate_with_ci(autoc_row, "AUTOC")
          if (nchar(autoc_est) > 0) rate_text <- c(rate_text, autoc_est)
        }
      }
    }
  } else {
    # Standard RATE results
    # RATE QINI
    if (!is.null(rate_results_list) && !is.null(rate_results_list$rate_qini)) {
      # try different column names
      if ("outcome" %in% names(rate_results_list$rate_qini)) {
        qini_row <- rate_results_list$rate_qini[rate_results_list$rate_qini$outcome == model_id, ]
      } else if ("Model" %in% names(rate_results_list$rate_qini)) {
        qini_row <- rate_results_list$rate_qini[rate_results_list$rate_qini$Model == model_id, ]
      } else {
        qini_row <- data.frame()
      }
      if (nrow(qini_row) > 0) {
        qini_est <- format_rate_with_ci(qini_row, "QINI")
        if (nchar(qini_est) > 0) rate_text <- c(rate_text, qini_est)
      }
    }
    
    # RATE AUTOC
    if (!is.null(rate_results_list) && !is.null(rate_results_list$rate_autoc)) {
      # try different column names
      if ("outcome" %in% names(rate_results_list$rate_autoc)) {
        autoc_row <- rate_results_list$rate_autoc[rate_results_list$rate_autoc$outcome == model_id, ]
      } else if ("Model" %in% names(rate_results_list$rate_autoc)) {
        autoc_row <- rate_results_list$rate_autoc[rate_results_list$rate_autoc$Model == model_id, ]
      } else {
        autoc_row <- data.frame()
      }
      if (nrow(autoc_row) > 0) {
        autoc_est <- format_rate_with_ci(autoc_row, "AUTOC")
        if (nchar(autoc_est) > 0) rate_text <- c(rate_text, autoc_est)
      }
    }
  }
  
  # combine RATE text based on actual statistical significance status
  if (length(rate_text) > 0) {
    rate_parts <- character()
    
    # Check actual statistical significance from evidence_row
    qini_sig <- evidence_row$rate_qini
    autoc_sig <- evidence_row$rate_autoc
    
    # Build description based on statistical significance and direction
    rate_descriptions <- character()
    
    # Add QINI description if available
    for (rt in rate_text) {
      if (grepl("QINI", rt)) {
        if (qini_sig == "positive") {
          rate_descriptions <- c(rate_descriptions, paste0("statistically significant positive RATE ", rt))
        } else if (qini_sig == "negative") {
          rate_descriptions <- c(rate_descriptions, paste0("statistically significant negative RATE ", rt))
        } else {
          rate_descriptions <- c(rate_descriptions, paste0("RATE ", rt, " (not statistically significant)"))
        }
      } else if (grepl("AUTOC", rt)) {
        if (autoc_sig == "positive") {
          rate_descriptions <- c(rate_descriptions, paste0("statistically significant positive RATE ", rt))
        } else if (autoc_sig == "negative") {
          rate_descriptions <- c(rate_descriptions, paste0("statistically significant negative RATE ", rt))
        } else {
          rate_descriptions <- c(rate_descriptions, paste0("RATE ", rt, " (not statistically significant)"))
        }
      }
    }
    
    if (length(rate_descriptions) > 0) {
      details <- paste0(details, paste(rate_descriptions, collapse = " and "))
    }
  }
  
  # add omnibus test results
  omnibus_text <- format_omnibus_results(model_id, model_name, omnibus_results)
  if (nchar(omnibus_text) > 0) {
    if (length(rate_text) > 0) {
      # Check if omnibus p-value suggests statistical significance
      # Extract p-value from omnibus_text which is like "Diff. Est. = 0.78, p = 0.158"
      p_match <- regmatches(omnibus_text, regexpr("p = [0-9.]+", omnibus_text))
      if (length(p_match) > 0) {
        p_val <- as.numeric(sub("p = ", "", p_match))
        if (!is.na(p_val) && p_val < 0.05) {
          details <- paste0(details, ", confirmed by omnibus tests (", omnibus_text, ")")
        } else {
          details <- paste0(details, ". Omnibus tests: ", omnibus_text)
        }
      } else {
        details <- paste0(details, ". Omnibus tests: ", omnibus_text)
      }
    } else {
      details <- paste0(details, "omnibus tests show ", omnibus_text)
    }
  }
  
  # add QINI curve differences only if requested
  if (include_qini) {
    qini_diff_text <- format_qini_differences(model_id, qini_results, spend_levels)
    if (nchar(qini_diff_text) > 0) {
      if (length(rate_text) > 0 || nchar(omnibus_text) > 0) {
        details <- paste0(details, ". Qini differences: ", qini_diff_text)
      } else {
        # check if Qini differences are positive or negative
        # qini_diff_text contains values like "10%: 0.001, 40%: -0.002"
        has_negative <- grepl("-", qini_diff_text)
        has_positive <- grepl("[^-]\\d+\\.\\d+", qini_diff_text)
        
        if (has_negative && !has_positive) {
          details <- paste0(details, "negative Qini differences ", qini_diff_text, " suggest targeting would worsen outcomes")
        } else if (has_positive && !has_negative) {
          details <- paste0(details, "positive Qini differences ", qini_diff_text, " suggest practical benefits")
        } else {
          details <- paste0(details, "mixed Qini differences ", qini_diff_text, " suggest variable targeting effects")
        }
      }
    }
  }
  
  # add interpretation based on category
  if (evidence_row$category == "selected") {
    details <- paste0(details, ". This supports effective targeting", selection_note, ".")
    
    # note calibration status
    if (evidence_row$mean_prediction_test == "calibrated") {
      details <- paste0(details, " Well-calibrated predictions enhance reliability.")
    } else if (evidence_row$mean_prediction_test == "not_calibrated") {
      details <- paste0(details, " Note: predictions not well-calibrated (interpret with caution).")
    }
  } else if (evidence_row$category == "excluded") {
    details <- paste0(details, ". Avoid targeting this outcome.")
  } else if (evidence_row$category == "unclear") {
    # Check if QINI curves were tested
    qini_tested <- evidence_row$qini_curve != "not_tested"
    if (qini_tested) {
      details <- paste0(details, ". Insufficient evidence for targeting (neither RATE tests nor QINI curves show statistically significant benefits).")
    } else {
      details <- paste0(details, ". Insufficient evidence for targeting.")
    }
  }
  
  details
}


#' Format RATE estimate with confidence interval
#' @keywords internal
format_rate_with_ci <- function(rate_row, type) {
  if (nrow(rate_row) == 0) return("")
  
  # extract estimate - check different possible column names
  est <- NULL
  if ("RATE Estimate" %in% names(rate_row)) {
    est <- rate_row$`RATE Estimate`[1]
  } else if ("Estimate" %in% names(rate_row)) {
    est <- rate_row$Estimate[1]
  }
  
  if (is.null(est)) return("")
  
  # extract CI - check for different possible column names
  ci_low <- NULL
  ci_high <- NULL
  
  if ("2.5%" %in% names(rate_row)) {
    ci_low <- rate_row$`2.5%`[1]
    ci_high <- rate_row$`97.5%`[1]
  } else if ("conf.low" %in% names(rate_row)) {
    ci_low <- rate_row$conf.low[1]
    ci_high <- rate_row$conf.high[1]
  } else if ("Lower.CI" %in% names(rate_row)) {
    ci_low <- rate_row$Lower.CI[1]
    ci_high <- rate_row$Upper.CI[1]
  } else if ("CI.Lower" %in% names(rate_row)) {
    ci_low <- rate_row$CI.Lower[1]
    ci_high <- rate_row$CI.Upper[1]
  }
  
  if (!is.null(ci_low) && !is.null(ci_high)) {
    # format with appropriate precision, converting to numeric if needed
    est_num <- suppressWarnings(as.numeric(est))
    ci_low_num <- suppressWarnings(as.numeric(ci_low))
    ci_high_num <- suppressWarnings(as.numeric(ci_high))
    
    if (is.na(est_num)) {
      # if conversion failed, return as is
      sprintf("%s (%s)", type, est)
    } else {
      sprintf("%s (%0.3f [95%% CI: %0.3f, %0.3f])", type, est_num, ci_low_num, ci_high_num)
    }
  } else {
    # convert to numeric if needed
    est_num <- suppressWarnings(as.numeric(est))
    if (is.na(est_num)) {
      # if conversion failed, return as is
      sprintf("%s (%s)", type, est)
    } else {
      sprintf("%s (%0.3f)", type, est_num)
    }
  }
}


#' Format omnibus test results
#' @keywords internal
format_omnibus_results <- function(model_id, model_name, omnibus_results) {
  if (is.null(omnibus_results) || is.null(omnibus_results$summary_table)) {
    return("")
  }
  
  # find matching row - try different name formats
  clean_id <- sub("^model_", "", model_id)
  omni_row <- omnibus_results$summary_table[
    omnibus_results$summary_table$Outcome == model_name |
    omnibus_results$summary_table$Outcome == clean_id,
  ]
  
  if (nrow(omni_row) == 0) return("")
  
  # the summary table has formatted columns, so we need to extract from the text
  # look for "Diff. Est. (SE)" column and "Diff. p-value" column
  if ("Diff. Est. (SE)" %in% names(omni_row) && "Diff. p-value" %in% names(omni_row)) {
    # extract estimate from formatted text like "1.02 (0.45)"
    diff_text <- omni_row$`Diff. Est. (SE)`[1]
    diff_est <- as.numeric(sub("\\s*\\(.*", "", diff_text))
    
    # p-value is already formatted
    p_formatted <- omni_row$`Diff. p-value`[1]
    
    return(sprintf("Diff. Est. = %.2f, p = %s", diff_est, p_formatted))
  }
  
  # fallback: check if we have the raw columns
  if ("differential_prediction_estimate" %in% names(omni_row)) {
    diff_est <- omni_row$differential_prediction_estimate[1]
    diff_p <- omni_row$differential_prediction_p[1]
    
    # format p-value
    p_formatted <- if (!is.na(diff_p) && diff_p < 0.001) {
      sprintf("p < 0.001")
    } else {
      sprintf("p = %.3f", diff_p)
    }
    
    return(sprintf("Diff. Est. = %.2f, %s", diff_est, p_formatted))
  }
  
  return("")
}


#' Format QINI differences at spend levels
#' @keywords internal
format_qini_differences <- function(model_id, qini_results, spend_levels) {
  if (is.null(qini_results) || is.null(qini_results$summary_table)) {
    return("")
  }
  
  # find matching row - check different column names
  qini_row <- NULL
  
  if ("Model" %in% names(qini_results$summary_table)) {
    # first column is usually the model identifier
    model_col <- names(qini_results$summary_table)[1]
    # try to match by model_id or its variations
    for (i in seq_len(nrow(qini_results$summary_table))) {
      row_model <- qini_results$summary_table[[model_col]][i]
      # check if this row matches our model_id
      if (row_model == model_id || 
          row_model == sub("^model_", "", model_id) ||
          grepl(sub("^model_", "", model_id), row_model, ignore.case = TRUE)) {
        qini_row <- qini_results$summary_table[i, , drop = FALSE]
        break
      }
    }
  }
  
  # if still no match, try first column
  if (is.null(qini_row) || nrow(qini_row) == 0) {
    model_name_col <- names(qini_results$summary_table)[1]
    qini_row <- qini_results$summary_table[
      qini_results$summary_table[[model_name_col]] == model_id |
      qini_results$summary_table[[model_name_col]] == sub("^model_", "", model_id),
    ]
  }
  
  if (nrow(qini_row) == 0) return("")
  
  # extract differences at each spend level
  diff_texts <- character()
  
  for (spend in spend_levels) {
    col_name <- sprintf("Spend %s%%", spend * 100)
    if (col_name %in% names(qini_row)) {
      value <- qini_row[[col_name]][1]
      if (!is.na(value) && value != "") {
        # extract estimate and CI from formatted string like "0.03 [-0.03, 0.09]"
        diff_texts <- c(diff_texts, 
                       sprintf("%s at %s%% spend", value, spend * 100))
      }
    }
  }
  
  if (length(diff_texts) == 0) return("")
  
  paste(diff_texts, collapse = "; ")
}


#' Format ATE value
#' @keywords internal
format_ate_value <- function(model_id, models) {
  if (is.null(models) || is.null(models$results)) return("")
  
  model_name <- if (grepl("^model_", model_id)) model_id else paste0("model_", model_id)
  
  if (model_name %in% names(models$results)) {
    ate <- models$results[[model_name]]$ate
    if (!is.null(ate) && length(ate) >= 1) {
      return(sprintf("near-zero ATE (%.3f)", ate[1]))
    }
  }
  
  return("")
}