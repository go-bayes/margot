#' Interpret Heterogeneity Evidence from Multiple Sources
#'
#' @description
#' Combines evidence from multiple heterogeneity tests (RATE AUTOC, RATE QINI, 
#' QINI curves, and omnibus calibration tests) to provide unified recommendations 
#' about which models show treatment effect heterogeneity.
#'
#' @param models Output from `margot_causal_forest()` containing model results
#' @param spend_levels Numeric vector of spend levels for QINI analysis. 
#'   Default is c(0.1, 0.4).
#' @param require_any_positive Logical. If TRUE (default), include models that 
#'   show positive evidence in ANY method. If FALSE, require positive evidence 
#'   in ALL methods.
#' @param exclude_negative_any Logical. If TRUE (default), exclude models that 
#'   show negative evidence in ANY method. If FALSE, only exclude if negative 
#'   in ALL methods.
#' @param require_omnibus Logical. If TRUE, only include models that pass the 
#'   omnibus calibration test. Default is FALSE.
#' @param alpha Numeric. Significance level for RATE tests. Default is 0.05.
#'   Note: this controls which RATE estimates are considered significant after
#'   multiple testing correction.
#' @param adjust Character. Multiple testing adjustment method for RATE estimates. 
#'   Options include "BH" (Benjamini-Hochberg), "BY" (Benjamini-Yekutieli), 
#'   "bonferroni", "holm", "fdr", or "none". Default is "BH".
#' @param flipped_outcomes Character vector of outcome names that were flipped 
#'   (reversed) in preprocessing. Used for interpretation text.
#' @param label_mapping Named list for mapping model names to human-readable labels.
#' @param verbose Logical. If TRUE, show progress messages. Default is TRUE.
#' @param include_extended_report Logical. If TRUE, generate detailed academic-style report
#'   with full statistics and confidence intervals. Default is FALSE.
#' @param rate_results Optional pre-computed RATE results to skip computation.
#' @param qini_results Optional pre-computed QINI results to skip computation.
#' @param omnibus_results Optional pre-computed omnibus test results to skip computation.
#' @param use_cross_validation Logical. If TRUE, use cross-validation for RATE tests
#'   instead of standard approach (default FALSE).
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
#'   \item{cautiously_selected_model_ids}{Character vector of model IDs with mixed evidence (positive in some tests but negative in others)}
#'   \item{cautiously_selected_model_names}{Character vector of human-readable model names with mixed evidence}
#'   \item{all_selected_model_ids}{Combined vector of selected_model_ids and cautiously_selected_model_ids}
#'   \item{all_selected_model_names}{Combined vector of selected_model_names and cautiously_selected_model_names}
#'   \item{excluded_model_ids}{Character vector of model IDs to exclude}
#'   \item{excluded_model_names}{Character vector of human-readable excluded model names}
#'   \item{evidence_summary}{Data frame with detailed evidence by source including evidence_type categorization. Contains columns: model_id, model_name, mean_prediction_test (calibration status), differential_prediction_test (heterogeneity test), rate_autoc, rate_qini, qini_curve, positive_count, negative_count, and evidence_type. Note: mean_prediction_test indicates calibration quality but is not included in heterogeneity scoring}
#'   \item{interpretation}{Character string with main interpretation text organized by evidence categories}
#'   \item{summary}{Character string with brief summary}
#'   \item{recommendations}{Character string with actionable recommendations}
#'   \item{rate_results}{List containing AUTOC and QINI RATE results}
#'   \item{qini_results}{QINI curve interpretation results}
#'   \item{omnibus_results}{Omnibus calibration test results}
#'   \item{concordance}{List analyzing agreement between methods}
#'   \item{extended_report}{Character string with detailed academic report (if include_extended_report = TRUE)}
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
#' # View interpretation
#' cat(het_evidence$interpretation)
#' 
#' # Use selected models for targeting
#' policy_results <- margot_policy(
#'   causal_forest_results,
#'   model_names = het_evidence$selected_model_ids
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
#' }
#'
#' @export
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_progress_step cli_progress_done
#' @importFrom dplyr bind_rows mutate case_when
#' @importFrom tibble tibble
#' @importFrom future availableCores
margot_interpret_heterogeneity <- function(
  models = NULL,
  spend_levels = c(0.1, 0.4),
  require_any_positive = TRUE,
  exclude_negative_any = TRUE,
  require_omnibus = FALSE,
  alpha = 0.05,
  adjust = "BH",
  flipped_outcomes = NULL,
  label_mapping = NULL,
  verbose = TRUE,
  include_extended_report = FALSE,
  rate_results = NULL,
  qini_results = NULL,
  omnibus_results = NULL,
  use_cross_validation = FALSE,
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
      task_msg <- if (use_cross_validation) {
        "Computing cross-validation heterogeneity tests"
      } else {
        "Computing RATE estimates"
      }
      cli::cli_progress_step(
        paste0("[{current_task}/{n_tasks}] ", task_msg),
        msg_done = paste0("[{current_task}/{n_tasks}] ", task_msg, " ... done")
      )
    }
    
    if (use_cross_validation) {
      # Use cross-validation approach
      if (is.null(cv_results)) {
        # For CV, only "bonferroni" or "none" are valid
        cv_adjust <- adjust
        if (!adjust %in% c("bonferroni", "none")) {
          if (verbose) {
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
          n_cores = n_cores
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
  
  # extract all model ids
  all_model_ids <- unique(c(
    names(models$results),
    rate_results$autoc_model_names,
    rate_results$qini_model_names,
    rate_results$not_excluded_autoc_model_names,
    rate_results$not_excluded_qini_model_names,
    qini_results$reliable_model_ids,
    qini_results$harmful_model_ids,
    qini_results$no_effect_model_ids
  ))
  
  # create evidence summary
  evidence_summary <- create_evidence_summary(
    all_model_ids,
    rate_results,
    qini_results,
    omnibus_results,
    label_mapping
  )
  
  # select models based on criteria
  selection_results <- select_models(
    evidence_summary,
    require_any_positive,
    exclude_negative_any,
    require_omnibus
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
  concordance <- analyze_concordance(evidence_summary)
  
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
    cautiously_selected_model_ids = selection_results$cautiously_selected_ids,
    cautiously_selected_model_names = selection_results$cautiously_selected_names,
    all_selected_model_ids = c(selection_results$selected_ids, selection_results$cautiously_selected_ids),
    all_selected_model_names = c(selection_results$selected_names, selection_results$cautiously_selected_names),
    excluded_model_ids = selection_results$excluded_ids,
    excluded_model_names = selection_results$excluded_names,
    evidence_summary = evidence_summary,
    interpretation = interpretation_text$full,
    summary = interpretation_text$summary,
    recommendations = interpretation_text$recommendations,
    rate_results = list(
      autoc = if (!is.null(rate_results_list)) rate_results_list$rate_autoc else NULL,
      qini = if (!is.null(rate_results_list)) rate_results_list$rate_qini else NULL,
      interpretation = rate_results
    ),
    qini_results = qini_results,
    omnibus_results = omnibus_results,
    concordance = concordance,
    positive_counts = selection_results$positive_counts,
    extended_report = extended_report
  )
}


#' Create evidence summary table
#' @keywords internal
create_evidence_summary <- function(model_ids, rate_results, qini_results, 
                                    omnibus_results, label_mapping) {
  
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
    clean_id <- sub("_z$", "", clean_id)  # remove _z suffix first
    clean_id <- gsub("_", " ", clean_id)   # then replace underscores
    tools::toTitleCase(clean_id)
  }
  
  # build summary for each model
  summary_list <- lapply(model_ids, function(id) {
    
    # rate autoc status
    rate_autoc_status <- case_when(
      id %in% rate_results$autoc_model_names ~ "positive",
      id %in% rate_results$excluded_either ~ "negative", 
      id %in% rate_results$not_excluded_autoc_model_names ~ "inconclusive",
      TRUE ~ "not_tested"
    )
    
    # rate qini status
    rate_qini_status <- case_when(
      id %in% rate_results$qini_model_names ~ "positive",
      id %in% rate_results$excluded_either ~ "negative",
      id %in% rate_results$not_excluded_qini_model_names ~ "inconclusive",
      TRUE ~ "not_tested"
    )
    
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
          # check mean prediction significance for calibration
          if ("mean_prediction_significant" %in% names(omnibus_row)) {
            if (isTRUE(omnibus_row$mean_prediction_significant[1])) {
              mean_pred_status <- "calibrated"
            } else {
              mean_pred_status <- "not_calibrated"
            }
          }
          
          # check differential prediction significance for heterogeneity
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
                          exclude_negative_any, require_omnibus) {
  
  # add positive_count, negative_count, and evidence_type to evidence_summary
  # Note: qini_curve is excluded from counts as it's exploratory/sensitive to spend levels
  evidence_summary <- evidence_summary %>%
    dplyr::mutate(
      # Count primary evidence only (RATE and differential prediction)
      positive_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini, differential_prediction_test) == "positive"
      ),
      # Only RATE can be reliably negative (differential prediction is positive/inconclusive only)
      negative_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini) == "negative"
      ),
      # Separate count for exploratory QINI curve evidence
      qini_positive = (qini_curve == "positive"),
      
      evidence_type = dplyr::case_when(
        # Evidence for heterogeneity - RATE positive AND differential prediction confirms
        (rate_autoc == "positive" | rate_qini == "positive") & 
        differential_prediction_test == "positive" ~ "evidence_for_heterogeneity",
        
        # Mixed evidence with caution - RATE has both positive and negative results
        rate_autoc == "positive" & rate_qini == "negative" ~ "mixed_evidence_caution",
        rate_autoc == "negative" & rate_qini == "positive" ~ "mixed_evidence_caution",
        
        # Targeting opportunity - RATE positive (primary evidence)
        (rate_autoc == "positive" | rate_qini == "positive") ~ "targeting_opportunity",
        
        # Statistical only - differential prediction positive but RATE not positive
        differential_prediction_test == "positive" & 
        rate_autoc != "positive" & 
        rate_qini != "positive" ~ "statistical_only",
        
        # Exploratory evidence only - only QINI curve positive
        qini_curve == "positive" & 
        rate_autoc != "positive" & 
        rate_qini != "positive" & 
        differential_prediction_test != "positive" ~ "exploratory_only",
        
        # No evidence - no positive results in any test
        TRUE ~ "no_evidence"
      )
    )
  
  # selection logic
  if (require_any_positive) {
    # include if ANY method shows positive
    selected_rows <- evidence_summary$positive_count > 0
  } else {
    # include if ALL tested methods show positive
    tested_cols <- c("rate_autoc", "rate_qini", "qini_curve", "differential_prediction_test")
    selected_rows <- apply(evidence_summary[tested_cols], 1, function(row) {
      tested <- row != "not_tested"
      positive <- row == "positive"
      all(positive[tested]) && any(tested)
    })
  }
  
  # apply exclusion criteria
  if (exclude_negative_any) {
    # exclude if ANY method shows negative
    selected_rows <- selected_rows & (evidence_summary$negative_count == 0)
  } else {
    # exclude if ALL tested methods show negative
    tested_cols <- c("rate_autoc", "rate_qini", "qini_curve", "differential_prediction_test")
    all_negative <- apply(evidence_summary[tested_cols], 1, function(row) {
      tested <- row != "not_tested"
      negative <- row == "negative"
      all(negative[tested]) && any(tested)
    })
    selected_rows <- selected_rows & !all_negative
  }
  
  # require omnibus if specified
  if (require_omnibus) {
    selected_rows <- selected_rows & (evidence_summary$differential_prediction_test == "positive")
  }
  
  # extract results
  selected_ids <- evidence_summary$model_id[selected_rows]
  selected_names <- evidence_summary$model_name[selected_rows]
  excluded_ids <- evidence_summary$model_id[!selected_rows]
  excluded_names <- evidence_summary$model_name[!selected_rows]
  
  # count positives by method
  positive_counts <- list(
    rate_autoc = sum(evidence_summary$rate_autoc == "positive"),
    rate_qini = sum(evidence_summary$rate_qini == "positive"),
    qini_curve = sum(evidence_summary$qini_curve == "positive"),
    differential_prediction = sum(evidence_summary$differential_prediction_test == "positive"),
    any_method = sum(evidence_summary$positive_count > 0),
    all_methods = sum(evidence_summary$positive_count == 4)
  )
  
  # reorder columns with model_id first for internal matching reliability
  evidence_summary_final <- evidence_summary %>%
    dplyr::select(model_id, model_name, mean_prediction_test, differential_prediction_test, 
                  rate_autoc, rate_qini, qini_curve, positive_count, negative_count, evidence_type)
  
  # extract cautiously selected models (mixed evidence)
  cautiously_selected_rows <- evidence_summary$evidence_type == "mixed_evidence_caution"
  cautiously_selected_ids <- evidence_summary$model_id[cautiously_selected_rows]
  cautiously_selected_names <- evidence_summary$model_name[cautiously_selected_rows]
  
  list(
    selected_ids = selected_ids,
    selected_names = selected_names,
    cautiously_selected_ids = cautiously_selected_ids,
    cautiously_selected_names = cautiously_selected_names,
    excluded_ids = excluded_ids,
    excluded_names = excluded_names,
    positive_counts = positive_counts,
    evidence_summary = evidence_summary_final  # includes model_id for reliable matching
  )
}


#' Analyze concordance between methods
#' @keywords internal
analyze_concordance <- function(evidence_summary) {
  
  # models positive in all methods
  all_positive <- evidence_summary$model_name[evidence_summary$positive_count == 4]
  
  # models positive in majority (3+ methods)
  majority_positive <- evidence_summary$model_name[evidence_summary$positive_count >= 3]
  
  # discordant models (mix of positive and negative)
  discordant <- evidence_summary$model_name[
    evidence_summary$positive_count > 0 & evidence_summary$negative_count > 0
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
  header <- "### Heterogeneity Evidence Summary"
  
  # organize outcomes by evidence type
  evidence_for_het <- evidence_summary[evidence_summary$evidence_type == "evidence_for_heterogeneity", ]
  targeting_opp <- evidence_summary[evidence_summary$evidence_type == "targeting_opportunity", ]
  statistical_only <- evidence_summary[evidence_summary$evidence_type == "statistical_only", ]
  exploratory_only <- evidence_summary[evidence_summary$evidence_type == "exploratory_only", ]
  mixed_caution <- evidence_summary[evidence_summary$evidence_type == "mixed_evidence_caution", ]
  
  # evidence for heterogeneity
  evidence_text <- if (nrow(evidence_for_het) > 0) {
    models_text <- paste(evidence_for_het$model_name, collapse = ", ")
    sprintf("\n\n**Evidence for Heterogeneity**: %s show%s both theoretical heterogeneity (RATE) and statistical validation (omnibus test). These outcomes have strong support for personalized treatment strategies.",
            models_text, 
            ifelse(nrow(evidence_for_het) == 1, "s", ""))
  } else {
    ""
  }
  
  # targeting opportunities
  targeting_text <- if (nrow(targeting_opp) > 0) {
    models_text <- paste(targeting_opp$model_name, collapse = ", ")
    sprintf("\n\n**Targeting Opportunities**: %s show%s positive treatment effect heterogeneity based on RATE analysis. These outcomes demonstrate practical benefits from personalized treatment allocation.",
            models_text,
            ifelse(nrow(targeting_opp) == 1, "s", ""))
  } else {
    ""
  }
  
  # statistical heterogeneity only
  statistical_text <- if (nrow(statistical_only) > 0) {
    models_text <- paste(statistical_only$model_name, collapse = ", ")
    sprintf("\n\n**Statistical Heterogeneity Only**: %s show%s statistical evidence of varying treatment effects (omnibus test) but no clear practical benefit from targeting. Individual differences exist but may not be actionable.",
            models_text,
            ifelse(nrow(statistical_only) == 1, "s", ""))
  } else {
    ""
  }
  
  # exploratory evidence only
  exploratory_text <- if (nrow(exploratory_only) > 0) {
    models_text <- paste(exploratory_only$model_name, collapse = ", ")
    sprintf("\n\n**Exploratory Evidence Only**: %s show%s positive QINI curves at specific spend levels (%s) but lack support from primary heterogeneity tests. These findings are sensitive to spend level selection and should be considered preliminary.",
            models_text,
            ifelse(nrow(exploratory_only) == 1, "s", ""),
            paste(spend_levels * 100, "%", sep = "", collapse = ", "))
  } else {
    ""
  }
  
  # proceed with caution
  caution_text <- if (nrow(mixed_caution) > 0) {
    models_text <- paste(mixed_caution$model_name, collapse = ", ")
    sprintf("\n\n**Proceed with Caution**: %s show%s positive evidence in some tests but negative results in others. Since different tests measure different aspects of heterogeneity, these conflicting results suggest heterogeneity may exist but could be challenging to exploit in practice. Careful validation is recommended before implementation.",
            models_text,
            ifelse(nrow(mixed_caution) == 1, "s", ""))
  } else {
    ""
  }
  
  # excluded outcomes
  excluded_text <- if (length(selection_results$excluded_names) > 0) {
    sprintf("\n\n**Excluded Outcomes**: %s should not be targeted, as they show negative or no evidence of heterogeneity.",
            paste(selection_results$excluded_names, collapse = ", "))
  } else {
    ""
  }
  
  # recommendations
  if (length(selection_results$selected_names) > 0) {
    # prioritize by evidence type
    primary_targets <- c(
      evidence_for_het$model_name,
      targeting_opp$model_name
    )
    caution_targets <- mixed_caution$model_name
    exploratory_targets <- exploratory_only$model_name
    
    rec_parts <- character()
    if (length(primary_targets) > 0) {
      rec_parts <- c(rec_parts, 
                     sprintf("Focus targeting efforts on %s.", 
                             paste(primary_targets, collapse = ", ")))
    }
    if (length(caution_targets) > 0) {
      rec_parts <- c(rec_parts,
                     sprintf("For %s, proceed with caution due to conflicting evidence across tests.", 
                             paste(caution_targets, collapse = ", ")))
    }
    if (length(exploratory_targets) > 0) {
      rec_parts <- c(rec_parts,
                     sprintf("Consider %s for exploratory analyses only, as evidence is limited to QINI curves.", 
                             paste(exploratory_targets, collapse = ", ")))
    }
    if (length(selection_results$excluded_names) > 0) {
      rec_parts <- c(rec_parts,
                     sprintf("Avoid targeting for %s.", 
                             paste(selection_results$excluded_names, collapse = ", ")))
    }
    recommendations <- paste("\n\n**Recommendations**: ", paste(rec_parts, collapse = " "))
  } else {
    recommendations <- "\n\n**Recommendations**: No models show sufficient evidence of heterogeneity to warrant targeted treatment."
  }
  
  # full interpretation
  full_text <- paste0(header, evidence_text, targeting_text, caution_text,
                      statistical_text, exploratory_text, excluded_text, recommendations)
  
  # summary
  summary_text <- sprintf(
    "Found heterogeneity evidence for %d of %d models. Evidence for heterogeneity: %d models. Targeting opportunities: %d models. Proceed with caution: %d models. Exploratory only: %d models. Selected for targeting: %d models.",
    selection_results$positive_counts$any_method,
    nrow(evidence_summary),
    nrow(evidence_for_het),
    nrow(targeting_opp),
    nrow(mixed_caution),
    nrow(exploratory_only),
    length(selection_results$selected_ids)
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
    sprintf("We used %d-fold sequential cross-validation for heterogeneity testing, which provides robust statistical inference by avoiding overfitting. ", cv_num_folds)
  } else {
    "We used standard heterogeneity testing methods. "
  }
  
  header <- sprintf(
    "We evaluated treatment effect heterogeneity (HTE) using complementary methods to identify outcomes suitable for targeted interventions covering %s of the population. We applied causal forests (grf package) with min.node.size = %s to obtain reliable estimates of conditional average treatment effects (CATE). %sStatistical significance was assessed at α = %.3f with %s correction for multiple testing.\n\nStatistical heterogeneity tests (RATE with AUTOC and QINI weighting, plus omnibus calibration) assess whether treatment effects vary across individuals. Practical targeting tests (QINI curves) assess whether targeting based on predicted effects improves outcomes at specific budget constraints. Each method provides different insights, and their agreement strengthens conclusions.\n\nOur methods include:\n\n",
    spend_range,
    grf_params$min.node.size,
    method_description,
    alpha,
    adjust_name
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
    "A positive RATE QINI (e.g., 0.022 for Forgiveness) indicates HTE that enhances outcomes beyond random assignment, ideal for policies targeting a segment of the population.",
    if (use_cross_validation) " Tests were conducted using sequential cross-validation for robust inference." else "",
    "\n\n",
    
    "#### Qini Curves (Budget Focus)\n",
    "Illustrate cumulative gains at specific budgets (e.g., ", paste(spend_text, collapse = ", "), 
    ") compared to a treat-all baseline, guiding resource allocation decisions.\n\n",
    
    "These methods offer complementary insights: QINI and AUTOC evaluate statistical HTE, while Qini curves quantify practical targeting benefits when budgets are limited to a portion of the population. ",
    "Consistency across methods increases confidence in personalised strategies.\n\n"
  )
  
  # evidence for heterogeneity
  evidence_het_text <- format_evidence_section(
    evidence_summary,
    "evidence_for_heterogeneity",
    "#### Evidence for Heterogeneity\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping
  )
  
  # targeting opportunities
  targeting_text <- format_evidence_section(
    evidence_summary,
    "targeting_opportunity",
    "#### Targeting Opportunities\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping
  )
  
  # proceed with caution
  caution_text <- format_evidence_section(
    evidence_summary,
    "mixed_evidence_caution",
    "#### Proceed with Caution\n\n",
    rate_results_list,
    qini_results,
    omnibus_results,
    models,
    spend_levels,
    label_mapping
  )
  
  # avoid targeting - only include models with no positive evidence OR only negative evidence
  avoid_models <- evidence_summary[evidence_summary$positive_count == 0, ]
  
  avoid_text <- if (nrow(avoid_models) > 0) {
    # separate those with negative evidence vs just inconclusive
    negative_models <- avoid_models[avoid_models$negative_count > 0, ]
    inconclusive_models <- avoid_models[avoid_models$negative_count == 0, ]
    
    avoid_parts <- character()
    
    if (nrow(negative_models) > 0) {
      avoid_parts <- c(avoid_parts,
        paste0(paste(negative_models$model_name, collapse = ", "),
               " show reliably negative results in one or more tests, suggesting targeting would worsen outcomes compared to random assignment"))
    }
    
    if (nrow(inconclusive_models) > 0) {
      avoid_parts <- c(avoid_parts,
        paste0(paste(inconclusive_models$model_name, collapse = ", "),
               " show no evidence of heterogeneity across all tests"))
    }
    
    paste0(
      "#### Avoid Targeting\n\n",
      paste(avoid_parts, collapse = ". Additionally, "),
      ".\n"
    )
  } else {
    ""
  }
  
  # combine all sections
  paste0(header, methods_desc, evidence_het_text, targeting_text, caution_text, avoid_text)
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


#' Format evidence section with detailed statistics
#' @keywords internal
format_evidence_section <- function(evidence_summary, evidence_type, header_text,
                                  rate_results_list, qini_results, omnibus_results,
                                  models, spend_levels, label_mapping) {
  
  # filter to relevant outcomes
  outcomes <- evidence_summary[evidence_summary$evidence_type == evidence_type, ]
  
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
      spend_levels
    )
    
    outcome_texts <- c(outcome_texts, outcome_text)
  }
  
  paste0(text, paste(outcome_texts, collapse = "\n\n"), "\n\n")
}


#' Format detailed statistics for a single outcome
#' @keywords internal
format_outcome_details <- function(model_id, model_name, evidence_row,
                                 rate_results_list, qini_results, omnibus_results,
                                 models, spend_levels) {
  
  details <- paste0("**", model_name, "**: ")
  
  # extract RATE statistics if available
  rate_text <- character()
  
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
  
  # combine RATE text
  if (length(rate_text) > 0) {
    details <- paste0(details, "positive RATE ", paste(rate_text, collapse = " and "))
  }
  
  # add omnibus test results
  omnibus_text <- format_omnibus_results(model_id, model_name, omnibus_results)
  if (nchar(omnibus_text) > 0) {
    if (length(rate_text) > 0) {
      details <- paste0(details, ", confirmed by omnibus tests (", omnibus_text, ")")
    } else {
      details <- paste0(details, "omnibus tests show ", omnibus_text)
    }
  }
  
  # add QINI curve differences
  qini_diff_text <- format_qini_differences(model_id, qini_results, spend_levels)
  if (nchar(qini_diff_text) > 0) {
    if (length(rate_text) > 0 || nchar(omnibus_text) > 0) {
      details <- paste0(details, ". Qini differences: ", qini_diff_text)
    } else {
      details <- paste0(details, "positive Qini differences ", qini_diff_text, " suggest practical benefits")
    }
  }
  
  # add interpretation based on evidence type
  if (evidence_row$evidence_type == "evidence_for_heterogeneity") {
    details <- paste0(details, ". This supports effective targeting.")
  } else if (evidence_row$evidence_type == "targeting_opportunity") {
    # add ATE if available
    ate_text <- format_ate_value(model_id, models)
    if (nchar(ate_text) > 0) {
      details <- paste0(details, " despite ", ate_text)
    }
    details <- paste0(details, ". Even modest HTE can improve outcomes for a subset.")
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
    sprintf("%s (%s [95%% CI: %s, %s])", type, est, ci_low, ci_high)
  } else {
    sprintf("%s (%s)", type, est)
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