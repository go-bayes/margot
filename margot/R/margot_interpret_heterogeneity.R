#' Interpret Heterogeneity Evidence from Multiple Sources
#'
#' @description
#' Combines evidence from multiple heterogeneity tests (RATE AUTOC, RATE QINI, 
#' QINI curves, and omnibus calibration tests) to provide unified recommendations 
#' about which models show treatment effect heterogeneity.
#'
#' @param models Output from `margot_causal_forest()` containing model results
#' @param spend_levels Numeric vector of spend levels for QINI analysis. 
#'   Default is c(0.2, 0.5).
#' @param require_any_positive Logical. If TRUE (default), include models that 
#'   show positive evidence in ANY method. If FALSE, require positive evidence 
#'   in ALL methods.
#' @param exclude_negative_any Logical. If TRUE (default), exclude models that 
#'   show negative evidence in ANY method. If FALSE, only exclude if negative 
#'   in ALL methods.
#' @param require_omnibus Logical. If TRUE, only include models that pass the 
#'   omnibus calibration test. Default is FALSE.
#' @param alpha Numeric. Significance level for tests. Default is 0.05.
#' @param adjust Character. Multiple testing adjustment method. Options include 
#'   "BH" (Benjamini-Hochberg), "BY", "bonferroni", "holm", etc. Default is "BH".
#' @param flipped_outcomes Character vector of outcome names that were flipped 
#'   (reversed) in preprocessing. Used for interpretation text.
#' @param label_mapping Named list for mapping model names to human-readable labels.
#' @param verbose Logical. If TRUE, show progress messages. Default is TRUE.
#' @param rate_results Optional pre-computed RATE results to skip computation.
#' @param qini_results Optional pre-computed QINI results to skip computation.
#' @param omnibus_results Optional pre-computed omnibus test results to skip computation.
#'
#' @return A list containing:
#'   \item{selected_model_ids}{Character vector of model IDs with heterogeneity evidence}
#'   \item{selected_model_names}{Character vector of human-readable model names}
#'   \item{excluded_model_ids}{Character vector of model IDs to exclude}
#'   \item{excluded_model_names}{Character vector of human-readable excluded model names}
#'   \item{evidence_summary}{Data frame with detailed evidence by source including evidence_type categorization}
#'   \item{interpretation}{Character string with main interpretation text organized by evidence categories}
#'   \item{summary}{Character string with brief summary}
#'   \item{recommendations}{Character string with actionable recommendations}
#'   \item{rate_results}{List containing AUTOC and QINI RATE results}
#'   \item{qini_results}{QINI curve interpretation results}
#'   \item{omnibus_results}{Omnibus calibration test results}
#'   \item{concordance}{List analyzing agreement between methods}
#'
#' @examples
#' \dontrun{
#' # Simple usage - let the function handle everything
#' het_evidence <- margot_interpret_heterogeneity(
#'   models = causal_forest_results,
#'   spend_levels = c(0.2, 0.5),
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
#' }
#'
#' @export
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success
#' @importFrom dplyr bind_rows mutate case_when
#' @importFrom tibble tibble
margot_interpret_heterogeneity <- function(
  models = NULL,
  spend_levels = c(0.2, 0.5),
  require_any_positive = TRUE,
  exclude_negative_any = TRUE,
  require_omnibus = FALSE,
  alpha = 0.05,
  adjust = "BH",
  flipped_outcomes = NULL,
  label_mapping = NULL,
  verbose = TRUE,
  rate_results = NULL,
  qini_results = NULL,
  omnibus_results = NULL
) {
  
  # validate inputs
  if (is.null(models) && (is.null(rate_results) || is.null(qini_results))) {
    stop("Either 'models' or pre-computed results must be provided")
  }
  
  # header
  if (verbose) cli::cli_h2("Computing heterogeneity evidence")
  
  # initialize rate_results_list
  rate_results_list <- NULL
  
  # 1. compute rate results if not provided
  if (is.null(rate_results) && !is.null(models)) {
    if (verbose) cli::cli_alert_info("Computing RATE estimates...")
    
    # compute rate - margot_rate() handles adjustment internally
    rate_results_list <- margot_rate(
      models, 
      adjust = adjust, 
      alpha = alpha,
      apply_adjustment = TRUE
    )
    
    # get interpretation
    rate_results <- margot_interpret_rate(
      rate_results_list,
      flipped_outcomes = flipped_outcomes,
      adjust_positives_only = TRUE
    )
  }
  
  # 2. compute qini results if not provided
  if (is.null(qini_results) && !is.null(models)) {
    if (verbose) cli::cli_alert_info("Computing QINI curves...")
    
    # run margot_policy to get diff_gain_summaries
    qini_batch <- margot_policy(
      models,
      spend_levels = spend_levels,
      output_objects = c("diff_gain_summaries")
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
    if (verbose) cli::cli_alert_info("Computing omnibus calibration tests...")
    omnibus_results <- margot_omnibus_hetero_test(models)
  }
  
  # 4. combine evidence
  if (verbose) cli::cli_alert_info("Combining heterogeneity evidence...")
  
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
  
  # return results
  if (verbose) cli::cli_alert_success("Heterogeneity analysis complete")
  
  list(
    selected_model_ids = selection_results$selected_ids,
    selected_model_names = selection_results$selected_names,
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
    positive_counts = selection_results$positive_counts
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
    
    # omnibus test status
    omnibus_status <- "not_tested"
    if (!is.null(omnibus_results) && !is.null(omnibus_results$summary_table)) {
      # extract model name without prefix for matching
      clean_id <- sub("^model_", "", id)
      omnibus_row <- omnibus_results$summary_table[
        omnibus_results$summary_table$Outcome == get_model_name(id) |
        omnibus_results$summary_table$Outcome == clean_id,
      ]
      if (nrow(omnibus_row) > 0) {
        if (grepl("Heterogeneity present", omnibus_row$Heterogeneity[1])) {
          omnibus_status <- "positive"
        } else {
          omnibus_status <- "negative"
        }
      }
    }
    
    tibble::tibble(
      model_id = id,  # keep for internal use but will remove later
      model_name = get_model_name(id),
      omnibus_test = omnibus_status,
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
  evidence_summary <- evidence_summary %>%
    dplyr::mutate(
      positive_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini, qini_curve, omnibus_test) == "positive"
      ),
      negative_count = rowSums(
        dplyr::select(., rate_autoc, rate_qini, qini_curve, omnibus_test) == "negative"
      ),
      evidence_type = dplyr::case_when(
        # Evidence for heterogeneity - RATE positive and omnibus confirms
        (rate_autoc == "positive" | rate_qini == "positive") & 
        omnibus_test == "positive" ~ "evidence_for_heterogeneity",
        
        # Targeting opportunity - QINI works despite weak heterogeneity
        qini_curve == "positive" & 
        rate_autoc != "positive" & 
        rate_qini != "positive" ~ "targeting_opportunity",
        
        # Statistical only - omnibus positive but no practical benefit
        omnibus_test == "positive" & 
        qini_curve != "positive" & 
        rate_autoc != "positive" & 
        rate_qini != "positive" ~ "statistical_only",
        
        # Unconfirmed heterogeneity - RATE positive but not confirmed by omnibus
        (rate_autoc == "positive" | rate_qini == "positive") & 
        omnibus_test != "positive" ~ "unconfirmed_heterogeneity",
        
        # Mixed signals
        positive_count > 0 & negative_count > 0 ~ "mixed_evidence",
        
        # No evidence
        TRUE ~ "no_evidence"
      )
    )
  
  # selection logic
  if (require_any_positive) {
    # include if ANY method shows positive
    selected_rows <- evidence_summary$positive_count > 0
  } else {
    # include if ALL tested methods show positive
    tested_cols <- c("rate_autoc", "rate_qini", "qini_curve", "omnibus_test")
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
    tested_cols <- c("rate_autoc", "rate_qini", "qini_curve", "omnibus_test")
    all_negative <- apply(evidence_summary[tested_cols], 1, function(row) {
      tested <- row != "not_tested"
      negative <- row == "negative"
      all(negative[tested]) && any(tested)
    })
    selected_rows <- selected_rows & !all_negative
  }
  
  # require omnibus if specified
  if (require_omnibus) {
    selected_rows <- selected_rows & (evidence_summary$omnibus_test == "positive")
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
    omnibus = sum(evidence_summary$omnibus_test == "positive"),
    any_method = sum(evidence_summary$positive_count > 0),
    all_methods = sum(evidence_summary$positive_count == 4)
  )
  
  # reorder columns and remove model_id for final output
  evidence_summary_final <- evidence_summary %>%
    dplyr::select(model_name, omnibus_test, rate_autoc, rate_qini, 
                  qini_curve, positive_count, negative_count, evidence_type)
  
  list(
    selected_ids = selected_ids,
    selected_names = selected_names,
    excluded_ids = excluded_ids,
    excluded_names = excluded_names,
    positive_counts = positive_counts,
    evidence_summary = evidence_summary_final  # return the reordered evidence_summary without model_id
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
  methods <- c("rate_autoc", "rate_qini", "qini_curve", "omnibus_test")
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
  
  # header
  header <- "### Heterogeneity Evidence Summary\n\nWe evaluated treatment effect heterogeneity using complementary methods that address different aspects. Statistical heterogeneity tests (RATE with AUTOC and QINI weighting, plus omnibus calibration) assess whether treatment effects vary across individuals. Practical targeting tests (QINI curves) assess whether targeting based on predicted effects improves outcomes at specific budget constraints. Each method provides different insights, and their agreement strengthens conclusions."
  
  # organize outcomes by evidence type
  evidence_for_het <- evidence_summary[evidence_summary$evidence_type == "evidence_for_heterogeneity", ]
  targeting_opp <- evidence_summary[evidence_summary$evidence_type == "targeting_opportunity", ]
  statistical_only <- evidence_summary[evidence_summary$evidence_type == "statistical_only", ]
  unconfirmed_het <- evidence_summary[evidence_summary$evidence_type == "unconfirmed_heterogeneity", ]
  mixed_evidence <- evidence_summary[evidence_summary$evidence_type == "mixed_evidence", ]
  
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
    sprintf("\n\n**Targeting Opportunities**: %s show%s practical benefits from targeted allocation (positive QINI) despite limited evidence of overall heterogeneity. This suggests that even modest individual differences can be leveraged for improved outcomes.",
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
  
  # unconfirmed heterogeneity
  unconfirmed_text <- if (nrow(unconfirmed_het) > 0) {
    models_text <- paste(unconfirmed_het$model_name, collapse = ", ")
    sprintf("\n\n**Unconfirmed Heterogeneity**: %s show%s theoretical heterogeneity (RATE) not confirmed by omnibus calibration test. These findings should be interpreted cautiously.",
            models_text,
            ifelse(nrow(unconfirmed_het) == 1, "s", ""))
  } else {
    ""
  }
  
  # mixed evidence
  mixed_text <- if (nrow(mixed_evidence) > 0) {
    models_text <- paste(mixed_evidence$model_name, collapse = ", ")
    sprintf("\n\n**Mixed Evidence**: %s show%s conflicting results across methods. Further investigation is warranted before implementing targeting strategies.",
            models_text,
            ifelse(nrow(mixed_evidence) == 1, "s", ""))
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
    secondary_targets <- unconfirmed_het$model_name
    
    rec_parts <- character()
    if (length(primary_targets) > 0) {
      rec_parts <- c(rec_parts, 
                     sprintf("Focus targeting efforts on %s.", 
                             paste(primary_targets, collapse = ", ")))
    }
    if (length(secondary_targets) > 0) {
      rec_parts <- c(rec_parts,
                     sprintf("Consider %s for secondary analyses with appropriate caution.", 
                             paste(secondary_targets, collapse = ", ")))
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
  full_text <- paste0(header, evidence_text, targeting_text, statistical_text, 
                      unconfirmed_text, mixed_text, recommendations)
  
  # summary
  summary_text <- sprintf(
    "Found heterogeneity evidence for %d of %d models. Evidence for heterogeneity: %d models. Targeting opportunities: %d models. Selected for targeting: %d models.",
    selection_results$positive_counts$any_method,
    nrow(evidence_summary),
    nrow(evidence_for_het),
    nrow(targeting_opp),
    length(selection_results$selected_ids)
  )
  
  list(
    full = full_text,
    summary = summary_text,
    recommendations = gsub("^.*\\*\\*Recommendations\\*\\*: ", "", recommendations)
  )
}