#' Diagnose QINI Gain Discrepancies
#' 
#' Compare QINI gains from plotted data, direct maq calculations, and diff summaries
#' to help identify discrepancies between different calculation methods.
#' 
#' @param mc_result Results from margot_causal_forest or similar with qini_data and qini_objects
#' @param model_names Character vector of models to check (NULL = all models)
#' @param spend_levels Numeric vector of spend levels to check (default: c(0.1, 0.4))
#' @param tolerance Numeric tolerance for flagging discrepancies (default: 0.01)
#' @param verbose Logical for detailed output (default: TRUE)
#' 
#' @return A tibble comparing gain values from different sources with columns:
#'   - model: Model name
#'   - spend: Spend level
#'   - source: Source of the gain value ("plot_data", "maq_direct", "diff_summary")
#'   - cate_gain: CATE gain value
#'   - ate_gain: ATE/baseline gain value  
#'   - difference: CATE - ATE difference
#'   - se: Standard error (where available)
#'   - discrepancy: Logical flag if values differ by more than tolerance
#' 
#' @examples
#' \dontrun{
#' # Run diagnostic on all models
#' diag_results <- margot_qini_diagnostic(mc_result)
#' 
#' # Check specific models at custom spend levels
#' diag_results <- margot_qini_diagnostic(
#'   mc_result,
#'   model_names = c("model_anxiety", "model_depression"),
#'   spend_levels = c(0.1, 0.2, 0.5)
#' )
#' }
#' 
#' @importFrom cli cli_h1 cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble
#' @importFrom maq average_gain
#' 
#' @export
margot_qini_diagnostic <- function(mc_result, 
                                  model_names = NULL,
                                  spend_levels = c(0.1, 0.4),
                                  tolerance = 0.01,
                                  verbose = TRUE) {
  
  if (verbose) cli::cli_h1("QINI Gain Diagnostic")
  
  # determine which models to check
  if (is.null(model_names)) {
    model_names <- names(mc_result$results)
  } else {
    # ensure models exist
    missing <- setdiff(model_names, names(mc_result$results))
    if (length(missing) > 0) {
      cli::cli_alert_warning("Models not found: {paste(missing, collapse = ', ')}")
      model_names <- intersect(model_names, names(mc_result$results))
    }
  }
  
  if (length(model_names) == 0) {
    cli::cli_alert_warning("No valid models to diagnose")
    return(tibble::tibble())
  }
  
  # collect results
  all_results <- list()
  
  for (model_name in model_names) {
    if (verbose) cli::cli_alert_info("Checking model: {model_name}")
    
    model_result <- mc_result$results[[model_name]]
    
    # check if we have necessary components
    if (is.null(model_result$qini_data) || is.null(model_result$qini_objects)) {
      cli::cli_alert_warning("Missing QINI data for {model_name}")
      next
    }
    
    # get qini objects - handle both naming conventions
    qini_objects <- model_result$qini_objects
    cate_obj <- qini_objects$cate %||% qini_objects$treatment
    ate_obj <- qini_objects$ate %||% qini_objects$baseline
    
    if (is.null(cate_obj) || is.null(ate_obj)) {
      cli::cli_alert_warning("Missing CATE or ATE qini objects for {model_name}")
      next
    }
    
    # check each spend level
    for (spend in spend_levels) {
      
      # 1. Extract from plot data
      plot_cate <- extract_gain_from_plot_data(model_result$qini_data, "cate", spend)
      plot_ate <- extract_gain_from_plot_data(model_result$qini_data, "ate", spend)
      
      # handle alternative names
      if (is.na(plot_cate)) {
        plot_cate <- extract_gain_from_plot_data(model_result$qini_data, "CATE", spend)
      }
      if (is.na(plot_ate)) {
        plot_ate <- extract_gain_from_plot_data(model_result$qini_data, "ATE", spend)
      }
      
      plot_result <- tibble::tibble(
        model = model_name,
        spend = spend,
        source = "plot_data",
        cate_gain = plot_cate,
        ate_gain = plot_ate,
        difference = plot_cate - plot_ate,
        se = NA_real_,
        discrepancy = FALSE
      )
      
      # 2. Calculate using maq::average_gain directly
      maq_cate <- tryCatch({
        result <- maq::average_gain(cate_obj, spend = spend)
        if (is.list(result)) result$estimate else result[1]
      }, error = function(e) NA_real_)
      
      maq_ate <- tryCatch({
        result <- maq::average_gain(ate_obj, spend = spend)
        if (is.list(result)) result$estimate else result[1]
      }, error = function(e) NA_real_)
      
      maq_se <- tryCatch({
        cate_result <- maq::average_gain(cate_obj, spend = spend)
        ate_result <- maq::average_gain(ate_obj, spend = spend)
        cate_se <- if (is.list(cate_result)) cate_result$std.err else cate_result[2]
        ate_se <- if (is.list(ate_result)) ate_result$std.err else ate_result[2]
        sqrt(cate_se^2 + ate_se^2)
      }, error = function(e) NA_real_)
      
      maq_result <- tibble::tibble(
        model = model_name,
        spend = spend,
        source = "maq_direct",
        cate_gain = maq_cate,
        ate_gain = maq_ate,
        difference = maq_cate - maq_ate,
        se = maq_se,
        discrepancy = FALSE
      )
      
      # 3. Extract from diff_gain_summaries if available
      diff_result <- NULL
      if (!is.null(mc_result[[model_name]]$diff_gain_summaries)) {
        spend_key <- paste0("spend_", spend)
        if (spend_key %in% names(mc_result[[model_name]]$diff_gain_summaries)) {
          dg <- mc_result[[model_name]]$diff_gain_summaries[[spend_key]]
          
          # extract values from diff_gain string
          diff_vals <- extract_from_diff_gain(dg)
          
          diff_result <- tibble::tibble(
            model = model_name,
            spend = spend,
            source = "diff_summary",
            cate_gain = NA_real_,  # not directly available
            ate_gain = NA_real_,   # not directly available
            difference = diff_vals$estimate,
            se = diff_vals$se,
            discrepancy = FALSE
          )
        }
      }
      
      # combine results for this spend level
      results <- list(plot_result, maq_result)
      if (!is.null(diff_result)) results[[3]] <- diff_result
      
      all_results <- append(all_results, results)
    }
  }
  
  # combine all results
  diagnostic_table <- dplyr::bind_rows(all_results)
  
  # flag discrepancies
  if (nrow(diagnostic_table) > 0) {
    diagnostic_table <- diagnostic_table %>%
      dplyr::group_by(model, spend) %>%
      dplyr::mutate(
        discrepancy = any(
          abs(difference - difference[source == "maq_direct"]) > tolerance,
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup()
  }
  
  # print summary if verbose
  if (verbose && nrow(diagnostic_table) > 0) {
    discrepant_models <- unique(diagnostic_table$model[diagnostic_table$discrepancy])
    
    if (length(discrepant_models) > 0) {
      cli::cli_alert_warning("Discrepancies found in models: {paste(discrepant_models, collapse = ', ')}")
    } else {
      cli::cli_alert_success("No significant discrepancies found (tolerance: {tolerance})")
    }
    
    # print detailed comparison for discrepant models
    for (disc_model in discrepant_models) {
      cli::cli_alert_info("Details for {disc_model}:")
      disc_data <- diagnostic_table[diagnostic_table$model == disc_model, ]
      print(disc_data)
    }
  }
  
  return(diagnostic_table)
}

#' Extract gain value from plot data at specific spend level
#' @keywords internal
extract_gain_from_plot_data <- function(qini_data, curve_name, spend_level) {
  curve_data <- qini_data[qini_data$curve == curve_name, ]
  
  if (nrow(curve_data) == 0) return(NA_real_)
  
  # find closest proportion to spend level
  idx <- which.min(abs(curve_data$proportion - spend_level))
  
  # if exact match or very close, return it
  if (abs(curve_data$proportion[idx] - spend_level) < 0.01) {
    return(curve_data$gain[idx])
  }
  
  # otherwise interpolate
  if (spend_level < min(curve_data$proportion) || spend_level > max(curve_data$proportion)) {
    return(NA_real_)
  }
  
  # linear interpolation
  approx_result <- approx(curve_data$proportion, curve_data$gain, xout = spend_level)
  return(approx_result$y)
}

#' Extract estimate and SE from diff_gain string
#' @keywords internal
extract_from_diff_gain <- function(dg) {
  if (is.null(dg) || !is.list(dg) || is.null(dg$diff_gain)) {
    return(list(estimate = NA_real_, se = NA_real_))
  }
  
  # parse string like "0.123 (SE: 0.045)"
  diff_str <- as.character(dg$diff_gain)
  
  # extract estimate
  est <- as.numeric(sub("^\\s*([+-]?[0-9.]+).*", "\\1", diff_str))
  
  # extract SE
  se <- as.numeric(sub(".*SE:?\\s*([0-9.]+)\\).*", "\\1", diff_str))
  
  list(estimate = est, se = se)
}

# define %||% operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x