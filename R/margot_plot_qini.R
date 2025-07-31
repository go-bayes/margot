#' Plot Qini Curves from margot_multi_arm_causal_forest Results
#'
#' This function creates a ggplot object displaying Qini curves based on the
#' results of a margot_multi_arm_causal_forest() model. It includes label
#' transformations and informative CLI messages. The function is designed to be
#' consistent with maq::plot() functionality while providing additional features
#' like theme selection and spend level indicators.
#'
#' @param mc_result A list containing the results from margot_multi_arm_causal_forest().
#' @param outcome_var A character string specifying the name of the outcome variable
#'   to plot. This should match one of the model names in mc_result$results.
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param spend_levels Numeric vector of spend levels to show with vertical lines. Default is 0.1.
#' @param show_spend_lines Logical indicating whether to show vertical lines at spend levels. Default is TRUE.
#' @param spend_line_color Color for spend level lines. Default is "red".
#' @param spend_line_alpha Alpha transparency for spend lines. Default is 0.5.
#' @param theme Character string specifying the ggplot2 theme. Default is "classic". Options include "classic", "minimal", "bw", "gray", "light", "dark", "void".
#' @param show_ci Logical or character indicating which confidence intervals to show. 
#'   Options: FALSE (none), TRUE or "both" (both curves), "cate" (CATE only), "ate" (ATE only). 
#'   Default is FALSE.
#' @param ci_alpha Significance level for confidence intervals. Default is 0.05.
#' @param ci_n_points Number of points at which to compute confidence intervals. Default is 20.
#' @param ci_ribbon_alpha Alpha transparency for confidence interval ribbons. Default is 0.3.
#' @param ci_ribbon_color Color for confidence interval ribbons. If NULL (default), uses the curve color.
#' @param horizontal_line Logical indicating whether to draw horizontal lines where Qini curves plateau
#'   when the path is complete. Default is TRUE. Consistent with maq::plot() behavior.
#' @param grid_step Integer specifying the step size for subsampling the curve data. If NULL (default),
#'   uses max(floor(nrow(qini_data) / 1000), 1). Set to 1 to plot all points.
#' @param return_data Logical indicating whether to return the plot data as a data.frame instead of
#'   the ggplot object. Default is FALSE. When TRUE, returns data with columns: proportion, gain,
#'   lower, upper, curve.
#' @param ylim Numeric vector of length 2 specifying the y-axis limits c(min, max). Default is NULL (automatic scaling).
#' @param fixed_ylim Logical; if TRUE and ylim is NULL, uses the actual data range with padding
#'   for consistent y-axis scaling across plots. Default is FALSE.
#' @param baseline_method Method for generating baseline: "auto" (default), 
#'   "maq_no_covariates", "simple", "maq_only", or "none". See details in margot_generate_qini_data().
#' @param cate_color Color for the CATE (targeted treatment) curve. Default is "#d8a739" (gold).
#' @param ate_color Color for the ATE (no-priority/uniform assignment) curve. Default is "#4d4d4d" (dark gray).
#' @param scale Character string specifying the scale for gains: "average" (default), "cumulative", 
#'   or "population". "average" shows average policy effect per unit (maq default), "cumulative" 
#'   shows traditional cumulative gains, "population" shows total population impact.
#' @param treatment_cost Numeric scalar; the treatment cost used in QINI calculations. Default is NULL,
#'   which attempts to extract the cost from model metadata. If not found, assumes cost = 1.
#'   When cost differs from stored cost, QINI curves are automatically regenerated.
#'   When cost differs from 1, it will be shown in the plot subtitle.
#' @param seed Integer; seed for reproducible QINI generation when treatment_cost differs
#'   from stored cost. Default is 12345.
#' @param x_axis Type of x-axis for QINI curves: "proportion" (default) or "budget".
#'   "proportion" shows proportion of population treated (0 to 1).
#'   "budget" shows budget per unit (0 to treatment_cost), matching maq's visualization.
#'
#' @return If return_data is FALSE (default), returns a ggplot object. If return_data is TRUE,
#'   returns a data.frame with the plot data.
#'
#' @details 
#' This function provides maq::plot() compatible features including:
#' \itemize{
#'   \item Horizontal line extension for complete paths
#'   \item Grid step subsampling for large datasets
#'   \item Data frame return option matching maq::plot() output format
#'   \item Standard error extraction from maq objects
#' }
#' 
#' Key differences from maq::plot():
#' \itemize{
#'   \item Uses ggplot2 instead of base R graphics
#'   \item More descriptive axis labels
#'   \item Additional features like theme selection and spend indicators
#'   \item Confidence intervals computed via maq::average_gain() for accuracy
#'   \item Binary treatment colors: Customizable via cate_color and ate_color parameters
#' }
#' 
#' \strong{Important Note on Scale:} The y-axis shows \strong{average policy effects per unit}, 
#' not cumulative gains. This follows the maq package implementation where gains represent 
#' Q(B) = E[⟨πB(Xi), τ(Xi)⟩], the expected (average) gain from treating units according 
#' to the policy πB. This differs from traditional uplift modeling QINI curves which show 
#' cumulative gains. At 100\% spend, both CATE and ATE curves converge to similar values 
#' because the average effect is similar regardless of treatment ordering when everyone is treated.
#'
#' @import ggplot2
#' @import cli
#' @importFrom ggokabeito scale_color_okabe_ito scale_fill_okabe_ito
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#'
#' @examples
#' \dontrun{
#' # Basic usage - uses stored treatment cost
#' margot_plot_qini(cf_results, "model_anxiety")
#'
#' # Auto-regenerates with different treatment cost
#' plot1 <- margot_plot_qini(cf_results, "model_anxiety", treatment_cost = 1)
#' plot2 <- margot_plot_qini(cf_results, "model_anxiety", treatment_cost = 5)
#'
#' # Compare costs with fixed y-axis scaling
#' plot3 <- margot_plot_qini(cf_results, "model_anxiety", treatment_cost = 5, fixed_ylim = TRUE)
#'
#' # Show CI only for CATE curve
#' plot4 <- margot_plot_qini(cf_results, "model_anxiety", show_ci = "cate")
#'
#' # Show CI for both curves
#' plot5 <- margot_plot_qini(cf_results, "model_anxiety", show_ci = TRUE)
#'
#' # Custom seed for reproducibility when regenerating
#' plot6 <- margot_plot_qini(cf_results, "model_anxiety", treatment_cost = 2, seed = 42)
#'
#' # Using custom label mapping
#' label_mapping <- list(
#'   "anxiety" = "Anxiety Symptoms",
#'   "depression" = "Depression Symptoms"
#' )
#' margot_plot_qini(cf_results, "model_anxiety", label_mapping = label_mapping)
#' }
#'
#' @export
margot_plot_qini <- function(mc_result, outcome_var,
                             label_mapping = NULL,
                             spend_levels = 0.1,
                             show_spend_lines = TRUE,
                             spend_line_color = "red",
                             spend_line_alpha = 0.5,
                             theme = "classic",
                             show_ci = FALSE,
                             ci_alpha = 0.05,
                             ci_n_points = 20,
                             ci_ribbon_alpha = 0.3,
                             ci_ribbon_color = NULL,
                             horizontal_line = TRUE,
                             grid_step = NULL,
                             return_data = FALSE,
                             ylim = NULL,
                             fixed_ylim = FALSE,
                             baseline_method = "auto",
                             cate_color = "#d8a739",
                             ate_color = "#4d4d4d",
                             scale = "average",
                             treatment_cost = NULL,
                             seed = 12345,
                             x_axis = c("proportion", "budget")) {
  cli::cli_h1("Margot Plot Qini Curves")
  
  # match x_axis argument
  x_axis <- match.arg(x_axis)

  # Transform the outcome variable name
  # First try direct lookup in label_mapping if provided
  transformed_outcome_var <- outcome_var
  
  if (!is.null(label_mapping)) {
    # Try exact match first (with model_ prefix)
    if (outcome_var %in% names(label_mapping)) {
      transformed_outcome_var <- label_mapping[[outcome_var]]
    } else {
      # Try without model_ prefix
      outcome_var_clean <- sub("^model_", "", outcome_var)
      if (outcome_var_clean %in% names(label_mapping)) {
        transformed_outcome_var <- label_mapping[[outcome_var_clean]]
      } else {
        # Use transform_label as fallback with label_mapping
        transformed_outcome_var <- transform_label(
          label = outcome_var,
          label_mapping = label_mapping,
          options = list(
            remove_tx_prefix = TRUE,
            remove_z_suffix = TRUE,
            remove_underscores = TRUE,
            use_title_case = TRUE
          )
        )
      }
    }
  } else {
    # No label mapping provided, use transform_label for default transformations
    transformed_outcome_var <- transform_label(
      label = outcome_var,
      label_mapping = NULL,
      options = list(
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        remove_underscores = TRUE,
        use_title_case = TRUE
      )
    )
  }

  # Extract the qini data for the specified outcome variable
  # Handle both with and without model_ prefix
  if (!outcome_var %in% names(mc_result$results)) {
    # Try adding model_ prefix
    outcome_var_with_prefix <- paste0("model_", outcome_var)
    if (outcome_var_with_prefix %in% names(mc_result$results)) {
      outcome_var <- outcome_var_with_prefix
    } else {
      cli::cli_abort("Outcome variable not found in mc_result$results: {outcome_var} (also tried {outcome_var_with_prefix})")
    }
  }

  # initialize treatment_cost early so it's available for regeneration
  if (is.null(treatment_cost)) {
    # try to get from qini metadata
    if (!is.null(mc_result$results[[outcome_var]]$qini_metadata) &&
        !is.null(mc_result$results[[outcome_var]]$qini_metadata$treatment_cost)) {
      treatment_cost <- mc_result$results[[outcome_var]]$qini_metadata$treatment_cost
    } else {
      # assume default of 1
      treatment_cost <- 1
    }
  }
  
  # check if qini_data already exists, otherwise generate it
  qini_data <- mc_result$results[[outcome_var]]$qini_data
  qini_objects <- mc_result$results[[outcome_var]]$qini_objects
  
  # check if we need to regenerate due to different baseline method
  regenerate_needed <- FALSE
  if (!is.null(qini_data)) {
    # check if current data matches requested baseline method
    has_ate <- "ate" %in% unique(qini_data$curve) || "ATE" %in% unique(qini_data$curve)
    
    if (baseline_method == "none" && has_ate) {
      # want no baseline but have one
      regenerate_needed <- TRUE
      cli::cli_alert_info("Regenerating QINI data without baseline curve")
    } else if (baseline_method != "none" && !has_ate) {
      # want baseline but don't have one
      regenerate_needed <- TRUE
      cli::cli_alert_info("Regenerating QINI data with {baseline_method} baseline")
    } else if (baseline_method == "simple" || baseline_method == "maq_no_covariates" || baseline_method == "maq_only") {
      # for specific baseline methods, always regenerate to ensure we get the right type
      regenerate_needed <- TRUE
      cli::cli_alert_info("Regenerating QINI data with {baseline_method} baseline")
    }
    
    # also check if treatment cost has changed
    stored_cost <- 1
    if (!is.null(mc_result$results[[outcome_var]]$qini_metadata) &&
        !is.null(mc_result$results[[outcome_var]]$qini_metadata$treatment_cost)) {
      stored_cost <- mc_result$results[[outcome_var]]$qini_metadata$treatment_cost
    }
    if (treatment_cost != stored_cost) {
      regenerate_needed <- TRUE
      cli::cli_alert_info("Regenerating QINI curves with treatment_cost = {treatment_cost} (was {stored_cost})")
    }
  }
  
  # Try to regenerate if needed and possible
  if (is.null(qini_data) || regenerate_needed) {
    # First check if we can actually regenerate before trying
    can_regenerate <- FALSE
    
    # Quick check if data is available
    model_result <- mc_result$results[[outcome_var]]
    outcome_name_clean <- gsub("^model_", "", outcome_var)
    
    # Check various data sources
    if (!is.null(mc_result$data) && (outcome_name_clean %in% names(mc_result$data) || outcome_var %in% names(mc_result$data))) {
      can_regenerate <- TRUE
    } else if (!is.null(model_result$model) && !is.null(model_result$model$Y.orig)) {
      can_regenerate <- TRUE
    } else if (!is.null(model_result$full_model) && !is.null(model_result$full_model$Y.orig)) {
      can_regenerate <- TRUE
    } else if (!is.null(mc_result$full_models) && outcome_var %in% names(mc_result$full_models)) {
      full_model <- mc_result$full_models[[outcome_var]]
      if (!is.null(full_model) && !is.null(full_model$Y.orig)) {
        can_regenerate <- TRUE
      }
    }
    
    if (!can_regenerate && regenerate_needed) {
      # Can't regenerate - use existing data if available
      if (!is.null(qini_data)) {
        cli::cli_alert_warning("Cannot regenerate QINI with {baseline_method} baseline - data not available. Using existing QINI data.")
        regenerate_needed <- FALSE
      } else {
        cli::cli_abort("Cannot generate QINI curves - no data available for {outcome_var}")
      }
    }
  }
  
  # Now proceed with regeneration if still needed
  if (is.null(qini_data) || regenerate_needed) {
    cli::cli_alert_info("Generating QINI curves on-demand for {outcome_var}")
    
    # extract necessary components
    model_result <- mc_result$results[[outcome_var]]
    
    # find outcome data - try various locations
    outcome_name_clean <- gsub("^model_", "", outcome_var)
    is_flipped <- grepl("_r$", outcome_name_clean)
    outcome_data <- NULL
    
    # For flipped models, prioritize data from forest object which has the correct data
    if (is_flipped) {
      if (!is.null(model_result$model) && !is.null(model_result$model$Y.orig)) {
        outcome_data <- model_result$model$Y.orig
        cli::cli_alert_info("Using outcome data from forest object for flipped model")
      } else if (!is.null(model_result$full_model) && !is.null(model_result$full_model$Y.orig)) {
        outcome_data <- model_result$full_model$Y.orig
        cli::cli_alert_info("Using outcome data from full_model forest object for flipped model")
      } else {
        # check if model is in full_models
        if (!is.null(mc_result$full_models) && outcome_var %in% names(mc_result$full_models)) {
          full_model <- mc_result$full_models[[outcome_var]]
          if (!is.null(full_model) && !is.null(full_model$Y.orig)) {
            outcome_data <- full_model$Y.orig
            cli::cli_alert_info("Using outcome data from full_models storage for flipped model")
          } else {
            cli::cli_alert_warning("Forest object found in full_models but Y.orig is NULL")
          }
        } else {
          cli::cli_alert_warning("No forest object found for flipped model {outcome_var}")
          cli::cli_alert_info("model_result$model exists: {!is.null(model_result$model)}")
          cli::cli_alert_info("model_result$full_model exists: {!is.null(model_result$full_model)}")
          cli::cli_alert_info("mc_result$full_models exists: {!is.null(mc_result$full_models)}")
          if (!is.null(mc_result$full_models)) {
            cli::cli_alert_info("Models in full_models: {paste(names(mc_result$full_models), collapse = ', ')}")
          }
        }
      }
    }
    
    # If not found yet, try standard locations
    if (is.null(outcome_data)) {
      if (is.null(mc_result$data)) {
        cli::cli_alert_warning("mc_result$data is NULL - checking for data in model result")
        # try to get data from the model result itself
        if (!is.null(model_result$Y)) {
          outcome_data <- model_result$Y
        } else if (!is.null(model_result$model) && !is.null(model_result$model$Y.orig)) {
          # try to get from the grf forest object
          outcome_data <- model_result$model$Y.orig
          cli::cli_alert_info("Found outcome data in forest object (Y.orig)")
        } else if (!is.null(model_result$full_model) && !is.null(model_result$full_model$Y.orig)) {
          # try full_model as well
          outcome_data <- model_result$full_model$Y.orig
          cli::cli_alert_info("Found outcome data in full_model forest object")
        }
      } else if (outcome_name_clean %in% names(mc_result$data)) {
        outcome_data <- mc_result$data[[outcome_name_clean]]
      } else if (outcome_var %in% names(mc_result$data)) {
        outcome_data <- mc_result$data[[outcome_var]]
      }
    }
    
    if (is.null(outcome_data)) {
      # provide more helpful error message
      if (!is.null(mc_result$data)) {
        available_outcomes <- names(mc_result$data)
        cli::cli_abort(c(
          "Cannot find outcome data for {outcome_var}",
          "i" = "Looked for: {outcome_name_clean} and {outcome_var}",
          "i" = "Available outcomes in data: {paste(available_outcomes, collapse = ', ')}"
        ))
      } else {
        cli::cli_abort(c(
          "Cannot find outcome data for {outcome_var}",
          "i" = "mc_result$data is NULL and no data found in forest objects",
          "i" = "For flipped models, ensure forest objects contain Y.orig"
        ))
      }
    }
    
    # get treatment and weights
    W <- mc_result$W
    weights <- mc_result$weights
    
    if (is.null(W)) {
      # try to get from forest object
      if (!is.null(model_result$model) && !is.null(model_result$model$W.orig)) {
        W <- model_result$model$W.orig
        cli::cli_alert_info("Found treatment data in forest object (W.orig)")
      } else if (!is.null(model_result$full_model) && !is.null(model_result$full_model$W.orig)) {
        W <- model_result$full_model$W.orig
        cli::cli_alert_info("Found treatment data in full_model forest object")
      } else {
        cli::cli_abort("Treatment assignment vector (W) not found in mc_result or forest objects")
      }
    }
    
    # generate qini data
    qini_result <- tryCatch({
      margot_generate_qini_data(
        model_result = model_result,
        outcome_data = outcome_data,
        treatment = W,
        weights = weights,
        baseline_method = baseline_method,
        seed = 42,
        verbose = TRUE,
        treatment_cost = treatment_cost,
        x_axis = x_axis
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to regenerate QINI data: {e$message}")
      
      # try to use existing QINI data if available
      if (!is.null(mc_result$results[[outcome_var]]$qini_data)) {
        cli::cli_alert_info("Falling back to existing QINI data (baseline method may differ)")
        list(
          qini_data = mc_result$results[[outcome_var]]$qini_data,
          qini_objects = mc_result$results[[outcome_var]]$qini_objects
        )
      } else {
        NULL
      }
    })
    
    if (!is.null(qini_result)) {
      qini_data <- qini_result$qini_data
      qini_objects <- qini_result$qini_objects
    } else {
      cli::cli_abort("Failed to generate QINI data for {outcome_var} and no existing data available")
    }
  }
  
  # Handle case where we want a different baseline but can't regenerate
  if (regenerate_needed && is.null(mc_result$data) && !is.null(qini_data)) {
    if (baseline_method == "simple") {
      # Special case: can't regenerate but want simple baseline
      # If we have CATE data, we can add a simple baseline
      has_cate <- "cate" %in% unique(qini_data$curve) || "CATE" %in% unique(qini_data$curve)
      has_ate <- "ate" %in% unique(qini_data$curve) || "ATE" %in% unique(qini_data$curve)
      
      if (has_cate) {
        cli::cli_alert_info("Cannot regenerate data, but adding simple baseline to existing QINI data")
        
        # Get model result
        model_result <- mc_result$results[[outcome_var]]
        
        # Try to get mean tau from existing objects or model
        mean_tau <- NULL
        if (!is.null(model_result$tau_hat)) {
          mean_tau <- mean(model_result$tau_hat)
        } else if (!is.null(model_result$ATE) && is.numeric(model_result$ATE)) {
          mean_tau <- model_result$ATE
        } else if (!is.null(model_result$ate) && is.numeric(model_result$ate)) {
          mean_tau <- model_result$ate
        } else if (!is.null(model_result$estimate) && is.numeric(model_result$estimate)) {
          # Try the estimate field (common in causal forest output)
          mean_tau <- model_result$estimate
        } else if (!is.null(model_result$custom_table) && "E[Y(1)]-E[Y(0)]" %in% rownames(model_result$custom_table)) {
          # Try to extract from custom_table
          ate_row <- model_result$custom_table["E[Y(1)]-E[Y(0)]", , drop = FALSE]
          if ("Estimate" %in% colnames(ate_row)) {
            mean_tau <- ate_row[1, "Estimate"]
          }
        }
        
        if (!is.null(mean_tau)) {
          # Add simple baseline to existing data
          n_points <- length(unique(qini_data$proportion))
          baseline_data <- data.frame(
            proportion = seq(0, 1, length.out = n_points),
            gain = seq(0, 1, length.out = n_points) * mean_tau,
            curve = "ate"
          )
          # Remove any existing ate curve and add the new one
          qini_data <- qini_data[!(qini_data$curve %in% c("ate", "ATE")), ]
          qini_data <- rbind(qini_data, baseline_data)
          cli::cli_alert_success("Added simple baseline with mean_tau = {round(mean_tau, 4)}")
        } else {
          cli::cli_alert_warning("Could not find mean_tau to create simple baseline")
          cli::cli_alert_info("Available fields in model_result: {paste(names(model_result), collapse = ', ')}")
        }
      }
    }
  }
  
  cli::cli_alert_success("Qini data ready for outcome variable: {outcome_var}")
  cli::cli_alert_info("Structure of qini_data:")
  print(str(qini_data))

  # Check if qini_data is empty
  if (nrow(qini_data) == 0) {
    cli::cli_abort("Qini data is empty for the specified outcome variable: {outcome_var}")
  }

  # Ensure required columns are present
  required_cols <- c("proportion", "gain", "curve")
  if (!all(required_cols %in% colnames(qini_data))) {
    missing_cols <- setdiff(required_cols, colnames(qini_data))
    cli::cli_abort("Required columns missing from Qini data: {paste(missing_cols, collapse = ', ')}")
  }

  # Check if treatment is binary - look for cate/ate curve names
  unique_curves <- unique(qini_data$curve)
  is_binary <- all(unique_curves %in% c("cate", "ate")) || length(unique_curves) <= 2

  # Transform curve labels
  if (is_binary) {
    # For binary treatments, use consistent CATE/ATE naming
    transformed_curves <- character(length(unique_curves))
    names(transformed_curves) <- unique_curves
    for (i in seq_along(unique_curves)) {
      if (unique_curves[i] %in% c("cate", "treatment")) {
        transformed_curves[i] <- "CATE"
      } else if (unique_curves[i] %in% c("ate", "baseline")) {
        transformed_curves[i] <- "ATE"
      } else {
        transformed_curves[i] <- toupper(unique_curves[i])
      }
    }
  } else {
    # For multi-arm, use standard label transformation
    transformed_curves <- sapply(unique_curves, function(x) transform_label(
      label = x,
      label_mapping = label_mapping,
      options = list(
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        remove_underscores = TRUE,
        use_title_case = TRUE
      )
    ))
  }

  curve_mapping <- setNames(transformed_curves, unique_curves)
  qini_data$curve <- curve_mapping[qini_data$curve]
  
  # ensure curve is a factor with both levels to maintain consistent colors
  if (is_binary) {
    qini_data$curve <- factor(qini_data$curve, levels = c("CATE", "ATE"))
  } else {
    qini_data$curve <- factor(qini_data$curve)
  }
  
  cli::cli_alert_success("Treatment curve labels transformed")
  
  # apply scale transformation
  if (!is.null(scale)) {
    cli::cli_alert_info("Applying scale transformation: {scale}")
    
    # store original gains for reference
    qini_data$original_gain <- qini_data$gain
    
    if (scale == "cumulative") {
      # transform from average to cumulative: multiply by proportion
      qini_data$gain <- qini_data$gain * qini_data$proportion
      cli::cli_alert_success("Transformed gains to cumulative scale")
      
    } else if (scale == "population") {
      # transform to population scale
      # need to get the total number of units
      n_units <- NULL
      
      # try to get n_units from qini metadata
      if (!is.null(mc_result$results[[outcome_var]]$qini_metadata$n_test)) {
        n_units <- mc_result$results[[outcome_var]]$qini_metadata$n_test
      } else if (!is.null(mc_result$not_missing)) {
        n_units <- length(mc_result$not_missing)
      } else if (!is.null(mc_result$data)) {
        n_units <- nrow(mc_result$data)
      }
      
      if (!is.null(n_units)) {
        # population gain = average gain * proportion * n_units
        qini_data$gain <- qini_data$gain * qini_data$proportion * n_units
        cli::cli_alert_success("Transformed gains to population scale (n = {n_units})")
      } else {
        cli::cli_alert_warning("Could not determine population size, using relative scale")
        # fall back to cumulative
        qini_data$gain <- qini_data$gain * qini_data$proportion
      }
    }
    # else scale == "average", no transformation needed
  }
  
  # apply grid step subsampling if needed
  if (is.null(grid_step)) {
    grid_step <- max(floor(nrow(qini_data) / 1000), 1)
  }
  
  if (grid_step > 1) {
    # subsample the data
    indices <- seq(1, nrow(qini_data), by = grid_step)
    # ensure we always include the last point for each curve
    for (crv in unique(qini_data$curve)) {
      crv_data <- qini_data[qini_data$curve == crv, ]
      last_idx <- nrow(crv_data)
      if (!(last_idx %in% indices)) {
        indices <- c(indices, last_idx)
      }
    }
    indices <- sort(unique(indices))
    qini_data <- qini_data[indices, ]
    cli::cli_alert_info("Subsampled qini data using grid_step = {grid_step}")
  }

  # parse show_ci parameter
  compute_ci <- FALSE
  ci_curves <- character(0)
  
  if (is.logical(show_ci)) {
    if (show_ci) {
      compute_ci <- TRUE
      ci_curves <- c("cate", "ate")  # show both
    }
  } else if (is.character(show_ci)) {
    show_ci <- tolower(show_ci)
    if (show_ci %in% c("both", "true")) {
      compute_ci <- TRUE
      ci_curves <- c("cate", "ate")
    } else if (show_ci == "cate") {
      compute_ci <- TRUE
      ci_curves <- "cate"
    } else if (show_ci == "ate") {
      compute_ci <- TRUE
      ci_curves <- "ate"
    } else if (show_ci != "false") {
      cli::cli_alert_warning("Invalid show_ci value '{show_ci}'. Using FALSE.")
    }
  }
  
  # compute confidence intervals if requested
  ci_data <- NULL
  if (compute_ci) {
    cli::cli_alert_info("Computing confidence intervals for {paste(ci_curves, collapse = ' and ')} curve(s)...")
    
    # qini_objects should already be available from the generation above
    # no need to re-extract
    
    if (!is.null(qini_objects)) {
      # create spend sequence for CI computation
      # include small buffer at extremes to avoid numerical issues
      spend_seq <- seq(0.01, 0.99, length.out = ci_n_points)
      
      # function to compute CIs for a single qini object
      compute_curve_ci <- function(qini_obj, curve_name) {
        ci_list <- lapply(spend_seq, function(s) {
          tryCatch({
            # handle both maq objects and simple baselines
            if (inherits(qini_obj, "qini_simple_baseline")) {
              avg_gain_result <- average_gain.qini_simple_baseline(qini_obj, spend = s)
            } else {
              avg_gain_result <- maq::average_gain(qini_obj, spend = s)
            }
            
            # handle different return formats from maq
            if (is.list(avg_gain_result)) {
              estimate <- avg_gain_result$estimate
              std_err <- avg_gain_result$std.err
            } else if (is.numeric(avg_gain_result) && length(avg_gain_result) >= 2) {
              estimate <- avg_gain_result[1]
              std_err <- avg_gain_result[2]
            } else {
              return(NULL)
            }
            
            z_val <- qnorm(1 - ci_alpha/2)
            lower <- estimate - z_val * std_err
            upper <- estimate + z_val * std_err
            
            # we'll use the std_err but need to center on actual gain values
            data.frame(
              proportion = s,
              estimate = estimate,
              std_err = std_err,
              lower = lower,
              upper = upper,
              curve = curve_name
            )
          }, error = function(e) {
            cli::cli_alert_warning("CI computation failed at spend = {s}: {e$message}")
            return(NULL)
          })
        })
        
        # combine results and remove NULLs
        ci_results <- ci_list[!sapply(ci_list, is.null)]
        if (length(ci_results) > 0) {
          # ensure all results are data frames with the same columns
          ci_results <- lapply(ci_results, function(x) {
            if (is.data.frame(x) && all(c("proportion", "estimate", "std_err", "lower", "upper", "curve") %in% names(x))) {
              x[, c("proportion", "estimate", "std_err", "lower", "upper", "curve")]
            } else {
              NULL
            }
          })
          ci_results <- ci_results[!sapply(ci_results, is.null)]
          
          if (length(ci_results) > 0) {
            do.call(rbind, ci_results)
          } else {
            NULL
          }
        } else {
          NULL
        }
      }
      
      # compute CIs for each curve
      ci_list <- list()
      
      # for binary treatments - check both old and new naming conventions
      if (is_binary) {
        # cli::cli_alert_info("Binary treatment detected, checking for CATE and ATE curves")
        
        # check each curve individually based on ci_curves selection
        if ("cate" %in% ci_curves) {
          if ("cate" %in% names(qini_objects)) {
            # cli::cli_alert_info("Computing CI for CATE curve")
            result <- compute_curve_ci(qini_objects$cate, "CATE")
            if (!is.null(result)) {
              ci_list[["CATE"]] <- result
              # cli::cli_alert_info("CATE CI computed: {nrow(result)} rows")
            }
          } else if ("treatment" %in% names(qini_objects)) {
            # cli::cli_alert_info("Computing CI for treatment curve (as CATE)")
            result <- compute_curve_ci(qini_objects$treatment, "CATE")
            if (!is.null(result)) ci_list[["CATE"]] <- result
          }
        }
        
        if ("ate" %in% ci_curves) {
          if ("ate" %in% names(qini_objects)) {
            # cli::cli_alert_info("Computing CI for ATE curve")
            result <- compute_curve_ci(qini_objects$ate, "ATE")
            if (!is.null(result)) {
              ci_list[["ATE"]] <- result
              # cli::cli_alert_info("ATE CI computed: {nrow(result)} rows")
            }
          } else if ("baseline" %in% names(qini_objects)) {
            # cli::cli_alert_info("Computing CI for baseline curve (as ATE)")
            result <- compute_curve_ci(qini_objects$baseline, "ATE")
            if (!is.null(result)) ci_list[["ATE"]] <- result
          }
        }
      } else {
        # for multi-arm or other cases
        for (curve_name in names(qini_objects)) {
          mapped_name <- curve_mapping[curve_name]
          if (!is.null(mapped_name)) {
            ci_list[[mapped_name]] <- compute_curve_ci(qini_objects[[curve_name]], mapped_name)
          }
        }
      }
      
      # combine CI data with better error handling
      non_null_ci <- ci_list[!sapply(ci_list, is.null)]
      if (length(non_null_ci) > 0) {
        tryCatch({
          ci_data <- do.call(rbind, non_null_ci)
        }, error = function(e) {
          cli::cli_alert_warning("Error combining CI data: {e$message}")
          # debug: check structure of each CI result
          for (nm in names(non_null_ci)) {
            cli::cli_alert_info("CI structure for {nm}: {paste(names(non_null_ci[[nm]]), collapse = ', ')}")
          }
          ci_data <- NULL
        })
      } else {
        ci_data <- NULL
      }
      
      # re-center CIs on actual gain values from qini_data
      if (!is.null(ci_data) && nrow(ci_data) > 0) {
        for (crv in unique(ci_data$curve)) {
          # get the actual gains from qini_data
          qini_crv_data <- qini_data[qini_data$curve == crv, ]
          ci_crv_idx <- ci_data$curve == crv
          
          if (nrow(qini_crv_data) > 0) {
            # for each CI point, find the actual gain at that proportion
            for (i in which(ci_crv_idx)) {
              # find closest proportion in qini_data
              idx <- which.min(abs(qini_crv_data$proportion - ci_data$proportion[i]))
              actual_gain <- qini_crv_data$gain[idx]
              
              # recenter the CI on the actual gain
              if (length(actual_gain) > 0 && !is.na(actual_gain)) {
                z_val <- qnorm(1 - ci_alpha/2)
                ci_data$lower[i] <- actual_gain - z_val * ci_data$std_err[i]
                ci_data$upper[i] <- actual_gain + z_val * ci_data$std_err[i]
                ci_data$estimate[i] <- actual_gain  # use actual gain as estimate
              }
            }
          }
        }
        
        # remove std_err column as it's no longer needed
        if ("std_err" %in% names(ci_data)) {
          ci_data$std_err <- NULL
        }
      }
      
      # add endpoints at 0 and 1 if missing
      if (!is.null(ci_data) && nrow(ci_data) > 0) {
        # for each curve, ensure we have 0 and 1
        curves <- unique(ci_data$curve)
        
        # get column names from existing ci_data to ensure consistency
        ci_cols <- names(ci_data)
        
        for (crv in curves) {
          crv_data <- ci_data[ci_data$curve == crv, ]
          # add 0 if missing
          if (nrow(crv_data) > 0 && !any(crv_data$proportion == 0)) {
            new_row <- data.frame(
              proportion = 0,
              estimate = 0,
              lower = 0,
              upper = 0,
              curve = crv,
              stringsAsFactors = FALSE
            )
            # ensure columns match exactly
            new_row <- new_row[, ci_cols, drop = FALSE]
            ci_data <- rbind(ci_data, new_row)
          }
          # add 1 if missing (use last available values)
          if (nrow(crv_data) > 0 && !any(crv_data$proportion == 1)) {
            last_row <- crv_data[which.max(crv_data$proportion), ]
            new_row <- data.frame(
              proportion = 1,
              estimate = last_row$estimate * (1 / last_row$proportion),  # extrapolate
              lower = last_row$lower * (1 / last_row$proportion),
              upper = last_row$upper * (1 / last_row$proportion),
              curve = crv,
              stringsAsFactors = FALSE
            )
            # ensure columns match exactly
            new_row <- new_row[, ci_cols, drop = FALSE]
            ci_data <- rbind(ci_data, new_row)
          }
        }
        # sort by curve and proportion
        ci_data <- ci_data[order(ci_data$curve, ci_data$proportion), ]
      }
      
      # apply scale transformation to CI data if needed
      if (!is.null(ci_data) && !is.null(scale) && scale != "average") {
        cli::cli_alert_info("Applying scale transformation to confidence intervals")
        
        if (scale == "cumulative") {
          # transform CI bounds to cumulative scale
          ci_data$estimate <- ci_data$estimate * ci_data$proportion
          ci_data$lower <- ci_data$lower * ci_data$proportion
          ci_data$upper <- ci_data$upper * ci_data$proportion
          
        } else if (scale == "population") {
          # use same n_units as before
          n_units <- NULL
          if (!is.null(mc_result$results[[outcome_var]]$qini_metadata$n_test)) {
            n_units <- mc_result$results[[outcome_var]]$qini_metadata$n_test
          } else if (!is.null(mc_result$not_missing)) {
            n_units <- length(mc_result$not_missing)
          } else if (!is.null(mc_result$data)) {
            n_units <- nrow(mc_result$data)
          }
          
          if (!is.null(n_units)) {
            ci_data$estimate <- ci_data$estimate * ci_data$proportion * n_units
            ci_data$lower <- ci_data$lower * ci_data$proportion * n_units
            ci_data$upper <- ci_data$upper * ci_data$proportion * n_units
          } else {
            # fall back to cumulative
            ci_data$estimate <- ci_data$estimate * ci_data$proportion
            ci_data$lower <- ci_data$lower * ci_data$proportion
            ci_data$upper <- ci_data$upper * ci_data$proportion
          }
        }
      }
      
      # also try to get std.err directly from maq objects for consistency
      std_err_data <- NULL
      for (qini_name in names(qini_objects)) {
        qini_obj <- qini_objects[[qini_name]]
        if (!is.null(qini_obj[["_path"]]$std.err)) {
          # map qini object name to curve name
          curve_name <- NULL
          if (qini_name %in% c("cate", "treatment")) curve_name <- "CATE"
          else if (qini_name %in% c("ate", "baseline")) curve_name <- "ATE"
          else if (qini_name %in% names(curve_mapping)) curve_name <- curve_mapping[qini_name]
          
          if (!is.null(curve_name)) {
            # extract std.err at the same proportions as the main data
            main_props <- unique(qini_data$proportion[qini_data$curve == curve_name])
            std_err_values <- qini_obj[["_path"]]$std.err
            
            # check if we have enough values to interpolate
            if (length(std_err_values) < 2 || length(main_props) < 1) {
              next  # skip this curve
            }
            
            path_props <- seq(0, 1, length.out = length(std_err_values))
            
            # interpolate std.err to match main proportions
            std_err_interp <- tryCatch({
              approx(path_props, std_err_values, 
                     xout = main_props, rule = 2)$y
            }, error = function(e) {
              cli::cli_alert_warning("Could not interpolate std.err for {curve_name}")
              NULL
            })
            
            if (is.null(std_err_interp)) {
              next
            }
            
            temp_df <- data.frame(
              proportion = main_props,
              std_err = std_err_interp,
              curve = curve_name
            )
            
            if (is.null(std_err_data)) {
              std_err_data <- temp_df
            } else {
              std_err_data <- rbind(std_err_data, temp_df)
            }
          }
        }
      }
      
      # if we have direct std.err data, we can add it to our results
      if (!is.null(std_err_data)) {
        cli::cli_alert_info("Also extracted std.err directly from maq objects")
      }
      
      if (!is.null(ci_data) && nrow(ci_data) > 0) {
        cli::cli_alert_success("Confidence intervals computed")
      } else {
        cli::cli_alert_warning("No confidence interval data available")
        show_ci <- FALSE
      }
    } else {
      cli::cli_alert_warning("No qini objects found for confidence interval computation")
      show_ci <- FALSE
    }
  }
  
  # check for complete paths and extend with horizontal lines if needed
  if (horizontal_line && !is.null(qini_objects)) {
    # qini_objects already available from generation above
    extended_data <- NULL
    
    for (crv in unique(qini_data$curve)) {
      crv_data <- qini_data[qini_data$curve == crv, ]
      
      # map curve names back to qini object names
      qini_obj_name <- NULL
      if (crv == "CATE") {
        if ("cate" %in% names(qini_objects)) qini_obj_name <- "cate"
        else if ("treatment" %in% names(qini_objects)) qini_obj_name <- "treatment"
      } else if (crv == "ATE") {
        if ("ate" %in% names(qini_objects)) qini_obj_name <- "ate"
        else if ("baseline" %in% names(qini_objects)) qini_obj_name <- "baseline"
      } else {
        # for multi-arm, use direct mapping
        qini_obj_name <- names(curve_mapping)[curve_mapping == crv][1]
      }
      
      if (!is.null(qini_obj_name) && qini_obj_name %in% names(qini_objects)) {
        qini_obj <- qini_objects[[qini_obj_name]]
        
        # check if path is complete
        if (!is.null(qini_obj[["_path"]]$complete.path) && qini_obj[["_path"]]$complete.path) {
          # check if we need to extend to proportion = 1
          max_prop <- max(crv_data$proportion)
          if (max_prop < 1) {
            # get the last gain value
            last_gain <- crv_data$gain[which.max(crv_data$proportion)]
            
            # create extension points
            n_extend <- 20  # number of points for smooth line
            extend_props <- seq(max_prop, 1, length.out = n_extend)[-1]  # exclude the overlap point
            
            extend_data <- data.frame(
              proportion = extend_props,
              gain = rep(last_gain, length(extend_props)),
              curve = crv,
              stringsAsFactors = FALSE
            )
            
            # if qini_data has original_gain column (from scale transformation), add it to extend_data
            if ("original_gain" %in% names(qini_data)) {
              # get the original gain for this curve at max proportion
              original_last_gain <- crv_data$original_gain[which.max(crv_data$proportion)]
              extend_data$original_gain <- rep(original_last_gain, nrow(extend_data))
            }
            
            if (is.null(extended_data)) {
              extended_data <- extend_data
            } else {
              extended_data <- rbind(extended_data, extend_data)
            }
          }
        }
      }
    }
    
    # add extended data if any
    if (!is.null(extended_data)) {
      # ensure columns match exactly before rbind
      common_cols <- intersect(names(qini_data), names(extended_data))
      qini_data <- rbind(qini_data[, common_cols], extended_data[, common_cols])
      qini_data <- qini_data[order(qini_data$curve, qini_data$proportion), ]
      cli::cli_alert_info("Extended complete paths with horizontal lines")
    }
  }
  
  # create plot
  cli::cli_alert("Creating Qini curves plot...")
  p <- ggplot(qini_data, aes(x = proportion, y = gain, colour = curve, linetype = curve))
  
  # add confidence intervals if available
  if (compute_ci && !is.null(ci_data)) {
    if (is.null(ci_ribbon_color)) {
      # use the curve color for ribbons
      p <- p + geom_ribbon(
        data = ci_data,
        aes(x = proportion, ymin = lower, ymax = upper, fill = curve),
        alpha = ci_ribbon_alpha,
        linetype = 0,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    } else {
      # use specified color for all ribbons
      p <- p + geom_ribbon(
        data = ci_data,
        aes(x = proportion, ymin = lower, ymax = upper),
        fill = ci_ribbon_color,
        alpha = ci_ribbon_alpha,
        linetype = 0,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    }
  }
  
  # add the main curves
  p <- p + geom_line(linewidth = 0.5)

  # add vertical lines at spend levels if requested
  if (show_spend_lines && length(spend_levels) > 0) {
    # adjust spend levels for budget x-axis
    adjusted_spend_levels <- if (x_axis == "budget") {
      spend_levels * treatment_cost
    } else {
      spend_levels
    }
    
    p <- p + geom_vline(
      xintercept = adjusted_spend_levels,
      linetype = "dashed",
      color = spend_line_color,
      alpha = spend_line_alpha,
      linewidth = 0.5
    )

    # add text annotations for spend levels
    # position labels at different heights to avoid overlap
    label_y_positions <- seq(from = 1.5, to = 2.5, length.out = length(spend_levels))

    for (i in seq_along(spend_levels)) {
      p <- p + annotate(
        "text",
        x = spend_levels[i],
        y = Inf,
        label = paste0(spend_levels[i] * 100, "% spend"),
        vjust = label_y_positions[i],
        hjust = -0.1,
        size = 3,
        color = spend_line_color,
        alpha = 0.8
      )
    }
  }

  # set y-axis label based on scale
  y_label <- switch(scale,
    "average" = "Average policy effect",
    "cumulative" = "Cumulative gain",
    "population" = "Total population impact",
    "Average policy effect"  # default
  )
  
  # treatment_cost was already initialized and cost-based regeneration handled earlier
  
  # handle fixed_ylim if requested
  if (fixed_ylim && is.null(ylim)) {
    # for fixed ylim, just use the actual data range with padding
    if (!is.null(qini_data) && nrow(qini_data) > 0) {
      current_range <- range(qini_data$gain, na.rm = TRUE)
      # add some padding (5%)
      padding <- diff(current_range) * 0.05
      ylim <- c(current_range[1] - padding, current_range[2] + padding)
    }
  }
  
  # create subtitle if cost is not 1
  subtitle_text <- NULL
  if (treatment_cost != 1) {
    subtitle_text <- paste0("Treatment cost = ", treatment_cost)
  }
  
  # set x-axis label based on x_axis type
  x_label <- if (x_axis == "budget") {
    "Budget per unit (B)"
  } else {
    "Proportion of population targeted"
  }
  
  p <- p +
    labs(
      x = x_label,
      y = y_label,
      title = paste("Qini Curves for", transformed_outcome_var),
      subtitle = subtitle_text
    ) +
    # apply selected theme
    switch(theme,
      "classic" = theme_classic(),
      "minimal" = theme_minimal(),
      "bw" = theme_bw(),
      "gray" = theme_gray(),
      "light" = theme_light(),
      "dark" = theme_dark(),
      "void" = theme_void(),
      theme_classic()  # default fallback
    ) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    ) +
    # use hardcoded colors for binary treatments
    {if (is_binary) {
      list(
        scale_color_manual(values = c("CATE" = cate_color, "ATE" = ate_color)),
        scale_fill_manual(values = c("CATE" = cate_color, "ATE" = ate_color))
      )
    } else {
      list(
        ggokabeito::scale_color_okabe_ito(),
        ggokabeito::scale_fill_okabe_ito()
      )
    }} +
    guides(colour = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm"),
           linetype = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm")) +
    {if (!is.null(ylim)) coord_cartesian(ylim = ylim)}

  cli::cli_alert_success("Qini curves plot created successfully 👍")
  
  # return data if requested
  if (return_data) {
    # prepare return data frame
    plot_data <- qini_data
    
    # add CI columns if available
    if (compute_ci && !is.null(ci_data)) {
      # merge CI data with main data
      plot_data <- merge(
        plot_data, 
        ci_data[, c("proportion", "curve", "lower", "upper")],
        by = c("proportion", "curve"),
        all.x = TRUE
      )
      
      # fill in missing CI values with NA
      plot_data$lower[is.na(plot_data$lower)] <- NA
      plot_data$upper[is.na(plot_data$upper)] <- NA
    } else {
      plot_data$lower <- NA
      plot_data$upper <- NA
    }
    
    # order columns consistently with maq::plot() output
    plot_data <- plot_data[order(plot_data$curve, plot_data$proportion), 
                          c("proportion", "gain", "lower", "upper", "curve")]
    
    return(plot_data)
  }
  
  return(p)
}
