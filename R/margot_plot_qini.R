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
#' @param spend_levels Numeric vector of spend levels to show with vertical lines. Default is c(0.2, 0.5).
#' @param show_spend_lines Logical indicating whether to show vertical lines at spend levels. Default is TRUE.
#' @param spend_line_color Color for spend level lines. Default is "red".
#' @param spend_line_alpha Alpha transparency for spend lines. Default is 0.5.
#' @param theme Character string specifying the ggplot2 theme. Default is "classic". Options include "classic", "minimal", "bw", "gray", "light", "dark", "void".
#' @param show_ci Logical indicating whether to show confidence intervals. Default is FALSE.
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
#'   \item Binary treatment colors: CATE = green (#009E73), ATE = gold (#d8a739)
#' }
#'
#' @import ggplot2
#' @import cli
#' @importFrom ggokabeito scale_color_okabe_ito scale_fill_okabe_ito
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#'
#' @examples
#' \dontrun{
#' # Assuming mc.test is the result of margot_multi_arm_causal_forest()
#' plot_qini_curves(mc.test, "model_t2_belong_z")
#'
#' # Using custom label mapping
#' label_mapping <- list(
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
#' )
#' plot_qini_curves(mc.test, "model_t2_env_not_env_efficacy_z", label_mapping = label_mapping)
#' }
#'
#' @export
margot_plot_qini <- function(mc_result, outcome_var,
                             label_mapping = NULL,
                             spend_levels = c(0.2, 0.5),
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
                             ylim = NULL) {
  cli::cli_h1("Margot Plot Qini Curves")

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

  qini_data <- mc_result$results[[outcome_var]]$qini_data
  if (is.null(qini_data)) {
    cli::cli_abort("Qini data not found for the specified outcome variable: {outcome_var}")
  }

  cli::cli_alert_success("Qini data extracted for outcome variable: {outcome_var}")
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

  # compute confidence intervals if requested
  ci_data <- NULL
  if (show_ci) {
    cli::cli_alert_info("Computing confidence intervals...")
    
    # extract qini objects from the model results
    qini_objects <- mc_result$results[[outcome_var]]$qini_objects
    
    if (!is.null(qini_objects)) {
      # create spend sequence for CI computation
      # include small buffer at extremes to avoid numerical issues
      spend_seq <- seq(0.01, 0.99, length.out = ci_n_points)
      
      # function to compute CIs for a single qini object
      compute_curve_ci <- function(qini_obj, curve_name) {
        # special handling for simplified ATE baseline
        if (!is.null(qini_obj$mean_tau) && curve_name %in% c("ATE", "ate")) {
          # for straight line ATE, CI is 0 at all points (no variability in constant allocation)
          return(data.frame(
            proportion = spend_seq,
            estimate = spend_seq * qini_obj$mean_tau,
            std_err = 0,
            lower = spend_seq * qini_obj$mean_tau,
            upper = spend_seq * qini_obj$mean_tau,
            curve = curve_name
          ))
        }
        
        ci_list <- lapply(spend_seq, function(s) {
          tryCatch({
            avg_gain_result <- maq::average_gain(qini_obj, spend = s)
            
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
          do.call(rbind, ci_results)
        } else {
          NULL
        }
      }
      
      # compute CIs for each curve
      ci_list <- list()
      
      # for binary treatments - check both old and new naming conventions
      if (is_binary) {
        # check each curve individually
        if ("cate" %in% names(qini_objects)) {
          result <- compute_curve_ci(qini_objects$cate, "CATE")
          if (!is.null(result)) ci_list[["CATE"]] <- result
        } else if ("treatment" %in% names(qini_objects)) {
          result <- compute_curve_ci(qini_objects$treatment, "CATE")
          if (!is.null(result)) ci_list[["CATE"]] <- result
        }
        
        if ("ate" %in% names(qini_objects)) {
          result <- compute_curve_ci(qini_objects$ate, "ATE")
          if (!is.null(result)) ci_list[["ATE"]] <- result
        } else if ("baseline" %in% names(qini_objects)) {
          result <- compute_curve_ci(qini_objects$baseline, "ATE")
          if (!is.null(result)) ci_list[["ATE"]] <- result
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
      
      ci_data <- do.call(rbind, ci_list[!sapply(ci_list, is.null)])
      
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
        for (crv in curves) {
          crv_data <- ci_data[ci_data$curve == crv, ]
          # add 0 if missing
          if (nrow(crv_data) > 0 && !any(crv_data$proportion == 0)) {
            ci_data <- rbind(ci_data, data.frame(
              proportion = 0,
              estimate = 0,
              lower = 0,
              upper = 0,
              curve = crv
            ))
          }
          # add 1 if missing (use last available values)
          if (nrow(crv_data) > 0 && !any(crv_data$proportion == 1)) {
            last_row <- crv_data[which.max(crv_data$proportion), ]
            ci_data <- rbind(ci_data, data.frame(
              proportion = 1,
              estimate = last_row$estimate * (1 / last_row$proportion),  # extrapolate
              lower = last_row$lower * (1 / last_row$proportion),
              upper = last_row$upper * (1 / last_row$proportion),
              curve = crv
            ))
          }
        }
        # sort by curve and proportion
        ci_data <- ci_data[order(ci_data$curve, ci_data$proportion), ]
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
  if (horizontal_line && !is.null(mc_result$results[[outcome_var]]$qini_objects)) {
    qini_objects <- mc_result$results[[outcome_var]]$qini_objects
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
        # for simplified ATE baseline, the path is always complete
        is_complete <- FALSE
        if (!is.null(qini_obj$mean_tau) && crv == "ATE") {
          is_complete <- TRUE  # ATE baseline is always complete
        } else if (!is.null(qini_obj[["_path"]]$complete.path)) {
          is_complete <- qini_obj[["_path"]]$complete.path
        }
        
        if (is_complete) {
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
              curve = crv
            )
            
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
      qini_data <- rbind(qini_data, extended_data)
      qini_data <- qini_data[order(qini_data$curve, qini_data$proportion), ]
      cli::cli_alert_info("Extended complete paths with horizontal lines")
    }
  }
  
  # create plot
  cli::cli_alert("Creating Qini curves plot...")
  p <- ggplot(qini_data, aes(x = proportion, y = gain, colour = curve, linetype = curve))
  
  # add confidence intervals if available
  if (show_ci && !is.null(ci_data)) {
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
    p <- p + geom_vline(
      xintercept = spend_levels,
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

  p <- p +
    labs(
      x = "Proportion of population targeted",
      y = "Cumulative gain",
      title = paste("Qini Curves for", transformed_outcome_var)
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
        scale_color_manual(values = c("CATE" = "#009E73", "ATE" = "#d8a739")),
        scale_fill_manual(values = c("CATE" = "#009E73", "ATE" = "#d8a739"))
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
    if (show_ci && !is.null(ci_data)) {
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
