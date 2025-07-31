#' Plot QINI Curves (Simplified Version)
#'
#' @description
#' Creates a ggplot2 visualization of QINI curves computed by margot_qini().
#' Shows CATE-based targeting vs no-priority baseline.
#'
#' @param margot_result Output from margot_qini()
#' @param model_name Character string specifying which model to plot
#' @param show_ci Logical or character. Show confidence intervals: FALSE (none),
#'   TRUE/"both" (both curves), "cate" (CATE only), "baseline" (baseline only)
#' @param ci_alpha Significance level for confidence intervals (default 0.1)
#' @param colors Named vector of colors. Default: c(cate = "#1f77b4", baseline = "#ff7f0e")
#' @param spend_levels Numeric vector of spend levels to mark with vertical lines
#' @param title Character string for plot title. If NULL, auto-generated
#' @param subtitle Character string for plot subtitle. If NULL, auto-generated
#' @param theme ggplot2 theme. Default is theme_minimal()
#' @param return_data Logical. Return data frame instead of plot (default FALSE)
#'
#' @return A ggplot2 object or data frame (if return_data = TRUE)
#'
#' @export
#' @import ggplot2
#' @importFrom cli cli_alert_info cli_alert_warning
#' @importFrom maq average_gain
margot_plot_qini_simple <- function(margot_result,
                                   model_name,
                                   show_ci = FALSE,
                                   ci_alpha = 0.1,
                                   colors = c(cate = "#1f77b4", baseline = "#ff7f0e"),
                                   spend_levels = c(0.1, 0.4),
                                   title = NULL,
                                   subtitle = NULL,
                                   theme = theme_minimal(),
                                   return_data = FALSE) {
  
  # validate inputs - check in results structure
  if (!("results" %in% names(margot_result))) {
    stop("No results found in margot_result.")
  }
  
  if (!model_name %in% names(margot_result$results)) {
    stop("Model '", model_name, "' not found in results. Available models: ",
         paste(names(margot_result$results), collapse = ", "))
  }
  
  # check if qini has been computed
  if (is.null(margot_result$results[[model_name]]$qini_objects)) {
    stop("No qini_objects found for model '", model_name, "'. Run margot_qini() first.")
  }
  
  # extract qini results for this model
  qini_model <- margot_result$results[[model_name]]
  
  # extract data from maq objects
  cate_obj <- qini_model$qini_objects$cate
  baseline_obj <- qini_model$qini_objects$ate
  
  # create data frame for plotting
  # handle potentially different path lengths
  cate_df <- data.frame(
    spend = cate_obj[["_path"]]$spend,
    gain = cate_obj[["_path"]]$gain,
    curve = "CATE"
  )
  
  baseline_df <- data.frame(
    spend = baseline_obj[["_path"]]$spend,
    gain = baseline_obj[["_path"]]$gain,
    curve = "Baseline"
  )
  
  plot_data <- rbind(cate_df, baseline_df)
  
  # add confidence intervals if requested
  if (show_ci != FALSE) {
    # determine which curves to compute CI for
    compute_ci_for <- if (show_ci == TRUE || show_ci == "both") {
      c("CATE", "Baseline")
    } else if (show_ci == "cate") {
      "CATE"
    } else if (show_ci == "baseline") {
      "Baseline"
    } else {
      character(0)
    }
    
    # compute confidence intervals at selected spend levels
    ci_spend_levels <- seq(0, 1, length.out = 20)
    
    if ("CATE" %in% compute_ci_for) {
      ci_cate <- sapply(ci_spend_levels, function(s) {
        ag <- maq::average_gain(cate_obj, spend = s)
        c(lower = ag["estimate"] - qnorm(1 - ci_alpha/2) * ag["std.err"],
          upper = ag["estimate"] + qnorm(1 - ci_alpha/2) * ag["std.err"])
      })
      
      ci_data_cate <- data.frame(
        spend = ci_spend_levels,
        lower = ci_cate[1, ],  # first row is lower
        upper = ci_cate[2, ],  # second row is upper
        curve = "CATE"
      )
      plot_data <- merge(plot_data, ci_data_cate, 
                        by = c("spend", "curve"), all.x = TRUE)
    }
    
    if ("Baseline" %in% compute_ci_for) {
      ci_baseline <- sapply(ci_spend_levels, function(s) {
        ag <- maq::average_gain(baseline_obj, spend = s)
        c(lower = ag["estimate"] - qnorm(1 - ci_alpha/2) * ag["std.err"],
          upper = ag["estimate"] + qnorm(1 - ci_alpha/2) * ag["std.err"])
      })
      
      ci_data_baseline <- data.frame(
        spend = ci_spend_levels,
        lower = ci_baseline[1, ],  # first row is lower
        upper = ci_baseline[2, ],  # second row is upper
        curve = "Baseline"
      )
      
      if ("lower" %in% names(plot_data)) {
        # merge with existing CI data
        ci_baseline_rows <- plot_data$curve == "Baseline"
        plot_data$lower[ci_baseline_rows] <- approx(
          ci_data_baseline$spend, 
          ci_data_baseline$lower, 
          plot_data$spend[ci_baseline_rows]
        )$y
        plot_data$upper[ci_baseline_rows] <- approx(
          ci_data_baseline$spend, 
          ci_data_baseline$upper, 
          plot_data$spend[ci_baseline_rows]
        )$y
      } else {
        plot_data <- merge(plot_data, ci_data_baseline, 
                          by = c("spend", "curve"), all.x = TRUE)
      }
    }
  }
  
  # return data if requested
  if (return_data) {
    return(plot_data)
  }
  
  # create plot
  p <- ggplot(plot_data, aes(x = spend, y = gain, color = curve)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c(CATE = colors["cate"], Baseline = colors["baseline"]),
      labels = c(CATE = "CATE-based targeting", 
                 Baseline = "No-priority baseline")
    ) +
    labs(
      x = "Proportion treated",
      y = "Average gain per unit",
      color = "Strategy",
      title = title %||% paste("QINI Curve:", gsub("^model_", "", model_name)),
      subtitle = subtitle %||% paste("Test set n =", qini_model$qini_metadata$n_test)
    ) +
    theme +
    theme(legend.position = "bottom")
  
  # add confidence intervals if computed
  if (show_ci != FALSE && "lower" %in% names(plot_data)) {
    p <- p + geom_ribbon(
      aes(ymin = lower, ymax = upper, fill = curve),
      alpha = 0.2,
      color = NA
    ) +
    scale_fill_manual(
      values = c(CATE = colors["cate"], Baseline = colors["baseline"]),
      guide = "none"
    )
  }
  
  # add spend level lines
  if (length(spend_levels) > 0) {
    p <- p + geom_vline(
      xintercept = spend_levels,
      linetype = "dashed",
      alpha = 0.5,
      color = "gray40"
    )
    
    # add labels for spend levels
    y_pos <- min(plot_data$gain) + 0.9 * diff(range(plot_data$gain))
    for (level in spend_levels) {
      p <- p + annotate(
        "text",
        x = level,
        y = y_pos,
        label = paste0(level * 100, "%"),
        vjust = -0.5,
        hjust = 0.5,
        size = 3,
        color = "gray40"
      )
    }
  }
  
  return(p)
}

# helper function for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x