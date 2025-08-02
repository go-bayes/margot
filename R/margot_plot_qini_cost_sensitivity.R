#' Plot QINI Curves Across Treatment Cost Scenarios
#'
#' @description
#' Visualizes how QINI curves change with different treatment costs, showing
#' the impact of budget constraints on optimal treatment allocation.
#'
#' @param cost_sensitivity_result Output from margot_qini_cost_sensitivity()
#' @param model_name Character string specifying which model to plot
#' @param plot_type Character; either "overlay" (all costs on one plot) or
#'   "facet" (separate panel for each cost). Default is "overlay".
#' @param show_baseline Logical; whether to show baseline curves. Default is TRUE.
#' @param colors Optional vector of colors for different cost scenarios.
#'   If NULL, uses a gradient from blue (low cost) to red (high cost).
#' @param title Optional plot title. If NULL, auto-generated based on model name.
#' @param subtitle Optional plot subtitle. If NULL, describes the cost scenarios.
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot object
#'
#' @details
#' This function creates visualizations showing how QINI curves change with
#' treatment cost. Lower costs result in steeper curves (more people can be
#' treated cost-effectively), while higher costs result in shallower curves
#' (only highest-effect individuals justify treatment).
#'
#' The "overlay" plot type shows all cost scenarios on one plot with different
#' colors, making it easy to compare curve shapes. The "facet" plot type creates
#' separate panels for each cost, useful when curves overlap significantly.
#'
#' @examples
#' \dontrun{
#' # Run cost sensitivity analysis
#' cost_sens <- margot_qini_cost_sensitivity(
#'   causal_forest_results,
#'   costs = c(0.2, 0.5, 1, 2, 5)
#' )
#'
#' # Overlay plot (default)
#' margot_plot_qini_cost_sensitivity(cost_sens, "model_anxiety")
#'
#' # Faceted plot
#' margot_plot_qini_cost_sensitivity(
#'   cost_sens,
#'   "model_anxiety",
#'   plot_type = "facet"
#' )
#'
#' # Custom styling
#' margot_plot_qini_cost_sensitivity(
#'   cost_sens,
#'   "model_anxiety",
#'   colors = c("darkgreen", "gold", "orange", "red", "darkred"),
#'   title = "Treatment Cost Impact on Anxiety Intervention",
#'   subtitle = "Lower costs enable treating more patients"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon facet_wrap
#' @importFrom dplyr bind_rows mutate
#' @importFrom cli cli_alert_info cli_alert_warning
margot_plot_qini_cost_sensitivity <- function(cost_sensitivity_result,
                                              model_name,
                                              plot_type = c("overlay", "facet"),
                                              show_baseline = TRUE,
                                              colors = NULL,
                                              title = NULL,
                                              subtitle = NULL,
                                              ...) {
  plot_type <- match.arg(plot_type)

  # validate inputs
  if (!inherits(cost_sensitivity_result, "margot_qini_cost_sensitivity")) {
    stop("cost_sensitivity_result must be output from margot_qini_cost_sensitivity()")
  }

  if (!model_name %in% cost_sensitivity_result$models_processed) {
    stop(paste(
      "Model", model_name, "not found. Available models:",
      paste(cost_sensitivity_result$models_processed, collapse = ", ")
    ))
  }

  # extract qini data for each cost scenario
  plot_data_list <- list()
  costs <- cost_sensitivity_result$costs

  for (i in seq_along(costs)) {
    cost <- costs[i]
    cost_label <- names(cost_sensitivity_result$results)[i]

    qini_data <- cost_sensitivity_result$results[[cost_label]][[model_name]]$qini_data

    if (!is.null(qini_data)) {
      # add cost information
      qini_data$cost <- cost
      qini_data$cost_label <- paste0("Cost = ", cost)
      plot_data_list[[i]] <- qini_data
    }
  }

  if (length(plot_data_list) == 0) {
    stop("No QINI data available for model ", model_name)
  }

  # combine all data
  plot_data <- dplyr::bind_rows(plot_data_list)

  # filter to CATE curves only if not showing baseline
  if (!show_baseline) {
    plot_data <- plot_data[plot_data$curve == "cate", ]
  }

  # create color palette if not provided
  if (is.null(colors)) {
    # gradient from blue (low cost) to red (high cost)
    n_costs <- length(unique(plot_data$cost))
    colors <- colorRampPalette(c(
      "#2166AC", "#4393C3", "#92C5DE",
      "#F4A582", "#D6604D", "#B2182B"
    ))(n_costs)
  }

  # extract clean model name for title
  model_display <- gsub("^model_", "", model_name)

  # create title and subtitle if not provided
  if (is.null(title)) {
    title <- paste("QINI Curves Across Treatment Costs:", model_display)
  }

  if (is.null(subtitle)) {
    subtitle <- paste("Costs evaluated:", paste(costs, collapse = ", "))
  }

  # create base plot
  if (plot_type == "overlay") {
    # all costs on one plot
    p <- ggplot(plot_data, aes(x = proportion, y = gain)) +
      theme_minimal() +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Proportion of Population Treated",
        y = "Gain"
      )

    if (show_baseline) {
      # plot with both curve types
      p <- p +
        geom_line(aes(color = cost_label, linetype = curve), size = 1.2) +
        scale_linetype_manual(
          values = c("cate" = "solid", "ate" = "dashed"),
          labels = c("cate" = "CATE", "ate" = "Baseline")
        ) +
        scale_color_manual(values = colors, name = "Treatment Cost")
    } else {
      # CATE curves only
      p <- p +
        geom_line(aes(color = cost_label), size = 1.2) +
        scale_color_manual(values = colors, name = "Treatment Cost")
    }
  } else {
    # faceted plot
    p <- ggplot(plot_data, aes(x = proportion, y = gain)) +
      theme_minimal() +
      facet_wrap(~cost_label, scales = "free_y") +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Proportion of Population Treated",
        y = "Gain"
      )

    if (show_baseline) {
      p <- p +
        geom_line(aes(linetype = curve, color = curve), size = 1.2) +
        scale_linetype_manual(
          values = c("cate" = "solid", "ate" = "dashed"),
          labels = c("cate" = "CATE", "ate" = "Baseline")
        ) +
        scale_color_manual(
          values = c("cate" = "#2166AC", "ate" = "#B2182B"),
          labels = c("cate" = "CATE", "ate" = "Baseline")
        )
    } else {
      p <- p +
        geom_line(color = "#2166AC", size = 1.2)
    }
  }

  # add common theme elements
  p <- p +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    )

  # apply any additional arguments
  if (length(list(...)) > 0) {
    p <- p + ...
  }

  cli::cli_alert_info("Created cost sensitivity plot for {model_name}")

  return(p)
}


#' Create Summary Plot of Optimal Treatment Fractions Across Costs
#'
#' @description
#' Creates a line plot showing how the optimal treatment fraction changes
#' with treatment cost for one or more models.
#'
#' @param cost_sensitivity_result Output from margot_qini_cost_sensitivity()
#' @param model_names Optional character vector of models to include.
#'   Default NULL includes all models.
#' @param spend_level Numeric; which spend level to plot. Default 0.1.
#' @param metric Character; which metric to plot: "comparison_gain" (default),
#'   "difference_gain", or "reference_gain".
#' @param ... Additional arguments passed to ggplot2
#'
#' @return A ggplot object
#'
#' @export
margot_plot_qini_cost_summary <- function(cost_sensitivity_result,
                                          model_names = NULL,
                                          spend_level = 0.1,
                                          metric = "comparison_gain",
                                          ...) {
  if (!inherits(cost_sensitivity_result, "margot_qini_cost_sensitivity")) {
    stop("cost_sensitivity_result must be output from margot_qini_cost_sensitivity()")
  }

  # filter summary data
  summary_df <- cost_sensitivity_result$summary

  if (nrow(summary_df) == 0) {
    stop("No summary data available")
  }

  # filter by spend level
  summary_df <- summary_df[summary_df$spend_level == spend_level, ]

  if (nrow(summary_df) == 0) {
    stop(paste("No data for spend_level", spend_level))
  }

  # filter by model names if specified
  if (!is.null(model_names)) {
    # add model_ prefix if needed
    model_names <- ifelse(grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    summary_df <- summary_df[summary_df$model %in% model_names, ]
  }

  # validate metric
  if (!metric %in% names(summary_df)) {
    stop(paste(
      "Metric", metric, "not found. Available metrics:",
      paste(setdiff(names(summary_df), c("model", "cost", "spend_level")),
        collapse = ", "
      )
    ))
  }

  # clean model names for display
  summary_df$model_display <- gsub("^model_", "", summary_df$model)

  # create plot
  p <- ggplot(summary_df, aes_string(x = "cost", y = metric, color = "model_display")) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
      title = paste("Treatment Gain vs Cost at", spend_level * 100, "% Spend Level"),
      x = "Treatment Cost",
      y = switch(metric,
        "comparison_gain" = "CATE-based Gain",
        "difference_gain" = "Gain Difference (CATE - Baseline)",
        "reference_gain" = "Baseline Gain",
        metric
      ),
      color = "Model"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )

  # apply additional arguments
  if (length(list(...)) > 0) {
    p <- p + ...
  }

  return(p)
}
