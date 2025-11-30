#' Plot Cross-Validation Heterogeneity Test Results
#'
#' Creates a forest plot visualization of cross-validation heterogeneity test results
#' from margot_rate_cv(). Shows t-statistics, p-values, and significance for each model.
#'
#' @param cv_results A margot_cv_results object from margot_rate_cv()
#' @param title Character string for the plot title. If NULL (default), a title is
#'   generated based on the CV method details.
#' @param subtitle Character string for the plot subtitle. Default shows the adjustment method.
#' @param x_lab Character string for the x-axis label. Default is "t-statistic".
#' @param y_lab Character string for the y-axis label. Default is "Model".
#' @param remove_model_prefix Logical; remove "model_" prefix from model names. Default TRUE.
#' @param label_mapping Optional named list for custom label mappings. Keys should be model names
#'   (with or without "model_" prefix), and values should be the desired display labels.
#' @param show_p_values Logical; show p-values on the plot. Default TRUE.
#' @param significance_color Color for significant results. Default "#4CAF50" (green).
#' @param non_significance_color Color for non-significant results. Default "#9E9E9E" (gray).
#' @param null_line_color Color for the null hypothesis line. Default "#FF5252" (red).
#' @param point_size Size of the points. Default 3.
#' @param text_size Size of p-value text. Default 3.
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @examples
#' \dontrun{
#' # Run cross-validation heterogeneity test
#' cv_results <- margot_rate_cv(
#'   model_results = cf_results,
#'   num_folds = 5,
#'   target = "AUTOC",
#'   alpha = 0.2, # recommended for Bonferroni
#'   adjust = "bonferroni"
#' )
#'
#' # Create forest plot
#' p <- margot_plot_cv_results(cv_results)
#' print(p)
#'
#' # Create summary plot
#' p_summary <- margot_plot_cv_summary(cv_results)
#' print(p_summary)
#'
#' # With custom labels
#' label_mapping <- list(
#'   "model_happiness" = "Happiness",
#'   "model_depression" = "Depression"
#' )
#' p <- margot_plot_cv_results(cv_results, label_mapping = label_mapping)
#'
#' # Note: If you try to use margot_plot_rate() with CV results, you'll get an error:
#' # margot_plot_rate(cv_results$cv_results)  # Error - use margot_plot_cv_results() instead
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_text labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual coord_flip annotate
#' @importFrom ggplot2 geom_bar scale_fill_identity ylim
#' @importFrom dplyr mutate arrange case_when
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom stats qnorm
#'
#' @export
margot_plot_cv_results <- function(cv_results,
                                   title = NULL,
                                   subtitle = NULL,
                                   x_lab = "t-statistic",
                                   y_lab = "Model",
                                   remove_model_prefix = TRUE,
                                   label_mapping = NULL,
                                   show_p_values = TRUE,
                                   significance_color = "#4CAF50",
                                   non_significance_color = "#9E9E9E",
                                   null_line_color = "#FF5252",
                                   point_size = 3,
                                   text_size = 3) {
  # validate input
  if (!inherits(cv_results, "margot_cv_results")) {
    stop("Input must be a margot_cv_results object from margot_rate_cv()")
  }

  cli::cli_alert_info("Creating forest plot for CV heterogeneity test results")

  # extract data
  plot_data <- cv_results$cv_results
  method_details <- cv_results$method_details

  # transform model names
  plot_data <- plot_data %>%
    dplyr::mutate(
      model_display = dplyr::case_when(
        remove_model_prefix ~ gsub("^model_", "", model),
        TRUE ~ model
      )
    )

  # apply custom label mapping if provided
  if (!is.null(label_mapping)) {
    for (old_name in names(label_mapping)) {
      # try matching with and without model_ prefix
      matches <- plot_data$model == old_name |
        plot_data$model == paste0("model_", old_name) |
        plot_data$model_display == old_name

      if (any(matches)) {
        plot_data$model_display[matches] <- label_mapping[[old_name]]
      }
    }
  }

  # format p-values for display
  plot_data <- plot_data %>%
    dplyr::mutate(
      p_label = dplyr::case_when(
        p_value < 0.001 ~ "p < 0.001",
        p_value < 0.01 ~ sprintf("p = %.3f", p_value),
        p_value < 0.05 ~ sprintf("p = %.3f", p_value),
        TRUE ~ sprintf("p = %.2f", p_value)
      ),
      color = ifelse(significant, significance_color, non_significance_color)
    ) %>%
    dplyr::arrange(t_statistic) # arrange by t-statistic for better visualization

  # create title if not provided
  if (is.null(title)) {
    target_text <- if (length(method_details$target) > 1) {
      "AUTOC & QINI"
    } else {
      method_details$target
    }
    title <- sprintf(
      "Cross-Validation Heterogeneity Test (%s, %d folds)",
      target_text,
      method_details$num_folds
    )
  }

  # create subtitle if not provided
  if (is.null(subtitle)) {
    adjust_text <- if (method_details$adjust == "none") {
      "No multiple testing correction"
    } else {
      sprintf(
        "%s correction (alpha = %.2f)",
        tools::toTitleCase(method_details$adjust),
        method_details$alpha
      )
    }
    subtitle <- adjust_text
  }

  # check if we have multiple targets
  has_multiple_targets <- length(unique(plot_data$target)) > 1

  # create the plot
  if (has_multiple_targets) {
    # for multiple targets, create a combined label
    plot_data <- plot_data %>%
      dplyr::mutate(
        model_target = paste0(model_display, " (", target, ")")
      )

    p <- ggplot(plot_data, aes(x = t_statistic, y = reorder(model_target, t_statistic))) +
      # add vertical line at 0 (null hypothesis)
      geom_vline(xintercept = 0, linetype = "dashed", color = null_line_color, size = 0.8) +
      # add points
      geom_point(aes(color = color), size = point_size) +
      # add p-values if requested
      {
        if (show_p_values) {
          geom_text(aes(label = p_label, hjust = ifelse(t_statistic > 0, -0.1, 1.1)),
            size = text_size, color = "black"
          )
        }
      } +
      # customize colors
      scale_color_identity() +
      # labels
      labs(
        title = title,
        subtitle = subtitle,
        x = x_lab,
        y = y_lab
      ) +
      # theme
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  } else {
    # single target - original plot
    p <- ggplot(plot_data, aes(x = t_statistic, y = reorder(model_display, t_statistic))) +
      # add vertical line at 0 (null hypothesis)
      geom_vline(xintercept = 0, linetype = "dashed", color = null_line_color, size = 0.8) +
      # add points
      geom_point(aes(color = color), size = point_size) +
      # add p-values if requested
      {
        if (show_p_values) {
          geom_text(aes(label = p_label, hjust = ifelse(t_statistic > 0, -0.1, 1.1)),
            size = text_size, color = "black"
          )
        }
      } +
      # customize colors
      scale_color_identity() +
      # labels
      labs(
        title = title,
        subtitle = subtitle,
        x = x_lab,
        y = y_lab
      ) +
      # theme
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  }

  # add significance region shading (optional)
  # critical value for two-tailed test
  if (method_details$adjust == "none") {
    crit_val <- qnorm(1 - method_details$alpha / 2)
  } else if (method_details$adjust == "bonferroni") {
    n_tests <- nrow(plot_data)
    crit_val <- qnorm(1 - method_details$alpha / (2 * n_tests))
  }

  # add shaded regions for significance
  if (exists("crit_val")) {
    p <- p +
      annotate("rect",
        xmin = crit_val, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        alpha = 0.1, fill = significance_color
      ) +
      annotate("rect",
        xmin = -Inf, xmax = -crit_val,
        ymin = -Inf, ymax = Inf,
        alpha = 0.1, fill = significance_color
      )
  }

  cli::cli_alert_success("CV results forest plot created successfully")
  return(p)
}

#' Create summary plot for cross-validation heterogeneity results
#'
#' Creates a summary bar plot showing the proportion of significant models
#' and their distribution across positive/negative effects.
#'
#' @param cv_results A margot_cv_results object from margot_rate_cv()
#' @param title Character string for the plot title. Default generates based on results.
#' @param positive_color Color for positive effects. Default "#4CAF50" (green).
#' @param negative_color Color for negative effects. Default "#F44336" (red).
#' @param neutral_color Color for non-significant effects. Default "#9E9E9E" (gray).
#'
#' @return A ggplot object
#' @export
margot_plot_cv_summary <- function(cv_results,
                                   title = NULL,
                                   positive_color = "#4CAF50",
                                   negative_color = "#F44336",
                                   neutral_color = "#9E9E9E") {
  if (!inherits(cv_results, "margot_cv_results")) {
    stop("Input must be a margot_cv_results object from margot_rate_cv()")
  }

  # extract data
  data <- cv_results$cv_results

  # categorize results
  summary_data <- data.frame(
    category = c("Positive\nHeterogeneity", "No\nHeterogeneity", "Negative\nHeterogeneity"),
    count = c(
      sum(data$significant & data$t_statistic > 0),
      sum(!data$significant),
      sum(data$significant & data$t_statistic < 0)
    ),
    color = c(positive_color, neutral_color, negative_color)
  )

  # calculate percentages
  summary_data$percentage <- round(100 * summary_data$count / sum(summary_data$count), 1)

  # create title if not provided
  if (is.null(title)) {
    n_sig <- sum(data$significant)
    n_total <- nrow(data)
    title <- sprintf("CV Heterogeneity Test Summary: %d/%d Significant", n_sig, n_total)
  }

  # create the plot
  ggplot(summary_data, aes(x = category, y = count, fill = color)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(count, "\n(", percentage, "%)")),
      vjust = -0.5, size = 4
    ) +
    scale_fill_identity() +
    labs(
      title = title,
      x = "",
      y = "Number of Models"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(size = 12),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(summary_data$count) * 1.2) # add space for labels
}
