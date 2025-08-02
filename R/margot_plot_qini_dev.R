#' Development Version of QINI Curve Plotting
#'
#' Creates QINI curve plots from margot_qini_dev() results. Can handle single
#' or multiple outcomes/models in one plot without needing a separate batch function.
#'
#' @param qini_results Results from margot_qini_dev() or a named list of such results
#' @param outcomes Character vector of outcomes to plot. NULL = all.
#' @param models Character vector of model names when qini_results is a list. NULL = all.
#' @param scale Character. Scale for gains: "average" (default), "cumulative", or "population"
#' @param show_confidence Logical. Show confidence intervals (default: TRUE)
#' @param show_baseline Logical. Show baseline curve (default: TRUE)
#' @param spend_markers Numeric vector. Budget levels to mark with vertical lines
#' @param colors Character vector of colors for curves. Auto-generated if NULL.
#' @param theme_fn ggplot2 theme function (default: theme_minimal())
#' @param title Character. Plot title. Auto-generated if NULL.
#' @param subtitle Character. Plot subtitle. Auto-generated if NULL.
#' @param facet_outcomes Logical. Create facets by outcome when multiple (default: TRUE)
#' @param facet_scales Character. Facet scales: "fixed", "free", "free_x", "free_y"
#' @param label_mapping Named list for variable label translation
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' This function replaces both margot_plot_qini() and margot_plot_qini_batch()
#' by intelligently handling both single and multiple model inputs.
#'
#' Scale options:
#' - "average": Average gain per unit treated (maq default)
#' - "cumulative": Cumulative gain (gain * proportion)
#' - "population": Total population gain (gain * proportion * n)
#'
#' When multiple models are provided (via a named list), the function can either:
#' - Create separate facets for each outcome (facet_outcomes = TRUE)
#' - Overlay all curves on a single plot (facet_outcomes = FALSE)
#'
#' @examples
#' \dontrun{
#' # Single model plot
#' test_data <- margot_simulate_test_data()
#' cf_results <- margot_causal_forest_dev(test_data$data, c("Y1", "Y2"), "A")
#' qini_results <- margot_qini_dev(cf_results)
#'
#' plot1 <- margot_plot_qini_dev(qini_results)
#'
#' # Multiple models comparison
#' cf_results2 <- margot_causal_forest_dev(test_data$data, c("Y3", "Y4"), "A")
#' qini_results2 <- margot_qini_dev(cf_results2)
#'
#' plot2 <- margot_plot_qini_dev(
#'   list(model1 = qini_results, model2 = qini_results2),
#'   scale = "cumulative",
#'   spend_markers = c(0.2, 0.5)
#' )
#' }
#'
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline
#'   scale_color_manual scale_fill_manual labs theme_minimal
#'   facet_wrap facet_grid theme element_text
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
margot_plot_qini_dev <- function(
    qini_results,
    outcomes = NULL,
    models = NULL,
    scale = "average",
    show_confidence = TRUE,
    show_baseline = TRUE,
    spend_markers = NULL,
    colors = NULL,
    theme_fn = ggplot2::theme_minimal,
    title = NULL,
    subtitle = NULL,
    facet_outcomes = TRUE,
    facet_scales = "fixed",
    label_mapping = NULL,
    verbose = TRUE) {
  if (verbose) cli::cli_alert_info("Starting margot_plot_qini_dev()")

  # determine input type
  if (inherits(qini_results, "margot_qini_dev")) {
    # single model - wrap in list
    qini_list <- list(model = qini_results)
    single_model <- TRUE
  } else if (is.list(qini_results)) {
    # check if it's a list of margot_qini_dev objects
    is_qini_list <- all(sapply(qini_results, function(x) {
      inherits(x, "margot_qini_dev")
    }))

    if (is_qini_list) {
      qini_list <- qini_results
      single_model <- FALSE

      # ensure names
      if (is.null(names(qini_list))) {
        names(qini_list) <- paste0("model", seq_along(qini_list))
      }
    } else {
      stop("qini_results must be a margot_qini_dev object or list of such objects")
    }
  } else {
    stop("qini_results must be a margot_qini_dev object or list of such objects")
  }

  # filter models if specified
  if (!is.null(models)) {
    missing_models <- setdiff(models, names(qini_list))
    if (length(missing_models) > 0) {
      stop("Models not found: ", paste(missing_models, collapse = ", "))
    }
    qini_list <- qini_list[models]
  }

  # extract and combine curve data
  all_curves <- list()

  for (model_name in names(qini_list)) {
    qini_obj <- qini_list[[model_name]]

    if (is.null(qini_obj$qini_curves)) {
      cli::cli_alert_warning("No curve data for model: {model_name}")
      next
    }

    # add model identifier
    curves <- qini_obj$qini_curves
    curves$model <- model_name

    # filter outcomes if specified
    if (!is.null(outcomes)) {
      curves <- curves[curves$outcome %in% outcomes, ]
    }

    # filter baseline if not showing
    if (!show_baseline) {
      curves <- curves[curves$curve != "baseline", ]
    }

    all_curves[[model_name]] <- curves
  }

  if (length(all_curves) == 0) {
    stop("No curve data to plot")
  }

  # combine all curves
  plot_data <- do.call(rbind, all_curves)
  rownames(plot_data) <- NULL

  # apply scale transformation
  n_units <- max(sapply(qini_list, function(x) x$metadata$n_test))

  if (scale == "cumulative") {
    plot_data$gain <- plot_data$gain * plot_data$proportion
    if (!is.na(plot_data$ci_lower[1])) {
      plot_data$ci_lower <- plot_data$ci_lower * plot_data$proportion
      plot_data$ci_upper <- plot_data$ci_upper * plot_data$proportion
    }
    y_label <- "Cumulative Gain"
  } else if (scale == "population") {
    plot_data$gain <- plot_data$gain * plot_data$proportion * n_units
    if (!is.na(plot_data$ci_lower[1])) {
      plot_data$ci_lower <- plot_data$ci_lower * plot_data$proportion * n_units
      plot_data$ci_upper <- plot_data$ci_upper * plot_data$proportion * n_units
    }
    y_label <- "Population Gain"
  } else {
    y_label <- "Average Gain per Unit"
  }

  # apply label mapping to outcomes
  if (!is.null(label_mapping)) {
    plot_data$outcome_label <- plot_data$outcome
    for (old_name in names(label_mapping)) {
      plot_data$outcome_label[plot_data$outcome == old_name] <- label_mapping[[old_name]]
    }
  } else {
    plot_data$outcome_label <- plot_data$outcome
  }

  # create curve labels
  if (single_model) {
    plot_data$curve_label <- plot_data$curve
  } else {
    plot_data$curve_label <- paste(plot_data$model, plot_data$curve, sep = "_")
  }

  # determine unique curves for coloring
  unique_curves <- unique(plot_data$curve_label)
  n_curves <- length(unique_curves)

  # set colors
  if (is.null(colors)) {
    if (n_curves <= 3) {
      colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")
    } else {
      colors <- scales::hue_pal()(n_curves)
    }
  }

  if (length(colors) < n_curves) {
    colors <- rep(colors, length.out = n_curves)
  }

  names(colors) <- unique_curves

  # create base plot
  p <- ggplot(plot_data, aes(
    x = proportion, y = gain,
    color = curve_label, fill = curve_label
  ))

  # add confidence intervals if requested
  if (show_confidence && !all(is.na(plot_data$ci_lower))) {
    p <- p + geom_ribbon(
      aes(ymin = ci_lower, ymax = ci_upper),
      alpha = 0.2,
      color = NA
    )
  }

  # add lines
  p <- p + geom_line(linewidth = 1.2)

  # add spend markers
  if (!is.null(spend_markers)) {
    p <- p + geom_vline(
      xintercept = spend_markers,
      linetype = "dashed",
      alpha = 0.5
    )
  }

  # faceting
  if (length(unique(plot_data$outcome)) > 1 && facet_outcomes) {
    if (single_model || length(unique(plot_data$model)) == 1) {
      p <- p + facet_wrap(~outcome_label, scales = facet_scales)
    } else {
      p <- p + facet_grid(model ~ outcome_label, scales = facet_scales)
    }
  }

  # styling
  p <- p +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      x = "Proportion Treated",
      y = y_label,
      color = if (single_model) "Curve" else "Model/Curve",
      fill = if (single_model) "Curve" else "Model/Curve"
    ) +
    theme_fn() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )

  # add title/subtitle
  if (is.null(title)) {
    if (single_model) {
      title <- "QINI Curves"
    } else {
      title <- paste("QINI Curves:", length(qini_list), "Models")
    }
  }

  if (is.null(subtitle)) {
    subtitle <- paste("Scale:", scale, "| Test set size:", n_units)
  }

  p <- p + labs(title = title, subtitle = subtitle)

  if (verbose) {
    cli::cli_alert_success("Created QINI plot with {n_curves} curves")
  }

  return(p)
}

#' Create Annotated QINI Plot with Gain Information
#'
#' Enhanced version that adds gain annotations at specified spend levels
#'
#' @param qini_results Results from margot_qini_dev()
#' @param outcome Single outcome to plot with annotations
#' @param annotate_spends Numeric vector of spend levels to annotate
#' @param ... Additional arguments passed to margot_plot_qini_dev()
#'
#' @return A ggplot2 object with annotations
#'
#' @export
#' @keywords internal
#' @importFrom ggplot2 annotate geom_point
margot_plot_qini_annotated_dev <- function(
    qini_results,
    outcome,
    annotate_spends = c(0.2),
    ...) {
  # create base plot
  p <- margot_plot_qini_dev(
    qini_results,
    outcomes = outcome,
    spend_markers = annotate_spends,
    ...
  )

  # extract gain summaries for annotations
  if (inherits(qini_results, "margot_qini_dev")) {
    gain_summaries <- qini_results$gain_summaries[[outcome]]
  } else {
    stop("Annotations only supported for single margot_qini_dev objects")
  }

  # add annotations for each spend level
  for (spend in annotate_spends) {
    spend_key <- paste0("spend_", spend)
    if (spend_key %in% names(gain_summaries)) {
      gains <- gain_summaries[[spend_key]]

      # add point and text annotation
      p <- p +
        geom_point(
          data = data.frame(
            proportion = spend,
            gain = gains$cate_gain,
            curve_label = "cate"
          ),
          size = 3,
          shape = 21,
          fill = "white"
        ) +
        annotate(
          "text",
          x = spend,
          y = gains$cate_gain,
          label = sprintf(
            "Gain: %.3f\n(%.0f%% treated)",
            gains$diff_gain, spend * 100
          ),
          hjust = -0.1,
          vjust = 0.5,
          size = 3
        )
    }
  }

  return(p)
}

#' Compare QINI Curves Across Different Methods
#'
#' Convenience function for comparing QINI curves from different baseline methods
#'
#' @param cf_results Results from margot_causal_forest_dev()
#' @param outcome Single outcome to analyze
#' @param baseline_methods Vector of baseline methods to compare
#' @param ... Additional arguments passed to margot_plot_qini_dev()
#'
#' @return A ggplot2 object comparing methods
#'
#' @export
#' @keywords internal
margot_plot_qini_compare_methods_dev <- function(
    cf_results,
    outcome,
    baseline_methods = c("maq_no_covariates", "simple", "none"),
    ...) {
  # compute QINI for each method
  qini_list <- list()

  for (method in baseline_methods) {
    qini_list[[method]] <- margot_qini_dev(
      cf_results,
      outcome_vars = outcome,
      baseline_method = method,
      verbose = FALSE
    )
  }

  # create comparison plot
  margot_plot_qini_dev(
    qini_list,
    outcomes = outcome,
    facet_outcomes = FALSE,
    title = paste("QINI Curves:", outcome),
    subtitle = "Comparing baseline methods",
    ...
  )
}
