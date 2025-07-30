#' Plot QINI Curves from Pre-computed Data
#'
#' Internal function to create QINI plots directly from pre-computed qini_data
#' without requiring the full causal forest results structure.
#'
#' @param qini_data Data frame with columns: proportion, gain, curve
#' @param qini_objects List of maq objects (optional, for confidence intervals)
#' @param outcome_var Character string for the outcome variable name
#' @param label_mapping Optional named list for label transformations
#' @param spend_levels Numeric vector of spend levels for vertical lines
#' @param show_spend_lines Logical; show vertical spend lines
#' @param spend_line_color Color for spend lines
#' @param spend_line_alpha Alpha for spend lines
#' @param theme ggplot2 theme name
#' @param show_ci Logical or character for confidence intervals
#' @param ci_alpha Significance level for CI
#' @param ci_n_points Number of points for CI computation
#' @param ci_ribbon_alpha Alpha for CI ribbons
#' @param ci_ribbon_color Color for CI ribbons
#' @param horizontal_line Logical; draw horizontal lines for complete paths
#' @param ylim Y-axis limits
#' @param fixed_ylim Logical; use fixed y-axis scaling
#' @param cate_color Color for CATE curve
#' @param ate_color Color for ATE curve
#' @param treatment_cost Treatment cost value
#' @param x_axis Type of x-axis: "proportion" or "budget". If not specified, will be inferred from data.
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot object
#' @keywords internal
#' @import ggplot2
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline labs 
#' @importFrom ggplot2 scale_color_manual scale_fill_manual guides guide_legend
#' @importFrom ggplot2 theme_classic theme_minimal theme_bw theme_gray theme_light theme_dark theme_void
#' @importFrom ggplot2 theme element_text coord_cartesian
margot_plot_qini_direct <- function(
  qini_data,
  qini_objects = NULL,
  outcome_var = "Outcome",
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
  ylim = NULL,
  fixed_ylim = FALSE,
  cate_color = "#d8a739",
  ate_color = "#4d4d4d",
  treatment_cost = 1,
  x_axis = NULL,
  ...
) {
  
  # transform outcome variable name
  transformed_outcome_var <- transform_var_name(
    gsub("^model_", "", outcome_var),
    label_mapping,
    remove_tx_prefix = TRUE,
    remove_z_suffix = TRUE,
    use_title_case = TRUE,
    remove_underscores = TRUE
  )
  
  # ensure qini_data has the right structure
  if (is.null(qini_data) || nrow(qini_data) == 0) {
    stop("No QINI data available for plotting")
  }
  
  # infer x_axis type if not specified
  if (is.null(x_axis)) {
    # if max x value > 1, assume it's budget
    max_x <- max(qini_data$proportion, na.rm = TRUE)
    x_axis <- if (max_x > 1.1) "budget" else "proportion"
  }
  
  # transform curve labels
  qini_data$curve <- ifelse(
    tolower(qini_data$curve) %in% c("cate", "treatment"), "CATE",
    ifelse(tolower(qini_data$curve) %in% c("ate", "baseline"), "ATE", qini_data$curve)
  )
  
  # handle fixed_ylim
  if (fixed_ylim && is.null(ylim)) {
    # for fixed ylim, just use the actual data range with padding
    current_range <- range(qini_data$gain, na.rm = TRUE)
    padding <- diff(current_range) * 0.05
    ylim <- c(current_range[1] - padding, current_range[2] + padding)
  }
  
  # parse show_ci
  compute_ci <- FALSE
  ci_curves <- character(0)
  
  if (is.logical(show_ci)) {
    if (show_ci) {
      compute_ci <- TRUE
      ci_curves <- c("CATE", "ATE")
    }
  } else if (is.character(show_ci)) {
    show_ci_lower <- tolower(show_ci)
    if (show_ci_lower %in% c("both", "true")) {
      compute_ci <- TRUE
      ci_curves <- c("CATE", "ATE")
    } else if (show_ci_lower == "cate") {
      compute_ci <- TRUE
      ci_curves <- "CATE"
    } else if (show_ci_lower == "ate") {
      compute_ci <- TRUE
      ci_curves <- "ATE"
    }
  }
  
  # compute confidence intervals if requested and qini_objects available
  ci_data <- NULL
  if (compute_ci && !is.null(qini_objects)) {
    # simplified CI computation for direct plotting
    # this is a placeholder - full CI computation would be more complex
    ci_data <- NULL
  }
  
  # create base plot
  p <- ggplot(qini_data, aes(x = proportion, y = gain, colour = curve, linetype = curve)) +
    geom_line(size = 1)
  
  # add confidence intervals if available
  if (!is.null(ci_data)) {
    if (is.null(ci_ribbon_color)) {
      p <- p + geom_ribbon(
        data = ci_data,
        aes(x = proportion, ymin = lower, ymax = upper, fill = curve),
        alpha = ci_ribbon_alpha,
        linetype = 0,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
    }
  }
  
  # add spend lines
  if (show_spend_lines && length(spend_levels) > 0) {
    # adjust spend levels for budget x-axis
    adjusted_spend_levels <- if (x_axis == "budget") {
      spend_levels * treatment_cost
    } else {
      spend_levels
    }
    
    for (spend in adjusted_spend_levels) {
      p <- p + geom_vline(
        xintercept = spend,
        linetype = "dashed",
        color = spend_line_color,
        alpha = spend_line_alpha
      )
    }
  }
  
  # horizontal line extension
  if (horizontal_line) {
    # find where each curve ends
    for (crv in unique(qini_data$curve)) {
      crv_data <- qini_data[qini_data$curve == crv, ]
      max_prop <- max(crv_data$proportion)
      
      if (max_prop < 1) {
        last_gain <- crv_data$gain[which.max(crv_data$proportion)]
        extension_data <- data.frame(
          proportion = c(max_prop, 1),
          gain = c(last_gain, last_gain),
          curve = crv
        )
        p <- p + geom_line(
          data = extension_data,
          aes(x = proportion, y = gain),
          color = if (crv == "CATE") cate_color else ate_color,
          linetype = "solid",
          size = 1
        )
      }
    }
  }
  
  # create subtitle
  subtitle_text <- if (treatment_cost != 1) {
    paste0("Treatment cost = ", treatment_cost)
  } else {
    NULL
  }
  
  # finalize plot
  x_label <- if (x_axis == "budget") {
    "Budget per unit (B)"
  } else {
    "Proportion of population targeted"
  }
  
  p <- p +
    labs(
      x = x_label,
      y = "Average treatment effect",
      title = paste("Qini Curves for", transformed_outcome_var),
      subtitle = subtitle_text
    ) +
    scale_color_manual(values = c("CATE" = cate_color, "ATE" = ate_color)) +
    scale_fill_manual(values = c("CATE" = cate_color, "ATE" = ate_color)) +
    guides(colour = guide_legend(title = "Curve Type"),
           linetype = guide_legend(title = "Curve Type"))
  
  # apply theme
  p <- p + switch(theme,
    "classic" = theme_classic(),
    "minimal" = theme_minimal(),
    "bw" = theme_bw(),
    "gray" = theme_gray(),
    "light" = theme_light(),
    "dark" = theme_dark(),
    "void" = theme_void(),
    theme_classic()
  )
  
  # additional theme adjustments
  p <- p + theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )
  
  # apply ylim if specified
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  return(p)
}