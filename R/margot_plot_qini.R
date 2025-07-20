#' Plot Qini Curves from margot_multi_arm_causal_forest Results
#'
#' This function creates a ggplot object displaying Qini curves based on the
#' results of a margot_multi_arm_causal_forest() model. It includes label
#' transformations and informative CLI messages.
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
#'
#' @return A ggplot object representing the Qini curves for the specified outcome variable.
#'
#' @import ggplot2
#' @import ggokabeito
#' @import cli
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
                             theme = "classic") {
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
  if (!outcome_var %in% names(mc_result$results)) {
    cli::cli_abort("Outcome variable not found in mc_result$results: {outcome_var}")
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

  # Check if treatment is binary
  is_binary <- length(unique(qini_data$curve)) == 2

  # Transform curve labels (only once for each unique label)
  unique_curves <- unique(qini_data$curve)
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

  # For binary treatments, rename to CATE and ATE (in this order)
  if (is_binary) {
    transformed_curves <- c("CATE", "ATE")
    names(transformed_curves) <- unique_curves
  }

  curve_mapping <- setNames(transformed_curves, unique_curves)
  qini_data$curve <- curve_mapping[qini_data$curve]
  cli::cli_alert_success("Treatment curve labels transformed")

  # create plot
  cli::cli_alert("Creating Qini curves plot...")
  p <- ggplot(qini_data, aes(x = proportion, y = gain, colour = curve, linetype = curve)) +
    geom_line(linewidth = 0.5)

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
    ggokabeito::scale_color_okabe_ito() +
    guides(colour = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm"),
           linetype = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm"))

  cli::cli_alert_success("Qini curves plot created successfully 👍")
  return(p)
}
