#' Plot Rank Average Treatment Effect
#'
#' This function creates a ggplot visualisation of the Rank Average Treatment Effect.
#' It displays the estimate with a confidence interval, using a simple black line
#' and light gray shading by default.
#'
#' @param x An object of class "rank_average_treatment_effect", typically the output
#'   of the rank_average_treatment_effect() function.
#' @param outcome_var A character string specifying the name of the outcome variable
#'   to plot. This is used for the plot title with proper label transformation.
#' @param title Character string for the plot title. If NULL (default), a title is
#'   generated using the transformed outcome variable name.
#' @param subtitle Character string for the plot subtitle. Default explains the confidence interval.
#' @param x_lab Character string for the x-axis label. Default is "Treated fraction (q)".
#' @param y_lab Character string for the y-axis label. Default is "Estimate".
#' @param remove_tx_prefix Logical value indicating whether to remove the "tx_" prefix from labels. Default is TRUE.
#' @param remove_z_suffix Logical value indicating whether to remove the "_z" suffix from labels. Default is TRUE.
#' @param use_title_case Logical value indicating whether to convert labels to title case. Default is TRUE.
#' @param remove_underscores Logical value indicating whether to remove underscores from labels. Default is TRUE.
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param ... Additional arguments passed to ggplot.
#'
#' @return A ggplot object that can be further customised or printed.
#'
#' @examples
#' \dontrun{
#' # Assuming rate_eval is your rank_average_treatment_effect object
#' p <- margot_plot_rate(rate_eval, "model_t2_belong_z")
#' print(p)
#'
#' # Using custom label mapping
#' label_mapping <- list(
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
#' )
#' p <- margot_plot_rate(rate_eval, "model_t2_env_not_env_efficacy_z",
#'                       label_mapping = label_mapping)
#' print(p)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline labs theme_minimal theme
#' @importFrom dplyr mutate
#'
#' @export
margot_plot_rate <- function(x,
                             outcome_var,
                             title = NULL,
                             subtitle = "(95% confidence interval shown as shaded area)",
                             x_lab = "Treated fraction (q)",
                             y_lab = "Estimate",
                             remove_tx_prefix = TRUE,
                             remove_z_suffix = TRUE,
                             use_title_case = TRUE,
                             remove_underscores = TRUE,
                             label_mapping = NULL,
                             ...) {
  # transform the outcome variable name for the title
  if (!is.null(outcome_var)) {
    transformed_outcome_var <- transform_var_name(outcome_var, label_mapping,
                                                  remove_tx_prefix, remove_z_suffix,
                                                  use_title_case, remove_underscores)

    # if no title is provided, create one with the transformed outcome name
    if (is.null(title)) {
      title <- paste("Targeting Operator Characteristic for", transformed_outcome_var)
    }
  } else if (is.null(title)) {
    # fallback title if no outcome_var is provided
    title <- "Targeting Operator Characteristic"
  }

  cli::cli_alert_info("Creating RATE plot for: {outcome_var}")

  TOC <- x$TOC
  # prepare the data
  plot_data <- TOC %>%
    mutate(
      lower = estimate - 1.96 * std.err,
      upper = estimate + 1.96 * std.err
    )

  # transform priority labels if present
  if ("priority" %in% colnames(plot_data) && length(unique(plot_data$priority)) > 1) {
    unique_priorities <- unique(plot_data$priority)
    transformed_priorities <- sapply(unique_priorities, function(p) {
      transform_var_name(p, label_mapping, remove_tx_prefix, remove_z_suffix,
                         use_title_case, remove_underscores)
    })

    priority_mapping <- setNames(transformed_priorities, unique_priorities)
    plot_data$priority <- priority_mapping[plot_data$priority]
    cli::cli_alert_success("Priority labels transformed")
  }

  # create the plot
  p <- ggplot(plot_data, aes(x = q, y = estimate, color = priority, fill = priority)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "top"  # place legend at the top
    )

  # if there's only one priority, use default colors
  if (length(unique(plot_data$priority)) <= 1) {
    p <- p +
      scale_color_manual(values = "black") +
      scale_fill_manual(values = "lightgray")
  } else {
    # use okabe-ito palette for multiple priorities
    p <- p +
      ggokabeito::scale_color_okabe_ito() +
      ggokabeito::scale_fill_okabe_ito(alpha = 0.5)
  }

  # additional customisation options
  dots <- list(...)
  if (length(dots) > 0) {
    p <- p + dots
  }

  cli::cli_alert_success("RATE plot created successfully 👍")
  return(p)
}

#' #' Plot Rank Average Treatment Effect
#' #'
#' #' This function creates a ggplot visualisation of the Rank Average Treatment Effect.
#' #' It displays the estimate with a confidence interval, using a simple black line
#' #' and light gray shading by default.
#' #'
#' #' @param x An object of class "rank_average_treatment_effect", typically the output
#' #'   of the rank_average_treatment_effect() function.
#' #' @param title Character string for the plot title. Default is "Targeting Operator Characteristic".
#' #' @param subtitle Character string for the plot subtitle. Default explains the confidence interval.
#' #' @param x_lab Character string for the x-axis label. Default is "Treated fraction (q)".
#' #' @param y_lab Character string for the y-axis label. Default is "Estimate".
#' #' @param ... Additional arguments passed to ggplot.
#' #'
#' #' @return A ggplot object that can be further customised or printed.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Assuming rate_eval is your rank_average_treatment_effect object
#' #' p <- plot_rank_average_treatment_effect(rate_eval)
#' #' print(p)
#' #'
#' #' # Customise colors using ggokabeito
#' #' p_colored <- p +
#' #'   ggokabeito::scale_fill_okabe_ito() +
#' #'   ggokabeito::scale_color_okabe_ito()
#' #' print(p_colored)
#' #' }
#' #'
#' #' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline labs theme_minimal theme
#' #' @importFrom dplyr mutate
#' #'
#' #' @export
#' margot_plot_rate <- function(x,
#'                                  title = "Targeting Operator Characteristic",
#'                                  subtitle = "(95% confidence interval shown as shaded area)",
#'                                  x_lab = "Treated fraction (q)",
#'                                  y_lab = "Estimate",
#'                                  ...) {
#'   TOC <- x$TOC
#'
#'   # Prepare the data
#'   plot_data <- TOC %>%
#'     mutate(
#'       lower = estimate - 1.96 * std.err,
#'       upper = estimate + 1.96 * std.err
#'     )
#'
#'   # Create the plot
#'   p <- ggplot(plot_data, aes(x = q, y = estimate, color = priority, fill = priority)) +
#'     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#'     geom_line() +
#'     geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#'     labs(
#'       title = title,
#'       subtitle = subtitle,
#'       x = x_lab,
#'       y = y_lab
#'     ) +
#'     theme_classic() +
#'     theme(
#'       plot.title = element_text(hjust = 0.5),
#'       plot.subtitle = element_text(hjust = 0.5),
#'       legend.position = "top"  # Place legend at the top
#'     ) +
#'     scale_color_manual(values = "black") +
#'     scale_fill_manual(values = "lightgray")
#'
#'   #  additional customisation options
#'   dots <- list(...)
#'   if (length(dots) > 0) {
#'     p <- p + dots
#'   }
#'
#'   return(p)
#' }
