#' Plot Qini Curves from margot_multi_arm_causal_forest Results
#'
#' This function creates a ggplot object displaying Qini curves based on the
#' results of a margot_multi_arm_causal_forest() model. It now includes label
#' transformations and informative CLI messages.
#'
#' @param mc_result A list containing the results from margot_multi_arm_causal_forest().
#' @param outcome_var A character string specifying the name of the outcome variable
#'   to plot. This should match one of the model names in mc_result$results.
#' @param remove_tx_prefix Logical value indicating whether to remove the "tx_" prefix from labels. Default is TRUE.
#' @param remove_z_suffix Logical value indicating whether to remove the "_z" suffix from labels. Default is TRUE.
#' @param use_title_case Logical value indicating whether to convert labels to title case. Default is TRUE.
#' @param remove_underscores Logical value indicating whether to remove underscores from labels. Default is TRUE.
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
#' }
#'
#' @export
margot_plot_qini <- function(mc_result, outcome_var,
                             remove_tx_prefix = TRUE,
                             remove_z_suffix = TRUE,
                             use_title_case = TRUE,
                             remove_underscores = TRUE) {

  cli::cli_h1("Margot Plot Qini Curves")

  # Function to transform labels
  transform_label <- function(label) {
    original_label <- label
    if (remove_tx_prefix) {
      label <- sub("^t[0-9]+_", "", label)
    }
    if (remove_z_suffix) {
      label <- sub("_z$", "", label)
    }
    if (remove_underscores) {
      label <- gsub("_", " ", label)
    }
    if (use_title_case) {
      label <- tools::toTitleCase(label)
    }
    if (label != original_label) {
      cli::cli_alert_info("Transformed label: {original_label} -> {label}")
    }
    return(label)
  }

  # Transform the outcome variable name
  transformed_outcome_var <- transform_label(outcome_var)

  # Extract the qini data for the specified outcome variable
  qini_data <- mc_result$results[[outcome_var]]$qini_data
  if (is.null(qini_data)) {
    cli::cli_abort("Qini data not found for the specified outcome variable: {outcome_var}")
  }
  cli::cli_alert_success("Qini data extracted for outcome variable: {outcome_var}")

  # Check if treatment is binary
  is_binary <- length(unique(qini_data$arm)) == 2

  # Transform arm labels (only once for each unique label)
  unique_arms <- unique(qini_data$arm)
  transformed_arms <- sapply(unique_arms, transform_label)

  # For binary treatments, rename to ATE and CATE
  if (is_binary) {
    transformed_arms <- c("ATE", "CATE")
    names(transformed_arms) <- unique_arms
  }

  arm_mapping <- setNames(transformed_arms, unique_arms)
  qini_data$arm <- arm_mapping[qini_data$arm]
  cli::cli_alert_success("Treatment arm labels transformed")

  # Create the plot
  cli::cli_alert("Creating Qini curves plot...")
  p <- ggplot(qini_data, aes(x = index, y = gain, colour = arm, linetype = arm)) +
    geom_line(linewidth = 0.5) +
    labs(
      x = "Number of individuals targeted",
      y = "Cumulative gain",
      title = paste("Qini Curves for", transformed_outcome_var)
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    ) +
    ggokabeito::scale_color_okabe_ito() +
    guides(colour = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm"),
           linetype = guide_legend(title = if(is_binary) "Curve Type" else "Treatment Arm"))

  cli::cli_alert_success("Qini curves plot created successfully \U0001F44D")

  return(p)
}
# margot_plot_qini <- function(mc_result, outcome_var,
#                              remove_tx_prefix = TRUE,
#                              remove_z_suffix = TRUE,
#                              use_title_case = TRUE,
#                              remove_underscores = TRUE) {
#
#   cli::cli_h1("Margot Plot Qini Curves")
#
#   # Function to transform labels
#   transform_label <- function(label) {
#     original_label <- label
#     if (remove_tx_prefix) {
#       label <- sub("^t[0-9]+_", "", label)
#     }
#     if (remove_z_suffix) {
#       label <- sub("_z$", "", label)
#     }
#     if (remove_underscores) {
#       label <- gsub("_", " ", label)
#     }
#     if (use_title_case) {
#       label <- tools::toTitleCase(label)
#     }
#     if (label != original_label) {
#       cli::cli_alert_info("Transformed label: {original_label} -> {label}")
#     }
#     return(label)
#   }
#
#   # Transform the outcome variable name
#   transformed_outcome_var <- transform_label(outcome_var)
#
#   # Extract the qini data for the specified outcome variable
#   qini_data <- mc_result$results[[outcome_var]]$qini_data
#   if (is.null(qini_data)) {
#     cli::cli_abort("Qini data not found for the specified outcome variable: {outcome_var}")
#   }
#   cli::cli_alert_success("Qini data extracted for outcome variable: {outcome_var}")
#
#   # Transform arm labels (only once for each unique label)
#   unique_arms <- unique(qini_data$arm)
#   transformed_arms <- sapply(unique_arms, transform_label)
#   arm_mapping <- setNames(transformed_arms, unique_arms)
#   qini_data$arm <- arm_mapping[qini_data$arm]
#   cli::cli_alert_success("Treatment arm labels transformed")
#
#   # Create the plot
#   cli::cli_alert("Creating Qini curves plot...")
#   p <- ggplot(qini_data, aes(x = index, y = gain, colour = arm, linetype = arm)) +
#     geom_line(linewidth = 0.5) +
#     labs(
#       x = "Spend",
#       y = "Gain",
#       title = paste("Qini Curves for", transformed_outcome_var)
#     ) +
#     theme_classic() +
#     theme(
#       legend.position = "top",
#       plot.title = element_text(hjust = 0.5)
#     ) +
#     ggokabeito::scale_color_okabe_ito() +
#     guides(colour = guide_legend(title = "Treatment Arm"),
#            linetype = guide_legend(title = "Treatment Arm"))
#
#   cli::cli_alert_success("Qini curves plot created successfully \U0001F44D")
#
#   return(p)
# }
# margot_plot_qini <- function(mc_result, outcome_var) {
#   # Extract the qini data for the specified outcome variable
#   qini_data <- mc_result$results[[outcome_var]]$qini_data
#
#   # Create the plot
#   p <- ggplot(qini_data, aes(x = index, y = gain, colour = arm, linetype = arm)) +
#     geom_line(linewidth = 0.5) +
#     labs(
#       x = "Spend",
#       y = "Gain",
#       title = paste("Qini Curves for", outcome_var)
#     ) +
#     theme_classic() +
#     theme(
#       legend.position = "top",
#       plot.title = element_text(hjust = 0.5)
#     ) +
#     ggokabeito::scale_color_okabe_ito() +
#     guides(colour = guide_legend(title = "Treatment Arm"),
#            linetype = guide_legend(title = "Treatment Arm"))
#
#   return(p)
# }
