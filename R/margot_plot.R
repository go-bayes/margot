#' Visualise Causal Effect Estimates with Enhanced Flexibility
#'
#' @description This function renders a graphical representation of causal effect estimates, organised by effect size on either the risk difference (RD) or risk ratio (RR) scale. It categorises estimates into "positive", "negative", or "zero_crossing" based on their confidence intervals. The function features advanced customisability for graphical parameters, including error bar width and the application of custom ggplot2 themes. Notably, it avoids direct manipulation of the input data and implements an advanced labelling strategy for the x-axis to prevent incoherent negative values for RR.
#'
#' @param .data A data frame of causal effect estimates, ideally output by a `group_tab` function or similar. The data should include confidence intervals and effect estimate values.
#' @param type Character string indicating the scale of effect estimates: "RD" for risk difference or "RR" for risk ratio, with "RD" as the default.
#' @param order Character string indicating the ordering of the output: "default" for default descending, "alphabetical" for alphabetical, and "custom" for custom order provided in .data.
#' @param custom_order Optional vector specifying custom ordering if 'order' is set to 'custom'.
#' @param title Main title for the plot.
#' @param subtitle Subtitle for the plot.
#' @param estimate_scale Numeric multiplier to adjust the horizontal offset of estimate labels, aiding in plot clarity. Default is 1.
#' @param base_size Base font size for the plot, applied globally unless overridden. Default is 11.
#' @param text_size Font size for the estimate labels. Default is 2.75.
#' @param point_size Size of points representing the estimates. Default is 0.5.
#' @param title_size Font size for the plot title. Default is 10.
#' @param subtitle_size Font size for the plot subtitle. Default is 9.
#' @param legend_text_size Font size for legend text. Default is 6.
#' @param legend_title_size Font size for legend titles. Default is 6.
#' @param x_offset Horizontal adjustment for estimate labels, varied based on `type`. Default adjustments are 0 for "RR" and -1.75 for "RD".
#' @param x_lim_lo Lower limit of the x-axis, automatically adjusted based on `type`.
#' @param x_lim_hi Upper limit of the x-axis, automatically adjusted based on `type`.
#' @param linewidth Width of the error bars in the plot. Default is 0.5.
#' @param plot_theme ggplot2 theme object for customising plot appearance. Inherits `base_size` from `base_size` parameter to maintain consistency. Uses `theme_classic()` as default but allows for customisation.
#'
#' @return A ggplot object displaying the causal effect estimates with categorisation and error bars. This plot is tailored for further modifications or direct usage.
#' @export
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline scale_color_manual labs geom_text coord_cartesian theme element_text margin
#' @importFrom rlang .data
#' @import dplyr
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("default", "alphabetical", "custom"),
                        custom_order = NULL,
                        title,
                        subtitle,
                        estimate_scale = 1,
                        base_size = 11,
                        text_size = 2.75,
                        point_size = .5,
                        title_size = 10,
                        subtitle_size = 9,
                        legend_text_size = 6,
                        legend_title_size = 6,
                        x_offset = ifelse(type == "RR", 0, -1.75),
                        x_lim_lo = ifelse(type == "RR", .1, -1.75),
                        x_lim_hi = ifelse(type == "RR", 2.5, 1),
                        linewidth = .5,
                        plot_theme = NULL) {
  require("ggplot2")
  require("dplyr")

  type <- match.arg(type)
  order <- match.arg(order)

  # Check if the data needs processing by group_tab
  if (!"Estimate" %in% names(.data) || !"outcome" %in% names(.data)) {
    .data <- group_tab(.data, type = type, order = order, custom_order = custom_order)
  }

  # Dynamic theme adjustment
  if (is.null(plot_theme)) {
    plot_theme <- theme_classic(base_size = base_size)
  } else {
    plot_theme <- plot_theme + theme(text = element_text(size = base_size))
  }

  # Start building the plot
  x_name <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
  out <- ggplot(
    data = .data,
    aes(
      y = reorder(outcome, .data[[x_name]]),
      x = .data[[x_name]],
      xmin = .data$`2.5 %`,
      xmax = .data$`97.5 %`,
      color = Estimate
    )
  ) + geom_errorbarh(aes(color = Estimate), height = .3,
                     linewidth = linewidth, position = position_dodge(width = .3)) +
    geom_point(size = point_size, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
    scale_color_manual(values = c("positive" = "dodgerblue", "not reliable" = "black", "negative" = "orange")) +
    labs(x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"), y = NULL, title = title, subtitle = subtitle) +
    geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = "bold") +
    coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
    plot_theme +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

  # Conditionally add x-axis scale modifications for RR
  if (type == "RR") {
    custom_x_labels <- function(x) {
      ifelse(x < 0, "", as.character(x))
    }
    out <- out + scale_x_continuous(labels = custom_x_labels)
  }

  return(out)
}

# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         title,
#                         subtitle,
#                         estimate_scale = 1,
#                         base_size = 11,
#                         text_size = 2.75,
#                         point_size = .5,
#                         title_size = 10,
#                         subtitle_size = 9,
#                         legend_text_size = 6,
#                         legend_title_size = 6,
#                         x_offset = ifelse(type == "RR", 0, -1.75),
#                         x_lim_lo = ifelse(type == "RR", .1, -1.75),
#                         x_lim_hi = ifelse(type == "RR", 2.5, 1),
#                         linewidth = .5,
#                         plot_theme = NULL){
#   type <- match.arg(type)
#
#   # dynamic theme adjustment
#   if (is.null(plot_theme)) {
#     plot_theme <- theme_classic(base_size = base_size)
#   } else {
#     # Apply base_size to the plot_theme
#     plot_theme <- plot_theme + theme(text = element_text(size = base_size))
#   }
#
#
#   # Copy data to avoid direct manipulation
#   plot_data <- .data
#   # Define reliability based on type
#   reliability_condition <- if (type == "RR") {
#     list(condition = c(1, 1), label = "Causal risk ratio scale")
#   } else {
#     list(condition = c(0, 0), label = "Causal difference scale")
#   }
#
#   plot_data$Reliability <- ifelse(
#     plot_data$`2.5 %` > reliability_condition$condition[1] & plot_data$`97.5 %` > reliability_condition$condition[2],
#     "positive",
#     ifelse(
#       plot_data$`2.5 %` < reliability_condition$condition[1] & plot_data$`97.5 %` < reliability_condition$condition[2],
#       "negative",
#       "zero_crossing"
#     )
#   )
#
#   # Start building the plot
#   x_name <- paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")
#   out <- ggplot(
#     data = plot_data,
#     aes(
#       y = reorder(outcome, .data[[x_name]]),
#       x = .data[[x_name]],
#       xmin = .data$`2.5 %`,
#       xmax = .data$`97.5 %`,
#       group = Estimate,
#       color = Reliability
#     )
#   ) + geom_errorbarh(aes(color = Reliability), height = .3,
#                      linewidth = linewidth, position = position_dodge(width = .3)) +
#     geom_point(size = point_size, position = position_dodge(width = 0.3)) +
#     geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
#     scale_color_manual(values = c("positive" = "dodgerblue", "zero_crossing" = "black", "negative" = "orange")) +
#     labs(x = reliability_condition$label, y = NULL, title = title, subtitle = subtitle) +
#     geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = ifelse(plot_data$Estimate == "unreliable", "plain", "bold")) +
#     coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
#     plot_theme +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.title = element_text(face = "bold", size = title_size, hjust = 0),
#       plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
#       legend.text = element_text(size = legend_text_size),
#       legend.title = element_text(size = legend_title_size),
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#     )
#
#   # Conditionally add x-axis scale modifications
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + scale_x_continuous(labels = custom_x_labels)
#   }
#
#   return(out)
# }
margot_interpret_table <- function(df, causal_scale, estimand) {
  # Load required libraries
  require(dplyr)
  require(glue)

  # Estimand descriptions
  estimand_description <- dplyr::case_when(
    estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
    estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
    estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
    estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
    estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
    TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
  )

  # Identify the correct column for calculations based on causal_scale
  causal_contrast_column <- if (causal_scale == "causal_difference") {
    "E[Y(1)]-E[Y(0)]"
  } else if (causal_scale == "risk_ratio") {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Invalid causal_scale. Valid options are 'causal_difference' or 'risk_ratio'.")
  }

  # Format the causal_scale for output
  formatted_causal_scale <- if (causal_scale == "causal_difference") {
    "causal difference"
  } else {
    "risk ratio"
  }

  # Check if the required columns are in the dataframe and process with group_tab if not
  if (!"Estimate" %in% names(df) || !"outcome" %in% names(df)) {
    df <- group_tab(df, type = if (causal_scale == "causal_difference") "RD" else "RR")
  }

  if (!causal_contrast_column %in% names(df)) {
    stop(paste("Dataframe does not contain the required column:", causal_contrast_column))
  }

  # Data processing and interpretation
  interpretation <- df %>%
    dplyr::mutate(
      causal_contrast = round(.data[[causal_contrast_column]], 3),
      E_Value = round(E_Value, 3),
      E_Val_bound = round(E_Val_bound, 3),
      `2.5 %` = round(`2.5 %`, 3),
      `97.5 %` = round(`97.5 %`, 3),
      strength_of_evidence = case_when(
        E_Val_bound == 1 ~ "**that evidence for causality is not reliable**",
        E_Val_bound <= 1 | (`2.5 %` <= 0 & `97.5 %` >= 0) ~ "that the **evidence for causality is not reliable**",
        E_Val_bound > 1 & E_Val_bound < 1.1 ~ "that the **evidence for causality is weak**",
        E_Val_bound > 2 ~ "that **the evidence for causality is not reliable**",
        TRUE ~ "**there is evidence for causality**"
      ),
      outcome_interpretation = glue::glue(
        "For '{outcome}', the effect estimate on the {formatted_causal_scale} scale is {causal_contrast} [{`2.5 %`}, {`97.5 %`}]. ",
        "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
        "At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of {E_Val_bound} to negate the observed effect. Weaker confounding would not overturn it. ",
        "We infer {strength_of_evidence}."
      )
    )

  # Compile results
  result <- glue::glue(
    "\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}"
  )
  return(result)
}
