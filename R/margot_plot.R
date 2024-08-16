#' Visualise Causal Effect Estimates with Enhanced Flexibility
#'
#' @description This function renders a graphical representation of causal effect estimates, organised by effect size on either the risk difference (RD) or risk ratio (RR) scale. It categorises estimates into "positive", "negative", or "not reliable" based on their confidence intervals. The function features advanced customisability for graphical parameters, including error bar width and the application of custom ggplot2 themes.
#'
#' @param .data A data frame of causal effect estimates, ideally output by a `group_tab` function or similar. The data should include confidence intervals and effect estimate values.
#' @param type Character string indicating the scale of effect estimates: "RD" for risk difference or "RR" for risk ratio. Default is "RD".
#' @param order Character string indicating the ordering of the output: "default" for descending order by effect size, or "alphabetical" for alphabetical order. Default is "default".
#' @param title_binary Main title for the plot. If NULL, no title is displayed.
#' @param push_mods Directory path for saving the plot. If NULL, the plot is saved in the current working directory.
#' @param ... Additional arguments passed to the function.
#' @param options A list of optional parameters to customize the plot. See details for available options.
#'
#' @details
#' The `options` list can include the following parameters:
#' \itemize{
#'   \item `title`: Main title for the plot (overrides `title_binary` if both are provided).
#'   \item `subtitle`: Subtitle for the plot.
#'   \item `estimate_scale`: Numeric multiplier to adjust the horizontal offset of estimate labels. Default is 1.
#'   \item `base_size`: Base font size for the plot. Default is 11.
#'   \item `text_size`: Font size for the estimate labels. Default is 2.75.
#'   \item `point_size`: Size of points representing the estimates. Default is 3.
#'   \item `title_size`: Font size for the plot title. Default is 10.
#'   \item `subtitle_size`: Font size for the plot subtitle. Default is 9.
#'   \item `legend_text_size`: Font size for legend text. Default is 6.
#'   \item `legend_title_size`: Font size for legend titles. Default is 6.
#'   \item `x_offset`: Horizontal adjustment for estimate labels. Default is 0 for "RR" and -1.75 for "RD".
#'   \item `x_lim_lo`: Lower limit of the x-axis. Default is 0.1 for "RR" and -1.75 for "RD".
#'   \item `x_lim_hi`: Upper limit of the x-axis. Default is 2.5 for "RR" and 1 for "RD".
#'   \item `linewidth`: Width of the error bars in the plot. Default is 0.5.
#'   \item `plot_theme`: ggplot2 theme object for customising plot appearance.
#'   \item `colors`: Named vector of colors for positive, not reliable, and negative estimates. Default is c("positive" = "#E69F00", "not reliable" = "grey50", "negative" = "#56B4E9").
#'   \item `facet_var`: Variable name for faceting the plot.
#'   \item `confidence_level`: Confidence level for the intervals. Default is 0.95.
#'   \item `annotations`: Additional ggplot2 annotations to add to the plot.
#'   \item `save_plot`: Logical indicating whether to save the plot. Default is TRUE.
#'   \item `save_plot_options`: List with details for saving the plot. It can include `width`, `height`, `dpi`, and `filename`.
#'         If `filename` is not specified, the subtitle (if provided) will be used as the filename, with spaces replaced by underscores and ".png" appended.
#'   \item `push_mods`: Directory path for saving the plot (overrides the `push_mods` parameter if both are provided).
#'   \item `show_evalues`: Logical indicating whether to show E-values in labels. Default is TRUE.
#'   \item `evalue_digits`: Number of digits for rounding E-values. Default is 2.
#' }
#'
#' @return A ggplot object displaying the causal effect estimates with categorisation and error bars. This plot is tailored for further modifications or direct usage.
#' @export
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline scale_color_manual labs geom_text coord_cartesian theme element_text margin theme_classic scale_x_continuous ggsave
#' @importFrom dplyr mutate arrange desc
#' @importFrom stats setNames
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("default", "alphabetical"),
                        title_binary = NULL,
                        push_mods = NULL,
                        ...,
                        options = list()) {

  # Capture additional arguments
  additional_args <- list(...)

  # Default values
  default_options <- list(
    title = title_binary,
    subtitle = NULL,
    estimate_scale = 1,
    base_size = 11,
    text_size = 2.75,
    point_size = 3,
    title_size = 10,
    subtitle_size = 9,
    legend_text_size = 6,
    legend_title_size = 6,
    x_offset = NULL,  # Will be set based on type
    x_lim_lo = NULL,  # Will be set based on type
    x_lim_hi = NULL,  # Will be set based on type
    linewidth = 0.5,
    plot_theme = NULL,
    colors = c("positive" = "#E69F00", "not reliable" = "black", "negative" = "#56B4E9"),
    facet_var = NULL,
    confidence_level = 0.95,
    annotations = NULL,
    save_plot = TRUE,
    save_plot_options = list(
      width = 10,
      height = 6,
      dpi = 300,
      filename = NULL
    ),
    push_mods = push_mods,
    show_evalues = TRUE,
    evalue_digits = 2
  )

  # Merge user-provided options with defaults and additional arguments
  options <- modifyList(modifyList(default_options, options), additional_args)

  # Ensure push_mods from function parameter takes precedence
  if (!is.null(push_mods)) {
    options$push_mods <- push_mods
  }

  require("ggplot2")
  require("dplyr")

  # Input validation
  type <- match.arg(type)
  order <- match.arg(order)
  if (!is.data.frame(.data)) stop("Input must be a data frame")

  # Determine the effect size column based on the data structure
  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
  }

  # Set type-dependent options if not provided
  if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
  if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
  if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)

  # Add row names as outcome column if it doesn't exist
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
  }

  # Prepare the data for plotting, including ordering
  .data <- .data %>%
    mutate(outcome = factor(outcome, levels = if (order == "alphabetical") sort(unique(outcome)) else unique(outcome))) %>%
    arrange(if (order == "alphabetical") outcome else desc(!!sym(effect_size_col))) %>%
    mutate(Estimate = case_when(
      `2.5 %` > 0 ~ "positive",
      `97.5 %` < 0 ~ "negative",
      TRUE ~ "not reliable"
    ))

  # Create label including E-value if option is set
  if (options$show_evalues) {
    .data$label <- sprintf(paste0("%.3f (E-value: %.", options$evalue_digits, "f, CI: %.", options$evalue_digits, "f)"),
                           .data[[effect_size_col]], .data$E_Value, .data$E_Val_bound)
  } else {
    .data$label <- sprintf("%.3f", .data[[effect_size_col]])
  }

  # Start building the plot
  out <- ggplot(
    data = .data,
    aes(
      y = outcome,
      x = !!sym(effect_size_col),
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      color = Estimate
    )
  ) + geom_errorbarh(aes(color = Estimate), height = .3,
                     linewidth = options$linewidth, position = position_dodge(width = .3)) +
    geom_point(size = options$point_size, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
    scale_color_manual(values = options$colors) +
    labs(
      x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
      y = NULL,
      title = options$title,
      subtitle = options$subtitle
    ) +
    geom_text(aes(x = options$x_offset * options$estimate_scale,
                  label = label),
              size = options$text_size, hjust = 0, fontface = "bold") +
    coord_cartesian(xlim = c(options$x_lim_lo, options$x_lim_hi)) +
    theme_classic(base_size = options$base_size) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = options$title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = options$subtitle_size, hjust = 0),
      legend.text = element_text(size = options$legend_text_size),
      legend.title = element_text(size = options$legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

  # Conditionally add x-axis scale modifications for RR
  if (type == "RR") {
    custom_x_labels <- function(x) {
      ifelse(x < 0, "", as.character(x))
    }
    out <- out + scale_x_continuous(labels = custom_x_labels)
  }

  # Add faceting if specified
  if (!is.null(options$facet_var)) {
    out <- out + facet_wrap(vars(!!sym(options$facet_var)), scales = "free_y")
  }

  # Add custom annotations if provided
  if (!is.null(options$annotations)) {
    out <- out + options$annotations
  }

  # Save plot logic
  if (isTRUE(options$save_plot)) {
    save_options <- options$save_plot_options

    # Default to subtitle if no filename is provided
    default_filename <- if (!is.null(options$subtitle)) {
      paste0(gsub("[^a-zA-Z0-9]", "_", options$subtitle), ".png")
    } else if (!is.null(options$title)) {
      paste0(gsub("[^a-zA-Z0-9]", "_", options$title), ".png")
    } else {
      "margot_plot.png"
    }

    # Use user-specified filename if provided, otherwise use default
    filename <- if (!is.null(save_options$filename)) {
      save_options$filename
    } else {
      default_filename
    }

    # Determine save path
    save_path <- if (!is.null(options$push_mods)) {
      file.path(options$push_mods, filename)
    } else {
      filename
    }

    # Create directory if it doesn't exist
    if (!is.null(options$push_mods) && !dir.exists(options$push_mods)) {
      dir.create(options$push_mods, recursive = TRUE)
    }

    # Save the plot
    tryCatch({
      ggsave(save_path, out,
             width = save_options$width,
             height = save_options$height,
             dpi = save_options$dpi)
      message(paste("Plot saved to:", normalizePath(save_path)))
    }, error = function(e) {
      message(paste("Error saving plot:", e$message))
    })
  } else {
    message("Plot was not saved as per user request.")
  }

  return(out)
}
# # old better tested
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         order = c("default", "alphabetical"),
#                         title_binary = NULL,
#                         push_mods = NULL,
#                         ...,
#                         options = list()) {
#
#   # Capture additional arguments
#   additional_args <- list(...)
#
#   # Default values
#   default_options <- list(
#     title = title_binary,
#     subtitle = NULL,
#     estimate_scale = 1,
#     base_size = 11,
#     text_size = 2.75,
#     point_size = 3,
#     title_size = 10,
#     subtitle_size = 9,
#     legend_text_size = 6,
#     legend_title_size = 6,
#     x_offset = NULL,  # Will be set based on type
#     x_lim_lo = NULL,  # Will be set based on type
#     x_lim_hi = NULL,  # Will be set based on type
#     linewidth = 0.5,
#     plot_theme = NULL,
#     colors = c("positive" = "#E69F00", "not reliable" = "black", "negative" = "#56B4E9"),
#     facet_var = NULL,
#     confidence_level = 0.95,
#     annotations = NULL,
#     save_plot = list(
#       width = 10,
#       height = 6,
#       dpi = 300,
#       filename = NULL
#     ),
#     push_mods = push_mods,
#     show_evalues = TRUE,
#     evalue_digits = 2
#   )
#
#   # Merge user-provided options with defaults and additional arguments
#   options <- modifyList(modifyList(default_options, options), additional_args)
#
#   # Ensure push_mods from function parameter takes precedence
#   if (!is.null(push_mods)) {
#     options$push_mods <- push_mods
#   }
#
#   require("ggplot2")
#   require("dplyr")
#
#   # Input validation
#   type <- match.arg(type)
#   order <- match.arg(order)
#   if (!is.data.frame(.data)) stop("Input must be a data frame")
#
#   # Determine the effect size column based on the data structure
#   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]/E[Y(0)]"
#   } else {
#     stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
#   }
#
#   # Set type-dependent options if not provided
#   if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
#   if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
#   if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)
#
#   # Add row names as outcome column if it doesn't exist
#   if (!"outcome" %in% names(.data)) {
#     .data$outcome <- rownames(.data)
#   }
#
#   # Prepare the data for plotting, including ordering
#   .data <- .data %>%
#     mutate(outcome = factor(outcome, levels = if (order == "alphabetical") sort(unique(outcome)) else unique(outcome))) %>%
#     arrange(if (order == "alphabetical") outcome else desc(!!sym(effect_size_col))) %>%
#     mutate(Estimate = case_when(
#       `2.5 %` > 0 ~ "positive",
#       `97.5 %` < 0 ~ "negative",
#       TRUE ~ "not reliable"
#     ))
#
#   # Create label including E-value if option is set
#   if (options$show_evalues) {
#     .data$label <- sprintf(paste0("%.3f (E-value: %.", options$evalue_digits, "f, CI: %.", options$evalue_digits, "f)"),
#                            .data[[effect_size_col]], .data$E_Value, .data$E_Val_bound)
#   } else {
#     .data$label <- sprintf("%.3f", .data[[effect_size_col]])
#   }
#
#   # Start building the plot
#   out <- ggplot(
#     data = .data,
#     aes(
#       y = outcome,
#       x = !!sym(effect_size_col),
#       xmin = `2.5 %`,
#       xmax = `97.5 %`,
#       color = Estimate
#     )
#   ) + geom_errorbarh(aes(color = Estimate), height = .3,
#                      linewidth = options$linewidth, position = position_dodge(width = .3)) +
#     geom_point(size = options$point_size, position = position_dodge(width = 0.3)) +
#     geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
#     scale_color_manual(values = options$colors) +
#     labs(
#       x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
#       y = NULL,
#       title = options$title,
#       subtitle = options$subtitle
#     ) +
#     geom_text(aes(x = options$x_offset * options$estimate_scale,
#                   label = label),
#               size = options$text_size, hjust = 0, fontface = "bold") +
#     coord_cartesian(xlim = c(options$x_lim_lo, options$x_lim_hi)) +
#     theme_classic(base_size = options$base_size) +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.title = element_text(face = "bold", size = options$title_size, hjust = 0),
#       plot.subtitle = element_text(face = "bold", size = options$subtitle_size, hjust = 0),
#       legend.text = element_text(size = options$legend_text_size),
#       legend.title = element_text(size = options$legend_title_size),
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#     )
#
#   # Conditionally add x-axis scale modifications for RR
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + scale_x_continuous(labels = custom_x_labels)
#   }
#
#   # Add faceting if specified
#   if (!is.null(options$facet_var)) {
#     out <- out + facet_wrap(vars(!!sym(options$facet_var)), scales = "free_y")
#   }
#
#   # Add custom annotations if provided
#   if (!is.null(options$annotations)) {
#     out <- out + options$annotations
#   }
#
#   # Ensure push_mods is set
#   if (is.null(options$push_mods)) {
#     options$push_mods <- push_mods
#   }
#
#   # Save plot automatically, unless explicitly set to FALSE
#   if (is.list(options$save_plot) && length(options$save_plot) > 0) {
#     # Default to subtitle if no filename is provided
#     default_filename <- if (!is.null(options$subtitle)) {
#       paste0(gsub("[^a-zA-Z0-9]", "_", options$subtitle), ".png")
#     } else if (!is.null(options$title)) {
#       paste0(gsub("[^a-zA-Z0-9]", "_", options$title), ".png")
#     } else {
#       "margot_plot.png"
#     }
#
#     # Use user-specified filename if provided, otherwise use default
#     filename <- if (!is.null(options$save_plot$filename)) {
#       options$save_plot$filename
#     } else {
#       default_filename
#     }
#
#     # Determine save path
#     save_path <- if (!is.null(options$push_mods)) {
#       file.path(options$push_mods, filename)
#     } else {
#       filename
#     }
#
#     # Create directory if it doesn't exist
#     if (!is.null(options$push_mods) && !dir.exists(options$push_mods)) {
#       dir.create(options$push_mods, recursive = TRUE)
#     }
#
#     # Save the plot
#     tryCatch({
#       ggsave(save_path, out,
#              width = options$save_plot$width,
#              height = options$save_plot$height,
#              dpi = options$save_plot$dpi)
#       message(paste("Plot saved to:", normalizePath(save_path)))
#     }, error = function(e) {
#       message(paste("Error saving plot:", e$message))
#     })
#   } else if (isFALSE(options$save_plot)) {
#     message("Plot was not saved as per user request.")
#   } else {
#     message("Plot was not saved. To save the plot, provide a 'save_plot' option or set it to FALSE to suppress this message.")
#   }
#
#   return(out)
# }
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         order = c("default", "alphabetical"),
#                         title = NULL,
#                         subtitle = NULL,
#                         estimate_scale = 1,
#                         base_size = 11,
#                         text_size = 2.75,
#                         point_size = 3,
#                         title_size = 10,
#                         subtitle_size = 9,
#                         legend_text_size = 6,
#                         legend_title_size = 6,
#                         x_offset = ifelse(type == "RR", 0, -1.75),
#                         x_lim_lo = ifelse(type == "RR", .1, -1.75),
#                         x_lim_hi = ifelse(type == "RR", 2.5, 1),
#                         linewidth = .5,
#                         plot_theme = NULL) {
#   require("ggplot2")
#   require("dplyr")
#
#   type <- match.arg(type)
#   order <- match.arg(order)
#
#   # Check if the data needs processing by group_tab
#   if (!"Estimate" %in% names(.data) || !"outcome" %in% names(.data)) {
#     .data <- group_tab(.data, type = type, order = order)
#   }
#
#   # Dynamic theme adjustment
#   if (is.null(plot_theme)) {
#     plot_theme <- theme_classic(base_size = base_size)
#   } else {
#     plot_theme <- plot_theme + theme(text = element_text(size = base_size))
#   }
#
#   # Prepare the data for plotting, including ordering
#   effect_size_col <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
#   .data <- .data %>%
#     mutate(outcome = factor(outcome, levels = if (order == "alphabetical") sort(unique(outcome)) else unique(outcome))) %>%
#     arrange(if (order == "alphabetical") outcome else desc(!!sym(effect_size_col)))
#
#   # Start building the plot
#   out <- ggplot(
#     data = .data,
#     aes(
#       y = outcome,
#       x = !!sym(effect_size_col),
#       xmin = `2.5 %`,
#       xmax = `97.5 %`,
#       color = Estimate
#     )
#   ) + geom_errorbarh(aes(color = Estimate), height = .3,
#                      linewidth = linewidth, position = position_dodge(width = .3)) +
#     geom_point(size = point_size, position = position_dodge(width = 0.3)) +
#     geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
#     scale_color_manual(values = c("positive" = "dodgerblue", "not reliable" = "black", "negative" = "orange")) +
#     labs(
#       x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
#       y = NULL,
#       title = ifelse(is.null(title), "", title),
#       subtitle = ifelse(is.null(subtitle), "", subtitle)
#     ) +
#     geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = "bold") +
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
#   # Conditionally add x-axis scale modifications for RR
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + scale_x_continuous(labels = custom_x_labels)
#   }
#
#   return(out)
# }
