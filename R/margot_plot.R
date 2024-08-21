#' Create a Margot Plot with Interpretation and Transformed Table
#'
#' This function creates a Margot plot, which is useful for visualizing causal effects
#' in epidemiological studies. It provides various options for customizing the plot
#' and transforming labels. Additionally, it generates an interpretation of the results
#' using the margot_interpret_marginal function and returns a transformed table.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param type Character string specifying the type of plot. Either "RD" (Risk Difference) or "RR" (Risk Ratio).
#' @param order Character string specifying the order of outcomes. Either "default" or "alphabetical".
#' @param title_binary Optional title for the plot.
#' @param push_mods Optional directory path for saving the plot.
#' @param ... Additional arguments passed to the plotting function.
#' @param options A list of additional options for customizing the plot and interpretation. See Details for available options.
#'
#' @details
#' The `options` list can include the following parameters:
#' \itemize{
#'   \item `remove_tx_prefix`: Logical. If TRUE, removes "tx_" prefix from labels and interpretation. Default is TRUE.
#'   \item `remove_z_suffix`: Logical. If TRUE, removes "_z" suffix from labels and interpretation. Default is TRUE.
#'   \item `use_title_case`: Logical. If TRUE, converts labels and interpretation to title case. Default is TRUE.
#'   \item `remove_underscores`: Logical. If TRUE, removes underscores from labels and interpretation. Default is TRUE.
#'   \item `save_plot`: Logical. If TRUE, saves the plot to a file. Default is FALSE.
#'   \item `save_plot_options`: List of options for saving the plot (width, height, dpi, filename).
#'   \item `show_evalues`: Logical. If TRUE, shows E-values in the plot. Default is TRUE.
#'   \item `evalue_digits`: Integer. Number of digits for E-value display. Default is 2.
#' }
#'
#' @return A list containing three elements:
#' \itemize{
#'   \item `plot`: A ggplot object representing the Margot plot.
#'   \item `interpretation`: A character string containing the interpretation of the results, with the same formatting applied as the plot labels.
#'   \item `transformed_table`: A data frame with the original data and transformed row names, using the same transformation options as the plot labels.
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sample_data <- data.frame(
#'   outcome = c("t1_outcome_a_z", "t2_outcome_b_z", "t3_outcome_c_z"),
#'   `E[Y(1)]-E[Y(0)]` = c(0.1, -0.2, 0.3),
#'   `2.5 %` = c(0.05, -0.3, 0.2),
#'   `97.5 %` = c(0.15, -0.1, 0.4),
#'   E_Value = c(1.5, 1.8, 2.0),
#'   E_Val_bound = c(1.3, 1.5, 1.7)
#' )
#'
#' # Create a basic Margot plot with interpretation and transformed table
#' result <- margot_plot(sample_data, type = "RD")
#' print(result$plot)
#' cat(result$interpretation)
#' print(result$transformed_table)
#'
#' # Create a Margot plot with custom options
#' custom_result <- margot_plot(sample_data,
#'                              type = "RD",
#'                              title_binary = "Custom Margot Plot",
#'                              options = list(
#'                                remove_tx_prefix = TRUE,
#'                                remove_z_suffix = TRUE,
#'                                use_title_case = TRUE,
#'                                remove_underscores = TRUE,
#'                                save_plot = TRUE,
#'                                save_plot_options = list(
#'                                  filename = "custom_margot_plot.png",
#'                                  width = 12,
#'                                  height = 8,
#'                                  dpi = 300
#'                                )
#'                              ))
#' print(custom_result$plot)
#' cat(custom_result$interpretation)
#' print(custom_result$transformed_table)
#' }
#'
#' @export
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("default", "alphabetical"),
                        title_binary = NULL,
                        push_mods = NULL,
                        ...,
                        options = list()) {

  # Load required packages
  require("ggplot2")
  require("dplyr")
  require("cli")

  # Create a copy of the original data for table transformation
  .data_for_table <- .data

  # Capture additional arguments
  additional_args <- list(...)

  # Default values
  default_options <- list(
    title = NULL,
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
    save_plot = FALSE,
    save_plot_options = list(
      width = 10,
      height = 6,
      dpi = 300,
      filename = NULL
    ),
    push_mods = push_mods,
    show_evalues = TRUE,
    evalue_digits = 2,
    # Label transformation options
    remove_tx_prefix = TRUE,
    remove_z_suffix = TRUE,
    use_title_case = TRUE,
    remove_underscores = TRUE
  )

  # Merge user-provided options with defaults and additional arguments
  options <- modifyList(modifyList(default_options, options), additional_args)

  # Input validation
  if (!is.data.frame(.data)) {
    cli::cli_abort("Input must be a data frame")
  }

  type <- match.arg(type)
  order <- match.arg(order)

  # Validate options
  for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores", "save_plot")) {
    if (!is.logical(options[[opt]])) {
      cli::cli_abort("{opt} must be a logical value (TRUE or FALSE)")
    }
  }

  # Determine the effect size column based on the data structure
  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
  }

  # Set type-dependent options if not provided
  if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
  if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
  if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)

  # Add row names as outcome column if it doesn't exist
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    cli::cli_alert_info("Added 'outcome' column based on row names")
  }

  # Function to transform labels
  transform_label <- function(label) {
    original_label <- label
    if (options$remove_tx_prefix) {
      label <- sub("^t[0-9]+_", "", label)
    }
    if (options$remove_z_suffix) {
      label <- sub("_z$", "", label)
    }
    if (options$remove_underscores) {
      label <- gsub("_", " ", label)
    }
    if (options$use_title_case) {
      label <- tools::toTitleCase(label)
    }
    if (label != original_label) {
      cli::cli_alert_info("Transformed label: {original_label} -> {label}")
    }
    return(label)
  }

  # Apply transformations to outcome labels
  .data$outcome <- sapply(.data$outcome, transform_label)

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

  # Generate interpretation using margot_interpret_marginal
  interpretation <- margot_interpret_marginal(
    df = .data,
    type = type,
    estimand = NULL,
    order = order
  )

  # Apply the same label transformations to the interpretation text
  transformed_interpretation <- interpretation$interpretation
  if (options$remove_tx_prefix) {
    transformed_interpretation <- gsub("t[0-9]+_", "", transformed_interpretation)
  }
  if (options$remove_z_suffix) {
    transformed_interpretation <- gsub("_z", "", transformed_interpretation)
  }
  if (options$remove_underscores) {
    transformed_interpretation <- gsub("_", " ", transformed_interpretation)
  }
  if (options$use_title_case) {
    transformed_interpretation <- tools::toTitleCase(transformed_interpretation)
  }


  # Transform table rownames
  transformed_table <- transform_table_rownames(
    .data_for_table,
    remove_tx_prefix = options$remove_tx_prefix,
    remove_z_suffix = options$remove_z_suffix,
    use_title_case = options$use_title_case,
    remove_underscores = options$remove_underscores
  )


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
      cli::cli_alert_success("Plot saved to: {normalizePath(save_path)} \U0001F44D")
    }, error = function(e) {
      cli::cli_alert_danger("Error saving plot: {e$message}")
    })
  } else {
    cli::cli_alert_info("Plot was not saved as per user request.")
  }

  cli::cli_alert_success("Margot plot created successfully \U0001F44D")

  # Return the plot, interpretation, and transformed table
  return(list(
    plot = out,
    interpretation = transformed_interpretation,
    transformed_table = transformed_table
  ))
}
# old
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         order = c("default", "alphabetical"),
#                         title_binary = NULL,
#                         push_mods = NULL,
#                         ...,
#                         options = list()) {
#
#   # Load required packages
#   require("ggplot2")
#   require("dplyr")
#   require("cli")
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
#     save_plot = FALSE,  # Changed default to FALSE
#     save_plot_options = list(
#       width = 10,
#       height = 6,
#       dpi = 300,
#       filename = NULL
#     ),
#     push_mods = push_mods,
#     show_evalues = TRUE,
#     evalue_digits = 2,
#     # Label transformation options
#     remove_tx_prefix = FALSE,
#     remove_z_suffix = FALSE,
#     use_title_case = FALSE,
#     remove_underscores = FALSE
#   )
#
#   # Merge user-provided options with defaults and additional arguments
#   options <- modifyList(modifyList(default_options, options), additional_args)
#
#   # Input validation
#   if (!is.data.frame(.data)) {
#     cli::cli_abort("Input must be a data frame")
#   }
#
#   type <- match.arg(type)
#   order <- match.arg(order)
#
#   # Validate options
#   for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores", "save_plot")) {
#     if (!is.logical(options[[opt]])) {
#       cli::cli_abort("{opt} must be a logical value (TRUE or FALSE)")
#     }
#   }
#
#   # Determine the effect size column based on the data structure
#   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]/E[Y(0)]"
#   } else {
#     cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
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
#     cli::cli_alert_info("Added 'outcome' column based on row names")
#   }
#
#   # Function to transform labels
#   transform_label <- function(label) {
#     original_label <- label
#     if (options$remove_tx_prefix) {
#       label <- sub("^t[0-9]+_", "", label)
#     }
#     if (options$remove_z_suffix) {
#       label <- sub("_z$", "", label)
#     }
#     if (options$remove_underscores) {
#       label <- gsub("_", " ", label)
#     }
#     if (options$use_title_case) {
#       label <- tools::toTitleCase(label)
#     }
#     if (label != original_label) {
#       cli::cli_alert_info("Transformed label: {original_label} -> {label}")
#     }
#     return(label)
#   }
#
#   # Apply transformations to outcome labels
#   .data$outcome <- sapply(.data$outcome, transform_label)
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
#   # Save plot logic
#   if (isTRUE(options$save_plot)) {
#     save_options <- options$save_plot_options
#
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
#     filename <- if (!is.null(save_options$filename)) {
#       save_options$filename
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
#              width = save_options$width,
#              height = save_options$height,
#              dpi = save_options$dpi)
#       cli::cli_alert_success("Plot saved to: {normalizePath(save_path)} \U0001F44D")
#     }, error = function(e) {
#       cli::cli_alert_danger("Error saving plot: {e$message}")
#     })
#   } else {
#     cli::cli_alert_info("Plot was not saved as per user request.")
#   }
#
#   cli::cli_alert_success("Margot plot created successfully \U0001F44D")
#   return(out)
# }
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
#     save_plot = TRUE,
#     save_plot_options = list(
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
#   # Save plot logic
#   if (isTRUE(options$save_plot)) {
#     save_options <- options$save_plot_options
#
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
#     filename <- if (!is.null(save_options$filename)) {
#       save_options$filename
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
#              width = save_options$width,
#              height = save_options$height,
#              dpi = save_options$dpi)
#       message(paste("Plot saved to:", normalizePath(save_path)))
#     }, error = function(e) {
#       message(paste("Error saving plot:", e$message))
#     })
#   } else {
#     message("Plot was not saved as per user request.")
#   }
#
#   return(out)
# }
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
