#' Create a Margot Plot with Interpretation and Transformed Table
#'
#' This function creates a Margot plot, which is useful for visualizing causal effects.
#' It provides various options for customizing the plot
#' and transforming labels. Additionally, it generates an interpretation of the results
#' and returns a transformed table.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param type Character string specifying the type of plot. Either "RD" (Risk Difference) or "RR" (Risk Ratio). Default is "RD".
#' @param order Character string specifying the order of outcomes. Either "default" or "alphabetical". Default is "default".
#' @param title_binary Optional title for the plot.
#' @param ... Additional arguments passed to the plotting function.
#' @param options A list of additional options for customizing the plot and interpretation. See Details for available options.
#' @param label_mapping A named list for custom outcome label mapping. See Details for usage.
#' @param save_output Logical. If TRUE, saves the complete output to a file. Default is FALSE.
#' @param use_timestamp Logical. If TRUE, adds a timestamp to the saved filename. Default is FALSE.
#' @param base_filename Character string. The base name for the saved file. Default is "margot_plot_output".
#' @param prefix Character string. An optional prefix for the saved filename. Default is NULL.
#' @param save_path Character string. The directory path where the output will be saved. Default is here::here("push_mods").
#' @param original_df Optional data frame containing the original (non-transformed) data for back-transformation of results.
#'
#' @details
#' The `options` list can include the following parameters:
#' \itemize{
#'   \item `title`: Character string. Main title for the plot.
#'   \item `subtitle`: Character string. Subtitle for the plot.
#'   \item `estimate_scale`: Numeric. Scaling factor for estimates. Default is 1.
#'   \item `base_size`: Numeric. Base font size for the plot. Default is 11.
#'   \item `text_size`: Numeric. Font size for text labels. Default is 2.75.
#'   \item `point_size`: Numeric. Size of points in the plot. Default is 3.
#'   \item `title_size`: Numeric. Font size for the main title. Default is 10.
#'   \item `subtitle_size`: Numeric. Font size for the subtitle. Default is 9.
#'   \item `legend_text_size`: Numeric. Font size for legend text. Default is 6.
#'   \item `legend_title_size`: Numeric. Font size for legend title. Default is 6.
#'   \item `linewidth`: Numeric. Width of lines in the plot. Default is 0.5.
#'   \item `plot_theme`: ggplot2 theme object. Custom theme for the plot.
#'   \item `colors`: Named vector. Custom colors for different estimate categories.
#'   \item `facet_var`: Character string. Variable name for faceting the plot.
#'   \item `confidence_level`: Numeric. Confidence level for intervals. Default is 0.95.
#'   \item `annotations`: ggplot2 layer. Custom annotations to add to the plot.
#'   \item `show_evalues`: Logical. If TRUE, shows E-values in the plot. Default is TRUE.
#'   \item `evalue_digits`: Integer. Number of digits for E-value display. Default is 2.
#'   \item `remove_tx_prefix`: Logical. If TRUE, removes "tx_" prefix from labels and interpretation. Default is TRUE.
#'   \item `remove_z_suffix`: Logical. If TRUE, removes "_z" suffix from labels and interpretation. Default is TRUE.
#'   \item `use_title_case`: Logical. If TRUE, converts labels and interpretation to title case. Default is TRUE.
#'   \item `remove_underscores`: Logical. If TRUE, removes underscores from labels and interpretation. Default is TRUE.
#' }
#'
#' The `label_mapping` parameter allows for custom renaming of specific outcomes:
#' \itemize{
#'   \item It should be a named list where names are original outcome labels and values are new labels.
#'   \item Outcomes not specified in `label_mapping` will use default transformations based on `options`.
#'   \item Custom mapped labels are used as-is, without applying default transformations.
#' }
#'
#' @return A list containing three elements:
#' \itemize{
#'   \item `plot`: A ggplot object representing the Margot plot.
#'   \item `interpretation`: A character string containing the interpretation of the results, with the same formatting applied as the plot labels.
#'   \item `transformed_table`: A data frame with the original data and transformed row names, using the same transformation options as the plot labels.
#' }
#'
#' If `save_output` is TRUE, the complete output will be saved to a file using margot::here_save_qs().
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
#' # Create a Margot plot with custom options, label mapping, and save output
#' custom_result <- margot_plot(sample_data,
#'   type = "RD",
#'   options = list(
#'     title = "Custom Margot Plot",
#'     subtitle = "With custom options",
#'     remove_tx_prefix = TRUE,
#'     remove_z_suffix = TRUE,
#'     use_title_case = TRUE,
#'     remove_underscores = TRUE,
#'     colors = c("positive" = "green", "not reliable" = "gray", "negative" = "red")
#'   ),
#'   label_mapping = list(
#'     "t1_outcome_a_z" = "Custom Label A",
#'     "t2_outcome_b_z" = "Custom Label B"
#'   ),
#'   save_output = TRUE,
#'   use_timestamp = TRUE,
#'   prefix = "custom",
#'   save_path = here::here("output", "margot_plots")
#' )
#' print(custom_result$plot)
#' cat(custom_result$interpretation)
#' print(custom_result$transformed_table)
#'
#' # Create a Margot plot with original data for back-transformation
#' original_data <- data.frame(
#'   t1_outcome_a = rnorm(100),
#'   t2_outcome_b = rnorm(100),
#'   t3_outcome_c = rnorm(100)
#' )
#' result_with_original <- margot_plot(sample_data,
#'   type = "RD",
#'   original_df = original_data
#' )
#' print(result_with_original$plot)
#' cat(result_with_original$interpretation)
#' print(result_with_original$transformed_table)
#' }
#'
#' @export
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("default", "alphabetical"),
                        title_binary = NULL,
                        ...,
                        options = list(),
                        label_mapping = NULL,
                        save_output = FALSE,
                        use_timestamp = FALSE,
                        base_filename = "margot_plot_output",
                        prefix = NULL,
                        save_path = here::here("push_mods"),
                        original_df = NULL) {
  # Set default type to "RD"
  type <- match.arg(type, c("RD", "RR"), several.ok = FALSE)

  # Create a copy of the original data for table transformation
  .data_for_table <- .data

  # Capture additional arguments
  additional_args <- list(...)

  # Default values
  default_options <- list(
    title = NULL,
    subtitle = NULL,
    estimate_scale = 1,
    base_size = 18,
    text_size = 2.75,
    point_size = 3,
    title_size = 20,
    subtitle_size = 18,
    legend_text_size = 10,
    legend_title_size = 10,
    x_offset = NULL, # will be set based on type
    x_lim_lo = NULL, # will be set based on type
    x_lim_hi = NULL, # will be set based on type
    linewidth = 0.4,
    plot_theme = NULL,
    colors = c("positive" = "#E69F00", "not reliable" = "black", "negative" = "#56B4E9"),
    facet_var = NULL,
    confidence_level = 0.95,
    annotations = NULL,
    show_evalues = TRUE,
    evalue_digits = 2,
    # Label transformation options
    remove_tx_prefix = TRUE,
    remove_z_suffix = TRUE,
    use_title_case = FALSE,
    remove_underscores = TRUE
  )
  # Merge user-provided options with defaults and additional arguments
  options <- modifyList(modifyList(default_options, options), additional_args)

  # Input validation
  if (!is.data.frame(.data)) {
    cli::cli_abort("Input must be a data frame")
  }

  order <- match.arg(order)

  # Validate options
  for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores")) {
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

  # Transform to original scale if original_df is provided
  if (!is.null(original_df)) {
    transformed_data <- transform_to_original_scale(results_df = .data, original_df = original_df, label_mapping = label_mapping)
    .data <- transformed_data$results_df
    label_mapping <- transformed_data$label_mapping
  } else {
    cli::cli_alert_info("No original_df provided. Results will be on the transformed scale.")
  }

  # Ensure .data has an 'outcome' column
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    cli::cli_alert_info("Added 'outcome' column based on row names")
  }

  # Apply transformations to outcome labels
  .data$outcome <- sapply(.data$outcome, transform_label, label_mapping = label_mapping, options = options)

  # Prepare the data for plotting, including ordering
  .data <- group_tab(.data, type = type, order = order)

  # Set type-dependent options if not provided
  if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
  if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
  if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)

  # Start building the plot
  out <- ggplot2::ggplot(
    data = .data,
    ggplot2::aes(
      y = outcome,
      x = !!rlang::sym(effect_size_col),
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      color = Estimate
    )
  ) +
    ggplot2::geom_errorbarh(ggplot2::aes(color = Estimate),
                            height = .3,
                            linewidth = options$linewidth, position = ggplot2::position_dodge(width = .3)
    ) +
    ggplot2::geom_point(size = options$point_size, position = ggplot2::position_dodge(width = 0.3)) +
    ggplot2::geom_vline(xintercept = if (type == "RR") 1 else 0, linetype = "solid") +
    ggplot2::scale_color_manual(values = options$colors) +
    ggplot2::labs(
      x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
      y = NULL,
      title = options$title,
      subtitle = options$subtitle
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = options$x_offset * options$estimate_scale,
        label = label
      ),
      size = options$text_size, hjust = 0, fontface = "bold"
    ) +
    ggplot2::coord_cartesian(xlim = c(options$x_lim_lo, options$x_lim_hi)) +
    ggplot2::theme_classic(base_size = options$base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = options$title_size, hjust = 0),
      plot.subtitle = ggplot2::element_text(face = "bold", size = options$subtitle_size, hjust = 0),
      legend.text = ggplot2::element_text(size = options$legend_text_size),
      legend.title = ggplot2::element_text(size = options$legend_title_size),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

  # Conditionally add x-axis scale modifications for RR
  if (type == "RR") {
    custom_x_labels <- function(x) {
      ifelse(x < 0, "", as.character(x))
    }
    out <- out + ggplot2::scale_x_continuous(labels = custom_x_labels)
  }

  # Add faceting if specified
  if (!is.null(options$facet_var)) {
    out <- out + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(options$facet_var)), scales = "free_y")
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
  transform_table_rownames <- function(df, label_mapping, options) {
    rownames_vector <- rownames(df)
    transformed_rownames <- sapply(rownames_vector, transform_label, label_mapping = label_mapping, options = options)
    rownames(df) <- transformed_rownames
    return(df)
  }

  transformed_table <- transform_table_rownames(.data_for_table, label_mapping, options)

  # Create the complete output
  complete_output <- list(
    plot = out,
    interpretation = transformed_interpretation,
    transformed_table = transformed_table
  )

  # Handle saving logic
  if (save_output) {
    cli::cli_alert_info("Saving complete output...")
    tryCatch({
      if (use_timestamp) {
        output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
                                  base_filename, "_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"))
      } else {
        output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
                                  base_filename)
      }
      margot::here_save_qs(
        obj = complete_output,
        name = output_filename,
        dir_path = save_path,
        preset = "high",
        nthreads = 1
      )
      cli::cli_alert_success("Complete output saved successfully")
    }, error = function(e) {
      cli::cli_alert_danger(paste("Failed to save complete output:", e$message))
    })
  } else {
    cli::cli_alert_info("Output was not saved as per user request.")
  }

  cli::cli_alert_success("Margot plot analysis complete \U0001F44D")
  return(complete_output)
}
#' margot_plot <- function(.data,
#'                         type = c("RD", "RR"),
#'                         order = c("default", "alphabetical"),
#'                         title_binary = NULL,
#'                         ...,
#'                         options = list(),
#'                         label_mapping = NULL,
#'                         save_output = FALSE,
#'                         use_timestamp = FALSE,
#'                         base_filename = "margot_plot_output",
#'                         prefix = NULL,
#'                         save_path = here::here("push_mods"),
#'                         original_df = NULL) {
#'   # Set default type to "RD"
#'   type <- match.arg(type, c("RD", "RR"), several.ok = FALSE)
#'
#'   # Create a copy of the original data for table transformation
#'   .data_for_table <- .data
#'
#'   # Capture additional arguments
#'   additional_args <- list(...)
#'
#'   # Default values
#'   default_options <- list(
#'     title = NULL,
#'     subtitle = NULL,
#'     estimate_scale = 1,
#'     base_size = 18,
#'     text_size = 2.75,
#'     point_size = 3,
#'     title_size = 20,
#'     subtitle_size = 18,
#'     legend_text_size = 10,
#'     legend_title_size = 10,
#'     x_offset = NULL, # will be set based on type
#'     x_lim_lo = NULL, # will be set based on type
#'     x_lim_hi = NULL, # will be set based on type
#'     linewidth = 0.4,
#'     plot_theme = NULL,
#'     colors = c("positive" = "#E69F00", "not reliable" = "black", "negative" = "#56B4E9"),
#'     facet_var = NULL,
#'     confidence_level = 0.95,
#'     annotations = NULL,
#'     show_evalues = TRUE,
#'     evalue_digits = 2,
#'     # Label transformation options
#'     remove_tx_prefix = TRUE,
#'     remove_z_suffix = TRUE,
#'     use_title_case = FALSE,
#'     remove_underscores = TRUE
#'   )
#'   # Merge user-provided options with defaults and additional arguments
#'   options <- modifyList(modifyList(default_options, options), additional_args)
#'
#'   # Input validation
#'   if (!is.data.frame(.data)) {
#'     cli::cli_abort("Input must be a data frame")
#'   }
#'
#'   order <- match.arg(order)
#'
#'   # Validate options
#'   for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores")) {
#'     if (!is.logical(options[[opt]])) {
#'       cli::cli_abort("{opt} must be a logical value (TRUE or FALSE)")
#'     }
#'   }
#'
#'   # Determine the effect size column based on the data structure
#'   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
#'     "E[Y(1)]-E[Y(0)]"
#'   } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
#'     "E[Y(1)]/E[Y(0)]"
#'   } else {
#'     cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
#'   }
#'
#'   # Transform to original scale if original_df is provided
#'   if (!is.null(original_df)) {
#'     transformed_data <- transform_to_original_scale(results_df = .data, original_df = original_df, label_mapping = label_mapping)
#'     .data <- transformed_data$results_df
#'     label_mapping <- transformed_data$label_mapping
#'   } else {
#'     cli::cli_alert_info("No original_df provided. Results will be on the transformed scale.")
#'   }
#'
#'   # Ensure .data has an 'outcome' column
#'   if (!"outcome" %in% names(.data)) {
#'     .data$outcome <- rownames(.data)
#'     cli::cli_alert_info("Added 'outcome' column based on row names")
#'   }
#'
#'   # Apply transformations to outcome labels
#'   .data$outcome <- sapply(.data$outcome, transform_label, label_mapping = label_mapping, options = options)
#'
#'   # Prepare the data for plotting, including ordering
#'   .data <- group_tab(.data, type = type, order = order)
#'
#'   # Set type-dependent options if not provided
#'   if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
#'   if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
#'   if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)
#'
#'   # Start building the plot
#'   out <- ggplot2::ggplot(
#'     data = .data,
#'     ggplot2::aes(
#'       y = outcome,
#'       x = !!rlang::sym(effect_size_col),
#'       xmin = `2.5 %`,
#'       xmax = `97.5 %`,
#'       color = Estimate
#'     )
#'   ) +
#'     ggplot2::geom_errorbarh(ggplot2::aes(color = Estimate),
#'                             height = .3,
#'                             linewidth = options$linewidth, position = ggplot2::position_dodge(width = .3)
#'     ) +
#'     ggplot2::geom_point(size = options$point_size, position = ggplot2::position_dodge(width = 0.3)) +
#'     ggplot2::geom_vline(xintercept = if (type == "RR") 1 else 0, linetype = "solid") +
#'     ggplot2::scale_color_manual(values = options$colors) +
#'     ggplot2::labs(
#'       x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
#'       y = NULL,
#'       title = options$title,
#'       subtitle = options$subtitle
#'     ) +
#'     ggplot2::geom_text(
#'       ggplot2::aes(
#'         x = options$x_offset * options$estimate_scale,
#'         label = label
#'       ),
#'       size = options$text_size, hjust = 0, fontface = "bold"
#'     ) +
#'     ggplot2::coord_cartesian(xlim = c(options$x_lim_lo, options$x_lim_hi)) +
#'     ggplot2::theme_classic(base_size = options$base_size) +
#'     ggplot2::theme(
#'       legend.position = "top",
#'       legend.direction = "horizontal",
#'       axis.ticks.x = ggplot2::element_blank(),
#'       axis.ticks.y = ggplot2::element_blank(),
#'       plot.title = ggplot2::element_text(face = "bold", size = options$title_size, hjust = 0),
#'       plot.subtitle = ggplot2::element_text(face = "bold", size = options$subtitle_size, hjust = 0),
#'       legend.text = ggplot2::element_text(size = options$legend_text_size),
#'       legend.title = ggplot2::element_text(size = options$legend_title_size),
#'       plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#'     )
#'
#'   # Conditionally add x-axis scale modifications for RR
#'   if (type == "RR") {
#'     custom_x_labels <- function(x) {
#'       ifelse(x < 0, "", as.character(x))
#'     }
#'     out <- out + ggplot2::scale_x_continuous(labels = custom_x_labels)
#'   }
#'
#'   # Add faceting if specified
#'   if (!is.null(options$facet_var)) {
#'     out <- out + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(options$facet_var)), scales = "free_y")
#'   }
#'
#'   # Add custom annotations if provided
#'   if (!is.null(options$annotations)) {
#'     out <- out + options$annotations
#'   }
#'
#'   # Generate interpretation using margot_interpret_marginal
#'   interpretation <- margot_interpret_marginal(
#'     df = .data,
#'     type = type,
#'     estimand = NULL,
#'     order = order
#'   )
#'
#'   # Apply the same label transformations to the interpretation text
#'   transformed_interpretation <- interpretation$interpretation
#'   if (options$remove_tx_prefix) {
#'     transformed_interpretation <- gsub("t[0-9]+_", "", transformed_interpretation)
#'   }
#'   if (options$remove_z_suffix) {
#'     transformed_interpretation <- gsub("_z", "", transformed_interpretation)
#'   }
#'   if (options$remove_underscores) {
#'     transformed_interpretation <- gsub("_", " ", transformed_interpretation)
#'   }
#'   if (options$use_title_case) {
#'     transformed_interpretation <- tools::toTitleCase(transformed_interpretation)
#'   }
#'
#'   # Transform table rownames
#'   transform_table_rownames <- function(df, label_mapping, options) {
#'     rownames_vector <- rownames(df)
#'     transformed_rownames <- sapply(rownames_vector, transform_label, label_mapping = label_mapping, options = options)
#'     rownames(df) <- transformed_rownames
#'     return(df)
#'   }
#'
#'   transformed_table <- transform_table_rownames(.data_for_table, label_mapping, options)
#'
#'   # Create the complete output
#'   complete_output <- list(
#'     plot = out,
#'     interpretation = transformed_interpretation,
#'     transformed_table = transformed_table
#'   )
#'
#'   # Handle saving logic
#'   if (save_output) {
#'     cli::cli_alert_info("Saving complete output...")
#'     tryCatch({
#'       if (use_timestamp) {
#'         output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#'                                   base_filename, "_",
#'                                   format(Sys.time(), "%Y%m%d_%H%M%S"))
#'       } else {
#'         output_filename <- paste0(ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#'                                   base_filename)
#'       }
#'       margot::here_save_qs(
#'         obj = complete_output,
#'         name = output_filename,
#'         dir_path = save_path,
#'         preset = "high",
#'         nthreads = 1
#'       )
#'       cli::cli_alert_success("Complete output saved successfully")
#'     }, error = function(e) {
#'       cli::cli_alert_danger(paste("Failed to save complete output:", e$message))
#'     })
#'   } else {
#'     cli::cli_alert_info("Output was not saved as per user request.")
#'   }
#'
#'   cli::cli_alert_success("Margot plot analysis complete \U0001F44D")
#'   return(complete_output)
#' }
#'
#'
#' # Helper function for label transformation
#' #' @keywords internal
#' transform_label <- function(label, label_mapping = NULL, options = list()) {
#'   original_label <- label
#'
#'   # Apply mapping with partial substitutions and remove numbers
#'   if (!is.null(label_mapping)) {
#'     for (pattern in names(label_mapping)) {
#'       if (grepl(pattern, label, fixed = TRUE)) {
#'         replacement <- label_mapping[[pattern]]
#'         label <- gsub(pattern, replacement, label, fixed = TRUE)
#'         cli::cli_alert_info("Mapped label: {pattern} -> {replacement}")
#'       }
#'     }
#'   }
#'
#'   # Remove the numerical part (e.g., " - (3.0,7.0] - [1.0,2.0]")
#'   label <- sub(" - \\(.*\\]$", "", label)
#'
#'   # Apply default transformations if the label wasn't fully replaced
#'   if (label == original_label) {
#'     if (options$remove_tx_prefix) {
#'       label <- sub("^t[0-9]+_", "", label)
#'     }
#'     if (options$remove_z_suffix) {
#'       label <- sub("_z$", "", label)
#'     }
#'     if (options$remove_underscores) {
#'       label <- gsub("_", " ", label)
#'     }
#'     if (options$use_title_case) {
#'       label <- tools::toTitleCase(label)
#'       # Preserve "NZ" capitalization
#'       label <- gsub("Nz", "NZ", label)
#'     }
#'   }
#'
#'   if (label != original_label) {
#'     cli::cli_alert_info("Transformed label: {original_label} -> {label}")
#'   }
#'
#'   return(label)
#' }
#' Transform Z-Score Results to Original Scale
#' #'
#' #' This function takes a dataframe of z-score results and transforms them back to the original scale
#' #' using the standard deviations from the original dataset. It is particularly useful for
#' #' interpreting causal contrasts that were calculated on z-transformed data.
#' #'
#' #' @param results_df A dataframe containing the z-score results.
#' #' @param original_df A dataframe containing the original (non-transformed) data. Column names
#' #'   should match those in results_df or be mappable via label_mapping.
#' #' @param label_mapping An optional named list mapping row names in results_df to column names
#' #'   in original_df. Use this if the names do not match exactly.
#' #'
#' #' @return A dataframe similar to results_df, with additional columns for the back-transformed
#' #'   estimates and confidence intervals (suffixed with "_original").
#' #'
#' #' @keywords internal
#' transform_to_original_scale <- function(results_df, original_df, label_mapping = NULL) {
#'   cli::cli_alert_info("Starting transformation to original scale...")
#'
#'   # If label_mapping is provided, create a reverse mapping
#'   reverse_mapping <- if (!is.null(label_mapping)) {
#'     setNames(names(label_mapping), unlist(label_mapping))
#'   } else {
#'     NULL
#'   }
#'
#'   if (!is.null(reverse_mapping)) {
#'     cli::cli_alert_info("Reverse mapping: {paste(names(reverse_mapping), reverse_mapping, sep = ' -> ', collapse = ', ')}")
#'   }
#'
#'   # Function to get the original column name
#'   get_original_col <- function(row_name) {
#'     if (!is.null(reverse_mapping) && row_name %in% names(reverse_mapping)) {
#'       return(reverse_mapping[[row_name]])
#'     }
#'     return(row_name)
#'   }
#'
#'   # Calculate standard deviations for all columns in the original dataframe
#'   sds <- sapply(original_df, function(x) sd(x, na.rm = TRUE))
#'
#'   for (i in 1:nrow(results_df)) {
#'     row_name <- rownames(results_df)[i]
#'     cli::cli_alert_info("Processing row: {row_name}")
#'
#'     original_col <- get_original_col(row_name)
#'     # Remove "_z" suffix if present
#'     full_col <- sub("_z$", "", original_col)
#'
#'     if (full_col %in% names(original_df)) {
#'       cli::cli_alert_success("Found matching column in original data: {full_col} \U0001F44D")
#'
#'       sd_original <- sds[full_col]
#'       cli::cli_alert_info("Original data standard deviation for {full_col}: sd = {sd_original}")
#'
#'       # Back-transform the estimate and confidence intervals
#'       results_df[i, "E[Y(1)]-E[Y(0)]_original"] <- results_df[i, "E[Y(1)]-E[Y(0)]"] * sd_original
#'       results_df[i, "2.5 %_original"] <- results_df[i, "2.5 %"] * sd_original
#'       results_df[i, "97.5 %_original"] <- results_df[i, "97.5 %"] * sd_original
#'
#'       cli::cli_alert_success("Transformation complete for {row_name} \U0001F44D")
#'     } else {
#'       cli::cli_alert_warning("No matching column found in original data for {full_col}")
#'     }
#'   }
#'
#'   cli::cli_alert_success("Transformation to original scale completed \U0001F44D")
#'   return(list(results_df = results_df, label_mapping = label_mapping))
#' }

