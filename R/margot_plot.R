#' Create a Margot Plot with Interpretation
#'
#' This function creates a Margot plot, which is useful for visualizing causal effects.
#' It provides various options for customizing the plot
#' and transforming labels. Additionally, it generates a compact interpretation of the results
#' and returns a transformed table.
#'
#' @param .data A data frame containing the data to be plotted.
#'   It must include an `outcome` column and either `E[Y(1)]-E[Y(0)]`
#'   or `E[Y(1)]/E[Y(0)]` columns representing the causal estimates.
#' @param type Character string specifying the type of plot.
#'   Either `"RD"` (Risk Difference) or `"RR"` (Risk Ratio). Default is `"RD"`.
#' @param order Character string specifying the order of outcomes.
#'   Can be `"alphabetical"`, `"magnitude"`, or `"custom"`.
#'   - `"alphabetical"`: Orders outcomes alphabetically.
#'   - `"magnitude"`: Orders outcomes by the absolute magnitude of the effect size in descending order.
#'   - `"custom"`: Allows for a custom ordering (requires additional implementation).
#'   - `"default"`: Deprecated. Uses `"magnitude"` ordering and issues a warning.
#'   Default is `"alphabetical"`.
#' @param title_binary Optional title for the plot. If not provided, the title from `options` is used.
#' @param ... Additional arguments passed to the plotting function, allowing further customization.
#' @param options A list of additional options for customizing the plot.
#'   See **Details** for available options.
#' @param label_mapping A named list for custom outcome label mapping.
#'   See **Details** for usage.
#' @param save_output Logical. If `TRUE`, saves the complete output to a file. Default is `FALSE`.
#' @param use_timestamp Logical. If `TRUE`, adds a timestamp to the saved filename. Default is `FALSE`.
#' @param base_filename Character string. The base name for the saved file. Default is `"margot_plot_output"`.
#' @param prefix Character string. An optional prefix for the saved filename. Default is `NULL`.
#' @param save_path Character string. The directory path where the output will be saved.
#'   Default is `here::here("push_mods")`.
#' @param original_df Optional data frame containing the original (non-transformed) data
#'   for back-transformation of results. If provided, it should correspond to `.data` before any transformations.
#'
#' @details
#' The `options` list can include the following parameters:
#' \itemize{
#'   \item `title`: \strong{Character string}. Main title for the plot.
#'   \item `subtitle`: \strong{Character string}. Subtitle for the plot.
#'   \item `estimate_scale`: \strong{Numeric}. Scaling factor for estimates. Default is `1`.
#'   \item `base_size`: \strong{Numeric}. Base font size for the plot. Default is `18`.
#'   \item `text_size`: \strong{Numeric}. Font size for text labels. Default is `2.75`.
#'   \item `point_size`: \strong{Numeric}. Size of points in the plot. Default is `3`.
#'   \item `title_size`: \strong{Numeric}. Font size for the main title. Default is `20`.
#'   \item `subtitle_size`: \strong{Numeric}. Font size for the subtitle. Default is `18`.
#'   \item `legend_text_size`: \strong{Numeric}. Font size for legend text. Default is `10`.
#'   \item `legend_title_size`: \strong{Numeric}. Font size for legend title. Default is `10`.
#'   \item `linewidth`: \strong{Numeric}. Width of lines in the plot. Default is `0.4`.
#'   \item `x_offset`: \strong{Numeric}. Horizontal offset for text labels on the plot.
#'     If `NULL`, it is set based on the `type` (`0` for "RR" and `-1.75` for "RD").
#'   \item `x_lim_lo`: \strong{Numeric}. Lower limit for the x-axis.
#'     If `NULL`, it is set based on the `type` (`0.1` for "RR" and `-1.75` for "RD").
#'   \item `x_lim_hi`: \strong{Numeric}. Upper limit for the x-axis.
#'     If `NULL`, it is set based on the `type` (`2.5` for "RR" and `1` for "RD").
#'   \item `plot_theme`: \strong{ggplot2 theme object}. Custom theme for the plot.
#'   \item `colors`: \strong{Named vector}. Custom colors for different estimate categories.
#'     Example: `c("positive" = "green", "not reliable" = "gray", "negative" = "red")`.
#'   \item `facet_var`: \strong{Character string}. Variable name for faceting the plot.
#'     Allows creating subplots based on a categorical variable.
#'   \item `confidence_level`: \strong{Numeric}. Confidence level for intervals. Default is `0.95`.
#'   \item `annotations`: \strong{ggplot2 layer}. Custom annotations to add to the plot, such as text or shapes.
#'   \item `show_evalues`: \strong{Logical}. If `TRUE`, shows E-values in the plot. Default is `TRUE`.
#'   \item `evalue_digits`: \strong{Integer}. Number of digits for E-value display. Default is `2`.
#'   \item `remove_tx_prefix`: \strong{Logical}. If `TRUE`, removes `"tx_"` prefix from labels and interpretation. Default is `TRUE`.
#'   \item `remove_z_suffix`: \strong{Logical}. If `TRUE`, removes `"_z"` suffix from labels and interpretation. Default is `TRUE`.
#'   \item `use_title_case`: \strong{Logical}. If `TRUE`, converts labels and interpretation to title case. Default is `TRUE`.
#'   \item `remove_underscores`: \strong{Logical}. If `TRUE`, removes underscores from labels and interpretation. Default is `TRUE`.
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
#'   \item `plot`: A `ggplot` object representing the Margot plot.
#'   \item `interpretation`: A character string containing the compact interpretation of the results.
#'   \item `transformed_table`: A data frame with the original data and transformed row names, using the same transformation options as the plot labels.
#' }
#'
#' If `save_output` is `TRUE`, the complete output will be saved to a file using `margot::here_save_qs()`.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence
#' @importFrom here here
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
#'   E_Val_bound = c(1.3, 1.5, 1.7),
#'   unit = c("unit1", "unit2", "unit3")  # Assuming there's a 'unit' column
#' )
#'
#' # Create a Margot plot with interpretation and transformed table
#' result <- margot_plot(
#'   .data = sample_data,
#'   type = "RD",
#'   order = "alphabetical",
#'   options = list(
#'     title = "Causal Effect Estimates",
#'     subtitle = "Risk Difference Scale",
#'     colors = c("positive" = "green", "not reliable" = "gray", "negative" = "red"),
#'     remove_tx_prefix = TRUE,
#'     remove_z_suffix = TRUE,
#'     use_title_case = TRUE,  # This will be overridden to enforce sentence case
#'     remove_underscores = TRUE
#'   ),
#'   label_mapping = list(
#'     "t1_outcome_a_z" = "Outcome A",
#'     "t2_outcome_b_z" = "Outcome B",
#'     "t3_outcome_c_z" = "Outcome C"
#'   ),
#'   save_output = FALSE  # Set to TRUE to save the output
#' )
#'
#' # Display the plot
#' print(result$plot)
#'
#' # Display the interpretation
#' cat(result$interpretation)
#'
#' # Display the transformed table
#' print(result$transformed_table)
#' }
#'
#' @export
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("alphabetical", "magnitude", "custom", "default"),
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
  # **Set default type to "RD"**
  type <- match.arg(type, choices = c("RD", "RR"), several.ok = FALSE)

  # **Set default order to "alphabetical"**
  order <- match.arg(order, choices = c("alphabetical", "magnitude", "custom", "default"))

  # Deprecate "default" order with a warning
  if (order == "default") {
    warning("'default' order is deprecated. Please use 'magnitude' instead.")
    order <- "magnitude"
  }

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
    use_title_case = TRUE,  # This will be overridden to enforce sentence case
    remove_underscores = TRUE
  )

  # Merge user-provided options with defaults and additional arguments
  options <- modifyList(modifyList(default_options, options), additional_args)

  # Input validation
  if (!is.data.frame(.data)) {
    stop("Input must be a data frame.")
  }

  # Validate options
  for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores")) {
    if (!is.logical(options[[opt]])) {
      stop(sprintf("`%s` must be a logical value (TRUE or FALSE).", opt))
    }
  }

  # Determine the effect size column based on the data structure
  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column.")
  }

  # Ensure .data has an 'outcome' column
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    message("Added 'outcome' column based on row names.")
  }

  # Store original variable names before any label transformations
  if (!"original_var_name" %in% names(.data)) {
    .data$original_var_name <- .data$outcome
  }

  # Transform to original scale if original_df is provided
  if (!is.null(original_df)) {
    .data <- back_transform_estimates(.data, original_df)
  } else {
    message("No original_df provided. Results will be on the transformed scale.")
  }

  # Apply transformations to outcome labels
  .data$outcome <- sapply(.data$outcome, transform_label, label_mapping = label_mapping, options = options)

  # Prepare the data for plotting, including ordering
  .data <- group_tab(.data, type = type, order = order)

  # **Set type-dependent options if not provided**
  if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
  if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
  if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)

  # **Adjust 'Estimate' based on whether the confidence interval crosses the null value**
  null_value <- ifelse(type == "RR", 1, 0)

  .data <- .data %>%
    dplyr::mutate(
      Estimate = dplyr::case_when(
        (`2.5 %` > null_value & `97.5 %` > null_value) ~ "positive",
        (`2.5 %` < null_value & `97.5 %` < null_value) ~ "negative",
        TRUE ~ "not reliable"
      )
    )

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
                            linewidth = options$linewidth,
                            position = ggplot2::position_dodge(width = .3)
    ) +
    ggplot2::geom_point(size = options$point_size, position = ggplot2::position_dodge(width = 0.3)) +
    ggplot2::geom_vline(xintercept = null_value, linetype = "solid") +
    ggplot2::scale_color_manual(values = options$colors) +
    ggplot2::labs(
      x = paste0("Causal ", ifelse(type == "RR", "Risk Ratio", "Risk Difference"), " Scale"),
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

  # Generate interpretation using margot_interpret_marginal()
  interpretation_result <- margot_interpret_marginal(
    df = .data,
    type = type,
    order = order,
    original_df = original_df
  )

  # Extract the interpretation text
  interpretation_text <- interpretation_result$interpretation

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
    interpretation = interpretation_text,  # For backward compatibility
    transformed_table = transformed_table
  )

  # Handle saving logic
  if (save_output) {
    message("Saving complete output...")
    tryCatch({
      if (use_timestamp) {
        output_filename <- paste0(
          ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
          base_filename, "_",
          format(Sys.time(), "%Y%m%d_%H%M%S")
        )
      } else {
        output_filename <- paste0(
          ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
          base_filename
        )
      }
      margot::here_save_qs(
        obj = complete_output,
        name = output_filename,
        dir_path = save_path,
        preset = "high",
        nthreads = 1
      )
      message("Complete output saved successfully.")
    }, error = function(e) {
      warning(paste("Failed to save complete output:", e$message))
    })
  } else {
    message("Output was not saved as per user request.")
  }

  message("Margot plot analysis complete 👍")
  return(complete_output)
}
