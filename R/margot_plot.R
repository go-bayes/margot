#' Create a Margot Plot with Interpretation
#'
#' This function creates a Margot plot for visualising causal effects with flexible sorting,
#' embeds a compact interpretation, and returns a transformed table.
#'
#' @param .data A data frame with:
#'   - `outcome` column,
#'   - effect columns: `E[Y(1)]-E[Y(0)]` or `E[Y(1)]/E[Y(0)]`,
#'   - confidence limits: `2.5 %`, `97.5 %`,
#'   - `E_Value`, `E_Val_bound`, and optional `unit`.
#' @param type One of `"RD"` (Risk Difference) or `"RR"` (Risk Ratio). Default: `"RD"`.
#' @param order Outcome sorting. Options:
#'   \describe{
#'     \item{alphabetical}{A–Z}
#'     \item{magnitude_desc}{|effect| descending (alias for old `"magnitude"`/`"default"`) }
#'     \item{magnitude_asc}{|effect| ascending}
#'     \item{evaluebound_desc}{E‑value bound descending}
#'     \item{evaluebound_asc}{E‑value bound ascending}
#'     \item{custom}{use `custom_order`}
#'     \item{default}{alias for `"magnitude_desc"` (deprecated)}
#'   }
#'   Default: `"alphabetical"`.
#' @param custom_order Character vector for `order = "custom"`.
#' @param title_binary Optional title for binary plots.
#' @param include_coefficients Logical; include effect values on plot. Default: `TRUE`.
#' @param standardize_label One of `"NZ"`, `"US"`, or `"none"`. Controls x-axis label. Default: `"NZ"`.
#' @param interpret_all_E_gt1 Logical; if `TRUE`, interprets all effects with `E_Value>1` & `E_Val_bound>1`. Default: `FALSE`.
#' @param ... Additional arguments passed to `options`.
#' @param options List of plot & label options (see details).
#' @param label_mapping Named vector mapping original to display labels.
#' @param save_output Logical; if `TRUE`, saves output via `here_save_qs()`. Default: `FALSE`.
#' @param use_timestamp Logical; append timestamp. Default: `FALSE`.
#' @param base_filename Base of output filename. Default: `"margot_plot_output"`.
#' @param prefix Optional filename prefix.
#' @param save_path Directory for saving. Default: `here::here("push_mods")`.
#' @param original_df Optional data frame for back-transformation to original scale.
#'
#' @details
#' Sorting is implemented via `group_tab()`. The `options` list can include:
#'   title, subtitle, base_size, text_size, point_size, colors, theme, facet_var,
#'   show_evalues, evalue_digits, remove_tx_prefix, remove_z_suffix,
#'   use_title_case, remove_underscores.
#'
#' @return A list with:
#'   \itemize{
#'     \item `plot`: a `ggplot2` object
#'     \item `interpretation`: markdown string
#'     \item `transformed_table`: input `.data` with transformed rownames
#'   }
#'
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline
#'   scale_color_manual labs coord_cartesian theme_classic theme element_text
#'   margin position_dodge geom_text facet_wrap scale_x_continuous
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @export
margot_plot <- function(
    .data,
    type = c("RD","RR"),
    order = c(
      "alphabetical",
      "magnitude_desc",
      "magnitude_asc",
      "evaluebound_desc",
      "evaluebound_asc",
      "custom",
      "default"
    ),
    custom_order = NULL,
    title_binary = NULL,
    include_coefficients = TRUE,
    standardize_label = c("NZ","US","none"),
    interpret_all_E_gt1 = FALSE,
    ...,
    options = list(),
    label_mapping = NULL,
    save_output = FALSE,
    use_timestamp = FALSE,
    base_filename = "margot_plot_output",
    prefix = NULL,
    save_path = here::here("push_mods"),
    original_df = NULL
) {
  # match arguments
  type <- match.arg(type)
  order <- match.arg(order)
  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }
  standardize_label <- match.arg(standardize_label)

  # keep a copy for transformed_table
  .data_for_table <- .data

  # merge options
  default_opts <- list(
    title = NULL, subtitle = NULL, estimate_scale = 1,
    base_size = 18, text_size = 2.75, point_size = 3,
    title_size = 20, subtitle_size = 18,
    legend_text_size = 10, legend_title_size = 10,
    x_offset = NULL, x_lim_lo = NULL, x_lim_hi = NULL,
    linewidth = 0.4, plot_theme = NULL,
    colors = c("positive"="#E69F00","not reliable"="black","negative"="#56B4E9"),
    facet_var = NULL, confidence_level = 0.95,
    annotations = NULL, show_evalues = TRUE, evalue_digits = 2,
    remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
    use_title_case = TRUE, remove_underscores = TRUE
  )
  opts <- modifyList(modifyList(default_opts, options), list(...))
  # coerce logicals
  for (nm in c("remove_tx_prefix","remove_z_suffix","use_title_case","remove_underscores")) {
    opts[[nm]] <- as.logical(opts[[nm]])[1]
  }

  # effect column
  eff_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data))
    "E[Y(1)]-E[Y(0)]" else "E[Y(1)]/E[Y(0)]"

  # ensure outcome column
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    message("Added 'outcome' column.")
  }

  # back-transform if needed
  if (!is.null(original_df)) {
    .data <- back_transform_estimates(.data, original_df)
  }

  # map & transform labels
  .data$outcome <- sapply(.data$outcome, transform_label, label_mapping, opts)

  # sort via group_tab
  .data <- group_tab(.data, type = type, order = order, custom_order = custom_order)
  # lock factor levels
  .data$outcome <- factor(.data$outcome, levels = .data$outcome)

  # compute Estimate category
  null_val <- ifelse(type=="RR", 1, 0)
  .data <- .data %>%
    dplyr::mutate(
      Estimate = dplyr::case_when(
        (`2.5 %` > null_val & `97.5 %` > null_val) ~ "positive",
        (`2.5 %` < null_val & `97.5 %` < null_val) ~ "negative",
        TRUE ~ "not reliable"
      )
    )

  # x-axis label
  lw <- switch(standardize_label, NZ="Standardised", US="Standardized", none="Effect")
  xlab <- if (type=="RR") "Effect (Risk Ratio)" else
    if (lw!="Effect") paste0(lw," Effect (Difference Scale)") else "Effect (Difference Scale)"

  # build plot
  out <- ggplot2::ggplot(.data, ggplot2::aes(
    y = outcome, x = !!rlang::sym(eff_col),
    xmin = `2.5 %`, xmax = `97.5 %`, color = Estimate
  )) +
    ggplot2::geom_errorbarh(height = 0.3, linewidth = opts$linewidth,
                            position = ggplot2::position_dodge(0.3)) +
    ggplot2::geom_point(size = opts$point_size,
                        position = ggplot2::position_dodge(0.3)) +
    ggplot2::geom_vline(xintercept = null_val) +
    ggplot2::scale_color_manual(values = opts$colors) +
    ggplot2::labs(x = xlab, title = opts$title, subtitle = opts$subtitle) +
    ggplot2::coord_cartesian(xlim = c(opts$x_lim_lo, opts$x_lim_hi)) +
    ggplot2::theme_classic(base_size = opts$base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = opts$title_size)
    )

  if (type == "RR") {
    out <- out + ggplot2::scale_x_continuous(
      labels = function(x) ifelse(x < 0, "", as.character(x))
    )
  }

  if (include_coefficients) {
    out <- out + ggplot2::geom_text(
      ggplot2::aes(
        x = !!rlang::sym(eff_col) + opts$x_offset * opts$estimate_scale,
        label = sprintf("%.2f", !!rlang::sym(eff_col))
      ),
      size = opts$text_size, hjust = ifelse(type=="RR",0,1), fontface = "bold"
    )
  }

  # generate interpretation
  interpretation <- margot_interpret_marginal(
    df = .data, type = type, order = order,
    original_df = original_df, interpret_all_E_gt1 = interpret_all_E_gt1
  )$interpretation

  # --- after building `out` and computing `interpretation` ---

  # 1) first transform the row names as before
  transformed_table <- transform_table_rownames(
    .data_for_table,
    label_mapping = label_mapping,
    options       = opts
  )

  # 2) capture the plotting order from the locked‐factor levels
  ordered_labels <- as.character(.data$outcome)

  # 3) reorder the table to match that exact order
  transformed_table <- transformed_table[
    match(ordered_labels, rownames(transformed_table)),
    ,
    drop = FALSE
  ]

  # 4) return the three elements
  list(
    plot              = out,
    interpretation    = interpretation,
    transformed_table = transformed_table
  )
}
# old
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         order = c("alphabetical", "magnitude", "custom", "default"),
#                         title_binary = NULL,
#                         include_coefficients = TRUE,
#                         standardize_label = c("NZ", "US", "none"),
#                         interpret_all_E_gt1 = TRUE,  # NEW parameter
#                         ...,
#                         options = list(),
#                         label_mapping = NULL,
#                         save_output = FALSE,
#                         use_timestamp = FALSE,
#                         base_filename = "margot_plot_output",
#                         prefix = NULL,
#                         save_path = here::here("push_mods"),
#                         original_df = NULL) {
#
#   # Set default type to "RD"
#   type <- match.arg(type, choices = c("RD", "RR"), several.ok = FALSE)
#
#   # Set default order to "alphabetical"
#   order <- match.arg(order, choices = c("alphabetical", "magnitude", "custom", "default"))
#
#   # Deprecate "default" order with a warning
#   if (order == "default") {
#     warning("'default' order is deprecated. Please use 'magnitude' instead.")
#     order <- "magnitude"
#   }
#
#   # Match argument for standardise label
#   standardize_label <- match.arg(standardize_label, c("NZ", "US", "none"))
#
#   # Create a copy of the original data for table transformation
#   .data_for_table <- .data
#
#   # Capture additional arguments
#   additional_args <- list(...)
#
#   # Default values for options
#   default_options <- list(
#     title = NULL,
#     subtitle = NULL,
#     estimate_scale = 1,
#     base_size = 18,
#     text_size = 2.75,
#     point_size = 3,
#     title_size = 20,
#     subtitle_size = 18,
#     legend_text_size = 10,
#     legend_title_size = 10,
#     x_offset = NULL,   # will be set based on type
#     x_lim_lo = NULL,   # will be set based on type
#     x_lim_hi = NULL,   # will be set based on type
#     linewidth = 0.4,
#     plot_theme = NULL,
#     colors = c("positive" = "#E69F00", "not reliable" = "black", "negative" = "#56B4E9"),
#     facet_var = NULL,
#     confidence_level = 0.95,
#     annotations = NULL,
#     show_evalues = TRUE,
#     evalue_digits = 2,
#     # Label transformation options
#     remove_tx_prefix = TRUE,
#     remove_z_suffix = TRUE,
#     use_title_case = TRUE,
#     remove_underscores = TRUE
#   )
#
#   # Merge user-provided options with defaults and additional arguments
#   options <- modifyList(modifyList(default_options, options), additional_args)
#
#   # Input validation for data
#   if (!is.data.frame(.data)) {
#     stop("Input must be a data frame.")
#   }
#
#   # Validate and coerce label transformation options to single logical values
#   for (opt in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores")) {
#     options[[opt]] <- as.logical(options[[opt]])[1]
#     if (!is.logical(options[[opt]]) || length(options[[opt]]) != 1) {
#       stop(sprintf("`%s` must be a single logical value (TRUE or FALSE).", opt))
#     }
#   }
#
#   # Determine the effect size column based on the data structure
#   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else if ("E[Y(1)]/E[Y(0)]" %in% names(.data)) {
#     "E[Y(1)]/E[Y(0)]"
#   } else {
#     stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column.")
#   }
#
#   # Ensure .data has an 'outcome' column
#   if (!"outcome" %in% names(.data)) {
#     .data$outcome <- rownames(.data)
#     message("Added 'outcome' column based on row names.")
#   }
#
#   # Store original variable names before any label transformations
#   if (!"original_var_name" %in% names(.data)) {
#     .data$original_var_name <- .data$outcome
#   }
#
#   # Transform to original scale if original_df is provided
#   if (!is.null(original_df)) {
#     .data <- back_transform_estimates(.data, original_df)
#   } else {
#     message("No original_df provided. Results will be on the transformed scale.")
#   }
#
#   # Apply transformations to outcome labels
#   .data$outcome <- sapply(.data$outcome, transform_label,
#                           label_mapping = label_mapping,
#                           options = options)
#
#   # Prepare the data for plotting, including ordering
#   .data <- group_tab(.data, type = type, order = order)
#
#   # Set type-dependent options if not provided
#   if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
#   if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
#   if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)
#
#   # Adjust 'Estimate' based on whether the confidence interval crosses the null value
#   null_value <- ifelse(type == "RR", 1, 0)
#
#   .data <- .data %>%
#     dplyr::mutate(
#       Estimate = dplyr::case_when(
#         (`2.5 %` > null_value & `97.5 %` > null_value) ~ "positive",
#         (`2.5 %` < null_value & `97.5 %` < null_value) ~ "negative",
#         TRUE ~ "not reliable"
#       )
#     )
#
#   # Build x-axis label conditionally
#   label_word <- switch(
#     standardize_label,
#     NZ   = "Standardised",
#     US   = "Standardized",
#     none = "Effect"
#   )
#   x_axis_label <- if (type == "RR") {
#     paste0("Effect (Risk Ratio Scale)")
#   } else {
#     if (label_word %in% c("Standardised", "Standardized")) {
#       paste0(label_word, " Effect (Difference Scale)")
#     } else {
#       paste0(label_word, " (Difference Scale)")
#     }
#   }
#
#   # Start building the plot
#   out <- ggplot2::ggplot(
#     data = .data,
#     ggplot2::aes(
#       y = outcome,
#       x = !!rlang::sym(effect_size_col),
#       xmin = `2.5 %`,
#       xmax = `97.5 %`,
#       color = Estimate
#     )
#   ) +
#     ggplot2::geom_errorbarh(ggplot2::aes(color = Estimate),
#                             height = 0.3,
#                             linewidth = options$linewidth,
#                             position = ggplot2::position_dodge(width = 0.3)
#     ) +
#     ggplot2::geom_point(size = options$point_size,
#                         position = ggplot2::position_dodge(width = 0.3)) +
#     ggplot2::geom_vline(xintercept = null_value, linetype = "solid") +
#     ggplot2::scale_color_manual(values = options$colors) +
#     ggplot2::labs(
#       x = x_axis_label,
#       y = NULL,
#       title = options$title,
#       subtitle = options$subtitle
#     ) +
#     ggplot2::coord_cartesian(xlim = c(options$x_lim_lo, options$x_lim_hi)) +
#     ggplot2::theme_classic(base_size = options$base_size) +
#     ggplot2::theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       axis.ticks.x = ggplot2::element_blank(),
#       axis.ticks.y = ggplot2::element_blank(),
#       plot.title = ggplot2::element_text(face = "bold", size = options$title_size, hjust = 0),
#       plot.subtitle = ggplot2::element_text(face = "bold", size = options$subtitle_size, hjust = 0),
#       legend.text = ggplot2::element_text(size = options$legend_text_size),
#       legend.title = ggplot2::element_text(size = options$legend_title_size),
#       plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#     )
#
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + ggplot2::scale_x_continuous(labels = custom_x_labels)
#   }
#
#   if (include_coefficients) {
#     out <- out + ggplot2::geom_text(
#       ggplot2::aes(
#         x = !!rlang::sym(effect_size_col) + options$x_offset * options$estimate_scale,
#         label = sprintf("%.2f", !!rlang::sym(effect_size_col))
#       ),
#       size = options$text_size,
#       hjust = ifelse(type == "RR", 0, 1),
#       fontface = "bold"
#     )
#   }
#
#   if (!is.null(options$facet_var)) {
#     out <- out + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(options$facet_var)),
#                                      scales = "free_y")
#   }
#
#   if (!is.null(options$annotations)) {
#     out <- out + options$annotations
#   }
#
#   # Generate interpretation using the marginal interpretation helper
#   interpretation_result <- margot_interpret_marginal(
#     df = .data,
#     type = type,
#     order = order,
#     original_df = original_df,
#     interpret_all_E_gt1 = interpret_all_E_gt1
#   )
#
#   interpretation_text <- interpretation_result$interpretation
#
#   # Transform table row names using the provided label mapping and options
#   transform_table_rownames <- function(df, label_mapping, options) {
#     rownames_vector <- rownames(df)
#     transformed_rownames <- sapply(rownames_vector,
#                                    transform_label,
#                                    label_mapping = label_mapping,
#                                    options = options)
#     rownames(df) <- transformed_rownames
#     return(df)
#   }
#
#   transformed_table <- transform_table_rownames(.data_for_table, label_mapping, options)
#
#   complete_output <- list(
#     plot = out,
#     interpretation = interpretation_text,
#     transformed_table = transformed_table
#   )
#
#   # Save output if requested
#   if (save_output) {
#     message("Saving complete output...")
#     tryCatch({
#       if (use_timestamp) {
#         output_filename <- paste0(
#           ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#           base_filename, "_",
#           format(Sys.time(), "%Y%m%d_%H%M%S")
#         )
#       } else {
#         output_filename <- paste0(
#           ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#           base_filename
#         )
#       }
#       margot::here_save_qs(
#         obj = complete_output,
#         name = output_filename,
#         dir_path = save_path,
#         preset = "high",
#         nthreads = 1
#       )
#       message("Complete output saved successfully.")
#     }, error = function(e) {
#       warning(paste("Failed to save complete output:", e$message))
#     })
#   } else {
#     message("Output was not saved as per user request.")
#   }
#
#   message("Margot plot analysis complete 👍")
#   return(complete_output)
# }
