#' Interpret and Describe Causal Effect Estimates Using E-values
#'
#' This function interprets the output of causal effect analysis, providing textual descriptions
#' of causal effect estimates. It categorizes the strength of evidence for causality based on
#' E-values and confidence intervals, and generates a detailed interpretation of the effect
#' estimates according to specified types (i.e., "RD" for risk difference or "RR" for risk ratio)
#' and estimands. It now also includes interpretation of original scale results when available.
#'
#' @param df Data frame containing causal effect estimates, expected to include columns for
#' outcome names, effect estimates (either differences or ratios), confidence intervals,
#' E-values, and a summary estimate label. Can also be the list output from `transform_to_original_scale()`.devtools::install(".")
#' @param type Character string specifying the type of effect estimate. Must be either "RD"
#' (Risk Difference) or "RR" (Risk Ratio). Default is "RD".
#' @param estimand Optional character string indicating the type of causal estimand interpreted: "PATE"
#' (Population Average Treatment Effect), "ATE" (Average Treatment Effect), "ATT" (Average
#' Treatment Effect in the Treated), "CATE" (Conditional Average Treatment Effect), or "LMTP"
#' (Longitudinal Modified Treatment Policy). Default is NULL.
#' @param order Character string specifying the order of results. Default is "default".
#' @param original_df Optional data frame for back-transforming estimates to the original scale.
#'
#' @return A list containing two elements:
#'   \item{estimand_description}{A character string describing the specified estimand, or NULL if no estimand was provided.}
#'   \item{interpretation}{A character string containing a detailed interpretation of each outcome in `df`,
#'   including the causal contrast, E-values, the strength of evidence for causality, and original scale results if available.}
#'
#' @details
#' The function handles both transformed and original scale results. If original scale results
#' are available (indicated by the presence of columns with "_original" suffix), these will be included
#' in the interpretation. The strength of evidence for causality is categorized as follows:
#'
#' - **Strong evidence:** E-value lower bound > 2
#' - **Evidence:** E-value lower bound > 1.1 and <= 2
#' - **Weak evidence:** E-value lower bound > 1 and <= 1.1
#' - **Not reliable evidence:** E-value lower bound <= 1 or confidence interval includes null effect
#'
#' @examples
#' \dontrun{
#' # Assuming `group_tab_output` is the result from a causal analysis
#' result <- margot_interpret_marginal(group_tab_output, type = "RD", estimand = "ATE")
#' cat(result$estimand_description)
#' cat(result$interpretation)
#'
#' # Using Risk Ratio without specifying an estimand
#' result <- margot_interpret_marginal(group_tab_output, type = "RR")
#' cat(result$interpretation)
#'
#' # Using output from transform_to_original_scale()
#' transformed_data <- transform_to_original_scale(results_df, original_df, label_mapping)
#' result <- margot_interpret_marginal(transformed_data, type = "RD")
#' cat(result$interpretation)
#' }
#'
#' @importFrom dplyr case_when mutate rowwise ungroup if_else
#' @importFrom glue glue
#' @keywords internal
margot_interpret_marginal <- function(df, type = c("RD", "RR"), estimand = NULL, order = "default", original_df = NULL) {
  type <- match.arg(type)

  message("Starting interpretation of causal effect estimates...")

  # Define estimand descriptions
  estimand_description <- if (!is.null(estimand)) {
    dplyr::case_when(
      estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
      estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire population.",
      estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
      estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
      estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
      TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
    )
  } else {
    NULL
  }

  message("Processing and interpreting data...")

  # Process df via group_tab to ensure 'Estimate' variable is present
  df <- group_tab(df, type = type, order = order)

  # If original_df is provided, back-transform estimates
  if (!is.null(original_df)) {
    df <- back_transform_estimates(df, original_df)
  }

  # Determine the effect size column
  effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
    "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
    "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
  }

  # Determine if we have original scale results
  has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)

  # Define the null_value based on type
  null_value <- ifelse(type == "RR", 1, 0)

  interpretation <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Determine the strength of evidence based on E_Val_bound and CI
      evidence_strength = dplyr::case_when(
        (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ "**Strong evidence** for causality",
        (`2.5 %` > null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) | (`97.5 %` < null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) ~ "**Evidence** for causality",
        (`2.5 %` > null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) | (`97.5 %` < null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) ~ "**Weak evidence** for causality",
        TRUE ~ "**Not reliable evidence** for causality"
      ),
      # Units
      unit = ifelse(!is.na(unit) & unit != "", unit, ""),
      # Define estimate_lab
      estimate_lab = paste0(
        round(!!rlang::sym(effect_size_col), 3), " (",
        round(`2.5 %`, 3), ", ",
        round(`97.5 %`, 3), ")"
      ),
      # Define estimate_lab_original if available
      estimate_lab_original = if (has_original_scale) {
        if (unit != "") {
          paste0(
            round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
            round(`2.5 %_original`, 3), " to ",
            round(`97.5 %_original`, 3), " ", unit, ")"
          )
        } else {
          paste0(
            round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
            round(`2.5 %_original`, 3), ", ",
            round(`97.5 %_original`, 3), ")"
          )
        }
      } else {
        NA_character_
      },
      outcome_interpretation = glue::glue(
        "### {outcome}\n",
        "The effect estimate ({type}) is {estimate_lab}. ",
        "{if (!is.na(estimate_lab_original)) paste0('On the original data scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
        "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
        "{if (E_Val_bound > 1) paste0('At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of ', E_Val_bound, ' to negate the observed effect. Weaker confounding would not overturn it. ') else ''}",
        "{evidence_strength}."
      )
    ) %>%
    dplyr::ungroup()

  # Compile results
  interpretation_text <- paste(interpretation$outcome_interpretation, collapse = '\n\n')

  message("Interpretation completed successfully!")

  # Return results as a list
  return(list(
    estimand_description = estimand_description,
    interpretation = interpretation_text
  ))
}
#'
#'
#' #' Create a Margot Plot with Interpretation and Transformed Table
#' #'
#' #' This function creates a Margot plot, which is useful for visualizing causal effects.
#' #' It provides various options for customizing the plot
#' #' and transforming labels. Additionally, it generates an interpretation of the results
#' #' and returns a transformed table.
#' #'
#' #' @param .data A data frame containing the data to be plotted.
#' #'   It must include an `outcome` column and either `E[Y(1)]-E[Y(0)]`
#' #'   or `E[Y(1)]/E[Y(0)]` columns representing the causal estimates.
#' #' @param type Character string specifying the type of plot.
#' #'   Either `"RD"` (Risk Difference) or `"RR"` (Risk Ratio). Default is `"RD"`.
#' #' @param order Character string specifying the order of outcomes.
#' #'   Can be `"default"`, `"alphabetical"`, or `"custom"`.
#' #'   - `"default"`: Uses the default ordering based on the data.
#' #'   - `"alphabetical"`: Orders outcomes alphabetically.
#' #'   - `"custom"`: Allows for a custom ordering (requires additional implementation).
#' #'   Default is `"default"`.
#' #' @param title_binary Optional title for the plot. If not provided, the title from `options` is used.
#' #' @param ... Additional arguments passed to the plotting function, allowing further customization.
#' #' @param options A list of additional options for customizing the plot and interpretation.
#' #'   See **Details** for available options.
#' #' @param label_mapping A named list for custom outcome label mapping.
#' #'   See **Details** for usage.
#' #' @param save_output Logical. If `TRUE`, saves the complete output to a file. Default is `FALSE`.
#' #' @param use_timestamp Logical. If `TRUE`, adds a timestamp to the saved filename. Default is `FALSE`.
#' #' @param base_filename Character string. The base name for the saved file. Default is `"margot_plot_output"`.
#' #' @param prefix Character string. An optional prefix for the saved filename. Default is `NULL`.
#' #' @param save_path Character string. The directory path where the output will be saved.
#' #'   Default is `here::here("push_mods")`.
#' #' @param original_df Optional data frame containing the original (non-transformed) data
#' #'   for back-transformation of results. If provided, it should correspond to `.data` before any transformations.
#' #'
#' #' @details
#' #' The `options` list can include the following parameters:
#' #' \itemize{
#' #'   \item `title`: \strong{Character string}. Main title for the plot.
#' #'   \item `subtitle`: \strong{Character string}. Subtitle for the plot.
#' #'   \item `estimate_scale`: \strong{Numeric}. Scaling factor for estimates. Default is `1`.
#' #'   \item `base_size`: \strong{Numeric}. Base font size for the plot. Default is `18`.
#' #'   \item `text_size`: \strong{Numeric}. Font size for text labels. Default is `2.75`.
#' #'   \item `point_size`: \strong{Numeric}. Size of points in the plot. Default is `3`.
#' #'   \item `title_size`: \strong{Numeric}. Font size for the main title. Default is `20`.
#' #'   \item `subtitle_size`: \strong{Numeric}. Font size for the subtitle. Default is `18`.
#' #'   \item `legend_text_size`: \strong{Numeric}. Font size for legend text. Default is `10`.
#' #'   \item `legend_title_size`: \strong{Numeric}. Font size for legend title. Default is `10`.
#' #'   \item `linewidth`: \strong{Numeric}. Width of lines in the plot. Default is `0.4`.
#' #'   \item `x_offset`: \strong{Numeric}. Horizontal offset for text labels on the plot.
#' #'     If `NULL`, it is set based on the `type` (`0` for "RR" and `-1.75` for "RD").
#' #'   \item `x_lim_lo`: \strong{Numeric}. Lower limit for the x-axis.
#' #'     If `NULL`, it is set based on the `type` (`0.1` for "RR" and `-1.75` for "RD").
#' #'   \item `x_lim_hi`: \strong{Numeric}. Upper limit for the x-axis.
#' #'     If `NULL`, it is set based on the `type` (`2.5` for "RR" and `1` for "RD").
#' #'   \item `plot_theme`: \strong{ggplot2 theme object}. Custom theme for the plot.
#' #'   \item `colors`: \strong{Named vector}. Custom colors for different estimate categories.
#' #'     Example: `c("positive" = "green", "not reliable" = "gray", "negative" = "red")`.
#' #'   \item `facet_var`: \strong{Character string}. Variable name for faceting the plot.
#' #'     Allows creating subplots based on a categorical variable.
#' #'   \item `confidence_level`: \strong{Numeric}. Confidence level for intervals. Default is `0.95`.
#' #'   \item `annotations`: \strong{ggplot2 layer}. Custom annotations to add to the plot, such as text or shapes.
#' #'   \item `show_evalues`: \strong{Logical}. If `TRUE`, shows E-values in the plot. Default is `TRUE`.
#' #'   \item `evalue_digits`: \strong{Integer}. Number of digits for E-value display. Default is `2`.
#' #'   \item `remove_tx_prefix`: \strong{Logical}. If `TRUE`, removes `"tx_"` prefix from labels and interpretation. Default is `TRUE`.
#' #'   \item `remove_z_suffix`: \strong{Logical}. If `TRUE`, removes `"_z"` suffix from labels and interpretation. Default is `TRUE`.
#' #'   \item `use_title_case`: \strong{Logical}. If `TRUE`, converts labels and interpretation to title case. Default is `TRUE`.
#' #'   \item `remove_underscores`: \strong{Logical}. If `TRUE`, removes underscores from labels and interpretation. Default is `TRUE`.
#' #' }
#' #'
#' #' The `label_mapping` parameter allows for custom renaming of specific outcomes:
#' #' \itemize{
#' #'   \item It should be a named list where names are original outcome labels and values are new labels.
#' #'   \item Outcomes not specified in `label_mapping` will use default transformations based on `options`.
#' #'   \item Custom mapped labels are used as-is, without applying default transformations.
#' #' }
#' #'
#' #' @return A list containing three elements:
#' #' \itemize{
#' #'   \item `plot`: A `ggplot` object representing the Margot plot.
#' #'   \item `interpretation`: A character string containing the interpretation of the results, with the same formatting applied as the plot labels.
#' #'   \item `transformed_table`: A data frame with the original data and transformed row names, using the same transformation options as the plot labels.
#' #' }
#' #'
#' #' If `save_output` is `TRUE`, the complete output will be saved to a file using `margot::here_save_qs()`.
#' #'
#' #' @import ggplot2
#' #' @import dplyr
#' #' @import cli
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create sample data
#' #' sample_data <- data.frame(
#' #'   outcome = c("t1_outcome_a_z", "t2_outcome_b_z", "t3_outcome_c_z"),
#' #'   `E[Y(1)]-E[Y(0)]` = c(0.1, -0.2, 0.3),
#' #'   `2.5 %` = c(0.05, -0.3, 0.2),
#' #'   `97.5 %` = c(0.15, -0.1, 0.4),
#' #'   E_Value = c(1.5, 1.8, 2.0),
#' #'   E_Val_bound = c(1.3, 1.5, 1.7)
#' #' )
#' #'
#' #' # Create a basic Margot plot with interpretation and transformed table
#' #' result <- margot_plot(sample_data, type = "RD")
#' #' print(result$plot)
#' #' cat(result$interpretation)
#' #' print(result$transformed_table)
#' #'
#' #' # Create a Margot plot with custom options, label mapping, and save output
#' #' custom_result <- margot_plot(
#' #'   sample_data,
#' #'   type = "RD",
#' #'   options = list(
#' #'     title = "Custom Margot Plot",
#' #'     subtitle = "With custom options",
#' #'     remove_tx_prefix = TRUE,
#' #'     remove_z_suffix = TRUE,
#' #'     use_title_case = TRUE,
#' #'     remove_underscores = TRUE,
#' #'     colors = c("positive" = "green", "not reliable" = "gray", "negative" = "red"),
#' #'     x_lim_hi = 1.5,
#' #'     x_lim_lo = -0.5,
#' #'     x_offset = -0.2
#' #'   ),
#' #'   label_mapping = list(
#' #'     "t1_outcome_a_z" = "Custom Label A",
#' #'     "t2_outcome_b_z" = "Custom Label B"
#' #'   ),
#' #'   save_output = TRUE,
#' #'   use_timestamp = TRUE,
#' #'   prefix = "custom",
#' #'   save_path = here::here("output", "margot_plots")
#' #' )
#' #' print(custom_result$plot)
#' #' cat(custom_result$interpretation)
#' #' print(custom_result$transformed_table)
#' #'
#' #' # Create a Margot plot with original data for back-transformation
#' #' original_data <- data.frame(
#' #'   t1_outcome_a = rnorm(100),
#' #'   t2_outcome_b = rnorm(100),
#' #'   t3_outcome_c = rnorm(100)
#' #' )
#' #' result_with_original <- margot_plot(
#' #'   sample_data,
#' #'   type = "RD",
#' #'   original_df = original_data
#' #' )
#' #' print(result_with_original$plot)
#' #' cat(result_with_original$interpretation)
#' #' print(result_with_original$transformed_table)
#' #' }
#' #'
#' #' @export
#' margot_plot <- function(.data,
#'                         type = c("RD", "RR"),
#'                         order = c("default", "alphabetical", "custom"),
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
#'   # **Set default type to "RD"**
#'   type <- match.arg(type, c("RD", "RR"), several.ok = FALSE)
#'
#'   # **Set default order to "default"**
#'   order <- match.arg(order)
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
#'
#'   # Merge user-provided options with defaults and additional arguments
#'   options <- modifyList(modifyList(default_options, options), additional_args)
#'
#'   # Input validation
#'   if (!is.data.frame(.data)) {
#'     cli::cli_abort("Input must be a data frame")
#'   }
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
#'   # Ensure .data has an 'outcome' column
#'   if (!"outcome" %in% names(.data)) {
#'     .data$outcome <- rownames(.data)
#'     cli::cli_alert_info("Added 'outcome' column based on row names")
#'   }
#'
#'   # Store original variable names before any label transformations
#'   if (!"original_var_name" %in% names(.data)) {
#'     .data$original_var_name <- .data$outcome
#'   }
#'
#'   # Transform to original scale if original_df is provided
#'   if (!is.null(original_df)) {
#'     .data <- back_transform_estimates(.data, original_df)
#'   } else {
#'     cli::cli_alert_info("No original_df provided. Results will be on the transformed scale.")
#'   }
#'
#'   # Apply transformations to outcome labels
#'   .data$outcome <- sapply(.data$outcome, transform_label, label_mapping = label_mapping, options = options)
#'
#'   # Prepare the data for plotting, including ordering
#'   .data <- group_tab(.data, type = type, order = order)
#'
#'   # **Set type-dependent options if not provided**
#'   if (is.null(options$x_offset)) options$x_offset <- ifelse(type == "RR", 0, -1.75)
#'   if (is.null(options$x_lim_lo)) options$x_lim_lo <- ifelse(type == "RR", 0.1, -1.75)
#'   if (is.null(options$x_lim_hi)) options$x_lim_hi <- ifelse(type == "RR", 2.5, 1)
#'
#'   # **Adjust 'Estimate' based on whether the confidence interval crosses the null value**
#'   null_value <- ifelse(type == "RR", 1, 0)
#'
#'   .data <- .data %>%
#'     dplyr::mutate(
#'       Estimate = dplyr::case_when(
#'         (`2.5 %` > null_value & `97.5 %` > null_value) ~ "positive",
#'         (`2.5 %` < null_value & `97.5 %` < null_value) ~ "negative",
#'         TRUE ~ "not reliable"
#'       )
#'     )
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
#'                             linewidth = options$linewidth,
#'                             position = ggplot2::position_dodge(width = .3)
#'     ) +
#'     ggplot2::geom_point(size = options$point_size, position = ggplot2::position_dodge(width = 0.3)) +
#'     ggplot2::geom_vline(xintercept = null_value, linetype = "solid") +
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
#'     order = order,
#'     original_df = original_df
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
#'         output_filename <- paste0(
#'           ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#'           base_filename, "_",
#'           format(Sys.time(), "%Y%m%d_%H%M%S")
#'         )
#'       } else {
#'         output_filename <- paste0(
#'           ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
#'           base_filename
#'         )
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
#' #' #' Interpret and Describe Causal Effect Estimates Using E-values
#' #' #'
#' #' #' This function interprets the output of causal effect analysis, providing textual descriptions
#' #' #' of causal effect estimates. It categorises the strength of evidence for causality based on
#' #' #' E-values and confidence intervals, and generates a detailed interpretation of the effect
#' #' #' estimates according to specified types (i.e., "RD" for risk difference or "RR" for risk ratio)
#' #' #' and estimands. It now also includes interpretation of original scale results when available.
#' #' #'
#' #' #' @param df Data frame containing causal effect estimates, expected to include columns for
#' #' #' outcome names, effect estimates (either differences or ratios), confidence intervals,
#' #' #' E-values, and a summary estimate label. Can also be the list output from transform_to_original_scale().
#' #' #' @param type Character string specifying the type of effect estimate. Must be either "RD"
#' #' #' (Risk Difference) or "RR" (Risk Ratio). Default is "RD".
#' #' #' @param estimand Optional character string indicating the type of causal estimand interpreted: "PATE"
#' #' #' (Population Average Treatment Effect), "ATE" (Average Treatment Effect), "ATT" (Average
#' #' #' Treatment Effect in the Treated), "CATE" (Conditional Average Treatment Effect), or "LMTP"
#' #' #' (Longitudinal Modified Treatment Policy). Default is NULL.
#' #' #' @param order Character string specifying the order of results. Default is "default".
#' #' #'
#' #' #' @return A list containing two elements:
#' #' #'   \item{estimand_description}{A character string describing the specified estimand, or NULL if no estimand was provided.}
#' #' #'   \item{interpretation}{A character string containing a detailed interpretation of each outcome in `df`,
#' #' #'   including the causal contrast, E-values, the strength of evidence for causality, and original scale results if available.}
#' #' #'
#' #' #' @details
#' #' #' The function now handles both transformed and original scale results. If original scale results
#' #' #' are available (indicated by the presence of columns with "_original" suffix), these will be included
#' #' #' in the interpretation. The strength of evidence for causality is categorized as follows:
#' #' #' \itemize{
#' #' #'   \item Strong evidence: E-value lower bound > 2
#' #' #'   \item Evidence: E-value lower bound > 1.1 and <= 2
#' #' #'   \item Weak evidence: E-value lower bound > 1 and <= 1.1
#' #' #'   \item Not reliable evidence: E-value lower bound <= 1 or confidence interval includes null effect
#' #' #' }
#' #' #'
#' #' #' @examples
#' #' #' \dontrun{
#' #' #' # Assuming `group_tab_output` is the result from a causal analysis
#' #' #' result <- margot_interpret_marginal(group_tab_output, type = "RD", estimand = "ATE")
#' #' #' cat(result$estimand_description)
#' #' #' cat(result$interpretation)
#' #' #'
#' #' #' # Using Risk Ratio without specifying an estimand
#' #' #' result <- margot_interpret_marginal(group_tab_output, type = "RR")
#' #' #' cat(result$interpretation)
#' #' #'
#' #' #' # Using output from transform_to_original_scale()
#' #' #' transformed_data <- transform_to_original_scale(results_df, original_df, label_mapping)
#' #' #' result <- margot_interpret_marginal(transformed_data, type = "RD")
#' #' #' cat(result$interpretation)
#' #' #' }
#' #' #'
#' #' #' @import rlang
#' #' #' @importFrom dplyr case_when mutate rowwise ungroup if_else
#' #' #' @importFrom glue glue
#' #' #' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' #' #' @keywords internal
#' #' margot_interpret_marginal <- function(df, type = c("RD", "RR"), estimand = NULL, order = "default", original_df = NULL) {
#' #'   type <- match.arg(type)
#' #'
#' #'   cli::cli_alert_info("Starting interpretation of causal effect estimates...")
#' #'
#' #'   # Define estimand descriptions
#' #'   estimand_description <- if (!is.null(estimand)) {
#' #'     dplyr::case_when(
#' #'       estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
#' #'       estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
#' #'       estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
#' #'       estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
#' #'       estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
#' #'       TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
#' #'     )
#' #'   } else {
#' #'     NULL
#' #'   }
#' #'
#' #'   cli::cli_alert_info("Processing and interpreting data...")
#' #'
#' #'   # Process df via group_tab to ensure 'Estimate' variable is present
#' #'   df <- group_tab(df, type = type, order = order)
#' #'
#' #'   # If original_df is provided, back-transform estimates
#' #'   if (!is.null(original_df)) {
#' #'     df <- back_transform_estimates(df, original_df)
#' #'   }
#' #'
#' #'   # Determine the effect size column
#' #'   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
#' #'     "E[Y(1)]-E[Y(0)]"
#' #'   } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
#' #'     "E[Y(1)]/E[Y(0)]"
#' #'   } else {
#' #'     cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
#' #'   }
#' #'
#' #'   # Determine if we have original scale results
#' #'   has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)
#' #'
#' #'   # Define the null_value based on type
#' #'   null_value <- ifelse(type == "RR", 1, 0)
#' #'
#' #'   interpretation <- df %>%
#' #'     dplyr::rowwise() %>%
#' #'     dplyr::mutate(
#' #'       # Determine if the variable was log-transformed
#' #'       was_log_transformed = grepl("_log_", original_var_name),
#' #'       # Define evidence_strength based on E_Val_bound and CI
#' #'       evidence_strength = dplyr::case_when(
#' #'         (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ cli::col_green('**the evidence for causality is strong**'),
#' #'         (`2.5 %` > null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) | (`97.5 %` < null_value & E_Val_bound > 1.1 & E_Val_bound <= 2) ~ cli::col_blue('**there is evidence for causality**'),
#' #'         (`2.5 %` > null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) | (`97.5 %` < null_value & E_Val_bound > 1 & E_Val_bound <= 1.1) ~ cli::col_yellow('**the evidence for causality is weak**'),
#' #'         TRUE ~ cli::col_red('**the evidence for causality is not reliable**')
#' #'       ),
#' #'       # Units
#' #'       unit = ifelse(!is.na(unit) & unit != "", unit, ""),
#' #'       # Define estimate_lab
#' #'       estimate_lab = paste0(
#' #'         round(!!rlang::sym(effect_size_col), 3), " (",
#' #'         round(`2.5 %`, 3), ", ",
#' #'         round(`97.5 %`, 3), ")"
#' #'       ),
#' #'       # Define estimate_lab_original if available
#' #'       estimate_lab_original = if (has_original_scale) {
#' #'         if (unit != "") {
#' #'           paste0(
#' #'             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
#' #'             round(`2.5 %_original`, 3), " to ",
#' #'             round(`97.5 %_original`, 3), " ", unit, ")"
#' #'           )
#' #'         } else {
#' #'           paste0(
#' #'             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
#' #'             round(`2.5 %_original`, 3), ", ",
#' #'             round(`97.5 %_original`, 3), ")"
#' #'           )
#' #'         }
#' #'       } else {
#' #'         NA_character_
#' #'       },
#' #'       outcome_interpretation = glue::glue(
#' #'         "For '{outcome}', the effect estimate ({type}) is {estimate_lab}. ",
#' #'         "{if (has_original_scale) paste0('On the original data scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
#' #'         "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
#' #'         "{if (E_Val_bound > 1) paste0('At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of ', E_Val_bound, ' to negate the observed effect. Weaker confounding would not overturn it. ') else ''}",
#' #'         "Here, {evidence_strength}."
#' #'       )
#' #'     ) %>%
#' #'     dplyr::ungroup()
#' #'
#' #'   # Compile results
#' #'   interpretation_text <- paste(interpretation$outcome_interpretation, collapse = '\n\n')
#' #'
#' #'   cli::cli_alert_success("Interpretation completed successfully!")
#' #'
#' #'   # Return results as a list
#' #'   return(list(
#' #'     estimand_description = estimand_description,
#' #'     interpretation = interpretation_text
#' #'   ))
#' #' }
#' # margot_interpret_marginal <- function(df, type = c("RD", "RR"), estimand = NULL, order = "default", original_df = NULL) {
#' #   type <- match.arg(type)
#' #
#' #   cli::cli_alert_info("Starting interpretation of causal effect estimates...")
#' #
#' #   # Define estimand descriptions (as before)
#' #   estimand_description <- if (!is.null(estimand)) {
#' #     dplyr::case_when(
#' #       estimand == "LMTP" ~ "A Longitudinal Modified Treatment Policy (LMTP) calculates the expected outcome difference between treatment and contrast conditions over a sequential regime of treatments for a prespecified target population.",
#' #       estimand == "PATE" ~ "The Population Average Treatment Effect (PATE) estimates the expected outcome difference between treatment and contrast groups across the entire New Zealand population.",
#' #       estimand == "ATE" ~ "The Average Treatment Effect (ATE) measures the mean difference in outcomes between treatment and contrast groups within the target population.",
#' #       estimand == "ATT" ~ "The Average Treatment Effect on the Treated (ATT) assesses the expected outcome difference for those receiving the treatment, compared to a similar group that did not, within the target population.",
#' #       estimand == "CATE" ~ "The Conditional Average Treatment Effect (CATE) evaluates the expected difference in outcomes between treatment and contrast groups within specific population strata.",
#' #       TRUE ~ "The specified estimand is not recognized. Valid options include: 'PATE', 'ATE', 'ATT', 'CATE', 'LMTP'."
#' #     )
#' #   } else {
#' #     NULL
#' #   }
#' #
#' #   cli::cli_alert_info("Processing and interpreting data...")
#' #
#' #   # Process df via group_tab to ensure 'Estimate' variable is present
#' #   df <- group_tab(df, type = type, order = order)
#' #
#' #   # If original_df is provided, back-transform estimates
#' #   if (!is.null(original_df)) {
#' #     df <- back_transform_estimates(df, original_df)
#' #   }
#' #
#' #   # Determine the effect size column
#' #   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
#' #     "E[Y(1)]-E[Y(0)]"
#' #   } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
#' #     "E[Y(1)]/E[Y(0)]"
#' #   } else {
#' #     cli::cli_abort("Data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column")
#' #   }
#' #
#' #   # Determine if we have original scale results
#' #   has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)
#' #
#' #   interpretation <- df %>%
#' #     dplyr::rowwise() %>%
#' #     dplyr::mutate(
#' #       # Determine if the variable was log-transformed
#' #       was_log_transformed = grepl("_log_", original_var_name),
#' #       # Define evidence_strength
#' #       evidence_strength = dplyr::case_when(
#' #         Estimate == 'positive' ~ cli::col_green('**the evidence for causality is strong**'),
#' #         Estimate == 'negative' ~ cli::col_blue('**there is evidence for causality**'),
#' #         TRUE ~ cli::col_red('**the evidence for causality is not reliable**')
#' #       ),
#' #       # Units
#' #       unit = ifelse(!is.na(unit) & unit != "", unit, ""),
#' #       # Define estimate_lab
#' #       estimate_lab = paste0(
#' #         round(!!rlang::sym(effect_size_col), 3), " (",
#' #         round(`2.5 %`, 3), ", ",
#' #         round(`97.5 %`, 3), ")"
#' #       ),
#' #       # Define estimate_lab_original if available
#' #       estimate_lab_original = if (has_original_scale) {
#' #         if (unit != "") {
#' #           paste0(
#' #             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
#' #             round(`2.5 %_original`, 3), " to ",
#' #             round(`97.5 %_original`, 3), " ", unit, ")"
#' #           )
#' #         } else {
#' #           paste0(
#' #             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
#' #             round(`2.5 %_original`, 3), ", ",
#' #             round(`97.5 %_original`, 3), ")"
#' #           )
#' #         }
#' #       } else {
#' #         NA_character_
#' #       },
#' #       outcome_interpretation = glue::glue(
#' #         "For '{outcome}', the effect estimate ({type}) is {estimate_lab}. ",
#' #         "{if (has_original_scale) paste0('On the original data scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
#' #         "The E-value for this estimate is {E_Value}, with a lower bound of {E_Val_bound}. ",
#' #         "{if (E_Val_bound > 1) paste0('At this lower bound, unmeasured confounders would need a minimum association strength with both the intervention sequence and outcome of ', E_Val_bound, ' to negate the observed effect. Weaker confounding would not overturn it. ') else ''}",
#' #         "Here, {evidence_strength}."
#' #       )
#' #     ) %>%
#' #     dplyr::ungroup()
#' #
#' #   # Compile results
#' #   interpretation_text <- paste(interpretation$outcome_interpretation, collapse = '\n\n')
#' #
#' #   cli::cli_alert_success("Interpretation completed successfully!")
#' #
#' #   # Return results as a list
#' #   return(list(
#' #     estimand_description = estimand_description,
#' #     interpretation = interpretation_text
#' #   ))
#' # }
