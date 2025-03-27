#' Generate summary tables and plots for longitudinal data
#'
#' @param data A data frame containing the longitudinal data
#' @param baseline_wave The wave number for baseline measurements
#' @param exposure_waves A vector of wave numbers for exposure measurements
#' @param outcome_wave The wave number for outcome measurements
#' @param name_exposure The name of the exposure variable
#' @param name_exposure_cat The name of the categorical exposure variable (optional)
#' @param baseline_vars A vector of baseline variable names
#' @param outcome_vars A vector of outcome variable names
#' @param extra_vars A vector of additional variable names (default: c("id", "wave", "year_measured", "not_lost", "sample_weights"))
#' @param baseline_labels A named vector of labels for baseline variables (optional)
#' @param exposure_labels A named vector of labels for exposure variables (optional)
#' @param outcome_labels A named vector of labels for outcome variables (optional)
#' @param create_plots Logical, whether to create plots (default: FALSE)
#' @param plot_type The type of plot to create ("boxplot" or "density", default: "boxplot")
#' @param show_progress Logical, whether to show a progress bar (default: TRUE)
#'
#' @return A list containing summary tables, information, and optional plots
#' @export
#'
#' @importFrom dplyr filter select all_of
#' @importFrom gtsummary tbl_summary modify_header bold_labels
#' @importFrom janitor clean_names
#' @importFrom labelled to_factor
#' @importFrom cli cli_h1 cli_h2 cli_alert_success cli_alert_info cli_alert_warning cli_abort cli_progress_bar cli_progress_update cli_progress_done cli_ul cli_li cli_end cli_text cli_rule
#' @importFrom ggplot2 ggplot aes geom_density labs theme_minimal geom_boxplot theme element_text annotate geom_jitter scale_fill_manual scale_color_manual scale_y_continuous scale_x_discrete unit sym
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_title
margot_summary_tables <- function(data,
                                  baseline_wave,
                                  exposure_waves,
                                  outcome_wave,
                                  name_exposure,
                                  name_exposure_cat = NULL,
                                  baseline_vars = NULL,
                                  outcome_vars = NULL,
                                  extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
                                  baseline_labels = NULL,
                                  exposure_labels = NULL,
                                  outcome_labels = NULL,
                                  create_plots = FALSE,
                                  plot_type = "boxplot",
                                  show_progress = TRUE) {
  # Check if at least one variable list is provided
  if (is.null(baseline_vars) && is.null(outcome_vars)) {
    cli::cli_abort("At least one of baseline_vars or outcome_vars must be provided.")
  }

  # Warn if less than three variable lists are provided
  if (sum(!is.null(baseline_vars), !is.null(outcome_vars), !is.null(name_exposure)) < 3) {
    cli::cli_alert_warning("Less than three variable lists (baseline, exposure, outcome) were provided. Some analyses may be limited.")
  }

  cli::cli_h1("Margot Summary Tables")

  # Determine the number of steps for the progress bar
  base_steps <- 8
  per_exposure_steps <- 2
  additional_steps <- ifelse(create_plots, 1, 0)
  total_steps <- base_steps + length(name_exposure) * (per_exposure_steps + additional_steps)

  # Initialize progress bar
  pb <- NULL
  if (show_progress) {
    pb <- cli::cli_progress_bar(
      total = total_steps,
      format = "Generating summary tables and plots {cli::pb_bar} {cli::pb_percent}",
      clear = FALSE
    )
  }

  # Function to update progress
  update_progress <- function() {
    if (show_progress && !is.null(pb)) {
      tryCatch({
        cli::cli_progress_update(id = pb)
      }, error = function(e) {
        cli::cli_alert_warning(paste("Error updating progress bar:", e$message))
      })
    }
  }

  # Handle exposure_waves input
  if (length(exposure_waves) == 1 && grepl(",", exposure_waves)) {
    exposure_waves <- unlist(strsplit(exposure_waves, ","))
  }
  exposure_waves <- trimws(exposure_waves) # Remove any whitespace

  # Sort variables if provided
  if (!is.null(baseline_vars)) baseline_vars <- sort(baseline_vars)
  if (!is.null(outcome_vars)) outcome_vars <- sort(outcome_vars)

  update_progress()

  # Calculate number of unique participants
  n_participants <- length(unique(data$id))
  n_participants_formatted <- format(n_participants, big.mark = ",")

  cli::cli_alert_success("Number of unique participants: {.val {n_participants_formatted}} \U0001F44D")

  # Baseline table (if baseline_vars provided)
  table_baseline <- NULL
  if (!is.null(baseline_vars)) {
    dt_baseline <- dplyr::filter(data, wave == baseline_wave) %>%
      dplyr::select(dplyr::all_of(baseline_vars)) %>%
      droplevels()

    table_baseline <- janitor::clean_names(dt_baseline, case = "title") %>%
      gtsummary::tbl_summary(
        missing = "ifany",
        percent = "column",
        statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
        type = list(all_continuous() ~ "continuous2"),
        label = baseline_labels
      ) %>%
      gtsummary::modify_header(label = "**Exposure + Demographic Variables**") %>%
      gtsummary::bold_labels()
  }

  update_progress()

  # Function to format variable names
  format_var_name <- function(name) {
    stringr::str_to_title(gsub("_", " ", name))
  }

  # Initialize lists to store exposure tables, summaries, and plots
  exposure_tables <- list()
  exposure_summaries <- list()
  plots <- list()

  # Process each exposure variable
  for (exposure_var in name_exposure) {
    cli::cli_h2(paste("Processing exposure variable:", exposure_var))

    # Exposure table and summary
    dt_exposure <- dplyr::filter(data, wave %in% c(baseline_wave, exposure_waves)) %>%
      dplyr::select(dplyr::all_of(c(exposure_var, "wave"))) %>%
      droplevels()

    exposure_tables[[exposure_var]] <- janitor::clean_names(dt_exposure, case = "title") %>%
      labelled::to_factor() %>%
      gtsummary::tbl_summary(
        by = "Wave",
        missing = "always",
        percent = "column",
        label = exposure_labels
      ) %>%
      gtsummary::modify_header(label = paste("**Exposure Variable:", format_var_name(exposure_var), "by Wave**")) %>%
      gtsummary::bold_labels()

    update_progress()

    # Calculate exposure summary for each wave
    exposure_summaries[[exposure_var]] <- lapply(c(baseline_wave, exposure_waves), function(wave) {
      wave_data <- dplyr::filter(dt_exposure, wave == !!wave)
      list(
        wave = wave,
        mean_exposure = mean(wave_data[[exposure_var]], na.rm = TRUE),
        sd_exposure = sd(wave_data[[exposure_var]], na.rm = TRUE),
        median_exposure = median(wave_data[[exposure_var]], na.rm = TRUE)
      )
    })

    update_progress()

    # Create plots if specified
    if (create_plots) {
      formatted_exposure_name <- format_var_name(exposure_var)

      # Exposure boxplot
      exposure_plot <- margot_plot_exposure_boxplot(
        data = dt_exposure,
        exposure_var = exposure_var,
        waves = exposure_waves,
        baseline_wave = baseline_wave,
        y_label = formatted_exposure_name
      )

      plots[[paste0(exposure_var, "_boxplot")]] <- exposure_plot

      update_progress()

      cli::cli_alert_success("Exposure plot created successfully 👍")
    }
  }

  # Outcome table (if outcome_vars provided)
  table_outcomes <- NULL
  if (!is.null(outcome_vars)) {
    dt_outcome <- dplyr::filter(data, wave %in% c(baseline_wave, outcome_wave)) %>%
      dplyr::select(dplyr::all_of(c(outcome_vars, "wave"))) %>%
      droplevels()

    table_outcomes <- janitor::clean_names(dt_outcome, case = "title") %>%
      labelled::to_factor() %>%
      gtsummary::tbl_summary(
        by = "Wave",
        missing = "always",
        percent = "column",
        label = outcome_labels
      ) %>%
      gtsummary::modify_header(label = "**Outcome Variables by Wave**") %>%
      gtsummary::bold_labels()
  }

  update_progress()

  # Outcome variables plot (if outcome_vars provided and create_plots is TRUE)
  if (!is.null(outcome_vars) && create_plots) {
    outcome_long <- tidyr::pivot_longer(
      dt_outcome,
      cols = dplyr::all_of(outcome_vars),
      names_to = "Variable",
      values_to = "Value"
    )

    # Format variable names
    outcome_long$Variable <- sapply(outcome_long$Variable, format_var_name)

    if (plot_type == "boxplot") {
      outcome_plot <- ggplot2::ggplot(outcome_long, ggplot2::aes(x = Variable, y = Value, fill = wave)) +
        ggplot2::geom_boxplot(notch = TRUE) +
        ggplot2::labs(
          title = "Distribution of Outcome Variables by Wave",
          x = "Outcome Variables",
          y = "Value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::scale_fill_manual(values = okabe_ito_palette())
    } else if (plot_type == "density") {
      outcome_plot <- ggplot2::ggplot(outcome_long, ggplot2::aes(x = Value, fill = wave)) +
        ggplot2::geom_density(alpha = 0.7) +
        ggplot2::facet_wrap(~Variable, scales = "free") +
        ggplot2::labs(
          title = "Density Distribution of Outcome Variables by Wave",
          x = "Value",
          y = "Density"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_manual(values = okabe_ito_palette())
    }

    plots$outcome_plot <- outcome_plot

    cli::cli_alert_success("Plots generated successfully 📈")

    update_progress()
  }

  # Display summary information
  cli::cli_h2("Summary Information")
  cli::cli_ul()
  cli::cli_li("Baseline wave: {.val {baseline_wave}}")
  cli::cli_li("Exposure waves: {.val {paste(exposure_waves, collapse = ', ')}}")
  cli::cli_li("Outcome wave: {.val {paste(outcome_wave, collapse = ' and ')}}")
  cli::cli_li("Exposure variables: {.val {paste(name_exposure, collapse = ', ')}}")
  if (!is.null(name_exposure_cat)) {
    cli::cli_li("Categorical exposure variable: {.val {name_exposure_cat}}")
  }
  cli::cli_end()

  # Display exposure summaries
  cli::cli_h2("Exposure Summaries")
  for (exposure_var in name_exposure) {
    cli::cli_alert_info(paste("Summary for", exposure_var))
    for (summary in exposure_summaries[[exposure_var]]) {
      cli::cli_ul()
      cli::cli_li("Wave {.val {summary$wave}}:")
      cli::cli_li("  Mean: {.val {round(summary$mean_exposure, 2)}}")
      cli::cli_li("  SD: {.val {round(summary$sd_exposure, 2)}}")
      cli::cli_li("  Median: {.val {round(summary$median_exposure, 2)}}")
      cli::cli_end()
    }
  }

  update_progress()

  # Close progress bar
  if (show_progress && !is.null(pb)) {
    tryCatch({
      cli::cli_progress_done(id = pb)
    }, error = function(e) {
      cli::cli_alert_warning(paste("Error closing progress bar:", e$message))
    })
  }

  # Final success message
  cli::cli_alert_success("Summary tables, information, and optional plots generated successfully \U0001F44D")

  # Return results
  return(list(
    baseline_table = table_baseline,
    exposure_tables = exposure_tables,
    outcome_table = table_outcomes,
    n_participants = n_participants_formatted,
    exposure_summaries = exposure_summaries,
    plots = plots
  ))
}

# Helper function for boxplot
#' @keywords internal
margot_plot_exposure_boxplot <- function(data,
                                         exposure_var,
                                         waves,
                                         baseline_wave,
                                         y_label = NULL,
                                         x_label = "Wave",
                                         show_points = FALSE,
                                         point_alpha = 0.05,
                                         point_size = 0.5) {

  # Ensure wave column is present
  if (!"wave" %in% names(data)) {
    cli::cli_abort("The 'wave' column is missing from the data.")
  }

  # Ensure waves are unique and in the correct order
  all_waves <- unique(c(baseline_wave, waves))

  # Convert wave to factor with correct levels
  data$wave <- factor(data$wave, levels = all_waves)

  # Filter waves
  df <- dplyr::filter(data, wave %in% all_waves)

  # Function to convert to title case and remove underscores
  format_label <- function(x) {
    stringr::str_to_title(gsub("_", " ", x))
  }

  # Format labels
  exposure_label <- format_label(exposure_var)

  # Determine y-axis limits
  y_min <- min(df[[exposure_var]], na.rm = TRUE)
  y_max <- max(df[[exposure_var]], na.rm = TRUE)
  if (y_min >= 1 && y_max <= 7) {
    y_limits <- c(1, 7)
  } else {
    y_limits <- c(y_min, y_max)
  }

  # Get the number of waves
  n_waves <- length(all_waves)

  # Create a color palette that recycles if needed
  okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                         "#0072B2", "#D55E00", "#CC79A7", "#999999")
  color_palette <- rep(okabe_ito_palette, length.out = n_waves)

  # Create the ggplot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = wave, y = !!ggplot2::sym(exposure_var), fill = wave)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::scale_x_discrete(drop = FALSE) + # Force all levels to be shown
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      strip.text = ggplot2::element_text(size = 10),
      panel.spacing = ggplot2::unit(0.2, "lines")
    ) +
    ggplot2::labs(
      title = paste("Distribution of", exposure_label, "by Wave"),
      y = y_label %||% exposure_label,
      x = x_label
    ) +
    ggplot2::scale_y_continuous(limits = y_limits)

  # Add points if requested
  if (show_points) {
    p <- p + ggplot2::geom_jitter(ggplot2::aes(color = wave), width = 0.2, alpha = point_alpha, size = point_size) +
      ggplot2::scale_color_manual(values = color_palette)
  }

  return(p)
}

# Helper for the %||% operator (null coalescing)
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper function for the Okabe-Ito palette
#' @keywords internal
okabe_ito_palette <- function() {
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
}
#' margot_summary_tables <- function(data,
#'                                   baseline_wave,
#'                                   exposure_waves,
#'                                   outcome_wave,
#'                                   name_exposure,
#'                                   name_exposure_cat = NULL,
#'                                   baseline_vars = NULL,
#'                                   outcome_vars = NULL,
#'                                   extra_vars = c("id", "wave", "year_measured", "not_lost", "sample_weights"),
#'                                   baseline_labels = NULL,
#'                                   exposure_labels = NULL,
#'                                   outcome_labels = NULL,
#'                                   create_plots = FALSE,
#'                                   plot_type = "boxplot",
#'                                   show_progress = TRUE) {
#'   # Load required packages
#'   require(cli)
#'   require(dplyr)
#'   require(janitor)
#'   require(gtsummary)
#'   require(labelled)
#'   require(tidyr)
#'   require(ggplot2)
#'   require(crayon)
#'
#'   # Check if at least one variable list is provided
#'   if (is.null(baseline_vars) && is.null(outcome_vars)) {
#'     stop("At least one of baseline_vars or outcome_vars must be provided.")
#'   }
#'
#'   # Warn if less than three variable lists are provided
#'   if (sum(!is.null(baseline_vars), !is.null(outcome_vars), !is.null(name_exposure)) < 3) {
#'     warning("Less than three variable lists (baseline, exposure, outcome) were provided. Some analyses may be limited.")
#'   }
#'
#'   cli::cli_h1("Margot Summary Tables")
#'
#'   # Determine the number of steps for the progress bar
#'   base_steps <- 8
#'   per_exposure_steps <- 2
#'   additional_steps <- ifelse(create_plots, 1, 0)
#'   total_steps <- base_steps + length(name_exposure) * (per_exposure_steps + additional_steps)
#'
#'   # Initialize progress bar
#'   pb <- NULL
#'   if (show_progress) {
#'     pb <- cli::cli_progress_bar(
#'       total = total_steps,
#'       format = "Generating summary tables and plots {cli::pb_bar} {cli::pb_percent}",
#'       clear = FALSE
#'     )
#'   }
#'
#'   # Function to update progress
#'   update_progress <- function() {
#'     if (show_progress && !is.null(pb)) {
#'       tryCatch({
#'         cli::cli_progress_update(id = pb)
#'       }, error = function(e) {
#'         warning("Error updating progress bar: ", e$message)
#'       })
#'     }
#'   }
#'
#'   # Handle exposure_waves input
#'   if (length(exposure_waves) == 1 && grepl(",", exposure_waves)) {
#'     exposure_waves <- unlist(strsplit(exposure_waves, ","))
#'   }
#'   exposure_waves <- trimws(exposure_waves) # Remove any whitespace
#'
#'   # Sort variables if provided
#'   if (!is.null(baseline_vars)) baseline_vars <- sort(baseline_vars)
#'   if (!is.null(outcome_vars)) outcome_vars <- sort(outcome_vars)
#'
#'   update_progress()
#'
#'   # Calculate number of unique participants
#'   n_participants <- length(unique(data$id))
#'   n_participants_formatted <- format(n_participants, big.mark = ",")
#'
#'   cli::cli_alert_success("Number of unique participants: {.val {n_participants_formatted}} \U0001F44D")
#'
#'   # Baseline table (if baseline_vars provided)
#'   table_baseline <- NULL
#'   if (!is.null(baseline_vars)) {
#'     dt_baseline <- data %>%
#'       dplyr::filter(wave == baseline_wave) %>%
#'       dplyr::select(all_of(baseline_vars)) %>%
#'       droplevels()
#'
#'     table_baseline <- dt_baseline %>%
#'       janitor::clean_names(case = "title") %>%
#'       gtsummary::tbl_summary(
#'         missing = "ifany",
#'         percent = "column",
#'         statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")),
#'         type = list(all_continuous() ~ "continuous2"),
#'         label = baseline_labels
#'       ) %>%
#'       gtsummary::modify_header(label = "**Exposure + Demographic Variables**") %>%
#'       gtsummary::bold_labels()
#'   }
#'
#'   update_progress()
#'
#'   # Function to format variable names
#'   format_var_name <- function(name) {
#'     name %>%
#'       gsub("_", " ", .) %>%
#'       stringr::str_to_title()
#'   }
#'
#'   # Initialize lists to store exposure tables, summaries, and plots
#'   exposure_tables <- list()
#'   exposure_summaries <- list()
#'   plots <- list()
#'
#'   # Process each exposure variable
#'   # Process each exposure variable
#'   for (exposure_var in name_exposure) {
#'     cli::cli_h2(paste("Processing exposure variable:", exposure_var))
#'
#'     # Exposure table and summary
#'     dt_exposure <- data %>%
#'       dplyr::filter(wave %in% c(baseline_wave, exposure_waves)) %>%
#'       dplyr::select(all_of(c(exposure_var, "wave"))) %>%
#'       droplevels()
#'
#'     exposure_tables[[exposure_var]] <- dt_exposure %>%
#'       janitor::clean_names(case = "title") %>%
#'       labelled::to_factor() %>%
#'       gtsummary::tbl_summary(
#'         by = "Wave",
#'         missing = "always",
#'         percent = "column",
#'         label = exposure_labels
#'       ) %>%
#'       gtsummary::modify_header(label = paste("**Exposure Variable:", format_var_name(exposure_var), "by Wave**")) %>%
#'       gtsummary::bold_labels()
#'
#'     update_progress()
#'
#'     # Calculate exposure summary for each wave
#'     exposure_summaries[[exposure_var]] <- lapply(c(baseline_wave, exposure_waves), function(wave) {
#'       wave_data <- dt_exposure %>% dplyr::filter(wave == !!wave)
#'       list(
#'         wave = wave,
#'         mean_exposure = mean(wave_data[[exposure_var]], na.rm = TRUE),
#'         sd_exposure = sd(wave_data[[exposure_var]], na.rm = TRUE),
#'         median_exposure = median(wave_data[[exposure_var]], na.rm = TRUE)
#'       )
#'     })
#'
#'     update_progress()
#'
#'     # Create plots if specified
#'     if (create_plots) {
#'       formatted_exposure_name <- format_var_name(exposure_var)
#'
#'       # Exposure boxplot
#'       exposure_plot <- margot_plot_exposure_boxplot(
#'         data = dt_exposure,
#'         exposure_var = exposure_var,
#'         waves = exposure_waves,
#'         baseline_wave = baseline_wave,
#'         y_label = formatted_exposure_name
#'       )
#'
#'       plots[[paste0(exposure_var, "_boxplot")]] <- exposure_plot
#'
#'       update_progress()
#'
#'       cli::cli_alert_success("Exposure plot created successfully 👍")
#'     }
#'   }
#'
#'   # Outcome table (if outcome_vars provided)
#'   table_outcomes <- NULL
#'   if (!is.null(outcome_vars)) {
#'     dt_outcome <- data %>%
#'       dplyr::filter(wave %in% c(baseline_wave, outcome_wave)) %>%
#'       dplyr::select(all_of(c(outcome_vars, "wave"))) %>%
#'       droplevels()
#'
#'     table_outcomes <- dt_outcome %>%
#'       janitor::clean_names(case = "title") %>%
#'       labelled::to_factor() %>%
#'       gtsummary::tbl_summary(
#'         by = "Wave",
#'         missing = "always",
#'         percent = "column",
#'         label = outcome_labels
#'       ) %>%
#'       gtsummary::modify_header(label = "**Outcome Variables by Wave**") %>%
#'       gtsummary::bold_labels()
#'   }
#'
#'   update_progress()
#'
#'   # Outcome variables plot (if outcome_vars provided and create_plots is TRUE)
#'   if (!is.null(outcome_vars) && create_plots) {
#'     outcome_long <- dt_outcome %>%
#'       tidyr::pivot_longer(cols = all_of(outcome_vars), names_to = "Variable", values_to = "Value")
#'
#'     # Format variable names
#'     outcome_long$Variable <- sapply(outcome_long$Variable, format_var_name)
#'
#'     if (plot_type == "boxplot") {
#'       outcome_plot <- ggplot2::ggplot(outcome_long, ggplot2::aes(x = Variable, y = Value, fill = wave)) +
#'         ggplot2::geom_boxplot(notch = TRUE) +
#'         ggplot2::labs(
#'           title = "Distribution of Outcome Variables by Wave",
#'           x = "Outcome Variables",
#'           y = "Value"
#'         ) +
#'         ggplot2::theme_minimal() +
#'         ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
#'         ggplot2::scale_fill_manual(values = okabe_ito_palette())
#'     } else if (plot_type == "density") {
#'       outcome_plot <- ggplot2::ggplot(outcome_long, ggplot2::aes(x = Value, fill = wave)) +
#'         ggplot2::geom_density(alpha = 0.7) +
#'         ggplot2::facet_wrap(~Variable, scales = "free") +
#'         ggplot2::labs(
#'           title = "Density Distribution of Outcome Variables by Wave",
#'           x = "Value",
#'           y = "Density"
#'         ) +
#'         ggplot2::theme_minimal() +
#'         ggplot2::scale_fill_manual(values = okabe_ito_palette())
#'     }
#'
#'     plots$outcome_plot <- outcome_plot
#'
#'     cli::cli_alert_success("Plots generated successfully 📈")
#'
#'     update_progress()
#'   }
#'
#'   # Display summary information
#'   cli::cli_h2("Summary Information")
#'   cli::cli_ul()
#'   cli::cli_li("Baseline wave: {.val {baseline_wave}}")
#'   cli::cli_li("Exposure waves: {.val {paste(exposure_waves, collapse = ', ')}}")
#'   cli::cli_li("Outcome wave: {.val {paste(outcome_wave, collapse = ' and ')}}")
#'   cli::cli_li("Exposure variables: {.val {paste(name_exposure, collapse = ', ')}}")
#'   if (!is.null(name_exposure_cat)) {
#'     cli::cli_li("Categorical exposure variable: {.val {name_exposure_cat}}")
#'   }
#'   cli::cli_end()
#'
#'   # Display exposure summaries
#'   cli::cli_h2("Exposure Summaries")
#'   for (exposure_var in name_exposure) {
#'     cli::cli_alert_info(
#'       paste("Summary for", exposure_var) %>%
#'         crayon::bold() %>%
#'         crayon::blue()
#'     )
#'     for (summary in exposure_summaries[[exposure_var]]) {
#'       cli::cli_ul()
#'       cli::cli_li("Wave {.val {summary$wave}}:")
#'       cli::cli_li("  Mean: {.val {round(summary$mean_exposure, 2)}}")
#'       cli::cli_li("  SD: {.val {round(summary$sd_exposure, 2)}}")
#'       cli::cli_li("  Median: {.val {round(summary$median_exposure, 2)}}")
#'       cli::cli_end()
#'     }
#'   }
#'
#'   update_progress()
#'
#'   # Close progress bar
#'   if (show_progress && !is.null(pb)) {
#'     tryCatch({
#'       cli::cli_progress_done(id = pb)
#'     }, error = function(e) {
#'       warning("Error closing progress bar: ", e$message)
#'     })
#'   }
#'
#'   # Final success message
#'   cli::cli_alert_success("Summary tables, information, and optional plots generated successfully \U0001F44D")
#'
#'   # Return results
#'   return(list(
#'     baseline_table = table_baseline,
#'     exposure_tables = exposure_tables,
#'     outcome_table = table_outcomes,
#'     n_participants = n_participants_formatted,
#'     exposure_summaries = exposure_summaries,
#'     plots = plots
#'   ))
#' }
#'
#' # helper plot for color palette (assuming `okabe_ito_palette` is defined)
#' #' @keywords internal
#' margot_plot_exposure_boxplot <- function(data,
#'                                          exposure_var,
#'                                          waves,
#'                                          baseline_wave,
#'                                          y_label = NULL,
#'                                          x_label = "Wave",
#'                                          show_points = FALSE,
#'                                          point_alpha = 0.05,
#'                                          point_size = 0.5) {
#'
#'   require(ggplot2)
#'   require(dplyr)
#'
#'   # Ensure wave column is present
#'   if (!"wave" %in% names(data)) {
#'     stop("The 'wave' column is missing from the data.")
#'   }
#'
#'   # Ensure waves are unique and in the correct order
#'   all_waves <- unique(c(baseline_wave, waves))
#'
#'   # Convert wave to factor with correct levels
#'   data$wave <- factor(data$wave, levels = all_waves)
#'
#'   # Filter waves
#'   df <- data %>% dplyr::filter(wave %in% all_waves)
#'
#'   # Function to convert to title case and remove underscores
#'   format_label <- function(x) {
#'     stringr::str_to_title(gsub("_", " ", x))
#'   }
#'
#'   # Format labels
#'   exposure_label <- format_label(exposure_var)
#'
#'   # Determine y-axis limits
#'   y_min <- min(df[[exposure_var]], na.rm = TRUE)
#'   y_max <- max(df[[exposure_var]], na.rm = TRUE)
#'   if (y_min >= 1 && y_max <= 7) {
#'     y_limits <- c(1, 7)
#'   } else {
#'     y_limits <- c(y_min, y_max)
#'   }
#'
#'   # Get the number of waves
#'   n_waves <- length(all_waves)
#'
#'   # Create a color palette that recycles if needed
#'   okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
#'                          "#0072B2", "#D55E00", "#CC79A7", "#999999")
#'   color_palette <- rep(okabe_ito_palette, length.out = n_waves)
#'
#'   # Create the ggplot
#'   p <- ggplot(df, aes(x = wave, y = !!sym(exposure_var), fill = wave)) +
#'     geom_boxplot() +
#'     theme_minimal() +
#'     scale_fill_manual(values = color_palette) +
#'     scale_x_discrete(drop = FALSE) + # Force all levels to be shown
#'     theme(
#'       legend.position = "none",
#'       axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#'       strip.text = element_text(size = 10),
#'       panel.spacing = unit(0.2, "lines")
#'     ) +
#'     labs(
#'       title = paste("Distribution of", exposure_label, "by Wave"),
#'       y = y_label %||% exposure_label,
#'       x = x_label
#'     ) +
#'     scale_y_continuous(limits = y_limits)
#'
#'   # Add points if requested
#'   if (show_points) {
#'     p <- p + geom_jitter(aes(color = wave), width = 0.2, alpha = point_alpha, size = point_size) +
#'       scale_color_manual(values = color_palette)
#'   }
#'
#'   return(p)
#' }
