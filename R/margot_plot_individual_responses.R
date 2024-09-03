#' Create Individual Response Plots
#'
#' This function creates a ggplot2 visualization of individual responses over time for one or more variables.
#' It allows for flexible data filtering, sampling, and customization of the plot appearance.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A character vector of column names in `data` to be plotted on the y-axis.
#' @param id_col The name of the column in `data` that contains unique identifiers for individuals. Default is "id".
#' @param wave_col The name of the column in `data` that contains the wave or time information. Default is "wave".
#' @param waves An optional vector of wave values to include in the plot. If NULL, all waves are included.
#' @param data_fraction The fraction of data to use (between 0 and 1). Default is 1 (use all data).
#' @param random_draws The number of random individuals to plot. If specified, overrides `data_fraction`.
#' @param title An optional title for the plot.
#' @param y_label An optional label for the y-axis.
#' @param x_label An optional label for the x-axis.
#' @param color_palette An optional vector of colors to use for the variables.
#' @param theme A ggplot2 theme to use for the plot. Default is theme_classic().
#' @param include_timestamp Logical, whether to include a timestamp in the saved file name. Default is FALSE.
#' @param save_path An optional file path to save the plot.
#' @param width The width of the saved plot in inches. Default is 16.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param seed An optional seed for reproducibility when sampling data.
#' @param wave_label_angle The angle of the x-axis labels in degrees. Default is 45.
#' @param eligibility_all Logical, whether to require responses in all waves (TRUE) or at least one wave (FALSE). Default is FALSE.
#' @param full_response_scale Logical, whether to use the full response scale for the y-axis. Default is TRUE.
#' @param scale_range An optional numeric vector of length 2 specifying the range for the y-axis. If NULL, the range is determined from the data.
#'
#' @return A ggplot2 object representing the individual response plot.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with default settings
#' plot1 <- margot_plot_individual_responses(
#'   data = your_data,
#'   y_vars = c("variable1", "variable2"),
#'   id_col = "participant_id",
#'   wave_col = "year"
#' )
#'
#' # Example 2: Plotting specific waves and using random draws
#' plot2 <- margot_plot_individual_responses(
#'   data = your_data,
#'   y_vars = c("score1", "score2", "score3"),
#'   waves = c(2020, 2021, 2022),
#'   random_draws = 50,
#'   title = "Individual Scores Over Time",
#'   y_label = "Score",
#'   x_label = "Year",
#'   seed = 123
#' )
#'
#' # Example 3: Using full response scale with a specified range
#' plot3 <- margot_plot_individual_responses(
#'   data = your_data,
#'   y_vars = c("measure1", "measure2"),
#'   full_response_scale = TRUE,
#'   scale_range = c(0, 10),
#'   theme = theme_minimal(),
#'   wave_label_angle = 90
#' )
#'
#' # Example 4: Edge case with minimal random draws and specific settings
#' plot4 <- margot_plot_individual_responses(
#'   data = dat_long_table,
#'   y_vars = outcome_vars,
#'   id_col = "id",
#'   waves = c("2020", "2021", "2022"),
#'   random_draws = 2,
#'   theme = theme_classic(),
#'   save_path = here::here(push_mods),
#'   seed = 124,
#'   eligibility_all = TRUE,
#'   scale_range = c(0, 10)
#' )
#'
#' # Example 5: Using with longitudinal survey data and requiring responses in all waves
#' plot5 <- margot_plot_individual_responses(
#'   data = survey_data,
#'   y_vars = c("life_satisfaction", "health_status", "income"),
#'   id_col = "respondent_id",
#'   wave_col = "survey_year",
#'   waves = seq(2010, 2020, by = 2),
#'   random_draws = 100,
#'   title = "Changes in Life Measures (2010-2020)",
#'   y_label = "Measure Value",
#'   x_label = "Survey Year",
#'   eligibility_all = TRUE,
#'   full_response_scale = TRUE,
#'   scale_range = c(0, 100)
#' )
#' }
#'
#' @export
margot_plot_individual_responses <- function(data,
                                             y_vars,
                                             id_col = "id",
                                             wave_col = "wave",
                                             waves = NULL,
                                             data_fraction = 1,
                                             random_draws = NULL,
                                             title = NULL,
                                             y_label = NULL,
                                             x_label = NULL,
                                             color_palette = NULL,
                                             theme = theme_classic(),
                                             include_timestamp = FALSE,
                                             save_path = NULL,
                                             width = 16,
                                             height = 8,
                                             seed = NULL,
                                             wave_label_angle = 45,
                                             eligibility_all = FALSE,
                                             full_response_scale = TRUE,
                                             scale_range = NULL) {

  cli::cli_h1("Margot Plot Individual Responses")

  # check for required columns
  required_cols <- c(id_col, wave_col, y_vars)
  if (!all(required_cols %in% colnames(data))) {
    missing_cols <- setdiff(required_cols, colnames(data))
    cli::cli_alert_danger("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    return(NULL)
  }

  # prepare the data
  cli::cli_alert_info("Preparing data...")

  # filter waves if specified
  if (!is.null(waves)) {
    cli::cli_alert_info("Filtering waves...")
    data <- data[data[[wave_col]] %in% waves, ]
  }

  # determine number of waves
  unique_waves <- unique(data[[wave_col]])
  num_waves <- length(unique_waves)

  # calculate eligibility
  cli::cli_alert_info("Calculating eligibility...")

  # optimise eligibility calculation
  eligibility_data <- data %>%
    group_by(!!sym(id_col)) %>%
    summarize(across(all_of(c(wave_col, y_vars)),
                     list(responded = ~sum(!is.na(.)),
                          waves = ~n_distinct(!!sym(wave_col)))),
              .groups = "drop")

  # set up progress bar
  total_steps <- nrow(eligibility_data)
  cli::cli_progress_bar(
    total = total_steps,
    format = "{cli::pb_spin} Calculating eligibility: {cli::pb_current}/{cli::pb_total} [{cli::pb_bar}] ETA: {cli::pb_eta}",
    clear = FALSE
  )

  # calculate eligibility with progress updates
  eligible_ids_one_wave <- c()
  eligible_ids_all_waves <- c()

  for (i in seq_len(nrow(eligibility_data))) {
    row <- eligibility_data[i, ]

    if (any(row[, grepl("_responded$", names(row))] > 0)) {
      eligible_ids_one_wave <- c(eligible_ids_one_wave, row[[id_col]])
    }

    if (all(row[, grepl("_responded$", names(row))] == row[[paste0(wave_col, "_waves")]])) {
      eligible_ids_all_waves <- c(eligible_ids_all_waves, row[[id_col]])
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # select the appropriate eligible IDs based on eligibility_all
  eligible_ids <- if (eligibility_all) eligible_ids_all_waves else eligible_ids_one_wave
  data <- data[data[[id_col]] %in% eligible_ids, ]

  # sample IDs if data_fraction < 1 or random_draws is specified
  if (data_fraction < 1 || !is.null(random_draws)) {
    cli::cli_alert_info("Sampling IDs...")
    if (!is.null(seed)) set.seed(seed)

    unique_ids <- unique(data[[id_col]])
    n_ids <- length(unique_ids)

    if (!is.null(random_draws)) {
      sample_size <- min(random_draws, n_ids)
    } else {
      sample_size <- max(1, round(n_ids * data_fraction))
    }

    sampled_ids <- sample(unique_ids, size = sample_size)
    data <- data[data[[id_col]] %in% sampled_ids, ]
  }

  cli::cli_alert_info("Pivoting data...")
  df <- tidyr::pivot_longer(data, cols = all_of(y_vars), names_to = "variable", values_to = "value")

  cli::cli_alert_info("Formatting variable names...")
  df$variable <- gsub("_", " ", df$variable)
  df$variable <- tools::toTitleCase(df$variable)

  cli::cli_alert_info("Removing NA values...")
  df <- df[!is.na(df$value), ]

  # determine eligible cases
  cli::cli_alert_info("Determining eligible cases...")
  eligible_count_one_wave <- length(eligible_ids_one_wave)
  eligible_count_all_waves <- length(eligible_ids_all_waves)
  total_ids <- length(unique(data[[id_col]]))

  eligibility_percentage <- if (eligibility_all) {
    (eligible_count_all_waves / eligible_count_one_wave) * 100
  } else {
    (eligible_count_one_wave / total_ids) * 100
  }

  eligibility_message <- sprintf(
    "Eligibility:\n- %d out of %d IDs (%.2f%%) responded in at least one wave.\n- %d out of %d IDs (%.2f%%) responded in all waves.\nCriteria used: Responded to %s in %d total waves.",
    eligible_count_one_wave, total_ids, (eligible_count_one_wave / total_ids) * 100,
    eligible_count_all_waves, eligible_count_one_wave, (eligible_count_all_waves / eligible_count_one_wave) * 100,
    if (eligibility_all) "all variables in all waves" else "at least one variable in at least one wave",
    num_waves
  )
  cli::cli_alert_info(eligibility_message)


  # determine y-axis limits if full_response_scale is TRUE
  if (full_response_scale) {
    cli::cli_alert_info("Calculating response scale limits...")

    if (is.null(scale_range)) {
      # if scale_range is not provided, use the observed ranges in the data
      y_limits <- range(sapply(y_vars, function(var) range(data[[var]], na.rm = TRUE)))
      cli::cli_alert_info("No scale_range provided. Using observed data range: {y_limits[1]} to {y_limits[2]}")
    } else {
      # verify that scale_range is correctly specified
      if (length(scale_range) != 2 || !is.numeric(scale_range) || scale_range[1] >= scale_range[2]) {
        cli::cli_alert_danger("Invalid scale_range. Using observed data range.")
        y_limits <- range(sapply(y_vars, function(var) range(data[[var]], na.rm = TRUE)))
      } else {
        # use the provided scale range
        y_limits <- scale_range
        cli::cli_alert_info("Using provided scale range: {y_limits[1]} to {y_limits[2]}")
      }
    }
  }

  # Create the plot
  cli::cli_alert_info("Creating plot...")

  p <- ggplot(df, aes(x = !!sym(wave_col), y = value, color = variable, group = interaction(!!sym(id_col), variable))) +
    geom_point() +
    geom_line(data = function(d) d[ave(seq_along(d$value), d[[id_col]], d$variable, FUN = length) > 1, ]) +
    facet_wrap(as.formula(paste("~", id_col))) +
    theme +
    theme(axis.text.x = element_text(angle = wave_label_angle, hjust = 1)) +
    labs(title = title,
         y = y_label %||% "Value",
         x = x_label %||% "Wave",
         color = "Variable")

  # Apply y-axis limits if full_response_scale is TRUE
  if (full_response_scale) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  # Apply color palette
  if (is.null(color_palette)) {
    color_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
                       "#D55E00", "#CC79A7", "#000000", "#999999")
  }
  p <- p + scale_color_manual(values = color_palette)

  cli::cli_alert_success("Plot created successfully \U0001F44D")

  # save plot if a save path is provided
  if (!is.null(save_path)) {
    cli::cli_alert_info("Saving plot...")
    filename <- "individual_responses_plot"
    if (include_timestamp) {
      filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    full_path <- file.path(save_path, paste0(filename, ".png"))
    ggsave(
      plot = p,
      filename = full_path,
      width = width,
      height = height,
      units = "in",
      dpi = 300
    )
    cli::cli_alert_success("Plot saved successfully \U0001F44D")
    cli::cli_alert_info("File saved at: {.file {full_path}}")
  }

  return(p)
}
