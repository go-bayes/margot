#' Plot Panel Study Response Timeline
#'
#' This function creates a ggplot2 visualization of a panel study response timeline.
#'
#' @param df_timeline A data frame containing the processed timeline data, typically output from `prepare_panel_data()`.
#' @param n_total_participants The total number of unique participants. If NULL, it will be extracted from df_timeline if available.
#' @param save Logical. If TRUE, saves the plot as a PNG file and a qs file. Default is FALSE.
#' @param save_path The directory path to save the plot. Default is "output" in the current working directory.
#' @param width The width of the saved plot in inches. Default is 12.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param base_filename The base filename for saving the plot. Default is "timeline_histogram".
#' @param title The main title for the plot. Default is "Panel Study Timeline".
#' @param x_label The label for the x-axis. Default is "Date".
#' @param y_label The label for the y-axis. Default is "Count of Responses".
#' @param color_palette A vector of colors to use for the waves. If NULL, uses a default color-blind friendly palette.
#'
#' @return A ggplot2 object representing the panel study response timeline.
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(dplyr)
#' library(lubridate)
#' library(ggplot2)
#' library(here)
#'
#' # Assume we have a data frame 'nzavs_data' with columns: id, wave, tscore
#'
#' # Step 1: Define NZAVS-specific wave breaks
#' nzavs_wave_breaks <- list(
#'   "time 1" = c(as.Date("2009-08-30"), as.Date("2010-10-15")),
#'   "time 2" = c(as.Date("2010-10-15"), as.Date("2011-10-15")),
#'   "time 3" = c(as.Date("2011-10-15"), as.Date("2012-10-15")),
#'   "time 4" = c(as.Date("2012-10-15"), as.Date("2013-10-15")),
#'   "time 5" = c(as.Date("2013-10-15"), as.Date("2014-10-15")),
#'   "time 6" = c(as.Date("2014-10-15"), as.Date("2015-10-15")),
#'   "time 7" = c(as.Date("2015-10-15"), as.Date("2016-10-15")),
#'   "time 8" = c(as.Date("2016-10-15"), as.Date("2017-10-15")),
#'   "time 9" = c(as.Date("2017-10-15"), as.Date("2018-10-15")),
#'   "time 10" = c(as.Date("2018-10-15"), as.Date("2019-10-15")),
#'   "time 11" = c(as.Date("2019-10-15"), as.Date("2020-10-15")),
#'   "time 12" = c(as.Date("2020-10-15"), as.Date("2021-10-15")),
#'   "time 13" = c(as.Date("2021-10-15"), as.Date("2022-10-15")),
#'   "time 14" = c(as.Date("2022-10-15"), as.Date("2023-10-15"))
#' )
#'
#' # Step 2: Prepare the NZAVS data
#' prepared_data <- prepare_panel_data(
#'   dat = nzavs_data,
#'   wave_col = "wave",
#'   tscore_col = "tscore",
#'   id_col = "id",
#'   base_date = as.Date("2009-06-30"),
#'   wave_breaks = nzavs_wave_breaks
#' )
#'
#' # Step 3: Create the NZAVS timeline plot
#' nzavs_timeline <- margot_plot_response_timeline(
#'   df_timeline = prepared_data$df_timeline,
#'   n_total_participants = prepared_data$n_total_participants,
#'   save = TRUE,
#'   save_path = here::here("output", "plots"),
#'   title = "New Zealand Attitudes and Values Study (panel)",
#'   x_label = paste("NZAVS years", min(year(prepared_data$df_timeline$day)),
#'                   "-", max(year(prepared_data$df_timeline$day)),
#'                   "cohort: daily counts by condition"),
#'   y_label = "Count of Responses"
#' )
#'
#' # Display the plot
#' print(nzavs_timeline)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous labs theme_classic scale_fill_manual theme element_text ggsave
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @export
margot_plot_response_timeline <- function(df_timeline,
                                          n_total_participants = NULL,
                                          save = FALSE,
                                          save_path = here::here("output"),
                                          width = 12,
                                          height = 8,
                                          base_filename = "timeline_histogram",
                                          title = "Panel Study Timeline",
                                          x_label = "Date",
                                          y_label = "Count of Responses",
                                          color_palette = NULL) {
  cli::cli_alert_info("Starting to create response timeline plot...")

  # Check if required columns exist
  required_cols <- c("day", "n_responses", "wave")
  missing_cols <- setdiff(required_cols, names(df_timeline))
  if (length(missing_cols) > 0) {
    cli::cli_alert_danger(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    stop("Missing required columns")
  }

  # Ensure day column is Date type
  if (!inherits(df_timeline$day, "Date")) {
    cli::cli_alert_warning("Converting 'day' column to Date type")
    df_timeline$day <- as.Date(df_timeline$day)
  }

  # Calculate year range
  year_range <- range(lubridate::year(df_timeline$day), na.rm = TRUE)

  # Calculate total responses
  total_responses <- sum(df_timeline$n_responses, na.rm = TRUE)

  # Handle n_total_participants
  if (is.null(n_total_participants) && "n_total_participants" %in% names(df_timeline)) {
    n_total_participants <- df_timeline$n_total_participants[1]
    cli::cli_alert_info("Using n_total_participants from df_timeline")
  }

  # Prepare subtitle
  if (!is.null(n_total_participants)) {
    subtitle <- sprintf("N = %s participants, %s responses; years %.0f - %.0f",
                        format(n_total_participants, big.mark = ","),
                        format(total_responses, big.mark = ","),
                        year_range[1], year_range[2])
  } else {
    subtitle <- sprintf("N = %s responses; years %.0f - %.0f",
                        format(total_responses, big.mark = ","),
                        year_range[1], year_range[2])
  }

  # Set default color palette if not provided
  if (is.null(color_palette)) {
    color_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
                       "#D55E00", "#CC79A7", "#000000", "#999999")
  }

  # Get the number of unique waves
  n_waves <- length(unique(df_timeline$wave))

  # Explicitly recycle colors
  recycled_colors <- rep_len(color_palette, length.out = n_waves)

  cli::cli_alert_info("Creating ggplot...")

  # Create ggplot
  gg <- ggplot2::ggplot(df_timeline, ggplot2::aes(x = day, y = n_responses, fill = wave)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = recycled_colors) +
    ggplot2::theme(
      legend.position = "none",
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12)
    )

  cli::cli_alert_success("ggplot created successfully")

  # Save ggplot if save is TRUE
  if (save) {
    cli::cli_alert_info("Saving plot...")
    tryCatch({
      margot::here_save_qs(
        obj = gg,
        name = base_filename,
        dir_path = save_path,
        preset = "high",
        nthreads = 1
      )
      ggplot2::ggsave(
        plot = gg,
        filename = file.path(save_path, paste0(base_filename, ".png")),
        width = width,
        height = height,
        units = "in",
        dpi = 400
      )
      cli::cli_alert_success("Plot saved successfully")
    }, error = function(e) {
      cli::cli_alert_danger(paste("Failed to save plot:", e$message))
    })
  }

  cli::cli_alert_success(paste("Response timeline plot creation complete!", "\U0001F44D"))

  # Return the ggplot object
  return(gg)
}
