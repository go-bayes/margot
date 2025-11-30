#' create boxplots with covariates using ggplot2
#'
#' this function creates boxplots for one outcome variable across specified panel waves,
#' allowing for different groups as covariates. it combines features from
#' margot_plot_boxplot() and margot_plot_slope_covariate().
#'
#' @param data a data frame containing the variables to be plotted.
#' @param outcome the name of the outcome variable to be plotted.
#' @param covariate the name of the covariate variable for grouping.
#' @param waves a vector of wave values to include in the plot (default is NULL, which includes all waves).
#' @param id_col name of the column containing unique identifiers (default is "id").
#' @param title the title of the plot (optional, auto-generated if NULL).
#' @param y_label the label for the y-axis (optional).
#' @param x_label the label for the x-axis (optional, defaults to "Wave").
#' @param color_label the label for the color legend (optional, defaults to the covariate name).
#' @param show_points logical, whether to show individual data points (default is FALSE).
#' @param point_alpha alpha value for data points if shown (default is 0.05).
#' @param point_size size of data points if shown (default is 0.5).
#' @param include_timestamp logical, whether to include timestamp in plot title and filename (default is FALSE).
#' @param save_path path to save the plot (optional).
#' @param prefix optional prefix for the saved file name (default is NULL).
#' @param width width of the saved plot in inches (default is 12).
#' @param height height of the saved plot in inches (default is 8).
#' @param legend_position position of the legend (default is "right").
#' @param y_limits y-axis limits (optional).
#' @param coord_flip logical, whether to flip the coordinates of the plot (default is FALSE).
#' @param ... additional arguments passed to geom_boxplot().
#'
#' @return a ggplot object representing the boxplot with covariates.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_minimal geom_jitter theme element_text unit scale_x_discrete scale_y_continuous labs coord_flip ggsave
#' @importFrom dplyr filter n_distinct
#' @importFrom rlang sym
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_danger
#' @importFrom stringr str_to_title
#' @importFrom ggokabeito scale_fill_okabe_ito scale_color_okabe_ito
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # example 1: basic usage with all waves
#' p1 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_climate_chg_concern",
#'   covariate = "education",
#'   id_col = "id"
#' )
#'
#' # example 2: plotting specific waves with custom labels
#' p2 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "political_orientation",
#'   covariate = "age_group",
#'   waves = c(2021, 2022, 2023),
#'   y_label = "Political Orientation",
#'   color_label = "Age Group",
#'   id_col = "id"
#' )
#'
#' # example 3: showing individual points and flipping coordinates
#' p3 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_sat_nz_environment",
#'   covariate = "income_bracket",
#'   show_points = TRUE,
#'   coord_flip = TRUE,
#'   y_label = "Satisfaction with NZ Environment",
#'   color_label = "Income Bracket",
#'   id_col = "id"
#' )
#'
#' # example 4: customizing plot appearance and saving
#' p4 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "envefficacy",
#'   covariate = "gender",
#'   y_label = "Environmental Efficacy",
#'   color_label = "Gender",
#'   legend_position = "bottom",
#'   y_limits = c(1, 7),
#'   save_path = "path/to/save",
#'   prefix = "env_efficacy",
#'   width = 10,
#'   height = 6,
#'   id_col = "id"
#' )
#'
#' # example 5: using with categorical outcome and including timestamp
#' p5 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_climate_chg_cause",
#'   covariate = "political_party",
#'   y_label = "Perceived Cause of Climate Change",
#'   color_label = "Political Party",
#'   include_timestamp = TRUE,
#'   id_col = "id"
#' )
#' }
margot_plot_boxplot_covariate <- function(data,
                                          outcome,
                                          covariate,
                                          waves = NULL,
                                          id_col = "id",
                                          title = NULL,
                                          y_label = NULL,
                                          x_label = "Wave",
                                          color_label = NULL,
                                          show_points = FALSE,
                                          point_alpha = 0.05,
                                          point_size = 0.5,
                                          include_timestamp = FALSE,
                                          save_path = NULL,
                                          prefix = NULL,
                                          width = 16,
                                          height = 8,
                                          legend_position = "right",
                                          y_limits = NULL,
                                          coord_flip = FALSE,
                                          ...) {
  cli::cli_h1("Margot Plot Boxplot with Covariate")

  tryCatch(
    {
      cli::cli_alert_info("Preparing data...")

      # prepare the data
      df <- data

      # ensure wave column is present
      if (!"wave" %in% names(df)) {
        stop("The 'wave' column is missing from the data.")
      }

      # convert wave to factor and ensure all specified waves are included
      if (!is.null(waves)) {
        df$wave <- factor(df$wave, levels = waves)
      } else {
        df$wave <- as.factor(df$wave)
      }

      # filter waves if specified
      if (!is.null(waves)) {
        df <- df %>% dplyr::filter(wave %in% waves)
      }

      # function to convert to title case and remove underscores
      format_label <- function(x) {
        stringr::str_to_title(gsub("_", " ", x))
      }

      # format labels
      outcome_label <- format_label(outcome)
      covariate_label <- format_label(covariate)

      # remove NAs and count participants who responded
      df <- df %>%
        dplyr::filter(!is.na(!!rlang::sym(outcome)) & is.finite(!!rlang::sym(outcome)) &
          !is.na(!!rlang::sym(covariate)) & is.finite(!!rlang::sym(covariate)))

      # count total unique participants
      total_unique <- dplyr::n_distinct(df[[id_col]])

      # calculate total observations
      total_obs <- nrow(df)

      # determine the title
      if (is.null(title)) {
        title <- sprintf(
          "Distribution of %s by Wave and %s\nTotal N = %d unique participants, %d observations",
          outcome_label, covariate_label, total_unique, total_obs
        )
      }

      if (include_timestamp) {
        title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }

      # determine y-axis limits
      if (is.null(y_limits)) {
        y_min <- min(df[[outcome]], na.rm = TRUE)
        y_max <- max(df[[outcome]], na.rm = TRUE)
        if (y_min >= 1 && y_max <= 7) {
          y_limits <- c(1, 7)
        } else {
          y_limits <- c(y_min, y_max)
        }
      }

      cli::cli_alert_success("Data prepared successfully")

      cli::cli_alert_info("Creating plot...")

      # create the ggplot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = wave, y = !!rlang::sym(outcome), fill = !!rlang::sym(covariate))) +
        ggplot2::geom_boxplot(...) +
        ggplot2::theme_minimal() +
        ggokabeito::scale_fill_okabe_ito() +
        ggplot2::scale_x_discrete(drop = FALSE) + # force all levels to be shown
        ggplot2::theme(
          legend.position = legend_position,
          legend.text = ggplot2::element_text(size = 10),
          legend.title = ggplot2::element_text(size = 12),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          strip.text = ggplot2::element_text(size = 10),
          panel.spacing = ggplot2::unit(0.2, "lines")
        ) +
        ggplot2::labs(
          title = title,
          y = y_label %||% outcome_label,
          x = x_label,
          fill = color_label %||% covariate_label
        ) +
        ggplot2::scale_y_continuous(limits = y_limits)

      # add points if requested
      if (show_points) {
        p <- p + ggplot2::geom_jitter(ggplot2::aes(color = !!rlang::sym(covariate)), width = 0.2, alpha = point_alpha, size = point_size) +
          ggokabeito::scale_color_okabe_ito()
      }

      # flip coordinates if requested
      if (coord_flip) {
        p <- p + ggplot2::coord_flip()

        # adjust text angle for better readability when coordinates are flipped
        p <- p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
          axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 8)
        )
      }

      cli::cli_alert_success("Plot created")

      # save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- "boxplot_covariate"

        # add the optional prefix
        if (!is.null(prefix) && nzchar(prefix)) {
          filename <- paste0(prefix, "_", filename)
        }

        filename <- paste0(filename, "_", outcome, "_by_", covariate)

        if (include_timestamp) {
          filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        }

        cli::cli_alert_info("Saving plot...")

        ggplot2::ggsave(
          plot = p,
          filename = file.path(save_path, paste0(filename, ".png")),
          width = width,
          height = height,
          units = "in",
          dpi = 300
        )

        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

        cli::cli_alert_success("Plot saved successfully")
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot boxplot with covariate created successfully")

      return(p)
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    }
  )
}
