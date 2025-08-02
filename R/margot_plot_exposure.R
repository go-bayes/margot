#' Create a separate exposure plot
#'
#' @param data A data frame containing the exposure data
#' @param name_exposure The name of the exposure variable
#' @param exposure_waves A vector of wave numbers for exposure measurements
#' @param baseline_wave The wave number for baseline measurements
#'
#' @return A ggplot object representing the exposure plot
#'
#' @importFrom dplyr group_by summarize
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_point labs theme_minimal theme element_text annotate
#' @importFrom ggokabeito scale_fill_okabe_ito
#' @importFrom cli cli_alert_success
margot_plot_exposure <- function(data, name_exposure, exposure_waves, baseline_wave) {
  # Calculate mean exposure for each wave
  mean_exposure <- data |>
    group_by(wave) |>
    summarize(mean_exposure = mean(.data[[name_exposure]], na.rm = TRUE))

  # Create the plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = wave, y = .data[[name_exposure]], fill = wave)) +
    ggplot2::geom_violin(alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.1, color = "black", alpha = 0.7) +
    ggplot2::geom_point(data = mean_exposure, ggplot2::aes(y = mean_exposure), color = "red", size = 3) +
    ggplot2::labs(
      title = paste("Distribution of", name_exposure, "by Wave"),
      x = "Wave",
      y = name_exposure
    ) +
    ggplot2::theme_minimal() +
    scale_fill_okabe_ito() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::annotate("text",
      x = 1:length(c(baseline_wave, exposure_waves)),
      y = max(data[[name_exposure]], na.rm = TRUE),
      label = c(baseline_wave, exposure_waves),
      vjust = -0.5
    )

  # Add progress update with thumbs up
  cli::cli_alert_success("Exposure plot created successfully \U0001F44D")

  return(plot)
}
