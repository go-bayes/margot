#' Create Tau Hat Plot
#'
#' @description
#' Creates a histogram plot of tau hat values for each treatment comparison.
#'
#' @param tau_hat A matrix of estimated treatment effects.
#' @param outcome A character string specifying the name of the outcome variable.
#'
#' @return A ggplot object representing the distribution of tau hat values.
#'
#' @importFrom ggplot2 ggplot geom_histogram theme_minimal labs facet_wrap
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
create_tau_hat_plot <- function(tau_hat, outcome) {
  tau_hat_df <- as.data.frame(tau_hat)
  tau_hat_df_long <- tau_hat_df %>%
    tidyr::pivot_longer(cols = everything(), names_to = "comparison", values_to = "tau_value")

  ggplot2::ggplot(tau_hat_df_long, ggplot2::aes(x = tau_value, fill = comparison)) +
    ggplot2::geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Distribution of tau.hat for", outcome),
                  x = "tau.hat", y = "Count") +
    ggplot2::facet_wrap(~ comparison, scales = "free_y")
}
