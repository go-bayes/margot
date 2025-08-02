#' Create a Combined Slope Plot using ggeffects and patchwork
#'
#' This function creates multiple ggplot2 visualizations using ggeffects to calculate
#' predicted responses from models for multiple outcome variables and combines them
#' into a single plot using the patchwork package. It allows flexible specification
#' of the models and plotting options, including layout and annotations.
#'
#' @param data A data frame containing the variables for the models.
#' @param outcome_vars A character vector specifying the outcome variables to be modeled.
#' @param exposure_formula A formula specifying the exposure variables (right-hand side of the model).
#' @param terms A character vector specifying the terms to be used in predict_response.
#' @param label_mapping An optional named list mapping outcome variables to custom y-axis labels.
#' @param x_label An optional label for the x-axis. If NULL, the first term will be used.
#' @param color_label An optional label for the color legend. If NULL, the second term will be used.
#' @param save_path An optional path to save the combined plot. If NULL, the plot will not be saved.
#' @param file_prefix Optional prefix for the saved file name (default is "plot_slope_covariate_batch").
#' @param ncol Number of columns in the combined plot layout.
#' @param nrow Number of rows in the combined plot layout.
#' @param guides How to combine legends. Default is "collect".
#' @param patchwork_params A list of additional parameters to be passed to patchwork::plot_layout().
#' @param plot_annotation_params A list of parameters to be passed to patchwork::plot_annotation().
#' @param include_individual_titles Logical, whether to include titles in individual plots (default is FALSE).
#' @param width Width of the combined plot in inches. Default is 12.
#' @param height Height of the combined plot in inches. Default is 8.
#' @param dpi Resolution of the saved plot. Default is 400.
#' @param ... Additional arguments to be passed to margot_plot_slope_covariate().
#'
#' @return A ggplot2 object representing the combined plot.
#'
#' @importFrom ggplot2 element_text ggsave
#' @importFrom ggeffects predict_response
#' @importFrom ggokabeito scale_color_okabe_ito
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom dplyr filter select
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
#' @importFrom tools toTitleCase
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define outcome variables and label mapping
#' outcome_vars <- c("var1", "var2", "var3")
#' label_mapping <- list("var1" = "Variable 1", "var2" = "Variable 2", "var3" = "Variable 3")
#'
#' # Create combined plot
#' combined_plot <- margot_plot_slope_covariate_batch(
#'   data = dat,
#'   outcome_vars = outcome_vars,
#'   exposure_formula = "~ wave:covariate",
#'   terms = c("wave", "covariate"),
#'   label_mapping = label_mapping,
#'   x_label = "Time",
#'   color_label = "Covariate",
#'   ncol = 2,
#'   plot_annotation_params = list(
#'     title = "Combined Slope Plots",
#'     subtitle = "Subtitle for the combined plot"
#'   ),
#'   save_path = "path/to/save/directory",
#'   width = 14,
#'   height = 10
#' )
#' }
margot_plot_slope_covariate_batch <- function(data,
                                              outcome_vars,
                                              exposure_formula,
                                              terms,
                                              label_mapping = NULL,
                                              x_label = NULL,
                                              color_label = NULL,
                                              save_path = NULL,
                                              file_prefix = "plot_slope_covariate_batch",
                                              ncol = NULL,
                                              nrow = NULL,
                                              guides = "collect",
                                              patchwork_params = list(),
                                              plot_annotation_params = list(),
                                              caption_size = 10, # new parameter for caption size
                                              include_individual_titles = FALSE,
                                              width = 10,
                                              height = 8,
                                              dpi = 400,
                                              ...) {
  # initialize empty lists to store the ggplot objects and IDs
  plot_list <- list()
  all_ids <- c()
  total_obs <- 0

  # iterate over the outcome variables
  for (outcome_var in outcome_vars) {
    # create the full formula
    full_formula <- as.formula(paste(outcome_var, exposure_formula))

    # get the label for the outcome variable
    if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
      y_label <- label_mapping[[outcome_var]]
    } else {
      y_label <- gsub("_", " ", outcome_var)
      y_label <- tools::toTitleCase(y_label)
    }

    # create plot
    plot_result <- margot_plot_slope_covariate(
      data = data,
      formula = full_formula,
      terms = terms,
      y_label = y_label,
      x_label = x_label,
      color_label = color_label,
      include_title = include_individual_titles,
      save_path = NULL, # we don't save individual plots here
      ...
    )

    if (!is.null(plot_result)) {
      # add the plot to the list
      plot_list[[outcome_var]] <- plot_result$plot
      # collect IDs and total observations
      all_ids <- c(all_ids, plot_result$ids)
      total_obs <- total_obs + plot_result$total_obs
    }
  }

  # get the total unique participants across all plots
  combined_total_unique <- length(unique(all_ids))
  combined_total_obs <- total_obs

  # combine the plots using wrap_plots, passing in ncol, nrow, guides
  combined_plot <- patchwork::wrap_plots(plot_list, ncol = ncol, nrow = nrow, guides = guides)

  # apply any additional patchwork parameters via plot_layout
  if (length(patchwork_params) > 0) {
    combined_plot <- combined_plot & do.call(patchwork::plot_layout, patchwork_params)
  }

  # prepare total participants and observations info
  caption_text <- sprintf(
    "Total N = %d unique participants, %d observations",
    combined_total_unique, combined_total_obs
  )

  # add participant counts to plot annotations
  if (is.null(plot_annotation_params$caption)) {
    plot_annotation_params$caption <- caption_text
  }

  # apply plot annotations
  if (length(plot_annotation_params) > 0) {
    combined_plot <- combined_plot & do.call(patchwork::plot_annotation, plot_annotation_params)
  }

  # adjust caption size
  combined_plot <- combined_plot & ggplot2::theme(plot.caption = ggplot2::element_text(size = caption_size))

  # save the combined plot if a save path is provided
  if (!is.null(save_path)) {
    filename <- paste0(file_prefix, "_combined_plot")
    cli::cli_alert_info("Saving combined plot...")
    ggplot2::ggsave(
      plot = combined_plot,
      filename = file.path(save_path, paste0(filename, ".png")),
      width = width,
      height = height,
      units = "in",
      device = "png",
      dpi = dpi
    )
    # save the ggplot2 object
    margot::here_save_qs(combined_plot, filename, save_path, preset = "high", nthreads = 1)
    cli::cli_alert_success("Combined plot saved successfully as {filename}.png and {filename}.qs \U0001F44D")
  }

  # return the combined plot
  return(combined_plot)
}
