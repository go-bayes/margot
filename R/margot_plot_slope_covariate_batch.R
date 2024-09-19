#' Create Multiple Slope Plots using ggeffects
#'
#' This function creates multiple ggplot2 visualizations using ggeffects to calculate
#' predicted responses from models for multiple outcome variables. It allows flexible
#' specification of the models and plotting options. The function automatically handles
#' NA and infinite values, and reports the number of unique participants and observations
#' used in each analysis.
#'
#' @param data A data frame containing the variables for the models.
#' @param outcome_vars A character vector specifying the outcome variables to be modeled.
#' @param exposure_formula A formula specifying the exposure variables (right-hand side of the model).
#' @param terms A character vector specifying the terms to be used in predict_response.
#' @param label_mapping An optional named list mapping outcome variables to custom y-axis labels.
#' @param x_label An optional label for the x-axis. If NULL, the first term will be used.
#' @param color_label An optional label for the color legend. If NULL, the second term will be used.
#' @param save_path An optional path to save the plots. If NULL, the plots will not be saved.
#' @param file_prefix Optional prefix for the saved file name (default is "outcome").
#' @param ... Additional arguments to be passed to margot_plot_slope_covariate.
#'
#' @return A list of ggplot2 objects representing the plots.
#'
#' @import ggplot2
#' @import ggeffects
#' @import ggokabeito
#' @import cli
#' @import dplyr
#' @import qs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggeffects)
#' library(ggokabeito)
#' library(dplyr)
#' library(qs)
#'
#' # Define outcome variables and label mapping
#' outcome_vars <- c(
#'   "env_not_climate_chg_concern",
#'   "env_not_climate_chg_cause",
#'   "env_not_climate_chg_real",
#'   "env_not_env_efficacy",
#'   "env_not_sat_nz_environment"
#' )
#'
#' label_mapping <- list(
#'   "env_not_climate_chg_concern" = "Deny Climate Change Concern",
#'   "env_not_climate_chg_cause" = "Deny Humans Cause Climate Change",
#'   "env_not_climate_chg_real" = "Deny Climate Change Real",
#'   "env_not_env_efficacy" = "Deny Personal Env Efficacy",
#'   "env_not_sat_nz_environment" = "Not Sat with NZ Environment"
#' )
#'
#' # Create batch of plots
#' plot_list <- margot_plot_slope_covariate_batch(
#'   data = dat,
#'   outcome_vars = outcome_vars,
#'   exposure_formula = "~ wave:political_conservative",
#'   terms = c("wave", "political_conservative"),
#'   label_mapping = label_mapping,
#'   x_label = "Time",
#'   color_label = "Political Conservatism",
#'   save_path = "path/to/save/directory",
#'   file_prefix = "environmental_attitudes",
#'   plot_points = TRUE,
#'   point_alpha = 0.03
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
                                              ...) {
  # initialise empty list to store the ggplot objects
  plot_list <- list()

  # iterate over the outcome variables
  for (outcome_var in outcome_vars) {
    # Create the full formula
    full_formula <- as.formula(paste(outcome_var, exposure_formula))

    # get the label for the outcome variable
    if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
      y_label <- label_mapping[[outcome_var]]
    } else {
      y_label <- gsub("_", " ", outcome_var)
      y_label <- tools::toTitleCase(y_label)
    }

    # create plot
    plot <- margot_plot_slope_covariate(
      data = data,
      formula = full_formula,
      terms = terms,
      y_label = y_label,
      x_label = x_label,
      color_label = color_label,
      save_path = save_path,
      prefix = outcome_var,
      ...
    )

    # add the plot to the list
    plot_list[[outcome_var]] <- plot
  }

  # save the list of ggplot objects if a save path is provided
  if (!is.null(save_path)) {
    # Create the filename with the optional prefix
    filename <- paste0(file_prefix, "_plot_slope_covariate_list.qs")
    qs::qsave(plot_list, file.path(save_path, filename))
    cli::cli_alert_success("List of ggplot objects saved successfully as {filename} \U0001F44D")
  }

  # return the list of ggplot objects
  return(plot_list)
}
