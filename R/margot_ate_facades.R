#' Plot Average Treatment Effect Estimates
#'
#' Return the figure produced by [margot_plot()]. The plotting, table and interpretation interfaces use the same reporting calculations and scale metadata.
#'
#' @param .data A combined estimate table accepted by [margot_plot()].
#' @param ... Additional arguments to [margot_plot()], including ordering, labels, multiplicity adjustment and plotting options.
#' @param scale_info Outcome transformation metadata accepted by [margot_plot()]. Supply the constants used during preparation when reporting standardised outcomes in measurement units.
#'
#' @details
#' This interface returns a figure directly. [margot_table_ate()] returns the corresponding table and [margot_interpret_ate()] returns the corresponding prose. The existing [margot_plot()] interface retains its list containing all three outputs.
#'
#' All arguments in `...` retain their [margot_plot()] behaviour. In particular, `save_output = TRUE` saves the complete reporting list before this function returns the figure.
#'
#' @return A `ggplot` object.
#' @seealso [margot_table_ate()], [margot_interpret_ate()], [margot_plot()]
#' @export
margot_plot_ate <- function(.data, ..., scale_info = NULL) {
  # select the figure from the common reporting calculation
  if (missing(scale_info)) return(margot_plot(.data, ...)$plot)
  margot_plot(.data, ..., scale_info = scale_info)$plot
}

#' Interpret Average Treatment Effect Estimates
#'
#' Return the prose produced by [margot_plot()], using the same estimates, intervals, ordering and transformation metadata as its figure and table.
#'
#' @inheritParams margot_plot_ate
#' @details Arguments in `...` retain their [margot_plot()] behaviour, including the evidence threshold controlling which estimates enter the prose. If `save_output = TRUE`, the complete reporting list is saved.
#' @return A character string containing the formatted interpretation.
#' @seealso [margot_plot_ate()], [margot_table_ate()], [margot_plot()]
#' @export
margot_interpret_ate <- function(.data, ..., scale_info = NULL) {
  # select prose from the common reporting calculation
  if (missing(scale_info)) return(margot_plot(.data, ...)$interpretation)
  margot_plot(.data, ..., scale_info = scale_info)$interpretation
}

#' Tabulate Average Treatment Effect Estimates
#'
#' Return the table produced by [margot_plot()], retaining numerical precision for subsequent formatting.
#'
#' @inheritParams margot_plot_ate
#' @details The table retains model-scale estimates and includes separately labelled reported quantities when transformation metadata is supplied. Arguments in `...` retain their [margot_plot()] behaviour, including column renaming, outcome ordering and multiplicity adjustment. If `save_output = TRUE`, the complete reporting list is saved.
#' @return A data frame corresponding to the `transformed_table` element of [margot_plot()].
#' @seealso [margot_plot_ate()], [margot_interpret_ate()], [margot_plot()]
#' @export
margot_table_ate <- function(.data, ..., scale_info = NULL) {
  # select the numerical table from the common reporting calculation
  if (missing(scale_info)) return(margot_plot(.data, ...)$transformed_table)
  margot_plot(.data, ..., scale_info = scale_info)$transformed_table
}
