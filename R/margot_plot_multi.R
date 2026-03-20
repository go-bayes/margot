#' Create Multi-panel Margot Plots with Shared Axes
#'
#' @param tables A named list of combined tables to plot.
#' @param panel_titles Optional character vector of per-panel titles.
#' @param panel_subtitles Optional character vector of per-panel subtitles.
#' @param ncol Number of columns in the patchwork layout.
#' @param tag_levels Tag levels passed to `patchwork::plot_annotation()`.
#' @param collect_guides Logical; if `TRUE`, collect legends at the bottom.
#' @param title Optional overall patchwork title.
#' @param subtitle Optional overall patchwork subtitle.
#' @param caption Optional overall patchwork caption.
#' @param shared_x_limits Optional numeric vector of length two. If omitted,
#'   limits are computed across all panels.
#' @param options Base options passed to each `margot_plot()` call.
#' @param per_panel_options Optional list of option lists, one per panel.
#' @param ... Additional arguments passed to `margot_plot()`.
#'
#' @return An invisible list containing the combined `plot`, the per-panel
#'   `panels`, and the computed `shared_x_limits`.
#' @export
margot_plot_multi <- function(
    tables,
    panel_titles = names(tables),
    panel_subtitles = NULL,
    ncol = 1,
    tag_levels = "A",
    collect_guides = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    shared_x_limits = NULL,
    options = list(),
    per_panel_options = NULL,
    ...) {
  if (!is.list(tables) || length(tables) == 0) {
    cli::cli_abort("`tables` must be a non-empty list of combined tables.")
  }

  if (is.null(names(tables))) {
    names(tables) <- paste0("Panel ", seq_along(tables))
  }

  if (is.null(panel_titles)) {
    panel_titles <- names(tables)
  }
  if (length(panel_titles) != length(tables)) {
    cli::cli_abort("`panel_titles` must have the same length as `tables`.")
  }

  if (!is.null(panel_subtitles) && length(panel_subtitles) != length(tables)) {
    cli::cli_abort("`panel_subtitles` must have the same length as `tables`.")
  }

  if (is.null(per_panel_options)) {
    per_panel_options <- replicate(length(tables), list(), simplify = FALSE)
  }
  if (length(per_panel_options) != length(tables)) {
    cli::cli_abort("`per_panel_options` must have the same length as `tables`.")
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_abort("Package 'patchwork' is required for `margot_plot_multi()`.")
  }

  build_panel_options <- function(i, shared_limits = NULL) {
    panel_opts <- modifyList(options, per_panel_options[[i]])

    if (!is.null(panel_titles[[i]]) && is.null(panel_opts$title)) {
      panel_opts$title <- panel_titles[[i]]
    }
    if (!is.null(panel_subtitles) && is.null(panel_opts$subtitle)) {
      panel_opts$subtitle <- panel_subtitles[[i]]
    }
    if (!is.null(shared_limits)) {
      panel_opts$x_lim_lo <- shared_limits[1]
      panel_opts$x_lim_hi <- shared_limits[2]
    }

    panel_opts
  }

  if (is.null(shared_x_limits)) {
    initial_panels <- lapply(seq_along(tables), function(i) {
      margot_plot(
        .data = tables[[i]],
        options = build_panel_options(i),
        ...
      )
    })
    names(initial_panels) <- names(tables)

    panel_limits <- lapply(initial_panels, function(panel) {
      panel_data <- panel$plot$data
      effect_info <- detect_effect_column(panel_data)
      compute_margot_plot_limits(
        panel_data,
        effect_col = effect_info$column
      )
    })

    shared_x_limits <- c(
      min(vapply(panel_limits, `[`, numeric(1), 1), na.rm = TRUE),
      max(vapply(panel_limits, `[`, numeric(1), 2), na.rm = TRUE)
    )
  } else {
    if (!is.numeric(shared_x_limits) || length(shared_x_limits) != 2) {
      cli::cli_abort("`shared_x_limits` must be a numeric vector of length two.")
    }

    if (any(!is.finite(shared_x_limits))) {
      cli::cli_abort("`shared_x_limits` must contain finite numeric values.")
    }
  }

  panels <- lapply(seq_along(tables), function(i) {
    margot_plot(
      .data = tables[[i]],
      options = build_panel_options(i, shared_limits = shared_x_limits),
      ...
    )
  })
  names(panels) <- names(tables)

  plot_list <- lapply(panels, `[[`, "plot")
  combined_plot <- patchwork::wrap_plots(
    plotlist = plot_list,
    ncol = ncol,
    guides = if (isTRUE(collect_guides)) "collect" else "keep"
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag_levels = tag_levels
    )

  if (isTRUE(collect_guides)) {
    combined_plot <- combined_plot & ggplot2::theme(legend.position = "bottom")
  }

  invisible(list(
    plot = combined_plot,
    panels = panels,
    shared_x_limits = shared_x_limits,
    interpretations = lapply(panels, `[[`, "interpretation"),
    transformed_tables = lapply(panels, `[[`, "transformed_table")
  ))
}
