#' Plot Policy-Tree Regret and Value Contrasts
#'
#' @description
#' Creates a compact forest plot from \code{\link{margot_policy_regret_summary}}
#' output. The default view shows regret relative to the oracle benchmark and
#' gains over universal treatment and universal control, with bootstrap
#' uncertainty intervals.
#'
#' @param x A data frame returned by \code{\link{margot_policy_regret_summary}}
#'   or \code{\link{policy_regret_summary}}.
#' @param metrics Character vector of contrasts to plot. Supported values are
#'   \code{"regret_vs_oracle"}, \code{"gain_vs_treat"}, and
#'   \code{"gain_vs_control"}.
#' @param depths Optional integer vector of depths to include.
#' @param model_names Optional character vector of model names or bare outcome
#'   names to include.
#' @param title Optional plot title. Defaults to \code{"Policy-tree regret"}.
#' @param subtitle Optional plot subtitle. Defaults to a short note about
#'   intervals.
#' @param x_lab Optional x-axis label. Defaults to
#'   \code{"Estimated value contrast"}.
#' @param y_lab Optional y-axis label. Defaults to \code{NULL}.
#' @param metric_labels Optional named character vector for display labels.
#' @param point_size Numeric point size.
#' @param interval_height Numeric error-bar height.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' regret <- policy_regret_summary(policy_tree_results, depths = 2)
#' margot_plot_policy_regret(regret)
#' margot_plot_policy_regret(regret, metrics = "regret_vs_oracle")
#' }
#'
#' @export
margot_plot_policy_regret <- function(x,
                                      metrics = c(
                                        "regret_vs_oracle",
                                        "gain_vs_treat",
                                        "gain_vs_control"
                                      ),
                                      depths = NULL,
                                      model_names = NULL,
                                      title = "Policy-tree regret",
                                      subtitle = "Points show value contrasts; lines show bootstrap intervals.",
                                      x_lab = "Estimated value contrast",
                                      y_lab = NULL,
                                      metric_labels = NULL,
                                      point_size = 2.4,
                                      interval_height = 0.22) {
  # validate that the summary table carries the columns needed for plotting.
  required_cols <- c("model", "outcome", "outcome_label", "depth")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols)) {
    stop(
      "x must be a policy-regret summary with columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  supported_metrics <- c("regret_vs_oracle", "gain_vs_treat", "gain_vs_control")
  unknown_metrics <- setdiff(metrics, supported_metrics)
  if (length(unknown_metrics)) {
    stop(
      "unsupported metric(s): ",
      paste(unknown_metrics, collapse = ", "),
      call. = FALSE
    )
  }
  metrics <- unique(metrics)

  plot_df <- as.data.frame(x)

  if (!is.null(depths)) {
    # keep depth filtering explicit because summaries may contain depths 1 and 2.
    depths <- as.integer(depths)
    plot_df <- plot_df[plot_df$depth %in% depths, , drop = FALSE]
  }

  if (!is.null(model_names)) {
    # allow either stored model ids or bare outcome names.
    keep_models <- model_names
    keep_outcomes <- gsub("^model_", "", model_names)
    plot_df <- plot_df[
      plot_df$model %in% keep_models | plot_df$outcome %in% keep_outcomes,
      ,
      drop = FALSE
    ]
  }

  if (!nrow(plot_df)) {
    stop("no rows remain after filtering", call. = FALSE)
  }

  plot_data <- .policy_regret_plot_data(plot_df, metrics, metric_labels)
  plot_data$outcome_label <- .policy_regret_order_outcomes(plot_data)
  plot_data$depth_label <- paste0("Depth ", plot_data$depth)

  dodge <- ggplot2::position_dodge(width = 0.55)
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$estimate,
      y = .data$outcome_label,
      colour = .data$metric_label
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.35) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
      width = interval_height,
      linewidth = 0.55,
      position = dodge,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = point_size, position = dodge, na.rm = TRUE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab,
      colour = NULL
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  if (length(unique(plot_data$depth)) > 1L) {
    # facet by depth only when needed to avoid duplicating outcome rows.
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$depth_label), ncol = 1)
  }

  p
}

#' @keywords internal
.policy_regret_plot_data <- function(x, metrics, metric_labels = NULL) {
  # reshape selected contrasts without requiring tidyr at runtime.
  default_labels <- c(
    regret_vs_oracle = "Regret vs oracle",
    gain_vs_treat = "Gain vs treat-all",
    gain_vs_control = "Gain vs control-all"
  )
  if (!is.null(metric_labels)) {
    default_labels[names(metric_labels)] <- metric_labels
  }

  rows <- lapply(metrics, function(metric) {
    lower_col <- paste0(metric, "_lower")
    upper_col <- paste0(metric, "_upper")
    needed <- c(metric, lower_col, upper_col)
    missing_cols <- setdiff(needed, names(x))
    if (length(missing_cols)) {
      stop(
        "x is missing required metric column(s): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    data.frame(
      model = x$model,
      outcome = x$outcome,
      outcome_label = x$outcome_label,
      depth = x$depth,
      metric = metric,
      metric_label = unname(default_labels[[metric]]),
      estimate = x[[metric]],
      lower = x[[lower_col]],
      upper = x[[upper_col]],
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

#' @keywords internal
.policy_regret_order_outcomes <- function(plot_data) {
  # order outcomes by largest displayed regret, falling back to the first metric.
  regret_rows <- plot_data[plot_data$metric == "regret_vs_oracle", , drop = FALSE]
  if (!nrow(regret_rows)) regret_rows <- plot_data

  order_df <- stats::aggregate(
    regret_rows$estimate,
    by = list(outcome_label = regret_rows$outcome_label),
    FUN = max,
    na.rm = TRUE
  )
  order_df <- order_df[order(order_df$x, decreasing = FALSE), , drop = FALSE]

  factor(
    plot_data$outcome_label,
    levels = unique(order_df$outcome_label)
  )
}
