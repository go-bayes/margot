# helper ---------------------------------------------------------------
.policy_tree_build_predict_df <- function(plot_data, columns) {
  candidates <- list(plot_data$X_test, plot_data$X_test_full)
  for (cand in candidates) {
    if (!is.null(cand)) {
      cand_df <- as.data.frame(cand)
      if (all(columns %in% colnames(cand_df))) {
        return(cand_df[, columns, drop = FALSE])
      }
    }
  }
  missing <- setdiff(
    columns,
    unique(unlist(lapply(candidates, function(cand) {
      if (is.null(cand)) character() else colnames(as.data.frame(cand))
    })))
  )
  cli::cli_abort(
    "Unable to locate required column(s) for prediction: {paste(missing, collapse = \", \")}")
}

#' Plot a policy tree (depth-adaptive)
#'
#' Visualise the first one or two splits of a `policytree` stored inside a
#' multi-arm causal-forest result.
#'
#' * **Depth 1** – a one–dimensional jitter plot coloured by the predicted
#'   action and annotated with the split point.
#' * **Depth 2** – the existing two-panel scatter-plot (hand-off to
#'   `margot_plot_policy_tree_depth2()`).
#'
#' @param result_object A list produced by `margot_multiclass_cf()` (or similar)
#'   whose `results` slot holds `policy_tree_depth_1`,
#'   `policy_tree_depth_2`, and `plot_data` entries for `model_name`.
#' @param model_name Character scalar identifying the result inside
#'   `result_object$results`.
#' @param max_depth Integer, 1 or 2; which stored tree to visualise.
#' @param original_df Optional data frame of raw-scale variables (only used
#'   by the depth-2 plot for secondary-axis labels).
#' @param shading Logical – draw shaded half-planes for non-decision regions
#'   (depth-2 only).  If `NULL` (default) the function decides automatically.
#' @param color_scale A pre-built `ggplot2` colour scale (rarely needed).
#' @param point_alpha Alpha transparency for plotted points.
#' @param theme_function A `ggplot2` theme function; default
#'   `ggplot2::theme_classic`.
#' @param label_mapping Named list for explicit string replacements; passed
#'   to `transform_label()`.
#' @param label_options List of logical flags understood by
#'   `transform_label()` (see that function for details).
#' @param ... Extra arguments forwarded **only** to the depth-2 helper
#'   (e.g. `title_size`, `jitter_width`, etc.).
#'
#' @return A `ggplot` object (depth 1) or a patchwork object (depth 2).
#'
#' @import ggplot2
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
#' @importFrom tibble tibble
#' @importFrom cli cli_h1 cli_alert_info cli_abort
#' @export
margot_plot_policy_tree <- function(
    result_object,
    model_name,
    max_depth = 2L,
    original_df = NULL,
    shading = NULL,
    color_scale = NULL,
    point_alpha = 0.5,
    theme_function = ggplot2::theme_classic,
    label_mapping = NULL,
    label_options = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ),
    ...) {
  cli::cli_h1("Margot Plot Policy Tree")

  # pull out requested tree
  tag <- paste0("policy_tree_depth_", max_depth)
  tree <- result_object$results[[model_name]][[tag]]
  if (is.null(tree)) {
    cli::cli_abort("no {tag} stored for model '{model_name}'")
  }

  depth <- tree$depth
  if (!depth %in% 1:2) {
    cli::cli_abort("only depth 1 or 2 supported (got {depth})")
  }

  if (is.null(shading)) shading <- depth == 2L

  # helper: safe transform_label
  tl <- function(x) {
    tryCatch(
      transform_label(x, label_mapping, label_options),
      error = function(e) x
    )
  }

  # helper: safe transform_var_name
  tv <- function(x) {
    tryCatch(
      transform_var_name(
        x,
        label_mapping = label_mapping,
        remove_tx_prefix = label_options$remove_tx_prefix,
        remove_z_suffix = label_options$remove_z_suffix,
        use_title_case = label_options$use_title_case,
        remove_underscores = label_options$remove_underscores
      ),
      error = function(e) x
    )
  }

  # helper: build prediction frame containing required columns
  build_predict_df <- function(pd, columns) {
    candidates <- list(pd$X_test, pd$X_test_full)
    for (cand in candidates) {
      if (!is.null(cand)) {
        cand_df <- as.data.frame(cand)
        if (all(columns %in% colnames(cand_df))) {
          return(cand_df[, columns, drop = FALSE])
        }
      }
    }
    missing <- setdiff(columns, unlist(lapply(candidates, function(cand) {
      if (is.null(cand)) character() else colnames(as.data.frame(cand))
    })))
    cli::cli_abort(
      "Unable to locate required column(s) for prediction: {paste(missing, collapse = \", \")}"
    )
  }

  # build colour scale
  build_colour_scale <- function(action_labels) {
    if (!is.null(color_scale)) {
      return(color_scale)
    }
    okabe <- c(
      "#56B4E9", "#E69F00", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#000000"
    )
    ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(action_labels)], action_labels)
    )
  }

  if (depth == 1L) {
    nd <- tree$nodes[[1L]]
    sp <- nd$split_variable
    cp <- nd$split_value

    pd <- result_object$results[[model_name]]$plot_data
    predict_df <- .policy_tree_build_predict_df(pd, tree$columns)

    # determine var_name
    if (is.numeric(sp)) {
      var_name <- tree$columns[as.integer(sp)]
    } else {
      var_name <- sp
    }

    if (!var_name %in% colnames(predict_df)) {
      cli::cli_abort("split variable '{var_name}' not found in prediction frame")
    }

    x_vec <- predict_df[[var_name]]
    if (length(x_vec) == 0) {
      cli::cli_abort("vector for split variable '{var_name}' is empty (check input data)")
    }

    # get labels
    var_label <- tv(var_name)
    act_labels <- vapply(tree$action.names, tl, FUN.VALUE = "")
    pred_raw <- predict(tree, predict_df)
    preds <- factor(pred_raw, seq_along(act_labels), act_labels)

    # compute original-scale split value
    orig_cp <- get_original_value_plot(var_name, cp, original_df)

    # build subtitle, adding original value if available
    subtitle_txt <- if (!is.null(orig_cp)) {
      sprintf("%s (baseline): split at %.3f (%s)*", var_label, cp, orig_cp)
    } else {
      sprintf("%s (baseline): split at %.3f", var_label, cp)
    }

    plot_df <- tibble::tibble(x = x_vec, y = 0, pred = preds)
    colour_scale <- build_colour_scale(act_labels)

    # build shape scale (now that act_labels is available)
    shape_scale <- ggplot2::scale_shape_manual(
      values = setNames(c(16, 17, 15, 3, 4, 18)[seq_along(act_labels)], act_labels)
    )

    # create the plot
    ggplot2::ggplot() +
      ggplot2::geom_jitter(
        data = plot_df,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$pred, shape = .data$pred),
        width = 0.30,
        height = 0.06,
        alpha = point_alpha,
        size = 1.5 # slightly larger to make shapes visible
      ) +
      ggplot2::geom_vline(xintercept = cp, linetype = "dashed") +
      colour_scale +
      shape_scale +
      ggplot2::labs(
        x        = paste0(var_label, " (baseline)"),
        y        = NULL,
        colour   = "Prediction",
        shape    = "Prediction", # add shape to legend
        subtitle = subtitle_txt
      ) +
      theme_function() +
      ggplot2::theme(
        axis.text.y     = ggplot2::element_blank(),
        axis.ticks.y    = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  } else {
    do.call(
      margot_plot_policy_tree_depth2,
      c(
        list(
          result_object = result_object,
          model_name = model_name,
          original_df = original_df,
          shading = shading,
          color_scale = color_scale,
          point_alpha = point_alpha,
          theme_function = theme_function,
          label_mapping = label_mapping,
          label_options = label_options
        ),
        list(...)
      )
    )
  }
}

# helper: null-coalescing operator ------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x


#' @keywords internal
margot_plot_policy_tree_depth2 <- function(
    result_object,
    model_name,
    original_df = NULL,
    shading = TRUE,
    color_scale = NULL,
    point_alpha = 0.5,
    theme_function = ggplot2::theme_classic,
    label_mapping = NULL,
    label_options = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ),
    title_size = 16,
    subtitle_size = 14,
    axis_title_size = 14,
    legend_title_size = 14,
    jitter_width = 0.3,
    jitter_height = 0.3,
    split_line_color = "red",
    split_line_alpha = 0.7,
    split_line_type = "dashed",
    split_line_linewidth = 0.5,
    split_label_size = 10,
    split_label_color = "red",
    custom_action_names = NULL,
    legend_position = "bottom",
    plot_selection = "both",
    shade_fill = "#6e6e6e",
    shade_alpha = 0.35,
    ...) {
  # ---- nudge fractions for annotation -----------------------------------
  nudge_frac_x <- 0.10 # top-of-panel label
  nudge_frac_y <- -0.10 # side (y-axis) label

  # ---- legacy: allow list input for shading -----------------------------
  if (is.list(shading)) {
    shade_enabled <- shading$enabled %||% TRUE
    shade_fill <- shading$fill %||% shade_fill
    shade_alpha <- shading$alpha %||% shade_alpha
  } else {
    shade_enabled <- isTRUE(shading)
  }

  # ---- helpers ----------------------------------------------------------
  tl <- function(x) {
    tryCatch(transform_label(
      x, label_mapping, label_options
    ), error = function(e) x)
  }
  tv <- function(x) {
    tryCatch(transform_var_name(
      x, label_mapping,
      remove_tx_prefix = label_options$remove_tx_prefix,
      remove_z_suffix = label_options$remove_z_suffix,
      use_title_case = label_options$use_title_case,
      remove_underscores = label_options$remove_underscores
    ), error = function(e) x)
  }

  # ---- get tree + plotting data ----------------------------------------
  tree_obj <- result_object$results[[model_name]]$policy_tree_depth_2
  if (is.null(tree_obj)) {
    cli::cli_abort("no depth-2 tree for model '{model_name}'")
  }

  pd <- result_object$results[[model_name]]$plot_data
  # ---- action labels & colour scale ------------------------------------
  act_labels <- if (is.null(custom_action_names)) {
    vapply(tree_obj$action.names, tl, FUN.VALUE = "")
  } else {
    vapply(custom_action_names, tl, FUN.VALUE = "")
  }

  if (is.null(color_scale)) {
    okabe <- c(
      "#56B4E9", "#E69F00", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#000000"
    )
    color_scale <- ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(act_labels)], act_labels)
    )
  }

  predict_df <- .policy_tree_build_predict_df(pd, tree_obj$columns)
  pred_raw <- predict(tree_obj, predict_df)
  preds <- factor(pred_raw, seq_along(act_labels), act_labels)
  nodes <- tree_obj$nodes

  # ---- split variables & cut-points ------------------------------------
  sv1 <- nodes[[1]]$split_variable
  cp1 <- nodes[[1]]$split_value
  sv2 <- nodes[[2]]$split_variable
  cp2 <- nodes[[2]]$split_value
  sv3 <- nodes[[3]]$split_variable
  cp3 <- nodes[[3]]$split_value

  resolve_var <- function(sv) {
    if (is.numeric(sv)) {
      tree_obj$columns[as.integer(sv)]
    } else {
      sv
    }
  }

  var1 <- resolve_var(sv1)
  var2 <- resolve_var(sv2)
  var3 <- resolve_var(sv3)

  if (!all(c(var1, var2, var3) %in% colnames(predict_df))) {
    missing <- setdiff(c(var1, var2, var3), colnames(predict_df))
    cli::cli_abort("Missing columns for plotting depth-2 tree: {paste(missing, collapse = \", \")}")
  }

  plot_df <- tibble::tibble(
    x1   = predict_df[[var1]],
    x2   = predict_df[[var2]],
    x3   = predict_df[[var3]],
    pred = preds
  )

  var_label1 <- tv(var1)
  var_label2 <- tv(var2)
  var_label3 <- tv(var3)

  # ---- panel constructor -----------------------------------------------
  build_panel <- function(x, y, xlab, ylab, xsp, ysp,
                          shade_side, xvar, yvar) {
    p <- ggplot2::ggplot()

    # --- shading ---------------------------------------------------------
    if (shade_enabled && shade_side != "none") {
      # get the actual data range to ensure shading stays within bounds
      x_range <- range(plot_df[[x]], na.rm = TRUE)
      y_range <- range(plot_df[[y]], na.rm = TRUE)

      # add small buffer to ensure we cover the plot area
      x_buffer <- diff(x_range) * 0.05
      y_buffer <- diff(y_range) * 0.05

      if (shade_side == "left") {
        # shade left side of vertical split
        p <- p + ggplot2::annotate(
          "rect",
          xmin = x_range[1] - x_buffer,
          xmax = xsp,
          ymin = y_range[1] - y_buffer,
          ymax = y_range[2] + y_buffer,
          fill = shade_fill,
          alpha = shade_alpha
        )
      } else if (shade_side == "right") {
        # shade right side of vertical split
        p <- p + ggplot2::annotate(
          "rect",
          xmin = xsp,
          xmax = x_range[2] + x_buffer,
          ymin = y_range[1] - y_buffer,
          ymax = y_range[2] + y_buffer,
          fill = shade_fill,
          alpha = shade_alpha
        )
      }
    }

    # --- back-transformed thresholds for annotation ----------------------
    orig_xsp <- get_original_value_plot(xvar, xsp, original_df)
    orig_ysp <- get_original_value_plot(yvar, ysp, original_df)

    # --- filter data based on shading to improve masking -----------------
    plot_data_filtered <- if (shade_enabled && shade_side == "left") {
      plot_df[plot_df[[x]] >= xsp, ] # only show right side (non-shaded)
    } else if (shade_enabled && shade_side == "right") {
      plot_df[plot_df[[x]] <= xsp, ] # only show left side (non-shaded)
    } else {
      plot_df # show all data if no shading
    }

    # --- build shape scale ------------------------------------------------
    shape_scale <- ggplot2::scale_shape_manual(
      values = setNames(c(16, 17, 15, 3, 4, 18)[seq_along(act_labels)], act_labels)
    )

    # --- points with both colour and shape --------------------------------
    p <- p + ggplot2::geom_jitter(
      data = plot_data_filtered,
      ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = pred, shape = pred),
      alpha = point_alpha,
      width = jitter_width,
      height = jitter_height,
      size = 1.5 # slightly larger to make shapes visible
    )

    # --- split lines -----------------------------------------------------
    p <- p + ggplot2::geom_vline(
      xintercept = xsp, colour = split_line_color,
      alpha = split_line_alpha,
      linetype = split_line_type,
      linewidth = split_line_linewidth
    ) +
      ggplot2::geom_hline(
        yintercept = ysp, colour = split_line_color,
        alpha = split_line_alpha,
        linetype = split_line_type,
        linewidth = split_line_linewidth
      )

    # --- offsets for text ------------------------------------------------
    x_rng <- range(plot_df[[x]], na.rm = TRUE) # use original data for text positioning
    y_rng <- range(plot_df[[y]], na.rm = TRUE)
    off_x <- nudge_frac_x * diff(x_rng)
    off_y <- nudge_frac_y * diff(y_rng)
    if (off_x == 0) off_x <- 0.05
    if (off_y == 0) off_y <- 0.05

    # annotate x split (top) - always show, fallback to transformed value
    x_label <- if (!is.null(orig_xsp)) {
      paste0("(", orig_xsp, ")*")
    } else {
      sprintf("%.3f", xsp)
    }
    p <- p + ggplot2::annotate(
      "text",
      x      = xsp + off_x,
      y      = y_rng[2] - abs(off_y),
      label  = x_label,
      vjust  = 1,
      hjust  = 0,
      size   = 5,
      colour = split_label_color
    )

    # annotate y split (side) - always show, fallback to transformed value
    y_label <- if (!is.null(orig_ysp)) {
      paste0("(", orig_ysp, ")*")
    } else {
      sprintf("%.3f", ysp)
    }
    p <- p + ggplot2::annotate(
      "text",
      x      = x_rng[1] + abs(off_x),
      y      = ysp + off_y,
      label  = y_label,
      hjust  = 0,
      vjust  = 0,
      size   = 5,
      colour = split_label_color
    )

    # --- final styling ---------------------------------------------------
    p + color_scale + shape_scale +
      ggplot2::labs(
        x        = paste0(xlab, " (baseline)"),
        y        = paste0(ylab, " (baseline)"),
        subtitle = paste(xlab, "(baseline) vs", ylab, "(baseline)"),
        colour   = "Prediction",
        shape    = "Prediction" # add shape to legend
      ) +
      theme_function() +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(size = subtitle_size),
        axis.title = ggplot2::element_text(size = axis_title_size),
        legend.title = ggplot2::element_text(size = legend_title_size),
        axis.text.x.top = ggplot2::element_text(
          size = split_label_size,
          colour = split_label_color
        ),
        axis.text.y.right = ggplot2::element_text(
          size = split_label_size,
          colour = split_label_color
        )
      )
  }

  # ---- build panels ----------------------------------------------------------
  p1 <- p2 <- NULL
  if (plot_selection %in% c("both", "p1")) {
    p1 <- build_panel(
      "x1", "x2",
      var_label1, var_label2,
      cp1, cp2,
      shade_side = if (shading) "right" else "none",
      xvar = var1, yvar = var2
    )
  }
  if (plot_selection %in% c("both", "p2")) {
    p2 <- build_panel(
      "x1", "x3",
      var_label1, var_label3,
      cp1, cp3,
      shade_side = if (shading) "left" else "none",
      xvar = var1, yvar = var3
    )
  }

  # ---- assemble layout with collected legend ---------------------------------
  main_title <- sprintf("Policy-tree results (baseline) – %s", tv(model_name))

  # Remove individual legends from panels before combining
  if (!is.null(p1)) p1 <- p1 + ggplot2::theme(legend.position = "none")
  if (!is.null(p2) && plot_selection == "both") {
    # Keep legend only on the last plot
    p2 <- p2 + ggplot2::theme(legend.position = legend_position)
  }

  # Combine plots
  combined <- patchwork::wrap_plots(p1, p2, ncol = 2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = main_title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          size   = title_size,
          margin = ggplot2::margin(b = 10)
        ),
        legend.position = legend_position,
        legend.box = "horizontal",
        legend.justification = "center"
      )
    )

  combined
}
