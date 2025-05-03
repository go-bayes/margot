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
#' @param mc_test A list produced by `margot_multiclass_cf()` (or similar)
#'   whose `results` slot holds `policy_tree_depth_1`,
#'   `policy_tree_depth_2`, and `plot_data` entries for `model_name`.
#' @param model_name Character scalar identifying the result inside
#'   `mc_test$results`.
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
#' @export
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom tibble tibble
#' @importFrom cli cli_h1 cli_alert_info cli_abort
margot_plot_policy_tree <- function(
    mc_test,
    model_name,
    max_depth      = 2L,
    original_df    = NULL,
    shading        = NULL,
    color_scale    = NULL,
    point_alpha    = 0.5,
    theme_function = ggplot2::theme_classic,
    label_mapping  = NULL,
    label_options  = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ),
    ...
) {
  cli::cli_h1("Margot Plot Policy Tree")

  ## ── pull out the requested tree ----------------------------------------
  tag  <- paste0("policy_tree_depth_", max_depth)
  tree <- mc_test$results[[model_name]][[tag]]
  if (is.null(tree))
    cli::cli_abort("no {tag} stored for model '{model_name}'")

  depth <- tree$depth
  if (!depth %in% 1:2)
    cli::cli_abort("only depth 1 or 2 supported (got {depth})")

  if (is.null(shading))
    shading <- depth == 2L

  ## ── helpers -------------------------------------------------------------
  tf <- function(x) transform_label(x, label_mapping, label_options)

  ## ── colour scale (shared) ----------------------------------------------
  build_colour_scale <- function(action_labels) {
    if (!is.null(color_scale)) return(color_scale)
    okabe <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
    ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(action_labels)], action_labels)
    )
  }

  ## ── depth-1 visualisation -------------------------------------------------
  if (depth == 1L) {

    nd <- tree$nodes[[1L]]
    sp <- nd$split_variable
    cp <- nd$split_value      # cut-point (standardised scale)

    ## pull plotting matrices -------------------------------------------------
    pd        <- mc_test$results[[model_name]]$plot_data
    Xt        <- pd$X_test
    Xt_full   <- pd$X_test_full %||% Xt            # may be identical

    ## determine split variable -------------------------------------------------
    if (is.numeric(sp)) {
      var_name <- tree$columns[as.integer(sp)]
    } else {
      var_name <- sp
    }
    ## look for that name in the skinny matrix ---------------------------------
    idx <- match(var_name, if (is.matrix(Xt)) colnames(Xt) else names(Xt))
    ## fallback to full matrix if missing --------------------------------------
    if (is.na(idx)) {
      cli::cli_alert_info(
        "split variable '{var_name}' not in X_test; falling back to X_test_full"
      )
      idx <- match(var_name,
                   if (is.matrix(Xt_full)) colnames(Xt_full) else names(Xt_full))
      if (is.na(idx))
        cli::cli_abort("split variable '{var_name}' absent from both X_test and X_test_full")
      Xt <- Xt_full   # switch data source
    }

    x_vec <- if (is.matrix(Xt)) Xt[, idx] else Xt[[var_name]]
    if (length(x_vec) == 0)
      cli::cli_abort("vector for split variable '{var_name}' is empty (check input data)")

    # ... remainder of depth-1 code unchanged ...
    # ──────────────────────────────────────────────────────────────────────────

    act_labels <- vapply(tree$action.names, tf, FUN.VALUE = "")
    preds      <- factor(pd$predictions,
                         seq_along(act_labels),
                         act_labels)


    plot_df <- tibble::tibble(
      x    = x_vec,
      y    = 0,
      pred = preds
    )

    colour_scale <- build_colour_scale(act_labels)

    ggplot2::ggplot() +
      ggplot2::geom_jitter(
        data   = plot_df,
        # --- use .data pronoun for clarity ---
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$pred),
        # ------------------------------------
        width  = 0.30,
        height = 0.06,
        alpha  = point_alpha
      ) +
      ggplot2::geom_vline(xintercept = cp, linetype = "dashed") +
      colour_scale +
      ggplot2::labs(
        x        = tf(var_name),
        y        = NULL,
        colour   = "Prediction",
        subtitle = sprintf("%s: split at %.3f", tf(var_name), cp)
      ) +
      theme_function() +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "bottom"
      )

  } else {

## depth-2 → delegate, but make sure label args are forwarded -----------
    do.call(
      margot_plot_policy_tree_depth2,
      c(
        list(
          mc_test        = mc_test,
          model_name     = model_name,
          original_df    = original_df,
          shading        = shading,
          color_scale    = color_scale,
          point_alpha    = point_alpha,
          theme_function = theme_function,
          label_mapping  = label_mapping,
          label_options  = label_options
        ),
        list(...)
      )
    )
  }
}

#' Depth-2 helper (internal)
#'
#' @keywords internal
#' @inheritParams margot_plot_policy_tree
#' @param title_size Base font size for the main title.
#' @param subtitle_size Base font size for subplot subtitles.
#' … (other graphic-tuning parameters documented here) …
#'
#' @return A patchwork object.
#' @import patchwork ggplot2
margot_plot_policy_tree_depth2 <- function(
    mc_test,
    model_name,
    original_df     = NULL,
    shading         = TRUE,
    color_scale     = NULL,
    point_alpha     = 0.5,
    theme_function  = ggplot2::theme_classic,
    label_mapping   = NULL,
    label_options   = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ),
    # ------ aesthetic tuning args (defaults match previous behaviour) ------
    title_size      = 16,
    subtitle_size   = 14,
    axis_title_size = 14,
    legend_title_size = 14,
    jitter_width    = 0.3,
    jitter_height   = 0.3,
    split_line_color = "red",
    split_line_alpha = 0.7,
    split_line_type  = "dashed",
    split_line_linewidth = 0.5,
    split_label_size = 10,
    split_label_color = "red",
    custom_action_names = NULL,
    legend_position = "bottom",
    plot_selection  = "both",
    shade_fill      = "#6e6e6e",
    shade_alpha     = 0.35,
    ...
) {
  tf <- function(x) transform_label(x, label_mapping, label_options)

  tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
  if (is.null(tree_obj))
    cli::cli_abort("no depth-2 tree for model '{model_name}'")

  # ── action names and colour scale ---------------------------------------
  act_labels <- if (is.null(custom_action_names))
    vapply(tree_obj$action.names, tf, FUN.VALUE = "")
  else
    vapply(custom_action_names, tf, FUN.VALUE = "")

  n_act      <- length(act_labels)
  if (is.null(color_scale)) {
    okabe <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
    color_scale <- ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(act_labels)], act_labels)
    )
  }

  # ── unpack data ----------------------------------------------------------
  pd       <- mc_test$results[[model_name]]$plot_data
  Xt       <- pd$X_test
  preds    <- factor(pd$predictions,
                     seq_along(act_labels),
                     act_labels)
  nodes    <- tree_obj$nodes
  idx1     <- nodes[[1]]$split_variable; sv1 <- nodes[[1]]$split_value
  idx2     <- nodes[[2]]$split_variable; sv2 <- nodes[[2]]$split_value
  idx3     <- nodes[[3]]$split_variable; sv3 <- nodes[[3]]$split_value

  varnames <- names(Xt)
  plot_df  <- tibble::tibble(
    x1 = Xt[[varnames[idx1]]],
    x2 = Xt[[varnames[idx2]]],
    x3 = Xt[[varnames[idx3]]],
    pred = preds
  )

  # ── helper to build a single panel --------------------------------------
  build_panel <- function(x, y, xlab, ylab,
                          xsp, ysp, xnm, ynm,
                          shade_side = c("none", "left", "right")) {

    sd <- match.arg(shade_side)
    p  <- ggplot2::ggplot()

    if (shading && sd != "none") {
      xmin <- if (sd == "left") -Inf else xsp
      xmax <- if (sd == "left") xsp  else Inf
      p <- p + ggplot2::annotate(
        "rect",
        xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf,
        fill = shade_fill, alpha = shade_alpha
      )
    }

    p +
      ggplot2::geom_jitter(
        data   = plot_df,
        ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = pred),
        alpha  = point_alpha,
        width  = jitter_width,
        height = jitter_height
      ) +
      ggplot2::geom_vline(
        xintercept = xsp, colour = split_line_color,
        alpha = split_line_alpha, linetype = split_line_type,
        linewidth = split_line_linewidth
      ) +
      ggplot2::geom_hline(
        yintercept = ysp, colour = split_line_color,
        alpha = split_line_alpha, linetype = split_line_type,
        linewidth = split_line_linewidth
      ) +
      color_scale +
      ggplot2::labs(
        x = xlab, y = ylab,
        subtitle = paste(xlab, "vs", ylab),
        colour = "Prediction"
      ) +
      theme_function() +
      ggplot2::theme(
        plot.subtitle     = ggplot2::element_text(size = subtitle_size),
        axis.title        = ggplot2::element_text(size = axis_title_size),
        legend.title      = ggplot2::element_text(size = legend_title_size),
        axis.text.x.top   = ggplot2::element_text(size = split_label_size,
                                                  colour = split_label_color),
        axis.text.y.right = ggplot2::element_text(size = split_label_size,
                                                  colour = split_label_color)
      )
  }

  # ── build requested panels ----------------------------------------------
  p1 <- p2 <- NULL
  if (plot_selection %in% c("both", "p1")) {
    p1 <- build_panel(
      "x1", "x2",
      tf(varnames[idx1]), tf(varnames[idx2]),
      sv1, sv2, varnames[idx1], varnames[idx2],
      shade_side = if (shading) "right" else "none"
    )
  }
  if (plot_selection %in% c("both", "p2")) {
    p2 <- build_panel(
      "x1", "x3",
      tf(varnames[idx1]), tf(varnames[idx3]),
      sv1, sv3, varnames[idx1], varnames[idx3],
      shade_side = if (shading) "left" else "none"
    )
  }

  # ── assemble -------------------------------------------------------------
  main_title <- sprintf("Policy-tree results – %s", tf(model_name))
  patchwork::wrap_plots(p1, p2, ncol = 2) +
    patchwork::plot_annotation(
      title   = main_title,
      theme   = ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size,
                                           margin = ggplot2::margin(b = 10))
      )
    ) &
    ggplot2::theme(legend.position = legend_position)
}

# fallback NULL-coalescing operator -----------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
