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
#' @import patchwork
#' @importFrom tibble tibble
#' @importFrom cli cli_h1 cli_alert_info cli_abort
margot_plot_policy_tree <- function(
    result_object,
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

  # pull out requested tree
  tag  <- paste0("policy_tree_depth_", max_depth)
  tree <- result_object$results[[model_name]][[tag]]
  if (is.null(tree))
    cli::cli_abort("no {tag} stored for model '{model_name}'")

  depth <- tree$depth
  if (!depth %in% 1:2)
    cli::cli_abort("only depth 1 or 2 supported (got {depth})")

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
        remove_tx_prefix   = label_options$remove_tx_prefix,
        remove_z_suffix    = label_options$remove_z_suffix,
        use_title_case     = label_options$use_title_case,
        remove_underscores = label_options$remove_underscores
      ),
      error = function(e) x
    )
  }

  # build colour scale
  build_colour_scale <- function(action_labels) {
    if (!is.null(color_scale)) return(color_scale)
    okabe <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
    ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(action_labels)], action_labels)
    )
  }

  if (depth == 1L) {
    nd        <- tree$nodes[[1L]]
    sp        <- nd$split_variable
    cp        <- nd$split_value

    pd        <- result_object$results[[model_name]]$plot_data
    Xt        <- pd$X_test
    Xt_full   <- pd$X_test_full %||% Xt

    # determine var_name
    if (is.numeric(sp)) {
      var_name <- tree$columns[as.integer(sp)]
    } else {
      var_name <- sp
    }

    idx <- match(var_name, if (is.matrix(Xt)) colnames(Xt) else names(Xt))
    if (is.na(idx)) {
      cli::cli_alert_info(
        "split variable '{var_name}' not in X_test; falling back to X_test_full"
      )
      idx <- match(var_name,
                   if (is.matrix(Xt_full)) colnames(Xt_full) else names(Xt_full))
      if (is.na(idx))
        cli::cli_abort("split variable '{var_name}' absent from both X_test and X_test_full")
      Xt <- Xt_full
    }

    x_vec <- if (is.matrix(Xt)) Xt[, idx] else Xt[[var_name]]
    if (length(x_vec) == 0)
      cli::cli_abort("vector for split variable '{var_name}' is empty (check input data)")

    # get labels
    var_label  <- tv(var_name)
    act_labels <- vapply(tree$action.names, tl, FUN.VALUE = "")
    preds      <- factor(pd$predictions, seq_along(act_labels), act_labels)

    # compute original-scale split value
    orig_cp <- get_original_value_plot(var_name, cp, original_df)

    # build subtitle, adding original value if available
    subtitle_txt <- if (!is.null(orig_cp)) {
      sprintf("%s: split at %.3f (%s)*", var_label, cp, orig_cp)
    } else {
      sprintf("%s: split at %.3f", var_label, cp)
    }

    plot_df <- tibble::tibble(x = x_vec, y = 0, pred = preds)
    colour_scale <- build_colour_scale(act_labels)

    ggplot2::ggplot() +
      ggplot2::geom_jitter(
        data   = plot_df,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$pred),
        width  = 0.30,
        height = 0.06,
        alpha  = point_alpha
      ) +
      ggplot2::geom_vline(xintercept = cp, linetype = "dashed") +
      colour_scale +
      ggplot2::labs(
        x        = var_label,
        y        = NULL,
        colour   = "Prediction",
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
          result_object        = result_object,
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

# helper: null-coalescing operator ------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x


#' @keywords internal
margot_plot_policy_tree_depth2 <- function(
    result_object,
    model_name,
    original_df            = NULL,
    shading                = TRUE,
    color_scale            = NULL,
    point_alpha            = 0.5,
    theme_function         = ggplot2::theme_classic,
    label_mapping          = NULL,
    label_options          = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ),
    title_size             = 16,
    subtitle_size          = 14,
    axis_title_size        = 14,
    legend_title_size      = 14,
    jitter_width           = 0.3,
    jitter_height          = 0.3,
    split_line_color       = "red",
    split_line_alpha       = 0.7,
    split_line_type        = "dashed",
    split_line_linewidth   = 0.5,
    split_label_size       = 10,
    split_label_color      = "red",
    custom_action_names    = NULL,
    legend_position        = "bottom",
    plot_selection         = "both",
    shade_fill             = "#6e6e6e",
    shade_alpha            = 0.35,
    ...
) {
  # ---- nudge fractions for annotation -----------------------------------
  nudge_frac_x <-  0.10   # top-of-panel label
  nudge_frac_y <- -0.10   # side (y-axis) label

  # ---- legacy: allow list input for shading -----------------------------
  if (is.list(shading)) {
    shade_enabled <- shading$enabled %||% TRUE
    shade_fill    <- shading$fill     %||% shade_fill
    shade_alpha   <- shading$alpha    %||% shade_alpha
  } else {
    shade_enabled <- isTRUE(shading)
  }

  # ---- helpers ----------------------------------------------------------
  tl <- function(x) tryCatch(transform_label(
    x, label_mapping, label_options), error = function(e) x)
  tv <- function(x) tryCatch(transform_var_name(
    x, label_mapping,
    remove_tx_prefix   = label_options$remove_tx_prefix,
    remove_z_suffix    = label_options$remove_z_suffix,
    use_title_case     = label_options$use_title_case,
    remove_underscores = label_options$remove_underscores
  ), error = function(e) x)

  # ---- get tree + plotting data ----------------------------------------
  tree_obj <- result_object$results[[model_name]]$policy_tree_depth_2
  if (is.null(tree_obj))
    cli::cli_abort("no depth-2 tree for model '{model_name}'")

  pd <- result_object$results[[model_name]]$plot_data
  Xt <- pd$X_test

  # ---- action labels & colour scale ------------------------------------
  act_labels <- if (is.null(custom_action_names)) {
    vapply(tree_obj$action.names, tl, FUN.VALUE = "")
  } else {
    vapply(custom_action_names, tl, FUN.VALUE = "")
  }

  if (is.null(color_scale)) {
    okabe <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
    color_scale <- ggplot2::scale_colour_manual(
      values = setNames(okabe[seq_along(act_labels)], act_labels)
    )
  }

  preds  <- factor(pd$predictions, seq_along(act_labels), act_labels)
  nodes  <- tree_obj$nodes

  # ---- split variables & cut-points ------------------------------------
  sv1 <- nodes[[1]]$split_variable; cp1 <- nodes[[1]]$split_value
  sv2 <- nodes[[2]]$split_variable; cp2 <- nodes[[2]]$split_value
  sv3 <- nodes[[3]]$split_variable; cp3 <- nodes[[3]]$split_value

  varnames <- names(Xt)
  plot_df  <- tibble::tibble(
    x1   = Xt[[varnames[sv1]]],
    x2   = Xt[[varnames[sv2]]],
    x3   = Xt[[varnames[sv3]]],
    pred = preds
  )

  # ---- panel constructor -----------------------------------------------
  build_panel <- function(x, y, xlab, ylab, xsp, ysp,
                          shade_side, xvar, yvar) {

    p <- ggplot2::ggplot()

    # --- shading ---------------------------------------------------------
    if (shade_enabled && shade_side != "none") {
      # Get the actual data range to ensure shading stays within bounds
      x_range <- range(plot_df[[x]], na.rm = TRUE)
      y_range <- range(plot_df[[y]], na.rm = TRUE)

      # Add small buffer to ensure we cover the plot area
      x_buffer <- diff(x_range) * 0.05
      y_buffer <- diff(y_range) * 0.05

      if (shade_side == "left") {
        # Shade left side of vertical split
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
        # Shade right side of vertical split
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
    # build_panel <- function(x, y, xlab, ylab, xsp, ysp,
    #                         shade_side, xvar, yvar) {
    #
    #   p <- ggplot2::ggplot()
    #
    #   # --- shading ---------------------------------------------------------
    #   if (shade_enabled && shade_side != "none") {
    #     xmin <- if (shade_side == "left") -Inf else xsp
    #     xmax <- if (shade_side == "left")  xsp else Inf
    #     p <- p + ggplot2::annotate(
    #       "rect", xmin = xmin, xmax = xmax,
    #       ymin  = -Inf, ymax = Inf,
    #       fill  = shade_fill, alpha = shade_alpha
    #     )
    #   }

    # --- back-transformed thresholds for annotation ----------------------
    orig_xsp <- get_original_value_plot(xvar, xsp, original_df)
    orig_ysp <- get_original_value_plot(yvar, ysp, original_df)

    # --- points ----------------------------------------------------------
    p <- p + ggplot2::geom_jitter(
      data   = plot_df,
      ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = pred),
      alpha  = point_alpha,
      width  = jitter_width,
      height = jitter_height
    )

    # --- split lines -----------------------------------------------------
    p <- p + ggplot2::geom_vline(
      xintercept = xsp, colour = split_line_color,
      alpha      = split_line_alpha,
      linetype   = split_line_type,
      linewidth  = split_line_linewidth
    ) +
      ggplot2::geom_hline(
        yintercept = ysp, colour = split_line_color,
        alpha      = split_line_alpha,
        linetype   = split_line_type,
        linewidth  = split_line_linewidth
      )

    # --- offsets for text ------------------------------------------------
    x_rng <- range(plot_df[[x]], na.rm = TRUE)
    off_x <- nudge_frac_x * diff(x_rng)
    off_y <- nudge_frac_y * diff(x_rng)
    if (off_x == 0) off_x <- 0.05
    if (off_y == 0) off_y <- 0.05

    # annotate x split (top)
    if (!is.null(orig_xsp))
      p <- p + ggplot2::annotate(
        "text",
        x      = xsp + off_x, y = Inf,
        label  = paste0("(", orig_xsp, ")*"),
        vjust  = 1,
        size   = split_label_size / ggplot2::.pt
      )

    # annotate y split (side)
    if (!is.null(orig_ysp))
      p <- p + ggplot2::annotate(
        "text",
        x      = x_rng[1] + off_y, y = ysp,
        label  = paste0("(", orig_ysp, ")*"),
        hjust  = 0,
        size   = split_label_size / ggplot2::.pt,
        angle  = 90
      )

    # --- final styling ---------------------------------------------------
    p + color_scale +
      ggplot2::labs(
        x        = xlab,
        y        = ylab,
        subtitle = paste(xlab, "vs", ylab),
        colour   = "Prediction"
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

  # ---- build panels -----------------------------------------------------

  # ---- build panels ----------------------------------------------------------
  p1 <- p2 <- NULL
  if (plot_selection %in% c("both","p1")) {
    p1 <- build_panel(
      "x1","x2",
      tv(varnames[sv1]), tv(varnames[sv2]),
      cp1, cp2,
      shade_side = if (shading) "right" else "none",
      xvar = varnames[sv1], yvar = varnames[sv2]
    )
  }
  if (plot_selection %in% c("both","p2")) {
    p2 <- build_panel(
      "x1","x3",
      tv(varnames[sv1]), tv(varnames[sv3]),
      cp1, cp3,
      shade_side = if (shading) "left" else "none",
      xvar = varnames[sv1], yvar = varnames[sv3]
    )
  }

  # ---- assemble layout with collected legend ---------------------------------
  main_title <- sprintf("Policy-tree results – %s", tv(model_name))

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



  #' Create a Combined Decision Tree and Policy Relationship Graph
  #'
  #' This function generates a combined plot consisting of a decision tree and a graph
  #' showing relationships between variables in the recommended policy.
  #'
  #' @param result_object An object containing the results from a multi-arm causal forest model.
  #' @param model_name A character string specifying the name of the model.
  #' @param max_depth Integer, 1 or 2; which decision tree depth to plot. Default: 2.
  #' @param label_mapping Optional named list for custom label mappings.
  #' @param original_df Optional dataframe with untransformed variables.
  #' @param layout A list specifying the layout of the combined plot when max_depth==2. Default is
  #'   `list(heights = c(1, 2))`, which sets the relative heights of the two plots.
  #' @param annotation A list specifying the annotation for the combined plot when max_depth==2. Default is
  #'   `list(tag_levels = "A")`, which adds alphabetic tags to the subplots.
  #' @param generate_policy_tree Logical, whether to generate the policy tree plot. Default is TRUE.
  #' @param generate_decision_tree Logical, whether to generate the decision tree plot. Default is TRUE.
  #' @param policy_tree_args A list of arguments to pass to `margot_plot_policy_tree`. Default is list().
  #' @param decision_tree_args A list of arguments to pass to `margot_plot_decision_tree`. Default is list().
  #'
  #' @return A list containing:
  #'   \item{policy_tree}{A ggplot object representing the policy tree (if generated)}
  #'   \item{decision_tree}{A ggplot object representing the decision tree (if generated)}
  #'   \item{combined_plot}{A ggplot object representing the combined plot (if both plots are generated)}
  #'
  #' @import ggplot2
  #' @import patchwork
  #' @import cli
  margot_plot_policy_combo <- function(
    result_object,
    model_name,
    max_depth              = 2L,
    label_mapping          = NULL,
    original_df            = NULL,
    layout                 = list(heights = c(1, 2)),
    annotation             = list(tag_levels = "A"),
    generate_policy_tree   = TRUE,
    generate_decision_tree = TRUE,
    policy_tree_args       = list(),
    decision_tree_args     = list()) {

    cli::cli_h1("Margot Plot Policy Combo")

    # ---- legacy: map shade_non_decision -> shading -----------------------
    if ("shade_non_decision" %in% names(policy_tree_args)) {
      policy_tree_args$shading <- isTRUE(policy_tree_args$shade_non_decision)
      policy_tree_args$shade_non_decision <- NULL
    }

    # ---- (1) decision-tree ----------------------------------------------
    decision_tree_plot <- NULL
    if (generate_decision_tree) {
      cli::cli_alert_info("Generating decision tree (depth {max_depth})...")
      decision_tree_plot <- do.call(
        margot_plot_decision_tree,
        c(
          list(
            result_object = result_object,
            model_name    = model_name,
            max_depth     = max_depth,
            original_df   = original_df,
            label_mapping = label_mapping
          ),
          decision_tree_args
        )
      )
      cli::cli_alert_success("Decision tree plot generated.")
    }

    # ---- (2) policy-tree -------------------------------------------------
    policy_tree_plot <- NULL
    if (generate_policy_tree) {
      cli::cli_alert_info("Generating policy tree (depth {max_depth})...")
      policy_tree_plot <- do.call(
        margot_plot_policy_tree,
        c(
          list(
            result_object = result_object,
            model_name    = model_name,
            max_depth     = max_depth,
            original_df   = original_df,
            label_mapping = label_mapping
          ),
          policy_tree_args
        )
      )
      cli::cli_alert_success("Policy tree plot generated.")
    }

    # ---- (3) combine if both requested -----------------------------------
    combined_plot <- NULL
    if (!is.null(decision_tree_plot) && !is.null(policy_tree_plot)) {
      cli::cli_alert_info("Combining plots...")
      if (max_depth == 1L) {
        # For depth 1, use special layout to reduce white space
        combined_plot <- (decision_tree_plot / policy_tree_plot) +
          patchwork::plot_layout(heights = c(1, 1)) +
          patchwork::plot_annotation(tag_levels = annotation$tag_levels) &
          ggplot2::theme(plot.margin = unit(c(5, 20, 5, 20), "pt"))  # Larger L/R margins
      } else {
        combined_plot <- (decision_tree_plot / policy_tree_plot) +
          patchwork::plot_layout(heights = layout$heights) +
          patchwork::plot_annotation(tag_levels = annotation$tag_levels)
      }
      cli::cli_alert_success("Plots combined successfully.")
    } else {
      combined_plot <- decision_tree_plot %||% policy_tree_plot
    }
    # old
    # combined_plot <- NULL
    # if (!is.null(decision_tree_plot) && !is.null(policy_tree_plot)) {
    #   cli::cli_alert_info("Combining plots...")
    #   if (max_depth == 1L) {
    #     combined_plot <- decision_tree_plot / policy_tree_plot
    #   } else {
    #     combined_plot <- (decision_tree_plot / policy_tree_plot) +
    #       patchwork::plot_layout(heights = layout$heights) +
    #       patchwork::plot_annotation(tag_levels = annotation$tag_levels)
    #   }
    #   cli::cli_alert_success("Plots combined successfully.")
    # } else {
    #   combined_plot <- decision_tree_plot %||% policy_tree_plot
    # }

    list(
      policy_tree   = policy_tree_plot,
      decision_tree = decision_tree_plot,
      combined_plot = combined_plot
    )
  }
