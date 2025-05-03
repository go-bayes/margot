#' @title Plot Policy Tree Results with Shading Option
#'
#' @description
#' Visualise the top-two splits of a policy tree with scatterplots of the two split variables,
#' points coloured by predicted action, and optional shading for non-decision regions.
#'
#' @param mc_test List from a multi-arm causal forest run containing
#'   \code{$results[[model_name]]$policy_tree_depth_2} and
#'   \code{$results[[model_name]]$plot_data}.
#' @param model_name String name of the model in \code{mc_test$results}.
#' @param original_df Optional data.frame of original-scale variables for axis annotations.
#' @param shading Logical; if \code{TRUE}, shade non-decision regions. Default: \code{TRUE}.
#' @param color_scale Optional ggplot2 colour scale. Default: Okabe–Ito palette.
#' @param point_alpha Numeric point transparency (0–1). Default: 0.5.
#' @param theme_function ggplot2 theme function. Default: \code{theme_classic()}.
#' @param title_size Numeric plot-title text size. Default: 16.
#' @param subtitle_size Numeric subtitle text size. Default: 14.
#' @param axis_title_size Numeric axis-title text size. Default: 14.
#' @param legend_title_size Numeric legend-title text size. Default: 14.
#' @param jitter_width Numeric horizontal jitter. Default: 0.3.
#' @param jitter_height Numeric vertical jitter. Default: 0.3.
#' @param split_line_color Colour of split lines. Default: "red".
#' @param split_line_alpha Alpha for split lines. Default: 0.7.
#' @param split_line_type Linetype for split lines. Default: "dashed".
#' @param split_line_linewidth Linewidth for split lines. Default: 0.5.
#' @param split_label_size Text size for secondary-axis labels. Default: 10.
#' @param split_label_color Colour for secondary-axis labels. Default: "red".
#' @param custom_action_names Optional vector of custom action labels.
#' @param legend_position Legend position: "top","bottom","left","right". Default: "bottom".
#' @param plot_selection Which panels to show: "both","p1","p2". Default: "both".
#' @param remove_tx_prefix Logical; strip "tx_" from names. Default: TRUE.
#' @param remove_z_suffix Logical; strip "_z" from names. Default: TRUE.
#' @param use_title_case Logical; convert labels to Title Case. Default: TRUE.
#' @param remove_underscores Logical; remove underscores. Default: TRUE.
#' @param label_mapping Named list of label overrides. Default: NULL.
#' @param shade_fill Fill colour for shading. Default: "#6e6e6e".
#' @param shade_alpha Alpha for shading. Default: 0.35.
#'
#' @return A patchwork ggplot of the two panels.
#'
#' @import ggplot2
#' @import patchwork
#' @import cli
margot_plot_policy_tree <- function(
    mc_test,
    model_name,
    original_df        = NULL,
    shading            = TRUE,
    color_scale        = NULL,
    point_alpha        = 0.5,
    theme_function     = theme_classic,
    title_size         = 16,
    subtitle_size      = 14,
    axis_title_size    = 14,
    legend_title_size  = 14,
    jitter_width       = 0.3,
    jitter_height      = 0.3,
    split_line_color   = "red",
    split_line_alpha   = 0.7,
    split_line_type    = "dashed",
    split_line_linewidth = 0.5,
    split_label_size   = 10,
    split_label_color  = "red",
    custom_action_names= NULL,
    legend_position    = "bottom",
    plot_selection     = "both",
    remove_tx_prefix   = TRUE,
    remove_z_suffix    = TRUE,
    use_title_case     = TRUE,
    remove_underscores = TRUE,
    label_mapping      = NULL,
    shade_fill         = "#6e6e6e",
    shade_alpha        = 0.5
) {
  cli::cli_h1("Margot Plot Policy Tree")

  # set up title
  title <- transform_var_name(
    model_name, label_mapping,
    remove_tx_prefix, remove_z_suffix,
    use_title_case, remove_underscores
  )
  if (!is.character(title) || is.null(title)) title <- as.character(title)
  cli::cli_alert_info("using title: {title}")

  # extract policy tree object
  tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
  if (is.null(tree_obj)) cli::cli_abort("Policy tree object not found for '{model_name}'")
  cli::cli_alert_success("policy tree object extracted")

  # action names
  action_names <- tree_obj$action.names
  n_actions    <- tree_obj$n.actions
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != n_actions) cli::cli_abort("custom_action_names length mismatch")
    action_names <- custom_action_names
    cli::cli_alert_info("using custom action names")
  }
  action_names <- vapply(
    action_names,
    transform_var_name,
    FUN.VALUE = "",
    label_mapping,
    remove_tx_prefix,
    remove_z_suffix,
    use_title_case,
    remove_underscores
  )
  cli::cli_alert_success("action names transformed")

  # colour scale
  okabe <- c("#56B4E9","#E69F00","#009E73","#F0E442",
             "#0072B2","#D55E00","#CC79A7","#000000")
  if (is.null(color_scale)) {
    cols <- setNames(okabe[1:n_actions], action_names)
    color_scale <- scale_colour_manual(values = cols)
    cli::cli_alert_info("okabe-ito palette applied")
  }

  # prepare data
  plot_data <- mc_test$results[[model_name]]$plot_data
  X_test     <- plot_data$X_test
  preds      <- plot_data$predictions
  nodes      <- tree_obj$nodes
  idx1 <- nodes[[1]]$split_variable; sv1 <- nodes[[1]]$split_value
  idx2 <- nodes[[2]]$split_variable; sv2 <- nodes[[2]]$split_value
  idx3 <- nodes[[3]]$split_variable; sv3 <- nodes[[3]]$split_value

  actual_cols <- names(X_test)
  trans_cols  <- vapply(
    actual_cols,
    transform_var_name,
    FUN.VALUE = "",
    label_mapping,
    remove_tx_prefix,
    remove_z_suffix,
    use_title_case,
    remove_underscores
  )

  plot_df <- data.frame(
    x1 = X_test[[actual_cols[idx1]]],
    x2 = X_test[[actual_cols[idx2]]],
    x3 = X_test[[actual_cols[idx3]]],
    prediction = factor(preds, seq_along(action_names), action_names)
  )
  cli::cli_alert_success("plot data frame ready")

  # secondary-axis helper
  create_sec <- function(rng, split, varnm) {
    brk <- c(min(rng), split, max(rng))
    if (!is.null(original_df)) {
      orig <- get_original_value_plot(varnm, split, original_df)
      lbl  <- if (!is.null(orig))
        c("", sprintf("Split: %.2f (%s)*", split, format(orig, big.mark = ",")), "")
      else c("", sprintf("Split: %.2f", split), "")
    } else {
      lbl <- c("", sprintf("Split: %.2f", split), "")
    }
    sec_axis(~., breaks = brk, labels = lbl)
  }

  # base-plot function
  base_plot <- function(x, y, xl, yl, sub,
                        xsp, ysp, xnm, ynm,
                        shade_side = c("none", "left", "right")) {
    sd    <- match.arg(shade_side)
    x_rng <- range(plot_df[[x]], na.rm = TRUE)
    y_rng <- range(plot_df[[y]], na.rm = TRUE)
    p     <- ggplot()

    # shading
    if (shading && sd != "none") {
      xmin <- if (sd == "left") -Inf else xsp
      xmax <- if (sd == "left") xsp  else Inf
      p    <- p + annotate(
        "rect",
        xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf,
        fill = shade_fill, alpha = shade_alpha
      )
    }

    p +
      geom_jitter(
        data  = plot_df,
        aes(x = .data[[x]], y = .data[[y]], colour = prediction),
        alpha = point_alpha,
        width = jitter_width,
        height = jitter_height
      ) +
      geom_vline(
        xintercept = xsp,
        color      = split_line_color,
        alpha      = split_line_alpha,
        linetype   = split_line_type,
        linewidth  = split_line_linewidth
      ) +
      geom_hline(
        yintercept = ysp,
        color      = split_line_color,
        alpha      = split_line_alpha,
        linetype   = split_line_type,
        linewidth  = split_line_linewidth
      ) +
      color_scale +
      labs(x = xl, y = yl, subtitle = sub, colour = "Prediction") +
      theme_function() +
      theme(
        plot.title        = element_text(size = title_size, margin = margin(b = 20)),
        plot.subtitle     = element_text(size = subtitle_size),
        axis.title        = element_text(size = axis_title_size),
        legend.title      = element_text(size = legend_title_size),
        axis.text.x.top   = element_text(size = split_label_size, color = split_label_color),
        axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
      ) +
      scale_x_continuous(sec.axis = create_sec(plot_df[[x]], xsp, xnm)) +
      scale_y_continuous(sec.axis = create_sec(plot_df[[y]], ysp, ynm))
  }

  # build panels
  cli::cli_h2("creating panels")
  p1 <- p2 <- NULL
  if (plot_selection %in% c("both", "p1")) {
    p1 <- base_plot(
      "x1", "x2",
      trans_cols[idx1], trans_cols[idx2],
      paste(trans_cols[idx1], "vs", trans_cols[idx2]),
      sv1, sv2, actual_cols[idx1], actual_cols[idx2],
      shade_side = if (shading) "right" else "none"
    )
    cli::cli_alert_success("panel A done")
  }
  if (plot_selection %in% c("both", "p2")) {
    p2 <- base_plot(
      "x1", "x3",
      trans_cols[idx1], trans_cols[idx3],
      paste(trans_cols[idx1], "vs", trans_cols[idx3]),
      sv1, sv3, actual_cols[idx1], actual_cols[idx3],
      shade_side = if (shading) "left" else "none"
    )
    cli::cli_alert_success("panel B done")
  }

  # assemble final plot
  cli::cli_h2("assembling final plot")
  combined <- switch(
    plot_selection,
    both = p1 + p2 + plot_layout(guides = "collect") +
      plot_annotation(
        title   = if (!is.null(title)) paste("Policy Tree Results for", title),
        caption = if (!is.null(original_df)) "* original scale value",
        tag_levels = 'A'
      ) & theme(legend.position = legend_position),
    p1   = p1 + plot_annotation(
      title   = paste("Policy Tree Results for", title, "(Panel A)"),
      caption = if (!is.null(original_df)) "* original scale value",
      tag_levels = 'A'
    ) & theme(legend.position = legend_position),
    p2   = p2 + plot_layout(guides = "collect") +
      plot_annotation(
        title   = paste("Policy Tree Results for", title, "(Panel B)"),
        caption = if (!is.null(original_df)) "* original scale value",
        tag_levels = 'A'
      ) & theme(legend.position = legend_position)
  )

  if (is.null(combined)) cli::cli_abort("failed to assemble final plot")
  cli::cli_alert_success("plot ready 🎉")
  combined
}
