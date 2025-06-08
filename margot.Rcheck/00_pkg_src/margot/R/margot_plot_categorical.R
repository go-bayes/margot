#' Create a coloured histogram with summary lines and optional median
#'
#' @inheritParams margot_plot_categorical
#' @param label_mapping named vector; optional remapping of variable names for labels.
#' @param show_mean logical; draw vertical line at mean. default FALSE.
#' @param show_sd logical; draw dashed lines at mean ± sd_multipliers. default FALSE.
#' @param sd_multipliers numeric(2); multipliers for sd bands, c(lower, upper). default c(-2,1).
#' @param show_median logical; draw vertical line at median. default FALSE.
#' @param print_summary logical; annotate mean/median values. default TRUE.
#' @export
margot_plot_categorical <- function(
    df,
    col_name,
    label_mapping   = NULL,
    n_divisions     = NULL,
    custom_breaks   = NULL,
    cutpoint_inclusive = "upper",
    ties.method     = NULL,
    colour_palette  = NULL,
    hist_colour     = NA,
    line_type       = "solid",
    line_width      = 0.75,
    title           = NULL,
    subtitle        = NULL,
    x_lab           = NULL,
    y_lab           = "Count",
    theme_choice    = theme_classic(),
    text_size       = 12,
    axis_text_angle = 45,
    x_scale_transform = NULL,
    y_scale_transform = NULL,
    additional_layers  = NULL,
    binwidth          = NULL,
    legend_position   = "right",
    show_mean      = FALSE,
    show_sd        = FALSE,
    sd_multipliers = c(-2,1),
    show_median    = FALSE,
    print_summary  = TRUE,
    save_path      = NULL,
    width          = 16,
    height         = 10,
    include_timestamp = FALSE,
    file_prefix    = ""
) {
  cli::cli_h1("margot plot categorical")
  tryCatch({
    if (!col_name %in% names(df)) stop("column not found in dataframe")
    n0 <- nrow(df)
    df <- df[!is.na(df[[col_name]]), ]
    if (n0 - nrow(df) > 0) cli::cli_alert_info("removed NA rows: {n0 - nrow(df)}")

    result <- create_ordered_variable(
      df,
      col_name,
      n_divisions        = n_divisions,
      custom_breaks      = custom_breaks,
      cutpoint_inclusive = cutpoint_inclusive,
      ties.method        = ties.method
    )

    suffix <- if (!is.null(custom_breaks) && length(custom_breaks) - 1 == 2) "_binary" else "_cat"
    cat_col <- paste0(col_name, suffix)
    if (!cat_col %in% names(result)) {
      alt <- if (suffix=="_binary") paste0(col_name, "_cat") else paste0(col_name, "_binary")
      cat_col <- grep(paste0("^",alt,"$"), names(result), value=TRUE)[1]
      if (is.na(cat_col)) stop("no categorical column for {col_name}")
    }

    levels_vec <- levels(result[[cat_col]])
    if (is.null(colour_palette)) {
      pal <- ggokabeito::palette_okabe_ito()
      colour_palette <- pal[seq_along(levels_vec)]
    }

    if (is.null(binwidth)) binwidth <- diff(range(df[[col_name]], na.rm=TRUE))/30

    formatted <- transform_var_name(col_name, label_mapping = label_mapping)
    stats <- list(
      mean = mean(df[[col_name]], na.rm=TRUE),
      sd   = sd  (df[[col_name]], na.rm=TRUE),
      med  = median(df[[col_name]], na.rm=TRUE)
    )
    if (show_mean)   cli::cli_alert_info("mean: {round(stats$mean,2)}")
    if (show_median) cli::cli_alert_info("median: {round(stats$med,2)}")

    p <- ggplot(result) +
      geom_histogram(
        aes(x=!!rlang::sym(col_name), y=after_stat(count), fill=!!rlang::sym(cat_col)),
        binwidth  = binwidth,
        position  = "identity",
        linewidth = line_width,
        linetype  = line_type,
        colour    = hist_colour
      ) +
      scale_fill_manual(values=colour_palette, name="Intervals") +
      labs(
        title    = title %||% paste(formatted, "Distribution"),
        subtitle = subtitle %||% paste("intervals:", paste(levels_vec, collapse=", ")),
        x        = x_lab %||% formatted,
        y        = y_lab,
        caption  = sprintf("N = %d", nrow(result))
      ) +
      theme_choice +
      theme(
        text        = element_text(size=text_size),
        axis.text.x = element_text(angle=axis_text_angle, hjust=1),
        legend.position = legend_position
      )

    if (show_mean) {
      p <- p + geom_vline(xintercept=stats$mean, linetype="dashed", linewidth=0.75)
      if (print_summary) {
        p <- p + annotate("text", x=stats$mean, y=0,
                          label=round(stats$mean,2), hjust=-0.1, vjust=-0.5)
      }
    }
    if (show_sd) {
      lo <- stats$mean + sd_multipliers[1] * stats$sd
      hi <- stats$mean + sd_multipliers[2] * stats$sd
      p <- p + geom_vline(xintercept=lo, linetype="dotted", linewidth=0.5) +
        geom_vline(xintercept=hi, linetype="dotted", linewidth=0.5)
      if (print_summary) {
        p <- p + annotate("text", x=lo, label=paste0(sd_multipliers[1], "σ"), y=0,
                          hjust=1.1, vjust=-0.5) +
          annotate("text", x=hi, label=paste0("+",sd_multipliers[2], "σ"), y=0,
                   hjust=-0.1, vjust=-0.5)
      }
    }
    if (show_median) {
      p <- p + geom_vline(xintercept=stats$med, linetype="dotdash", linewidth=0.75)
      if (print_summary) {
        p <- p + annotate("text", x=stats$med, y=0,
                          label=round(stats$med,2), hjust=1.1, vjust=1.5)
      }
    }

    if (!is.null(x_scale_transform)) p <- p + scale_x_continuous(trans=x_scale_transform)
    if (!is.null(y_scale_transform)) p <- p + scale_y_continuous(trans=y_scale_transform)
    if (!is.null(additional_layers)) for (ly in additional_layers) p <- p + ly

    if (!is.null(save_path)) {
      fn <- paste0(
        file_prefix,
        "categorical_", col_name,
        if (include_timestamp) paste0("_", format(Sys.Date(), "%Y%m%d")) else ""
      )
      cli::cli_alert_info("saving: {fn}.png")
      ggsave(filename=file.path(save_path, paste0(fn, ".png")),
             plot=p, width=width, height=height, units="in", dpi=300)
      margot::here_save_qs(p, fn, save_path, preset="high", nthreads=1)
      cli::cli_alert_success("saved: {fn}.png")
    }
    p
  }, error=function(e){
    cli::cli_alert_danger("error: {conditionMessage(e)}")
    NULL
  })
}

#' Visualise shifts in data distributions with highlighted ranges
#'
#' @inheritParams margot_plot_categorical
#' @export
margot_plot_shift <- function(
    df,
    col_name,
    label_mapping   = NULL,
    binwidth        = 1,
    range_highlight = NULL,
    shift           = "up",
    show_avg_line   = TRUE,
    print_avg_value = TRUE,
    show_sd_line    = TRUE,
    title           = NULL,
    subtitle        = NULL,
    x_lab           = NULL,
    y_lab           = "Count",
    save_path       = NULL,
    width           = 12,
    height          = 8,
    include_timestamp = FALSE
) {
  cli::cli_h1("margot plot shift")
  tryCatch({
    if (!col_name %in% names(df)) stop("column not found in dataframe")
    if (all(is.na(df[[col_name]]))) stop("column contains only NA")
    if (!shift %in% c("up","down")) stop("shift must be up or down")

    formatted <- transform_var_name(col_name, label_mapping = label_mapping)
    stats <- list(
      mean = mean(df[[col_name]], na.rm=TRUE),
      sd   = sd(df[[col_name]], na.rm=TRUE)
    )
    if (show_avg_line) cli::cli_alert_info("average: {round(stats$mean,2)}")

    df$fill_color <- "lightgray"
    if (!is.null(range_highlight) && length(range_highlight)==2) {
      mask <- df[[col_name]] >= range_highlight[1] & df[[col_name]] <= range_highlight[2]
      df$fill_color[mask] <- if (shift=="up") "gold2" else "dodgerblue"
      cli::cli_alert_info("highlight: [{range_highlight[1]}, {range_highlight[2]}]")
    }

    if (is.null(subtitle)) {
      subtitle <- paste0(
        if (shift=="up") "shifts up" else "shifts down",
        if (show_avg_line) "\nred dashed = mean" else "",
        if (show_sd_line) "\ngrey dashed = ±1 sd" else ""
      )
    }

    p <- ggplot(df, aes(x=!!rlang::sym(col_name), fill=fill_color)) +
      geom_histogram(binwidth=binwidth, alpha=0.7, linewidth=0.5) +
      scale_fill_identity() +
      labs(
        title    = title %||% paste(formatted, "Distribution"),
        subtitle = subtitle,
        x        = x_lab %||% formatted,
        y        = y_lab,
        caption  = sprintf("N = %d", nrow(df))
      ) +
      theme_minimal() +
      theme(
        text        = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1)
      )

    if (show_avg_line) {
      p <- p + geom_vline(xintercept=stats$mean, linetype="dashed", linewidth=0.75)
      if (print_avg_value) {
        p <- p + annotate("text", x=stats$mean, y=0,
                          label=round(stats$mean,2), hjust=1.1, vjust=-0.5)
      }
    }
    if (show_sd_line) {
      p <- p + geom_vline(xintercept=stats$mean-stats$sd, linetype="dashed", linewidth=0.5) +
        geom_vline(xintercept=stats$mean+stats$sd, linetype="dashed", linewidth=0.5)
    }

    if (!is.null(save_path)) {
      fn <- paste0(
        "shift_", col_name,
        if (include_timestamp) paste0("_", format(Sys.Date(),"%Y%m%d")) else ""
      )
      cli::cli_alert_info("saving: {fn}.png")
      ggsave(filename=file.path(save_path,paste0(fn,".png")), plot=p,
             width=width, height=height, units="in", dpi=300)
      margot::here_save_qs(p, fn, save_path, preset="high", nthreads=1)
      cli::cli_alert_success("saved: {fn}.png")
    }
    p
  }, error=function(e){
    cli::cli_alert_danger("error: {conditionMessage(e)}")
    NULL
  })
}
