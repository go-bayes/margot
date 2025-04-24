#' Create a Coloured Histogram with Quantile or Custom Breaks
#'
#' This function creates a histogram with coloured regions based on quantile breaks or custom breaks.
#' It uses the `create_ordered_variable` function to categorise the data and then plots the histogram
#' with different colours for each category.
#'
#' @param df A data frame containing the variable to be plotted.
#' @param col_name The name of the column in the data frame to be plotted.
#' @param n_divisions The number of divisions for quantile breaks. Ignored if custom_breaks is provided.
#' @param custom_breaks A numeric vector of custom break points.
#' @param cutpoint_inclusive Character. Either "lower" or "upper", specifying whether the cutpoint should be included in the lower or upper interval.
#' @param ties.method A character string specifying how ties should be handled. See ?quantile for details.
#' @param colour_palette A vector of colours to use for the intervals. If NULL, uses the Okabe‑Ito palette.
#' @param hist_colour The colour of the histogram borders.
#' @param line_type The type of line to use for the histogram borders.
#' @param line_width The width of the lines for the histogram borders.
#' @param title The title of the plot. If NULL, a default title is used.
#' @param subtitle The subtitle of the plot. If NULL, a default subtitle is used.
#' @param x_lab The label for the x‑axis. If NULL, the formatted column name is used.
#' @param y_lab The label for the y‑axis. Default is "Count".
#' @param theme_choice The ggplot2 theme to use. Default is theme_classic().
#' @param text_size The base text size for the plot.
#' @param axis_text_angle The angle of the x‑axis text.
#' @param x_scale_transform Optional. A transformation for the x‑axis (e.g., "log10").
#' @param y_scale_transform Optional. A transformation for the y‑axis (e.g., "log10").
#' @param additional_layers A list of additional ggplot2 layers to add to the plot.
#' @param binwidth The width of the bins for the histogram. If NULL, calculated automatically.
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 16.
#' @param height The height of the saved plot in inches. Default is 10.
#' @param legend_position The position of the legend. Can be "right", "left", "bottom", "top", or a two‑element numeric vector.
#' @param include_timestamp A logical value indicating whether to include a timestamp in the saved filename. Default is FALSE.
#' @param file_prefix An optional prefix to add to the beginning of the saved filename.
#'
#' @return A ggplot2 object representing the coloured histogram.
#'
#' @import ggplot2
#' @importFrom ggokabeito palette_okabe_ito
#' @importFrom rlang sym
#' @importFrom stringr str_to_title
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#'
#' @export
margot_plot_categorical <- function(df, col_name, n_divisions = NULL, custom_breaks = NULL,
                                    cutpoint_inclusive = "upper",
                                    ties.method = NULL,
                                    colour_palette = NULL,
                                    hist_colour = "black",
                                    line_type = "solid", line_width = 0.75,
                                    title = NULL, subtitle = NULL,
                                    x_lab = NULL, y_lab = "Count",
                                    theme_choice = theme_classic(),
                                    text_size = 12, axis_text_angle = 45,
                                    x_scale_transform = NULL, y_scale_transform = NULL,
                                    additional_layers = NULL,
                                    binwidth = NULL,
                                    save_path = NULL,
                                    width = 16, height = 10,
                                    legend_position = "right",
                                    include_timestamp = FALSE,
                                    file_prefix = "") {

  cli::cli_h1("Margot Plot Categorical")

  tryCatch({
    # input validation
    if (!col_name %in% names(df)) {
      cli::cli_alert_danger("column '{col_name}' not found in the dataframe.")
      return(NULL)
    }

    # helper: convert to title case and preserve NZ
    format_label <- function(x) {
      lbl <- stringr::str_to_title(gsub("_", " ", x))
      gsub("Nz", "NZ", lbl)
    }

    # drop missing and warn
    original_rows <- nrow(df)
    df <- df[!is.na(df[[col_name]]), ]
    removed <- original_rows - nrow(df)
    if (removed > 0) {
      cli::cli_alert_warning("{removed} rows with NA in {col_name} were removed.")
    }

    # categorise using helper function
    result_df <- create_ordered_variable(df, col_name,
                                         n_divisions       = n_divisions,
                                         custom_breaks     = custom_breaks,
                                         cutpoint_inclusive= cutpoint_inclusive,
                                         ties.method       = ties.method)

    # detect new column name (_cat or _binary)
    local_divs <- if (!is.null(custom_breaks)) length(custom_breaks)-1 else n_divisions
    suffix     <- if (!is.null(local_divs) && local_divs==2) "_binary" else "_cat"
    expected   <- paste0(col_name, suffix)
    if (!expected %in% names(result_df)) {
      alt <- if (suffix=="_binary") "_cat" else "_binary"
      poss <- grep(paste0("^", col_name, alt, "$"), names(result_df), value=TRUE)
      new_col <- if (length(poss)) poss[1] else {
        cli::cli_alert_danger("no categorical column found for '{col_name}'")
        return(NULL)
      }
    } else new_col <- expected

    message("\nUsing categorical column: ", new_col, "\n")

    # get levels and set palette
    cat_lvls <- levels(result_df[[new_col]])
    if (is.null(colour_palette)) {
      pal <- ggokabeito::palette_okabe_ito()
      colour_palette <- pal[seq_along(cat_lvls)]
    } else if (length(colour_palette) < length(cat_lvls)) {
      cli::cli_alert_danger("provided palette has too few colours.")
      return(NULL)
    }

    # auto‑compute binwidth if needed
    if (is.null(binwidth)) {
      binwidth <- diff(range(df[[col_name]], na.rm=TRUE)) / 30
    }

    formatted <- format_label(col_name)

    # create plot: map x, y and fill globally so bins wear their colours proudly
    p <- ggplot(result_df) +
      geom_histogram(
        aes(x = !!rlang::sym(col_name),
            y = after_stat(count),
            fill = !!rlang::sym(new_col)),
        binwidth  = binwidth,
        # show all categories side‑by‑side, not stacked
        position  = "identity",
        # border is optional; keep it very thin or drop it
        colour    = NA,         # or "black"
        size      = 0.1,        # handsome but unobtrusive
        linetype  = line_type
      ) +
      scale_fill_manual(values = colour_palette, name = "Intervals") +
      labs(title    = title    %||% paste(formatted, "Distribution"),
           subtitle = subtitle %||% paste("coloured regions indicate intervals:",
                                          paste(cat_lvls, collapse=", ")),
           x        = x_lab     %||% formatted,
           y        = y_lab,
           caption  = sprintf("N = %d observations", nrow(result_df))) +
      theme_choice +
      theme(text            = element_text(size = text_size),
            axis.text.x     = element_text(angle = axis_text_angle, hjust = 1),
            legend.position = legend_position)

    # optional axis transforms
    if (!is.null(x_scale_transform)) p <- p + scale_x_continuous(trans = x_scale_transform)
    if (!is.null(y_scale_transform)) p <- p + scale_y_continuous(trans = y_scale_transform)

    # add any extra layers
    if (!is.null(additional_layers)) {
      for (ly in additional_layers) p <- p + ly
    }

    # save if requested
    if (!is.null(save_path)) {
      fn <- paste0(file_prefix %||% "", "categorical_", col_name,
                   if (include_timestamp) paste0("_", format(Sys.Date(), "%Y%m%d")) else "")
      cli::cli_alert_info("saving plot to '{file_prefix}'…")
      ggsave(plot = p,
             filename = file.path(save_path, paste0(fn, ".png")),
             width    = width,
             height   = height,
             units    = "in",
             dpi      = 300)
      margot::here_save_qs(p, fn, save_path, preset = "high", nthreads = 1)
      cli::cli_alert_success("plot saved as '{fn}.png'")
    } else {
      cli::cli_alert_info("no save path provided; plot not saved.")
    }

    cli::cli_alert_success("margot plot categorical created successfully 👍")
    return(p)

  }, error = function(e) {
    cli::cli_alert_danger("an error occurred: {conditionMessage(e)}")
    print(e)
    NULL
  })
}
