#' Publication forest figure of estimated effects
#'
#' Draws estimated effects and their confidence intervals as a horizontal forest figure built to a journal column width: a label column, one interval panel per contrast, optional decimal-aligned numeric columns, and a footer with one axis title and a key. Every dimension is derived from the content and the target width, so the same call works across studies with different numbers of outcomes, label lengths, contrasts and scales.
#'
#' Points are estimates and horizontal lines are the intervals in `data`. A filled point marks an interval that excludes the null (0 for differences, 1 for ratios) and an open point one that includes it, so status does not depend on colour. Ratios are drawn on a log axis. The axis breaks bracket every interval and the null, are chosen so that adjacent tick labels stay at least 2.5 mm apart at the stated width, and the call stops rather than draw a figure that would cut off part of an interval.
#'
#' Numbers in the columns are rounded for display only. The number of decimals is the fewest, from two, at which no limit of an interval that excludes the null prints as the null itself; `digits` overrides it. Each character is placed from its unkerned advance width, so decimal points align exactly down each column, and the text stays live in the PDF.
#'
#' Type is set at `size_pt` (default 6.5 pt) in Arial when it is installed, falling back to Helvetica, Liberation Sans or DejaVu Sans with a warning. Text widths, and so the layout, depend on the font file, which is recorded on the returned object.
#'
#' @param data Tidy effects table, usually from [margot_lmtp_effects()]. Required columns: `outcome`, `estimate`, `lower`, `upper` and `conf_level`. Optional columns: `label`, `contrast`, `type` (`"difference"`, the default, or `"ratio"`), `group`, `raw_estimate`, `raw_lower`, `raw_upper` (differences in original units), `mean_reference`, `mean_intervention`, `evalue` and `evalue_bound`. Tables from other estimators can be supplied in the same format.
#' @param xlab Axis title for the interval panels. A line break in the string starts a second line.
#' @param plot Plotted quantity: `"estimate"` or `"raw"` (the original-unit columns).
#' @param columns Numeric columns to the right of the panels, in order: any of `"plotted"` (the plotted quantity), `"estimate"`, `"raw"`, `"means"` (reference and intervention means) and `"evalue"` (point and interval E-values).
#' @param column_titles List with one `c(title, subtitle)` per column; `NULL` entries use the defaults. `"{ci}"` is replaced by the interval label (for example `"99.58% CI"`), and a subtitle written `"a|b"` puts one sub-heading over each number.
#' @param family_title Title above the label column, for example `"Outcome"`.
#' @param group_titles Named character vector mapping values of `group` to display titles, in display order.
#' @param order `"estimate"` (descending within each group, by the first contrast's plotted estimate) or `"given"` (row order of `data`).
#' @param contrast_titles Named character vector of titles for the contrasts, used when there is more than one.
#' @param key_labels Labels for the filled and open points. Default `"Interval excludes 0"` and `"Interval includes 0"` (1 for ratios); `NULL` omits the key.
#' @param width_mm Final figure width in millimetres, margin included. Nature uses 89, 120 and 183 mm; Science and PNAS use similar column widths.
#' @param size_pt Type size in points at print size.
#' @param label_max_mm Widest label column before labels wrap.
#' @param stripe Shade alternate rows.
#' @param digits Decimals for the estimate and original-unit columns; `NULL` chooses them as described above.
#' @param margin_mm White margin inside `width_mm`.
#' @param family Font family; `NULL` chooses as described above.
#'
#' @return A patchwork object. Attributes: `size_mm` (final width and height), `margin_mm`, `layout_mm` (widths of the label column, each interval panel and each numeric column), `axis` (breaks, data range, limits and smallest gap between tick labels in mm), `font` (family, font file and its MD5 checksum) and `plotted` (the drawn estimate and interval for each outcome and contrast, top to bottom, with `excludes_null`, which sets the filled point). Save it with [margot_save_effects_plot()], which uses `size_mm`.
#'
#' @seealso [margot_lmtp_effects()], [margot_save_effects_plot()]
#' @md
#'
#' @examples
#' effects <- data.frame(
#'   outcome = c("y_warmth", "y_trust", "y_belonging"),
#'   label = c("Warmth", "Trust in others", "Sense of belonging"),
#'   estimate = c(0.12, -0.03, 0.05),
#'   lower = c(0.07, -0.08, -0.01),
#'   upper = c(0.17, 0.02, 0.11),
#'   conf_level = 1 - 0.05 / 3
#' )
#' fig <- margot_plot_effects(effects, xlab = "Difference in mean outcome, SD units",
#'   columns = "estimate", family_title = "Outcome", width_mm = 120)
#' attr(fig, "size_mm")
#'
#' @export
margot_plot_effects <- function(data, xlab, plot = c("estimate", "raw"), columns = character(),
                                column_titles = list(), family_title = NULL, group_titles = NULL,
                                order = c("estimate", "given"), contrast_titles = NULL,
                                key_labels = NULL, width_mm = 120, size_pt = 6.5, label_max_mm = 45,
                                stripe = TRUE, digits = NULL, margin_mm = 1, family = NULL) {
  ordering <- match.arg(order)
  plot <- match.arg(plot)
  data <- .margot_effects_validate(data)
  family <- .margot_effects_font(family)
  type <- data$type[1L]
  null <- if (type == "ratio") 1 else 0
  if (plot == "raw") {
    if (type != "difference" || is.null(data$raw_estimate) || anyNA(data$raw_estimate)) {
      stop("plot = \"raw\" needs differences with raw_estimate, raw_lower and raw_upper.", call. = FALSE)
    }
    data$plot_estimate <- data$raw_estimate
    data$plot_lower <- data$raw_lower
    data$plot_upper <- data$raw_upper
  } else {
    data$plot_estimate <- data$estimate
    data$plot_lower <- data$lower
    data$plot_upper <- data$upper
  }
  columns[columns == "plotted"] <- plot
  needs <- list(estimate = c("estimate", "lower", "upper"), raw = c("raw_estimate", "raw_lower", "raw_upper"),
                means = c("mean_reference", "mean_intervention"), evalue = c("evalue", "evalue_bound"))
  for (col in setdiff(columns, names(needs))) stop("unknown column: ", col, call. = FALSE)
  for (col in columns) {
    have <- vapply(needs[[col]], function(f) !is.null(data[[f]]) && !anyNA(data[[f]]), logical(1))
    if (!all(have)) {
      stop("column \"", col, "\" needs ", paste(needs[[col]][!have], collapse = ", "),
           ", which the effects table lacks or leaves missing.", call. = FALSE)
    }
  }
  contrasts <- unique(data$contrast)
  sets <- lapply(contrasts, function(k) sort(data$outcome[data$contrast == k]))
  if (!all(vapply(sets, identical, logical(1), sets[[1L]]))) {
    stop("contrasts must share one outcome set; missing: ",
         paste(unique(unlist(lapply(sets, function(s) setdiff(unlist(sets), s)))), collapse = ", "), call. = FALSE)
  }
  has_group <- !is.null(data$group)
  if (!has_group) data$group <- ""
  data$group <- as.character(data$group)
  mm_pt <- 25.4 / 72.27
  line_mm <- size_pt * mm_pt * 1.25
  if (missing(key_labels)) key_labels <- paste("Interval", c("excludes", "includes"), null)
  ci_label <- .margot_effects_ci_label(data$conf_level)
  defaults <- list(estimate = c("Estimate", "[{ci}]"), raw = c("Original units", "[{ci}]"),
                   means = c("Mean outcome", "reference \u2192 intervention"),
                   evalue = c("E-value", "point|CI limit"))
  column_titles <- lapply(seq_along(columns), function(i) {
    title <- if (i <= length(column_titles)) column_titles[[i]] else NULL
    gsub("{ci}", ci_label, title %||% defaults[[columns[i]]], fixed = TRUE)
  })
  text_mm <- function(x, bold = FALSE) .margot_effects_text_mm(x, size_pt, family, bold)
  ts <- .margot_effects_ts(size_pt)

  # row order: groups in the order of `group_titles` (else first appearance); within
  # each group by the first contrast's plotted estimate, or as given.
  first <- data[data$contrast == contrasts[1L], , drop = FALSE]
  groups <- unique(c(intersect(names(group_titles), first$group), first$group))
  keyed <- do.call(rbind, lapply(groups, function(g) {
    rows <- first[first$group == g, , drop = FALSE]
    if (ordering == "estimate") rows[base::order(-rows$plot_estimate), , drop = FALSE] else rows
  }))
  contrast_title <- function(k) {
    title <- if (is.null(contrast_titles)) NA_character_ else unname(contrast_titles[k])
    if (is.na(title)) k else title
  }
  group_title <- function(g) {
    if (is.null(group_titles)) return(g)
    titles <- unname(group_titles[g])
    ifelse(is.na(titles), g, titles)
  }

  # label column width from the content, wrapping labels wider than `label_max_mm`.
  indent_mm <- if (has_group) 2 else 0
  heads <- c(family_title, if (has_group) group_title(groups))
  label_mm <- min(label_max_mm, max(c(text_mm(keyed$label) + indent_mm, text_mm(heads, bold = TRUE))) + 2)
  # a word or heading that cannot wrap widens the column rather than spill into the panel.
  words <- unlist(strsplit(keyed$label, " ", fixed = TRUE))
  label_mm <- max(label_mm, max(text_mm(words)) + indent_mm + 2, max(c(0, text_mm(heads, bold = TRUE))) + 2)
  wrap_label <- function(s) {
    room <- label_mm - indent_mm - 2
    if (text_mm(s) <= room) return(s)
    words <- strsplit(s, " ")[[1L]]
    lines <- character()
    current <- ""
    for (w in words) {
      trial <- if (nzchar(current)) paste(current, w) else w
      if (text_mm(trial) > room && nzchar(current)) {
        lines <- c(lines, current)
        current <- w
      } else {
        current <- trial
      }
    }
    paste(c(lines, current), collapse = "\n")
  }
  keyed$display <- vapply(keyed$label, wrap_label, character(1), USE.NAMES = FALSE)
  keyed$lines <- lengths(regmatches(keyed$display, gregexpr("\n", keyed$display))) + 1L

  # vertical layout in row units, top to bottom: header, then per group a heading and rows.
  two_line <- length(columns) > 0L || length(contrasts) > 1L
  header_h <- if (two_line) 1.9 else 1.1
  header_y <- -header_h / 2
  y <- -header_h
  layout <- list()
  for (g in groups) {
    if (has_group) {
      y <- y - if (g == groups[1L]) 0.75 else 1.05
      layout[[length(layout) + 1L]] <- data.frame(kind = "group", key = g, y = y, height = 1,
                                                  stringsAsFactors = FALSE)
      y <- y - 0.5
    }
    rows <- keyed[keyed$group == g, , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      h <- 1 + 0.85 * (rows$lines[i] - 1L)
      layout[[length(layout) + 1L]] <- data.frame(kind = "row", key = rows$outcome[i], y = y - h / 2,
                                                  height = h, stringsAsFactors = FALSE)
      y <- y - h
    }
  }
  layout <- do.call(rbind, layout)
  ylim <- c(y - 0.25, 0)
  rows_layout <- layout[layout$kind == "row", , drop = FALSE]
  rows_layout$band <- seq_len(nrow(rows_layout)) %% 2 == 0
  row_mm <- size_pt * mm_pt * 1.8
  row_top <- max(rows_layout$y + rows_layout$height / 2)
  row_bottom <- min(rows_layout$y - rows_layout$height / 2)

  base_theme <- ggplot2::theme_void(base_family = family, base_size = size_pt) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  stripes <- function(xr = c(-Inf, Inf)) {
    b <- rows_layout[rows_layout$band, , drop = FALSE]
    if (!stripe || !nrow(b)) return(NULL)
    ggplot2::annotate("rect", xmin = xr[1L], xmax = xr[2L], ymin = b$y - b$height / 2,
                      ymax = b$y + b$height / 2, fill = "grey94", colour = NA)
  }
  y_scale <- function() {
    list(ggplot2::scale_y_continuous(expand = c(0, 0)), ggplot2::coord_cartesian(ylim = ylim, clip = "off"))
  }
  header_text <- function(x, title) {
    if (length(title) > 1L) {
      list(ggplot2::annotate("text", x = x, y = header_y + 0.42, label = title[1L], fontface = "bold",
                             size = ts, family = family),
           ggplot2::annotate("text", x = x, y = header_y - 0.45, label = title[2L], size = ts, family = family))
    } else {
      ggplot2::annotate("text", x = x, y = if (two_line) header_y + 0.42 else header_y, label = title[1L],
                        fontface = "bold", size = ts, family = family)
    }
  }

  # label column.
  lab <- merge(rows_layout, keyed[c("outcome", "display")], by.x = "key", by.y = "outcome")
  grp <- layout[layout$kind == "group", , drop = FALSE]
  if (nrow(grp)) grp$title <- group_title(grp$key)
  p_label <- ggplot2::ggplot() + stripes() +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = indent_mm, y = .data$y, label = .data$display),
                       hjust = 0, lineheight = 0.95, size = ts, family = family, colour = "black") +
    ggplot2::scale_x_continuous(limits = c(0, label_mm), expand = c(0, 0)) + y_scale() + base_theme
  if (nrow(grp)) {
    p_label <- p_label + ggplot2::geom_text(data = grp, ggplot2::aes(x = 0, y = .data$y, label = .data$title),
                                            hjust = 0, fontface = "bold", size = ts, family = family,
                                            colour = "black")
  }
  if (!is.null(family_title)) {
    p_label <- p_label + ggplot2::annotate("text", x = 0, y = if (two_line) header_y - 0.45 else header_y,
                                           label = family_title, hjust = 0, size = ts, family = family,
                                           fontface = "bold")
  }

  # numeric columns: parts placed glyph by glyph and right-aligned.
  plot_rows <- merge(rows_layout, data, by.x = "key", by.y = "outcome")
  # status follows the interval that is drawn.
  plot_rows$excludes <- plot_rows$plot_lower > null | plot_rows$plot_upper < null
  numeric_column <- function(which, titles, k = contrasts[1L]) {
    d <- plot_rows[plot_rows$contrast == k, , drop = FALSE]
    if (which == "evalue") {
      parts <- list(.margot_effects_num(d$evalue, 2), .margot_effects_num(d$evalue_bound, 2))
    } else if (which == "means") {
      parts <- list(.margot_effects_num(d$mean_reference, 2), rep("\u2192", nrow(d)),
                    .margot_effects_num(d$mean_intervention, 2))
    } else {
      cols <- if (which == "raw") c("raw_estimate", "raw_lower", "raw_upper") else c("estimate", "lower", "upper")
      # one precision per column type across all contrasts.
      dg <- digits %||% .margot_effects_digits(plot_rows[[cols[2L]]], plot_rows[[cols[3L]]], null)
      parts <- list(.margot_effects_num(d[[cols[1L]]], dg),
                    paste0("[", .margot_effects_num(d[[cols[2L]]], dg), ","),
                    paste0(.margot_effects_num(d[[cols[3L]]], dg), "]"))
    }
    # a short gap inside "estimate [lower, upper]" and a wider one between separate
    # quantities; a subtitle "a|b" gives one sub-heading per number.
    widths <- vapply(parts, function(s) max(vapply(s, function(x) sum(text_mm(strsplit(x, "")[[1L]])),
                                                   numeric(1))), numeric(1))
    subs <- if (length(titles) > 1L) strsplit(titles[2L], "|", fixed = TRUE)[[1L]] else character()
    per_part <- length(subs) == length(parts) && length(parts) > 1L
    gap <- rep(text_mm("0") * if (which == "evalue") 1.6 else 0.6, length(parts) - 1L)
    if (per_part) {
      sub_w <- text_mm(subs)
      gap <- pmax(gap, sub_w[-1L] - widths[-1L] + text_mm("00"))
      widths <- pmax(widths, sub_w)
    }
    right <- cumsum(widths + c(0, gap))
    body_w <- max(right)
    title_w <- max(text_mm(titles[1L], bold = TRUE),
                   if (!per_part && length(titles) > 1L) text_mm(titles[2L]) else 0)
    pad <- 3
    inner <- max(body_w, title_w)
    shift <- pad + (inner - body_w) / 2
    glyphs <- do.call(rbind, lapply(seq_along(parts), function(j)
      .margot_effects_glyphs(parts[[j]], shift + right[j], d$y, size_pt, family)))
    p <- ggplot2::ggplot() + stripes() +
      ggplot2::geom_text(data = glyphs, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                         hjust = 0, size = ts, family = family, colour = "black")
    if (per_part) {
      p <- p + header_text(pad + inner / 2, titles[1L]) +
        ggplot2::annotate("text", x = shift + right, y = header_y - 0.45, label = subs, hjust = 1,
                          size = ts, family = family)
    } else {
      p <- p + header_text(pad + inner / 2, titles)
    }
    total <- pad + inner + 1
    p <- p + ggplot2::scale_x_continuous(limits = c(0, total), expand = c(0, 0)) + y_scale() + base_theme
    attr(p, "width_mm") <- total
    p
  }
  if (length(contrasts) == 1L) {
    p_columns <- Map(numeric_column, columns, column_titles)
  } else {
    # one column set per contrast: the contrast title above, the column title and unit below.
    grid <- expand.grid(i = seq_along(columns), k = contrasts, stringsAsFactors = FALSE)
    p_columns <- Map(function(i, k) numeric_column(columns[i],
      c(contrast_title(k), gsub("|", ", ", paste(column_titles[[i]], collapse = " "), fixed = TRUE)), k),
      grid$i, grid$k)
  }
  column_mm <- vapply(p_columns, function(p) attr(p, "width_mm"), numeric(1))

  # interval panels, one per contrast, sharing one x range.
  gap_mm <- 5
  content_mm <- width_mm - 2 * margin_mm
  forest_mm <- (content_mm - label_mm - sum(column_mm) - gap_mm * (length(contrasts) - 1L)) / length(contrasts)
  if (forest_mm <= 10) {
    stop("no room for the interval panel at width_mm = ", width_mm, "; widen the figure or drop a numeric column.",
         call. = FALSE)
  }
  if (forest_mm < 35) {
    warning("interval panel is only ", round(forest_mm), " mm wide; widen the figure or drop a numeric column.",
            call. = FALSE)
  }
  g <- if (type == "ratio") log10 else identity
  gi <- if (type == "ratio") function(v) 10^v else identity
  data_range <- range(c(null, data$plot_lower, data$plot_upper))
  breaks <- .margot_effects_breaks(data_range, type, panel_mm = forest_mm, size_pt = size_pt, family = family)
  if (length(breaks) < 2L || anyNA(breaks)) stop("no usable axis breaks for this figure.", call. = FALSE)
  span <- g(range(breaks))
  pad <- 2.5 * diff(span) / (forest_mm - 5)
  limits <- gi(span + c(-pad, pad))
  # never draw a figure that would drop part of an interval.
  if (limits[1L] > min(data$plot_lower) || limits[2L] < max(data$plot_upper)) {
    stop("axis limits do not contain every interval.", call. = FALSE)
  }
  to_data <- function(x0, mm) gi(g(x0) + mm * diff(g(limits)) / forest_mm)
  point_size <- size_pt * 0.26
  key_w <- if (is.null(key_labels)) 0 else text_mm(key_labels)
  key_in_header <- !is.null(key_labels) && length(contrasts) == 1L && 1.5 + 3.2 + sum(key_w) + 4 <= forest_mm

  panel <- function(k) {
    d <- plot_rows[plot_rows$contrast == k, , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$y)) + stripes(xr = limits) +
      ggplot2::annotate("segment", x = null, xend = null, y = row_bottom, yend = row_top,
                        colour = "grey45", linewidth = .margot_effects_lw(0.5)) +
      ggplot2::geom_segment(ggplot2::aes(x = .data$plot_lower, xend = .data$plot_upper, yend = .data$y),
                            linewidth = .margot_effects_lw(0.8), colour = "black", lineend = "butt") +
      ggplot2::geom_point(ggplot2::aes(x = .data$plot_estimate, fill = .data$excludes), shape = 21,
                          size = point_size, stroke = .margot_effects_lw(0.8), colour = "black") +
      ggplot2::scale_fill_manual(values = c(`TRUE` = "black", `FALSE` = "white"), guide = "none") +
      ggplot2::annotate("segment", x = min(breaks), xend = max(breaks), y = ylim[1L], yend = ylim[1L],
                        linewidth = .margot_effects_lw(0.5), colour = "black", lineend = "square") +
      y_scale() + ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_classic(base_family = family, base_size = size_pt) +
      ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_line(linewidth = .margot_effects_lw(0.5), colour = "black"),
                     axis.ticks.length.x = ggplot2::unit(0.9, "mm"),
                     axis.text.x = ggplot2::element_text(size = size_pt, colour = "black",
                                                         margin = ggplot2::margin(t = 0.6, unit = "mm")),
                     plot.margin = ggplot2::margin(0, 0, 0, 0), panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank())
    labeller <- function(v) .margot_effects_axis_labels(v, type)
    p <- if (type == "ratio") {
      p + ggplot2::scale_x_log10(limits = limits, breaks = breaks, labels = labeller, expand = c(0, 0))
    } else {
      p + ggplot2::scale_x_continuous(limits = limits, breaks = breaks, labels = labeller, expand = c(0, 0))
    }
    if (length(contrasts) > 1L) p <- p + header_text(gi(mean(g(limits))), contrast_title(k))
    if (key_in_header && k == contrasts[1L]) {
      x_point <- c(to_data(limits[1L], 1.5), to_data(limits[1L], 1.5 + 1.6 + key_w[1L] + 4))
      key_y <- if (two_line) header_y - 0.45 else header_y
      p <- p + ggplot2::annotate("point", x = x_point, y = key_y, shape = 21, size = point_size,
                                 stroke = .margot_effects_lw(0.8), colour = "black", fill = c("black", "white")) +
        ggplot2::annotate("text", x = vapply(x_point, to_data, numeric(1), mm = 1.6), y = key_y,
                          label = key_labels, hjust = 0, size = ts, family = family, colour = "black")
    }
    p
  }
  p_panels <- lapply(contrasts, panel)
  if (length(p_panels) > 1L) {
    p_gap <- ggplot2::ggplot() + stripes() + ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      y_scale() + base_theme
    spaced <- list(p_panels[[1L]])
    for (j in seq_along(p_panels)[-1L]) spaced <- c(spaced, list(p_gap, p_panels[[j]]))
    panel_widths <- c(forest_mm, rep(c(gap_mm, forest_mm), length(p_panels) - 1L))
  } else {
    spaced <- p_panels
    panel_widths <- forest_mm
  }
  plots <- c(list(p_label), spaced, unname(p_columns))
  main <- patchwork::wrap_plots(plots, nrow = 1, widths = ggplot2::unit(c(label_mm, panel_widths, column_mm), "mm"))

  # footer row: one axis title centred under the interval panels and, when it did not
  # fit in the header, the key under the label column.
  title_lines <- strsplit(xlab, "\n")[[1L]]
  panels_mid <- label_mm + sum(panel_widths) / 2
  key_lines <- 0
  if (!is.null(key_labels) && !key_in_header) {
    key_lines <- if (1.2 + 3.2 + sum(key_w) + 4 <= label_mm) 1 else 2
  }
  title_mm <- size_pt * mm_pt * 1.05
  # the title's text box carries about 1 mm of ascender space above the capitals, so it
  # starts 1 mm above the footer's top edge (clipping off).
  lift <- 1
  foot_mm <- max(length(title_lines) * title_mm, key_lines * line_mm) + 0.6 - lift
  p_foot <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = panels_mid, y = foot_mm + lift - 0.1, label = xlab, vjust = 1,
                      size = ts, family = family, lineheight = 1.05) +
    ggplot2::scale_x_continuous(limits = c(0, content_mm), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, foot_mm), clip = "off") + base_theme
  if (key_lines > 0) {
    key_x <- if (key_lines == 1) c(1.2, 1.2 + 1.6 + key_w[1L] + 4) else c(1.2, 1.2)
    key_y <- foot_mm + lift - 0.1 - line_mm * (if (key_lines == 1) c(0.5, 0.5) else c(0.5, 1.5))
    p_foot <- p_foot +
      ggplot2::annotate("point", x = key_x, y = key_y, shape = 21, size = point_size,
                        stroke = .margot_effects_lw(0.8), colour = "black", fill = c("black", "white")) +
      ggplot2::annotate("text", x = key_x + 1.6, y = key_y, label = key_labels, hjust = 0, size = ts,
                        family = family, colour = "black")
  }
  axis_mm <- 0.9 + 0.6 + size_pt * mm_pt * 1.0
  main_mm <- diff(ylim) * row_mm + axis_mm
  out <- patchwork::wrap_plots(list(patchwork::wrap_elements(full = main), p_foot), ncol = 1,
                               heights = ggplot2::unit(c(main_mm, foot_mm), "mm")) &
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(0, 0, 0, 0))
  attr(out, "size_mm") <- c(width = width_mm, height = main_mm + foot_mm + 2 * margin_mm)
  attr(out, "margin_mm") <- margin_mm
  attr(out, "layout_mm") <- c(label = label_mm, panel = forest_mm, columns = unname(column_mm))
  label_gap <- .margot_effects_label_gap(breaks, type, forest_mm, size_pt, family)
  if (length(contrasts) > 1L) {
    # the last label of one panel and the first of the next sit across the gutter.
    w <- text_mm(.margot_effects_axis_labels(range(breaks), type))
    cross <- 2.5 + gap_mm + 2.5 - sum(w) / 2
    if (cross < 2.5) {
      warning(sprintf("tick labels across adjacent panels are %.1f mm apart; widen the figure.", cross),
              call. = FALSE)
    }
    label_gap <- min(label_gap, cross)
  }
  attr(out, "axis") <- list(breaks = breaks, data_range = data_range, limits = limits, label_gap_mm = label_gap)
  attr(out, "font") <- .margot_effects_font_record(family)
  plotted <- plot_rows[base::order(-plot_rows$y, match(plot_rows$contrast, contrasts)),
                       c("key", "contrast", "plot_estimate", "plot_lower", "plot_upper", "excludes")]
  names(plotted) <- c("outcome", "contrast", "estimate", "lower", "upper", "excludes_null")
  rownames(plotted) <- NULL
  attr(out, "plotted") <- plotted
  out
}

#' Save an effects figure at print size
#'
#' Writes a figure from [margot_plot_effects()] at its exact print size: a vector PDF with embedded fonts (through `grDevices::cairo_pdf()`) and a PNG (through `ragg::agg_png()`). The white margin lies inside the stated size.
#'
#' @param plot Figure from [margot_plot_effects()].
#' @param file Output path without extension; `.pdf` and `.png` are added.
#' @param formats Any of `"pdf"` and `"png"`.
#' @param dpi PNG resolution. Default 600.
#'
#' @return The written paths, invisibly.
#'
#' @seealso [margot_plot_effects()]
#' @md
#'
#' @examples
#' \donttest{
#' effects <- data.frame(outcome = c("a", "b"), estimate = c(0.1, -0.02),
#'   lower = c(0.04, -0.07), upper = c(0.16, 0.03), conf_level = 0.95)
#' fig <- margot_plot_effects(effects, xlab = "Difference, SD units", width_mm = 89)
#' if (capabilities("cairo")) {
#'   margot_save_effects_plot(fig, file.path(tempdir(), "effects"), formats = "pdf")
#' }
#' }
#'
#' @export
margot_save_effects_plot <- function(plot, file, formats = c("pdf", "png"), dpi = 600) {
  formats <- unique(match.arg(formats, several.ok = TRUE))
  size_mm <- attr(plot, "size_mm")
  if (is.null(size_mm)) stop("`plot` must come from margot_plot_effects().", call. = FALSE)
  margin_mm <- attr(plot, "margin_mm") %||% 0
  family <- unname(attr(plot, "font")[["family"]])
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  framed <- patchwork::wrap_elements(full = plot) +
    ggplot2::theme(plot.margin = ggplot2::margin(margin_mm, margin_mm, margin_mm, margin_mm, "mm"),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  w <- size_mm[[1L]] / 25.4
  h <- size_mm[[2L]] / 25.4
  paths <- character()
  if ("pdf" %in% formats) {
    path <- paste0(file, ".pdf")
    grDevices::cairo_pdf(path, width = w, height = h, family = family)
    tryCatch(print(framed), finally = grDevices::dev.off())
    paths <- c(paths, path)
  }
  if ("png" %in% formats) {
    if (!requireNamespace("ragg", quietly = TRUE)) stop("PNG output needs the ragg package.", call. = FALSE)
    path <- paste0(file, ".png")
    ragg::agg_png(path, width = w, height = h, units = "in", res = dpi, background = "white")
    tryCatch(print(framed), finally = grDevices::dev.off())
    paths <- c(paths, path)
  }
  invisible(paths)
}
