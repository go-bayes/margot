#' Arrange LMTP overlap ratio_plots into a grid (waves x shifts)
#'
#' Given the `ratio_plots` returned from `margot_lmtp_overlap()`, build a
#' patchwork grid organized by waves (rows) and shifts (columns) for a single
#' outcome. Accepts either the full result list (with `$ratio_plots`) or the
#' plot list itself. Shift names may be provided as full names or cleaned
#' suffixes; missing cells are filled with blank panels.
#'
#' @param x A list as returned by `margot_lmtp_overlap()` or a named list of
#'   ggplot objects keyed as `"outcome::shift::wave"`.
#' @param outcome Optional character. If not supplied, inferred when only a
#'   single outcome is present in `x`. Required when multiple outcomes exist.
#' @param shifts Optional character vector of shifts to include; accepts full
#'   names (e.g., `t5_pwi_z_shift_up`) or cleaned (e.g., `shift_up`). If NULL,
#'   includes all available shifts for the outcome.
#' @param waves Optional integer vector specifying waves (e.g., `1:5`). If NULL,
#'   includes all waves found for the outcome.
#' @param ncol Optional integer for number of columns. Defaults to number of
#'   selected shifts.
#' @param drop_titles Logical; if TRUE (default), strip titles from individual
#'   plots and add a single overall title.
#' @param title Optional overall title; if NULL, a default is constructed.
#' @param label_mapping Optional named list to prettify column (shift) labels
#'   via `transform_label()` when available.
#' @param annotate_zeros Logical; if TRUE, adds "zeros: X%" label to top-right of each panel.
#'   Default is FALSE.
#' @param annotate_graph Character; controls graph annotations: `"waves"` places wave labels at top,
#'   `"shifts"` places shift labels at top, `"none"` disables annotations (default: `"none"`).
#' @param ymax_harmonize Character or named vector; controls y-axis harmonization: `"none"` (default) gives
#'   each plot independent y-scale, `"row"` harmonizes within rows, `"column"` harmonizes within columns,
#'   `"global"` harmonizes all plots. Can also be a named vector with custom values.
#' @param xlim_harmonize Character or named vector; controls x-axis harmonization: `"none"` (default) gives
#'   each plot independent x-scale, `"row"` harmonizes within rows, `"column"` harmonizes within columns,
#'   `"global"` harmonizes all plots. Can also be a named vector with custom values.
#' @param text_size Numeric size for facet annotations (wave/shift/zeros labels).
#'
#' @return A patchwork object combining the selected plots.
#' @export
#' @importFrom patchwork wrap_plots plot_annotation
margot_lmtp_overlap_plot_grid <- function(x,
                                          outcome = NULL,
                                          shifts = NULL,
                                          waves = NULL,
                                          ncol = NULL,
                                          drop_titles = TRUE,
                                          title = NULL,
                                          label_mapping = NULL,
                                          annotate_zeros = FALSE,
                                          ymax = NULL,
                                          annotate_graph = c("none", "waves", "shifts"),
                                          xlim = NULL,
                                          layout = c("waves_by_shifts","shifts_by_waves"),
                                          ymax_harmonize = "none",
                                          xlim_harmonize = "none",
                                          headroom = 0.12,
                                          text_size = text_size) {

  stopifnot(is.logical(annotate_zeros), length(annotate_zeros) == 1L)
  annotate_graph <- match.arg(annotate_graph)
  layout         <- match.arg(layout)

  # validate ymax_harmonize: either a character option or a named/unnamed vector
  harmonize_y_mode <- "none"  # default
  harmonize_y_custom <- NULL
  if (is.character(ymax_harmonize) && length(ymax_harmonize) == 1L) {
    harmonize_y_mode <- match.arg(ymax_harmonize, c("none", "row", "column", "global"))
  } else if (is.numeric(ymax_harmonize) || (is.character(ymax_harmonize) && length(ymax_harmonize) > 1L)) {
    harmonize_y_mode <- "custom"
    harmonize_y_custom <- ymax_harmonize
  } else {
    stop("`ymax_harmonize` must be one of 'none', 'row', 'column', 'global', or a named vector.")
  }

  # validate xlim_harmonize: either a character option or a named/unnamed vector
  harmonize_x_mode <- "none"  # default
  harmonize_x_custom <- NULL
  if (is.character(xlim_harmonize) && length(xlim_harmonize) == 1L) {
    harmonize_x_mode <- match.arg(xlim_harmonize, c("none", "row", "column", "global"))
  } else if (is.numeric(xlim_harmonize) || (is.character(xlim_harmonize) && length(xlim_harmonize) > 1L)) {
    harmonize_x_mode <- "custom"
    harmonize_x_custom <- xlim_harmonize
  } else {
    stop("`xlim_harmonize` must be one of 'none', 'row', 'column', 'global', or a named vector.")
  }

  # Extract ratio_plots list
  ratio_plots <- NULL
  if (is.list(x) && !is.null(x$ratio_plots) && is.list(x$ratio_plots)) {
    ratio_plots <- x$ratio_plots
  } else if (is.list(x) && all(vapply(x, inherits, logical(1), what = "gg"))) {
    ratio_plots <- x
  } else {
    stop("`x` must be a result from margot_lmtp_overlap() or a named list of ggplots.")
  }

  if (!length(ratio_plots)) stop("No plots found in `ratio_plots`.")

  # Parse keys: outcome::shift::wave
  parse_key <- function(k) {
    parts <- strsplit(k, "::", fixed = TRUE)[[1]]
    data.frame(
      key = k,
      outcome = if (length(parts) >= 1) parts[[1]] else NA_character_,
      shift   = if (length(parts) >= 2) parts[[2]] else NA_character_,
      wave    = if (length(parts) >= 3) parts[[3]] else NA_character_,
      stringsAsFactors = FALSE
    )
  }
  info <- do.call(rbind, lapply(names(ratio_plots), parse_key))

  # Infer/validate outcome
  outcomes_avail <- unique(info$outcome)
  if (is.null(outcome)) {
    if (length(outcomes_avail) != 1L) {
      stop("Multiple outcomes present; please specify `outcome`.")
    }
    outcome <- outcomes_avail[[1]]
  }

  info <- info[info$outcome == outcome, , drop = FALSE]
  if (!nrow(info)) stop("No plots for specified outcome: ", outcome)

  # Clean shift suffix for matching convenience
  clean_shift <- function(sh, outc) {
    if (startsWith(sh, paste0(outc, "_"))) substring(sh, nchar(outc) + 2L) else sh
  }
  info$shift_clean <- vapply(info$shift, clean_shift, character(1), outc = outcome)

  # filter by waves
  info$wave_num <- suppressWarnings(as.integer(info$wave))
  if (!is.null(waves)) info <- info[info$wave_num %in% waves, , drop = FALSE]
  if (!nrow(info)) stop("No plots after applying wave filter.")

  # Select shifts: allow full or cleaned name matching
  if (is.null(shifts)) {
    shifts_full  <- unique(info$shift)
    shifts_clean <- unique(info$shift_clean)
  } else {
    keep <- (info$shift %in% shifts) | (info$shift_clean %in% shifts)
    info <- info[keep, , drop = FALSE]
    if (!nrow(info)) stop("No plots after applying shift filter.")
    shifts_full  <- unique(info$shift)
    shifts_clean <- unique(info$shift_clean)
  }

  # Order shifts: null, shift_down, shift_up first if present, then others alpha
  pref <- c("null", "shift_down", "shift_up")
  ord_names <- c(intersect(pref, shifts_clean), setdiff(sort(shifts_clean), pref))
  # map cleaned order to a representative full name (first occurrence)
  map_full <- function(sc) info$shift[match(sc, info$shift_clean)]
  shifts_order_clean <- ord_names
  shifts_order_full  <- vapply(shifts_order_clean, map_full, character(1))

  # Build pretty labels for columns using transform_label when available
  map_label <- function(lbl) {
    if (exists("transform_label", mode = "function")) {
      out <- tryCatch(
        transform_label(
          label = lbl,
          label_mapping = label_mapping,
          options = list(
            remove_tx_prefix = TRUE,
            remove_z_suffix = TRUE,
            remove_underscores = TRUE,
            use_title_case = TRUE
          )
        ),
        error = function(e) lbl
      )
      if (is.null(out) || is.na(out)) lbl else out
    } else {
      gsub("_", " ", tools::toTitleCase(lbl))
    }
  }
  col_labels <- vapply(shifts_order_clean, map_label, character(1))

  waves_order <- sort(unique(info$wave_num))
  if (is.null(ncol)) ncol <- if (identical(layout, "waves_by_shifts")) length(shifts_order_full) else length(waves_order)

  # compute y-axis and x-axis maxima: global, per-wave, and per-shift
  safe_max_count <- function(p) {
    if (is.null(p)) return(NA_real_)
    gb <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    if (is.null(gb) || !length(gb$data)) return(NA_real_)
    d <- gb$data[[1]]
    ycols <- intersect(c("y", "count", "..count.."), names(d))
    if (!length(ycols)) return(NA_real_)
    max(unlist(d[ycols]), na.rm = TRUE)
  }
  safe_max_x <- function(p) {
    if (is.null(p)) return(NA_real_)
    gb <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    if (is.null(gb) || !length(gb$data)) return(NA_real_)
    d <- gb$data[[1]]
    xcols <- intersect(c("x", "xmin", "xmax"), names(d))
    if (!length(xcols)) return(NA_real_)
    max(unlist(d[xcols]), na.rm = TRUE)
  }
  selected_keys <- paste(info$outcome, info$shift, as.character(info$wave_num), sep = "::")
  max_y_global <- 0
  max_y_by_wave <- setNames(rep(0, length(unique(info$wave_num))), sort(unique(info$wave_num)))
  max_y_by_shift <- setNames(rep(0, length(shifts_order_full)), shifts_order_full)
  max_x_global <- 0
  max_x_by_wave <- setNames(rep(0, length(unique(info$wave_num))), sort(unique(info$wave_num)))
  max_x_by_shift <- setNames(rep(0, length(shifts_order_full)), shifts_order_full)

  for (k in selected_keys) {
    count_val <- safe_max_count(ratio_plots[[k]])
    x_val <- safe_max_x(ratio_plots[[k]])
    max_y_global <- max(max_y_global, count_val, na.rm = TRUE)
    max_x_global <- max(max_x_global, x_val, na.rm = TRUE)

    # parse key components: outcome::shift::wave
    parts <- strsplit(k, "::", fixed = TRUE)[[1]]
    shift_k <- if (length(parts) >= 2) parts[[2]] else NA_character_
    wv <- suppressWarnings(as.integer(parts[3]))

    # update per-wave max
    if (!is.na(wv)) {
      max_y_by_wave[as.character(wv)] <- max(max_y_by_wave[as.character(wv)], count_val, na.rm = TRUE)
      max_x_by_wave[as.character(wv)] <- max(max_x_by_wave[as.character(wv)], x_val, na.rm = TRUE)
    }

    # update per-shift max
    if (!is.na(shift_k) && shift_k %in% names(max_y_by_shift)) {
      max_y_by_shift[[shift_k]] <- max(max_y_by_shift[[shift_k]], count_val, na.rm = TRUE)
      max_x_by_shift[[shift_k]] <- max(max_x_by_shift[[shift_k]], x_val, na.rm = TRUE)
    }
  }

  # override global max if ymax specified
  if (!is.null(ymax) && is.finite(ymax) && ymax > 0) {
    max_y_global <- ymax
  }
  if (!is.finite(max_y_global) || max_y_global <= 0) max_y_global <- NA_real_

  # handle xlim: if xlim is a 2-element vector, it's a fixed range (backward compat)
  # otherwise xlim_harmonize controls behavior
  xlim_fixed <- NULL
  if (!is.null(xlim) && length(xlim) == 2 && is.finite(xlim[1]) && is.finite(xlim[2])) {
    xlim_fixed <- xlim
  }
  if (!is.finite(max_x_global) || max_x_global <= 0) max_x_global <- NA_real_

  # Helper to extract zeros% from original plot title
  parse_zeros <- function(p) {
    ttl <- tryCatch(p$labels$title, error = function(e) NULL)
    if (is.null(ttl)) return(NA_character_)
    m <- regexec("zeros:\\s*([0-9.]+)%", ttl)
    r <- regmatches(ttl, m)[[1]]
    if (length(r) >= 2) r[2] else NA_character_
  }

  if (length(unique(info$wave_num)) == 1L && identical(layout, "shifts_by_waves")) {
    layout <- "waves_by_shifts"
  }

  # column labels depend on layout
  if (identical(layout, "waves_by_shifts")) {
    # columns are shifts
    col_lab_vals <- col_labels
    col_keys <- shifts_order_full
    top_key_context <- list(type = "wave", value = waves_order[1])
  } else {
    # columns are waves
    col_lab_vals <- paste0("Wave ", waves_order)
    col_keys <- as.character(waves_order)
    top_key_context <- list(type = "shift", value = shifts_order_full[1])
  }

  # build grid: row-major by waves x shifts
  get_plot <- function(outc, sh_full, w_num, is_top, col_idx, row_idx) {
    k <- paste(outc, sh_full, as.character(w_num), sep = "::")
    p <- ratio_plots[[k]]
    zeros_str <- parse_zeros(ratio_plots[[k]])
    if (is.null(p)) {
      # fill empty
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    if (isTRUE(drop_titles)) {
      if (isTRUE(is_top)) {
        p <- p + ggplot2::labs(title = col_lab_vals[[col_idx]])
      } else {
        p <- p + ggplot2::labs(title = NULL)
      }
    }

    # determine y-axis limit based on harmonize_y_mode and layout
    y_top <- NA_real_

    if (harmonize_y_mode == "custom") {
      # custom values: check for named match on wave or shift
      if (!is.null(names(harmonize_y_custom))) {
        # try wave key
        wave_key <- paste0("wave_", w_num)
        if (wave_key %in% names(harmonize_y_custom)) {
          y_top <- harmonize_y_custom[[wave_key]]
        } else if (as.character(w_num) %in% names(harmonize_y_custom)) {
          y_top <- harmonize_y_custom[[as.character(w_num)]]
        } else if (sh_full %in% names(harmonize_y_custom)) {
          y_top <- harmonize_y_custom[[sh_full]]
        }
      } else {
        # unnamed vector: index by row
        if (row_idx <= length(harmonize_y_custom)) {
          y_top <- harmonize_y_custom[[row_idx]]
        }
      }
    } else if (harmonize_y_mode == "global") {
      y_top <- max_y_global
    } else if (harmonize_y_mode == "row") {
      # row harmonization depends on layout
      if (identical(layout, "waves_by_shifts")) {
        # rows are waves
        y_top <- max_y_by_wave[as.character(w_num)]
      } else {
        # rows are shifts
        y_top <- max_y_by_shift[[sh_full]]
      }
    } else if (harmonize_y_mode == "column") {
      # column harmonization depends on layout
      if (identical(layout, "waves_by_shifts")) {
        # columns are shifts
        y_top <- max_y_by_shift[[sh_full]]
      } else {
        # columns are waves
        y_top <- max_y_by_wave[as.character(w_num)]
      }
    }
    # else harmonize_y_mode == "none": y_top stays NA, each plot independent

    # apply y-axis limit if determined
    if (is.finite(y_top) && !is.na(y_top) && y_top > 0) {
      y_top_adj <- y_top * (1 + max(0, headroom))
      p <- p + ggplot2::scale_y_continuous(limits = c(0, y_top_adj))
    }

    # determine x-axis limit based on xlim_fixed (backward compat) or harmonize_x_mode
    x_max <- NA_real_

    if (!is.null(xlim_fixed)) {
      # backward compatibility: fixed xlim overrides harmonization
      p <- p + ggplot2::scale_x_continuous(limits = xlim_fixed)
    } else {
      # determine x-max based on harmonize_x_mode
      if (harmonize_x_mode == "custom") {
        # custom values: check for named match on wave or shift
        if (!is.null(names(harmonize_x_custom))) {
          wave_key <- paste0("wave_", w_num)
          if (wave_key %in% names(harmonize_x_custom)) {
            x_max <- harmonize_x_custom[[wave_key]]
          } else if (as.character(w_num) %in% names(harmonize_x_custom)) {
            x_max <- harmonize_x_custom[[as.character(w_num)]]
          } else if (sh_full %in% names(harmonize_x_custom)) {
            x_max <- harmonize_x_custom[[sh_full]]
          }
        } else {
          # unnamed vector: index by row
          if (row_idx <= length(harmonize_x_custom)) {
            x_max <- harmonize_x_custom[[row_idx]]
          }
        }
      } else if (harmonize_x_mode == "global") {
        x_max <- max_x_global
      } else if (harmonize_x_mode == "row") {
        # row harmonization depends on layout
        if (identical(layout, "waves_by_shifts")) {
          # rows are waves
          x_max <- max_x_by_wave[as.character(w_num)]
        } else {
          # rows are shifts
          x_max <- max_x_by_shift[[sh_full]]
        }
      } else if (harmonize_x_mode == "column") {
        # column harmonization depends on layout
        if (identical(layout, "waves_by_shifts")) {
          # columns are shifts
          x_max <- max_x_by_shift[[sh_full]]
        } else {
          # columns are waves
          x_max <- max_x_by_wave[as.character(w_num)]
        }
      }
      # else harmonize_x_mode == "none": x_max stays NA, each plot independent

      # apply x-axis limit if determined
      if (is.finite(x_max) && !is.na(x_max) && x_max > 0) {
        p <- p + ggplot2::scale_x_continuous(limits = c(NA, x_max))
      }
    }

    # apply annotations based on annotate_graph parameter
    if (annotate_graph == "waves") {
      # place wave label at top of graph
      p <- p + ggplot2::annotate("text", x = -Inf, y = Inf,
                                 label = paste0("Wave ", w_num),
                                 hjust = -0.1, vjust = 1.2, size = text_size)
    } else if (annotate_graph == "shifts") {
      # place shift label at top of graph (ensure sufficient space with adjusted vjust)
      sh_clean <- if (startsWith(sh_full, paste0(outc, "_"))) substring(sh_full, nchar(outc) + 2L) else sh_full
      sh_pretty <- map_label(sh_clean)
      p <- p + ggplot2::annotate("text", x = -Inf, y = Inf,
                                 label = sh_pretty,
                                 hjust = -0.1, vjust = 1.2, size = text_size)
    }
    if (isTRUE(annotate_zeros)) {
      if (!is.na(zeros_str)) {
        p <- p + ggplot2::annotate("text", x = Inf, y = Inf,
                                   label = paste0("zeros: ", zeros_str, "%"),
                                   hjust = 1.1, vjust = 1.1, size = text_size)
      }
    }
    p
  }

  plots <- list()
  if (identical(layout, "waves_by_shifts")) {
    # rows = waves, columns = shifts
    for (w_idx in seq_along(waves_order)) {
      w <- waves_order[[w_idx]]
      for (sc_idx in seq_along(shifts_order_full)) {
        is_top <- (w_idx == 1L)
        col_idx <- sc_idx
        row_idx <- w_idx
        plots[[length(plots) + 1]] <- get_plot(outcome, shifts_order_full[[sc_idx]], w, is_top, col_idx, row_idx)
      }
    }
  } else {
    # rows = shifts, columns = waves
    for (sc_idx in seq_along(shifts_order_full)) {
      sh <- shifts_order_full[[sc_idx]]
      for (w_idx in seq_along(waves_order)) {
        is_top <- (sc_idx == 1L)
        col_idx <- w_idx
        row_idx <- sc_idx
        plots[[length(plots) + 1]] <- get_plot(outcome, sh, waves_order[[w_idx]], is_top, col_idx, row_idx)
      }
    }
  }

  main_title <- if (!is.null(title)) title else paste0("Density ratios — ", outcome)
  patchwork::wrap_plots(plots, ncol = ncol) + patchwork::plot_annotation(title = main_title)
}
