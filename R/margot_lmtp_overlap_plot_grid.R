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
#' @param annotate_zeros Character; one of "column" (default), "facet", "none".
#'   "column" appends the zeros for the top-row wave to the column title;
#'   "facet" prints zeros in the top-right of each panel; "none" disables.
#'
#' @return A patchwork object combining the selected plots.
#' @export
#' @importFrom patchwork wrap_plots plot_annotation
margot_lmtp_overlap_plot_grid <- function(x,
                                          outcome = NULL,
                                          shifts = NULL,
                                          waves = NULL,
                                          remove_waves = NULL,
                                          ncol = NULL,
                                          drop_titles = TRUE,
                                          title = NULL,
                                          label_mapping = NULL,
                                          annotate_zeros = FALSE,
                                          ymax = NULL,
                                          annotate_wave = TRUE,
                                          annotate_shift = TRUE,
                                          xlim = NULL,
                                          layout = c("waves_by_shifts","shifts_by_waves"),
                                          ymax_by_wave = NULL,
                                          headroom = 0.06) {

  stopifnot(is.logical(annotate_zeros), length(annotate_zeros) == 1L)
  stopifnot(is.logical(annotate_wave),  length(annotate_wave)  == 1L)
  stopifnot(is.logical(annotate_shift), length(annotate_shift) == 1L)
  layout         <- match.arg(layout)

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

  # Filter by waves
  info$wave_num <- suppressWarnings(as.integer(info$wave))
  if (!is.null(waves)) info <- info[info$wave_num %in% waves, , drop = FALSE]
  if (!is.null(remove_waves)) info <- info[!(info$wave_num %in% remove_waves), , drop = FALSE]
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

  # Compute Y-axis maxima: global and per-wave; allow explicit per-wave override via `ymax_by_wave`.
  safe_max_count <- function(p) {
    if (is.null(p)) return(NA_real_)
    gb <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    if (is.null(gb) || !length(gb$data)) return(NA_real_)
    d <- gb$data[[1]]
    ycols <- intersect(c("y", "count", "..count.."), names(d))
    if (!length(ycols)) return(NA_real_)
    max(unlist(d[ycols]), na.rm = TRUE)
  }
  selected_keys <- paste(info$outcome, info$shift, as.character(info$wave_num), sep = "::")
  max_y <- 0
  max_y_by_wave <- setNames(rep(0, length(unique(info$wave_num))), sort(unique(info$wave_num)))
  for (k in selected_keys) {
    max_y <- max(max_y, safe_max_count(ratio_plots[[k]]), na.rm = TRUE)
    # parse wave number from key (3rd part)
    parts <- strsplit(k, "::", fixed = TRUE)[[1]]
    wv <- suppressWarnings(as.integer(parts[3]))
    if (!is.na(wv)) max_y_by_wave[as.character(wv)] <- max(max_y_by_wave[as.character(wv)], safe_max_count(ratio_plots[[k]]), na.rm = TRUE)
  }
  if (!is.null(ymax) && is.finite(ymax) && ymax > 0) {
    max_y <- ymax
  }
  if (!is.finite(max_y) || max_y <= 0) max_y <- NA_real_

  # Helper to extract zeros% from original plot title
  parse_zeros <- function(p) {
    ttl <- tryCatch(p$labels$title, error = function(e) NULL)
    if (is.null(ttl)) return(NA_character_)
    m <- regexec("zeros:\\s*([0-9.]+)%", ttl)
    r <- regmatches(ttl, m)[[1]]
    if (length(r) >= 2) r[2] else NA_character_
  }

  # Column labels depend on layout
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

  # Optionally fix x-axis limits across all selected plots
  if (!is.null(xlim) && length(xlim) == 2 && is.finite(xlim[1]) && is.finite(xlim[2])) {
    for (k in selected_keys) {
      if (!is.null(ratio_plots[[k]])) {
        ratio_plots[[k]] <- ratio_plots[[k]] + ggplot2::scale_x_continuous(limits = xlim)
      }
    }
  }

  # Build grid: row-major by waves x shifts
  get_plot <- function(outc, sh_full, w_num, is_top, col_idx) {
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
    # Harmonize Y-axis limits across panels, when available
    # Apply Y-axis limit (override > per-wave auto > global)
    y_top <- NA_real_
    # explicit override
    if (!is.null(ymax_by_wave)) {
      if (!is.null(names(ymax_by_wave)) && as.character(w_num) %in% names(ymax_by_wave)) {
        y_top <- ymax_by_wave[[as.character(w_num)]]
      } else if (length(ymax_by_wave) == length(waves_order)) {
        idx <- match(w_num, waves_order)
        if (!is.na(idx)) y_top <- ymax_by_wave[[idx]]
      }
    }
    # if no explicit, use per-wave auto
    if (!is.finite(y_top) || is.na(y_top)) {
      if (is.finite(max_y_by_wave[as.character(w_num)])) y_top <- max_y_by_wave[as.character(w_num)]
    }
    # fall back to global
    if (!is.finite(y_top) || is.na(y_top)) {
      if (is.finite(max_y)) y_top <- max_y
    }
    if (is.finite(y_top)) {
      y_top_adj <- y_top * (1 + max(0, headroom))
      p <- p + ggplot2::scale_y_continuous(limits = c(0, y_top_adj))
    }

    if (isTRUE(annotate_wave)) {
      p <- p + ggplot2::annotate("text", x = -Inf, y = Inf,
                                 label = paste0("Wave ", w_num),
                                 hjust = -0.1, vjust = 1.1, size = 3)
    }
    if (isTRUE(annotate_shift)) {
      # pretty shift label for facet annotation (bottom-left, nudged inside)
      sh_clean <- if (startsWith(sh_full, paste0(outc, "_"))) substring(sh_full, nchar(outc) + 2L) else sh_full
      sh_pretty <- map_label(sh_clean)
      p <- p + ggplot2::annotate("text", x = -Inf, y = -Inf,
                                 label = sh_pretty,
                                 hjust = -0.1, vjust = -0.6, size = 3)
    }
    if (isTRUE(annotate_zeros)) {
      if (!is.na(zeros_str)) {
        p <- p + ggplot2::annotate("text", x = Inf, y = Inf,
                                   label = paste0("zeros: ", zeros_str, "%"),
                                   hjust = 1.1, vjust = 1.1, size = 3)
      }
    }
    p
  }

  plots <- list()
  if (identical(layout, "waves_by_shifts")) {
    for (w_idx in seq_along(waves_order)) {
      w <- waves_order[[w_idx]]
      for (sc_idx in seq_along(shifts_order_full)) {
        is_top <- (w_idx == 1L)
        col_idx <- sc_idx
        plots[[length(plots) + 1]] <- get_plot(outcome, shifts_order_full[[sc_idx]], w, is_top, col_idx)
      }
    }
  } else {
    for (sc_idx in seq_along(shifts_order_full)) {
      sh <- shifts_order_full[[sc_idx]]
      for (w_idx in seq_along(waves_order)) {
        is_top <- (sc_idx == 1L)
        col_idx <- w_idx
        plots[[length(plots) + 1]] <- get_plot(outcome, sh, waves_order[[w_idx]], is_top, col_idx)
      }
    }
  }

  main_title <- if (!is.null(title)) title else paste0("Density ratios — ", outcome)
  patchwork::wrap_plots(plots, ncol = ncol) + patchwork::plot_annotation(title = main_title)
}
