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
                                          ncol = NULL,
                                          drop_titles = TRUE,
                                          title = NULL,
                                          label_mapping = NULL,
                                          annotate_zeros = c("column","facet","none"),
                                          ymax = NULL) {

  annotate_zeros <- match.arg(annotate_zeros)

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
  if (is.null(ncol)) ncol <- length(shifts_order_full)

  # Compute a common Y-axis maximum across the selected plots so column heights
  # are comparable. Allow an explicit `ymax` override when provided.
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
  for (k in selected_keys) {
    max_y <- max(max_y, safe_max_count(ratio_plots[[k]]), na.rm = TRUE)
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

  # For column annotation, pre-compute zeros for the top row wave per column
  col_zeros <- rep(NA_character_, length(shifts_order_full))
  names(col_zeros) <- shifts_order_full
  top_wave <- waves_order[1]
  if (annotate_zeros == "column") {
    for (i in seq_along(shifts_order_full)) {
      k_top <- paste(outcome, shifts_order_full[[i]], as.character(top_wave), sep = "::")
      if (!is.null(ratio_plots[[k_top]])) {
        col_zeros[[i]] <- parse_zeros(ratio_plots[[k_top]])
      }
    }
  }

  # Build grid: row-major by waves x shifts
  get_plot <- function(outc, sh_full, w_num, is_top, col_idx) {
    k <- paste(outc, sh_full, as.character(w_num), sep = "::")
    p <- ratio_plots[[k]]
    if (is.null(p)) {
      # fill empty
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    if (isTRUE(drop_titles)) {
      # Only show the shift label as title on the top row
      if (isTRUE(is_top)) {
        ttl <- col_labels[[col_idx]]
        if (annotate_zeros == "column" && !is.na(col_zeros[[col_idx]])) {
          ttl <- paste0(ttl, " — zeros: ", col_zeros[[col_idx]], "%")
        }
        p <- p + ggplot2::labs(title = ttl)
      } else {
        p <- p + ggplot2::labs(title = NULL)
      }
    }
    # Harmonize Y-axis limits across panels, when available
    if (is.finite(max_y)) {
      p <- p + ggplot2::scale_y_continuous(limits = c(0, max_y))
    }
    if (annotate_zeros == "facet") {
      z <- parse_zeros(p)
      if (!is.na(z)) {
        p <- p + ggplot2::annotate("text", x = Inf, y = Inf,
                                   label = paste0("zeros: ", z, "%"),
                                   hjust = 1.1, vjust = 1.1, size = 3)
      }
    }
    p
  }

  plots <- list()
  for (w_idx in seq_along(waves_order)) {
    w <- waves_order[[w_idx]]
    for (sc_idx in seq_along(shifts_order_full)) {
      is_top <- (w_idx == 1L)
      plots[[length(plots) + 1]] <- get_plot(outcome, shifts_order_full[[sc_idx]], w, is_top, sc_idx)
    }
  }

  main_title <- if (!is.null(title)) title else paste0("Density ratios — ", outcome)
  patchwork::wrap_plots(plots, ncol = ncol) + patchwork::plot_annotation(title = main_title)
}
