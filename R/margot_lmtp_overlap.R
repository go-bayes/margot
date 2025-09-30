#' Assess Overlap/Positivity from LMTP Models via Density Ratios
#'
#' Summarizes density-ratio overlap and effective sample size (ESS) for LMTP
#' models using `margot_lmtp_positivity()`, and optionally creates diagnostic
#' plots of the density-ratio distributions by wave and shift.
#'
#' @param x LMTP run output with `$models` (e.g., result from margot_lmtp()),
#'   or a list compatible with `margot_lmtp_positivity()`.
#' @param outcomes Optional character vector of outcome names to include.
#' @param shifts Optional character vector of shifts/policies to include
#'   (accepts full names like `t5_pwi_z_shift_up` or cleaned names like `shift_up`).
#' @param plot Logical; if TRUE, create ratio distribution plots.
#' @param theme ggplot2 theme keyword: "classic" (default), "minimal", "bw",
#'   "gray", "light", "dark", "void".
#' @param scale Character; x-axis scale for plotted ratios: "log10" (default)
#'   shows log10(density ratio) for w>0, or "linear" shows the raw ratio.
#' @param digits Integer rounding for summaries (passed to positivity helper).
#' @param verbose Logical; emit informative messages.
#' @param color_by Character; how to colour histogram fills: `"wave"` (default),
#'   `"shift"` (one colour per shift), or `"constant"` (single colour).
#' @param color_by_wave Legacy logical alias for `color_by` (`TRUE` = `"wave"`,
#'   `FALSE` = `"constant"`).
#' @param fill_palette Optional vector of colours (named or unnamed) used when colouring histograms.
#'   Character aliases include `"lab"` (blue/red/grey with a constant fallback) and
#'   `"classic"` (the default qualitative palette).
#' @param ... Ignored. Present for backward compatibility with older
#'   arguments like `save_plots` and `output_dir` that are being phased out.
#'
#' @return A list with:
#'   - overlap_summary: tibble combining by-wave and overall positivity/overlap metrics
#'   - ratio_plots: list of ggplot objects (if plot = TRUE)
#'   - flags: tibble of positivity flags
#'   - text_summary: brief prose summary
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_classic theme_minimal theme_bw theme_gray theme_light theme_dark theme_void
margot_lmtp_overlap <- function(x,
                                outcomes = NULL,
                                shifts = NULL,
                                plot = TRUE,
                                theme = "classic",
                                scale = "log10",
                                digits = 3,
                                verbose = TRUE,
                                color_by = c("wave", "shift", "constant"),
                                color_by_wave = NULL,
                                fill_palette = NULL,
                                bins = 40,
                                binwidth = NULL,
                                xlim = NULL,
                                ...) {

  # compute positivity/overlap summaries
  pos <- margot_lmtp_positivity(x, digits = digits, verbose = verbose)

  by_wave <- pos$by_wave
  overall <- pos$overall
  flags   <- pos$flags

  color_by <- match.arg(color_by)
  if (!is.null(color_by_wave)) {
    color_by <- if (isTRUE(color_by_wave)) "wave" else "constant"
  }

  # Normalize/clean shift suffixes for easy filtering
  clean_shift <- function(df) {
    if (!nrow(df)) return(df)
    starts <- startsWith(df$shift, paste0(df$outcome, "_"))
    df$shift_clean <- ifelse(starts, substring(df$shift, nchar(df$outcome) + 2L), df$shift)
    df
  }
  by_wave <- clean_shift(by_wave)
  overall <- clean_shift(overall)
  flags   <- clean_shift(flags)

  # Filter by requested outcomes/shifts
  if (!is.null(outcomes)) {
    by_wave <- by_wave[by_wave$outcome %in% outcomes, , drop = FALSE]
    overall <- overall[overall$outcome %in% outcomes, , drop = FALSE]
    flags   <- flags[flags$outcome %in% outcomes, , drop = FALSE]
  }
  if (!is.null(shifts)) {
    keep_by <- by_wave$shift %in% shifts | by_wave$shift_clean %in% shifts
    keep_ov <- overall$shift %in% shifts | overall$shift_clean %in% shifts
    keep_fl <- flags$shift %in% shifts | flags$shift_clean %in% shifts
    by_wave <- by_wave[keep_by, , drop = FALSE]
    overall <- overall[keep_ov, , drop = FALSE]
    flags   <- flags[keep_fl, , drop = FALSE]
  }

  by_wave$type <- "by_wave"
  overall$type <- "overall"
  overlap_summary <- rbind(by_wave, overall)

  # Prepare ratio plots, if requested
  ratio_plots <- list()
  if (isTRUE(plot)) {
    # Resolve models from x
    models <- NULL
    if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
      models <- x$models
    } else if (is.list(x) && !is.null(x$density_ratios)) {
      models <- list(`(outcome)` = list(`(model)` = x))
    } else if (is.numeric(x)) {
      fake <- list(density_ratios = x)
      models <- list(`(outcome)` = list(`(model)` = fake))
    } else if (is.list(x) && all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
      models <- list(`(outcome)` = x)
    }

    if (!is.null(models)) {
      # theme switcher
      ggtheme <- switch(theme,
                        classic = ggplot2::theme_classic(),
                        minimal = ggplot2::theme_minimal(),
                        bw      = ggplot2::theme_bw(),
                        gray    = ggplot2::theme_gray(),
                        light   = ggplot2::theme_light(),
                        dark    = ggplot2::theme_dark(),
                        void    = ggplot2::theme_void(),
                        ggplot2::theme_classic())

      default_palette <- c("#4f88c6", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#D55E00", "#999999")
      palette_aliases <- list(
        lab = c(shift_up = "#2c7fb8", shift_down = "#f03b20", null = "#7f7f7f", constant = "#4f88c6"),
        classic = default_palette
      )
      if (is.character(fill_palette) && length(fill_palette) == 1L && fill_palette %in% names(palette_aliases)) {
        fill_palette <- palette_aliases[[fill_palette]]
      }
      shift_names_global <- unique(unlist(lapply(models, names)))
      palette_vec <- if (is.null(fill_palette)) default_palette else as.character(fill_palette)
      if (!length(palette_vec)) palette_vec <- default_palette
      palette_names <- if (is.null(fill_palette)) NULL else names(fill_palette)

      fetch_named_color <- function(key) {
        if (is.null(palette_names) || is.null(key)) return(NA_character_)
        key_chr <- as.character(key)
        if (!(key_chr %in% palette_names)) return(NA_character_)
        val <- fill_palette[[key_chr]]
        if (is.null(val)) return(NA_character_)
        val_chr <- as.character(val)
        if (!nzchar(val_chr)) return(NA_character_)
        val_chr
      }

      clean_shift_name <- function(outc, sh) {
        if (startsWith(sh, paste0(outc, "_"))) substring(sh, nchar(outc) + 2L) else sh
      }
      get_wave_color <- function(idx) {
        named <- fetch_named_color(idx)
        if (!is.na(named)) return(named)
        palette_vec[((as.integer(idx) - 1L) %% length(palette_vec)) + 1L]
      }
      get_shift_color <- function(outcome_name, shift_name) {
        named <- fetch_named_color(shift_name)
        if (!is.na(named)) return(named)
        named_clean <- fetch_named_color(clean_shift_name(outcome_name, shift_name))
        if (!is.na(named_clean)) return(named_clean)
        idx <- match(shift_name, shift_names_global)
        if (is.na(idx)) idx <- 1L
        palette_vec[((idx - 1L) %% length(palette_vec)) + 1L]
      }
      get_constant_color <- function() {
        named <- fetch_named_color("constant")
        if (!is.na(named)) return(named)
        palette_vec[1L]
      }
      add_plot <- function(outcome_name, shift_name, wave_idx, w_vec) {
        # separate zeros for text annotation
        prop_zero <- mean(w_vec == 0)
        w_pos <- w_vec[w_vec > 0]
        # Handle all-zero case (e.g., censored/no support)
        if (length(w_pos) == 0L) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0, y = 0,
                               label = "all ratios = 0",
                               hjust = 0, vjust = 0, size = 3)
          ratio_plots[[paste(outcome_name, shift_name, wave_idx, sep = "::")]] <<- p
          return(invisible())
        }
        # choose fill colouring strategy
        fill_col <- get_constant_color()
        if (identical(color_by, "wave")) {
          fill_col <- get_wave_color(wave_idx)
        } else if (identical(color_by, "shift")) {
          fill_col <- get_shift_color(outcome_name, shift_name)
        }
        if (identical(tolower(scale), "log10")) {
          plot_df <- data.frame(x = log10(w_pos + 1e-12))
          xlab <- NULL
          scale_txt <- "log10"
        } else {
          plot_df <- data.frame(x = w_pos)
          xlab <- NULL
          scale_txt <- "linear"
        }
        title_txt <- paste0("Density ratios — ", outcome_name, " | ", shift_name,
                             " | wave ", wave_idx,
                             " | zeros: ", sprintf("%.1f%%", 100*prop_zero),
                             " | scale: ", scale_txt)
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x)) +
          { if (!is.null(binwidth)) ggplot2::geom_histogram(binwidth = binwidth, color = "white", fill = fill_col, linewidth = 0.3) else ggplot2::geom_histogram(bins = bins, color = "white", fill = fill_col, linewidth = 0.3) } +
          ggplot2::labs(title = title_txt, x = xlab, y = "Count") +
          ggtheme
        if (!is.null(xlim) && length(xlim) == 2 && is.finite(xlim[1]) && is.finite(xlim[2])) {
          p <- p + ggplot2::scale_x_continuous(limits = xlim)
        }
        ratio_plots[[paste(outcome_name, shift_name, wave_idx, sep = "::")]] <<- p
      }

      for (outcome_name in names(models)) {
        shifts_list <- models[[outcome_name]]
        for (shift_name in names(shifts_list)) {
          mod <- shifts_list[[shift_name]]
          dr  <- mod$density_ratios
          if (is.null(dr)) next

          # filter based on requested subsets if provided
          if (!is.null(outcomes) && !(outcome_name %in% outcomes)) next
          # allow shift filter on full or cleaned name
          shift_clean <- if (startsWith(shift_name, paste0(outcome_name, "_"))) substring(shift_name, nchar(outcome_name) + 2L) else shift_name
          if (!is.null(shifts) && !(shift_name %in% shifts || shift_clean %in% shifts)) next

          if (is.matrix(dr)) {
            for (j in seq_len(ncol(dr))) add_plot(outcome_name, shift_name, j, as.numeric(dr[, j]))
          } else {
            add_plot(outcome_name, shift_name, 1L, as.numeric(dr))
          }
        }
      }
    }
  }

  # Text summary
  key_cols <- c("prop_zero","ess_frac","ess_pos_frac")
  med <- function(v) if (length(v)) median(v, na.rm = TRUE) else NA_real_
  medians <- vapply(key_cols, function(k) med(overlap_summary[[k]]), numeric(1))
  text_summary <- sprintf(
    "Across selected LMTP models: median zeros = %.1f%%, median ESS/N = %.3f, median ESS+/(N+) = %.3f.",
    100*medians[["prop_zero"]], medians[["ess_frac"]], medians[["ess_pos_frac"]]
  )

  list(
    overlap_summary = overlap_summary,
    ratio_plots = ratio_plots,
    flags = flags,
    text_summary = text_summary
  )
}
