#' Assess Overlap/Positivity from LMTP Models via Density Ratios
#'
#' Summarises density-ratio overlap and effective sample size (ESS) for LMTP
#' models using `margot_lmtp_positivity()`, and optionally creates diagnostic
#' plots of the density-ratio distributions by wave and shift.
#'
#' **Censoring vs. Treatment Positivity:** By default, histograms show only
#' **uncensored observations** (density ratios r > 0), as zeros primarily reflect
#' dropout/censoring rather than treatment positivity violations. The censoring
#' rate (proportion r = 0) is reported in plot titles and summaries. Use
#' `show_censored = TRUE` to include zeros in histograms (shown as a bar at r = 0).
#'
#' @param x LMTP run output with `$models` (e.g., result from margot_lmtp()),
#'   or a list compatible with `margot_lmtp_positivity()`.
#' @param outcomes Optional character vector of outcome names to include.
#' @param shifts Optional character vector of shifts/policies to include
#'   (accepts full names like `t5_pwi_z_shift_up` or cleaned names like `shift_up`).
#' @param plot Logical; if TRUE, create ratio distribution plots.
#' @param show_censored Logical; if FALSE (default), histograms exclude zeros (r = 0)
#'   to focus on uncensored observations. If TRUE, includes zeros in the histogram.
#'   Censoring rate is always reported in plot titles.
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
#' @param ... Optional named arguments recognised by
#'   `margot_interpret_lmtp_positivity()` when assembling the text summary
#'   (e.g., `label_mapping`, `waves`, `remove_waves`, `include_methods`,
#'   `include_diagnostics`). Unrecognised entries are silently ignored for
#'   backward compatibility with deprecated arguments such as `save_plots`.
#'
#' @return A list with:
#'   - overlap_summary: tibble combining by-wave and overall positivity/overlap metrics
#'   - ratio_plots: list of ggplot objects (if plot = TRUE)
#'   - flags: tibble of positivity flags
#'   - text_summary: markdown-ready prose from `margot_interpret_lmtp_positivity()`
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_classic theme_minimal theme_bw theme_gray theme_light theme_dark theme_void
margot_lmtp_overlap <- function(x,
                                outcomes = NULL,
                                shifts = NULL,
                                plot = TRUE,
                                show_censored = FALSE,
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

      default_palette <- if (exists("margot_palette", mode = "function")) {
        tryCatch(margot_palette("classic"), error = function(e) NULL)
      } else NULL
      if (is.null(default_palette) || !length(default_palette)) {
        default_palette <- c("#4f88c6", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#D55E00", "#999999")
      }
      lab_palette_internal <- if (exists("margot_palette", mode = "function")) {
        tryCatch(margot_palette("lab"), error = function(e) NULL)
      } else NULL
      lab_resolver <- if (exists("margot_palette_lab_resolve", mode = "function")) {
        get("margot_palette_lab_resolve", mode = "function")
      } else NULL
      palette_aliases <- list(
        lab = lab_palette_internal,
        classic = default_palette
      )
      if (is.null(palette_aliases$lab) || !length(palette_aliases$lab)) {
        palette_aliases$lab <- c(
          shift_up = "#d95f0e",
          shift_down = "#2c7fb8",
          null = "#7f7f7f",
          constant = "#4f88c6",
          ipsi_02 = "#fdd0a2",
          ipsi_05 = "#fd8d3c",
          ipsi_10 = "#d95f0e",
          ipsi_15 = "#a63603"
        )
      }
      fill_palette_alias <- NULL
      if (is.character(fill_palette) && length(fill_palette) == 1L && fill_palette %in% names(palette_aliases)) {
        fill_palette_alias <- fill_palette
        fill_palette <- palette_aliases[[fill_palette]]
      }
      lab_palette_in_use <- identical(fill_palette_alias, "lab")
      if (!lab_palette_in_use && !is.null(fill_palette) && !is.null(palette_aliases$lab)) {
        lab_palette_in_use <- isTRUE(all.equal(unname(fill_palette), unname(palette_aliases$lab)))
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
        if (isTRUE(lab_palette_in_use) && !is.null(lab_resolver) && !is.null(palette_aliases$lab)) {
          resolved <- tryCatch(lab_resolver(
            labels = c(shift_name, clean_shift_name(outcome_name, shift_name)),
            palette = palette_aliases$lab,
            default = NA_character_
          ), error = function(e) c(NA_character_, NA_character_))
          resolved <- resolved[!is.na(resolved)]
          if (length(resolved)) return(resolved[[1L]])
        }
        # pattern-based fallback for common shift types
        shift_lower <- tolower(shift_name)
        if (grepl("null", shift_lower)) return("#7f7f7f")  # grey for null
        if (grepl("up", shift_lower)) return("#d95f0e")    # orange for up
        if (grepl("down", shift_lower)) return("#2c7fb8")  # blue for down
        # final fallback: cycle through palette
        idx <- match(shift_name, shift_names_global)
        if (is.na(idx)) idx <- 1L
        palette_vec[((idx - 1L) %% length(palette_vec)) + 1L]
      }
      get_constant_color <- function() {
        named <- fetch_named_color("constant")
        if (!is.na(named)) return(named)
        if (isTRUE(lab_palette_in_use) && !is.null(lab_resolver) && !is.null(palette_aliases$lab)) {
          resolved <- tryCatch(lab_resolver(
            labels = "constant",
            palette = palette_aliases$lab,
            default = NA_character_
          ), error = function(e) NA_character_)
          if (!is.na(resolved)) return(resolved)
        }
        palette_vec[1L]
      }
      add_plot <- function(outcome_name, shift_name, wave_idx, w_vec) {
        # separate zeros for censoring rate annotation
        prop_zero <- mean(w_vec == 0)
        w_pos <- w_vec[w_vec > 0]

        # determine what to plot based on show_censored parameter
        w_plot <- if (isTRUE(show_censored)) w_vec else w_pos

        # handle all-zero case (e.g., censored/no support)
        if (length(w_pos) == 0L) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0, y = 0,
                               label = "all ratios = 0 (fully censored)",
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

        # prepare plot data based on scale
        if (identical(tolower(scale), "log10")) {
          # for log scale, always exclude zeros (log(0) undefined)
          plot_df <- data.frame(x = log10(w_pos + 1e-12))
          xlab <- NULL
          scale_txt <- "log10"
        } else {
          # for linear scale, respect show_censored parameter
          plot_df <- data.frame(x = w_plot)
          xlab <- NULL
          scale_txt <- "linear"
        }

        censor_label <- if (isTRUE(show_censored)) "censored: " else "uncensored | censored: "
        title_txt <- paste0("Density ratios — ", outcome_name, " | ", shift_name,
                             " | wave ", wave_idx,
                             " | ", censor_label, sprintf("%.1f%%", 100*prop_zero),
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

          if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
          if (is.data.frame(dr)) dr <- as.matrix(dr)
          if (!is.matrix(dr) && !is.vector(dr)) dr <- as.matrix(dr)

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

  # Text summary assembled via margot_interpret_lmtp_positivity()
  dots <- list(...)
  allowed_opts <- c(
    "label_mapping",
    "waves",
    "remove_waves",
    "include_methods",
    "include_diagnostics",
    "digits",
    # Forward IPSI/context and policy-rate options to the interpreter
    "include_ipsi_context",
    "treatment_label",
    "ipsi_example_g",
    "include_policy_rates",
    "policy_rate_threshold",
    "policy_rate_strict",
    "include_deterministic_context",
    # New testing and IPSI recommendation options
    "include_tests",
    "test_thresholds",
    "include_ipsi_recommend",
    "include_test_explanations"
  )
  interpret_opts <- dots[intersect(names(dots), allowed_opts)]
  if (!("digits" %in% names(interpret_opts))) {
    interpret_opts$digits <- digits
  }
  interpret_opts$shifts <- shifts
  interpret_opts$return <- "text"

  selected_outcomes <- unique(overlap_summary$outcome)
  selected_outcomes <- selected_outcomes[!is.na(selected_outcomes) & nzchar(selected_outcomes)]

  text_chunks <- vapply(selected_outcomes, function(outcome_name) {
    res <- tryCatch(
      do.call(
        margot_interpret_lmtp_positivity,
        c(list(x = x, outcome = outcome_name), interpret_opts)
      ),
      error = function(e) {
        if (isTRUE(verbose)) {
          warn_msg <- paste0(
            "margot_interpret_lmtp_positivity() failed for outcome ",
            outcome_name, ": ", conditionMessage(e)
          )
          if (requireNamespace("cli", quietly = TRUE)) {
            cli::cli_alert_warning(warn_msg)
          } else {
            warning(warn_msg, call. = FALSE)
          }
        }
        ""
      }
    )
    if (is.null(res)) "" else res
  }, character(1))
  text_chunks <- text_chunks[nzchar(text_chunks)]
  text_summary <- if (length(text_chunks)) paste(text_chunks, collapse = "\n\n") else ""
  # LaTeX sanitiser for any residual glyphs
  sanitize_latex <- function(txt) {
    if (!is.character(txt) || !length(txt)) return(txt)
    out <- txt
    out <- gsub("→", " $\\\\to$ ", out, fixed = TRUE)
    out <- gsub("±", " $\\\\pm$ ", out, fixed = TRUE)
    out <- gsub("≥", " $\\\\ge$ ", out, fixed = TRUE)
    out <- gsub("≤", " $\\\\le$ ", out, fixed = TRUE)
    out <- gsub("≈", " $\\\\approx$ ", out, fixed = TRUE)
    out <- gsub("×", " $\\\\times$ ", out, fixed = TRUE)
    out
  }
  text_summary <- sanitize_latex(text_summary)

  list(
    overlap_summary = overlap_summary,
    ratio_plots = ratio_plots,
    flags = flags,
    text_summary = text_summary
  )
}
