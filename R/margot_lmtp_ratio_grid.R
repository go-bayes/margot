#' LMTP density-ratio panel (per-wave grid)
#'
#' Build a grid of density-ratio histograms across waves for a selected
#' outcome and shift/policy from an LMTP run. Plots use either log10 or
#' linear scale on the x-axis and annotate the proportion of zeros.
#'
#' @param x LMTP run output with `$models` (e.g., result from `margot_lmtp()`),
#'   or a single LMTP model (has `$density_ratios`), or a list of such models.
#' @param outcome Character. Outcome name to plot.
#' @param shift Character. Shift/policy name. May be the full name (e.g.,
#'   `t5_pwi_z_shift_up`) or the cleaned suffix (e.g., `shift_up`).
#' @param waves Optional integer vector of wave indices to include. If NULL,
#'   includes all available waves.
#' @param scale Character; `"log10"` (default) or `"linear"` for the x-axis.
#' @param theme ggplot2 theme keyword: `"classic"` (default), `"minimal"`,
#'   `"bw"`, `"gray"`, `"light"`, `"dark"`, `"void"`.
#' @param bins Integer; histogram bins (default 40).
#' @param ncol Integer; number of columns in the grid (default 3).
#' @param verbose Logical; emit informative messages.
#'
#' @return A patchwork plot object (ggplot) composing per-wave histograms.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_classic theme_minimal theme_bw theme_gray theme_light theme_dark theme_void
#' @importFrom patchwork wrap_plots plot_annotation
margot_lmtp_ratio_grid <- function(x,
                                   outcome,
                                   shift,
                                   waves = NULL,
                                   scale = "log10",
                                   theme = "classic",
                                   bins = 40,
                                   ncol = 3,
                                   verbose = TRUE) {

  # Resolve models structure similar to margot_lmtp_overlap
  models <- NULL
  if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
    models <- x$models
  } else if (is.list(x) && !is.null(x$density_ratios)) {
    models <- list(`(outcome)` = list(`(model)` = x))
  } else if (is.list(x) && all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
    models <- list(`(outcome)` = x)
  } else {
    stop("Unsupported input to `margot_lmtp_ratio_grid()`. Pass a margot_lmtp() result or an LMTP model/list with $density_ratios.")
  }

  if (!outcome %in% names(models)) {
    stop("Outcome not found in LMTP models: ", outcome)
  }

  shifts_list <- models[[outcome]]
  # find shift by exact or cleaned match
  find_match <- function(nm) {
    if (identical(nm, shift)) return(TRUE)
    if (startsWith(nm, paste0(outcome, "_"))) {
      return(substring(nm, nchar(outcome) + 2L) == shift)
    }
    FALSE
  }
  shift_names <- names(shifts_list)
  match_idx <- vapply(shift_names, find_match, logical(1))
  if (!any(match_idx)) stop("Shift not found for outcome=", outcome, ": ", shift)
  shift_name <- shift_names[which(match_idx)[1]]

  mod <- shifts_list[[shift_name]]
  dr  <- mod$density_ratios
  if (is.null(dr)) stop("No density ratios found for outcome=", outcome, ", shift=", shift_name)

  if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
  if (is.data.frame(dr)) dr <- as.matrix(dr)
  if (!is.matrix(dr) && !is.vector(dr)) dr <- as.matrix(dr)

  # Theme
  ggtheme <- switch(theme,
                   classic = ggplot2::theme_classic(),
                   minimal = ggplot2::theme_minimal(),
                   bw      = ggplot2::theme_bw(),
                   gray    = ggplot2::theme_gray(),
                   light   = ggplot2::theme_light(),
                   dark    = ggplot2::theme_dark(),
                   void    = ggplot2::theme_void(),
                   ggplot2::theme_classic())

  # Helper to make one plot
  make_plot <- function(w_vec, wave_idx) {
    prop_zero <- mean(w_vec == 0)
    w_pos <- w_vec[w_vec > 0]
    if (identical(tolower(scale), "log10")) {
      plot_df <- data.frame(x = log10(w_pos + 1e-12))
      xlab <- "log10(density ratio)"
      scale_txt <- "log10"
    } else {
      plot_df <- data.frame(x = w_pos)
      xlab <- "density ratio"
      scale_txt <- "linear"
    }
    ttl <- paste0("wave ", wave_idx, " — zeros: ", sprintf("%.1f%%", 100*prop_zero))
    ggplot2::ggplot(plot_df, ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(bins = bins, color = "white", fill = "#4f88c6", linewidth = 0.3) +
      ggplot2::labs(title = ttl, x = xlab, y = "Count") +
      ggtheme
  }

  plots <- list()
  if (is.matrix(dr)) {
    wave_ids <- seq_len(ncol(dr))
    if (!is.null(waves)) wave_ids <- intersect(wave_ids, waves)
    for (j in wave_ids) plots[[as.character(j)]] <- make_plot(as.numeric(dr[, j]), j)
  } else {
    if (!is.null(waves) && !(1L %in% waves)) {
      if (verbose && isTRUE(requireNamespace("cli", quietly = TRUE))) cli::cli_alert_info("Only one wave available; ignoring waves filter")
    }
    plots[["1"]] <- make_plot(as.numeric(dr), 1L)
  }

  if (!length(plots)) stop("No plots generated; check waves filter.")

  title_main <- paste0("Density ratios — ", outcome, " | ", shift_name,
                       " (scale: ", ifelse(tolower(scale)=="log10","log10","linear"), ")")
  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(title = title_main)
}
