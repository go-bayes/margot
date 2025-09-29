#' LMTP density-ratio multi-shift grid (wave x shift)
#'
#' Build a facet grid of density-ratio histograms across waves (rows) and
#' shifts (columns) for a selected outcome from an LMTP run. Each facet
#' annotates the proportion of zero ratios (censoring mass).
#'
#' @param x LMTP run output with `$models` (e.g., result from `margot_lmtp()`),
#'   or a single LMTP model (has `$density_ratios`), or a list of such models.
#' @param outcome Character. Outcome name to plot.
#' @param shifts Optional character vector of shifts/policies to include.
#'   Accepts full names (e.g., `t5_pwi_z_shift_up`) or cleaned suffixes
#'   (e.g., `shift_up`, `shift_down`, `null`). If NULL, includes all shifts.
#' @param waves Optional integer vector of waves to include. If NULL, includes all.
#' @param scale Character; `"log10"` (default) or `"linear"` for the x-axis.
#' @param theme ggplot2 theme keyword: `"classic"` (default), `"minimal"`,
#'   `"bw"`, `"gray"`, `"light"`, `"dark"`, `"void"`.
#' @param bins Integer; histogram bins (default 40).
#' @param verbose Logical; emit informative messages.
#' @param label_mapping Optional named list for human-friendly labels. When
#'   provided, used via `transform_label()` if available.
#'
#' @return A ggplot object with `facet_grid(rows = vars(wave), cols = vars(shift))`.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs facet_grid annotate theme_classic theme_minimal theme_bw theme_gray theme_light theme_dark theme_void
margot_lmtp_ratio_multigrid <- function(x,
                                        outcome,
                                        shifts = NULL,
                                        waves = NULL,
                                        scale = "log10",
                                        theme = "classic",
                                        bins = 40,
                                        verbose = TRUE,
                                        label_mapping = NULL) {

  # Resolve models
  models <- NULL
  if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
    models <- x$models
  } else if (is.list(x) && !is.null(x$density_ratios)) {
    models <- list(`(outcome)` = list(`(model)` = x))
  } else if (is.list(x) && all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
    models <- list(`(outcome)` = x)
  } else {
    stop("Unsupported input to `margot_lmtp_ratio_multigrid()`. Pass a margot_lmtp() result or LMTP model/list with $density_ratios.")
  }

  if (!outcome %in% names(models)) stop("Outcome not found in LMTP models: ", outcome)
  shifts_list <- models[[outcome]]

  # Helper: cleaned shift suffix
  clean_shift_name <- function(outc, sh) if (startsWith(sh, paste0(outc, "_"))) substring(sh, nchar(outc) + 2L) else sh

  # Select shifts (full or cleaned)
  all_shift_full  <- names(shifts_list)
  all_shift_clean <- vapply(all_shift_full, function(s) clean_shift_name(outcome, s), character(1))
  if (is.null(shifts)) {
    keep_idx <- rep(TRUE, length(all_shift_full))
  } else {
    keep_idx <- (all_shift_full %in% shifts) | (all_shift_clean %in% shifts)
  }
  if (!any(keep_idx)) stop("No matching shifts found for outcome=", outcome)
  sel_full  <- all_shift_full[keep_idx]
  sel_clean <- all_shift_clean[keep_idx]

  # Label helper using transform_label if available
  map_label <- function(lbl) {
    if (exists("transform_label", mode = "function")) {
      tryCatch(
        transform_label(
          label = lbl,
          label_mapping = label_mapping,
          options = list(remove_tx_prefix = TRUE,
                         remove_z_suffix = TRUE,
                         remove_underscores = TRUE,
                         use_title_case = TRUE)
        ),
        error = function(e) lbl
      )
    } else {
      # fallback: simple prettify
      gsub("_", " ", tools::toTitleCase(lbl))
    }
  }

  # Build long dataset for plotting
  rows <- list()
  anns <- list()
  for (i in seq_along(sel_full)) {
    sh_full  <- sel_full[i]
    sh_clean <- sel_clean[i]
    mod <- shifts_list[[sh_full]]
    dr  <- mod$density_ratios
    if (is.null(dr)) next

    if (is.matrix(dr)) {
      wave_ids <- seq_len(ncol(dr))
      if (!is.null(waves)) wave_ids <- intersect(wave_ids, waves)
      for (j in wave_ids) {
        w_vec <- as.numeric(dr[, j])
        prop_zero <- mean(w_vec == 0)
        w_pos <- w_vec[w_vec > 0]
        xvals <- if (identical(tolower(scale), "log10")) log10(w_pos + 1e-12) else w_pos
        if (length(xvals)) {
          rows[[length(rows)+1]] <- data.frame(
            x = xvals,
            wave = paste0("wave ", j),
            shift = map_label(sh_clean),
            stringsAsFactors = FALSE
          )
        }
        anns[[length(anns)+1]] <- data.frame(
          wave = paste0("wave ", j),
          shift = map_label(sh_clean),
          zeros_label = sprintf("zeros: %.1f%%", 100*prop_zero),
          x = Inf, y = Inf,
          stringsAsFactors = FALSE
        )
      }
    } else {
      w_vec <- as.numeric(dr)
      prop_zero <- mean(w_vec == 0)
      w_pos <- w_vec[w_vec > 0]
      xvals <- if (identical(tolower(scale), "log10")) log10(w_pos + 1e-12) else w_pos
      if (length(xvals)) {
        rows[[length(rows)+1]] <- data.frame(
          x = xvals,
          wave = "wave 1",
          shift = map_label(sh_clean),
          stringsAsFactors = FALSE
        )
      }
      anns[[length(anns)+1]] <- data.frame(
        wave = "wave 1",
        shift = map_label(sh_clean),
        zeros_label = sprintf("zeros: %.1f%%", 100*prop_zero),
        x = Inf, y = Inf,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) stop("No positive ratios to plot for the selected outcome/shifts/waves.")
  df  <- do.call(rbind, rows)
  ann <- do.call(rbind, anns)

  ggtheme <- switch(theme,
                   classic = ggplot2::theme_classic(),
                   minimal = ggplot2::theme_minimal(),
                   bw      = ggplot2::theme_bw(),
                   gray    = ggplot2::theme_gray(),
                   light   = ggplot2::theme_light(),
                   dark    = ggplot2::theme_dark(),
                   void    = ggplot2::theme_void(),
                   ggplot2::theme_classic())

  xlab <- if (identical(tolower(scale), "log10")) "log10(density ratio)" else "density ratio"
  title_main <- paste0("Density ratios — ", outcome, " (scale: ", ifelse(tolower(scale)=="log10","log10","linear"), ")")

  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = bins, color = "white", fill = "#4f88c6", linewidth = 0.3) +
    ggplot2::facet_grid(rows = ggplot2::vars(wave), cols = ggplot2::vars(shift), scales = "free_y") +
    ggplot2::labs(title = title_main, x = xlab, y = "Count") +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = x, y = y, label = zeros_label), inherit.aes = FALSE, hjust = 1.1, vjust = 1.1, size = 3) +
    ggtheme
}

