#' Summarise Super Learner weights for LMTP nuisance models
#'
#' Internal helper that collapses the cross-fit Super Learner weights stored in
#' an LMTP model (`fits_m` for the outcome regression and `fits_r` for the
#' density-ratio regression) to per-wave summaries. Returns a tidy data frame
#' used by downstream plotting and interpretation utilities.
#'
#' @keywords internal
summarise_lmtp_learners <- function(x,
                                    outcome,
                                    shifts = NULL,
                                    label_mapping = NULL,
                                    waves = NULL,
                                    remove_waves = NULL) {
  stopifnot(is.list(x))

  # normalise models list --------------------------------------------------
  models <- NULL
  if (!is.null(x$models) && is.list(x$models)) {
    models <- x$models
  } else {
    stop("`x` must be the result of margot_lmtp() (or similar) with `$models`.", call. = FALSE)
  }

  if (!outcome %in% names(models)) {
    stop("Outcome not found in LMTP models: ", outcome, call. = FALSE)
  }
  outcome_models <- models[[outcome]]
  if (!length(outcome_models)) return(data.frame())

  clean_shift <- function(name) {
    prefix <- paste0(outcome, "_")
    if (startsWith(name, prefix)) substring(name, nchar(prefix) + 1L) else name
  }

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
            use_title_case = TRUE,
            quiet = TRUE
          )
        ),
        error = function(e) lbl
      )
      if (!is.null(out) && !is.na(out)) return(out)
    }
    gsub("_", " ", tools::toTitleCase(lbl))
  }

  shift_df <- data.frame(
    shift_full = names(outcome_models),
    stringsAsFactors = FALSE
  )
  shift_df$shift_clean <- vapply(shift_df$shift_full, clean_shift, character(1))

  if (is.null(shifts)) {
    keep_idx <- seq_len(nrow(shift_df))
  } else {
    keep_idx <- which(shift_df$shift_full %in% shifts | shift_df$shift_clean %in% shifts)
    if (!length(keep_idx)) {
      stop("Requested shifts not found for outcome ", outcome, ": ", paste(shifts, collapse = ", "), call. = FALSE)
    }
  }
  shift_df <- shift_df[keep_idx, , drop = FALSE]

  pref <- c("null", "shift_down", "shift_up")
  ordered_clean <- c(intersect(pref, shift_df$shift_clean), setdiff(shift_df$shift_clean, pref))
  shift_df <- shift_df[match(ordered_clean, shift_df$shift_clean), , drop = FALSE]

  collapse_weights <- function(fits_list, component, wave_labels = NULL) {
    if (is.null(fits_list) || !length(fits_list)) return(data.frame())
    wave_indices <- seq_along(fits_list)
    if (!is.null(waves)) {
      wave_indices <- intersect(wave_indices, as.integer(waves))
    }
    if (!is.null(remove_waves)) {
      wave_indices <- setdiff(wave_indices, as.integer(remove_waves))
    }
    if (!length(wave_indices)) return(data.frame())

    wave_lab <- if (!is.null(wave_labels) && length(wave_labels) >= length(fits_list)) {
      wave_labels
    } else {
      seq_along(fits_list)
    }

    rows <- lapply(wave_indices, function(w) {
      weights_list <- fits_list[[w]]
      if (is.null(weights_list) || !length(weights_list)) return(NULL)

      learners <- unique(unlist(lapply(weights_list, names)))
      if (!length(learners)) return(NULL)

      mat <- matrix(NA_real_, nrow = length(weights_list), ncol = length(learners),
                    dimnames = list(NULL, learners))
      for (i in seq_along(weights_list)) {
        vec <- weights_list[[i]]
        if (is.null(vec) || !length(vec)) next
        mat[i, names(vec)] <- as.numeric(vec)
      }
      valid <- rowSums(!is.na(mat)) > 0
      mat <- mat[valid, , drop = FALSE]
      if (!nrow(mat)) return(NULL)

      weight_mean <- colMeans(mat, na.rm = TRUE)
      weight_sd <- apply(mat, 2, stats::sd, na.rm = TRUE)
      weight_median <- apply(mat, 2, stats::median, na.rm = TRUE)
      weight_min <- apply(mat, 2, min, na.rm = TRUE)
      weight_max <- apply(mat, 2, max, na.rm = TRUE)

      data.frame(
        wave_index = w,
        wave_label = as.character(wave_lab[[w]]),
        learner = names(weight_mean),
        weight_mean = as.numeric(weight_mean),
        weight_sd = as.numeric(weight_sd),
        weight_median = as.numeric(weight_median),
        weight_min = as.numeric(weight_min),
        weight_max = as.numeric(weight_max),
        weight_n = nrow(mat),
        component = component,
        stringsAsFactors = FALSE
      )
    })

    rows <- do.call(rbind, rows)
    if (is.null(rows) || !nrow(rows)) return(data.frame())
    rows
  }

  outcome_label <- map_label(outcome)

  learner_rows <- list()
  for (i in seq_len(nrow(shift_df))) {
    shift_full <- shift_df$shift_full[i]
    shift_clean <- shift_df$shift_clean[i]
    model <- outcome_models[[shift_full]]
    wave_labels <- NULL
    if (!is.null(model$density_ratios)) {
      wave_labels <- colnames(model$density_ratios)
    }

    rows_m <- collapse_weights(model$fits_m, "outcome", wave_labels)
    rows_r <- collapse_weights(model$fits_r, "treatment", wave_labels)

    combined <- rbind(rows_m, rows_r)
    if (!nrow(combined)) next

    combined$shift_full <- shift_full
    combined$shift_clean <- shift_clean
    combined$shift_label <- map_label(shift_clean)
    combined$outcome_label <- outcome_label
    learner_rows[[length(learner_rows) + 1L]] <- combined
  }

  if (!length(learner_rows)) return(data.frame())
  out <- do.call(rbind, learner_rows)
  rownames(out) <- NULL
  out
}


pretty_learner_label <- function(x) {
  x <- gsub("^SL\\.", "", x)
  x <- gsub("_All$", "", x)
  gsub("_", " ", x, fixed = TRUE)
}

component_titles <- function(component) {
  out <- ifelse(component == "outcome", "Outcome regression (m)", "Treatment regression (r)")
  factor(out, levels = c("Outcome regression (m)", "Treatment regression (r)"))
}

#' Plot Super Learner weights for LMTP nuisance fits
#'
#' Produces a heatmap of average Super Learner weights by wave, learner, shift,
#' and nuisance component (outcome regression `m` and density-ratio regression
#' `r`). Colours encode the mean Super Learner weight averaged across
#' cross-fitting folds.
#'
#' @inheritParams margot_interpret_lmtp_positivity
#' @param waves Optional integer vector of waves to keep (matching the wave index
#'   used by the LMTP fits).
#' @param remove_waves Optional integer vector of waves to drop after
#'   subsetting.
#' @param title Optional plot title. Defaults to the pretty outcome label of the
#'   requested contrast when `NULL`.
#' @param component Which nuisance models to include: `"both"` (default),
#'   `"outcome"` (only `m`), or `"treatment"` (only `r`).
#' @return A `ggplot2` object.
#' @export
margot_plot_lmtp_learners <- function(x,
                                      outcome,
                                      shifts = NULL,
                                      label_mapping = NULL,
                                      waves = NULL,
                                      remove_waves = NULL,
                                      title = NULL,
                                      component = c("both", "outcome", "treatment")) {
  component <- match.arg(component)
  data <- summarise_lmtp_learners(x, outcome, shifts, label_mapping, waves, remove_waves)
  if (!nrow(data)) {
    stop("No learner weights available for the requested configuration.", call. = FALSE)
  }

  if (!identical(component, "both")) {
    data <- data[data$component == component, , drop = FALSE]
  }
  if (!nrow(data)) {
    stop("No learner weights available after filtering components.", call. = FALSE)
  }

  data$learner_label <- pretty_learner_label(data$learner)
  learner_order <- stats::aggregate(weight_mean ~ learner_label, data, mean)
  learner_levels <- learner_order$learner_label[order(-learner_order$weight_mean)]
  data$learner_label <- factor(data$learner_label, levels = learner_levels)

  wave_levels <- unique(data$wave_label[order(data$wave_index)])
  data$wave_label <- factor(data$wave_label, levels = wave_levels)

  data$component_label <- component_titles(data$component)
  data$text_colour <- ifelse(data$weight_mean >= 0.6, "#ffffff", "#222222")

  clamp01 <- function(x, range) {
    pmax(pmin(x, range[2]), range[1])
  }

  if (is.null(title)) {
    title <- data$outcome_label[1]
  } else {
    title <- as.character(title)[1]
  }

  ggplot2::ggplot(data, ggplot2::aes(x = wave_label, y = learner_label, fill = weight_mean)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 100 * weight_mean), colour = text_colour), size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(
      name = "Mean weight",
      low = "#f7fbff",
      high = "#08306b",
      limits = c(0, 1),
      oob = clamp01
    ) +
    ggplot2::labs(x = "Wave", y = "Learner", title = title) +
    ggplot2::facet_grid(component_label ~ shift_label, scales = "free_y") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Interpret Super Learner weights for LMTP nuisance fits
#'
#' Generates concise prose describing which Super Learner components dominate
#' the outcome (`m`) and density-ratio (`r`) nuisance regressions across waves
#' and shifts. Highlights waves where a single learner receives (approximately)
#' all the weight, which can signal limited information (e.g., after LOCF
#' imputation).
#'
#' @inheritParams margot_plot_lmtp_learners
#' @param digits Integer number of decimal places to use when reporting
#'   percentages.
#' @param return Either `"text"` (default) for a single character string or
#'   `"list"` for structured components.
#' @return Either a character string or a list (depending on `return`).
#' @export
margot_interpret_lmtp_learners <- function(x,
                                           outcome,
                                           shifts = NULL,
                                           label_mapping = NULL,
                                           waves = NULL,
                                           remove_waves = NULL,
                                           component = c("both", "outcome", "treatment"),
                                           digits = 0,
                                           return = c("text", "list")) {
  component <- match.arg(component)
  return <- match.arg(return)
  digits <- max(0L, as.integer(digits))

  data <- summarise_lmtp_learners(x, outcome, shifts, label_mapping, waves, remove_waves)
  if (!nrow(data)) return(if (identical(return, "text")) "" else list())

  if (!identical(component, "both")) {
    data <- data[data$component == component, , drop = FALSE]
  }
  if (!nrow(data)) return(if (identical(return, "text")) "" else list())

  data$learner_label <- pretty_learner_label(data$learner)
  data$component_label <- component_titles(data$component)

  outcome_label <- unique(data$outcome_label)
  shift_levels <- unique(data$shift_clean[order(data$shift_clean)])

  fmt_pct <- function(x) sprintf(paste0("%.", digits, "f%%"), 100 * x)

  bullets <- list()
  details <- list()
  for (sc in shift_levels) {
    sub_shift <- data[data$shift_clean == sc, , drop = FALSE]
    if (!nrow(sub_shift)) next
    shift_title <- unique(sub_shift$shift_label)
    component_levels <- unique(sub_shift$component)
    lines <- character(0)
    comp_details <- list()

    for (comp in component_levels) {
      comp_data <- sub_shift[sub_shift$component == comp, , drop = FALSE]
      wave_order <- unique(comp_data$wave_index[order(comp_data$wave_index)])
      wave_lines <- character(0)
      wave_list <- list()
      for (w in wave_order) {
        wd <- comp_data[comp_data$wave_index == w, , drop = FALSE]
        if (!nrow(wd)) next
        wd <- wd[order(-wd$weight_mean, wd$learner_label), , drop = FALSE]
        top <- wd[1, , drop = FALSE]
        note <- ""
        if (is.finite(top$weight_max) && top$weight_max >= 0.99) {
          if (is.finite(top$weight_mean) && top$weight_mean >= 0.95) {
            note <- " (dominant; other learners near zero)"
          } else {
            note <- " (peaked at 100% in some folds)"
          }
        } else if (is.finite(top$weight_mean) && top$weight_mean >= 0.75) {
          note <- " (strong preference)"
        }

        runner <- ""
        if (nrow(wd) >= 2 && is.finite(wd$weight_mean[2]) && wd$weight_mean[2] >= 0.15) {
          runner <- paste0("; runner-up ", wd$learner_label[2], " (", fmt_pct(wd$weight_mean[2]), ")")
        }

        wave_lines <- c(wave_lines,
                        paste0(top$wave_label, ": ", top$learner_label, " (", fmt_pct(top$weight_mean), ")",
                               note, runner))
        wave_list[[length(wave_list) + 1L]] <- wd
      }
      comp_title <- unique(component_titles(comp))
      if (length(wave_lines)) {
        lines <- c(lines, paste0("- ", comp_title, ": ", paste(wave_lines, collapse = "; "), "."))
      }
      comp_details[[comp]] <- wave_list
    }

    if (length(lines)) {
      bullets[[length(bullets) + 1L]] <- c(paste0(shift_title, ":"), lines)
      details[[shift_title]] <- comp_details
    }
  }

  header <- paste0("LMTP learner weights for ", outcome_label[1], ".")
  text <- paste(c(header, unlist(bullets)), collapse = "\n")

  if (identical(return, "text")) {
    text
  } else {
    list(
      header = header,
      bullets = bullets,
      details = details,
      data = data
    )
  }
}
