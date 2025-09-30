#' Interpret LMTP positivity via effective sample sizes
#'
#' Builds a concise textual summary of LMTP density-ratio diagnostics for a
#' single outcome. For each requested shift, the function computes effective
#' sample sizes (ESS) by wave and for the pooled person-time using
#' `colSums()` over the underlying density ratios. The output mirrors the
#' filtering and labelling conventions used by
#' [`margot_plot_lmtp_overlap_grid()`] for consistency between prose and
#' graphics.
#'
#' @param x LMTP run output (e.g., the result of [margot_lmtp()]) or any object
#'   that exposes `$density_ratios` in the same structure as the plot helpers.
#' @param outcome Character scalar giving the outcome name to summarise.
#' @param shifts Optional character vector of shifts to include (either full
#'   names such as `t5_pwi_z_shift_up` or cleaned suffixes such as `shift_up`).
#'   If `NULL`, all available shifts for the outcome are used.
#' @param label_mapping Optional named list passed to [transform_label()] for
#'   readable outcome/shift labels.
#' @param waves Optional integer vector of wave indices to keep (matching the
#'   column positions used by the overlap plot). Defaults to all available
#'   waves.
#' @param remove_waves Optional integer vector of waves to exclude after any
#'   inclusion via `waves`.
#' @param digits Integer number of decimal places to use when reporting
#'   ESS-based fractions (e.g., `ESS/N`).
#' @param include_overview Logical; if `TRUE`, append a short paragraph
#'   summarising the distribution of ESS across the selected waves and the
#'   pooled estimates across shifts.
#' @param return Character; either `"text"` (default, a single markdown-ready
#'   string) or `"list"` (detailed components including the computed ESS
#'   tables).
#'
#' @return Either a single character string (default) or a list containing the
#'   header, per-shift lines, overview text, and the underlying ESS summaries
#'   when `return = "list"`.
#'
#' @examples
#' \dontrun{
#' txt <- margot_interpret_lmtp_positivity(fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping
#' )
#' cat(txt)
#' }
#'
#' @export
margot_interpret_lmtp_positivity <- function(x,
                                             outcome,
                                             shifts = NULL,
                                             label_mapping = NULL,
                                             waves = NULL,
                                             remove_waves = NULL,
                                             digits = 2,
                                             include_overview = TRUE,
                                             return = c("text", "list")) {
  stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))
  if (!is.null(waves)) stopifnot(is.numeric(waves))
  if (!is.null(remove_waves)) stopifnot(is.numeric(remove_waves))
  digits <- max(0L, as.integer(digits))
  return <- match.arg(return)

  # -----------------------------------------------------------------------
  # Normalise input to outcome -> shift -> model (with density ratios)
  normalise_models <- function(obj) {
    if (is.list(obj) && !is.null(obj$models) && is.list(obj$models)) {
      return(obj$models)
    }
    if (is.list(obj) && !is.null(obj$density_ratios)) {
      return(list(`(outcome)` = list(`(shift)` = obj)))
    }
    if (is.numeric(obj)) {
      fake <- list(density_ratios = obj)
      return(list(`(outcome)` = list(`(shift)` = fake)))
    }
    if (is.list(obj) && length(obj) &&
        all(vapply(obj, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
      return(list(`(outcome)` = obj))
    }
    stop("Unsupported input to `margot_interpret_lmtp_positivity()`. Pass a `margot_lmtp()` result, a single LMTP model, or a numeric vector/matrix of density ratios.")
  }

  models_nested <- normalise_models(x)
  if (!outcome %in% names(models_nested)) {
    stop("Outcome not found in models: ", outcome)
  }
  outcome_models <- models_nested[[outcome]]
  if (!length(outcome_models)) {
    return(if (identical(return, "text")) "" else list())
  }

  clean_shift <- function(name) {
    prefix <- paste0(outcome, "_")
    if (startsWith(name, prefix)) substring(name, nchar(prefix) + 1L) else name
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
      stop("Requested shifts not found for outcome ", outcome, ": ", paste(shifts, collapse = ", "))
    }
  }
  shift_df <- shift_df[keep_idx, , drop = FALSE]

  # Preferred ordering for reporting
  pref_order <- c("null", "shift_down", "shift_up")
  ord <- c(intersect(pref_order, shift_df$shift_clean),
           setdiff(shift_df$shift_clean, pref_order))
  shift_df <- shift_df[match(ord, shift_df$shift_clean), , drop = FALSE]

  # Helpers ----------------------------------------------------------------
  fmt_frac <- function(x) {
    out <- rep("NA", length(x))
    idx <- is.finite(x)
    if (any(idx)) out[idx] <- sprintf(paste0("%.", digits, "f"), x[idx])
    out
  }
  fmt_int <- function(x) {
    formatter <- function(val) format(round(val), big.mark = ",", scientific = FALSE)
    if (exists("pretty_number", mode = "function")) {
      tryCatch(pretty_number(round(x)), error = function(e) formatter(x))
    } else {
      formatter(x)
    }
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

  summarise_shift <- function(shift_full, shift_clean) {
    mod <- outcome_models[[shift_full]]
    dr <- mod$density_ratios
    if (is.null(dr)) return(NULL)
    if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
    if (is.vector(dr)) dr <- matrix(dr, ncol = 1L)
    if (!is.matrix(dr)) dr <- as.matrix(dr)
    storage.mode(dr) <- "double"

    total_cols <- ncol(dr)
    if (total_cols == 0L) return(NULL)

    col_idx <- seq_len(total_cols)
    if (!is.null(waves)) col_idx <- intersect(col_idx, as.integer(waves))
    if (!is.null(remove_waves)) col_idx <- setdiff(col_idx, as.integer(remove_waves))
    if (!length(col_idx)) return(NULL)

    sub_dr <- dr[, col_idx, drop = FALSE]
    # Ensure matrix structure even when a single column is selected
    if (!is.matrix(sub_dr)) sub_dr <- matrix(sub_dr, ncol = 1L)

    sum_w <- colSums(sub_dr, na.rm = TRUE)
    sum_w2 <- colSums(sub_dr^2, na.rm = TRUE)
    n_wave <- colSums(!is.na(sub_dr))

    ess <- ifelse(sum_w2 > 0, (sum_w^2) / sum_w2, NA_real_)
    ess_frac <- ifelse(n_wave > 0, ess / n_wave, NA_real_)

    overall_sum <- sum(sum_w)
    overall_sum2 <- sum(sum_w2)
    overall_n <- sum(n_wave)
    overall_ess <- if (overall_sum2 > 0) (overall_sum^2) / overall_sum2 else NA_real_
    overall_frac <- if (overall_n > 0) overall_ess / overall_n else NA_real_

    data.frame(
      shift_full = shift_full,
      shift_clean = shift_clean,
      wave = col_idx,
      ess = ess,
      ess_frac = ess_frac,
      n = n_wave,
      stringsAsFactors = FALSE
    ) -> wave_df

    list(
      waves = wave_df,
      overall = data.frame(
        shift_full = shift_full,
        shift_clean = shift_clean,
        ess = overall_ess,
        ess_frac = overall_frac,
        n = overall_n,
        stringsAsFactors = FALSE
      ),
      label = map_label(shift_clean)
    )
  }

  shift_results <- lapply(seq_len(nrow(shift_df)), function(i) {
    summarise_shift(shift_df$shift_full[i], shift_df$shift_clean[i])
  })
  names(shift_results) <- shift_df$shift_clean
  shift_results <- Filter(Negate(is.null), shift_results)
  if (!length(shift_results)) {
    return(if (identical(return, "text")) "" else list())
  }

  # Reference counts from the first shift for baseline N / person-time
  ref_waves <- shift_results[[1]]$waves
  baseline_n <- if (nrow(ref_waves)) max(ref_waves$n, na.rm = TRUE) else NA_real_
  person_time <- if (nrow(ref_waves)) sum(ref_waves$n, na.rm = TRUE) else NA_real_

  outcome_label <- map_label(outcome)

  # Summary statistics across shifts/waves
  all_wave_ess <- unlist(lapply(shift_results, function(res) res$waves$ess))
  all_wave_ess <- all_wave_ess[is.finite(all_wave_ess)]
  overall_ess_vals <- vapply(shift_results, function(res) res$overall$ess, numeric(1))
  overall_ess_vals <- overall_ess_vals[is.finite(overall_ess_vals)]

  wave_summary_line <- NULL
  if (length(all_wave_ess)) {
    q <- stats::quantile(all_wave_ess, probs = c(0.25, 0.5, 0.75), names = FALSE, type = 2)
    wave_summary_line <- paste0(
      "Across selected shifts and waves, ESS had median ", fmt_int(q[2]),
      " (IQR ", fmt_int(q[1]), "–", fmt_int(q[3]), ")."
    )
  }
  overall_summary_line <- NULL
  if (length(overall_ess_vals)) {
    med <- stats::median(overall_ess_vals)
    rng <- range(overall_ess_vals)
    overall_summary_line <- paste0(
      "Overall ESS (pooled within shifts) ranged ", fmt_int(rng[1]), "–",
      fmt_int(rng[2]), " (median ", fmt_int(med), ")."
    )
  }

  header_bits <- c(
    paste0("LMTP positivity for ", outcome_label, ". Effective sample sizes",
           " (ESS) are computed from raw density ratios using colSums."),
    if (is.finite(baseline_n)) paste0("Baseline N ≈ ", fmt_int(baseline_n), ".") else "",
    if (is.finite(person_time)) paste0("Person-time rows per shift (selected waves) = ", fmt_int(person_time), ".") else ""
  )
  header <- paste(header_bits[nzchar(header_bits)], collapse = " ")

  overview_lines <- character(0)
  if (isTRUE(include_overview)) {
    overview_lines <- c(wave_summary_line, overall_summary_line)
    overview_lines <- overview_lines[nzchar(overview_lines)]
  }

  # Per-shift bullet lines -------------------------------------------------
  lines <- vapply(names(shift_results), function(name) {
    res <- shift_results[[name]]
    wave_df <- res$waves
    if (!nrow(wave_df)) return("")
    wave_bits <- paste0(
      wave_df$wave, ": ", fmt_int(wave_df$ess),
      " (ESS/N = ", fmt_frac(wave_df$ess_frac), ")"
    )
    wave_clause <- paste(wave_bits, collapse = ", ")
    overall_df <- res$overall
    overall_clause <- ifelse(
      is.finite(overall_df$ess),
      paste0("; overall ESS = ", fmt_int(overall_df$ess),
             " (ESS/N_pt = ", fmt_frac(overall_df$ess_frac), ")"),
      ""
    )
    paste0("- ", res$label, ": ESS by wave — ", wave_clause, overall_clause)
  }, character(1))
  lines <- lines[nzchar(lines)]

  text_lines <- c(header, overview_lines, lines)
  text <- paste(text_lines, collapse = "\n")

  if (identical(return, "text")) {
    text
  } else {
    list(
      text = text,
      header = header,
      overview = overview_lines,
      lines = lines,
      outcome_label = outcome_label,
      shifts = shift_results,
      baseline_n = baseline_n,
      person_time = person_time
    )
  }
}

#' @rdname margot_interpret_lmtp_positivity
#' @export
margot_interpret_lmtp_overlap <- function(...) {
  msg <- "`margot_interpret_lmtp_overlap()` is soft-deprecated; use `margot_interpret_lmtp_positivity()` instead."
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_warn(msg)
  } else {
    warning(msg, call. = FALSE)
  }
  margot_interpret_lmtp_positivity(...)
}
