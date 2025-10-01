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
#' @param include_methods Logical; if TRUE, prepends a methodological explanation of
#'   density ratios, their interpretation, and ESS computation (default: FALSE).
#' @param include_diagnostics Logical; if TRUE, appends detailed diagnostics per shift
#'   including zeros, range, quantiles, and tail probabilities (default: FALSE).
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
#' # Basic usage
#' txt <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping
#' )
#' cat(txt)
#'
#' # With methodological explanation
#' txt_methods <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_methods = TRUE
#' )
#' cat(txt_methods)
#'
#' # With detailed diagnostics
#' txt_diagnostics <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_diagnostics = TRUE
#' )
#' cat(txt_diagnostics)
#'
#' # Complete report with methods and diagnostics
#' txt_full <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_methods = TRUE,
#'   include_diagnostics = TRUE
#' )
#' cat(txt_full)
#'
#' # Get structured list output
#' result <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   include_methods = TRUE,
#'   include_diagnostics = TRUE,
#'   return = "list"
#' )
#' # Access components: result$methods, result$diagnostics, result$shifts
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
                                             include_methods = FALSE,
                                             include_diagnostics = FALSE,
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

  # methods text helper ----------------------------------------------------
  methods_text <- function() {
    c(
      "## Understanding Density Ratios in LMTP",
      "",
      "Density ratios act as weights in the estimator to rebalance the observed data to mimic the distribution under the modified policy. A value $r_t > 1$ indicates that the observed treatment $A_t$ is more likely under the modified policy than under the observed mechanism, so the observation is up-weighted at that time point. Conversely, $r_t < 1$ down-weights it.",
      "",
      "Examining the distribution of density ratios allows us to assess practical positivity. Many values near 0 or extremely large suggest violations of the positivity assumption, where certain treatments are improbable under the policy, leading to unstable estimates.",
      "",
      "In a single-time-point average treatment effect (ATE) where the policy sets treatment to 1, the density ratio reduces to the inverse probability weight $1 / \\Pr(A=1 \\mid W)$ for treated units (and 0 for untreated). Where exposures are repeated, the product of ratios across time points gives the overall weight for each observation.",
      "",
      "**Effective Sample Size (ESS)** summarises the effective information in weighted data using the formula: $\\text{ESS} = (\\sum w)^2 / \\sum w^2$. Higher variance in weights reduces ESS. When all weights are equal, $\\text{ESS} = N$ (the actual sample size). $\\text{ESS}/N$ gives the proportion of effective information retained.",
      "",
      "### Censoring vs. Treatment Positivity",
      "",
      "In longitudinal LMTP, zeros ($r_t = 0$) in density ratios primarily reflect **censoring** (dropout) rather than treatment positivity violations. When an individual is censored at time $t$, they have no observed treatment at subsequent waves, yielding $r_t = 0$ in the numerator regardless of the policy. These censoring-induced zeros appear identically across all policies for the same individual.",
      "",
      "In contrast, true **treatment positivity violations** are policy-specific: an observed treatment trajectory may be incompatible with one policy but not another. To distinguish these cases, we focus positivity diagnostics on **uncensored observations** ($r > 0$), where the density ratio reflects actual treatment mechanism compatibility rather than missing data.",
      "",
      "The censoring rate (proportion of $r = 0$) is reported separately per shift to quantify data loss, while ESS and distributional diagnostics are computed only on uncensored observations to assess positivity where treatment was actually observed.",
      ""
    )
  }

  # helpers ----------------------------------------------------------------
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

    # compute metrics per wave using uncensored observations (r > 0)
    wave_metrics <- lapply(seq_along(col_idx), function(i) {
      w <- sub_dr[, i]
      w_all <- w[!is.na(w)]
      n_all <- length(w_all)
      n_zero <- sum(w_all == 0)
      prop_censored <- if (n_all > 0) n_zero / n_all else NA_real_

      # uncensored subset
      w_pos <- w_all[w_all > 0]
      n_pos <- length(w_pos)

      # uncensored ESS
      ess_pos <- if (n_pos > 0) {
        sum_w <- sum(w_pos)
        sum_w2 <- sum(w_pos^2)
        if (sum_w2 > 0) (sum_w^2) / sum_w2 else NA_real_
      } else NA_real_

      ess_pos_frac <- if (n_pos > 0) ess_pos / n_pos else NA_real_

      list(
        wave = col_idx[i],
        n_all = n_all,
        n_pos = n_pos,
        prop_censored = prop_censored,
        ess_pos = ess_pos,
        ess_pos_frac = ess_pos_frac
      )
    })

    wave_df <- do.call(rbind, lapply(wave_metrics, function(m) {
      data.frame(
        shift_full = shift_full,
        shift_clean = shift_clean,
        wave = m$wave,
        n_all = m$n_all,
        n_pos = m$n_pos,
        prop_censored = m$prop_censored,
        ess_pos = m$ess_pos,
        ess_pos_frac = m$ess_pos_frac,
        stringsAsFactors = FALSE
      )
    }))

    # overall metrics across waves (uncensored)
    all_w <- as.vector(sub_dr)
    all_w <- all_w[!is.na(all_w)]
    overall_n_all <- length(all_w)
    overall_n_zero <- sum(all_w == 0)
    overall_prop_censored <- if (overall_n_all > 0) overall_n_zero / overall_n_all else NA_real_

    all_w_pos <- all_w[all_w > 0]
    overall_n_pos <- length(all_w_pos)

    overall_ess_pos <- if (overall_n_pos > 0) {
      sum_w <- sum(all_w_pos)
      sum_w2 <- sum(all_w_pos^2)
      if (sum_w2 > 0) (sum_w^2) / sum_w2 else NA_real_
    } else NA_real_

    overall_ess_pos_frac <- if (overall_n_pos > 0) overall_ess_pos / overall_n_pos else NA_real_

    list(
      waves = wave_df,
      overall = data.frame(
        shift_full = shift_full,
        shift_clean = shift_clean,
        n_all = overall_n_all,
        n_pos = overall_n_pos,
        prop_censored = overall_prop_censored,
        ess_pos = overall_ess_pos,
        ess_pos_frac = overall_ess_pos_frac,
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

  # diagnostics helper -----------------------------------------------------
  diagnostics_text <- function() {
    if (!isTRUE(include_diagnostics)) return(character(0))

    # call margot_lmtp_positivity() to get detailed diagnostics
    # note: explicitly rebuild dataframe structure to ensure we have updated columns
    pos_result <- tryCatch({
      result <- margot_lmtp_positivity(x, verbose = FALSE)
      # verify required columns exist
      if (!is.null(result$by_wave)) {
        required <- c("min_pos", "max_pos", "mean_pos", "sd_pos", "cv_pos")
        if (!all(required %in% names(result$by_wave))) {
          warning("margot_lmtp_positivity() missing *_pos columns. Detailed diagnostics unavailable.")
          return(NULL)
        }
      }
      result
    }, error = function(e) {
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_warning("Could not compute detailed diagnostics: {e$message}")
      }
      return(NULL)
    })

    if (is.null(pos_result)) return(character(0))

    # filter to selected outcome and shifts
    by_wave <- pos_result$by_wave
    by_wave <- by_wave[by_wave$outcome == outcome, , drop = FALSE]
    if (!is.null(shifts)) {
      keep <- by_wave$shift %in% shifts |
              sapply(by_wave$shift, function(s) {
                clean <- if (startsWith(s, paste0(outcome, "_"))) {
                  substring(s, nchar(outcome) + 2L)
                } else s
                clean %in% shifts
              })
      by_wave <- by_wave[keep, , drop = FALSE]
    }
    if (!is.null(waves)) {
      by_wave <- by_wave[by_wave$wave %in% waves, , drop = FALSE]
    }
    if (!is.null(remove_waves)) {
      by_wave <- by_wave[!(by_wave$wave %in% remove_waves), , drop = FALSE]
    }

    if (!nrow(by_wave)) return(character(0))

    # build detailed text per shift
    diag_lines <- character(0)
    for (shift_name in names(shift_results)) {
      shift_full <- shift_results[[shift_name]]$waves$shift_full[1]
      shift_data <- by_wave[by_wave$shift == shift_full, , drop = FALSE]
      if (!nrow(shift_data)) next

      # aggregate across waves for shift-level summary
      zeros_pct <- mean(shift_data$prop_zero, na.rm = TRUE) * 100

      # use uncensored (_pos) metrics for distributional diagnostics
      range_min_pos <- min(shift_data$min_pos, na.rm = TRUE)
      range_max_pos <- max(shift_data$max_pos, na.rm = TRUE)
      mean_val_pos <- mean(shift_data$mean_pos, na.rm = TRUE)
      sd_val_pos <- mean(shift_data$sd_pos, na.rm = TRUE)
      cv_val_pos <- mean(shift_data$cv_pos, na.rm = TRUE)

      # tail probabilities (average across waves) - uncensored only
      tail_cols <- grep("^p_gt_.*_pos$", names(shift_data), value = TRUE)
      tail_vals <- if (length(tail_cols)) {
        sapply(tail_cols, function(col) mean(shift_data[[col]], na.rm = TRUE) * 100)
      } else NULL

      # quantiles (average across waves) - uncensored only
      quant_cols <- grep("^q.*_pos$", names(shift_data), value = TRUE)
      quants <- if (length(quant_cols)) {
        sapply(quant_cols, function(col) mean(shift_data[[col]], na.rm = TRUE))
      } else NULL

      shift_label <- shift_results[[shift_name]]$label

      diag_lines <- c(diag_lines,
                      paste0("### ", shift_label, " — Detailed Diagnostics (Uncensored)"),
                      paste0("- **Censoring rate**: ", fmt_frac(zeros_pct), "%"),
                      paste0("- **Range**: [", fmt_frac(range_min_pos), ", ", fmt_frac(range_max_pos), "]"),
                      paste0("- **Mean ± SD**: ", fmt_frac(mean_val_pos), " ± ", fmt_frac(sd_val_pos),
                             " (CV = ", fmt_frac(cv_val_pos), ")"))

      if (!is.null(tail_vals) && length(tail_vals)) {
        # clean up column names for display
        names_clean <- gsub("^p_gt_", "", gsub("_pos$", "", names(tail_vals)))
        tail_text <- paste(paste0("P(r > ", names_clean, ")"), "=", paste0(fmt_frac(tail_vals), "%"), collapse = ", ")
        diag_lines <- c(diag_lines, paste0("- **Tails**: ", tail_text))
      }

      if (!is.null(quants) && length(quants) >= 4) {
        # show key quantiles: 1st, 5th, 95th, 99.9th if available
        q_labels <- c("q0001_pos" = "p0.1%", "q001_pos" = "p1%", "q005_pos" = "p5%", "q05_pos" = "p50%",
                      "q095_pos" = "p95%", "q0999_pos" = "p99.9%")
        available_q <- intersect(names(quants), names(q_labels))
        if (length(available_q)) {
          q_text <- paste(q_labels[available_q], "=", fmt_frac(quants[available_q]), collapse = ", ")
          diag_lines <- c(diag_lines, paste0("- **Quantiles**: ", q_text))
        }
      }

      diag_lines <- c(diag_lines, "")
    }

    if (length(diag_lines)) {
      c("", "## Detailed Diagnostics by Shift", "", diag_lines)
    } else {
      character(0)
    }
  }

  # Reference counts from the first shift for baseline N / person-time
  ref_waves <- shift_results[[1]]$waves
  baseline_n <- if (nrow(ref_waves)) max(ref_waves$n_all, na.rm = TRUE) else NA_real_
  person_time <- if (nrow(ref_waves)) sum(ref_waves$n_all, na.rm = TRUE) else NA_real_

  outcome_label <- map_label(outcome)

  header_bits <- c(
    paste0("LMTP positivity diagnostics for ", outcome_label, " (uncensored observations)."),
    if (is.finite(baseline_n)) paste0("Baseline N $\\approx$ ", fmt_int(baseline_n), ".") else "",
    if (is.finite(person_time)) paste0("Person-time rows per shift (selected waves) = ", fmt_int(person_time), ".") else ""
  )
  header <- paste(header_bits[nzchar(header_bits)], collapse = " ")

  # overview lines removed - averaging across interventions is incoherent
  overview_lines <- character(0)

  # per-shift bullet lines -------------------------------------------------
  lines <- vapply(names(shift_results), function(name) {
    res <- shift_results[[name]]
    wave_df <- res$waves
    overall_df <- res$overall
    if (!nrow(wave_df)) return("")

    # censoring rate (shown once per shift)
    cens_pct <- fmt_frac(overall_df$prop_censored * 100)
    cens_clause <- paste0("censoring = ", cens_pct, "%")

    # uncensored ESS by wave
    wave_bits <- paste0(
      wave_df$wave, ": ", fmt_int(wave_df$ess_pos),
      " (ESS/N = ", fmt_frac(wave_df$ess_pos_frac), ")"
    )
    wave_clause <- paste(wave_bits, collapse = ", ")

    # overall uncensored ESS
    overall_clause <- ifelse(
      is.finite(overall_df$ess_pos),
      paste0("; overall ESS = ", fmt_int(overall_df$ess_pos),
             " (ESS/N_pt = ", fmt_frac(overall_df$ess_pos_frac), ")"),
      ""
    )

    paste0("- ", res$label, ": ", cens_clause, "; uncensored ESS by wave — ", wave_clause, overall_clause)
  }, character(1))
  lines <- lines[nzchar(lines)]

  # assemble final output with optional methods and diagnostics
  methods_section <- if (isTRUE(include_methods)) methods_text() else character(0)
  diagnostics_section <- diagnostics_text()  # returns empty vector if include_diagnostics = FALSE

  text_lines <- c(methods_section, header, overview_lines, lines, diagnostics_section)
  text <- paste(text_lines, collapse = "\n")

  if (identical(return, "text")) {
    text
  } else {
    list(
      text = text,
      methods = if (isTRUE(include_methods)) paste(methods_section, collapse = "\n") else NULL,
      header = header,
      overview = overview_lines,
      lines = lines,
      diagnostics = if (isTRUE(include_diagnostics)) paste(diagnostics_section, collapse = "\n") else NULL,
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
