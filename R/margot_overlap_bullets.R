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
      "Density ratios act as weights in the estimator to rebalance the observed data to mimic the distribution under the modified policy. A value $r_t > 1$ indicates that the observed treatment $A_t$ is more likely under the modified policy than under the observed mechanism, so the observation is up-weighted at that time point. Conversely, $r_t < 1$ down-weights it, and $r_t = 0$ means the observed treatment is incompatible with the policy (e.g., effectively excluding that trajectory).",
      "",
      "Examining the distribution of density ratios allows us to assess practical positivity. Many values near 0 or extremely large suggest violations of the positivity assumption, where certain treatments are improbable under the policy, leading to unstable estimates.",
      "",
      "In a single-time-point average treatment effect (ATE) where the policy sets treatment to 1, the density ratio reduces to the inverse probability weight $1 / \\Pr(A=1 \\mid W)$ for treated units (and 0 for untreated). Where exposures are repeated, the product of ratios across time points gives the overall weight for each observation.",
      "",
      "**Effective Sample Size (ESS)** summarises the effective information in weighted data using the formula: $\\text{ESS} = (\\sum w)^2 / \\sum w^2$. Higher variance in weights reduces ESS. When all weights are equal, $\\text{ESS} = N$ (the actual sample size). $\\text{ESS}/N$ gives the proportion of effective information retained.",
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

  # diagnostics helper -----------------------------------------------------
  diagnostics_text <- function() {
    if (!isTRUE(include_diagnostics)) return(character(0))

    # call margot_lmtp_positivity() to get detailed diagnostics
    pos_result <- tryCatch({
      margot_lmtp_positivity(x, verbose = FALSE)
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
      range_min <- min(shift_data$min, na.rm = TRUE)
      range_max <- max(shift_data$max, na.rm = TRUE)
      mean_val <- mean(shift_data$mean, na.rm = TRUE)
      sd_val <- mean(shift_data$sd, na.rm = TRUE)
      cv_val <- mean(shift_data$cv, na.rm = TRUE)

      # tail probabilities (average across waves)
      tail_cols <- grep("^p_gt_", names(shift_data), value = TRUE)
      tail_cols <- setdiff(tail_cols, grep("_pos$", tail_cols, value = TRUE))
      tail_vals <- if (length(tail_cols)) {
        sapply(tail_cols, function(col) mean(shift_data[[col]], na.rm = TRUE) * 100)
      } else NULL

      # quantiles (average across waves)
      quant_cols <- grep("^q", names(shift_data), value = TRUE)
      quant_cols <- setdiff(quant_cols, grep("_pos$", quant_cols, value = TRUE))
      quants <- if (length(quant_cols)) {
        sapply(quant_cols, function(col) mean(shift_data[[col]], na.rm = TRUE))
      } else NULL

      shift_label <- shift_results[[shift_name]]$label

      diag_lines <- c(diag_lines,
                      paste0("### ", shift_label, " — Detailed Diagnostics"),
                      paste0("- **Zeros**: ", fmt_frac(zeros_pct), "%"),
                      paste0("- **Range**: [", fmt_frac(range_min), ", ", fmt_frac(range_max), "]"),
                      paste0("- **Mean ± SD**: ", fmt_frac(mean_val), " ± ", fmt_frac(sd_val),
                             " (CV = ", fmt_frac(cv_val), ")"))

      if (!is.null(tail_vals) && length(tail_vals)) {
        tail_text <- paste(names(tail_vals), "=", paste0(fmt_frac(tail_vals), "%"), collapse = ", ")
        diag_lines <- c(diag_lines, paste0("- **Tails**: ", gsub("p_gt_", "P(r > ", gsub("=", ") = ", tail_text))))
      }

      if (!is.null(quants) && length(quants) >= 4) {
        # show key quantiles: 1st, 5th, 95th, 99.9th if available
        q_labels <- c("q0001" = "p0.1%", "q001" = "p1%", "q005" = "p5%", "q05" = "p50%",
                      "q095" = "p95%", "q0999" = "p99.9%")
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
  baseline_n <- if (nrow(ref_waves)) max(ref_waves$n, na.rm = TRUE) else NA_real_
  person_time <- if (nrow(ref_waves)) sum(ref_waves$n, na.rm = TRUE) else NA_real_

  outcome_label <- map_label(outcome)

  header_bits <- c(
    paste0("LMTP positivity for ", outcome_label, "."),
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
