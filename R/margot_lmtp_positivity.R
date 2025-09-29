#' Summarize positivity via density ratios for LMTP fits
#'
#' Computes by-wave and overall summaries of density ratios, including
#' zeros, extreme quantiles, tail mass above thresholds, and effective sample size (ESS).
#'
#' @param x Either:
#'   - the full result of `margot_lmtp()` (list with $models),
#'   - a single LMTP model fit (must have $density_ratios),
#'   - or a numeric vector/matrix of density ratios.
#' @param thresholds Numeric vector of tail thresholds to report (P(ratio > threshold)).
#' @param probs Quantiles to report (must include 0.5 if you want the median).
#' @param ess_warn Flag when ESS/N is below this fraction (per wave).
#' @param zero_warn Flag when proportion of exact zeros exceeds this fraction.
#' @param tail_warn Named numeric vector giving tail-mass flag thresholds
#'   (names must match thresholds). If length 1, recycled to all thresholds.
#' @param include_overall If TRUE, also compute an overall summary pooling all waves.
#' @param digits Optional integer for rounding in the returned data frames (NULL = no rounding).
#' @param verbose If TRUE, prints concise CLI messages when flags are raised.
#'
#' @return A list with:
#'   \item{by_wave}{data.frame of per-wave summaries (one row per outcome/shift/wave).}
#'   \item{overall}{data.frame of pooled summaries across waves (one row per outcome/shift).}
#'   \item{flags}{data.frame of flagged issues (subset of rows from by_wave/overall with reasons).}
#'
#' @examples
#' # Single model:
#' # pos <- margot_lmtp_positivity(fit$models$outcome$shift_model)
#' #
#' # Entire run from margot_lmtp():
#' # pos <- margot_lmtp_positivity(fit)
#' # head(pos$by_wave); head(pos$flags)
#'
#' @export
margot_lmtp_positivity <- function(
    x,
    thresholds = c(10, 25, 50, 100),
    probs = c(0.001, 0.01, 0.05, 0.50, 0.95, 0.999),
    ess_warn  = 0.50,
    zero_warn = 0.01,
    tail_warn = c(`10` = 0.05, `25` = 0.02, `50` = 0.01, `100` = 0.005),
    include_overall = TRUE,
    digits = NULL,
    verbose = TRUE
) {
  # local helpers --------------------------------------------------------------
  .finite <- function(v) v[is.finite(v)]
  .q      <- function(v, p) as.numeric(stats::quantile(v, probs = p, names = FALSE, na.rm = TRUE))
  .ess    <- function(w) {
    w <- .finite(w); if (!length(w)) return(NA_real_)
    s1 <- sum(w); s2 <- sum(w^2); if (s2 == 0) return(NA_real_)
    (s1^2) / s2
  }
  .summ_one <- function(w, wave_idx = NA_integer_, thresholds, probs) {
    w <- .finite(as.numeric(w))
    n  <- length(w)
    if (!n) {
      out <- data.frame(wave = wave_idx, n = 0, prop_zero = NA_real_,
                        min = NA_real_, max = NA_real_, mean = NA_real_, sd = NA_real_,
                        cv = NA_real_, ess = NA_real_, ess_frac = NA_real_,
                        n_pos = NA_real_, prop_nonzero = NA_real_,
                        ess_pos = NA_real_, ess_pos_frac = NA_real_)
      # add quantiles & tails with NA (all and positive-only)
      qn <- paste0("q", gsub("\\.", "", sprintf("%g", probs)))
      qs <- setNames(rep(NA_real_, length(probs)), qn)
      qs_pos <- setNames(rep(NA_real_, length(probs)), paste0(qn, "_pos"))
      tails <- setNames(rep(NA_real_, length(thresholds)), paste0("p_gt_", thresholds))
      tails_pos <- setNames(rep(NA_real_, length(thresholds)), paste0("p_gt_", thresholds, "_pos"))
      return(cbind(out,
                   as.data.frame(as.list(qs)),
                   as.data.frame(as.list(tails)),
                   as.data.frame(as.list(qs_pos)),
                   as.data.frame(as.list(tails_pos))))
    }

    # basic moments
    mn <- mean(w); sdv <- stats::sd(w)
    cv <- if (isTRUE(all.equal(mn, 0))) NA_real_ else sdv / mn
    qn <- paste0("q", gsub("\\.", "", sprintf("%g", probs)))
    qv <- setNames(.q(w, probs), qn)
    tails <- sapply(thresholds, function(t) mean(w > t))
    names(tails) <- paste0("p_gt_", thresholds)

    ess <- .ess(w)
    ess_frac <- ess / n

    # positive-only (uncensored) diagnostics
    w_pos <- w[w > 0]
    n_pos <- length(w_pos)
    prop_nonzero <- if (n > 0) n_pos / n else NA_real_
    ess_pos <- if (n_pos > 0) .ess(w_pos) else NA_real_
    ess_pos_frac <- if (n_pos > 0) ess_pos / n_pos else NA_real_

    # positive-only quantiles and tails
    if (n_pos > 0) {
      qv_pos <- setNames(.q(w_pos, probs), paste0(qn, "_pos"))
      tails_pos <- setNames(sapply(thresholds, function(t) mean(w_pos > t)), paste0("p_gt_", thresholds, "_pos"))
    } else {
      qv_pos <- setNames(rep(NA_real_, length(probs)), paste0(qn, "_pos"))
      tails_pos <- setNames(rep(NA_real_, length(thresholds)), paste0("p_gt_", thresholds, "_pos"))
    }

    data.frame(
      wave = wave_idx,
      n = n,
      prop_zero = mean(w == 0),
      min = min(w),
      max = max(w),
      mean = mn,
      sd = sdv,
      cv = cv,
      t(qv),
      as.data.frame(as.list(tails)),
      t(qv_pos),
      as.data.frame(as.list(tails_pos)),
      ess = ess,
      ess_frac = ess_frac,
      n_pos = n_pos,
      prop_nonzero = prop_nonzero,
      ess_pos = ess_pos,
      ess_pos_frac = ess_pos_frac,
      row.names = NULL,
      check.names = FALSE
    )
  }

  .as_df <- function(x) if (isTRUE(requireNamespace("tibble", quietly = TRUE))) tibble::as_tibble(x) else x

  .flag_rows <- function(df, thresholds, ess_warn, zero_warn, tail_warn) {
    if (!nrow(df)) return(df)

    # Tail warn vector handling
    if (length(tail_warn) == 1L) {
      tail_warn <- setNames(rep(tail_warn, length(thresholds)), thresholds)
    }
    # Ensure names match thresholds
    if (!all(as.character(thresholds) %in% names(tail_warn))) {
      stop("`tail_warn` must be named with the same values as `thresholds`.")
    }

    flags <- list()

    # zeros
    if ("prop_zero" %in% names(df)) {
      idx <- which(!is.na(df$prop_zero) & df$prop_zero > zero_warn)
      if (length(idx)) flags[["zeros"]] <- data.frame(idx = idx, reason = sprintf("prop_zero > %.3f", zero_warn))
    }

    # ESS (all weights)
    if ("ess_frac" %in% names(df)) {
      idx <- which(!is.na(df$ess_frac) & df$ess_frac < ess_warn)
      if (length(idx)) flags[["ess"]] <- data.frame(idx = idx, reason = sprintf("ESS/N < %.2f", ess_warn))
    }

    # ESS among positive weights only
    if ("ess_pos_frac" %in% names(df)) {
      idx <- which(!is.na(df$ess_pos_frac) & df$ess_pos_frac < ess_warn)
      if (length(idx)) flags[["ess_pos"]] <- data.frame(idx = idx, reason = sprintf("ESS+/(N+) < %.2f", ess_warn))
    }

    # tails
    for (t in thresholds) {
      col <- paste0("p_gt_", t)
      thr <- tail_warn[as.character(t)]
      if (col %in% names(df)) {
        idx <- which(!is.na(df[[col]]) & df[[col]] > thr)
        if (length(idx)) {
          flags[[paste0("tail_", t)]] <- data.frame(idx = idx, reason = sprintf("P(ratio > %g) > %.3f", t, thr))
        }
      }
    }

    # tails among positive-only weights
    for (t in thresholds) {
      col <- paste0("p_gt_", t, "_pos")
      thr <- tail_warn[as.character(t)]
      if (col %in% names(df)) {
        idx <- which(!is.na(df[[col]]) & df[[col]] > thr)
        if (length(idx)) {
          flags[[paste0("tail_pos_", t)]] <- data.frame(idx = idx, reason = sprintf("P+(ratio > %g) > %.3f", t, thr))
        }
      }
    }

    if (!length(flags)) return(df[0, ])

    idx_all <- do.call(rbind, flags)
    flagged <- df[idx_all$idx, , drop = FALSE]
    flagged$flag_reason <- idx_all$reason
    flagged
  }

  # normalize input to a nested list: outcome -> shift -> model ----------------
  models <- NULL

  # case 1: complete_output list from margot_lmtp()
  if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
    models <- x$models
  } else if (is.list(x) && !is.null(x$density_ratios)) {
    # case 2: a single model
    models <- list(`(outcome)` = list(`(model)` = x))
  } else if (is.numeric(x)) {
    # case 3: raw vector/matrix of density ratios
    fake <- list(density_ratios = x)
    models <- list(`(outcome)` = list(`(model)` = fake))
  } else if (is.list(x) && all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
    # case 4: list of models
    models <- list(`(outcome)` = x)
  } else {
    stop("Unsupported input to `margot_lmtp_positivity()`. Pass a margot_lmtp() result, a single LMTP model, or a numeric vector/matrix of density ratios.")
  }

  # iterate and summarize ------------------------------------------------------
  by_wave_rows <- list()
  overall_rows <- list()

  for (outcome in names(models)) {
    shifts <- models[[outcome]]
    for (shift_name in names(shifts)) {
      mod <- shifts[[shift_name]]
      dr  <- mod$density_ratios
      if (is.null(dr)) next

      # handle vector or matrix of density ratios (waves in columns if matrix)
      if (is.matrix(dr)) {
        cols <- seq_len(ncol(dr))
        wave_tabs <- lapply(cols, function(j) .summ_one(dr[, j], wave_idx = j, thresholds = thresholds, probs = probs))
      } else {
        wave_tabs <- list(.summ_one(dr, wave_idx = 1L, thresholds = thresholds, probs = probs))
      }

      wave_df <- do.call(rbind, wave_tabs)
      wave_df$outcome <- outcome
      wave_df$shift   <- shift_name

      # reorder columns nicely
      left  <- c("outcome", "shift", "wave", "n")
      qcols <- paste0("q", gsub("\\.", "", sprintf("%g", probs)))
      qcols_pos <- paste0(qcols, "_pos")
      mids1 <- c("prop_zero", "min", qcols, qcols_pos, "median", "max")
      # ensure "median" is present and not duplicated
      if (!"median" %in% mids1) mids1 <- c(mids1, "median")
      mids2 <- c("mean", "sd", "cv")
      tails <- paste0("p_gt_", thresholds)
      tails_pos <- paste0("p_gt_", thresholds, "_pos")
      # include positive-only diagnostics in the kept columns
      right <- c("ess", "ess_frac", "n_pos", "prop_nonzero", "ess_pos", "ess_pos_frac")
      keep  <- unique(c(left,
                        mids1[mids1 %in% names(wave_df)],
                        mids2,
                        tails[tails %in% names(wave_df)],
                        tails_pos[tails_pos %in% names(wave_df)],
                        right))
      wave_df <- wave_df[, intersect(keep, names(wave_df)), drop = FALSE]

      by_wave_rows[[length(by_wave_rows) + 1]] <- wave_df

      if (include_overall) {
        # pool across waves by stacking all ratios and re-summarizing
        if (is.matrix(dr)) {
          pooled <- as.numeric(dr)
        } else {
          pooled <- as.numeric(dr)
        }
        ov <- .summ_one(pooled, wave_idx = NA_integer_, thresholds = thresholds, probs = probs)
        ov$outcome <- outcome
        ov$shift   <- shift_name
        ov$wave    <- "overall"
        overall_rows[[length(overall_rows) + 1]] <- ov[, names(wave_df), drop = FALSE]
      }
    }
  }

  by_wave <- if (length(by_wave_rows)) do.call(rbind, by_wave_rows) else by_wave_rows
  overall <- if (include_overall && length(overall_rows)) do.call(rbind, overall_rows) else data.frame()

  # flags
  flags_wave <- if (length(by_wave_rows)) .flag_rows(by_wave, thresholds, ess_warn, zero_warn, tail_warn) else data.frame()
  flags_ov   <- if (nrow(overall))        .flag_rows(overall, thresholds, ess_warn, zero_warn, tail_warn) else data.frame()
  flags <- rbind(flags_wave, flags_ov)

  # rounding (optional)
  if (!is.null(digits) && (nrow(as.data.frame(by_wave)) > 0)) {
    num_cols <- vapply(by_wave, is.numeric, logical(1))
    by_wave[num_cols] <- lapply(by_wave[num_cols], round, digits = digits)
    if (nrow(overall)) {
      num_cols <- vapply(overall, is.numeric, logical(1))
      overall[num_cols] <- lapply(overall[num_cols], round, digits = digits)
    }
    if (nrow(flags)) {
      num_cols <- vapply(flags, is.numeric, logical(1))
      flags[num_cols] <- lapply(flags[num_cols], round, digits = digits)
    }
  }

  # optional CLI alerts on flags
  if (verbose && nrow(flags)) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_warning("Positivity flags detected ({nrow(flags)} rows). Examples:")
      utils::capture.output(print(utils::head(flags, 5)))
    }
  }

  # tibble if available
  out <- list(by_wave = .as_df(by_wave), overall = .as_df(overall), flags = .as_df(flags))
  class(out) <- c("margot_lmtp_positivity", class(out))
  out
}
