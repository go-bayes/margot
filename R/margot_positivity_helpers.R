margot_normalise_positivity_models <- function(x, caller) {
  if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
    return(x$models)
  }
  if (is.list(x) && !is.null(x$density_ratios)) {
    return(list(`(outcome)` = list(`(shift)` = x)))
  }
  if (is.numeric(x)) {
    fake <- list(density_ratios = x)
    return(list(`(outcome)` = list(`(shift)` = fake)))
  }
  if (is.list(x) && length(x) &&
      all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
    return(list(`(outcome)` = x))
  }
  stop(
    "Unsupported input to `", caller, "()`.",
    " Pass a margot_lmtp() result, a single LMTP model,",
    " or a list with $density_ratios."
  )
}

margot_clean_positivity_shift <- function(name, outcome) {
  if (!is.character(name) || !length(name)) {
    return(name)
  }
  if (!is.null(outcome)) {
    prefix <- paste0(outcome, "_")
    if (startsWith(name, prefix)) {
      return(substring(name, nchar(prefix) + 1L))
    }
  }
  name
}

margot_pretty_positivity_shift <- function(shift_name,
                                           label_mapping = NULL,
                                           outcome = NULL) {
  lbl <- margot_clean_positivity_shift(shift_name, outcome = outcome)
  lbl <- as.character(lbl %||% "")
  if (!nzchar(lbl) || is.na(lbl)) {
    return("")
  }
  if (identical(tolower(lbl), "null")) {
    return("Observed (Censoring-Adjusted)")
  }
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
    out <- as.character(out %||% lbl)
    if (nzchar(out) && !is.na(out)) {
      return(out)
    }
  }
  gsub("_", " ", tools::toTitleCase(lbl))
}

margot_order_positivity_shifts <- function(shift_df) {
  if (is.null(shift_df) || !nrow(shift_df)) {
    return(shift_df)
  }
  pref_order <- c("null", "shift_down", "shift_up")
  ord <- c(
    intersect(pref_order, shift_df$shift_clean),
    setdiff(shift_df$shift_clean, pref_order)
  )
  shift_df[match(ord, shift_df$shift_clean), , drop = FALSE]
}

margot_resolve_positivity_selection <- function(x,
                                                outcome = NULL,
                                                shifts = NULL,
                                                caller = "margot_positivity_report") {
  models_nested <- margot_normalise_positivity_models(x, caller = caller)
  outcome_names <- names(models_nested) %||% character()
  if (!length(outcome_names)) {
    stop("No outcomes found in the supplied object.")
  }

  if (is.null(outcome)) {
    outcome <- outcome_names[[1]]
  } else {
    if (!is.character(outcome) || length(outcome) != 1L) {
      stop("`outcome` must be NULL or a single character string.")
    }
    if (!(outcome %in% outcome_names)) {
      stop("Outcome not found in models: ", outcome)
    }
  }

  outcome_models <- models_nested[[outcome]]
  if (!length(outcome_models)) {
    return(list(
      models_nested = models_nested,
      outcome = outcome,
      outcome_models = outcome_models,
      shift_df = data.frame(
        shift_full = character(),
        shift_clean = character(),
        stringsAsFactors = FALSE
      )
    ))
  }

  shift_full <- names(outcome_models)
  if (is.null(shift_full) || !length(shift_full)) {
    shift_full <- paste0("shift_", seq_along(outcome_models))
    names(outcome_models) <- shift_full
  } else {
    missing_names <- which(!nzchar(shift_full))
    if (length(missing_names)) {
      for (idx in missing_names) {
        shift_full[[idx]] <- paste0("shift_", idx)
      }
      names(outcome_models) <- shift_full
    }
  }

  shift_df <- data.frame(
    shift_full = shift_full,
    stringsAsFactors = FALSE
  )
  shift_df$shift_clean <- vapply(
    shift_df$shift_full,
    margot_clean_positivity_shift,
    character(1),
    outcome = outcome
  )

  if (is.null(shifts)) {
    shift_df <- margot_order_positivity_shifts(shift_df)
  } else {
    if (!is.character(shifts)) {
      stop("`shifts` must be NULL or a character vector.")
    }
    sel_idx <- integer(0)
    missing <- character(0)
    for (sh in shifts) {
      hit <- which(shift_df$shift_full == sh | shift_df$shift_clean == sh)
      if (!length(hit)) {
        missing <- c(missing, sh)
      } else {
        sel_idx <- c(sel_idx, hit[1])
      }
    }
    if (length(missing)) {
      stop(
        "Requested shifts not found for outcome ", outcome, ": ",
        paste(unique(missing), collapse = ", ")
      )
    }
    shift_df <- shift_df[sel_idx, , drop = FALSE]
  }

  list(
    models_nested = models_nested,
    outcome = outcome,
    outcome_models = outcome_models,
    shift_df = shift_df
  )
}

margot_positivity_thresholds <- function(test_thresholds = NULL) {
  thr <- list(
    prod_log10 = -1,
    prod_frac_ok = 0.05,
    prod_frac_warn = 0.20,
    near_zero_median = 1e-3,
    near_zero_cv = 0.05
  )
  if (is.list(test_thresholds) && length(test_thresholds)) {
    for (nm in intersect(names(test_thresholds), names(thr))) {
      thr[[nm]] <- test_thresholds[[nm]]
    }
  }

  band <- abs(as.numeric(thr$prod_log10)[1])
  if (!is.finite(band) || band <= 0) {
    band <- 1
  }
  thr$prod_log10 <- -band

  thr$prod_frac_ok <- as.numeric(thr$prod_frac_ok)[1]
  thr$prod_frac_warn <- as.numeric(thr$prod_frac_warn)[1]
  if (!is.finite(thr$prod_frac_ok) || thr$prod_frac_ok < 0) {
    thr$prod_frac_ok <- 0.05
  }
  if (!is.finite(thr$prod_frac_warn) || thr$prod_frac_warn < 0) {
    thr$prod_frac_warn <- 0.20
  }
  thr$prod_frac_ok <- min(thr$prod_frac_ok, 1)
  thr$prod_frac_warn <- min(thr$prod_frac_warn, 1)
  if (thr$prod_frac_ok > thr$prod_frac_warn) {
    thr$prod_frac_ok <- thr$prod_frac_warn
  }

  thr$near_zero_median <- as.numeric(thr$near_zero_median)[1]
  if (!is.finite(thr$near_zero_median) || thr$near_zero_median <= 0) {
    thr$near_zero_median <- 1e-3
  }

  thr$near_zero_cv <- as.numeric(thr$near_zero_cv)[1]
  if (!is.finite(thr$near_zero_cv) || thr$near_zero_cv <= 0) {
    thr$near_zero_cv <- 0.05
  }

  thr
}

margot_positivity_band_strings <- function(thresholds) {
  band <- abs(as.numeric(thresholds$prod_log10)[1])
  if (!is.finite(band) || band <= 0) {
    band <- 1
  }

  fmt <- function(x) {
    if (!is.finite(x)) {
      return(NA_character_)
    }
    if (exists("format_minimal_decimals", mode = "function")) {
      return(format_minimal_decimals(x, max_decimals = 4))
    }
    format(signif(x, 4), trim = TRUE, scientific = FALSE)
  }

  lower <- 10^(-band)
  upper <- 10^(band)

  list(
    band = band,
    lower = lower,
    upper = upper,
    lower_label = fmt(lower),
    upper_label = fmt(upper),
    interval_label = paste0("[", fmt(lower), ", ", fmt(upper), "]"),
    interval_math = paste0("[10^{-", band, "}, 10^{", band, "}]")
  )
}

margot_positivity_product_metrics <- function(dr, cols, thresholds) {
  if (!length(cols)) {
    return(list(
      prop_zero_prod = NA_real_,
      prod_frac_below = NA_real_,
      prod_frac_above = NA_real_,
      prod_frac_outside = NA_real_,
      log10_prod_all = numeric(),
      log10_prod_uncensored = numeric()
    ))
  }

  band <- abs(as.numeric(thresholds$prod_log10)[1])
  if (!is.finite(band) || band <= 0) {
    band <- 1
  }

  sub <- dr[, cols, drop = FALSE]
  log10_prod_all <- apply(sub, 1L, function(row) {
    row <- row[is.finite(row)]
    if (!length(row)) {
      return(NA_real_)
    }
    if (any(row == 0)) {
      return(-Inf)
    }
    row <- row[row > 0]
    if (!length(row)) {
      return(NA_real_)
    }
    sum(log10(row))
  })
  log10_prod_uncensored <- apply(sub, 1L, function(row) {
    row <- row[is.finite(row) & row > 0]
    if (!length(row)) {
      return(NA_real_)
    }
    sum(log10(row))
  })

  observed_all <- is.finite(log10_prod_all) | is.infinite(log10_prod_all)
  observed_uncensored <- is.finite(log10_prod_uncensored)

  prop_zero_prod <- if (any(observed_all)) {
    mean((is.infinite(log10_prod_all) & log10_prod_all < 0)[observed_all])
  } else {
    NA_real_
  }
  prod_frac_below <- if (any(observed_uncensored)) {
    mean(log10_prod_uncensored[observed_uncensored] < (-band))
  } else {
    NA_real_
  }
  prod_frac_above <- if (any(observed_uncensored)) {
    mean(log10_prod_uncensored[observed_uncensored] > band)
  } else {
    NA_real_
  }
  prod_frac_outside <- if (any(observed_uncensored)) {
    mean(abs(log10_prod_uncensored[observed_uncensored]) > band)
  } else {
    NA_real_
  }

  list(
    prop_zero_prod = prop_zero_prod,
    prod_frac_below = prod_frac_below,
    prod_frac_above = prod_frac_above,
    prod_frac_outside = prod_frac_outside,
    log10_prod_all = log10_prod_all,
    log10_prod_uncensored = log10_prod_uncensored
  )
}

margot_positivity_support_status <- function(prod_frac_outside, thresholds) {
  if (!is.finite(prod_frac_outside)) {
    return("Unknown")
  }
  if (prod_frac_outside <= thresholds$prod_frac_ok) {
    return("Adequate")
  }
  if (prod_frac_outside <= thresholds$prod_frac_warn) {
    return("Caution")
  }
  "Limited"
}
