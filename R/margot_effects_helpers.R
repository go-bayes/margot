# internal helpers for margot_lmtp_effects(), margot_plot_effects() and
# margot_save_effects_plot(). sizes are in millimetres and type sizes in points.

.margot_effects_state <- new.env(parent = emptyenv())

# sans-serif family, in journal order of preference; warns once per session when
# arial is unavailable, because text metrics and so the layout depend on the font.
.margot_effects_font <- function(family = NULL) {
  candidates <- if (is.null(family)) c("Arial", "Helvetica", "Liberation Sans", "DejaVu Sans") else family
  for (f in candidates) {
    path <- systemfonts::match_fonts(f)$path
    if (grepl(gsub(" ", "", f), gsub(" ", "", basename(path)), ignore.case = TRUE)) {
      if (is.null(family) && f != "Arial" && is.null(.margot_effects_state$font_warned)) {
        warning("Arial not found; using ", f, ". Install Arial for journal output.", call. = FALSE)
        .margot_effects_state$font_warned <- TRUE
      }
      return(f)
    }
  }
  if (!is.null(family)) stop("font family not found: ", family, call. = FALSE)
  stop("no sans-serif font found (Arial, Helvetica, Liberation Sans or DejaVu Sans).", call. = FALSE)
}

# font family, file and checksum, recorded on the figure because layout depends on the font file.
.margot_effects_font_record <- function(family) {
  path <- systemfonts::match_fonts(family)$path
  c(family = family, file = basename(path), md5 = unname(tools::md5sum(path)))
}

# ggplot linewidth for a line weight in points, and text size for a type size in points.
.margot_effects_lw <- function(pt) pt / (0.75 * ggplot2::.pt)
.margot_effects_ts <- function(pt) pt / ggplot2::.pt

# width of strings in millimetres at a type size. measured at 7200 dpi, because
# systemfonts::string_width() returns whole pixels.
.margot_effects_text_mm <- function(x, size_pt, family, bold = FALSE) {
  if (!length(x)) return(0)
  w <- systemfonts::string_width(x, family = family, size = size_pt, res = 7200,
                                 weight = if (bold) "bold" else "normal")
  w * 25.4 / 7200
}

# numbers with a true minus sign and no negative zero.
.margot_effects_num <- function(x, digits) {
  x <- round(x, digits)
  x[x == 0] <- 0
  sub("^-", "\u2212", formatC(x, format = "f", digits = digits))
}

# fewest decimals (from `min_digits`) at which no limit of an interval that
# excludes the null prints as the null itself.
.margot_effects_digits <- function(lower, upper, null, min_digits = 2L, max_digits = 8L) {
  excludes <- lower > null | upper < null
  for (d in min_digits:max_digits) {
    shown <- c(round(lower[excludes], d), round(upper[excludes], d))
    if (!any(shown == round(null, d))) return(d)
  }
  max_digits
}

# characters of right-aligned strings placed one by one from unkerned advance
# widths, so digits and decimal points align exactly in every row (kerning pairs
# such as "11" would otherwise shift them).
.margot_effects_glyphs <- function(strings, x_right, y, size_pt, family) {
  rows <- lapply(seq_along(strings), function(i) {
    ch <- strsplit(strings[i], "")[[1L]]
    adv <- .margot_effects_text_mm(ch, size_pt, family)
    data.frame(x = x_right - sum(adv) + cumsum(c(0, utils::head(adv, -1L))), y = y[i], label = ch,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# validate and complete a tidy effects table (one row per outcome and contrast).
.margot_effects_validate <- function(data) {
  if (!is.data.frame(data)) stop("`data` must be a data frame of effects.", call. = FALSE)
  required <- c("outcome", "estimate", "lower", "upper", "conf_level")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("effects table lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$outcome <- as.character(data$outcome)
  if (is.null(data$label)) data$label <- data$outcome
  if (is.null(data$contrast)) data$contrast <- "contrast"
  if (is.null(data$type)) data$type <- "difference"
  if (!all(data$type %in% c("difference", "ratio")) || length(unique(data$type)) != 1L) {
    stop("`type` must be one of \"difference\" or \"ratio\", shared by every row.", call. = FALSE)
  }
  if (length(unique(data$conf_level)) != 1L || !is.numeric(data$conf_level) ||
      data$conf_level[1L] <= 0 || data$conf_level[1L] >= 1) {
    stop("`conf_level` must be one value between 0 and 1, shared by every row.", call. = FALSE)
  }
  numbers <- data[c("estimate", "lower", "upper")]
  if (!all(vapply(numbers, is.numeric, logical(1))) || anyNA(numbers) ||
      !all(vapply(numbers, function(v) all(is.finite(v)), logical(1)))) {
    stop("estimates and interval limits must be finite numbers.", call. = FALSE)
  }
  if (anyNA(data$label) || anyNA(data$contrast)) stop("labels and contrasts must not be missing.", call. = FALSE)
  if (!all(data$lower <= data$estimate & data$estimate <= data$upper)) {
    stop("every estimate must lie within its interval.", call. = FALSE)
  }
  if (anyDuplicated(data[c("outcome", "contrast")])) {
    stop("each outcome may appear once per contrast.", call. = FALSE)
  }
  raw <- c("raw_estimate", "raw_lower", "raw_upper")
  if (any(raw %in% names(data))) {
    if (!all(raw %in% names(data))) {
      stop("original-unit columns need all of raw_estimate, raw_lower and raw_upper.", call. = FALSE)
    }
    given <- !is.na(data$raw_estimate) | !is.na(data$raw_lower) | !is.na(data$raw_upper)
    r <- data[given, raw, drop = FALSE]
    if (!all(vapply(r, is.numeric, logical(1))) || anyNA(r) ||
        !all(vapply(r, function(v) all(is.finite(v)), logical(1))) ||
        !all(r$raw_lower <= r$raw_estimate & r$raw_estimate <= r$raw_upper)) {
      stop("original-unit estimates must be finite and lie within their intervals.", call. = FALSE)
    }
  }
  if (data$type[1L] == "ratio" && !all(data$lower > 0)) {
    stop("ratio estimates need positive interval limits.", call. = FALSE)
  }
  data
}

# e-values (VanderWeele and Ding) for the estimate and for the interval limit
# nearest the null, using margot's core helpers; a standardised difference d is
# converted with RR = exp(0.91 d). the bound is 1 when the interval includes the null.
.margot_effects_evalue <- function(estimate, lower, upper, type = c("difference", "ratio")) {
  type <- match.arg(type)
  rows <- Map(function(est, lo, hi) {
    if (type == "ratio") .margot_evalues_rr(est, lo = lo, hi = hi)
    else .margot_evalues_ols_limits(est, lo = lo, hi = hi, sd = 1)
  }, estimate, lower, upper)
  values <- do.call(rbind, rows)
  data.frame(evalue = unname(values[, "E_Value"]), evalue_bound = unname(values[, "E_Val_bound"]))
}

# interval label, for example "95% CI", "99.875% CI" (exact when three decimals
# suffice) or "99.58% CI" (two decimals otherwise).
.margot_effects_ci_label <- function(conf_level) {
  level <- unique(conf_level)
  stopifnot(length(level) == 1L)
  pct <- 100 * level
  d <- if (abs(round(pct, 3) - pct) < 1e-9) 3 else 2
  paste0(sub("\\.?0+$", "", sprintf(paste0("%.", d, "f"), pct)), "% CI")
}

# fewest decimals (up to 8) at which every break prints exactly.
.margot_effects_axis_digits <- function(breaks) {
  for (d in 0:8) if (all(abs(round(breaks, d) - breaks) < 1e-9 * pmax(1, abs(breaks)))) return(d)
  8L
}

# tick labels as printed: decimals common to the axis on difference scales and
# minimal per label on ratio scales.
.margot_effects_axis_labels <- function(breaks, type = c("difference", "ratio")) {
  type <- match.arg(type)
  if (type == "ratio") sub("\\.?0+$", "", sprintf("%.8f", breaks))
  else .margot_effects_num(breaks, .margot_effects_axis_digits(breaks))
}

# smallest gap (mm) between adjacent tick labels on a panel `panel_mm` wide whose
# outer breaks sit 2.5 mm inside its edges.
.margot_effects_label_gap <- function(breaks, type, panel_mm, size_pt, family) {
  g <- if (type == "ratio") log10 else identity
  x <- 2.5 + (g(breaks) - g(min(breaks))) / diff(g(range(breaks))) * (panel_mm - 5)
  w <- .margot_effects_text_mm(.margot_effects_axis_labels(breaks, type), size_pt, family)
  min((x[-1L] - w[-1L] / 2) - (x[-length(x)] + w[-length(x)] / 2))
}

# axis breaks that bracket `data_range`. difference scales use steps of 1, 2, 2.5
# or 5 x 10^k. ratio scales use doubling ladders (0.5, 1, 2, ...), 1.5 x 2^k rungs,
# decades with 2-5 or 3 rungs, and 0.8/1.25 or 0.9/1.1 rungs within 0.5-2; ticks keep
# at most two significant digits, narrow ranges near 1 add evenly spaced candidates,
# and 1 is always a break. candidates have 3-6 breaks and, when `panel_mm` is given,
# at least `min_gap_mm` between adjacent labels. sets whose outer breaks leave the
# data within 0.1 of the largest share of the axis (log scale for ratios) tie; a tie
# goes to a set of 4-6 breaks where there is one, then to the larger share, then to
# fewer breaks.
.margot_effects_breaks <- function(data_range, type = c("difference", "ratio"), panel_mm = NULL,
                                   size_pt = 6.5, min_gap_mm = 2.5, family) {
  type <- match.arg(type)
  # a zero-width range (every estimate and limit at the null) gets a small span around it.
  if (diff(data_range) == 0) {
    data_range <- if (type == "difference") data_range + c(-0.1, 0.1) * max(1, abs(data_range[1L]))
                  else data_range * c(1 / 1.1, 1.1)
  }
  if (type == "difference") {
    steps <- as.vector(outer(c(1, 2, 2.5, 5), 10^(floor(log10(diff(data_range))) + (-2:1))))
    sets <- lapply(steps, function(st) {
      lo <- floor(data_range[1L] / st + 1e-9) * st
      hi <- ceiling(data_range[2L] / st - 1e-9) * st
      round(seq(lo, hi, by = st), 10)
    })
    share <- function(b) diff(data_range) / diff(range(b))
  } else {
    doubling <- 2^(seq(floor(log2(data_range[1L])) - 1, ceiling(log2(data_range[2L])) + 1))
    tens <- 10^(seq(floor(log10(data_range[1L])), ceiling(log10(data_range[2L]))))
    rungs <- list(doubling, sort(c(doubling, 1.5 * doubling)), tens, sort(c(tens, 3 * tens)),
                  sort(c(tens, 2 * tens, 5 * tens)))
    if (data_range[1L] >= 0.5 && data_range[2L] <= 2) {
      rungs <- c(rungs, list(sort(c(doubling, 0.8, 1.25)), sort(c(doubling, 0.8, 0.9, 1.1, 1.25))))
    }
    rungs <- lapply(rungs, function(r) r[abs(signif(r, 2) - r) < 1e-12 * r])
    if (data_range[2L] / data_range[1L] < 1.5) {
      steps <- as.vector(outer(c(1, 2, 2.5, 5), 10^(floor(log10(diff(data_range))) + (-1:1))))
      rungs <- c(rungs, lapply(steps, function(st) round(seq(floor(data_range[1L] / st + 1e-9) * st,
        ceiling(data_range[2L] / st - 1e-9) * st, by = st), 10)))
    }
    # ratio breaks must be positive (a step can floor the lower break to 0).
    rungs <- Filter(function(r) length(r) > 0L && all(r > 0), rungs)
    sets <- lapply(rungs, function(r) {
      below <- r[r <= data_range[1L] * (1 + 1e-9)]
      above <- r[r >= data_range[2L] * (1 - 1e-9)]
      if (!length(below) || !length(above)) return(NULL)
      sort(unique(c(r[r >= max(below) & r <= min(above)], 1)))
    })
    sets <- Filter(Negate(is.null), sets)
    share <- function(b) diff(log(data_range)) / diff(log(range(b)))
  }
  sets <- unique(sets)
  sets <- Filter(function(b) all(is.finite(b)) && (type == "difference" || all(b > 0)), sets)
  if (!length(sets)) {
    stop("no axis breaks found for range ", paste(signif(data_range, 4), collapse = " to "), call. = FALSE)
  }
  ok <- vapply(sets, function(b) length(b) >= 3L && length(b) <= 6L, logical(1))
  if (!is.null(panel_mm)) {
    ok <- ok & vapply(sets, function(b) length(b) < 2L ||
      isTRUE(.margot_effects_label_gap(b, type, panel_mm, size_pt, family) >= min_gap_mm), logical(1))
  }
  ok[is.na(ok)] <- FALSE
  if (!any(ok)) {
    # nothing fits: the bracketing set with fewest breaks (at least 2), with a warning.
    n <- lengths(sets)
    b <- sets[[which.min(ifelse(n >= 2L, n, Inf))]]
    if (!is.null(panel_mm)) {
      warning(sprintf("tick labels have only %.1f mm between them (minimum %.1f mm); ",
                      .margot_effects_label_gap(b, type, panel_mm, size_pt, family), min_gap_mm),
              "widen the figure or drop a numeric column.", call. = FALSE)
    }
    return(b)
  }
  sets <- sets[ok]
  sh <- vapply(sets, share, numeric(1))
  sh[!is.finite(sh)] <- -Inf
  tied <- which(sh >= max(sh) - 0.1)
  middle <- tied[lengths(sets[tied]) %in% 4:6]
  if (length(middle)) tied <- middle
  best <- tied[sh[tied] >= max(sh[tied]) - 1e-9]
  best <- best[lengths(sets[best]) == min(lengths(sets[best]))]
  sets[[best[1L]]]
}
