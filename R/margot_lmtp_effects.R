#' Tidy effects table from margot_lmtp() output
#'
#' Converts the output of [margot_lmtp()], or one of its combined tables, into the tidy effects table that [margot_plot_effects()] draws: one row per outcome and contrast, with the estimate, interval limits, confidence level and scale. Intervals are rebuilt from standard errors when a multiplicity correction or a different `alpha` is requested, or when registered standard errors are supplied. The function checks that the inputs agree before it returns anything.
#'
#' With the full [margot_lmtp()] output, standard errors and the regime means come from lmtp's contrast objects (`x$contrasts`). Each table interval must equal g(estimate) -/+ 1.96 SE from those objects, where g is the identity for differences and the log for ratios; lmtp reports the standard error of a ratio on the log scale. With a bare combined table, the standard error is recovered from the 95% interval, which must be a symmetric Wald interval (on the log scale for ratios); an asymmetric interval is refused rather than recentred. A ratio in a bare table is read as a risk ratio; odds ratios (`contrast_scale = "or"` in [margot_lmtp()]) are refused when the contrast objects show them, because the E-value and the regime-mean interpretation used here assume risk ratios.
#'
#' Under `adjust = "bonferroni"` the interval is g^-1(g(estimate) -/+ z SE) with z = qnorm(1 - alpha / (2 m)) and m = `family_size`. The family size is required, so that the correction follows the registered family of outcomes rather than the number of rows in a table.
#'
#' E-values are computed for the reported interval on ratio scales and, when `standardised = TRUE`, for differences in standard-deviation units through RR = exp(0.91 d). They are omitted for differences on an outcome's original scale, because the conversion assumes a standardised outcome. margot's own E-value columns are not passed through.
#'
#' @param x Output of [margot_lmtp()] (a list with `combined_tables` and `contrasts`), or one combined table with columns `"E[Y(1)]-E[Y(0)]"` or `"E[Y(1)]/E[Y(0)]"`, `"2.5 %"` and `"97.5 %"`, and outcomes as row names.
#' @param contrast Contrast names to keep (for example `"lower_vs_null"`), matching the suffix of `combined_outcomes_<contrast>`. `NULL` keeps every contrast of the full output; for a bare table it names the contrast (default `"contrast"`).
#' @param outcomes Outcomes that must be present. The call stops if any is missing, because a table can lack an outcome that was expected.
#' @param labels Named character vector of display labels, keyed by outcome. Defaults to the outcome names.
#' @param adjust `"none"` or `"bonferroni"`.
#' @param alpha Two-sided significance level before correction. Default 0.05.
#' @param family_size Number of outcomes in the correction family. Required with `adjust = "bonferroni"`.
#' @param se_source `"native"` (lmtp's standard errors, or those implied by a bare table's interval) or `"supplied"` (the `standard_errors` argument).
#' @param standard_errors With `se_source = "supplied"`, a data frame with columns `outcome`, `contrast` and `standard_error`, for example linearised standard errors from a registered inference procedure. Ratio standard errors are on the log scale.
#' @param scale_info Optional data frame of preparation constants keyed by `outcome`, in the format used by [margot_plot()]: `scale`, and optionally `center`, `orientation` and `unit_multiplier`. The model outcome is orientation * (Y - center) / scale. When supplied, differences are converted to original units (`raw_estimate`, `raw_lower`, `raw_upper`), and regime means are converted when `center` is present. Only identity transformations are supported; ratios have no original-unit conversion.
#' @param standardised `TRUE` when differences are in standard-deviation units. Controls E-values; regime means of standardised outcomes are dropped unless `scale_info` supplies `center`.
#' @param tol Numerical tolerance for the consistency checks.
#'
#' @return A data frame with columns `outcome`, `label`, `contrast`, `estimate`, `lower`, `upper`, `conf_level`, `type` (`"difference"` or `"ratio"`), `standard_error`, `se_source`, `family_size`, `mean_reference` and `mean_intervention` (estimated population means under the reference and intervention regimes, when available), and, when computed, `evalue`, `evalue_bound`, `raw_estimate`, `raw_lower` and `raw_upper`.
#'
#' @seealso [margot_plot_effects()], [margot_save_effects_plot()]
#' @md
#'
#' @examples
#' tab <- data.frame(
#'   `E[Y(1)]-E[Y(0)]` = c(0.12, -0.03),
#'   `2.5 %` = c(0.12, -0.03) - 1.96 * 0.02,
#'   `97.5 %` = c(0.12, -0.03) + 1.96 * 0.02,
#'   check.names = FALSE, row.names = c("y_warmth", "y_trust")
#' )
#' margot_lmtp_effects(tab, contrast = "lower_vs_null", adjust = "bonferroni",
#'   family_size = 2, standardised = TRUE)
#'
#' @export
margot_lmtp_effects <- function(x, contrast = NULL, outcomes = NULL, labels = NULL,
                                adjust = c("none", "bonferroni"), alpha = 0.05, family_size = NULL,
                                se_source = c("native", "supplied"), standard_errors = NULL,
                                scale_info = NULL, standardised = FALSE, tol = 1e-6) {
  adjust <- match.arg(adjust)
  se_source <- match.arg(se_source)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number between 0 and 1.", call. = FALSE)
  }
  if (adjust == "bonferroni") {
    if (is.null(family_size)) stop("`family_size` is required with adjust = \"bonferroni\".", call. = FALSE)
    if (!is.numeric(family_size) || length(family_size) != 1L || !is.finite(family_size) ||
        family_size < 1 || family_size != floor(family_size)) {
      stop("`family_size` must be one positive whole number.", call. = FALSE)
    }
  }
  if (se_source == "supplied") {
    if (!is.data.frame(standard_errors) ||
        !all(c("outcome", "contrast", "standard_error") %in% names(standard_errors))) {
      stop("se_source = \"supplied\" requires `standard_errors`, a data frame with outcome, contrast and standard_error.",
           call. = FALSE)
    }
    dup <- duplicated(standard_errors[c("outcome", "contrast")])
    if (any(dup)) {
      stop("standard_errors repeats outcome and contrast: ",
           paste(unique(paste(standard_errors$outcome[dup], standard_errors$contrast[dup], sep = " / ")),
                 collapse = ", "), call. = FALSE)
    }
  }
  if (is.data.frame(x)) {
    tables <- stats::setNames(list(x), paste0("combined_outcomes_", contrast %||% "contrast"))
    contrasts_obj <- NULL
  } else {
    tables <- x$combined_tables
    if (!is.null(contrast)) tables <- tables[paste0("combined_outcomes_", contrast)]
    contrasts_obj <- x$contrasts
  }
  if (is.null(tables) || !length(tables) || any(vapply(tables, is.null, logical(1)))) {
    stop("combined table not found", if (!is.null(contrast)) paste0(" for contrast ", paste(contrast, collapse = ", ")),
         ".", call. = FALSE)
  }
  z95 <- stats::qnorm(0.975)
  rows <- lapply(names(tables), function(name) {
    k <- sub("^combined_outcomes_", "", name)
    tab <- as.data.frame(tables[[name]], check.names = FALSE)
    found <- rownames(tab)
    if (!is.null(outcomes) && length(setdiff(outcomes, found))) {
      stop("contrast ", k, " lacks outcomes: ", paste(setdiff(outcomes, found), collapse = ", "), call. = FALSE)
    }
    column <- intersect(c("E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]"), names(tab))
    if (length(column) != 1L || !all(c("2.5 %", "97.5 %") %in% names(tab))) {
      stop("cannot find the estimate and 95% interval columns in ", name, ".", call. = FALSE)
    }
    type <- if (column == "E[Y(1)]/E[Y(0)]") "ratio" else "difference"
    g <- if (type == "ratio") log else identity
    gi <- if (type == "ratio") exp else identity
    est <- tab[[column]]
    lo <- tab[["2.5 %"]]
    hi <- tab[["97.5 %"]]
    ref <- shifted <- rep(NA_real_, length(est))
    if (!is.null(contrasts_obj)) {
      vals <- lapply(found, function(o) {
        obj <- contrasts_obj[[o]][[paste0(o, "_", k)]]
        if (is.null(obj) || is.null(obj$estimates)) {
          stop("no contrast object for ", o, " (", k, ").", call. = FALSE)
        }
        obj$estimates
      })
      kinds <- unique(unlist(lapply(found, function(o) contrasts_obj[[o]][[paste0(o, "_", k)]]$type)))
      if ("or" %in% kinds) {
        stop("contrast ", k, " is an odds ratio; margot_lmtp_effects() supports differences and risk ratios.",
             call. = FALSE)
      }
      se <- vapply(vals, function(v) v$std.error, numeric(1))
      ref <- vapply(vals, function(v) v$ref, numeric(1))
      shifted <- vapply(vals, function(v) v$shift, numeric(1))
      if (!isTRUE(all.equal(vapply(vals, function(v) v$estimate, numeric(1)), est, tolerance = tol))) {
        stop("contrast ", k, ": table estimates differ from lmtp's contrasts.", call. = FALSE)
      }
      off <- pmax(abs(g(lo) - (g(est) - z95 * se)), abs(g(hi) - (g(est) + z95 * se)))
      bad <- off > tol * pmax(1, z95 * se)
      if (any(bad)) {
        stop("contrast ", k, ": table intervals for ", paste(found[bad], collapse = ", "),
             " are not g(estimate) -/+ 1.96 SE from lmtp's contrasts.", call. = FALSE)
      }
    } else {
      asym <- abs((g(hi) - g(est)) - (g(est) - g(lo))) > tol * pmax(1, g(hi) - g(lo))
      if (any(asym) && se_source == "native") {
        stop("contrast ", k, ": intervals for ", paste(found[asym], collapse = ", "),
             " are not symmetric Wald intervals; supply margot_lmtp() output with contrasts, ",
             "or registered standard errors.", call. = FALSE)
      }
      se <- (g(hi) - g(lo)) / (2 * z95)
    }
    if (se_source == "supplied") {
      s <- standard_errors[standard_errors$contrast == k, , drop = FALSE]
      se <- s$standard_error[match(found, s$outcome)]
      if (anyNA(se)) {
        stop("standard_errors lacks outcomes for ", k, ": ", paste(found[is.na(se)], collapse = ", "), call. = FALSE)
      }
      if (!is.numeric(se) || any(!is.finite(se)) || any(se < 0)) {
        stop("supplied standard errors must be non-negative finite numbers.", call. = FALSE)
      }
    }
    # level 1 - alpha / m; intervals are rebuilt whenever that level is not the
    # table's 95% or the standard errors were supplied.
    if (adjust == "bonferroni" && family_size < length(unique(found))) {
      stop("`family_size` (", family_size, ") is smaller than the ", length(unique(found)),
           " outcomes in contrast ", k, ".", call. = FALSE)
    }
    m <- if (adjust == "bonferroni") family_size else 1
    level <- 1 - alpha / m
    if (abs(level - 0.95) > 1e-12 || se_source == "supplied") {
      z <- stats::qnorm(1 - alpha / (2 * m))
      lo <- gi(g(est) - z * se)
      hi <- gi(g(est) + z * se)
    }
    out <- data.frame(outcome = found, contrast = k, estimate = est, lower = lo, upper = hi,
                      conf_level = level, type = type, standard_error = se, se_source = se_source,
                      family_size = if (adjust == "bonferroni") as.integer(family_size) else NA_integer_,
                      mean_reference = ref, mean_intervention = shifted, stringsAsFactors = FALSE)
    if (type == "ratio" || standardised) {
      ev <- .margot_effects_evalue(est, lo, hi, type)
      out$evalue <- ev$evalue
      out$evalue_bound <- ev$evalue_bound
    }
    out
  })
  types <- unique(vapply(rows, function(r) r$type[1L], character(1)))
  if (length(types) != 1L) stop("contrasts mix difference and ratio scales; select one with `contrast`.", call. = FALSE)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (is.null(labels)) {
    out$label <- out$outcome
  } else {
    out$label <- unname(labels[out$outcome])
    if (anyNA(out$label)) {
      stop("labels missing for: ", paste(unique(out$outcome[is.na(out$label)]), collapse = ", "), call. = FALSE)
    }
  }
  if (!is.null(scale_info)) {
    if (out$type[1L] == "ratio") {
      stop("scale_info applies to differences only; ratios have no original-unit conversion.", call. = FALSE)
    }
    has_center <- is.data.frame(scale_info) && "center" %in% names(scale_info)
    if (is.data.frame(scale_info) && !"scale" %in% names(scale_info)) {
      stop("scale_info needs a `scale` column.", call. = FALSE)
    }
    meta <- margot_ate_scale_info(unique(out$outcome), scale_info)
    if (any(meta$transformation != "identity")) {
      stop("original-unit differences need identity transformations; log and log1p outcomes are ",
           "reported as ratios of geometric means by margot_plot().", call. = FALSE)
    }
    i <- match(out$outcome, meta$outcome)
    multiplier <- meta$orientation[i] * meta$scale[i] * meta$unit_multiplier[i]
    a <- out$lower * multiplier
    b <- out$upper * multiplier
    out$raw_estimate <- out$estimate * multiplier
    out$raw_lower <- pmin(a, b)
    out$raw_upper <- pmax(a, b)
    if (has_center) {
      unit <- meta$unit_multiplier[i]
      out$mean_reference <- (meta$center[i] + meta$orientation[i] * meta$scale[i] * out$mean_reference) * unit
      out$mean_intervention <- (meta$center[i] + meta$orientation[i] * meta$scale[i] * out$mean_intervention) * unit
    } else {
      out$mean_reference <- out$mean_intervention <- NA_real_
    }
  } else if (standardised) {
    out$mean_reference <- out$mean_intervention <- NA_real_
  }
  first <- c("outcome", "label", "contrast")
  .margot_effects_validate(out[c(first, setdiff(names(out), first))])
}
