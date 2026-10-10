# presentation methods for the nonbinding LMTP evidence reports: compact printed
# summaries and ggplot2 displays of the stored aggregate tables. every method reads
# the tables the constructors already hold, adds no verdict, threshold or route
# action, and returns its input invisibly (print) or a ggplot or patchwork object (plot).

# ---- formatting helpers --------------------------------------------------------------

.margot_evidence_role_line <- function() {
  "Descriptive and nonbinding: the report carries no verdict, threshold or route action."
}

.margot_evidence_heading <- function(title) {
  cat(title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

# purpose: format numbers with a fixed number of decimals and thousands separators.
.margot_evidence_number <- function(x, digits = 2) {
  x <- as.numeric(x)
  out <- rep("NA", length(x))
  finite <- is.finite(x)
  out[finite] <- formatC(x[finite], format = "f", digits = digits, big.mark = ",")
  out
}

.margot_evidence_percent <- function(x, digits = 2) {
  .margot_evidence_number(100 * as.numeric(x), digits)
}

.margot_evidence_count <- function(x) {
  x <- as.numeric(x)
  out <- rep("NA", length(x))
  finite <- is.finite(x)
  out[finite] <- formatC(x[finite], format = "d", big.mark = ",")
  out
}

# purpose: print a display table without row names, indented under its heading.
.margot_evidence_table <- function(table, indent = "  ") {
  text <- utils::capture.output(print(table, row.names = FALSE, right = TRUE))
  cat(paste0(indent, text), sep = "\n")
}

# purpose: print one provenance record, which may be text or an aggregate table.
.margot_evidence_provenance <- function(label, value) {
  if (is.null(value)) return(invisible(NULL))
  if (is.data.frame(value)) {
    cat(label, ": aggregate table with ", nrow(value), " row",
        if (nrow(value) == 1L) "" else "s", "\n", sep = "")
  } else {
    cat(label, ": ", paste(as.character(unlist(value)), collapse = "; "), "\n", sep = "")
  }
  invisible(NULL)
}

# purpose: the top-share sentence for one concentration table.
.margot_evidence_concentration_text <- function(concentration, digits = 2) {
  paste0(
    "top ", formatC(100 * concentration$top_fraction, format = "fg"), "% of rows (",
    .margot_evidence_count(concentration$top_n), ") hold ",
    .margot_evidence_percent(concentration$weight_share, digits), "% of the weight",
    collapse = "; "
  )
}

# purpose: a display table for one per-transition numeric summary.
.margot_evidence_range_table <- function(summary, digits = 2) {
  data.frame(
    transition = summary$wave,
    median = .margot_evidence_number(summary$median, digits),
    p01 = .margot_evidence_number(summary$p01, digits),
    p99 = .margot_evidence_number(summary$p99, digits),
    min = .margot_evidence_number(summary$min, digits),
    max = .margot_evidence_number(summary$max, digits),
    `n zero` = .margot_evidence_count(summary$n_zero),
    `n missing` = .margot_evidence_count(summary$n_missing + summary$n_non_finite),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ---- print methods -------------------------------------------------------------------

#' Print an LMTP censoring report
#'
#' Prints observed retention by transition and, when the report holds them, the
#' fitted continued-observation probabilities, censoring factors, joint ratios and
#' exact-zero causes. The printed summary repeats the stored aggregates and adds
#' no classification.
#'
#' @param x A `margot_lmtp_censoring_report` object.
#' @param digits Decimal places for percentages and summaries.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.margot_lmtp_censoring_report <- function(x, digits = 2, ...) {
  m <- x$metadata
  .margot_evidence_heading("LMTP censoring report")
  cat(.margot_evidence_role_line(), "\n", sep = "")
  cat("Policy: ", m$policy_id %||% "none recorded",
      " | transitions: ", length(m$wave_labels),
      " | missing observation indicator read as ",
      if (isTRUE(m$na_is_unobserved)) "unobserved" else "missing", "\n\n", sep = "")
  r <- x$retention
  cat("Observed retention (share of all rows still observed)\n")
  .margot_evidence_table(data.frame(
    transition = r$wave,
    `n observed` = .margot_evidence_count(r$n_observed),
    `% unweighted` = .margot_evidence_number(r$pct_observed_unweighted, digits),
    `% weighted` = .margot_evidence_number(r$pct_observed_weighted, digits),
    `indicator missing` = .margot_evidence_count(r$n_indicator_missing),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  summaries <- list(
    `Fitted continued-observation probability` = x$fitted_probability_summary,
    `Censoring factor` = x$censoring_factor_summary,
    `Joint exposure-and-censoring ratio` = x$joint_ratio_summary
  )
  for (name in names(summaries)) {
    s <- summaries[[name]]
    if (is.null(s)) next
    cat("\n", name, " by transition\n", sep = "")
    .margot_evidence_table(.margot_evidence_range_table(s, digits))
  }
  zero <- x$exact_zero_causes
  if (!is.null(zero)) {
    zero <- zero[zero$n > 0, , drop = FALSE]
    cat("\nExact zeros and unavailable values\n")
    if (nrow(zero)) {
      .margot_evidence_table(data.frame(
        component = zero$component, transition = zero$wave, cause = zero$cause,
        n = .margot_evidence_count(zero$n), check.names = FALSE, stringsAsFactors = FALSE
      ))
    } else {
      cat("  none recorded\n")
    }
  }
  if (!is.null(x$learner_specification) || !is.null(x$out_of_fold_performance)) cat("\n")
  .margot_evidence_provenance("Learner specification", x$learner_specification)
  .margot_evidence_provenance("Out-of-fold performance", x$out_of_fold_performance)
  invisible(x)
}

#' Print a target-population projection report
#'
#' Prints the source, projected-source and target values for every registered
#' projection variable, names target strata without source representation, and
#' summarises the projection weights. Categorical rows are shown as percentages
#' with differences in percentage points; numeric rows as means with
#' standardised differences.
#'
#' @param x A `margot_target_projection_report` object.
#' @param digits Decimal places for percentages and summaries.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.margot_target_projection_report <- function(x, digits = 2, ...) {
  m <- x$metadata
  .margot_evidence_heading("Target-population projection report")
  cat(.margot_evidence_role_line(), "\n", sep = "")
  cat("Variables: ", paste(m$variables, collapse = ", "),
      " | source rows: ", .margot_evidence_count(m$source_n),
      " | target rows: ", .margot_evidence_count(m$target_n),
      if (length(m$strata)) paste0(" | strata: ", paste(m$strata, collapse = " x ")) else "",
      "\n\n", sep = "")
  b <- x$balance
  categorical <- b$variable_type == "categorical"
  if (any(categorical)) {
    cat("Categorical balance (percent of rows; differences in percentage points)\n")
    rows <- b[categorical, , drop = FALSE]
    .margot_evidence_table(data.frame(
      variable = rows$variable, level = rows$level,
      `source %` = .margot_evidence_percent(rows$source_value, digits),
      `projected %` = .margot_evidence_percent(rows$projected_source_value, digits),
      `target %` = .margot_evidence_percent(rows$target_value, digits),
      `diff before` = .margot_evidence_percent(rows$difference_before_projection, digits),
      `diff after` = .margot_evidence_percent(rows$difference_after_projection, digits),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  if (any(!categorical)) {
    if (any(categorical)) cat("\n")
    cat("Numeric balance (means; standardised differences)\n")
    rows <- b[!categorical, , drop = FALSE]
    .margot_evidence_table(data.frame(
      variable = rows$variable,
      `source mean` = .margot_evidence_number(rows$source_value, digits),
      `projected mean` = .margot_evidence_number(rows$projected_source_value, digits),
      `target mean` = .margot_evidence_number(rows$target_value, digits),
      `std diff before` = .margot_evidence_number(rows$standardised_difference_before, digits),
      `std diff after` = .margot_evidence_number(rows$standardised_difference_after, digits),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  s <- x$stratum_representation
  if (!is.null(s)) {
    unrepresented <- s[!s$represented_in_source, , drop = FALSE]
    cat("\nStrata: ", nrow(s), " in source or target; ", nrow(unrepresented),
        " without source representation", if (nrow(unrepresented)) ":" else ".", "\n", sep = "")
    if (nrow(unrepresented)) {
      .margot_evidence_table(data.frame(
        stratum = unrepresented$stratum,
        `target n` = .margot_evidence_count(unrepresented$target_n),
        `target weight %` = .margot_evidence_percent(unrepresented$target_weight_share, digits),
        check.names = FALSE, stringsAsFactors = FALSE
      ))
    }
  }
  w <- x$weight_summary
  cat("\nProjection weights: ", .margot_evidence_count(w$n), " rows (",
      .margot_evidence_count(w$n_missing + w$n_non_finite), " missing, ",
      .margot_evidence_count(w$n_zero), " zero); min ", .margot_evidence_number(w$min, digits),
      ", median ", .margot_evidence_number(w$median, digits),
      ", mean ", .margot_evidence_number(w$mean, digits),
      ", max ", .margot_evidence_number(w$max, digits), ".\n", sep = "")
  cat("Kish effective sample size ", .margot_evidence_number(w$kish_effective_n, digits),
      " (", .margot_evidence_number(w$kish_fraction_of_all_rows, digits), " of rows); ",
      .margot_evidence_concentration_text(x$weight_concentration, digits), ".\n", sep = "")
  if (!is.null(x$harmonisation) || !is.null(x$projection_model) || !is.null(x$uncertainty)) cat("\n")
  .margot_evidence_provenance("Harmonisation", x$harmonisation)
  .margot_evidence_provenance("Projection model", x$projection_model)
  .margot_evidence_provenance("Uncertainty", x$uncertainty)
  invisible(x)
}

#' Print an LMTP full analysis-weight report
#'
#' Prints the distribution of the full analysis weight (baseline weight times the
#' cumulative joint ratio) at every node and stage, the concentration at the final
#' node, the causes of zero or unavailable final weights, and the regularisation
#' comparison when one is stored.
#'
#' @param x A `margot_lmtp_analysis_weight_report` object.
#' @param digits Decimal places for summaries.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.margot_lmtp_analysis_weight_report <- function(x, digits = 2, ...) {
  m <- x$metadata
  .margot_evidence_heading("LMTP full analysis-weight report")
  cat(.margot_evidence_role_line(), "\n", sep = "")
  cat("Policy: ", m$policy_id %||% "none recorded",
      " | nodes: ", paste(m$wave_labels, collapse = ", "),
      " | regularised ratios: ", if (isTRUE(m$regularised_ratios_supplied)) "supplied" else "none",
      " | zero weights retained in every denominator\n\n", sep = "")
  s <- x$weight_summary
  cat("Full analysis weight by node (baseline weight x cumulative joint ratio)\n")
  .margot_evidence_table(data.frame(
    stage = s$stage, node = s$wave,
    n = .margot_evidence_count(s$n),
    `n zero` = .margot_evidence_count(s$n_zero),
    `n missing` = .margot_evidence_count(s$n_missing + s$n_non_finite),
    median = .margot_evidence_number(s$median, digits),
    p99 = .margot_evidence_number(s$p99, digits),
    max = .margot_evidence_number(s$max, digits),
    `Kish n` = .margot_evidence_number(s$kish_effective_n, digits),
    `Kish/n` = .margot_evidence_number(s$kish_fraction_of_all_rows, digits),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  final <- m$wave_labels[[length(m$wave_labels)]]
  k <- x$weight_concentration[x$weight_concentration$wave == final, , drop = FALSE]
  cat("\nConcentration at the final node (", final, ")\n", sep = "")
  for (stage in unique(k$stage)) {
    cat("  ", stage, ": ", .margot_evidence_concentration_text(k[k$stage == stage, , drop = FALSE], digits),
        "\n", sep = "")
  }
  causes <- x$exact_zero_and_missing_causes
  causes <- causes[causes$cause != "positive_finite_full_weight" & causes$n > 0, , drop = FALSE]
  cat("\nZero or unavailable final weights, by first cause\n")
  if (nrow(causes)) {
    .margot_evidence_table(data.frame(
      stage = causes$stage, cause = causes$cause, n = .margot_evidence_count(causes$n),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  } else {
    cat("  none: every final weight is positive and finite\n")
  }
  comparison <- x$regularisation_comparison
  if (!is.null(comparison)) {
    cat("\nRegularisation: raw final weight sum ", .margot_evidence_number(comparison$raw_final_weight_sum, digits),
        "; regularised ", .margot_evidence_number(comparison$regularised_final_weight_sum, digits),
        "; relative difference ", .margot_evidence_percent(comparison$relative_difference, digits), "%.\n", sep = "")
  }
  .margot_evidence_provenance("Regularisation description", x$regularisation)
  invisible(x)
}

# purpose: one-line summaries of each component for the evidence manifest.
.margot_evidence_censoring_line <- function(report, digits = 2) {
  r <- report$retention
  extras <- c(
    if (isTRUE(report$metadata$fitted_probabilities_supplied)) "fitted probabilities",
    if (isTRUE(report$metadata$censoring_factors_supplied)) "censoring factors",
    if (isTRUE(report$metadata$joint_ratios_supplied)) "joint ratios"
  )
  paste0(
    nrow(r), " transition", if (nrow(r) == 1L) "" else "s", "; rows observed ",
    .margot_evidence_number(r$pct_observed_unweighted[[1L]], digits), "% to ",
    .margot_evidence_number(r$pct_observed_unweighted[[nrow(r)]], digits), "% (weighted ",
    .margot_evidence_number(r$pct_observed_weighted[[1L]], digits), "% to ",
    .margot_evidence_number(r$pct_observed_weighted[[nrow(r)]], digits), "%)",
    if (length(extras)) paste0("; with ", paste(extras, collapse = ", ")) else ""
  )
}

.margot_evidence_projection_line <- function(report, digits = 2) {
  b <- report$balance
  w <- report$weight_summary
  categorical <- b[b$variable_type == "categorical", , drop = FALSE]
  largest <- if (nrow(categorical)) {
    i <- which.max(abs(categorical$difference_after_projection))
    paste0("; largest difference after projection ",
           .margot_evidence_percent(categorical$difference_after_projection[[i]], digits), " pp (",
           categorical$variable[[i]], " ", categorical$level[[i]], ")")
  } else ""
  strata <- report$stratum_representation
  unrepresented <- if (is.null(strata)) "" else paste0(
    "; strata without source representation: ", sum(!strata$represented_in_source), " of ", nrow(strata)
  )
  paste0(
    "variables ", paste(report$metadata$variables, collapse = ", "),
    "; Kish effective n ", .margot_evidence_number(w$kish_effective_n, digits), " (",
    .margot_evidence_number(w$kish_fraction_of_all_rows, digits), " of ", .margot_evidence_count(w$n), " rows)",
    largest, unrepresented
  )
}

.margot_evidence_weights_line <- function(reports, digits = 2) {
  pieces <- vapply(seq_along(reports), function(i) {
    report <- reports[[i]]
    label <- names(reports)[i] %||% report$metadata$policy_id %||% paste0("policy ", i)
    if (!nzchar(label)) label <- report$metadata$policy_id %||% paste0("policy ", i)
    final <- report$metadata$wave_labels[[length(report$metadata$wave_labels)]]
    s <- report$weight_summary[report$weight_summary$wave == final, , drop = FALSE]
    paste0(label, ": final-node Kish/n ", paste0(
      .margot_evidence_number(s$kish_fraction_of_all_rows, digits), " (", s$stage, ")", collapse = ", "
    ), "; max ", .margot_evidence_number(max(s$max), digits))
  }, character(1))
  paste0(length(reports), " polic", if (length(reports) == 1L) "y" else "ies", "; ", paste(pieces, collapse = "; "))
}

#' Print an LMTP evidence report
#'
#' Prints the availability manifest with a one-line summary of each available
#' component and the structural reason for each unavailable one. Print a
#' component for its tables, or `plot()` the report for the available displays.
#'
#' @param x A `margot_lmtp_evidence_report` object.
#' @param digits Decimal places for the component summaries.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.margot_lmtp_evidence_report <- function(x, digits = 2, ...) {
  .margot_evidence_heading("LMTP evidence report")
  cat(.margot_evidence_role_line(), "\n\n", sep = "")
  manifest <- x$manifest
  for (i in seq_len(nrow(manifest))) {
    name <- manifest$object[[i]]
    line <- if (!manifest$available[[i]]) {
      paste0("unavailable: ", manifest$structural_reason[[i]])
    } else {
      switch(name,
        censoring_report = .margot_evidence_censoring_line(x$censoring_report, digits),
        projection_report = .margot_evidence_projection_line(x$projection_report, digits),
        analysis_weight_reports = .margot_evidence_weights_line(x$analysis_weight_reports, digits)
      )
    }
    cat(formatC(name, width = -24), " ", line, "\n", sep = "")
  }
  cat("\nPrint a component for its tables: x$censoring_report, x$projection_report,",
      "x$analysis_weight_reports[[i]]. plot(x) draws the available components.\n")
  invisible(x)
}

# ---- plot methods --------------------------------------------------------------------

.margot_evidence_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "grey30")
    )
}

# colours: grey for the unweighted or raw series, Okabe-Ito blue for the weighted or
# regularised series, dark blue for medians.
.margot_evidence_palette <- function() c(base = "grey55", emphasis = "#0072B2", point = "#17365D")

# purpose: a horizontal range display (min-max, p01-p99 band and median) by transition.
.margot_evidence_range_plot <- function(summary, title, x_label, limits = NULL, log_scale = FALSE) {
  palette <- .margot_evidence_palette()
  rows <- summary
  rows$transition <- factor(rows$wave, levels = rev(unique(rows$wave)))
  graph <- ggplot2::ggplot(rows, ggplot2::aes(y = .data$transition)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$min, xend = .data$max, yend = .data$transition),
                          colour = "grey65", linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$p01, xend = .data$p99, yend = .data$transition),
                          colour = palette[["emphasis"]], linewidth = 2) +
    ggplot2::geom_point(ggplot2::aes(x = .data$median), colour = palette[["point"]], size = 2.6) +
    ggplot2::labs(x = x_label, y = NULL, title = title,
                  subtitle = "Range in grey, 1st to 99th percentile in blue, median as a point") +
    .margot_evidence_theme()
  if (log_scale) {
    graph <- graph + ggplot2::scale_x_log10()
  } else if (!is.null(limits)) {
    graph <- graph + ggplot2::scale_x_continuous(limits = limits)
  }
  graph
}

#' Plot an LMTP censoring report
#'
#' Draws observed retention by transition, unweighted and weighted, and adds a
#' range panel for the fitted continued-observation probabilities and for the
#' joint ratios when the report holds them.
#'
#' @param x A `margot_lmtp_censoring_report` object.
#' @param ... Ignored.
#' @return A `ggplot` object, or a `patchwork` object when more than one panel is drawn.
#' @export
plot.margot_lmtp_censoring_report <- function(x, ...) {
  palette <- .margot_evidence_palette()
  r <- x$retention
  rows <- rbind(
    data.frame(transition = r$wave, series = "Unweighted", value = r$pct_observed_unweighted,
               stringsAsFactors = FALSE),
    data.frame(transition = r$wave, series = "Weighted", value = r$pct_observed_weighted,
               stringsAsFactors = FALSE)
  )
  rows$transition <- factor(rows$transition, levels = r$wave)
  rows$series <- factor(rows$series, levels = c("Unweighted", "Weighted"))
  retention <- ggplot2::ggplot(rows, ggplot2::aes(.data$transition, .data$value,
                                                  colour = .data$series, group = .data$series)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_colour_manual(values = c(Unweighted = palette[["base"]], Weighted = palette[["emphasis"]])) +
    ggplot2::labs(x = NULL, y = "Rows still observed (%)", colour = NULL, title = "Observed retention",
                  subtitle = if (is.null(x$metadata$policy_id)) NULL else paste0("Policy: ", x$metadata$policy_id)) +
    .margot_evidence_theme()
  panels <- list(retention)
  if (!is.null(x$fitted_probability_summary)) {
    panels <- c(panels, list(.margot_evidence_range_plot(
      x$fitted_probability_summary, "Fitted continued-observation probability",
      "Probability", limits = c(0, 1)
    )))
  }
  if (!is.null(x$joint_ratio_summary)) {
    # joint ratios retain exact zeros, so the axis stays linear.
    panels <- c(panels, list(.margot_evidence_range_plot(
      x$joint_ratio_summary, "Joint exposure-and-censoring ratio", "Ratio (zeros retained)"
    )))
  }
  if (length(panels) == 1L) return(panels[[1L]])
  patchwork::wrap_plots(panels, ncol = length(panels))
}

#' Plot a target-population projection report
#'
#' Draws the difference between the source sample and the target before and after
#' projection weighting, one facet per registered variable (percentage points for
#' categorical levels; standardised differences for numeric variables), and the
#' share of the projection weight held by the largest weights beside the share
#' under equal weights.
#'
#' @param x A `margot_target_projection_report` object.
#' @param what Which panels to draw: `"balance"`, `"weights"`, or both (the default).
#' @param ... Ignored.
#' @return A `ggplot` object for one panel, or a `patchwork` object for both.
#' @export
plot.margot_target_projection_report <- function(x, what = c("balance", "weights"), ...) {
  what <- match.arg(what, several.ok = TRUE)
  palette <- .margot_evidence_palette()
  panels <- list()
  if ("balance" %in% what) {
    b <- x$balance
    categorical <- b$variable_type == "categorical"
    label <- ifelse(categorical, b$level, b$variable)
    facet <- ifelse(categorical, paste0(b$variable, " (percentage points)"),
                    paste0(b$variable, " (standardised difference)"))
    before <- ifelse(categorical, 100 * b$difference_before_projection, b$standardised_difference_before)
    after <- ifelse(categorical, 100 * b$difference_after_projection, b$standardised_difference_after)
    rows <- rbind(
      data.frame(facet = facet, label = label, series = "Unweighted", value = before, stringsAsFactors = FALSE),
      data.frame(facet = facet, label = label, series = "Weighted", value = after, stringsAsFactors = FALSE)
    )
    rows$facet <- factor(rows$facet, levels = unique(facet))
    rows$label <- factor(rows$label, levels = rev(unique(label)))
    rows$series <- factor(rows$series, levels = c("Unweighted", "Weighted"))
    balance <- ggplot2::ggplot(rows, ggplot2::aes(.data$value, .data$label,
                                                  colour = .data$series, shape = .data$series)) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
      ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.4)) +
      ggplot2::facet_grid(facet ~ ., scales = "free_y", space = "free_y") +
      ggplot2::scale_colour_manual(values = c(Unweighted = palette[["base"]], Weighted = palette[["emphasis"]])) +
      ggplot2::labs(x = "Source minus target", y = NULL, colour = NULL, shape = NULL,
                    title = "Difference from the target before and after projection") +
      .margot_evidence_theme() +
      ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
    panels <- c(panels, list(balance))
  }
  if ("weights" %in% what) {
    k <- x$weight_concentration
    w <- x$weight_summary
    rows <- data.frame(
      group = factor(paste0("Top ", formatC(100 * k$top_fraction, format = "fg"), "%"),
                     levels = paste0("Top ", formatC(100 * k$top_fraction, format = "fg"), "%")),
      share = 100 * k$weight_share,
      equal = 100 * k$top_fraction,
      stringsAsFactors = FALSE
    )
    weights <- ggplot2::ggplot(rows, ggplot2::aes(.data$group, .data$share)) +
      ggplot2::geom_col(fill = palette[["emphasis"]], width = 0.6) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$equal, ymax = .data$equal),
                             width = 0.75, colour = palette[["point"]], linetype = "dashed") +
      ggplot2::labs(x = "Rows with the largest weights", y = "Share of total weight (%)",
                    title = "Projection-weight concentration",
                    subtitle = paste0("Kish effective n ", .margot_evidence_number(w$kish_effective_n),
                                      " (", .margot_evidence_number(w$kish_fraction_of_all_rows),
                                      " of rows)\nDashed line: share under equal weights")) +
      .margot_evidence_theme()
    panels <- c(panels, list(weights))
  }
  if (length(panels) == 1L) return(panels[[1L]])
  patchwork::wrap_plots(panels, ncol = 2, widths = c(2, 1))
}

#' Plot an LMTP full analysis-weight report
#'
#' Draws, for every node and stage, the Kish effective sample size as a fraction
#' of all rows, the maximum full analysis weight, and the share of rows with a
#' zero weight.
#'
#' @param x A `margot_lmtp_analysis_weight_report` object.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @export
plot.margot_lmtp_analysis_weight_report <- function(x, ...) {
  palette <- .margot_evidence_palette()
  s <- x$weight_summary
  metrics <- c("Kish effective n / rows", "Maximum full weight", "Rows with zero weight (%)")
  rows <- rbind(
    data.frame(node = s$wave, stage = s$stage, metric = metrics[[1L]], value = s$kish_fraction_of_all_rows,
               stringsAsFactors = FALSE),
    data.frame(node = s$wave, stage = s$stage, metric = metrics[[2L]], value = s$max, stringsAsFactors = FALSE),
    data.frame(node = s$wave, stage = s$stage, metric = metrics[[3L]], value = 100 * s$n_zero / s$n,
               stringsAsFactors = FALSE)
  )
  rows$node <- factor(rows$node, levels = rev(x$metadata$wave_labels))
  rows$metric <- factor(rows$metric, levels = metrics)
  stages <- c("raw", "regularised")
  rows$stage <- factor(rows$stage, levels = stages[stages %in% unique(rows$stage)])
  ggplot2::ggplot(rows, ggplot2::aes(.data$value, .data$node, colour = .data$stage, shape = .data$stage)) +
    ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::facet_wrap(~metric, scales = "free_x") +
    ggplot2::scale_colour_manual(values = c(raw = palette[["base"]], regularised = palette[["emphasis"]])) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, shape = NULL,
                  title = "Full analysis weight by node",
                  subtitle = if (is.null(x$metadata$policy_id)) NULL else paste0("Policy: ", x$metadata$policy_id)) +
    .margot_evidence_theme()
}

#' Plot an LMTP evidence report
#'
#' Stacks the displays of the available components (censoring, projection and
#' full analysis weights) and lists each unavailable component with its
#' structural reason in the caption.
#'
#' @param x A `margot_lmtp_evidence_report` object.
#' @param ... Ignored.
#' @return A `patchwork` object.
#' @export
plot.margot_lmtp_evidence_report <- function(x, ...) {
  panels <- list()
  if (!is.null(x$censoring_report)) panels <- c(panels, list(plot(x$censoring_report)))
  if (!is.null(x$projection_report)) panels <- c(panels, list(plot(x$projection_report)))
  if (!is.null(x$analysis_weight_reports)) {
    panels <- c(panels, lapply(x$analysis_weight_reports, plot))
  }
  if (!length(panels)) {
    stop("The evidence report holds no available component to plot.", call. = FALSE)
  }
  missing <- x$manifest[!x$manifest$available, , drop = FALSE]
  caption <- if (nrow(missing)) {
    paste(strwrap(paste0(
      "Unavailable: ", paste0(missing$object, " (", missing$structural_reason, ")", collapse = "; ")
    ), width = 110), collapse = "\n")
  } else {
    NULL
  }
  patchwork::wrap_plots(panels, ncol = 1) +
    patchwork::plot_annotation(
      title = "LMTP evidence report (descriptive, nonbinding)", caption = caption,
      theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 8.5, colour = "grey30"))
    )
}
