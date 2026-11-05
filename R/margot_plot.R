##
## Stable plotting API (reverted to previous working version)
##

#' Helper function to detect effect column and type
#' @keywords internal
detect_effect_column <- function(data) {
  # check for new column types first (to prioritize them)
  new_cols <- c("ATE", "ATT", "ATC", "ATO")
  for (col in new_cols) {
    if (col %in% names(data)) {
      return(list(column = col, type = col))
    }
  }

  # check for naive regression column
  if ("E[Y|A]" %in% names(data)) {
    return(list(column = "E[Y|A]", type = "naive"))
  }

  # check for traditional columns
  if ("E[Y(1)]-E[Y(0)]" %in% names(data)) {
    return(list(column = "E[Y(1)]-E[Y(0)]", type = "ATE"))
  }
  if ("E[Y(1)]/E[Y(0)]" %in% names(data)) {
    return(list(column = "E[Y(1)]/E[Y(0)]", type = "ATE"))
  }

  # if nothing found, return NULL
  return(NULL)
}

#' @keywords internal
format_margot_percent <- function(x, digits = 2) {
  out <- sprintf(paste0("%.", digits, "f"), x * 100)
  out <- sub("\\.?0+$", "", out)
  paste0(out, "%")
}

#' @keywords internal
format_margot_probability_column <- function(x, digits = 2) {
  sub("%$", " %", format_margot_percent(x, digits = digits))
}

#' @keywords internal
format_margot_ci_level <- function(level) {
  if (length(level) == 0 || is.na(level[1])) {
    return("CI")
  }

  paste0(format_margot_percent(level[1]), " CI")
}

#' @keywords internal
margot_adjust_label <- function(adjust) {
  switch(adjust,
    none = NULL,
    bonferroni = "Bonferroni",
    holm = "Holm",
    BH = "BH",
    adjust
  )
}

#' @keywords internal
normalise_margot_confidence_levels <- function(df, default_level = 0.95) {
  levels <- if ("confidence_level" %in% names(df)) df$confidence_level else rep(default_level, nrow(df))
  levels <- as.numeric(levels)
  levels[is.na(levels)] <- default_level
  levels
}

#' @keywords internal
margot_ci_column_names <- function(df, default_level = 0.95) {
  levels <- normalise_margot_confidence_levels(df, default_level = default_level)
  unique_levels <- unique(round(levels, 10))

  if (length(unique_levels) == 1L) {
    alpha_level <- 1 - unique_levels[1]
    lower <- alpha_level / 2
    upper <- 1 - lower

    return(c(
      format_margot_probability_column(lower),
      format_margot_probability_column(upper)
    ))
  }

  c("CI lower", "CI upper")
}

#' @keywords internal
margot_ci_caption <- function(df, adjust = "none", default_level = 0.95) {
  levels <- normalise_margot_confidence_levels(df, default_level = default_level)
  unique_levels <- unique(round(levels, 10))

  if (length(unique_levels) == 1L) {
    if (identical(adjust, "none") && isTRUE(all.equal(unique_levels[1], default_level))) {
      return(NULL)
    }

    prefix <- margot_adjust_label(adjust)
    prefix <- if (is.null(prefix)) "" else paste0(prefix, "-adjusted ")
    return(paste0(prefix, format_margot_percent(unique_levels[1]), " confidence intervals"))
  }

  prefix <- margot_adjust_label(adjust)
  prefix <- if (is.null(prefix)) "" else paste0(prefix, "-adjusted ")
  paste0(prefix, "confidence interval coverage varies by outcome")
}

#' @keywords internal
format_margot_coefficient_labels <- function(x, digits = 2) {
  base_labels <- sprintf(paste0("%.", digits, "f"), x)
  width <- max(nchar(base_labels), na.rm = TRUE)
  sprintf(paste0("%", width, ".", digits, "f"), x)
}

#' @keywords internal
compute_margot_plot_limits <- function(df, effect_col, x_lim_lo = NULL, x_lim_hi = NULL, expansion = 0.1) {
  if (!is.null(x_lim_lo) && !is.null(x_lim_hi)) {
    return(c(x_lim_lo, x_lim_hi))
  }

  data_range <- range(c(df[[effect_col]], df$`2.5 %`, df$`97.5 %`), na.rm = TRUE)
  x_range <- diff(data_range)
  if (!is.finite(x_range) || x_range == 0) {
    x_range <- max(abs(data_range), 1, na.rm = TRUE)
  }

  lo <- if (is.null(x_lim_lo)) data_range[1] - (expansion * x_range) else x_lim_lo
  hi <- if (is.null(x_lim_hi)) data_range[2] + (expansion * x_range) else x_lim_hi

  c(lo, hi)
}

# interval decision, E-value flag and display helpers ----------------------

# the null value as it appears in labels and prose
margot_null_label <- function(type) {
  if (identical(type, "RR")) "1" else "0"
}

# compare E-value bounds with the reporting threshold at full precision
margot_evalue_flag <- function(bound, threshold, comparison = c(">", ">=")) {
  comparison <- match.arg(comparison)
  if (comparison == ">") bound > threshold else bound >= threshold
}

# header naming the E-value flag, e.g. "E-value bound > 1.1"
margot_evalue_flag_header <- function(threshold, comparison) {
  paste0("E-value bound ", comparison, " ", format(threshold))
}

# classify each interval relative to the null. the default rule uses the
# interval alone; the legacy rule also requires the E-value flag, as margot
# 1.3.2 did, and labels every other estimate "not reliable".
margot_interval_category <- function(lower, upper, null_val, bound = NULL, threshold = NULL,
                                     category_rule = c("interval", "interval_and_evalue"),
                                     comparison = c(">", ">=")) {
  category_rule <- match.arg(category_rule)
  comparison <- match.arg(comparison)
  above <- lower > null_val & upper > null_val
  below <- lower < null_val & upper < null_val

  if (category_rule == "interval") {
    out <- ifelse(above, "positive", ifelse(below, "negative", "includes null"))
    return(factor(out, levels = c("positive", "includes null", "negative")))
  }

  flag <- margot_evalue_flag(bound, threshold, comparison)
  out <- ifelse(flag & above, "positive", ifelse(flag & below, "negative", "not reliable"))
  factor(out, levels = c("positive", "not reliable", "negative"))
}

# refuse an interval decision on intervals that cannot carry it
margot_check_interval_rule_adjustment <- function(category_rule, adjust, warn = TRUE) {
  if (!adjust %in% c("holm", "BH")) {
    return(invisible(NULL))
  }
  if (category_rule == "interval") {
    stop(
      "category_rule = \"interval\" cannot be combined with adjust = \"", adjust, "\". ",
      "Margot's Holm and BH intervals have coverage 1 - adjusted p, so they exclude the null ",
      "whatever the adjusted p-value, and an interval decision would be wrong. ",
      "Use adjust = \"bonferroni\" or \"none\". category_rule = \"interval_and_evalue\" ",
      "reproduces output from margot 1.3.2 but carries the same limitation.",
      call. = FALSE
    )
  }
  if (warn) {
    warning(
      "Holm and BH intervals exclude the null by construction, so outcomes whose adjusted ",
      "p-value exceeds alpha may be classed positive or negative. Use this output only to ",
      "reproduce earlier results.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# legend labels for each category level
margot_category_labels <- function(category_rule, null_label) {
  if (category_rule == "interval") {
    return(c(
      positive = paste("Interval above", null_label),
      "includes null" = paste("Interval includes", null_label),
      negative = paste("Interval below", null_label)
    ))
  }
  c(positive = "positive", "not reliable" = "not reliable", negative = "negative")
}

# map colours or shapes supplied for either rule's level names onto the levels in use
margot_category_values <- function(values, category_rule) {
  if (is.list(values)) values <- unlist(values)
  value_names <- names(values)
  if (is.null(value_names)) {
    return(values)
  }
  if (category_rule == "interval" && !"includes null" %in% value_names && "not reliable" %in% value_names) {
    values[["includes null"]] <- values[["not reliable"]]
  }
  if (category_rule == "interval_and_evalue" && !"not reliable" %in% value_names && "includes null" %in% value_names) {
    values[["not reliable"]] <- values[["includes null"]]
  }
  values
}

# show an E-value bound with three decimals, adding decimals only when the
# printed value would equal the threshold or disagree with the full-precision
# flag; as a last resort print enough significant digits to round-trip
format_margot_evalue_bound <- function(x, threshold, comparison, digits = 3, max_digits = 15) {
  if (is.na(x)) {
    return("NA")
  }
  flag <- margot_evalue_flag(x, threshold, comparison)
  for (d in digits:max_digits) {
    printed <- formatC(x, format = "f", digits = d)
    shown <- as.numeric(printed)
    agrees <- identical(margot_evalue_flag(shown, threshold, comparison), flag)
    if (agrees && (shown != threshold || x == threshold)) {
      return(printed)
    }
  }
  sprintf("%.17g", x)
}

# print a number with fixed decimals, without a negative zero
format_margot_fixed <- function(x, digits) {
  if (is.na(x)) {
    return("NA")
  }
  if (round(x, digits) == 0) x <- 0
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

# decimals for one row's original-unit numbers, chosen from the largest of them
margot_unit_digits <- function(values) {
  largest <- max(abs(values), na.rm = TRUE)
  if (!is.finite(largest) || largest < 1) {
    3L
  } else if (largest < 10) {
    2L
  } else if (largest < 100) {
    1L
  } else {
    0L
  }
}

# describe the E-value flag beside a value in prose
margot_evalue_flag_phrase <- function(flag, threshold, comparison) {
  threshold_text <- format(threshold)
  if (is.na(flag)) {
    return(paste0("not compared with ", threshold_text))
  }
  if (comparison == ">") {
    if (flag) paste("exceeds", threshold_text) else paste("does not exceed", threshold_text)
  } else {
    if (flag) paste("at least", threshold_text) else paste("below", threshold_text)
  }
}

# column header for the model-scale estimate
margot_effect_header <- function(effect_type, type, standardize_label) {
  base <- if (identical(effect_type, "naive")) "Association" else effect_type
  if (identical(type, "RR")) {
    return(paste0(base, " (risk ratio)"))
  }
  switch(standardize_label,
    NZ = paste0(base, " (standardised)"),
    US = paste0(base, " (standardized)"),
    base
  )
}

# describe each reported original-scale quantity, with its unit when known
margot_reported_description <- function(quantity, unit) {
  base <- vapply(quantity, function(q) {
    if (is.na(q)) {
      return(NA_character_)
    }
    switch(q,
      mean_difference = "mean difference",
      geometric_mean_ratio = "ratio of geometric means",
      shifted_geometric_mean_ratio = "ratio of geometric means of outcome + 1",
      risk_ratio = "risk ratio",
      stop("Unknown reporting quantity.", call. = FALSE)
    )
  }, character(1), USE.NAMES = FALSE)
  with_unit <- !is.na(base) & base == "mean difference" & !is.na(unit) & nzchar(unit)
  base[with_unit] <- paste0(base[with_unit], " (", unit[with_unit], ")")
  base
}

# capitalise the first letter of a header
margot_capitalise <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

# readable reporting table: one row per outcome, top to bottom as plotted,
# with the display label in an explicit Outcome column
margot_report_table <- function(rows, eff_col, effect_header, category, category_rule,
                                null_label, flag, flag_header, alpha) {
  levels <- normalise_margot_confidence_levels(rows, default_level = 1 - alpha)
  unique_levels <- unique(round(levels, 10))
  ci <- if (length(unique_levels) == 1L) format_margot_ci_level(unique_levels[1]) else "CI"

  # c() of one-element lists keeps repeated names, so a clash is detected below
  # rather than silently replacing a column
  column <- function(name, value) stats::setNames(list(value), name)
  columns <- c(
    column("Outcome", as.character(rows$outcome)),
    column(effect_header, rows[[eff_col]]),
    column(paste(ci, "lower"), rows[["2.5 %"]]),
    column(paste(ci, "upper"), rows[["97.5 %"]])
  )

  # original-scale columns carry their unit, or "original scale", so that they
  # cannot share a header with the model-scale columns
  if ("reported_estimate" %in% names(rows)) {
    description <- margot_reported_description(rows$reporting_quantity, rows$reporting_unit)
    unique_description <- unique(description)
    if (length(unique_description) == 1L && !is.na(unique_description)) {
      quantity <- rows$reporting_quantity[1]
      unit <- rows$reporting_unit[1]
      has_unit <- quantity == "mean_difference" && !is.na(unit) && nzchar(unit)
      estimate_header <- if (has_unit) {
        margot_capitalise(unique_description)
      } else {
        paste0(margot_capitalise(unique_description), " (original scale)")
      }
      suffix <- if (has_unit) unit else "original scale"
      columns <- c(
        columns,
        column(estimate_header, rows$reported_estimate),
        column(paste0(ci, " lower (", suffix, ")"), rows$reported_lower),
        column(paste0(ci, " upper (", suffix, ")"), rows$reported_upper)
      )
    } else {
      columns <- c(
        columns,
        column("Original-scale estimate", rows$reported_estimate),
        column(paste0(ci, " lower (original scale)"), rows$reported_lower),
        column(paste0(ci, " upper (original scale)"), rows$reported_upper),
        column("Original-scale quantity", description)
      )
    }
  }

  if (length(unique_levels) > 1L) {
    columns <- c(columns, column("CI level", levels))
  }
  if (category_rule == "interval") {
    values <- sub("^Interval ", "", margot_category_labels(category_rule, null_label))
    columns <- c(columns, column(paste(ci, "relative to", null_label), unname(values[as.character(category)])))
  } else {
    columns <- c(columns, column("Estimate", as.character(category)))
  }
  if ("E_Value" %in% names(rows)) columns <- c(columns, column("E-value", rows$E_Value))
  if ("E_Val_bound" %in% names(rows)) columns <- c(columns, column("E-value bound", rows$E_Val_bound))
  columns <- c(columns, column(flag_header, ifelse(is.na(flag), NA_character_, ifelse(flag, "yes", "no"))))

  margot_stop_on_duplicate_headers(names(columns))
  out <- data.frame(columns, check.names = FALSE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# stop when two reporting columns would share a header
margot_stop_on_duplicate_headers <- function(headers) {
  duplicated_headers <- unique(headers[duplicated(headers)])
  if (length(duplicated_headers)) {
    stop(
      "Two table columns would share the header ",
      paste0("'", duplicated_headers, "'", collapse = ", "),
      ". Choose a different `rename_ate` or `col_renames` value.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @title Create a Margot Plot with Proper Multiplicity Correction
#' @description
#' Create coordinated plots, tables and text from supplied effect estimates and
#' confidence intervals, with the requested multiplicity adjustment. Model-scale
#' estimates and sensitivity quantities remain separate from reported unit conversions.
#'
#' @param .data data frame containing causal effect estimates with columns for
#'   effect sizes, confidence intervals, E-values and E-value bounds
#' @param type character. type of effect estimate: "RD" (risk difference) or "RR" (risk ratio)
#' @param adjust character. multiplicity correction method: "none", "bonferroni", "holm" or "BH". With `"none"`, the supplied intervals and E-values are used unchanged. `"holm"` and `"BH"` stop under the default interval rule: margot builds their intervals at coverage 1 - adjusted p, so the intervals exclude the null whatever the adjusted p-value and cannot carry an interval decision. `category_rule = "interval_and_evalue"` still runs them, with a warning, to reproduce earlier output; it carries the same limitation.
#' @param alpha numeric. significance level for corrections
#' @param order Outcome ordering rule. The first outcome in the ordering is drawn at the top of the plot (see `order_direction`), and the table and text follow the graph from top to bottom. `"magnitude_desc"` puts the largest absolute effect at the top; `"magnitude_asc"` the smallest; `"evaluebound_desc"` the largest E-value bound; `"alphabetical"` runs A to Z down the plot; `"custom"` follows `custom_order` from the top.
#' @param custom_order Outcome display labels in the requested order, first at the top of the plot (with the default `order_direction`).
#' @param title_binary Retained compatibility argument.
#' @param include_coefficients Whether to print numerical coefficients on the plot.
#' @param standardize_label Axis-label convention: New Zealand, US, or no standardisation label.
#' @param e_val_bound_threshold Reporting threshold for the E-value bound. By default it sets only the E-value flag reported beside each estimate (table column and prose); it does not change the plotted category. Under `category_rule = "interval_and_evalue"` it also gates the category, as in earlier versions.
#' @param category_rule How estimates are classified for colour, legend, table and prose. `"interval"` (default) uses the (adjusted) confidence interval alone: above the null, below the null, or including the null. `"interval_and_evalue"` reproduces margot 1.3.2 and earlier, for reproducing earlier results only: an estimate is "positive" or "negative" only when its interval excludes the null and its E-value bound passes the threshold, and is otherwise labelled "not reliable"; the prose then lists only estimates whose bound passes the threshold.
#' @param e_val_bound_comparison Comparison between the E-value bound and `e_val_bound_threshold`: `">"` (default) flags a bound that exceeds the threshold; `">="` reproduces margot 1.3.2 and earlier, which also flagged a bound equal to it. The comparison uses full precision.
#' @param e_val_bound_conversion How the E-value bound is computed from the adjusted interval of a difference-scale estimate when `adjust` is not `"none"`. `"exact"` (default) converts the limit nearest the null directly, RR = exp(0.91 |d_limit|); `"rounded"` reproduces margot 1.3.2 and earlier, for reproducing earlier results only (see [margot_correct_combined_table()]). Not used when `adjust = "none"`.
#' @param order_direction Where the ordering starts. `"top_to_bottom"` (default) draws the first outcome of `order` at the top. `"bottom_to_top"` reproduces margot 1.3.2 and earlier, for reproducing earlier results only: the first outcome was drawn at the bottom, so `"magnitude_desc"` put the largest effect at the bottom. The table and prose follow the plot from top to bottom under either value.
#' @param table_columns Layout of `transformed_table`. `"report"` (default) gives named reporting columns, described under Value. `"legacy"` reproduces the columns of margot 1.3.2 and earlier, for reproducing earlier results only: the effect column, interval columns headed by their quantiles (for example `"0.21 \%"`), `confidence_level`, `E_Value`, `E_Val_bound` and the `reported_*` and `reporting_*` columns.
#' @param options Plotting options; explicit arguments override corresponding option entries. `colors` may name the levels `"positive"`, `"includes null"` and `"negative"`; a colour named `"not reliable"` is used for `"includes null"` when that level has no colour of its own. `point_shapes` (default up-triangle 24, circle 21 and down-triangle 25 for above, including and below the null) repeats the category in the point shape; `NULL` draws one filled circle for every point, as earlier versions did. `coefficient_colour` (default `"black"`) colours the printed coefficients; `NULL` colours them by category, as earlier versions did. `coefficient_header` (default `"Estimate"`) labels the coefficient column above its top row; `NULL` omits it. `text_size` (default 3.5; 2.75 earlier) sets their size.
#' @param label_mapping Optional mapping from source outcome names to display labels.
#' @param save_output Whether to save the complete reporting list.
#' @param use_timestamp Whether to append a timestamp to saved filenames.
#' @param base_filename Base name for the saved reporting object.
#' @param prefix Optional saved-filename prefix.
#' @param save_path Directory for saved output.
#' @param original_df Legacy unstandardised source data for inferred scale metadata. Explicit saved metadata is preferred.
#' @param bold_rows Whether table row labels whose E-value bound passes the reporting threshold (under `e_val_bound_comparison`) receive Markdown emphasis.
#' @param rename_cols Whether to apply the requested table column-name mapping.
#' @param col_renames Named mapping from new table column names to existing names.
#' @param rename_ate Whether to rename the estimate column, or a supplied replacement name.
#' @param rename_evalue Whether to use display names for E-value columns. The default `"report"` table already uses display names, so this argument affects only `table_columns = "legacy"`.
#' @return An invisible list with `plot`, `interpretation`, and `transformed_table`.
#'
#'   With the default `table_columns = "report"`, `transformed_table` has one row per outcome, ordered as in the plot from top to bottom, with automatic row names. Numbers keep full precision. Its columns are: `"Outcome"`, the display label (in Markdown bold when `bold_rows = TRUE` and the E-value flag is set); the model-scale estimate (for example `"ATE (standardised)"`); its interval limits (for example `"99.58\% CI lower"`, or `"CI lower"` with a `"CI level"` column when coverage varies by outcome); when `scale_info` or `original_df` is supplied, the original-scale estimate and limits, headed by the quantity and unit when every outcome shares them (for example `"Mean difference (scale points)"`, or `"Mean difference (original scale)"` without a unit) and otherwise followed by an `"Original-scale quantity"` column; the decision, headed by the interval and the null (for example `"99.58\% CI relative to 0"`, with values `"above 0"`, `"includes 0"` or `"below 0"`; `"Estimate"` under the legacy category rule); `"E-value"`; `"E-value bound"`; and the E-value flag (`"yes"` or `"no"`), headed by the comparison, for example `"E-value bound > 1.1"`. Two columns never share a header: a clash, for example from `rename_ate`, stops with an error.
#'
#' @details
#' The statistical decision and the E-value flag are reported separately. By default the decision for each outcome follows its (adjusted) confidence interval alone, and the E-value flag records whether the interval's E-value bound exceeds `e_val_bound_threshold`. The flag sits beside the decision and does not change the plotted colour, legend, table decision or which outcomes the prose lists. The prose lists every outcome whose interval excludes the null, in plot order, with its E-value bound and flag, and names the correction and confidence level.
#'
#' To reproduce output from margot 1.3.2 and earlier, set `category_rule = "interval_and_evalue"`, `e_val_bound_comparison = ">="`, `e_val_bound_conversion = "rounded"`, `order_direction = "bottom_to_top"` and `table_columns = "legacy"`. Each argument restores one behaviour and can be set on its own. Together they reproduce the categories, plotted values, table and prose of margot 1.3.2; the figure keeps the current styling (legend centred on the figure, point shapes, black coefficients and sentence-case axis titles), which `options = list(point_shapes = NULL, coefficient_colour = NULL, coefficient_header = NULL, text_size = 2.75)` partly restores. For ratio estimates, the magnitude orderings now rank by the absolute log ratio; supply the earlier order through `order = "custom"` if it is needed.
#' @param scale_info Optional data frame keyed by the original outcome name, with
#'   transformation (`identity`, `log`, or `log1p`), saved `center` and positive
#'   `scale`, `orientation` (1 or -1), `unit`, and positive `unit_multiplier`.
#'   Constants describe the model outcome as orientation * (g(Y) - center) / scale.
#'   Explicit metadata overrides inference from `original_df`.
#' @param ... other parameters as in original function
#'
#' @export
margot_plot <- function(
    .data,
    type = c("RD", "RR"),
    order = c(
      "alphabetical", "magnitude_desc", "magnitude_asc",
      "evaluebound_desc", "evaluebound_asc", "custom", "default"
    ),
    custom_order = NULL,
    title_binary = NULL,
    include_coefficients = TRUE,
    standardize_label = c("NZ", "US", "none"),
    e_val_bound_threshold = 1.2,
    adjust = c("none", "bonferroni", "holm", "BH"),
    alpha = 0.05,
    ...,
    options = list(),
    label_mapping = NULL,
    save_output = FALSE,
    use_timestamp = FALSE,
    base_filename = "margot_plot_output",
    prefix = NULL,
    save_path = here::here("push_mods"),
    original_df = NULL,
    bold_rows = FALSE,
    rename_cols = FALSE,
    col_renames = list(
      "E-Value"       = "E_Value",
      "E-Value bound" = "E_Val_bound"
    ),
    rename_ate = FALSE,
    rename_evalue = FALSE,
    scale_info = NULL,
    category_rule = c("interval", "interval_and_evalue"),
    e_val_bound_comparison = c(">", ">="),
    e_val_bound_conversion = c("exact", "rounded"),
    order_direction = c("top_to_bottom", "bottom_to_top"),
    table_columns = c("report", "legacy")) {
  matched_call <- as.list(match.call(expand.dots = FALSE))
  control_arg_names <- c(
    "type",
    "order",
    "custom_order",
    "title_binary",
    "include_coefficients",
    "standardize_label",
    "e_val_bound_threshold",
    "adjust",
    "alpha",
    "label_mapping",
    "save_output",
    "use_timestamp",
    "base_filename",
    "prefix",
    "save_path",
    "original_df",
    "scale_info",
    "bold_rows",
    "rename_cols",
    "col_renames",
    "rename_ate",
    "rename_evalue",
    "category_rule",
    "e_val_bound_comparison",
    "e_val_bound_conversion",
    "order_direction",
    "table_columns"
  )

  for (nm in control_arg_names) {
    if (!(nm %in% names(matched_call)) && !is.null(options[[nm]])) {
      assign(nm, options[[nm]], envir = environment())
    }
  }

  # match and validate args -------------------------------------------------
  type <- match.arg(type)
  order <- match.arg(order)
  standardize_label <- match.arg(standardize_label)
  adjust <- match.arg(adjust)
  alpha <- as.numeric(alpha)[1]
  category_rule <- match.arg(category_rule)
  e_val_bound_comparison <- match.arg(e_val_bound_comparison)
  e_val_bound_conversion <- match.arg(e_val_bound_conversion)
  order_direction <- match.arg(order_direction)
  table_columns <- match.arg(table_columns)

  # fall back for deprecated order value
  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  # keep raw copy before any correction
  raw_table_df <- .data

  # detect single‑outcome case ---------------------------------------------
  n_outcomes <- nrow(raw_table_df)
  single_outcome <- n_outcomes == 1L

  if (single_outcome && adjust != "none") {
    cli::cli_alert_info("single outcome detected; multiplicity correction skipped")
    adjust <- "none"
  }

  # margot's holm and BH intervals have coverage 1 - adjusted p, so they exclude
  # the null whatever the adjusted p-value and cannot carry an interval decision
  margot_check_interval_rule_adjustment(category_rule, adjust)

  # apply correction -------------------------------------------------------
  if (adjust != "none") {
    cli::cli_alert_info(
      "applying {adjust} correction (alpha = {alpha}) to confidence intervals"
    )
    corrected_table_df <- margot_correct_combined_table(
      raw_table_df,
      adjust = adjust,
      alpha  = alpha,
      scale  = type,
      e_val_bound_conversion = e_val_bound_conversion
    )
    .data <- corrected_table_df
  } else {
    cli::cli_alert_info("no multiplicity adjustment applied")
  }

  if (!"confidence_level" %in% names(.data)) {
    .data$confidence_level <- rep(1 - alpha, nrow(.data))
  } else {
    .data$confidence_level <- normalise_margot_confidence_levels(.data, default_level = 1 - alpha)
  }

  # merge user options with defaults ---------------------------------------
  default_opts <- list(
    title = NULL,
    subtitle = NULL,
    estimate_scale = 1,
    base_size = 18,
    text_size = 3.5,
    point_size = 3,
    title_size = 20,
    subtitle_size = 18,
    legend_text_size = 10,
    legend_title_size = 10,
    x_offset = if (type == "RR") NULL else -0.5,
    x_lim_lo = if (type == "RR") NULL else -0.5,
    x_lim_hi = if (type == "RR") NULL else 0.5,
    linewidth = 0.4,
    plot_theme = NULL,
    colors = c(
      "positive" = "#E69F00",
      "includes null" = "black",
      "not reliable" = "black",
      "negative" = "#56B4E9"
    ),
    point_shapes = c(
      "positive" = 24,
      "includes null" = 21,
      "not reliable" = 21,
      "negative" = 25
    ),
    coefficient_colour = "black",
    coefficient_header = "Estimate",
    facet_var = NULL,
    confidence_level = 0.95,
    annotations = NULL,
    caption = NULL,
    show_evalues = TRUE,
    evalue_digits = 2,
    coefficient_digits = 2,
    remove_tx_prefix = TRUE,
    remove_z_suffix = TRUE,
    use_title_case = TRUE,
    remove_underscores = TRUE
  )

  plot_options <- options[setdiff(names(options), control_arg_names)]
  opts <- modifyList(modifyList(default_opts, plot_options), list(...))

  # coerce logical flags -----------------------------------------------------
  for (nm in c(
    "remove_tx_prefix", "remove_z_suffix",
    "use_title_case", "remove_underscores"
  )) {
    opts[[nm]] <- as.logical(opts[[nm]])[1]
  }

  # effect column -----------------------------------------------------------
  effect_info <- detect_effect_column(.data)
  if (is.null(effect_info)) {
    # fallback to old behavior for compatibility
    eff_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) {
      "E[Y(1)]-E[Y(0)]"
    } else {
      "E[Y(1)]/E[Y(0)]"
    }
    effect_type <- "ATE" # default assumption
  } else {
    eff_col <- effect_info$column
    effect_type <- effect_info$type
  }

  # ensure outcome column ---------------------------------------------------
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    message("added 'outcome' column as it was absent")
  }

  # optional back‑transformation -------------------------------------------
  if (!is.null(original_df) || !is.null(scale_info)) {
    .data <- back_transform_estimates(.data, original_df, scale_info = scale_info, type = type)
  }

  # label transformations ---------------------------------------------------
  .data$outcome <- sapply(
    .data$outcome,
    transform_label,
    label_mapping = label_mapping,
    options       = opts
  )

  # sorting and thresholds --------------------------------------------------
  thresh <- e_val_bound_threshold
  null_val <- ifelse(type == "RR", 1, 0)
  null_label <- margot_null_label(type)

  sorted_df <- group_tab(
    .data,
    type         = type,
    order        = order,
    custom_order = custom_order
  )

  # ggplot draws the first factor level at the bottom, so the levels run
  # bottom to top; by default the first outcome of the ordering is at the top
  sorted_outcomes <- as.character(sorted_df$outcome)
  top_to_bottom <- if (order_direction == "top_to_bottom") sorted_outcomes else rev(sorted_outcomes)
  sorted_df$outcome <- factor(sorted_outcomes, levels = rev(top_to_bottom))

  # interval decision and E-value flag -------------------------------------
  bound_values <- if ("E_Val_bound" %in% names(sorted_df)) sorted_df$E_Val_bound else rep(NA_real_, nrow(sorted_df))
  evalue_flag <- margot_evalue_flag(bound_values, thresh, e_val_bound_comparison)
  sorted_df$Estimate <- margot_interval_category(
    lower = sorted_df$`2.5 %`,
    upper = sorted_df$`97.5 %`,
    null_val = null_val,
    bound = bound_values,
    threshold = thresh,
    category_rule = category_rule,
    comparison = e_val_bound_comparison
  )
  category_labels <- margot_category_labels(category_rule, null_label)
  category_colours <- margot_category_values(opts$colors, category_rule)

  # axis label --------------------------------------------------------------
  lw <- switch(standardize_label,
    NZ = "Standardised",
    US = "Standardized",
    none = "Effect"
  )
  xlab <- if (type == "RR") {
    "Effect (risk ratio)"
  } else if (lw != "Effect") {
    paste0(lw, " effect (difference scale)")
  } else {
    "Effect (difference scale)"
  }

  # build ggplot ------------------------------------------------------------
  out_plot <- ggplot2::ggplot(
    sorted_df,
    ggplot2::aes(
      y = outcome,
      x = !!rlang::sym(eff_col),
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      colour = Estimate
    )
  ) +
    ggplot2::geom_errorbar(
      width      = 0.3,
      linewidth  = opts$linewidth,
      position   = ggplot2::position_dodge(0.3),
      orientation = "y"
    ) +
    ggplot2::geom_vline(xintercept = null_val) +
    ggplot2::scale_color_manual(
      values = category_colours,
      breaks = names(category_labels),
      labels = function(x) unname(category_labels[x])
    ) +
    ggplot2::labs(
      x        = xlab,
      y        = "",
      title    = opts$title,
      subtitle = opts$subtitle,
      caption  = opts$caption %||% margot_ci_caption(sorted_df, adjust = adjust, default_level = 1 - alpha)
    ) +
    ggplot2::coord_cartesian(xlim = c(opts$x_lim_lo, opts$x_lim_hi)) +
    ggplot2::theme_classic(base_size = opts$base_size) +
    ggplot2::theme(
      legend.position   = "top",
      legend.direction  = "horizontal",
      axis.ticks        = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(face = "bold", size = opts$title_size)
    )

  # points: shape repeats the category (up, circle, down) so that above and
  # below the null stay distinct in greyscale; point_shapes = NULL draws one shape
  point_shapes <- opts$point_shapes
  if (is.null(point_shapes)) {
    out_plot <- out_plot + ggplot2::geom_point(
      size     = opts$point_size,
      position = ggplot2::position_dodge(0.3)
    )
  } else {
    point_shapes <- margot_category_values(point_shapes, category_rule)
    out_plot <- out_plot +
      ggplot2::geom_point(
        ggplot2::aes(shape = Estimate, fill = Estimate),
        size     = opts$point_size,
        position = ggplot2::position_dodge(0.3)
      ) +
      ggplot2::scale_fill_manual(
        values = category_colours,
        breaks = names(category_labels),
        labels = function(x) unname(category_labels[x])
      ) +
      ggplot2::scale_shape_manual(
        values = point_shapes,
        breaks = names(category_labels),
        labels = function(x) unname(category_labels[x])
      )
  }

  # the interval legend is self-describing, so it carries no title
  if (category_rule == "interval") {
    out_plot <- out_plot + ggplot2::labs(colour = NULL, fill = NULL, shape = NULL)
  }
  # centre the legend on the whole figure so long outcome labels cannot push it off the page
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    out_plot <- out_plot + ggplot2::theme(legend.location = "plot")
  }

  # risk ratio specific axis formatting
  if (type == "RR") {
    out_plot <- out_plot + ggplot2::scale_x_continuous(
      labels = function(x) ifelse(x < 0, "", as.character(x))
    )
  }

  # add numeric labels if requested ----------------------------------------
  # coefficients aligned along inside edge of plot area
  if (include_coefficients) {
    plot_limits <- compute_margot_plot_limits(
      sorted_df,
      effect_col = eff_col,
      x_lim_lo = opts$x_lim_lo,
      x_lim_hi = opts$x_lim_hi
    )
    x_lim_lo <- plot_limits[1]
    x_lim_hi <- plot_limits[2]

    plot_width <- x_lim_hi - x_lim_lo
    fixed_offset_pct <- 0.05
    coeff_x_position <- opts$x_offset %||% (x_lim_lo + (fixed_offset_pct * plot_width))
    coefficient_labels <- format_margot_coefficient_labels(
      sorted_df[[eff_col]],
      digits = opts$coefficient_digits
    )

    # coefficients are printed in one dark colour for contrast and kept out of
    # the legend; coefficient_colour = NULL colours them by category as before
    text_args <- list(
      mapping = ggplot2::aes(
        x     = coeff_x_position,
        label = coefficient_labels
      ),
      size = opts$text_size,
      hjust = 0,
      fontface = "bold",
      family = "mono",
      show.legend = FALSE
    )
    if (!is.null(opts$coefficient_colour)) text_args$colour <- opts$coefficient_colour
    out_plot <- out_plot + do.call(ggplot2::geom_text, text_args)

    # name the coefficient column above its top row; coefficient_header = NULL omits it
    if (!is.null(opts$coefficient_header) && nzchar(opts$coefficient_header)) {
      out_plot <- out_plot + ggplot2::annotate(
        "text",
        x = coeff_x_position,
        y = nlevels(sorted_df$outcome) + 0.55,
        label = opts$coefficient_header,
        hjust = 0,
        vjust = 0,
        size = opts$text_size,
        fontface = "bold",
        colour = opts$coefficient_colour %||% "black"
      ) +
        # extra room above the top row so that the header is not clipped
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.6, 1.1)))
    }
  }

  # estimate header ---------------------------------------------------------
  # one name for the model-scale estimate in the table and the prose,
  # following rename_ate and any col_renames mapping of that header
  effect_header <- if (is.character(rename_ate)) {
    rename_ate
  } else {
    margot_effect_header(effect_type, type, standardize_label)
  }
  effect_label <- effect_header
  if (rename_cols && length(col_renames) > 0) {
    renamed_to <- names(col_renames)[vapply(col_renames, identical, logical(1), effect_header)]
    if (length(renamed_to)) effect_label <- renamed_to[[1]]
  }

  # interpretation ----------------------------------------------------------
  # pass character labels so that re-sorting cannot follow the plot's factor levels
  interpretation_df <- sorted_df
  interpretation_df$outcome <- sorted_outcomes
  interpretation <- margot_interpret_marginal(
    df                     = interpretation_df,
    type                   = type,
    order                  = order,
    original_df            = NULL,
    custom_order           = custom_order,
    e_val_bound_threshold  = thresh,
    adjust                 = adjust,
    alpha                  = alpha,
    include_adjust_note    = !single_outcome,
    effect_type            = effect_type,
    category_rule          = category_rule,
    e_val_bound_comparison = e_val_bound_comparison,
    order_direction        = order_direction,
    effect_label           = effect_label,
    listed_in_plot_order   = TRUE
  )$interpretation

  # transform table for display -------------------------------------------
  # use the exact same corrected and transformed data as the plot, in plot
  # order from top to bottom
  table_rows <- match(top_to_bottom, sorted_outcomes)

  if (table_columns == "report") {
    transformed_table <- margot_report_table(
      rows = as.data.frame(sorted_df)[table_rows, , drop = FALSE],
      eff_col = eff_col,
      effect_header = effect_header,
      category = sorted_df$Estimate[table_rows],
      category_rule = category_rule,
      null_label = null_label,
      flag = evalue_flag[table_rows],
      flag_header = margot_evalue_flag_header(thresh, e_val_bound_comparison),
      alpha = alpha
    )

    # emphasise before renaming, so that a renamed Outcome column keeps its emphasis
    if (bold_rows) {
      above <- !is.na(evalue_flag[table_rows]) & evalue_flag[table_rows]
      if (any(above)) {
        transformed_table$Outcome[above] <- paste0("**", transformed_table$Outcome[above], "**")
      }
    }

    if (rename_cols && length(col_renames) > 0) {
      for (new_nm in names(col_renames)) {
        old_nm <- col_renames[[new_nm]]
        if (old_nm %in% names(transformed_table)) {
          names(transformed_table)[names(transformed_table) == old_nm] <- new_nm
        }
      }
      margot_stop_on_duplicate_headers(names(transformed_table))
    }
  } else {
    # legacy layout: the columns of margot 1.3.2 and earlier
    table_for_transform <- sorted_df

    # keep only the core columns needed for display
    keep_cols <- c(eff_col, "2.5 %", "97.5 %", "confidence_level", "E_Value", "E_Val_bound",
      "reported_estimate", "reported_lower", "reported_upper", "reporting_quantity", "reporting_unit")
    table_for_transform <- table_for_transform[, intersect(keep_cols, names(table_for_transform)), drop = FALSE]
    table_for_transform <- as.data.frame(table_for_transform)

    # set rownames to outcome names (already transformed)
    rownames(table_for_transform) <- sorted_outcomes
    transformed_table <- table_for_transform[top_to_bottom, , drop = FALSE]

    # optional renaming -----------------------------------------------------
    # handle rename_ate with enhanced logic
    if (!isFALSE(rename_ate)) {
      old_eff <- eff_col
      if (old_eff %in% names(transformed_table)) {
        if (is.character(rename_ate)) {
          # use custom string
          new_name <- rename_ate
        } else if (isTRUE(rename_ate)) {
          # auto-detect appropriate name
          if (eff_col %in% c("ATE", "ATT", "ATC", "ATO")) {
            # already has the right name
            new_name <- eff_col
          } else if (eff_col == "E[Y|A]") {
            # for naive regressions, use special labeling
            new_name <- "E[Y|A] (misspecified)"
          } else {
            # use detected effect_type or default to ATE
            new_name <- effect_type
          }
        }
        names(transformed_table)[names(transformed_table) == old_eff] <- new_name
        eff_col <- new_name
      }
    }

    if (rename_cols && length(col_renames) > 0) {
      for (new_nm in names(col_renames)) {
        old_nm <- col_renames[[new_nm]]
        if (old_nm %in% names(transformed_table)) {
          names(transformed_table)[names(transformed_table) == old_nm] <- new_nm
        }
      }
    }

    # handle rename_evalue
    if (rename_evalue) {
      if ("E_Value" %in% names(transformed_table)) {
        names(transformed_table)[names(transformed_table) == "E_Value"] <- "E-Value"
      }
      if ("E_Val_bound" %in% names(transformed_table)) {
        names(transformed_table)[names(transformed_table) == "E_Val_bound"] <- "E-Value Bound"
      }
    }

    if (bold_rows) {
      # determine the correct column name for E-value bound
      bound_nm <- if ("E-Value Bound" %in% names(transformed_table)) {
        "E-Value Bound" # from rename_evalue
      } else if ("E-Value bound" %in% names(transformed_table)) {
        "E-Value bound" # from rename_cols
      } else {
        "E_Val_bound" # original
      }
      if (bound_nm %in% names(transformed_table)) {
        above <- margot_evalue_flag(transformed_table[[bound_nm]], e_val_bound_threshold, e_val_bound_comparison)
        if (any(above)) {
          rn <- rownames(transformed_table)
          rownames(transformed_table)[above] <- paste0("**", rn[above], "**")
        }
      }
    }

    ci_headers <- margot_ci_column_names(sorted_df, default_level = 1 - alpha)
    if ("2.5 %" %in% names(transformed_table)) {
      names(transformed_table)[names(transformed_table) == "2.5 %"] <- ci_headers[1]
    }
    if ("97.5 %" %in% names(transformed_table)) {
      names(transformed_table)[names(transformed_table) == "97.5 %"] <- ci_headers[2]
    }
  }

  # optional save -----------------------------------------------------------
  if (save_output) {
    filename <- paste0(
      prefix %||% "",
      base_filename,
      if (use_timestamp) paste0("_", format(Sys.time(), "%Y%m%d%H%M%S")) else "",
      ""
    )
    here_save(
      list(
        plot              = out_plot,
        interpretation    = interpretation,
        transformed_table = transformed_table
      ),
      filename,
      dir_path = save_path
    )
  }

  invisible(list(
    plot              = out_plot,
    interpretation    = interpretation,
    transformed_table = transformed_table
  ))
}


# -------------------------------------------------------------------------
# helper: interpretation with optional adjustment note --------------------
# -------------------------------------------------------------------------

#' @title Make Interpretation of ATE Results
#' @description
#' Helper that assembles a concise Markdown interpretation of the results. By
#' default it lists every outcome whose confidence interval excludes the null,
#' in plot order from top to bottom, with its E-value bound and E-value flag,
#' and states the correction and confidence level. When
#' `include_adjust_note = FALSE` (the default for a single-outcome call from
#' `margot_plot()`), the correction is not mentioned; the confidence level is
#' still stated.
#'
#' @inheritParams margot_plot
#' @param df Estimate table accepted by [margot_plot()], already corrected for
#'   multiplicity when `adjust` is not `"none"`.
#' @param effect_type Label identifying ATE, ATT, ATC, ATO or an association.
#' @param include_adjust_note logical; if `FALSE`, any reference to adjustment
#'   methods is omitted. default `TRUE`.
#' @param effect_label Name for the model-scale estimate in each bullet, for
#'   example `"ATE (standardised)"`; `NULL` uses the effect type.
#' @param listed_in_plot_order logical; `TRUE` when the prose accompanies a
#'   plot, so that it says the outcomes are listed in plot order.
#'
#' @return list with one element, `interpretation` (a character string).
#' @keywords internal
margot_interpret_marginal <- function(
    df,
    type = c("RD", "RR"),
    order = c(
      "alphabetical", "magnitude_desc", "magnitude_asc",
      "evaluebound_desc", "evaluebound_asc", "custom", "default"
    ),
    original_df = NULL,
    e_val_bound_threshold = 1,
    adjust = c("none", "bonferroni", "holm", "BH"),
    alpha = 0.05,
    include_adjust_note = TRUE,
    effect_type = "ATE",
    scale_info = NULL,
    custom_order = NULL,
    category_rule = c("interval", "interval_and_evalue"),
    e_val_bound_comparison = c(">", ">="),
    order_direction = c("top_to_bottom", "bottom_to_top"),
    effect_label = NULL,
    listed_in_plot_order = FALSE) {
  type <- match.arg(type)
  order <- match.arg(order)
  adjust <- match.arg(adjust)
  alpha <- as.numeric(alpha)[1]
  category_rule <- match.arg(category_rule)
  e_val_bound_comparison <- match.arg(e_val_bound_comparison)
  order_direction <- match.arg(order_direction)
  # margot_plot() has already warned about a legacy holm or BH call
  margot_check_interval_rule_adjustment(category_rule, adjust, warn = !listed_in_plot_order)
  if (is.null(effect_label)) {
    effect_label <- margot_effect_header(effect_type, type, "none")
  }

  if (!"confidence_level" %in% names(df)) {
    df$confidence_level <- rep(1 - alpha, nrow(df))
  } else {
    df$confidence_level <- normalise_margot_confidence_levels(df, default_level = 1 - alpha)
  }

  if (category_rule == "interval_and_evalue") {
    return(margot_interpret_marginal_legacy(
      df = df,
      type = type,
      order = order,
      original_df = original_df,
      e_val_bound_threshold = e_val_bound_threshold,
      adjust = adjust,
      alpha = alpha,
      include_adjust_note = include_adjust_note,
      effect_type = effect_type,
      scale_info = scale_info,
      custom_order = custom_order,
      e_val_bound_comparison = e_val_bound_comparison,
      order_direction = order_direction
    ))
  }

  # sort as plotted, top to bottom, after any back-transformation -----------
  if (!is.null(original_df) || !is.null(scale_info)) {
    df <- back_transform_estimates(df, original_df, scale_info = scale_info, type = type)
  }
  df <- group_tab(df, type = type, order = order, custom_order = custom_order)
  if (order_direction == "bottom_to_top") {
    df <- df[rev(seq_len(nrow(df))), , drop = FALSE]
  }

  effect_col <- detect_effect_column(df)$column
  null_val <- if (type == "RR") 1 else 0
  null_label <- margot_null_label(type)
  threshold_text <- format(e_val_bound_threshold)

  # decision from the interval alone; the E-value flag is reported beside it
  category <- margot_interval_category(
    lower = df[["2.5 %"]],
    upper = df[["97.5 %"]],
    null_val = null_val,
    category_rule = "interval"
  )
  bound <- if ("E_Val_bound" %in% names(df)) df$E_Val_bound else rep(NA_real_, nrow(df))
  flag <- margot_evalue_flag(bound, e_val_bound_threshold, e_val_bound_comparison)
  excludes <- !is.na(category) & category != "includes null"
  includes <- !is.na(category) & category == "includes null"

  n_outcomes <- nrow(df)
  n_excludes <- sum(excludes)
  interval_noun <- if (adjust == "none") "confidence interval" else "adjusted confidence interval"
  interval_article <- if (adjust == "none") "a" else "an"

  # correction and confidence level ------------------------------------------
  unique_levels <- unique(round(df$confidence_level, 10))
  level_phrase <- if (length(unique_levels) == 1L) {
    paste0(format_margot_percent(unique_levels[1]), " confidence intervals")
  } else {
    "confidence intervals whose coverage varies by outcome"
  }
  correction_sentence <- if (!include_adjust_note) {
    paste0("Intervals are ", level_phrase, ".")
  } else {
    switch(adjust,
      none = paste0("Intervals are ", level_phrase, ", with no adjustment for multiple comparisons."),
      bonferroni = paste0(
        "Confidence intervals were adjusted for ", n_outcomes,
        " comparisons using the Bonferroni correction (family-wise alpha = ", format(alpha),
        "), giving ", level_phrase, "."
      ),
      holm = paste0(
        "Confidence intervals were adjusted for ", n_outcomes,
        " comparisons using the Holm correction (family-wise alpha = ", format(alpha),
        "), giving ", level_phrase, "."
      ),
      BH = paste0(
        "Confidence intervals were adjusted for ", n_outcomes,
        " comparisons using the Benjamini-Hochberg correction (false discovery rate = ", format(alpha),
        "), giving ", level_phrase, "."
      )
    )
  }
  if (adjust != "none") {
    correction_sentence <- paste(correction_sentence, "E-value bounds were computed from the adjusted intervals.")
  }

  # count of outcomes whose interval excludes the null -----------------------
  effect_desc <- switch(effect_type,
    "ATE" = "average treatment effects",
    "ATT" = "average treatment effects on the treated",
    "ATC" = "average treatment effects on the control",
    "ATO" = "average treatment effects in the overlap population",
    "naive" = "naive regression associations (ignoring confounding)",
    "treatment effects" # fallback
  )
  count_sentence <- if (n_outcomes == 1L) {
    paste0(
      "The ", interval_noun, " for this estimate ",
      if (n_excludes == 1L) "excludes " else "includes ", null_label, "."
    )
  } else if (n_excludes == 0L) {
    paste0("The ", interval_noun, "s of all ", n_outcomes, " ", effect_desc, " include ", null_label, ".")
  } else {
    paste0(
      "Of the ", n_outcomes, " ", effect_desc, ", ", n_excludes,
      if (n_excludes == 1L) paste0(" has ", interval_article, " ", interval_noun, " that excludes ") else paste0(" have ", interval_noun, "s that exclude "),
      null_label,
      if (n_excludes == 1L) {
        "."
      } else if (listed_in_plot_order) {
        ". They are listed in plot order, from top to bottom."
      } else {
        ". They are listed in the requested order."
      }
    )
  }
  if (effect_type == "naive") {
    count_sentence <- paste0(
      count_sentence,
      "\n\n**Warning:** These are naive associations that ignore confounding and should NOT be interpreted causally."
    )
  }

  # explain the E-value flag once, before the list ---------------------------
  evalue_sentence <- if (n_excludes > 0L) {
    paste0(
      "The E-value bound is the smallest risk ratio that an unmeasured confounder would need ",
      "with both the treatment and the outcome, beyond the measured covariates, to move the ",
      "interval limit nearest ", null_label, " to ", null_label, ". ",
      if (type == "RD") {
        "For these difference-scale estimates it uses the approximation RR = exp(0.91 |d|), where d is the standardised difference. "
      } else {
        ""
      },
      "The note in brackets says whether the bound ",
      if (e_val_bound_comparison == ">") "exceeds " else "is at least ",
      threshold_text, "; the threshold does not change which outcomes are listed."
    )
  } else {
    NULL
  }

  # one bullet per outcome whose interval excludes the null, in plot order --
  # model-scale numbers use fixed decimals for their scale (3 for differences,
  # 2 for ratios); original-unit numbers share one choice within each bullet
  model_digits <- if (type == "RR") 2L else 3L
  bullets <- vapply(which(excludes), function(i) {
    row <- df[i, , drop = FALSE]
    ci_label <- format_margot_ci_level(row$confidence_level)
    lab <- paste0(effect_label, " = ", format_margot_fixed(row[[effect_col]], model_digits), " (", ci_label, ": ",
      format_margot_fixed(row[["2.5 %"]], model_digits), " to ",
      format_margot_fixed(row[["97.5 %"]], model_digits), ")")
    reported <- ""
    if ("reported_estimate" %in% names(row) && !is.na(row$reported_estimate)) {
      quantity <- switch(row$reporting_quantity,
        mean_difference = "on the original scale, mean difference",
        geometric_mean_ratio = "ratio of geometric means",
        shifted_geometric_mean_ratio = "ratio of geometric means of outcome + 1",
        risk_ratio = "risk ratio",
        stop("Unknown reporting quantity.", call. = FALSE))
      unit <- if (row$reporting_quantity == "mean_difference" && !is.na(row$reporting_unit) && nzchar(row$reporting_unit)) paste0(" ", row$reporting_unit) else ""
      reported_values <- c(row$reported_estimate, row$reported_lower, row$reported_upper)
      reported_digits <- if (row$reporting_quantity == "mean_difference") margot_unit_digits(reported_values) else 2L
      reported <- paste0("; ", quantity, " = ", format_margot_fixed(row$reported_estimate, reported_digits), unit,
        " (", ci_label, ": ", format_margot_fixed(row$reported_lower, reported_digits), " to ",
        format_margot_fixed(row$reported_upper, reported_digits), ")")
    }
    bound_text <- format_margot_evalue_bound(bound[i], e_val_bound_threshold, e_val_bound_comparison)
    paste0("- ", row$outcome, ": ", lab, reported, ". E-value bound = ", bound_text,
      " (", margot_evalue_flag_phrase(flag[i], e_val_bound_threshold, e_val_bound_comparison), ").")
  }, character(1))

  # outcomes whose interval includes the null, in plot order -----------------
  remaining_sentence <- if (n_excludes > 0L && any(includes)) {
    others <- as.character(df$outcome[includes])
    paste0(
      if (length(others) == 1L) {
        paste0("The ", interval_noun, " of the other outcome includes ", null_label, ": ")
      } else {
        paste0("The ", interval_noun, "s of the other ", length(others), " outcomes include ", null_label, ": ")
      },
      paste(others, collapse = "; "), "."
    )
  } else {
    NULL
  }

  paragraphs <- c(
    correction_sentence,
    count_sentence,
    evalue_sentence,
    if (length(bullets)) paste(bullets, collapse = "\n") else NULL,
    remaining_sentence
  )
  list(interpretation = paste(paragraphs, collapse = "\n\n"))
}

# legacy prose of margot 1.3.2 and earlier: lists only estimates whose E-value
# bound passes the threshold, whether or not the interval excludes the null.
# retained for reproducing earlier results under category_rule = "interval_and_evalue".
margot_interpret_marginal_legacy <- function(
    df,
    type,
    order,
    original_df,
    e_val_bound_threshold,
    adjust,
    alpha,
    include_adjust_note,
    effect_type,
    scale_info,
    custom_order,
    e_val_bound_comparison,
    order_direction) {
  # build adjustment sentences only when requested ------------------------
  if (include_adjust_note) {
    m <- nrow(df)
    ci_note <- margot_ci_caption(df, adjust = adjust, default_level = 1 - alpha)

    ci_sentence <- switch(adjust,
      none = if (!is.null(ci_note)) paste0(ci_note, ".") else
        paste0("Confidence intervals were reported as ", format_margot_ci_level(1 - alpha), "."),
      bonferroni = paste0(
        "Confidence intervals and E-values were adjusted for ",
        m, " comparisons using Bonferroni correction",
        if (!is.null(ci_note)) paste0(" (", ci_note, ").") else "."
      ),
      holm = paste0(
        "Confidence intervals and E-values were adjusted using Holm correction",
        if (!is.null(ci_note)) paste0(" (", ci_note, ").") else "."
      ),
      BH = paste0(
        "Confidence intervals and E-values were adjusted using BH correction",
        if (!is.null(ci_note)) paste0(" (", ci_note, ").") else "."
      )
    )

    ev_sentence <- switch(adjust,
      none = "No adjustment was made for family‑wise error rates to E‑values.",
      bonferroni = NULL,
      holm = NULL,
      BH = NULL
    )
    adj_note <- paste(ci_sentence, ev_sentence)
  } else {
    adj_note <- ""
  }

  # sort and optionally back-transform ------------------------------------
  if (!is.null(original_df) || !is.null(scale_info)) {
    df <- back_transform_estimates(df, original_df, scale_info = scale_info, type = type)
  }
  df <- group_tab(df, type = type, order = order, custom_order = custom_order)

  # identify columns -------------------------------------------------------
  effect_info <- detect_effect_column(df)
  effect_col <- effect_info$column

  # filter estimates whose bound passes the threshold ------------------------
  passes <- margot_evalue_flag(df$E_Val_bound, e_val_bound_threshold, e_val_bound_comparison)
  df_f <- df[!is.na(df$E_Value) & df$E_Value >= 1 & !is.na(passes) & passes, , drop = FALSE]

  if (nrow(df_f) == 0) {
    no_effects_msg <- "No outcomes meet the specified E-value reporting threshold."
    interpretation_text <- if (nzchar(adj_note)) {
      paste0(adj_note, "\n\n", no_effects_msg)
    } else {
      no_effects_msg
    }
    return(list(interpretation = interpretation_text))
  }

  # follow the plot from top to bottom ---------------------------------------
  if (order_direction == "bottom_to_top") {
    df_f <- df_f[rev(seq_len(nrow(df_f))), , drop = FALSE]
  }

  # create appropriate description based on effect type
  effect_desc <- switch(effect_type,
    "ATE" = "average treatment effects",
    "ATT" = "average treatment effects on the treated",
    "ATC" = "average treatment effects on the control",
    "ATO" = "average treatment effects in the overlap population",
    "naive" = "naive regression associations (ignoring confounding)",
    "treatment effects" # fallback
  )

  intro <- if (effect_type == "naive") {
    glue::glue(
      "The following outcomes show associations in {effect_desc} ",
      "(E‑value lower bound {e_val_bound_comparison} {e_val_bound_threshold}):\n\n",
      "**Warning:** These are naive associations that ignore confounding and should NOT be interpreted causally.\n\n\n"
    )
  } else {
    glue::glue(
      "The following estimates of {effect_desc} meet the specified reporting threshold ",
      "(E‑value lower bound {e_val_bound_comparison} {e_val_bound_threshold}):\n\n\n"
    )
  }

  # describe the shared numerical result without a second transformation or reference mean.
  bullets <- vapply(seq_len(nrow(df_f)), function(i) {
    row <- df_f[i, , drop = FALSE]
    ci_label <- format_margot_ci_level(row$confidence_level)
    lab <- paste0(format_minimal_decimals(row[[effect_col]]), " (", ci_label, ": ",
      format_minimal_decimals(row[["2.5 %"]]), " to ", format_minimal_decimals(row[["97.5 %"]]), ")")
    reported <- ""
    if ("reported_estimate" %in% names(row) && !is.na(row$reported_estimate)) {
      quantity <- switch(row$reporting_quantity,
        mean_difference = "on the original scale, mean difference",
        geometric_mean_ratio = "ratio of geometric means",
        shifted_geometric_mean_ratio = "ratio of geometric means of outcome + 1",
        risk_ratio = "risk ratio",
        stop("Unknown reporting quantity.", call. = FALSE))
      unit <- if (row$reporting_quantity == "mean_difference" && !is.na(row$reporting_unit) && nzchar(row$reporting_unit)) paste0(" ", row$reporting_unit) else ""
      reported <- paste0("; ", quantity, " = ", format_minimal_decimals(row$reported_estimate), unit,
        " (", ci_label, ": ", format_minimal_decimals(row$reported_lower), " to ",
        format_minimal_decimals(row$reported_upper), ")")
    }
    paste0("- ", row$outcome, ": ", lab, reported, ". E-value bound = ",
      format_minimal_decimals(row$E_Val_bound, 2))
  }, character(1))

  interpretation_text <- paste0(
    if (nzchar(adj_note)) paste0(adj_note, "\n\n") else "",
    intro,
    paste(bullets, collapse = "\n")
  )

  list(interpretation = interpretation_text)
}
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         title,
#                         subtitle,
#                         estimate_scale = 1,
#                         base_size = 11,
#                         text_size = 2.75,
#                         point_size = .5,
#                         title_size = 10,
#                         subtitle_size = 9,
#                         legend_text_size = 6,
#                         legend_title_size = 6,
#                         x_offset = ifelse(type == "RR", 0, -1.75),
#                         x_lim_lo = ifelse(type == "RR", .1, -1.75),
#                         x_lim_hi = ifelse(type == "RR", 2.5, 1),
#                         linewidth = .5,
#                         plot_theme = NULL){
#   type <- match.arg(type)
#
#   # dynamic theme adjustment
#   if (is.null(plot_theme)) {
#     plot_theme <- theme_classic(base_size = base_size)
#   } else {
#     # Apply base_size to the plot_theme
#     plot_theme <- plot_theme + theme(text = element_text(size = base_size))
#   }
#
#
#   # Copy data to avoid direct manipulation
#   plot_data <- .data
#   # Define reliability based on type
#   reliability_condition <- if (type == "RR") {
#     list(condition = c(1, 1), label = "Causal risk ratio scale")
#   } else {
#     list(condition = c(0, 0), label = "Causal difference scale")
#   }
#
#   plot_data$Reliability <- ifelse(
#     plot_data$`2.5 %` > reliability_condition$condition[1] & plot_data$`97.5 %` > reliability_condition$condition[2],
#     "positive",
#     ifelse(
#       plot_data$`2.5 %` < reliability_condition$condition[1] & plot_data$`97.5 %` < reliability_condition$condition[2],
#       "negative",
#       "zero_crossing"
#     )
#   )
#
#   # Start building the plot
#   x_name <- paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")
#   out <- ggplot(
#     data = plot_data,
#     aes(
#       y = reorder(outcome, .data[[x_name]]),
#       x = .data[[x_name]],
#       xmin = .data$`2.5 %`,
#       xmax = .data$`97.5 %`,
#       group = Estimate,
#       color = Reliability
#     )
#   ) + geom_errorbarh(aes(color = Reliability), height = .3,
#                      linewidth = linewidth, position = position_dodge(width = .3)) +
#     geom_point(size = point_size, position = position_dodge(width = 0.3)) +
#     geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
#     scale_color_manual(values = c("positive" = "dodgerblue", "zero_crossing" = "black", "negative" = "orange")) +
#     labs(x = reliability_condition$label, y = NULL, title = title, subtitle = subtitle) +
#     geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = ifelse(plot_data$Estimate == "unreliable", "plain", "bold")) +
#     coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
#     plot_theme +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.title = element_text(face = "bold", size = title_size, hjust = 0),
#       plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
#       legend.text = element_text(size = legend_text_size),
#       legend.title = element_text(size = legend_title_size),
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#     )
#
#   # Conditionally add x-axis scale modifications
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + scale_x_continuous(labels = custom_x_labels)
#   }
#
#   return(out)
# }
