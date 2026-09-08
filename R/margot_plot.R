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

#' @title Create a Margot Plot with Proper Multiplicity Correction
#' @description
#' Create coordinated plots, tables and text from supplied effect estimates and
#' confidence intervals, with the requested multiplicity adjustment. Model-scale
#' estimates and sensitivity quantities remain separate from reported unit conversions.
#'
#' @param .data data frame containing causal effect estimates with columns for
#'   effect sizes, confidence intervals, E-values and E-value bounds
#' @param type character. type of effect estimate: "RD" (risk difference) or "RR" (risk ratio)
#' @param adjust character. multiplicity correction method: "none", "bonferroni"
#' @param alpha numeric. significance level for corrections
#' @param order Outcome ordering rule; the table and text follow the graph from top to bottom.
#' @param custom_order Outcome labels in the requested custom factor order.
#' @param title_binary Retained compatibility argument.
#' @param include_coefficients Whether to print numerical coefficients on the plot.
#' @param standardize_label Axis-label convention: New Zealand, US, or no standardisation label.
#' @param e_val_bound_threshold E-value lower-bound threshold for colouring and prose selection.
#' @param options Plotting options; explicit arguments override corresponding option entries.
#' @param label_mapping Optional mapping from source outcome names to display labels.
#' @param save_output Whether to save the complete reporting list.
#' @param use_timestamp Whether to append a timestamp to saved filenames.
#' @param base_filename Base name for the saved reporting object.
#' @param prefix Optional saved-filename prefix.
#' @param save_path Directory for saved output.
#' @param original_df Legacy unstandardised source data for inferred scale metadata. Explicit saved metadata is preferred.
#' @param bold_rows Whether table row labels above the reporting threshold receive Markdown emphasis.
#' @param rename_cols Whether to apply the requested table column-name mapping.
#' @param col_renames Named mapping from new table column names to existing names.
#' @param rename_ate Whether to rename the estimate column, or a supplied replacement name.
#' @param rename_evalue Whether to use display names for E-value columns.
#' @return An invisible list with `plot`, `interpretation`, and `transformed_table`.
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
    scale_info = NULL) {
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
    "rename_evalue"
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

  # apply correction -------------------------------------------------------
  if (adjust != "none") {
    cli::cli_alert_info(
      "applying {adjust} correction (alpha = {alpha}) to confidence intervals"
    )
    corrected_table_df <- margot_correct_combined_table(
      raw_table_df,
      adjust = adjust,
      alpha  = alpha,
      scale  = type
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
    text_size = 2.75,
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
      "not reliable" = "black",
      "negative" = "#56B4E9"
    ),
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

  sorted_df <- group_tab(
    .data,
    type         = type,
    order        = order,
    custom_order = custom_order
  )
  sorted_df$outcome <- factor(sorted_df$outcome, levels = sorted_df$outcome)

  # categorise estimates ----------------------------------------------------
  cat_vec <- with(
    sorted_df,
    ifelse(
      E_Val_bound >= thresh & `2.5 %` > null_val & `97.5 %` > null_val, "positive",
      ifelse(
        E_Val_bound >= thresh & `2.5 %` < null_val & `97.5 %` < null_val, "negative",
        "not reliable"
      )
    )
  )
  sorted_df$Estimate <- factor(cat_vec, levels = c("positive", "not reliable", "negative"))

  # axis label --------------------------------------------------------------
  lw <- switch(standardize_label,
    NZ = "Standardised",
    US = "Standardized",
    none = "Effect"
  )
  xlab <- if (type == "RR") {
    "Effect (Risk Ratio)"
  } else if (lw != "Effect") {
    paste0(lw, " Effect (Difference Scale)")
  } else {
    "Effect (Difference Scale)"
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
    ggplot2::geom_point(
      size      = opts$point_size,
      position  = ggplot2::position_dodge(0.3)
    ) +
    ggplot2::geom_vline(xintercept = null_val) +
    ggplot2::scale_color_manual(values = opts$colors) +
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
    
    out_plot <- out_plot + ggplot2::geom_text(
      ggplot2::aes(
        x     = coeff_x_position,
        label = coefficient_labels
      ),
      size = opts$text_size,
      hjust = 0,
      fontface = "bold",
      family = "mono"
    )
  }

  # interpretation ----------------------------------------------------------
  interpretation <- margot_interpret_marginal(
    df                    = sorted_df,
    type                  = type,
    order                 = order,
    original_df           = NULL,
    custom_order          = custom_order,
    e_val_bound_threshold = thresh,
    adjust                = adjust,
    alpha                 = alpha,
    include_adjust_note   = !single_outcome,
    effect_type           = effect_type
  )$interpretation

  # transform table for display -------------------------------------------
  # use the exact same corrected and transformed data as the plot
  table_for_transform <- sorted_df

  # keep only the core columns needed for display
  keep_cols <- c(eff_col, "2.5 %", "97.5 %", "confidence_level", "E_Value", "E_Val_bound",
    "reported_estimate", "reported_lower", "reported_upper", "reporting_quantity", "reporting_unit")
  table_for_transform <- table_for_transform[, intersect(keep_cols, names(table_for_transform)), drop = FALSE]
  table_for_transform <- as.data.frame(table_for_transform)

  # set rownames to outcome names (already transformed)
  rownames(table_for_transform) <- as.character(sorted_df$outcome)

  # create transformed_table with proper ordering (reverse for ascending order)
  plot_outcome_names <- as.character(sorted_df$outcome)
  outcome_order <- rev(plot_outcome_names)
  transformed_table <- table_for_transform[outcome_order, , drop = FALSE]

  # optional renaming -------------------------------------------------------
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
      above <- transformed_table[[bound_nm]] >= e_val_bound_threshold
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
#' helper that assembles a concise markdown‑style interpretation of the results.
#' when `include_adjust_note = FALSE` (the default for a single‑outcome call
#' from `margot_plot()`), statements about multiplicity correction are
#' suppressed to avoid unnecessary noise.
#'
#' @inheritParams margot_interpret_marginal
#' @param df Estimate table accepted by [margot_plot()].
#' @param effect_type Label identifying ATE, ATT, ATC, ATO or an association.
#' @param include_adjust_note logical; if `FALSE`, any reference to adjustment
#'   methods is omitted. default `TRUE`.
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
    custom_order = NULL) {
  type <- match.arg(type)
  order <- match.arg(order)
  adjust <- match.arg(adjust)
  alpha <- as.numeric(alpha)[1]

  if (!"confidence_level" %in% names(df)) {
    df$confidence_level <- rep(1 - alpha, nrow(df))
  } else {
    df$confidence_level <- normalise_margot_confidence_levels(df, default_level = 1 - alpha)
  }

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

  # sort and optionally back‑transform ------------------------------------
  if (!is.null(original_df) || !is.null(scale_info)) {
    df <- back_transform_estimates(df, original_df, scale_info = scale_info, type = type)
  }
  df <- group_tab(df, type = type, order = order, custom_order = custom_order)

  # identify columns -------------------------------------------------------
  # use the helper function to detect effect column
  effect_info <- detect_effect_column(df)
  effect_col <- effect_info$column
  null_val <- if (type == "RR") 1 else 0

  # filter reliable effects -----------------------------------------------
  df_f <- df %>%
    dplyr::filter(E_Value >= 1, E_Val_bound >= e_val_bound_threshold)

  if (nrow(df_f) == 0) {
    no_effects_msg <- "No outcomes meet the specified E-value reporting threshold."
    interpretation_text <- if (nzchar(adj_note)) {
      paste0(adj_note, "\n\n", no_effects_msg)
    } else {
      no_effects_msg
    }
    return(list(interpretation = interpretation_text))
  }

  # preserve requested ordering -------------------------------------------
  df_f <- df_f[rev(seq_len(nrow(df_f))), , drop = FALSE]

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
      "(E‑value lower bound >= {e_val_bound_threshold}):\n\n",
      "**Warning:** These are naive associations that ignore confounding and should NOT be interpreted causally.\n\n\n"
    )
  } else {
    glue::glue(
      "The following estimates of {effect_desc} meet the specified reporting threshold ",
      "(E‑value lower bound >= {e_val_bound_threshold}):\n\n\n"
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
