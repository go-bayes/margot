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

#' @title Create a Margot Plot with Proper Multiplicity Correction
#' @description
#' Create a margot plot for visualising causal effects with proper simultaneous
#' confidence intervals using multcomp for family-wise error rate control.
#'
#' @param .data data frame containing causal effect estimates with columns for
#'   effect sizes, confidence intervals, E-values and E-value bounds
#' @param type character. type of effect estimate: "RD" (risk difference) or "RR" (risk ratio)
#' @param adjust character. multiplicity correction method: "none", "bonferroni"
#' @param alpha numeric. significance level for corrections
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
    adjust = c("none", "bonferroni"),
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
    rename_evalue = FALSE) {
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

  # merge user options with defaults early (needed for CI/E-value plumbing)
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
    x_offset = NULL,
    x_lim_lo = NULL,
    x_lim_hi = NULL,
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
    show_evalues = TRUE,
    evalue_digits = 2,
    remove_tx_prefix = TRUE,
    remove_z_suffix = TRUE,
    use_title_case = TRUE,
    remove_underscores = TRUE,
    # new: parameters for continuous-exposure E-values and intervention context
    delta_exposure = 1,
    sd_outcome = 1,
    intervention_type = "exposure_shift"
  )

  opts <- modifyList(modifyList(default_opts, options), list(...))

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
      "applying {adjust} correction (α = {alpha}) to confidence intervals"
    )
    corrected_table_df <- margot_correct_combined_table(
      raw_table_df,
      adjust = adjust,
      alpha  = alpha,
      scale  = type,
      delta  = opts$delta_exposure,
      sd     = opts$sd_outcome
    )
    .data <- corrected_table_df
  } else {
    cli::cli_alert_info("no multiplicity adjustment applied")
  }

  # rest of function remains the same...
  # [continuing with existing code for plotting, interpretation, etc.]

  # opts already prepared above

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
  if (!is.null(original_df)) {
    .data <- back_transform_estimates(.data, original_df)
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
      E_Val_bound > thresh & `2.5 %` > null_val & `97.5 %` > null_val, "positive",
      ifelse(
        E_Val_bound > thresh & `2.5 %` < null_val & `97.5 %` < null_val, "negative",
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
    ggplot2::geom_errorbarh(
      height    = 0.3,
      linewidth = opts$linewidth,
      position  = ggplot2::position_dodge(0.3)
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
      subtitle = opts$subtitle
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
    # get plot limits to determine positioning
    if (is.null(opts$x_lim_lo) || is.null(opts$x_lim_hi)) {
      # calculate default limits based on data range
      data_range <- range(c(sorted_df[[eff_col]], sorted_df$`2.5 %`, sorted_df$`97.5 %`), na.rm = TRUE)
      x_range <- diff(data_range)
      x_lim_lo <- data_range[1] - 0.1 * x_range
      x_lim_hi <- data_range[2] + 0.1 * x_range
    } else {
      x_lim_lo <- opts$x_lim_lo
      x_lim_hi <- opts$x_lim_hi
    }
    
    # position coefficients at fixed offset from the left edge of plot area
    plot_width <- x_lim_hi - x_lim_lo
    fixed_offset_pct <- 0.05  # 5% from left edge of plot area
    coeff_x_position <- x_lim_lo + (fixed_offset_pct * plot_width)
    
    out_plot <- out_plot + ggplot2::geom_text(
      ggplot2::aes(
        x     = coeff_x_position,
        label = sprintf("%.2f", !!rlang::sym(eff_col))
      ),
      size = opts$text_size,
      hjust = 0,  # left-align text
      fontface = "bold"
    )
  }

  # interpretation ----------------------------------------------------------
  interpretation <- margot_interpret_marginal(
    df                    = sorted_df,
    type                  = type,
    order                 = order,
    original_df           = original_df,
    e_val_bound_threshold = thresh,
    adjust                = adjust,
    alpha                 = alpha,
    include_adjust_note   = !single_outcome,
    effect_type           = effect_type,
    intervention_type     = opts$intervention_type,
    delta                 = opts$delta_exposure,
    sd                    = opts$sd_outcome
  )$interpretation

  # transform table for display -------------------------------------------
  # use the exact same corrected and transformed data as the plot
  table_for_transform <- sorted_df

  # keep only the core columns needed for display
  keep_cols <- c(eff_col, "2.5 %", "97.5 %", "E_Value", "E_Val_bound")
  table_for_transform <- table_for_transform[, intersect(keep_cols, names(table_for_transform)), drop = FALSE]

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
    above <- transformed_table[[bound_nm]] > e_val_bound_threshold
    if (any(above)) {
      rn <- rownames(transformed_table)
      rownames(transformed_table)[above] <- paste0("**", rn[above], "**")
    }
  }

  # optional save -----------------------------------------------------------
  if (save_output) {
    filename <- paste0(
      prefix %||% "",
      base_filename,
      if (use_timestamp) paste0("_", format(Sys.time(), "%Y%m%d%H%M%S")) else "",
      ".qs"
    )
    here_save_qs(
      list(
        plot              = out_plot,
        interpretation    = interpretation,
        transformed_table = transformed_table
      ),
      file.path(save_path, filename)
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
    adjust = c("none", "bonferroni"),
    alpha = 0.05,
    include_adjust_note = TRUE,
    effect_type = "ATE",
    intervention_type = c("exposure_shift", "ipsi"),
    delta = 1,
    sd = 1) {
  type <- match.arg(type)
  order <- match.arg(order)
  adjust <- match.arg(adjust)
  intervention_type <- match.arg(intervention_type)
  alpha <- as.numeric(alpha)[1]

  # build adjustment sentences only when requested ------------------------
  if (include_adjust_note) {
    ci_sentence <- switch(adjust,
      none = "No adjustment was made for family‑wise error rates to confidence intervals.",
      bonferroni = paste0(
        "Confidence intervals were adjusted for multiple comparisons using Bonferroni correction",
        " ($\\alpha$ = ", alpha, ")."
      ),
    )

    ev_sentence <- switch(adjust,
      none = "No adjustment was made for family‑wise error rates to E‑values.",
      bonferroni = paste0(
        "E‑values were also adjusted using Bonferroni correction",
        " ($\\alpha$ = ", alpha, ")."
      ),
    )
    adj_note <- paste(ci_sentence, ev_sentence)
  } else {
    adj_note <- ""
  }

  # sort and optionally back‑transform ------------------------------------
  df <- group_tab(df, type = type, order = order)
  if (!"unit" %in% names(df)) df$unit <- ""
  if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)

  # identify columns -------------------------------------------------------
  # use the helper function to detect effect column
  effect_info <- detect_effect_column(df)
  effect_col <- effect_info$column
  null_val <- if (type == "RR") 1 else 0

  # filter reliable effects -----------------------------------------------
  df_f <- df %>%
    dplyr::filter(E_Value > 1, E_Val_bound > e_val_bound_threshold)

  if (nrow(df_f) == 0) {
    no_effects_msg <- "No reliable effects are evident."
    interpretation_text <- if (nzchar(adj_note)) {
      paste0(adj_note, "\n\n", no_effects_msg)
    } else {
      no_effects_msg
    }
    return(list(interpretation = interpretation_text))
  }

  # preserve requested ordering -------------------------------------------
  if (grepl("_(asc|desc)$", order)) df_f <- df_f[nrow(df_f):1, ]

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
      "(E‑value lower bound > {e_val_bound_threshold}):\n\n",
      "**Warning:** These are naive associations that ignore confounding and should NOT be interpreted causally.\n\n\n"
    )
  } else {
    glue::glue(
      "The following outcomes present reliable causal evidence for {effect_desc} ",
      "(E‑value lower bound > {e_val_bound_threshold}):\n\n\n"
    )
  }

  bullets <- df_f %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # check if variable was log-transformed
      was_log_transformed = if ("original_var_name" %in% names(.)) {
        grepl("_log_", original_var_name)
      } else {
        FALSE
      },

      # format standardized scale label
      lab = glue::glue(
        "{format_minimal_decimals(.data[[effect_col]])}(",
        "{format_minimal_decimals(`2.5 %`)},",
        "{format_minimal_decimals(`97.5 %`)})"
      ),

      # format original scale label with proper interpretation
      lab_orig = if (paste0(effect_col, "_original") %in% names(df)) {
        if (was_log_transformed && type == "RD") {
          # for log-transformed outcomes on difference scale, use multiplicative interpretation
          # calculate percentage change from standardized effect
          if ("original_var_name" %in% names(.)) {
            transform_info <- get_outcome_transformation_info(original_var_name, original_df)
            if (!is.null(transform_info) && transform_info$has_z && transform_info$has_log) {
              # effect on log scale
              delta_log <- .data[[effect_col]] * transform_info$log_sd
              ratio <- exp(delta_log)
              pct_change <- (ratio - 1) * 100

              # confidence intervals
              delta_log_lower <- .data[["2.5 %"]] * transform_info$log_sd
              delta_log_upper <- .data[["97.5 %"]] * transform_info$log_sd
              ratio_lower <- exp(delta_log_lower)
              ratio_upper <- exp(delta_log_upper)
              pct_lower <- (ratio_lower - 1) * 100
              pct_upper <- (ratio_upper - 1) * 100

              # detect units
              units_info <- detect_variable_units(transform_info$original_var)

              # calculate absolute change based on population mean
              log_mean_to_use <- if (!is.null(transform_info$use_display_mean) && transform_info$use_display_mean) {
                transform_info$log_mean_display
              } else {
                transform_info$log_mean
              }
              pop_mean_orig <- exp(log_mean_to_use) - transform_info$log_offset

              if (!is.null(units_info$scale_factor)) {
                pop_mean_orig <- pop_mean_orig * units_info$scale_factor
              }

              abs_change <- pop_mean_orig * (ratio - 1)
              abs_change_lower <- pop_mean_orig * (ratio_lower - 1)
              abs_change_upper <- pop_mean_orig * (ratio_upper - 1)

              # format based on unit type
              change_word <- if (pct_change >= 0) "increase" else "decrease"

              if (units_info$type == "monetary") {
                # format with confidence intervals in original units
                glue::glue(
                  "{units_info$symbol}{format_minimal_decimals(abs(abs_change))} average {change_word} ",
                  "(95% CI: {units_info$symbol}{format_minimal_decimals(abs(abs_change_lower))} to ",
                  "{units_info$symbol}{format_minimal_decimals(abs(abs_change_upper))})"
                )
              } else if (units_info$type == "time") {
                # format with confidence intervals in original units
                glue::glue(
                  "{format_minimal_decimals(abs(abs_change))} {units_info$name} average {change_word} ",
                  "(95% CI: {format_minimal_decimals(abs(abs_change_lower))} to ",
                  "{format_minimal_decimals(abs(abs_change_upper))} {units_info$name})"
                )
              } else {
                # generic format - use standardized effect since units are unknown
                glue::glue(
                  "{format_minimal_decimals(.data[[effect_col]])} standardized effect ",
                  "(95% CI: {format_minimal_decimals(.data[['2.5 %']])} to ",
                  "{format_minimal_decimals(.data[['97.5 %']])})"
                )
              }
            } else {
              # fallback if transformation info not available
              glue::glue(
                "{format_minimal_decimals(.data[[paste0(effect_col, '_original')]])} {unit}(",
                "{format_minimal_decimals(.data[[paste0('2.5 %_original')]])},",
                "{format_minimal_decimals(.data[[paste0('97.5 %_original')]])})"
              )
            }
          } else {
            # no original_var_name available, use simple format
            glue::glue(
              "{format_minimal_decimals(.data[[paste0(effect_col, '_original')]])} {unit}(",
              "{format_minimal_decimals(.data[[paste0('2.5 %_original')]])},",
              "{format_minimal_decimals(.data[[paste0('97.5 %_original')]])})"
            )
          }
        } else {
          # not log-transformed or RR scale, use simple format
          unit_text <- if (!is.na(unit) && unit != "") paste0(" ", unit) else ""
          glue::glue(
            "{format_minimal_decimals(.data[[paste0(effect_col, '_original')]])}{unit_text}(",
            "{format_minimal_decimals(.data[[paste0('2.5 %_original')]])},",
            "{format_minimal_decimals(.data[[paste0('97.5 %_original')]])})"
          )
        }
      } else {
        NA_character_
      },
      text = glue::glue(
        "- {outcome}: {lab}{if (!is.na(lab_orig)) paste0('; on the original scale, ', lab_orig, '.') else ''} E‑value bound = {format_minimal_decimals(E_Val_bound, 2)}"
      )
    ) %>%
    dplyr::pull(text)

  # policy/intervention interpretation note (LaTeX-ready for Quarto)
  policy_note <- if (intervention_type == "ipsi") {
    paste0(
      "\n\n",
      "Interpretation is per the stated policy contrast; estimates reflect $\\mathbf{E}[Y(1)] - \\mathbf{E}[Y(0)]$ on standardized outcome units. ",
      "For IPSI (risk-scale) interventions, the E-value pertains to the contrast between policies ($\\alpha_1$ vs $\\alpha_0$)."
    )
  } else {
    paste0(
      "\n\n",
      "Interpretation is per the stated policy contrast; estimates reflect $\\mathbf{E}[Y(1)] - \\mathbf{E}[Y(0)]$ on standardized outcome units. ",
      "For continuous outcomes, E-values use an OLS-to-RR approximation with exposure contrast $\\delta = ",
      delta, "$ and outcome SD = ", sd, "."
    )
  }

  interpretation_text <- paste0(
    if (nzchar(adj_note)) paste0(adj_note, "\n\n") else "",
    intro,
    paste(bullets, collapse = "\n"),
    policy_note
  )

  list(interpretation = interpretation_text)
}


# ’ Transform Table Row Names with CLI Feedback
# ’
# ’ This function transforms the row names of a data frame based on specified criteria
# ’ and provides CLI feedback on the changes made.
# ’
# ’ @param df A data frame whose row names are to be transformed.
# ’ @param remove_tx_prefix logical. if TRUE, removes 't' followed by numbers and underscore from the start of row names.
# ’ @param remove_z_suffix logical. if TRUE, removes '_z' from the end of row names.
# ’ @param use_title_case logical. if TRUE, converts row names to title case.
# ’ @param remove_underscores logical. if TRUE, replaces underscores with spaces in row names.
# ’
# ’ @return A data frame with transformed row names.
# ’
# ’ @import cli
# ’
# ’ @keywords internal
transform_table_rownames <- function(df, label_mapping = NULL, options = list()) {
  # coerce each flag to a single TRUE/FALSE
  remove_tx_prefix <- isTRUE(options$remove_tx_prefix)
  remove_z_suffix <- isTRUE(options$remove_z_suffix)
  remove_underscores <- isTRUE(options$remove_underscores)
  use_title_case <- isTRUE(options$use_title_case)

  n_before <- nrow(df)
  cli::cli_h1("Transforming table row names")

  # helper: run transform_label() with only scalar flags
  transform_row <- function(rn) {
    new_rn <- transform_label(
      rn,
      label_mapping = label_mapping,
      options = list(
        remove_tx_prefix   = remove_tx_prefix,
        remove_z_suffix    = remove_z_suffix,
        remove_underscores = remove_underscores,
        use_title_case     = use_title_case
      )
    )
    if (!identical(new_rn, rn)) {
      cli::cli_alert_info("Changed {.val {rn}} → {.val {new_rn}}")
    }
    new_rn
  }

  # apply to every rowname
  rownames(df) <- vapply(rownames(df), transform_row, FUN.VALUE = "")

  # summary
  cli::cli_h2("Summary")
  cli::cli_alert_info("Rows processed: {.val {n_before}}")
  changes <- sum(rownames(df) != names(table(rownames(df))))
  cli::cli_alert_info("Changes made: {.val {changes}}")
  cli::cli_alert_success("Table row names successfully transformed")

  df
}

# causal effects tables ---------------------------------------------------
#' Group and Annotate Treatment Effect Estimates
#'
#' This function arranges and annotates a data frame based on specified
#' types of treatment effect estimates (RR or RD). It supports a variety of sorting
#' options including alphabetical, magnitude (ascending or descending), E-value bound
#' (ascending or descending), custom order, and a default alias for backward compatibility.
#' It also handles original scale estimates when available.
#'
#' @param df Data frame containing the variables of interest, or a list containing
#'   the results data frame and label mapping from transform_to_original_scale().
#' @param type Type of treatment effect to analyze. One of 'RR' (Risk Ratio) or
#'   'RD' (Risk Difference). Defaults to 'RD'.
#' @param order Sorting option for outcomes. Options are:
#'   \itemize{
#'     \item 'alphabetical': sort by outcome name (A–Z)
#'     \item 'magnitude_desc': sort by absolute effect size, descending (default for 'magnitude')
#'     \item 'magnitude_asc': sort by absolute effect size, ascending
#'     \item 'evaluebound_desc': sort by E-value bound, descending
#'     \item 'evaluebound_asc': sort by E-value bound, ascending
#'     \item 'custom': user-defined order (requires custom_order)
#'     \item 'default': alias for 'magnitude_desc' (deprecated)
#'   }
#'   Default is 'default'.
#' @param custom_order Character vector specifying a custom outcome ordering,
#'   used when order = 'custom'. Must contain all outcomes exactly once.
#'
#' @return A data frame arranged according to `order`, annotated with:
#'   \itemize{
#'     \item Estimate category (positive, negative, not reliable)
#'     \item Formatted label for the effect and confidence interval
#'     \item Optional original-scale label if _original columns are present
#'   }
#'
#' @details
#' The function detects whether `df` is a list output from transform_to_original_scale()
#' and extracts `results_df` and `label_mapping` accordingly. It then ensures an `outcome`
#' column, applies any label mapping, and sorts based on the chosen `order`. New options
#' 'magnitude_desc' and 'magnitude_asc' sort by absolute effect size; 'evaluebound_desc'
#' and 'evaluebound_asc' sort by the E-Value bound; 'alphabetical' sorts by outcome
#' name; 'custom' respects a user-provided vector; 'default' is an alias for 'magnitude_desc'.
#'
#' @examples
#' # descending magnitude (default for 'default')
#' result_df <- group_tab(df = analysis_df, order = "default")
#'
#' # ascending magnitude
#' result_df <- group_tab(df = analysis_df, order = "magnitude_asc")
#'
#' # strongest E-value bound first
#' result_df <- group_tab(df = analysis_df, order = "evaluebound_desc")
#'
#' # alphabetical
#' result_df <- group_tab(df = analysis_df, order = "alphabetical")
#'
#' # custom ordering
#' custom_order <- c("Outcome3", "Outcome1", "Outcome2")
#' result_df <- group_tab(df = analysis_df, order = "custom", custom_order = custom_order)
#'
#' @importFrom dplyr arrange desc mutate slice
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @keywords internal
# Note: group_tab function removed - use the one from helpers.R
# DEPRECATED - Remove this duplicate function
group_tab_deprecated <- function(
    df,
    type = c("RD", "RR"),
    order = c(
      "alphabetical",
      "magnitude_desc",
      "magnitude_asc",
      "evaluebound_desc",
      "evaluebound_asc",
      "custom",
      "default"
    ),
    custom_order = NULL) {
  # load dplyr verbs
  require(dplyr)

  # match arguments
  type <- match.arg(type)
  order <- match.arg(order)

  # alias old magnitude and default to magnitude_desc
  if (order %in% c("default")) {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  # handle list input
  if (is.list(df) && "results_df" %in% names(df)) {
    results_df <- df$results_df
    label_mapping <- df$label_mapping
  } else {
    results_df <- df
    label_mapping <- NULL
  }

  # ensure outcome column
  if (!"outcome" %in% names(results_df) && !is.null(rownames(results_df))) {
    results_df <- results_df %>% tibble::rownames_to_column(var = "outcome")
  } else if (!"outcome" %in% names(results_df)) {
    stop("No 'outcome' column or row names to convert.")
  }

  # apply label mapping
  if (!is.null(label_mapping)) {
    results_df <- results_df %>% mutate(outcome = dplyr::recode(outcome, !!!label_mapping))
  }

  # columns for sorting
  effect_col <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
  ev_bound <- "E_Val_bound"

  # apply ordering
  results_df <- switch(order,
    alphabetical = results_df %>% arrange(outcome),
    magnitude_desc = results_df %>% arrange(desc(abs(!!sym(effect_col)))),
    magnitude_asc = results_df %>% arrange(abs(!!sym(effect_col))),
    evaluebound_desc = results_df %>% arrange(desc(!!sym(ev_bound))),
    evaluebound_asc = results_df %>% arrange(!!sym(ev_bound)),
    custom = {
      if (is.null(custom_order)) stop("custom_order must be provided for 'custom' order")
      results_df %>% slice(match(custom_order, outcome))
    }
  )

  # annotate estimates
  results_df <- results_df %>% mutate(
    Estimate = factor(
      if (type == "RR") {
        ifelse(`E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
          "positive",
          ifelse(`E[Y(1)]/E[Y(0)]` < 1 & `97.5 %` < 1,
            "negative",
            "not reliable"
          )
        )
      } else {
        ifelse(`E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
          "positive",
          ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0,
            "negative",
            "not reliable"
          )
        )
      }
    ),
    estimate_lab = if (type == "RR") {
      paste0(
        round(`E[Y(1)]/E[Y(0)]`, 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]"
      )
    } else {
      paste0(
        round(`E[Y(1)]-E[Y(0)]`, 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3), ")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]"
      )
    }
  )

  # add original-scale label if present
  if (paste0(effect_col, "_original") %in% names(results_df)) {
    results_df <- results_df %>% mutate(
      estimate_lab_original = paste0(
        round(.data[[paste0(effect_col, "_original")]], 3), " (",
        round(.data[["2.5 %_original"]], 3), "-",
        round(.data[["97.5 %_original"]], 3), ")"
      )
    )
  }

  results_df
}

#' @keywords internal
process_evalue <- function(tab, scale, delta, sd) {
  ev <- if (scale == "RD") {
    EValue::evalues.OLS(tab$`E[Y(1)]-E[Y(0)]`,
      se    = tab$standard_error,
      sd    = sd,
      delta = delta,
      true  = 0
    )
  } else {
    EValue::evalues.RR(tab$`E[Y(1)]/E[Y(0)]`,
      lo   = tab$`2.5 %`,
      hi   = tab$`97.5 %`,
      true = 1
    )
  }

  ev_df <- as.data.frame(ev)[2, c("point", "lower", "upper"), drop = FALSE]

  tibble::tibble(
    E_Value     = ev_df$point,
    E_Val_bound = dplyr::coalesce(ev_df$lower, ev_df$upper, 1)
  )
}



#' Correct a "combined table" for multiplicity **and** recompute *E*-values
#'
#' @description
#' `margot_correct_combined_table()` takes the **combined_table** produced by the
#' various *margot* models (or by your own code) and
#' \enumerate{
#'   \item widens the confidence interval according to the chosen
#'         family–wise-error correction, **and**
#'   \item recalculates *E*-values (and their lower bounds) so they match the
#'         new interval.
#' }
#' By default it implements the single–step **Bonferroni** correction at
#' \eqn{\alpha = 0.05} as advocated by VanderWeele & Mathur (2019).
#'
#' @param combined_table A data frame with *at least* the columns
#'   \itemize{
#'     \item `E[Y(1)]-E[Y(0)]` **or** `E[Y(1)]/E[Y(0)]`
#'     \item `2.5 %`, `97.5 %`   (unadjusted CI limits)
#'   }
#'   Extra columns (e.g. the original *E*-values) are carried through.
#' @param adjust multiplicity correction method: "none", "bonferroni"
#'   Bonferroni provides conservative family-wise error rate control.
#' @param alpha  Family-wise error-rate for bonferroni. Default `0.05`.
#' @param scale  Scale to use when recomputing the *E*-value.
#'   `"RD"` (risk difference / ATE, **default**) or `"RR"` (risk ratio).
#' @param delta,sd Arguments passed to [EValue::evalues.OLS()] when
#'   `scale = "RD"`.  Ignored for `"RR"`.
#'
#' @return A data frame with the same rows (and order) as `combined_table`, but
#'   with
#'   \itemize{
#'     \item updated `2.5 %` and `97.5 %` columns, and
#'     \item freshly computed `E_Value` and `E_Val_bound`.
#'   }
#'
#' @section How the correction is applied:
#' Let \eqn{m} be the number of rows (tests).
#' \itemize{
#'   \item **Bonferroni** uses
#'     \deqn{ z^* = \Phi^{-1}\!\bigl(1-\alpha/(2m)\bigr) }
#'     and rescales the original half-width.
#'   \item **none** applies no correction, keeping the original confidence intervals.
#' }
#'
#' @references
#' VanderWeele TJ, Mathur MB (2019).
#' *Some desirable properties of the Bonferroni correction:
#' Is the Bonferroni correction really so bad?*
#' **Am J Epidemiol** 188(3): 617–618.
#'
#' @export
#' @importFrom stats qnorm pnorm p.adjust
#' @importFrom dplyr mutate across any_of bind_cols
#' @importFrom purrr pmap_dfr
#' @importFrom EValue evalues.OLS evalues.RR
margot_correct_combined_table <- function(combined_table,
                                          adjust = c("bonferroni", "none"),
                                          alpha = 0.05,
                                          scale = c("RD", "RR"),
                                          delta = 1,
                                          sd = 1) {
  adjust <- match.arg(adjust)
  scale <- match.arg(scale)

  ## ---- 0 • sanity checks ----------------------------------------------------
  if ("E[Y(1)]-E[Y(0)]" %in% names(combined_table)) {
    est_col <- "E[Y(1)]-E[Y(0)]"
  } else if ("E[Y(1)]/E[Y(0)]" %in% names(combined_table)) {
    est_col <- "E[Y(1)]/E[Y(0)]"
  } else {
    stop("Couldn't find a point-estimate column in `combined_table`.")
  }

  if (!all(c("2.5 %", "97.5 %") %in% names(combined_table))) {
    stop("`combined_table` must contain '2.5 %' and '97.5 %' columns.")
  }

  m <- nrow(combined_table) # number of tests
  z_orig <- stats::qnorm(0.975) # 1.96

  tbl <- combined_table

  ## ---- 1  adjust the CI ----------------------------------------------------
  if (adjust == "bonferroni") {
    z_star <- stats::qnorm(1 - alpha / (2 * m))

    if (scale == "RR") {
      # Adjust on the log scale, then exponentiate back to preserve positivity
      eps <- .Machine$double.eps
      est_rr   <- pmax(tbl[[est_col]], eps)
      lo_rr    <- pmax(tbl$`2.5 %`, eps)
      hi_rr    <- pmax(tbl$`97.5 %`, eps)

      est_log  <- log(est_rr)
      hi_log   <- log(hi_rr)
      se_log   <- (hi_log - est_log) / z_orig
      new_lo   <- exp(est_log - z_star * se_log)
      new_hi   <- exp(est_log + z_star * se_log)

      tbl$`2.5 %`  <- new_lo
      tbl$`97.5 %` <- new_hi
    } else {
      # RD: rescale original half-width (symmetric Wald on difference scale)
      half_w <- (tbl$`97.5 %` - tbl$`2.5 %`) / 2
      tbl <- tbl |>
        dplyr::mutate(
          `2.5 %`  = !!rlang::sym(est_col) - (half_w * z_star / z_orig),
          `97.5 %` = !!rlang::sym(est_col) + (half_w * z_star / z_orig)
        )
    }
  }
  # note: if adjust == "none", no CI adjustment is applied

  ## ---- 2  recompute E-values ----------------------------------------------
  new_EV <- purrr::pmap_dfr(
    list(
      est = tbl[[est_col]],
      lo = tbl$`2.5 %`,
      hi = tbl$`97.5 %`,
      se0 = (tbl$`97.5 %` - tbl[[est_col]]) / stats::qnorm(0.975)
    ),
    \(est, lo, hi, se0) {
      tmp <- tibble::tibble(
        `E[Y(1)]-E[Y(0)]` = est,
        `E[Y(1)]/E[Y(0)]` = NA_real_, # ignored for RD
        `2.5 %`           = lo,
        `97.5 %`          = hi,
        standard_error    = se0
      )
      process_evalue(tmp, scale, delta, sd)
    }
  )

  ## ---- 3 bind & round -----------------------------------------------------
  tbl |>
    dplyr::select(-dplyr::any_of(c("E_Value", "E_Val_bound"))) |>
    dplyr::bind_cols(new_EV) |>
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
}


#  margot_plot() calls:
#  1.	back-transformation
# 	back_transform_estimates() (aka  transform_to_original_scale())
# 2.	label transformation
# •	transform_label() (aka transform_var_name() in other helpers)
# 3.	tab-sorting & annotation
# •	group_tab()
# 4.	interpretation
# •	margot_interpret_marginal()
# 5.	row-name prettifier
# •	transform_table_rownames()
# 6.	Saving helper
# •	here_save_qs() (from  I/O utilities)
# 7. to add - calls margot_correct_combined_table() - not in this file
# 8. margot_correct_combined_table
