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
      scale  = type
    )
    .data <- corrected_table_df
  } else {
    cli::cli_alert_info("no multiplicity adjustment applied")
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
    remove_underscores = TRUE
  )

  opts <- modifyList(modifyList(default_opts, options), list(...))

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
    effect_type           = effect_type
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
    if (bound_nm %in% names(transformed_table)) {
      above <- transformed_table[[bound_nm]] > e_val_bound_threshold
      if (any(above)) {
        rn <- rownames(transformed_table)
        rownames(transformed_table)[above] <- paste0("**", rn[above], "**")
      }
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
    effect_type = "ATE") {
  type <- match.arg(type)
  order <- match.arg(order)
  adjust <- match.arg(adjust)
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
