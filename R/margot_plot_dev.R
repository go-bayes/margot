#' Experimental plot: advanced bias/adjustment features (DEV)
#'
#' @description
#' margot_plot_dev() is an experimental, feature‑rich plotting helper that keeps
#' all the recent bias analysis improvements (e.g., optional multiplicity
#' correction, flexible E‑value handling, label mapping, table renaming) while
#' leaving the stable margot_plot() API unchanged. Expect breaking changes in a
#' future release as we continue to refine RR/E‑value workflows.
#'
#' @param .data Data frame of effect estimates and 95% CI (as produced by
#'   upstream helpers, e.g., group_tab()).
#' @param type Character: "RD" (difference) or "RR" (ratio).
#' @param order Character ordering mode; see stable margot_plot() for options.
#' @param custom_order Optional character vector for custom ordering.
#' @param title_binary Optional title for binary outcomes.
#' @param include_coefficients Logical; if TRUE, prints numeric effect at left margin.
#' @param standardize_label One of "NZ", "US", "none"; controls x‑axis label text.
#' @param e_val_bound_threshold Numeric threshold for E‑value bound labelling.
#' @param adjust Multiplicity correction: "none" or "bonferroni".
#' @param alpha Numeric significance level used by multiplicity correction.
#' @param ... Additional named options merged into `options` (used by label mapping, theming, etc.).
#' @param options Named list of plotting/label options (e.g., colours, sizes, transforms).
#' @param label_mapping Optional named list for transform_label().
#' @param save_output Logical; if TRUE, saves the result list via here_save_qs().
#' @param use_timestamp Logical; append timestamp to saved filename.
#' @param base_filename,prefix,save_path Save controls passed to here_save_qs().
#' @param original_df Optional original data for back‑transform helpers.
#' @param bold_rows Logical; bold rows above the E‑value bound threshold.
#' @param rename_cols Logical; if TRUE, rename E‑value columns per `col_renames`.
#' @param col_renames Named list of column renames.
#' @param rename_ate Logical or character; if TRUE/character, rename the effect column.
#' @param rename_evalue Logical; if TRUE, rename E‑value column headers to display form.
#'
#' @return A list with `plot`, `interpretation`, and `transformed_table`.
#' @export
margot_plot_dev <- function(
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

  type              <- match.arg(type)
  order             <- match.arg(order)
  standardize_label <- match.arg(standardize_label)
  adjust            <- match.arg(adjust)
  alpha             <- as.numeric(alpha)[1]

  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  # optional multiplicity correction (DEV)
  raw_table_df <- .data
  n_outcomes <- tryCatch(nrow(raw_table_df), error = function(...) NA_integer_)
  if (isTRUE(n_outcomes == 1L) && adjust != "none") {
    cli::cli_alert_info("single outcome detected; multiplicity correction skipped")
    adjust <- "none"
  }

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
    # DEV: parameters for RR/E‑value context
    delta_exposure = 1,
    sd_outcome = 1,
    intervention_type = "exposure_shift"
  )
  opts <- modifyList(modifyList(default_opts, options), list(...))

  if (adjust != "none") {
    cli::cli_alert_info("applying {adjust} correction (alpha = {alpha}) to confidence intervals")
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
    cli::cli_alert_info("no multiplicity adjustment applied (DEV)")
  }

  # coerce label flags
  for (nm in c("remove_tx_prefix", "remove_z_suffix", "use_title_case", "remove_underscores")) {
    opts[[nm]] <- as.logical(opts[[nm]])[1]
  }

  # detect effect column
  eff_info <- detect_effect_column_dev(.data)
  if (is.null(eff_info)) {
    eff_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data)) "E[Y(1)]-E[Y(0)]" else "E[Y(1)]/E[Y(0)]"
    effect_type <- "ATE"
  } else {
    eff_col <- eff_info$column
    effect_type <- eff_info$type
  }

  # ensure outcome label exists
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    cli::cli_alert_info("added 'outcome' column (DEV)")
  }

  # optional back‑transform to original scale
  if (!is.null(original_df)) {
    .data <- back_transform_estimates(.data, original_df)
  }

  # transform display labels
  .data$outcome <- sapply(
    .data$outcome,
    transform_label,
    label_mapping = label_mapping,
    options       = opts
  )

  # sorting and thresholds
  thresh   <- e_val_bound_threshold
  null_val <- ifelse(type == "RR", 1, 0)

  sorted_df <- group_tab(
    .data,
    type         = type,
    order        = order,
    custom_order = custom_order
  )
  sorted_df$outcome <- factor(sorted_df$outcome, levels = sorted_df$outcome)

  # Colour categories should reflect the contrast sign and CI only
  # (previous stable behaviour). E-value thresholds are used for
  # interpretation and optional styling, not for colouring.
  if ("Estimate" %in% names(sorted_df)) {
    # trust group_tab's CI-based categorisation when available
    cat_vec <- as.character(sorted_df$Estimate)
  } else {
    cat_vec <- with(sorted_df,
      ifelse(
        `2.5 %` > null_val & `97.5 %` > null_val, "positive",
        ifelse(`2.5 %` < null_val & `97.5 %` < null_val, "negative", "not reliable")
      )
    )
  }
  sorted_df$Estimate <- factor(cat_vec, levels = c("positive", "not reliable", "negative"))

  # x‑axis label
  lw <- switch(standardize_label, NZ = "Standardised", US = "Standardized", none = "Effect")
  xlab <- if (type == "RR") "Effect (Risk Ratio)" else if (lw != "Effect") paste0(lw, " Effect (Difference Scale)") else "Effect (Difference Scale)"

  # plot
  out_plot <- ggplot2::ggplot(
    sorted_df,
    ggplot2::aes(y = outcome, x = !!rlang::sym(eff_col), xmin = `2.5 %`, xmax = `97.5 %`, colour = Estimate)
  ) +
    ggplot2::geom_errorbarh(height = 0.3, linewidth = opts$linewidth, position = ggplot2::position_dodge(0.3)) +
    ggplot2::geom_point(size = opts$point_size, position = ggplot2::position_dodge(0.3)) +
    ggplot2::geom_vline(xintercept = null_val) +
    ggplot2::scale_color_manual(values = opts$colors) +
    ggplot2::labs(x = xlab, y = "", title = opts$title, subtitle = opts$subtitle) +
    ggplot2::coord_cartesian(xlim = c(opts$x_lim_lo, opts$x_lim_hi)) +
    ggplot2::theme_classic(base_size = opts$base_size) +
    ggplot2::theme(legend.position = "top", legend.direction = "horizontal", axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = opts$title_size))

  # remove subtitle spacing if subtitle is empty or NULL
  if (is.null(opts$subtitle) || !nzchar(opts$subtitle)) {
    out_plot <- out_plot + ggplot2::theme(plot.subtitle = ggplot2::element_blank())
  }

  if (type == "RR") {
    out_plot <- out_plot + ggplot2::scale_x_continuous(labels = function(x) ifelse(x < 0, "", as.character(x)))
  }

  if (isTRUE(include_coefficients)) {
    # determine offset
    data_range <- range(c(sorted_df[[eff_col]], sorted_df$`2.5 %`, sorted_df$`97.5 %`), na.rm = TRUE)
    x_range <- diff(data_range)
    x_lim_lo <- if (!is.null(opts$x_lim_lo)) opts$x_lim_lo else data_range[1] - 0.1 * x_range
    x_lim_hi <- if (!is.null(opts$x_lim_hi)) opts$x_lim_hi else data_range[2] + 0.1 * x_range
    coeff_x_position <- x_lim_lo + 0.05 * (x_lim_hi - x_lim_lo)
    out_plot <- out_plot + ggplot2::geom_text(
      ggplot2::aes(x = coeff_x_position, label = sprintf("%.2f", !!rlang::sym(eff_col))),
      size = opts$text_size, hjust = 0, fontface = "bold"
    )
  }

  # interpretation (DEV)
  interp <- margot_interpret_marginal_dev(
    df                    = sorted_df,
    type                  = type,
    order                 = order,
    original_df           = original_df,
    e_val_bound_threshold = thresh,
    adjust                = adjust,
    alpha                 = alpha,
    include_adjust_note   = !isTRUE(n_outcomes == 1L),
    effect_type           = effect_type,
    intervention_type     = opts$intervention_type,
    delta                 = opts$delta_exposure,
    sd                    = opts$sd_outcome
  )
  transformed_table <- interp$transformed_table %||% sorted_df
  interpretation    <- interp$interpretation     %||% ""

  if (rename_cols) {
    for (cn in names(col_renames)) if (cn %in% names(transformed_table)) names(transformed_table)[names(transformed_table) == cn] <- col_renames[[cn]]
  }
  if (!identical(rename_ate, FALSE)) {
    eff_clean <- ifelse(type == "RR", "Effect (RR)", "Effect (RD)")
    nm <- if (isTRUE(rename_ate)) eff_clean else as.character(rename_ate)
    if (eff_col %in% names(transformed_table)) names(transformed_table)[names(transformed_table) == eff_col] <- nm
  }
  if (isTRUE(rename_evalue)) {
    if ("E_Value" %in% names(transformed_table)) names(transformed_table)[names(transformed_table) == "E_Value"] <- "E-Value"
    if ("E_Val_bound" %in% names(transformed_table)) names(transformed_table)[names(transformed_table) == "E_Val_bound"] <- "E-Value Bound"
  }

  if (bold_rows) {
    bound_nm <- if ("E-Value Bound" %in% names(transformed_table)) "E-Value Bound" else if ("E-Value bound" %in% names(transformed_table)) "E-Value bound" else "E_Val_bound"
    if (bound_nm %in% names(transformed_table)) {
      above <- transformed_table[[bound_nm]] > e_val_bound_threshold
      if (any(above)) {
        rn <- rownames(transformed_table); rownames(transformed_table)[above] <- paste0("**", rn[above], "**")
      }
    }
  }

  if (save_output) {
    filename <- paste0(prefix %||% "", base_filename, if (use_timestamp) paste0("_", format(Sys.time(), "%Y%m%d%H%M%S")) else "", ".qs")
    here_save_qs(list(plot = out_plot, interpretation = interpretation, transformed_table = transformed_table), file.path(save_path, filename))
  }

  invisible(list(plot = out_plot, interpretation = interpretation, transformed_table = transformed_table))
}

# internal helpers (DEV) ------------------------------------------------------

#' @keywords internal
detect_effect_column_dev <- function(data) {
  # prefer new columns
  for (col in c("ATE", "ATT", "ATC", "ATO")) if (col %in% names(data)) return(list(column = col, type = col))
  if ("E[Y|A]" %in% names(data)) return(list(column = "E[Y|A]", type = "naive"))
  if ("E[Y(1)]-E[Y(0)]" %in% names(data)) return(list(column = "E[Y(1)]-E[Y(0)]", type = "ATE"))
  if ("E[Y(1)]/E[Y(0)]" %in% names(data)) return(list(column = "E[Y(1)]/E[Y(0)]", type = "ATE"))
  NULL
}

#' @keywords internal
margot_interpret_marginal_dev <- function(
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

  if (!"unit" %in% names(df)) df$unit <- ""
  df <- group_tab(df, type = type, order = order)
  if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)

  eff_info <- detect_effect_column_dev(df)
  effect_col <- if (is.null(eff_info)) if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]" else eff_info$column

  # filter reliable effects per bound threshold
  df_f <- df %>% dplyr::filter(E_Value > 1, E_Val_bound > e_val_bound_threshold)
  if (nrow(df_f) == 0) {
    no_effects_msg <- "No reliable effects are evident."
    return(list(interpretation = no_effects_msg, transformed_table = df))
  }

  # compact bullets
  df_f <- group_tab(df_f, type = type, order = order)
  bullets <- df_f %>% dplyr::rowwise() %>% dplyr::mutate(
    effect_text = sprintf(
      "%.2f [%.2f, %.2f] (E-value: %.2f; bound: %.2f)",
      .data[[effect_col]], `2.5 %`, `97.5 %`, E_Value, E_Val_bound)
  ) %>% dplyr::pull(effect_text)

  adj_note <- if (include_adjust_note && adjust != "none") paste0("Confidence intervals adjusted by ", adjust, " (alpha=", alpha, "). ") else ""
  intro <- "Reliable effects (E-value lower bound above the reporting threshold):\n\n"
  list(
    interpretation    = paste0(adj_note, intro, paste0("- ", bullets, collapse = "\n")),
    transformed_table = df
  )
}
