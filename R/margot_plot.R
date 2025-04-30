#' @title Create a Margot Plot with Interpretation
#' @description
#' Create a Margot plot for visualising causal effects with flexible sorting,
#' embed a compact interpretation, and return a transformed table.
#'
#' @param .data  a data frame with:
#'   - `outcome` column,
#'   - effect columns: `E[Y(1)]-E[Y(0)]` or `E[Y(1)]/E[Y(0)]`,
#'   - confidence limits: `2.5 %`, `97.5 %`,
#'   - `E_Value`, `E_Val_bound`, and optional `unit`.
#' @param type   one of `"RD"` (risk difference) or `"RR"` (risk ratio). Default: `"RD"`.
#' @param order  outcome sorting. Options:
#'   - `alphabetical`, `magnitude_desc`, `magnitude_asc`,
#'   - `evaluebound_desc`, `evaluebound_asc`, `custom`, `default`.
#'   Default: `"alphabetical"`.
#' @param custom_order  character vector for `order = "custom"`.
#' @param title_binary  optional title for binary plots.
#' @param include_coefficients  logical; include effect values on plot. Default: `TRUE`.
#' @param standardize_label     one of `"NZ"`, `"US"`, or `"none"`. Default: `"NZ"`.
#' @param e_val_bound_threshold  numeric; minimum `E_Val_bound` to treat an effect as
#'   reliable. Default: `1.2`.
#' @param ...                     additional args passed to `options`.
#' @param options                 list of plot & label options.
#' @param label_mapping           named vector mapping original to display labels.
#' @param save_output             logical; if `TRUE`, save output via `here_save_qs()`.
#'   Default: `FALSE`.
#' @param use_timestamp           logical; append timestamp to filename. Default: `FALSE`.
#' @param base_filename           base of output filename. Default: `"margot_plot_output"`.
#' @param prefix                  optional filename prefix.
#' @param save_path               directory for saving. Default: `here::here("push_mods")`.
#' @param original_df             optional data frame for back-transformation.
#' @param bold_rows               logical; bold row-names above threshold. Default: `FALSE`.
#' @param rename_cols             logical; apply `col_renames`. Default: `FALSE`.
#' @param col_renames             named list mapping new = old. Default:
#'   `list("E-Value"="E_Value", "E-Value bound"="E_Val_bound")`.
#' @param rename_ate              logical; rename effect column to `"ATE"`. Default: `FALSE`.
#'
#' @return
#' A list with:
#' * `plot`:   a `ggplot2` object
#' * `interpretation`: markdown string
#' * `transformed_table`: input `.data` with transformed row-names
#'
#' @export
margot_plot <- function(
    .data,
    type = c("RD","RR"),
    order = c("alphabetical","magnitude_desc","magnitude_asc",
              "evaluebound_desc","evaluebound_asc","custom","default"),
    custom_order = NULL,
    title_binary = NULL,
    include_coefficients = TRUE,
    standardize_label = c("NZ","US","none"),
    e_val_bound_threshold = 1.2,
    ...,
    options = list(),
    label_mapping = NULL,
    save_output = FALSE,
    use_timestamp = FALSE,
    base_filename = "margot_plot_output",
    prefix = NULL,
    save_path = here::here("push_mods"),
    original_df = NULL,

    # ── NEW ARGS ───────────────────────────────────────────────
    bold_rows    = FALSE,
    rename_cols  = FALSE,
    col_renames  = list("E-Value" = "E_Value",
                        "E-Value bound" = "E_Val_bound"),
    rename_ate   = FALSE
) {
  # match and validate arguments
  type            <- match.arg(type)
  order           <- match.arg(order)
  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }
  standardize_label <- match.arg(standardize_label)

  # keep a copy of raw data for the table
  raw_table_df <- .data

  # merge user options with defaults
  default_opts <- list(
    title = NULL, subtitle = NULL, estimate_scale = 1,
    base_size = 18, text_size = 2.75, point_size = 3,
    title_size = 20, subtitle_size = 18,
    legend_text_size = 10, legend_title_size = 10,
    x_offset = NULL, x_lim_lo = NULL, x_lim_hi = NULL,
    linewidth = 0.4, plot_theme = NULL,
    colors = c("positive"="#E69F00","not reliable"="black","negative"="#56B4E9"),
    facet_var = NULL, confidence_level = 0.95,
    annotations = NULL, show_evalues = TRUE, evalue_digits = 2,
    remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
    use_title_case = TRUE, remove_underscores = TRUE
  )
  opts <- modifyList(modifyList(default_opts, options), list(...))
  # coerce logical flags
  for (nm in c("remove_tx_prefix","remove_z_suffix","use_title_case","remove_underscores")) {
    opts[[nm]] <- as.logical(opts[[nm]])[1]
  }

  # determine which effect column to use
  eff_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(.data))
    "E[Y(1)]-E[Y(0)]" else "E[Y(1)]/E[Y(0)]"

  # add outcome column if missing
  if (!"outcome" %in% names(.data)) {
    .data$outcome <- rownames(.data)
    message("Added 'outcome' column.")
  }

  # back-transform to original scale if requested
  if (!is.null(original_df)) {
    .data <- back_transform_estimates(.data, original_df)
  }

  # apply label transformations
  .data$outcome <- sapply(.data$outcome, transform_label, label_mapping, opts)

  # pull threshold into a plain variable
  thresh   <- e_val_bound_threshold
  null_val <- ifelse(type=="RR", 1, 0)

  # sort data for plotting & interpretation
  sorted_df <- group_tab(.data, type=type, order=order, custom_order=custom_order)
  sorted_df$outcome <- factor(sorted_df$outcome, levels = sorted_df$outcome)

  # sort raw data for the table in the same way
  table_sorted_df <- group_tab(raw_table_df, type=type, order=order, custom_order=custom_order)

  # compute estimate categories using threshold
  cat_vec <- with(sorted_df,
                  ifelse(
                    E_Val_bound > thresh & `2.5 %` > null_val & `97.5 %` > null_val, "positive",
                    ifelse(
                      E_Val_bound > thresh & `2.5 %` < null_val & `97.5 %` < null_val, "negative",
                      "not reliable"
                    )
                  )
  )
  sorted_df$Estimate <- factor(cat_vec, levels = c("positive","not reliable","negative"))

  # set up x-axis label
  lw   <- switch(standardize_label, NZ="Standardised", US="Standardized", none="Effect")
  xlab <- if (type=="RR") "Effect (Risk Ratio)" else
    if (lw!="Effect") paste0(lw," Effect (Difference Scale)") else "Effect (Difference Scale)"

  # build the ggplot
  out <- ggplot2::ggplot(sorted_df, ggplot2::aes(
    y = outcome, x = !!rlang::sym(eff_col),
    xmin = `2.5 %`, xmax = `97.5 %`, colour = Estimate
  )) +
    ggplot2::geom_errorbarh(
      height = 0.3, linewidth = opts$linewidth,
      position = ggplot2::position_dodge(0.3)
    ) +
    ggplot2::geom_point(
      size = opts$point_size,
      position = ggplot2::position_dodge(0.3)
    ) +
    ggplot2::geom_vline(xintercept = null_val) +
    ggplot2::scale_color_manual(values = opts$colors) +
    ggplot2::labs(x = xlab, title = opts$title, subtitle = opts$subtitle) +
    ggplot2::coord_cartesian(xlim = c(opts$x_lim_lo, opts$x_lim_hi)) +
    ggplot2::theme_classic(base_size = opts$base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = opts$title_size)
    )

  if (type == "RR") {
    out <- out + ggplot2::scale_x_continuous(
      labels = function(x) ifelse(x < 0, "", as.character(x))
    )
  }

  if (include_coefficients) {
    out <- out + ggplot2::geom_text(
      ggplot2::aes(
        x = !!rlang::sym(eff_col) + opts$x_offset * opts$estimate_scale,
        label = sprintf("%.2f", !!rlang::sym(eff_col))
      ),
      size = opts$text_size, hjust = ifelse(type=="RR", 0, 1), fontface = "bold"
    )
  }

  # generate interpretation
  interpretation <- margot_interpret_marginal(
    df                    = sorted_df,
    type                  = type,
    order                 = order,
    original_df           = original_df,
    e_val_bound_threshold = thresh
  )$interpretation

  # transform and return the table to match plot order
  table_for_transform <- table_sorted_df
  if ("outcome" %in% colnames(table_for_transform)) {
    rownames(table_for_transform) <- table_for_transform$outcome
    table_for_transform$outcome <- NULL
  }

  # 3b) now transform those rownames
  transformed_table <- transform_table_rownames(
    table_for_transform,
    label_mapping = label_mapping,
    options       = opts
  )

  # 3b-i) reorder to match the plot’s y-axis (top → bottom):
  plot_levels   <- levels(sorted_df$outcome)   # bottom→top
  outcome_order <- rev(plot_levels)            # top→bottom
  transformed_table <- transformed_table[outcome_order, , drop = FALSE]

  # 3c) keep only your core five columns
  keep_cols <- c(eff_col, "2.5 %", "97.5 %", "E_Value", "E_Val_bound")
  transformed_table <- transformed_table[, intersect(keep_cols, names(transformed_table)), drop = FALSE]

  # 4) optional: rename to "ATE", then rename_cols, then bold_rows…

  # ── 4) OPTIONAL: rename the effect column to "ATE" ──────────
  if (rename_ate) {
    old_eff <- eff_col
    if (old_eff %in% names(transformed_table)) {
      names(transformed_table)[names(transformed_table)==old_eff] <- "ATE"
      eff_col <- "ATE"  # so if you later refer to it, you get the new name
    }
  }

  # ── 5) OPTIONAL: apply your column‐renaming map ─────────────
  if (rename_cols && length(col_renames)>0) {
    for (new_nm in names(col_renames)) {
      old_nm <- col_renames[[new_nm]]
      if (old_nm %in% names(transformed_table)) {
        names(transformed_table)[names(transformed_table)==old_nm] <- new_nm
      }
    }
  }

  # ── 6) OPTIONAL: bold rownames above the E-value threshold ──
  if (bold_rows) {
    # figure out which column now holds the bound
    bound_nm <- if (rename_cols && "E-Value bound" %in% names(transformed_table))
      "E-Value bound" else "E_Val_bound"
    above    <- transformed_table[[bound_nm]] > e_val_bound_threshold
    if (any(above)) {
      rn <- rownames(transformed_table)
      rownames(transformed_table)[above] <- paste0("**", rn[above], "**")
    }
  }

  # ── 7) optionally save, then return ────────────────────────
  if (save_output) {
    filename <- paste0(
      prefix %||% "",
      base_filename,
      if (use_timestamp) paste0("_", format(Sys.time(), "%Y%m%d%H%M%S")) else "",
      ".qs"
    )
    here_save_qs(
      list(plot              = out,
           interpretation    = interpretation,
           transformed_table = transformed_table),
      file.path(save_path, filename)
    )
  }

  list(
    plot              = out,
    interpretation    = interpretation,
    transformed_table = transformed_table
  )
}

#’ Transform Table Row Names with CLI Feedback
#’
#’ This function transforms the row names of a data frame based on specified criteria
#’ and provides CLI feedback on the changes made.
#’
#’ @param df A data frame whose row names are to be transformed.
#’ @param remove_tx_prefix logical. if TRUE, removes 't' followed by numbers and underscore from the start of row names.
#’ @param remove_z_suffix logical. if TRUE, removes '_z' from the end of row names.
#’ @param use_title_case logical. if TRUE, converts row names to title case.
#’ @param remove_underscores logical. if TRUE, replaces underscores with spaces in row names.
#’
#’ @return A data frame with transformed row names.
#’
#’ @import cli
#’
#’ @keywords internal
transform_table_rownames <- function(df, label_mapping = NULL, options = list()) {
  # coerce each flag to a single TRUE/FALSE
  remove_tx_prefix   <- isTRUE(options$remove_tx_prefix)
  remove_z_suffix    <- isTRUE(options$remove_z_suffix)
  remove_underscores <- isTRUE(options$remove_underscores)
  use_title_case     <- isTRUE(options$use_title_case)

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


#’ @keywords internal
# ── Updated interpretation helper ─────────────────────────────────────────────
#’ @keywords internal
margot_interpret_marginal <- function(
    df,
    type = c("RD", "RR"),
    order = c(
      "alphabetical", "magnitude_desc", "magnitude_asc",
      "evaluebound_desc", "evaluebound_asc", "custom", "default"
    ),
    original_df = NULL,
    e_val_bound_threshold = 1
) {
  type  <- match.arg(type)
  order <- match.arg(order)
  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }
  message(glue::glue("starting interpretation with threshold = {e_val_bound_threshold}..."))

  df <- group_tab(df, type = type, order = order)
  if (!"unit" %in% names(df)) df$unit <- ""
  if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)

  effect_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
    "E[Y(1)]-E[Y(0)]"
  } else {
    "E[Y(1)]/E[Y(0)]"
  }
  null_val <- ifelse(type == "RR", 1, 0)

  # now use a single threshold for “reliable” evidence
  df_f <- df %>%
    filter(E_Value > 1, E_Val_bound > e_val_bound_threshold)

  if (nrow(df_f) == 0) {
    return(list(
      interpretation = paste0(
        "No outcomes had E-Value > 1 and E-Value bound > ",
        e_val_bound_threshold,
        "."
      )
    ))
  }

  # bullets in same order as the plot
  if (grepl("_(asc|desc)$", order)) df_f <- df_f[nrow(df_f):1, ]

  intro <- glue::glue(
    "The following outcomes showed reliable causal evidence",
    " (E-Value lower bound > {e_val_bound_threshold}):\n"
  )
  bullets <- df_f %>%
    rowwise() %>%
    mutate(
      lab = glue::glue(
        "{round(.data[[effect_col]], 3)} (",
        round(`2.5 %`, 3), ", ", round(`97.5 %`, 3), ")"
      ),
      text = glue::glue(
        "- {outcome}: {lab}; E-Value bound = {E_Val_bound}"
      )
    ) %>%
    pull(text)

  interpretation <- paste(c(intro, bullets), collapse = "\n")
  message("interpretation complete 👍")
  list(interpretation = interpretation)
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
#' result_df <- group_tab(df = analysis_df, order = 'default')
#'
#' # ascending magnitude
#' result_df <- group_tab(df = analysis_df, order = 'magnitude_asc')
#'
#' # strongest E-value bound first
#' result_df <- group_tab(df = analysis_df, order = 'evaluebound_desc')
#'
#' # alphabetical
#' result_df <- group_tab(df = analysis_df, order = 'alphabetical')
#'
#' # custom ordering
#' custom_order <- c('Outcome3','Outcome1','Outcome2')
#' result_df <- group_tab(df = analysis_df, order = 'custom', custom_order = custom_order)
#'
#' @importFrom dplyr arrange desc mutate slice
#' @importFrom tibble rownames_to_column
#' @importFrom rlang sym
#' @keywords internal
group_tab <- function(
    df,
    type = c("RD","RR"),
    order = c(
      "alphabetical",
      "magnitude_desc",
      "magnitude_asc",
      "evaluebound_desc",
      "evaluebound_asc",
      "custom",
      "default"
    ),
    custom_order = NULL
) {
  # load dplyr verbs
  require(dplyr)

  # match arguments
  type  <- match.arg(type)
  order <- match.arg(order)

  # alias old magnitude and default to magnitude_desc
  if (order %in% c("default")) {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  # handle list input
  if (is.list(df) && "results_df" %in% names(df)) {
    results_df   <- df$results_df
    label_mapping <- df$label_mapping
  } else {
    results_df   <- df
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
  ev_bound   <- "E_Val_bound"

  # apply ordering
  results_df <- switch(order,
                       alphabetical      = results_df %>% arrange(outcome),
                       magnitude_desc    = results_df %>% arrange(desc(abs(!!sym(effect_col)))),
                       magnitude_asc     = results_df %>% arrange(abs(!!sym(effect_col))),
                       evaluebound_desc  = results_df %>% arrange(desc(!!sym(ev_bound))),
                       evaluebound_asc   = results_df %>% arrange(!!sym(ev_bound)),
                       custom            = {
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
                      "not reliable"))
      } else {
        ifelse(`E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
               "positive",
               ifelse(`E[Y(1)]-E[Y(0)]` < 0 & `97.5 %` < 0,
                      "negative",
                      "not reliable"))
      }
    ),
    estimate_lab = if (type == "RR") {
      paste0(
        round(`E[Y(1)]/E[Y(0)]`, 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3),")",
        " [EV ", round(E_Value, 3), "/", round(E_Val_bound, 3), "]"
      )
    } else {
      paste0(
        round(`E[Y(1)]-E[Y(0)]`, 3), " (",
        round(`2.5 %`, 3), "-", round(`97.5 %`, 3),")",
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
