#' @title Create Faceted Tau Hat Distribution Plots
#' @description
#' creates a faceted grid of histograms showing the distribution of tau hat
#' (individual treatment effects) for multiple models. the range is standardised
#' across all facets to facilitate comparison. automatically detects and handles
#' models_binary structures by extracting the $results component.
#'
#' @param models_list list of models or results object containing models with
#'   tau_hat values. can be a nested list structure like
#'   `models_binary$results`, or the parent `models_binary` object itself
#'   (in which case `$results` will be extracted automatically).
#' @param label_mapping optional named list for transforming model names to
#'   display labels. if null, uses automatic label transformation.
#' @param binwidth numeric; width of histogram bins. default 0.01.
#' @param base_size numeric; base font size for the plot. default 14.
#' @param ncol integer; number of columns in facet grid. if null, automatically
#'   determined based on number of models.
#' @param title character; main title for the plot. default null.
#' @param subtitle character; subtitle for the plot. default null.
#' @param x_label character; label for x-axis. default uses expression for tau.
#' @param show_zero_line logical; whether to show vertical line at zero. default true.
#' @param fill_colour character; fill colour for histogram bars when colour_by_sign is false. default "white".
#' @param border_colour character; border colour for histogram bars. default NA (no border).
#' @param colour_by_sign logical; whether to colour bars differently above/below zero. default true.
#' @param colour_below character; colour for bars below zero when colour_by_sign is true. default "#4f88c6".
#' @param colour_above character; colour for bars above zero when colour_by_sign is true. default "#d8a739".
#' @param zero_line_colour character; colour for zero line. default "red".
#' @param zero_line_alpha numeric; transparency for zero line. default 0.5.
#' @param remove_tx_prefix logical; remove time prefixes from model names. default true.
#' @param remove_z_suffix logical; remove _z suffix from model names. default true.
#' @param use_title_case logical; convert labels to title case. default true.
#' @param remove_underscores logical; replace underscores with spaces. default true.
#' @param free_scales logical; whether to allow free scales in facets. default false
#'   to maintain fixed range across all facets.
#' @param theme character; ggplot2 theme to use. options include "classic", "minimal",
#'   "bw", "light", "dark", "void", "grey", "linedraw". default "void".
#'
#' @return a ggplot object with faceted tau hat distributions
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline facet_wrap
#'   coord_cartesian labs theme_minimal theme_classic theme_bw theme_light
#'   theme_dark theme_void theme_grey theme_linedraw theme element_text element_line
#'   scale_fill_manual
#' @importFrom dplyr bind_rows group_by summarise mutate count case_when
#' @importFrom cli cli_alert_warning cli_alert_info cli_alert_success
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
#' # with label mapping - pass models_binary directly
#' label_map <- list(
#'   "model_t2_belong_z" = "Social Belonging",
#'   "model_t2_trust_z" = "Trust in Others",
#'   "model_t2_log_charity_donate_z" = "Charitable Donations",
#'   "model_t2_log_hours_charity_z" = "Volunteer Hours"
#' )
#'
#' # method 1: pass the parent object (auto-extracts $results)
#' tau_plot <- margot_plot_tau(
#'   models_binary,
#'   label_mapping = label_map,
#'   title = "Individual Treatment Effects"
#' )
#'
#' # method 2: pass $results directly (also works)
#' tau_plot <- margot_plot_tau(
#'   models_binary$results,
#'   label_mapping = label_map,
#'   title = "Individual Treatment Effects"
#' )
#'
#' # with different theme
#' tau_plot <- margot_plot_tau(
#'   models_binary,
#'   label_mapping = label_map,
#'   title = "Individual Treatment Effects",
#'   theme = "minimal"
#' )
#'
#' # without conditional colouring
#' tau_plot <- margot_plot_tau(
#'   models_binary,
#'   label_mapping = label_map,
#'   title = "Individual Treatment Effects",
#'   colour_by_sign = FALSE,
#'   fill_colour = "lightblue"
#' )
#'
#' # with custom colours for above/below zero
#' tau_plot <- margot_plot_tau(
#'   models_binary,
#'   label_mapping = label_map,
#'   title = "Individual Treatment Effects",
#'   colour_below = "darkred",
#'   colour_above = "darkgreen"
#' )
#'
#' # add borders if desired
#' tau_plot <- margot_plot_tau(
#'   models_binary,
#'   label_mapping = label_map,
#'   border_colour = "grey50"
#' )
#'
#' # without label mapping (auto transform)
#' tau_plot <- margot_plot_tau(models_binary)
#' }
#'
#' @export
margot_plot_tau <- function(
    models_list,
    label_mapping      = NULL,
    binwidth           = 0.01,
    base_size          = 14,
    ncol               = NULL,
    title              = NULL,
    subtitle           = NULL,
    x_label            = expression(tau[i]),
    show_zero_line     = TRUE,
    fill_colour        = "white",
    border_colour      = NA,
    colour_by_sign     = TRUE,
    colour_below       = "#4f88c6",
    colour_above       = "#d8a739",
    zero_line_colour   = "red",
    zero_line_alpha    = 0.5,
    remove_tx_prefix   = TRUE,
    remove_z_suffix    = TRUE,
    use_title_case     = TRUE,
    remove_underscores = TRUE,
    free_scales        = FALSE,
    theme              = "void"
) {

  # validate inputs --------------------------------------------------------
  if (!is.list(models_list)) {
    stop("models_list must be a list of models")
  }

  # validate theme input
  valid_themes <- c("classic", "minimal", "bw", "light", "dark", "void", "grey", "linedraw")
  if (!theme %in% valid_themes) {
    stop("theme must be one of: ", paste(valid_themes, collapse = ", "))
  }

  # smart handling for models_binary structure -----------------------------
  # if user passes models_binary, extract $results automatically
  if ("results" %in% names(models_list) &&
      any(c("combined_table", "outcome_vars", "data", "covariates", "full_models") %in% names(models_list))) {
    cli::cli_alert_info("detected models_binary structure, using $results")
    if (!is.list(models_list$results)) {
      stop("models_binary$results is not a valid list")
    }
    models_list <- models_list$results
  }

  # extract tau_hat values from models -------------------------------------
  tau_data_list <- list()
  model_names <- names(models_list)

  # ensure we have meaningful names
  if (is.null(model_names) || all(model_names == "")) {
    model_names <- paste0("Model ", seq_along(models_list))
  }

  # loop through models and extract tau_hat
  for (i in seq_along(models_list)) {
    model <- models_list[[i]]
    model_name <- model_names[i]

    # skip if name is just "results" or empty
    if (model_name %in% c("results", "")) {
      next
    }

    # handle different model structures
    if ("tau_hat" %in% names(model)) {
      tau_values <- model$tau_hat
    } else if (is.list(model) && length(model) > 0 && "tau_hat" %in% names(model[[1]])) {
      # nested structure
      tau_values <- model[[1]]$tau_hat
    } else {
      cli::cli_alert_warning("no tau_hat found for {model_name}, skipping")
      next
    }

    # create data frame for this model
    if (length(tau_values) > 0 && is.numeric(tau_values)) {
      tau_data_list[[model_name]] <- data.frame(
        tau_hat = tau_values,
        model = model_name,
        stringsAsFactors = FALSE
      )
    }
  }

  # check if we have any valid data ----------------------------------------
  if (length(tau_data_list) == 0) {
    stop("no valid tau_hat values found in any models")
  }

  # show which models we're processing
  cli::cli_alert_info("processing {length(tau_data_list)} model{?s}: {.val {names(tau_data_list)}}")

  # combine all tau data
  tau_data_combined <- dplyr::bind_rows(tau_data_list)

  # transform model names --------------------------------------------------
  # get unique model names first to avoid repetitive transformations
  unique_models <- unique(tau_data_combined$model)
  model_display_map <- setNames(
    vapply(
      unique_models,
      function(x) {
        transform_var_name(
          x,
          label_mapping      = label_mapping,
          remove_tx_prefix   = remove_tx_prefix,
          remove_z_suffix    = remove_z_suffix,
          use_title_case     = use_title_case,
          remove_underscores = remove_underscores
        )
      },
      character(1)
    ),
    unique_models
  )

  # apply the mapping
  tau_data_combined$model_display <- model_display_map[tau_data_combined$model]

  # determine global range for consistent scales ---------------------------
  global_min <- min(tau_data_combined$tau_hat, na.rm = TRUE)
  global_max <- max(tau_data_combined$tau_hat, na.rm = TRUE)

  # ensure zero is always included in the range
  global_min <- min(global_min, 0)
  global_max <- max(global_max, 0)

  # add buffer to range
  range_buffer <- (global_max - global_min) * 0.05
  plot_limits <- c(global_min - range_buffer, global_max + range_buffer)

  # determine facet layout -------------------------------------------------
  n_models <- length(unique(tau_data_combined$model))

  if (is.null(ncol)) {
    ncol <- dplyr::case_when(
      n_models == 1 ~ 1,
      n_models <= 4 ~ 2,
      n_models <= 9 ~ 3,
      n_models <= 16 ~ 4,
      TRUE ~ 5
    )
  }

  # create plot ------------------------------------------------------------
  if (colour_by_sign) {
    # create a factor variable for cleaner colour mapping
    tau_data_combined$sign_group <- factor(
      ifelse(tau_data_combined$tau_hat > 0, "above zero", "below zero"),
      levels = c("below zero", "above zero")
    )

    p <- ggplot2::ggplot(tau_data_combined, ggplot2::aes(x = tau_hat)) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = sign_group),
        binwidth = binwidth,
        colour   = border_colour,
        boundary = 0,
        closed   = "left"
      ) +
      ggplot2::scale_fill_manual(
        values = c("below zero" = colour_below, "above zero" = colour_above),
        guide = "none"
      )
  } else {
    # standard single colour histogram
    p <- ggplot2::ggplot(tau_data_combined, ggplot2::aes(x = tau_hat)) +
      ggplot2::geom_histogram(
        binwidth = binwidth,
        colour   = border_colour,
        fill     = fill_colour,
        boundary = 0
      )
  }

  # add zero line if requested
  if (show_zero_line) {
    p <- p + ggplot2::geom_vline(
      xintercept = 0,
      colour     = zero_line_colour,
      alpha      = zero_line_alpha,
      linetype   = "dashed"
    )
  }

  # add faceting
  if (n_models > 1) {
    p <- p + ggplot2::facet_wrap(
      ~ model_display,
      ncol   = ncol,
      scales = if (free_scales) "free" else "fixed"
    )
  } else {
    # for single model, add model name as subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- unique(tau_data_combined$model_display)
    }
  }

  # set fixed x-axis limits if not using free scales
  if (!free_scales) {
    p <- p + ggplot2::coord_cartesian(xlim = plot_limits)
  }

  # add labels
  p <- p + ggplot2::labs(
    x        = x_label,
    y        = "Count",
    title    = title,
    subtitle = subtitle
  )

  # apply selected theme with base_size
  p <- p + switch(
    theme,
    "classic" = ggplot2::theme_classic(base_size = base_size),
    "minimal" = ggplot2::theme_minimal(base_size = base_size),
    "bw"      = ggplot2::theme_bw(base_size = base_size),
    "light"   = ggplot2::theme_light(base_size = base_size),
    "dark"    = ggplot2::theme_dark(base_size = base_size),
    "void"    = ggplot2::theme_void(base_size = base_size),
    "grey"    = ggplot2::theme_grey(base_size = base_size),
    "linedraw"= ggplot2::theme_linedraw(base_size = base_size)
  )

  # add custom theme modifications
  p <- p + ggplot2::theme(
    axis.title    = ggplot2::element_text(face = "bold"),
    strip.text    = ggplot2::element_text(face = "bold", size = base_size * 0.9),
    plot.title    = ggplot2::element_text(face = "bold", size = base_size * 1.2),
    plot.subtitle = ggplot2::element_text(size = base_size * 0.9)
  )

  # add summary statistics as caption if few models ------------------------
  if (n_models <= 3) {
    summary_stats <- tau_data_combined %>%
      dplyr::group_by(model_display) %>%
      dplyr::summarise(
        mean_tau = mean(tau_hat, na.rm = TRUE),
        sd_tau   = sd(tau_hat, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        stat_text = paste0(
          model_display, ": mean=", round(mean_tau, 3),
          ", sd=", round(sd_tau, 3)
        )
      )

    caption_text <- paste(summary_stats$stat_text, collapse = "; ")
    p <- p + ggplot2::labs(caption = caption_text)
  }

  # edge case handling -----------------------------------------------------
  # warn if very few observations
  obs_per_model <- tau_data_combined %>%
    dplyr::count(model)

  if (any(obs_per_model$n < 30)) {
    models_with_few_obs <- obs_per_model$model[obs_per_model$n < 30]
    cli::cli_alert_warning(
      "some models have fewer than 30 observations: {.val {models_with_few_obs}}"
    )
  }

  # inform about range
  cli::cli_alert_info(
    "tau values range from {round(global_min, 3)} to {round(global_max, 3)}"
  )
  cli::cli_alert_success(
    "created faceted plot for {n_models} model{?s}: {.val {unique(tau_data_combined$model_display)}}"
  )

  p
}
