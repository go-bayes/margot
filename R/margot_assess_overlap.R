#' Assess Covariate Overlap from Causal Forest Models
#'
#' @description
#' This function analyzes propensity score overlap and covariate balance
#' from the outputs of margot_causal_forest() or margot_flip_forests().
#' It provides diagnostics for assessing the validity of causal estimates.
#'
#' @param model_results Output from margot_causal_forest() or margot_flip_forests()
#' @param model_names Character vector of model names to assess. If NULL, all models.
#' @param exposure_name Character string naming the exposure/treatment variable. If NULL, defaults to "Treatment".
#' @param label_mapping Named list mapping variable names to custom labels for the exposure.
#' @param plot Logical indicating whether to create overlap plots. Default is TRUE.
#' @param save_plots Logical indicating whether to save plots. Default is FALSE.
#' @param output_dir Directory to save plots if save_plots is TRUE.
#' @param theme Character string specifying the ggplot2 theme. Default is "classic". Options include "classic", "minimal", "bw", "gray", "light", "dark", "void".
#' @param verbose Logical for detailed messages. Default is TRUE.
#'
#' @return A list containing:
#' \itemize{
#'   \item{overlap_summary}{Data frame with overlap statistics for each model}
#'   \item{propensity_plots}{List of ggplot objects (if plot = TRUE)}
#'   \item{balance_tables}{Covariate balance tables for each model}
#'   \item{trimming_summary}{Summary of observations trimmed due to poor overlap}
#'   \item{text_summary}{Character string with a prose summary suitable for reports}
#' }
#'
#' @details
#' The function assesses overlap using several metrics:
#' \itemize{
#'   \item Propensity score distributions by treatment group
#'   \item Common support region statistics
#'   \item Test calibration from grf (differential prediction test)
#'   \item Covariate balance within propensity score strata
#' }
#'
#' Poor overlap (propensity scores near 0 or 1) indicates limited comparability
#' between treatment groups and may lead to unreliable causal estimates.
#'
#' @examples
#' \dontrun{
#' # assess overlap for all models
#' overlap_results <- margot_assess_overlap(model_results)
#'
#' # assess specific models only
#' overlap_results <- margot_assess_overlap(
#'   model_results,
#'   model_names = c("model_outcome1", "model_outcome2")
#' )
#'
#' # save plots
#' overlap_results <- margot_assess_overlap(
#'   model_results,
#'   save_plots = TRUE,
#'   output_dir = "output/overlap_diagnostics"
#' )
#'
#' # use text summary in a report
#' cat(overlap_results$text_summary)
#'
#' # use different theme
#' overlap_results <- margot_assess_overlap(
#'   model_results,
#'   theme = "minimal"
#' )
#' }
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density facet_wrap
#' @importFrom dplyr bind_rows mutate group_by summarise
#' @importFrom stats quantile
margot_assess_overlap <- function(model_results,
                                  model_names = NULL,
                                  exposure_name = NULL,
                                  label_mapping = NULL,
                                  plot = TRUE,
                                  save_plots = FALSE,
                                  output_dir = NULL,
                                  theme = "classic",
                                  verbose = TRUE) {
  # soft deprecation notice
  if (isTRUE(requireNamespace("cli", quietly = TRUE))) {
    cli::cli_alert_warning("`margot_assess_overlap()` is soft-deprecated; use `margot_grf_overlap()` instead.")
  }
  # validate inputs
  if (!is.list(model_results) || !"full_models" %in% names(model_results)) {
    stop("model_results must contain full_models (ensure save_models = TRUE in margot_causal_forest)")
  }

  # determine which models to assess
  if (is.null(model_names)) {
    model_names <- names(model_results$full_models)
  } else {
    # ensure model names have prefix if needed
    model_prefix <- "model_"
    model_names <- ifelse(
      grepl(paste0("^", model_prefix), model_names),
      model_names,
      paste0(model_prefix, model_names)
    )
    model_names <- intersect(model_names, names(model_results$full_models))
  }

  if (length(model_names) == 0) {
    stop("no valid models found to assess")
  }

  # set up exposure name and label
  if (is.null(exposure_name)) {
    exposure_name <- "Treatment"
    if (verbose) {
      cli::cli_alert_info("no exposure_name specified, using default: 'Treatment'")
    }
  }

  # get exposure label using transform_label helper
  exposure_label <- transform_label(
    label = exposure_name,
    label_mapping = label_mapping,
    options = list(
      remove_tx_prefix = TRUE,
      remove_z_suffix = TRUE,
      remove_underscores = TRUE,
      use_title_case = TRUE
    )
  )

  if (verbose) {
    cli::cli_alert_info("assessing overlap for exposure: {exposure_label}")
  }

  # create output directory if needed
  if (save_plots && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }

  # initialize results
  overlap_summary <- list()
  propensity_plots <- list()
  balance_tables <- list()
  trimming_summary <- list()

  # extract treatment and propensity scores from first model (same for all)
  first_model <- model_results$full_models[[model_names[1]]]
  W <- first_model$W.orig
  W_hat <- first_model$W.hat

  # calculate overlap statistics once (for the exposure)
  overlap_stats <- calculate_overlap_statistics(W, W_hat, exposure_label)

  # create propensity score plot if requested
  if (plot) {
    p <- create_propensity_plot(W, W_hat, exposure_label, theme)
    propensity_plots[["exposure"]] <- p

    if (save_plots && !is.null(output_dir)) {
      filename <- file.path(output_dir, paste0("propensity_", exposure_name, ".png"))
      ggplot2::ggsave(filename, p, width = 8, height = 6, dpi = 300)
      if (verbose) cli::cli_alert_success("saved plot to {filename}")
    }
  }

  # calculate covariate balance if available
  if (!is.null(model_results$covariates)) {
    # use first model's top vars or all covariates
    top_vars <- NULL
    if (length(model_names) > 0 && !is.null(model_results$results[[model_names[1]]]$top_vars)) {
      top_vars <- model_results$results[[model_names[1]]]$top_vars
    }

    balance <- calculate_covariate_balance(
      covariates = model_results$covariates,
      W = W,
      W_hat = W_hat,
      top_vars = top_vars
    )
    balance_tables[["exposure"]] <- balance
  }

  # calculate trimming summary
  trim_summary <- calculate_trimming_summary(W_hat)
  trimming_summary[["exposure"]] <- trim_summary

  # process each model for test calibration and other model-specific metrics
  for (model_name in model_names) {
    if (verbose) cli::cli_alert_info("processing test calibration for {model_name}")

    # get outcome name
    outcome_name <- gsub("^model_", "", model_name)

    # apply transform_label to outcome if no label_mapping provided
    outcome_label <- transform_label(
      label = outcome_name,
      label_mapping = label_mapping,
      options = list(
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        remove_underscores = TRUE,
        use_title_case = TRUE
      )
    )

    # create model-specific stats with overlap info
    model_stats <- overlap_stats
    model_stats$model <- model_name
    model_stats$outcome <- outcome_name
    model_stats$outcome_label <- outcome_label

    # get test calibration
    test_cal <- model_results$results[[model_name]]$test_calibration
    if (!is.null(test_cal)) {
      # test_calibration might be a vector or matrix, handle appropriately
      if (is.matrix(test_cal) || (is.numeric(test_cal) && length(test_cal) > 1)) {
        # if multiple values, store the p-value (typically the second element)
        if (length(test_cal) >= 2) {
          model_stats$test_calibration_pvalue <- test_cal[2]
        }
      } else {
        model_stats$test_calibration_pvalue <- test_cal
      }
    }

    # store the model-specific statistics
    overlap_summary[[model_name]] <- model_stats
  }

  # combine summaries
  combined_summary <- dplyr::bind_rows(overlap_summary, .id = "model")

  # create text summary for easy document inclusion
  if (overlap_stats$poor_overlap_pct > 10) {
    text_summary <- sprintf(
      "Overlap refers to the extent to which treated and control groups have similar baseline characteristics. Poor overlap means some individuals have characteristics that make them almost certain to be in one group or the other, making it difficult to estimate causal effects reliably for these individuals. The exposure '%s' has %s%% of observations with poor overlap (propensity scores <0.05 or >0.95), suggesting limited comparability between treatment groups for %s%% of the sample.",
      exposure_label,
      overlap_stats$poor_overlap_pct,
      overlap_stats$poor_overlap_pct
    )
  } else {
    text_summary <- sprintf(
      "Overlap refers to the extent to which treated and control groups have similar baseline characteristics. Good overlap means we can find comparable individuals in both treatment and control groups, which strengthens our ability to estimate causal effects. The exposure '%s' has good overlap, with %s%% of observations having propensity scores between 0.1 and 0.9, indicating strong comparability between treatment groups.",
      exposure_label,
      overlap_stats$good_overlap_pct
    )
  }

  if (verbose) {
    cli::cli_alert_success("overlap assessment complete")

    # print summary of overlap
    if (overlap_stats$poor_overlap_pct > 10) {
      cli::cli_alert_warning(
        "exposure '{exposure_label}' has {overlap_stats$poor_overlap_pct}% of observations with poor overlap (propensity scores <0.05 or >0.95)"
      )
    } else {
      cli::cli_alert_success(
        "exposure '{exposure_label}' has good overlap ({overlap_stats$good_overlap_pct}% of observations have propensity scores between 0.1 and 0.9)"
      )
    }
  }

  return(list(
    overlap_summary = combined_summary,
    propensity_plots = propensity_plots,
    balance_tables = balance_tables,
    trimming_summary = trimming_summary,
    text_summary = text_summary
  ))
}

#' Calculate overlap statistics
#' @keywords internal
calculate_overlap_statistics <- function(W, W_hat, exposure_name) {
  # define overlap regions
  good_overlap <- W_hat > 0.1 & W_hat < 0.9
  moderate_overlap <- (W_hat > 0.05 & W_hat <= 0.1) | (W_hat >= 0.9 & W_hat < 0.95)
  poor_overlap <- W_hat <= 0.05 | W_hat >= 0.95

  # calculate statistics by treatment group
  treated_props <- W_hat[W == 1]
  control_props <- W_hat[W == 0]

  stats <- data.frame(
    exposure = exposure_name,
    n_total = length(W),
    n_treated = sum(W == 1),
    n_control = sum(W == 0),
    good_overlap_pct = round(100 * mean(good_overlap), 1),
    moderate_overlap_pct = round(100 * mean(moderate_overlap), 1),
    poor_overlap_pct = round(100 * mean(poor_overlap), 1),
    prop_score_min = round(min(W_hat), 3),
    prop_score_max = round(max(W_hat), 3),
    prop_score_mean = round(mean(W_hat), 3),
    prop_score_sd = round(sd(W_hat), 3),
    treated_prop_mean = round(mean(treated_props), 3),
    control_prop_mean = round(mean(control_props), 3),
    common_support_lower = round(max(min(treated_props), min(control_props)), 3),
    common_support_upper = round(min(max(treated_props), max(control_props)), 3)
  )

  return(stats)
}

#' Create propensity score plot
#' @keywords internal
create_propensity_plot <- function(W, W_hat, exposure_name, theme = "classic") {
  # create data frame for plotting
  plot_data <- data.frame(
    propensity_score = W_hat,
    treatment = factor(W, levels = c(0, 1), labels = c("Control", "Treated"))
  )

  # create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = propensity_score, fill = treatment)) +
    ggplot2::geom_histogram(
      alpha = 0.85,
      position = "identity",
      bins = 30,
      color = "white",
      linewidth = 0.5
    ) +
    ggplot2::facet_wrap(~treatment, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("Control" = "#4f88c6", "Treated" = "#d8a739")) +
    ggplot2::geom_vline(xintercept = c(0.1, 0.9), linetype = "dashed", alpha = 0.7, color = "black", linewidth = 0.7) +
    ggplot2::labs(
      title = paste("Propensity Score Distribution:", exposure_name),
      subtitle = "Dashed lines indicate common support region (0.1, 0.9)",
      x = "Propensity Score",
      y = "Count",
      fill = "Treatment"
    ) +
    # apply selected theme
    switch(theme,
      "classic" = ggplot2::theme_classic(),
      "minimal" = ggplot2::theme_minimal(),
      "bw" = ggplot2::theme_bw(),
      "gray" = ggplot2::theme_gray(),
      "light" = ggplot2::theme_light(),
      "dark" = ggplot2::theme_dark(),
      "void" = ggplot2::theme_void(),
      ggplot2::theme_classic() # default fallback
    ) +
    ggplot2::theme(legend.position = "none")

  return(p)
}

#' Calculate covariate balance
#' @keywords internal
calculate_covariate_balance <- function(covariates, W, W_hat, top_vars = NULL) {
  # use top variables if specified, otherwise use all
  if (!is.null(top_vars)) {
    covariates <- covariates[, top_vars, drop = FALSE]
  }

  # stratify by propensity score quintiles
  ps_quintiles <- cut(W_hat,
    breaks = quantile(W_hat, probs = seq(0, 1, 0.2)),
    include.lowest = TRUE,
    labels = FALSE
  )

  # calculate standardized differences within strata
  balance_list <- list()

  for (var in colnames(covariates)) {
    var_data <- covariates[[var]]

    # skip if not numeric
    if (!is.numeric(var_data)) next

    # overall balance
    treated_mean <- mean(var_data[W == 1], na.rm = TRUE)
    control_mean <- mean(var_data[W == 0], na.rm = TRUE)
    pooled_sd <- sqrt((var(var_data[W == 1], na.rm = TRUE) +
      var(var_data[W == 0], na.rm = TRUE)) / 2)

    std_diff_overall <- (treated_mean - control_mean) / pooled_sd

    # balance within quintiles
    std_diff_quintiles <- numeric(5)
    for (q in 1:5) {
      in_quintile <- ps_quintiles == q
      if (sum(W[in_quintile] == 1) > 1 && sum(W[in_quintile] == 0) > 1) {
        treated_mean_q <- mean(var_data[in_quintile & W == 1], na.rm = TRUE)
        control_mean_q <- mean(var_data[in_quintile & W == 0], na.rm = TRUE)
        pooled_sd_q <- sqrt((var(var_data[in_quintile & W == 1], na.rm = TRUE) +
          var(var_data[in_quintile & W == 0], na.rm = TRUE)) / 2)
        std_diff_quintiles[q] <- (treated_mean_q - control_mean_q) / pooled_sd_q
      } else {
        std_diff_quintiles[q] <- NA
      }
    }

    balance_list[[var]] <- list(
      variable = var,
      std_diff_overall = round(std_diff_overall, 3),
      std_diff_q1 = round(std_diff_quintiles[1], 3),
      std_diff_q2 = round(std_diff_quintiles[2], 3),
      std_diff_q3 = round(std_diff_quintiles[3], 3),
      std_diff_q4 = round(std_diff_quintiles[4], 3),
      std_diff_q5 = round(std_diff_quintiles[5], 3),
      max_abs_std_diff = round(max(abs(std_diff_quintiles), na.rm = TRUE), 3)
    )
  }

  # convert to data frame
  balance_df <- dplyr::bind_rows(balance_list)

  return(balance_df)
}

#' Calculate trimming summary
#' @keywords internal
calculate_trimming_summary <- function(W_hat, bounds = c(0.05, 0.95)) {
  summary <- list(
    n_total = length(W_hat),
    n_trimmed_lower = sum(W_hat < bounds[1]),
    n_trimmed_upper = sum(W_hat > bounds[2]),
    n_trimmed_total = sum(W_hat < bounds[1] | W_hat > bounds[2]),
    pct_trimmed = round(100 * mean(W_hat < bounds[1] | W_hat > bounds[2]), 1),
    bounds = bounds
  )

  return(summary)
}
