#' Batch process heterogeneity analyses across multiple outcome domains
#'
#' @description
#' runs the same planned subgroup contrasts (e.g., wealth, ethnicity,
#' political orientation) over multiple outcome domains, with optional
#' correction for multiple comparisons.
#'
#' @param domain_models list of model sets; one element per outcome domain
#' @param X model matrix (or data frame) of predictors used when the models were
#'   fitted
#' @param base_defaults named list of default arguments passed to the downstream
#'   plotting / helper functions
#' @param label_mapping named list of variable-to-label mappings; passed down to `margot_plot()`
#' @param subset_types named list of subset specifications (e.g., list(wealth = subsets_standard_wealth))
#' @param original_df the raw data frame containing all variables (needed for
#'   label recovery, plotting on the original scale, etc.)
#' @param domain_names character vector naming each element in `domain_models`
#' @param subtitles character vector of subtitles used in plot annotations;
#'   must be the same length as `domain_names`
#' @param adjust character; correction method for multiple comparisons in plots.
#'   one of "none", "bonferroni", or "holm". default: "none".
#' @param alpha numeric; significance threshold for correction. default: 0.05.
#' @param ... any additional arguments forwarded directly to [`margot_subset_batch()`]
#'
#' @return a nested list. the first level is the domain name; the second level
#'   is the subset type. each leaf contains the full list returned by
#'   `margot_subset_batch()`.
#'
#' @export
margot_planned_subgroups_batch <- function(
    domain_models,
    X,
    base_defaults,
    label_mapping = NULL,
    subset_types,
    original_df,
    domain_names,
    subtitles,
    adjust = c("none", "bonferroni", "holm"),
    alpha = 0.05,
    ...) {
  # match and validate correction args
  adjust <- match.arg(adjust)
  alpha <- as.numeric(alpha)[1]

  # initialise container
  results_list <- list()

  # iterate over domains
  for (i in seq_along(domain_models)) {
    domain_name <- domain_names[i]
    subtitle <- subtitles[i]

    cli::cli_h1(glue::glue("domain: {domain_name}"))
    results_list[[domain_name]] <- list()

    # iterate over subset families
    for (subset_name in names(subset_types)) {
      subset_spec <- subset_types[[subset_name]]

      cli::cli_alert_info(
        "processing subset '{subset_name}' ({length(subset_spec)} groups)"
      )

      # run the batch helper; forward correction args and label_mapping
      batch_result <- margot_subset_batch(
        domain_models[[i]],
        X             = X,
        base_defaults = base_defaults,
        label_mapping = label_mapping,
        subsets       = subset_spec,
        original_df   = original_df,
        adjust        = adjust,
        alpha         = alpha,
        ...
      )

      # markdown summary
      cat(glue::glue("\n## results for {domain_name} – {subset_name}\n"))
      print(
        kableExtra::kbl(batch_result$summary, format = "markdown")
      )

      # assemble composite plot
      plot_elements <- purrr::map(batch_result$results, "plot")
      ncol_val <- if (length(plot_elements) > 2) 2 else 1

      combined_plot <- patchwork::wrap_plots(plot_elements, ncol = ncol_val) +
        patchwork::plot_annotation(
          title = subtitle,
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(size = 18, face = "bold")
          )
        )
      print(combined_plot)

      # explanation
      cat("\n## explanation\n")
      cat(batch_result$explanation)

      # store result
      results_list[[domain_name]][[subset_name]] <- batch_result
    }
  }

  invisible(results_list)
}
