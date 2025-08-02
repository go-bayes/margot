#' Batch Plot QINI Curves Across Treatment Costs for Multiple Models
#'
#' @description
#' Creates visualizations showing how QINI curves change with treatment cost for
#' multiple models simultaneously. This function combines the functionality of
#' margot_qini_cost_sensitivity() and margot_plot_qini_batch() to create comprehensive
#' cost sensitivity visualizations.
#'
#' @param models List returned by margot_causal_forest(), containing results
#'   and optionally full_models.
#' @param costs Numeric vector of treatment costs to evaluate. Default is
#'   c(0.2, 0.5, 1, 2, 5) representing a range from cheap to expensive treatments.
#' @param model_names Optional character vector specifying which models to process.
#'   Default NULL (all models).
#' @param plot_type Character; how to arrange the plots:
#'   - "grid": Arrange in a grid with models as rows and costs as columns
#'   - "list": Return a nested list of individual plots
#'   - "combined": Create a single combined plot with facets
#'   Default is "grid".
#' @param spend_levels Numeric vector of spend levels for annotations.
#'   Default is 0.1.
#' @param baseline_method Method for generating baseline. See margot_qini() for details.
#'   Default is "maq_no_covariates".
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param ncol For plot_type = "grid", number of columns in the grid. Default NULL
#'   uses the number of costs.
#' @param heights For plot_type = "grid", relative heights of rows. Default NULL
#'   gives equal heights.
#' @param widths For plot_type = "grid", relative widths of columns. Default NULL
#'   gives equal widths.
#' @param verbose Logical; print progress messages (default TRUE).
#' @param x_axis Type of x-axis for QINI curves: "proportion" or "budget" (default).
#'   "budget" shows budget per unit (matching maq visualization) which better
#'   illustrates the effect of different treatment costs.
#' @param ... Additional arguments passed to margot_plot_qini().
#'
#' @return Depending on plot_type:
#' - "grid": A combined plot object (requires patchwork package)
#' - "list": A nested list where first level is model names, second level is costs
#' - "combined": A single ggplot object with facets
#'
#' @details
#' This function efficiently generates QINI curves for multiple models at different
#' treatment costs, enabling comprehensive cost sensitivity analysis. The plots
#' help identify:
#' - Which models are most sensitive to treatment cost
#' - Cost thresholds where targeting becomes ineffective
#' - Optimal cost-benefit trade-offs for different outcomes
#'
#' @examples
#' \dontrun{
#' # Create grid of plots for all models
#' grid_plots <- margot_plot_qini_batch_cost_sensitivity(
#'   cf_results,
#'   costs = c(0.5, 1, 2),
#'   plot_type = "grid"
#' )
#' print(grid_plots)
#'
#' # Get individual plots as a list
#' plot_list <- margot_plot_qini_batch_cost_sensitivity(
#'   cf_results,
#'   costs = c(0.5, 1, 2),
#'   model_names = c("anxiety", "depression"),
#'   plot_type = "list"
#' )
#' # Access specific plot: plot_list$model_anxiety$cost_0.5
#'
#' # Create combined faceted plot
#' combined_plot <- margot_plot_qini_batch_cost_sensitivity(
#'   cf_results,
#'   costs = c(0.5, 1, 2),
#'   model_names = c("anxiety", "depression"),
#'   plot_type = "combined"
#' )
#' }
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_h1 cli_h2
#' @importFrom ggplot2 ggplot labs
margot_plot_qini_batch_cost_sensitivity <- function(
    models,
    costs = c(0.2, 0.5, 1, 2, 5),
    model_names = NULL,
    plot_type = c("grid", "list", "combined"),
    spend_levels = 0.1,
    baseline_method = "maq_no_covariates",
    label_mapping = NULL,
    ncol = NULL,
    heights = NULL,
    widths = NULL,
    verbose = TRUE,
    x_axis = "budget",
    ...) {
  plot_type <- match.arg(plot_type)

  if (verbose) {
    cli::cli_h1("Batch QINI Cost Sensitivity Plots")
  }

  # run cost sensitivity analysis
  cost_sens <- margot_qini_cost_sensitivity(
    models = models,
    costs = costs,
    model_names = model_names,
    spend_levels = spend_levels,
    baseline_method = baseline_method,
    verbose = verbose
  )

  # extract processed model names
  models_to_plot <- cost_sens$models_processed

  if (length(models_to_plot) == 0) {
    stop("No models available for plotting")
  }

  if (verbose) {
    cli::cli_alert_info("Creating plots for {length(models_to_plot)} models at {length(costs)} cost levels")
  }

  # create plots based on type
  if (plot_type == "list") {
    # return nested list of individual plots
    plot_list <- list()

    for (model_name in models_to_plot) {
      plot_list[[model_name]] <- list()

      for (i in seq_along(costs)) {
        cost <- costs[i]
        cost_label <- paste0("cost_", cost)

        if (verbose) {
          cli::cli_alert_info("Creating plot for {model_name} at cost {cost}")
        }

        # get qini results for this cost
        qini_at_cost <- cost_sens$results[[cost_label]]

        # check if model exists and has qini_data
        if (!is.null(qini_at_cost[[model_name]]) &&
          !is.null(qini_at_cost[[model_name]]$qini_data)) {
          # create plot directly from QINI data
          plot <- tryCatch(
            {
              p <- margot_plot_qini_direct(
                qini_data = qini_at_cost[[model_name]]$qini_data,
                qini_objects = qini_at_cost[[model_name]]$qini_objects,
                outcome_var = model_name,
                label_mapping = label_mapping,
                spend_levels = spend_levels,
                treatment_cost = cost,
                x_axis = x_axis,
                ...
              )
              list(p)
            },
            error = function(e) {
              if (verbose) {
                cli::cli_alert_warning("Failed to create plot for {model_name} at cost {cost}: {e$message}")
              }
              list()
            }
          )
        } else {
          if (verbose) {
            cli::cli_alert_warning("No QINI data found for {model_name} at cost {cost}")
          }
          plot <- list()
        }

        if (length(plot) > 0) {
          # add cost to title
          plot[[1]] <- plot[[1]] +
            labs(subtitle = paste0("Treatment cost = ", cost))

          plot_list[[model_name]][[cost_label]] <- plot[[1]]
        }
      }
    }

    if (verbose) {
      cli::cli_alert_success("Created {length(models_to_plot) * length(costs)} individual plots")
    }

    return(plot_list)
  } else if (plot_type == "grid") {
    # create grid layout with patchwork
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for grid plots. Please install it.")
    }

    # collect all plots in order
    all_plots <- list()
    plot_labels <- character()

    for (model_name in models_to_plot) {
      model_display <- transform_var_name(
        gsub("^model_", "", model_name),
        label_mapping,
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        use_title_case = TRUE,
        remove_underscores = TRUE
      )

      for (i in seq_along(costs)) {
        cost <- costs[i]
        cost_label <- paste0("cost_", cost)

        # get qini results for this cost
        qini_at_cost <- cost_sens$results[[cost_label]]

        # check if model exists and has qini_data
        if (!is.null(qini_at_cost[[model_name]]) &&
          !is.null(qini_at_cost[[model_name]]$qini_data)) {
          # create plot directly from QINI data
          plot <- tryCatch(
            {
              p <- margot_plot_qini_direct(
                qini_data = qini_at_cost[[model_name]]$qini_data,
                qini_objects = qini_at_cost[[model_name]]$qini_objects,
                outcome_var = model_name,
                label_mapping = label_mapping,
                spend_levels = spend_levels,
                treatment_cost = cost,
                x_axis = x_axis,
                ...
              )
              list(p)
            },
            error = function(e) {
              if (verbose) {
                cli::cli_alert_warning("Failed to create plot for {model_name} at cost {cost}: {e$message}")
              }
              list()
            }
          )
        } else {
          if (verbose) {
            cli::cli_alert_warning("No QINI data found for {model_name} at cost {cost}")
          }
          plot <- list()
        }

        if (length(plot) > 0) {
          # customize title to show model and cost
          plot[[1]] <- plot[[1]] +
            labs(
              title = model_display,
              subtitle = paste0("Cost = ", cost)
            ) +
            theme(
              plot.title = element_text(size = 10, face = "bold"),
              plot.subtitle = element_text(size = 9),
              legend.position = "none" # remove legend from individual plots
            )

          all_plots[[length(all_plots) + 1]] <- plot[[1]]
          plot_labels <- c(plot_labels, paste0(model_name, "_", cost))
        }
      }
    }

    # determine grid layout
    if (is.null(ncol)) {
      ncol <- length(costs)
    }
    nrow <- ceiling(length(all_plots) / ncol)

    # create combined plot with patchwork
    combined <- patchwork::wrap_plots(
      all_plots,
      ncol = ncol,
      heights = heights,
      widths = widths
    )

    # add overall title and caption
    combined <- combined +
      patchwork::plot_annotation(
        title = "QINI Curves Across Treatment Costs",
        subtitle = paste("Models:", paste(models_to_plot, collapse = ", ")),
        caption = paste("Treatment costs evaluated:", paste(costs, collapse = ", "))
      )

    if (verbose) {
      cli::cli_alert_success("Created grid plot with {length(all_plots)} panels")
    }

    return(combined)
  } else { # combined
    # create single faceted plot
    # first collect all qini data
    all_qini_data <- list()

    for (model_name in models_to_plot) {
      model_display <- transform_var_name(
        gsub("^model_", "", model_name),
        label_mapping,
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        use_title_case = TRUE,
        remove_underscores = TRUE
      )

      for (i in seq_along(costs)) {
        cost <- costs[i]
        cost_label <- names(cost_sens$results)[i]

        qini_data <- cost_sens$results[[cost_label]][[model_name]]$qini_data

        if (!is.null(qini_data)) {
          # add model and cost information
          qini_data$model <- model_display
          qini_data$cost <- cost
          qini_data$cost_label <- paste0("Cost = ", cost)
          all_qini_data[[length(all_qini_data) + 1]] <- qini_data
        }
      }
    }

    if (length(all_qini_data) == 0) {
      stop("No QINI data available for plotting")
    }

    # combine all data
    combined_data <- do.call(rbind, all_qini_data)

    # create faceted plot
    p <- ggplot(combined_data, aes(x = proportion, y = gain, color = curve)) +
      geom_line(size = 1) +
      facet_grid(model ~ cost_label, scales = "free_y") +
      theme_minimal() +
      labs(
        title = "QINI Curves: Cost Sensitivity Analysis",
        x = "Proportion of Population Targeted",
        y = "Average Policy Effect",
        color = "Curve Type"
      ) +
      scale_color_manual(values = c("cate" = "#d8a739", "ate" = "#4d4d4d")) +
      theme(
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
      )

    # add spend level lines if requested
    if (length(spend_levels) > 0) {
      for (spend in spend_levels) {
        p <- p +
          geom_vline(
            xintercept = spend,
            linetype = "dashed",
            color = "red",
            alpha = 0.3
          )
      }
    }

    if (verbose) {
      cli::cli_alert_success("Created combined faceted plot")
    }

    return(p)
  }
}
