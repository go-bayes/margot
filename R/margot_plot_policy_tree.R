#' Plot Policy Tree Results
#'
#' This function creates a visualization of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates two plots showing the relationships between the top three split variables,
#' with points colored by the predicted optimal treatment. The function now includes label transformation options
#' and provides informative CLI messages throughout the process.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to plot.
#' @param original_df Optional dataframe with untransformed variables, used to display split values on the data scale.
#' @param color_scale An optional ggplot2 color scale. If NULL (default), a color scale based on the Okabe-Ito palette is created,
#'        with blue for the first level, orange for the second, and subsequent colors for additional levels.
#' @param point_alpha Numeric value between 0 and 1 for point transparency. Default is 0.5.
#' @param theme_function A ggplot2 theme function. Default is theme_classic.
#' @param title_size Numeric value for the size of the plot title. Default is 14.
#' @param subtitle_size Numeric value for the size of the plot subtitle. Default is 11.
#' @param axis_title_size Numeric value for the size of axis titles. Default is 10.
#' @param legend_title_size Numeric value for the size of the legend title. Default is 10.
#' @param jitter_width Numeric value for the amount of horizontal jitter. Default is 0.3.
#' @param jitter_height Numeric value for the amount of vertical jitter. Default is 0.3.
#' @param split_line_color Color of the split lines. Default is "darkgray".
#' @param split_line_alpha Numeric value between 0 and 1 for split line transparency. Default is 0.7.
#' @param split_line_type Line type for split lines. Default is "dashed".
#' @param split_line_linewidth Numeric value for the thickness of split lines. Default is 0.5.
#' @param split_label_size Numeric value for the size of split value labels. Default is 10.
#' @param split_label_color Color of split value labels. Default is "darkgray".
#' @param custom_action_names Optional vector of custom names for the actions. Must match the number of actions in the policy tree.
#' @param legend_position String specifying the position of the legend. Can be "top", "bottom", "left", or "right". Default is "bottom".
#' @param plot_selection String specifying which plots to display: "both", "p1", or "p2". Default is "both".
#' @param remove_tx_prefix Logical value indicating whether to remove the "tx_" prefix from labels. Default is TRUE.
#' @param remove_z_suffix Logical value indicating whether to remove the "_z" suffix from labels. Default is TRUE.
#' @param use_title_case Logical value indicating whether to convert labels to title case. Default is TRUE.
#' @param remove_underscores Logical value indicating whether to remove underscores from labels. Default is TRUE.
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names (with or without "model_" prefix),
#'        and values should be the desired display labels. Default is NULL.
#'
#' @return A ggplot object containing the specified plot(s) of the policy tree results.
#'
#' @import ggplot2
#' @import patchwork
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Default (both plots, legend at bottom)
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z")
#'
#' # Only the first plot (p1)
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", plot_selection = "p1")
#'
#' # Both plots with legend on the right
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", legend_position = "right")
#'
#' # Custom color scale
#' custom_scale <- scale_colour_manual(values = c("red", "green", "blue"))
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", color_scale = custom_scale)
#'
#' # Customize label transformations
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z",
#'                                 remove_tx_prefix = FALSE,
#'                                 remove_z_suffix = FALSE,
#'                                 use_title_case = FALSE,
#'                                 remove_underscores = FALSE)
#'
#' # Use custom label mapping and original_df for unstandardized values
#' label_mapping <- list(
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
#' )
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_env_not_climate_chg_concern_z",
#'                                 label_mapping = label_mapping,
#'                                 original_df = original_df)
#' }
#'
#' @export
margot_plot_policy_tree <- function(mc_test, model_name,
                                    original_df = NULL,
                                    color_scale = NULL,
                                    point_alpha = 0.5,
                                    theme_function = theme_classic,
                                    title_size = 14,
                                    subtitle_size = 11,
                                    axis_title_size = 10,
                                    legend_title_size = 10,
                                    jitter_width = 0.3,
                                    jitter_height = 0.3,
                                    split_line_color = "darkgray",
                                    split_line_alpha = 0.7,
                                    split_line_type = "dashed",
                                    split_line_linewidth = 0.5,
                                    split_label_size = 10,
                                    split_label_color = "darkgray",
                                    custom_action_names = NULL,
                                    legend_position = "bottom",
                                    plot_selection = "both",
                                    remove_tx_prefix = TRUE,
                                    remove_z_suffix = TRUE,
                                    use_title_case = TRUE,
                                    remove_underscores = TRUE,
                                    label_mapping = NULL
                                    ) {

  cli::cli_h1("Margot Plot Policy Tree")

  # ensure title is a valid character string or NULL
  title <- transform_var_name(model_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
  if (!is.character(title) || is.null(title)) {
    title <- as.character(title)  # Force title to be a string if NULL
  }

  cli::cli_alert_info("Using title: {title}")

  # extract policy tree object
  policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
  if (is.null(policy_tree_obj)) {
    cli::cli_abort("Policy tree object not found for the specified model name.")
  }

  cli::cli_alert_success("Policy tree object extracted for model: {model_name}")

  # Extract action names and number of actions from the policy tree object
  action_names <- policy_tree_obj$action.names
  n_actions <- policy_tree_obj$n.actions

  # if custom action names are provided, use them instead
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != n_actions) {
      cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
    cli::cli_alert_info("Using custom action names")
  }

  # transform action names using transform_var_name
  action_names <- sapply(action_names, function(x) transform_var_name(x, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores))
  cli::cli_alert_success("Action names transformed")

  # Define the Okabe-Ito palette, starting with blue and orange
  okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73",
                         "#F0E442", "#0072B2", "#D55E00",
                         "#CC79A7", "#000000")

  # if color_scale is not provided, create it based on the number of actions
  if (is.null(color_scale)) {
    # Create a named vector of colors
    color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)
    color_scale <- scale_colour_manual(values = color_vector)
    cli::cli_alert_info("Created color scale using Okabe-Ito palette")
  }

  # extract the plot data for the specified model
  plot_data <- mc_test$results[[model_name]]$plot_data
  cli::cli_alert_success("Plot data extracted")

  # extract X_test, predictions, and split_variables
  X_test <- plot_data$X_test
  predictions <- plot_data$predictions

  # extract split variables and their values from the policy tree
  policy_tree <- policy_tree_obj$nodes
  x1_col <- policy_tree[[1]]$split_variable
  x2_col <- policy_tree[[2]]$split_variable
  x3_col <- policy_tree[[3]]$split_variable

  # Extract split values
  x1_split <- policy_tree[[1]]$split_value
  x2_split <- policy_tree[[2]]$split_value
  x3_split <- policy_tree[[3]]$split_value

  # use the actual column names from X_test and transform them using transform_var_name
  actual_columns <- names(X_test)
  transformed_columns <- sapply(actual_columns, function(x) transform_var_name(x, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores))
  cli::cli_alert_success("Column names transformed")

  #create a data frame for plotting
  plot_df <- data.frame(
    x1 = X_test[[actual_columns[x1_col]]],
    x2 = X_test[[actual_columns[x2_col]]],
    x3 = X_test[[actual_columns[x3_col]]],
    prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
  )
  cli::cli_alert_success("Plot data frame created")

  # function to create a secondary axis with split value and original split value
  create_secondary_axis <- function(primary_range, split_value, var_name) {
    secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
    if (!is.null(original_df)) {
      original_split <- get_original_value_plot(var_name, split_value, original_df)
      if (!is.null(original_split)) {
        # Format: Split: standardized_value (original_value)*
        secondary_labels <- c("", sprintf("Split: %.2f (%s)*", split_value, format(original_split, scientific = FALSE, big.mark = ",")), "")
      } else {
        secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
      }
    } else {
      secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
    }
    sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
  }

  # **modify the base_plot function to check label_mapping for x_var_name and y_var_name:**

  base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split, x_var_name, y_var_name) {
    # Get the appropriate variable names for original value lookup
    lookup_x_var_name <- x_var_name
    lookup_y_var_name <- y_var_name

    # Add enhanced mapping lookup for original value lookups
    if (!is.null(original_df) && !is.null(label_mapping)) {
      # Check if we have reverse mappings (from mapped name to original name)
      reverse_mappings <- names(label_mapping)[sapply(label_mapping, function(mapped)
        mapped == x_lab || mapped == y_lab)]

      if (length(reverse_mappings) > 0) {
        # Use the original variable name for lookup if we find a reverse mapping
        for (orig_name in reverse_mappings) {
          if (label_mapping[[orig_name]] == x_lab) lookup_x_var_name <- orig_name
          if (label_mapping[[orig_name]] == y_lab) lookup_y_var_name <- orig_name
        }
        cli::cli_alert_info("Using original variable names for split value lookup: {lookup_x_var_name}, {lookup_y_var_name}")
      }
    }

    # Create the plot
    p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
      geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
      geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
                 linetype = split_line_type, linewidth = split_line_linewidth) +
      geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
                 linetype = split_line_type, linewidth = split_line_linewidth) +
      color_scale +
      labs(
        x = x_lab,
        y = y_lab,
        subtitle = subtitle
      ) +
      theme_function() +
      theme(
        plot.title = element_text(size = title_size, margin = margin(b = 20)), # Add margin below title
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_text(size = axis_title_size),
        legend.title = element_text(size = legend_title_size),
        axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
        axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
      )

    # Add secondary axes with split values and original split values if original_df is provided
    p <- p +
      scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split, lookup_x_var_name)) +
      scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split, lookup_y_var_name))

    return(p)
  }

  # create the individual plots based on plot_selection
  cli::cli_h2("Creating individual plots")
  p1 <- p2 <- NULL

  tryCatch({
    if (plot_selection %in% c("both", "p1")) {
      p1 <- base_plot("x1", "x2", transformed_columns[x1_col], transformed_columns[x2_col],
                      paste(transformed_columns[x1_col], "vs", transformed_columns[x2_col]),
                      x1_split, x2_split, actual_columns[x1_col], actual_columns[x2_col])
      cli::cli_alert_success("Plot 1 created")
    }

    if (plot_selection %in% c("both", "p2")) {
      p2 <- base_plot("x1", "x3", transformed_columns[x1_col], transformed_columns[x3_col],
                      paste(transformed_columns[x1_col], "vs", transformed_columns[x3_col]),
                      x1_split, x3_split, actual_columns[x1_col], actual_columns[x3_col])
      cli::cli_alert_success("Plot 2 created")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Error creating plots: {e$message}")
    print(str(plot_df))  # Debug information
    print(str(transformed_columns))  # Debug information
  })

  # combine the plots based on plot_selection
  cli::cli_h2("Combining plots")
  combined_plot <- NULL
  tryCatch({
    if (plot_selection == "both") {
      if (is.null(p1) || is.null(p2)) {
        cli::cli_abort("Both plots should be created when plot_selection is 'both'")
      }
      combined_plot <- p1 + p2 +
        plot_layout(guides = "collect") +
        plot_annotation(
          title = if (!is.null(title)) paste("Policy Tree Results for", title) else NULL,
          caption = if (!is.null(original_df)) "* Original scale value" else NULL,
          tag_levels = 'A'
        ) &
        theme(
          plot.tag = element_text(size = 12, face = "bold"),
          legend.position = legend_position,
          plot.caption = element_text(hjust = 1, size = 12)  # Increase caption size
        )
      cli::cli_alert_success("Both plots combined")
    } else if (plot_selection == "p1") {
      if (is.null(p1)) {
        cli::cli_abort("Plot 1 should be created when plot_selection is 'p1'")
      }
      combined_plot <- p1 +
        plot_annotation(
          title = if (!is.null(title)) paste("Policy Tree Results for", title, "- Plot 1") else NULL,
          caption = if (!is.null(original_df)) "* Original scale value" else NULL,
          tag_levels = 'A'
        ) &
        theme(
          legend.position = legend_position,
          plot.caption = element_text(hjust = 1, size = 12)  # Increase caption size
        )
      cli::cli_alert_success("Plot 1 finalized")
    } else if (plot_selection == "p2") {
      if (is.null(p2)) {
        cli::cli_abort("Plot 2 should be created when plot_selection is 'p2'")
      }
      combined_plot <- p1 + p2 +
        plot_layout(guides = "collect") +
        plot_annotation(
          title = if (!is.null(title)) paste("Policy Tree Results for", title) else NULL,
          caption = if (!is.null(original_df)) "* Original scale value" else NULL,
          tag_levels = 'A'
        ) &
        theme(
          plot.tag = element_text(size = 12, face = "bold"),
          legend.position = legend_position,
          plot.caption = element_text(hjust = 1, size = 12),
          plot.margin = margin(t = 30, r = 10, b = 10, l = 10) # Add more top margin
        )
      cli::cli_alert_success("Plot 2 finalized")
    } else {
      cli::cli_abort("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Error combining plots: {e$message}")
    print(class(p1))  # Debug information
    print(class(p2))  # Debug information
  })

  if (is.null(combined_plot)) {
    cli::cli_alert_danger("Failed to create combined plot")
    return(NULL)
  }

  cli::cli_alert_success("Margot plot policy tree created successfully 👍")

  # return the combined plot
  return(combined_plot)
}

