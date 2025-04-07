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

#' margot_plot_policy_tree <- function(mc_test, model_name,
#'                                     original_df = NULL,
#'                                     color_scale = NULL,
#'                                     point_alpha = 0.5,
#'                                     theme_function = theme_classic,
#'                                     title_size = 14,
#'                                     subtitle_size = 11,
#'                                     axis_title_size = 10,
#'                                     legend_title_size = 10,
#'                                     jitter_width = 0.3,
#'                                     jitter_height = 0.3,
#'                                     split_line_color = "darkgray",
#'                                     split_line_alpha = 0.7,
#'                                     split_line_type = "dashed",
#'                                     split_line_linewidth = 0.5,
#'                                     split_label_size = 10,
#'                                     split_label_color = "darkgray",
#'                                     custom_action_names = NULL,
#'                                     legend_position = "bottom",
#'                                     plot_selection = "both",
#'                                     remove_tx_prefix = TRUE,
#'                                     remove_z_suffix = TRUE,
#'                                     use_title_case = TRUE,
#'                                     remove_underscores = TRUE,
#'                                     label_mapping = NULL) {
#'
#'   cli::cli_h1("Margot Plot Policy Tree")
#'
#'   # Function to transform variable names for display
#'   transform_var_name <- function(var_name) {
#'     display_name <- var_name
#'
#'     # Remove 'model_' prefix if present (for model names)
#'     if (startsWith(display_name, "model_")) {
#'       display_name <- sub("^model_", "", display_name)
#'     }
#'
#'     # Apply label mapping first, if exists
#'     if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
#'       mapped_label <- label_mapping[[display_name]]
#'       cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
#'       return(mapped_label)
#'     }
#'
#'     # Else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
#'     if (startsWith(display_name, "t0_")) {
#'       t2_var <- sub("^t0_", "t2_", display_name)
#'       if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
#'         mapped_label <- label_mapping[[t2_var]]
#'         cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
#'         return(mapped_label)
#'       }
#'     }
#'
#'     # Else, apply transformations
#'     if (remove_tx_prefix) display_name <- sub("^t[0-9]+_", "", display_name)
#'     if (remove_z_suffix) display_name <- sub("_z$", "", display_name)
#'     if (remove_underscores) display_name <- gsub("_", " ", display_name)
#'
#'     if (use_title_case) {
#'       display_name <- tools::toTitleCase(display_name)
#'       # Replace "Nz" with "NZ"
#'       display_name <- gsub("Nz", "NZ", display_name)
#'     }
#'
#'     # Notify if transformed
#'     if (display_name != var_name) {
#'       cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
#'     }
#'
#'     return(display_name)
#'   }
#'
#'   # Ensure title is a valid character string or NULL
#'   title <- transform_var_name(model_name)
#'   if (!is.character(title) || is.null(title)) {
#'     title <- as.character(title)  # Force title to be a string if NULL
#'   }
#'
#'   cli::cli_alert_info("Using title: {title}")
#'
#'   # Helper function to get original scale split value
#'   get_original_split_value <- function(var_name, split_value, original_df) {
#'     if (is.null(original_df)) return(NULL)
#'
#'     orig_var <- var_name
#'     # If variable was z-transformed, assume original_df has the original variable without '_z'
#'     if (grepl("_z$", orig_var)) {
#'       orig_var <- sub("_z$", "", orig_var)
#'     }
#'
#'     if (!(orig_var %in% names(original_df))) {
#'       cli::cli_warn("Original variable '{orig_var}' not found in original_df. Skipping original scale value.")
#'       return(NULL)
#'     }
#'
#'     orig_data <- original_df[[orig_var]]
#'
#'     # Calculate mean and sd from original_df
#'     orig_mean <- mean(orig_data, na.rm = TRUE)
#'     orig_sd <- sd(orig_data, na.rm = TRUE)
#'
#'     # Back-transform z-score to original scale
#'     original_value <- orig_mean + split_value * orig_sd
#'
#'     return(round(original_value, 4))
#'   }
#'
#'   # Extract policy tree object
#'   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#'   if (is.null(policy_tree_obj)) {
#'     cli::cli_abort("Policy tree object not found for the specified model name.")
#'   }
#'
#'   cli::cli_alert_success("Policy tree object extracted for model: {model_name}")
#'
#'   # Extract action names and number of actions from the policy tree object
#'   action_names <- policy_tree_obj$action.names
#'   n_actions <- policy_tree_obj$n.actions
#'
#'   # If custom action names are provided, use them instead
#'   if (!is.null(custom_action_names)) {
#'     if (length(custom_action_names) != n_actions) {
#'       cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
#'     }
#'     action_names <- custom_action_names
#'     cli::cli_alert_info("Using custom action names")
#'   }
#'
#'   # Transform action names using transform_var_name
#'   action_names <- sapply(action_names, function(x) transform_var_name(x))
#'   cli::cli_alert_success("Action names transformed")
#'
#'   # Define the Okabe-Ito palette, starting with blue and orange
#'   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73",
#'                          "#F0E442", "#0072B2", "#D55E00",
#'                          "#CC79A7", "#000000")
#'
#'   # If color_scale is not provided, create it based on the number of actions
#'   if (is.null(color_scale)) {
#'     # Create a named vector of colors
#'     color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)
#'     color_scale <- scale_colour_manual(values = color_vector)
#'     cli::cli_alert_info("Created color scale using Okabe-Ito palette")
#'   }
#'
#'   # Extract the plot data for the specified model
#'   plot_data <- mc_test$results[[model_name]]$plot_data
#'   cli::cli_alert_success("Plot data extracted")
#'
#'   # Extract X_test, predictions, and split_variables
#'   X_test <- plot_data$X_test
#'   predictions <- plot_data$predictions
#'
#'   # Extract split variables and their values from the policy tree
#'   policy_tree <- policy_tree_obj$nodes
#'   x1_col <- policy_tree[[1]]$split_variable
#'   x2_col <- policy_tree[[2]]$split_variable
#'   x3_col <- policy_tree[[3]]$split_variable
#'
#'   # Extract split values
#'   x1_split <- policy_tree[[1]]$split_value
#'   x2_split <- policy_tree[[2]]$split_value
#'   x3_split <- policy_tree[[3]]$split_value
#'
#'   # Use the actual column names from X_test and transform them using transform_var_name
#'   actual_columns <- names(X_test)
#'   transformed_columns <- sapply(actual_columns, function(x) transform_var_name(x))
#'   cli::cli_alert_success("Column names transformed")
#'
#'   # Create a data frame for plotting
#'   plot_df <- data.frame(
#'     x1 = X_test[[actual_columns[x1_col]]],
#'     x2 = X_test[[actual_columns[x2_col]]],
#'     x3 = X_test[[actual_columns[x3_col]]],
#'     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#'   )
#'   cli::cli_alert_success("Plot data frame created")
#'
#'   # Function to create a secondary axis with split value and original split value
#'   create_secondary_axis <- function(primary_range, split_value, var_name) {
#'     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#'     if (!is.null(original_df)) {
#'       original_split <- get_original_split_value(var_name, split_value, original_df)
#'       if (!is.null(original_split)) {
#'         # Format: Split: standardized_value (original_value)*
#'         secondary_labels <- c("", sprintf("Split: %.2f (%.2f)*", split_value, original_split), "")
#'       } else {
#'         secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#'       }
#'     } else {
#'       secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#'     }
#'     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#'   }
#'
#'   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split, x_var_name, y_var_name) {
#'     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#'       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#'       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#'                  linetype = split_line_type, linewidth = split_line_linewidth) +
#'       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#'                  linetype = split_line_type, linewidth = split_line_linewidth) +
#'       color_scale +
#'       labs(
#'         x = x_lab,
#'         y = y_lab,
#'         subtitle = subtitle
#'       ) +
#'       theme_function() +
#'       theme(
#'         plot.title = element_text(size = title_size),
#'         plot.subtitle = element_text(size = subtitle_size),
#'         axis.title = element_text(size = axis_title_size),
#'         legend.title = element_text(size = legend_title_size),
#'         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#'         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#'       )
#'
#'     # Add secondary axes with split values and original split values if original_df is provided
#'     p <- p +
#'       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split, x_var_name)) +
#'       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split, y_var_name))
#'
#'     return(p)
#'   }
#'
#'   # Create the individual plots based on plot_selection
#'   cli::cli_h2("Creating individual plots")
#'   p1 <- p2 <- NULL
#'
#'   tryCatch({
#'     if (plot_selection %in% c("both", "p1")) {
#'       p1 <- base_plot("x1", "x2", transformed_columns[x1_col], transformed_columns[x2_col],
#'                       paste(transformed_columns[x1_col], "vs", transformed_columns[x2_col]),
#'                       x1_split, x2_split, actual_columns[x1_col], actual_columns[x2_col])
#'       cli::cli_alert_success("Plot 1 created")
#'     }
#'
#'     if (plot_selection %in% c("both", "p2")) {
#'       p2 <- base_plot("x1", "x3", transformed_columns[x1_col], transformed_columns[x3_col],
#'                       paste(transformed_columns[x1_col], "vs", transformed_columns[x3_col]),
#'                       x1_split, x3_split, actual_columns[x1_col], actual_columns[x3_col])
#'       cli::cli_alert_success("Plot 2 created")
#'     }
#'   }, error = function(e) {
#'     cli::cli_alert_danger("Error creating plots: {e$message}")
#'     print(str(plot_df))  # Debug information
#'     print(str(transformed_columns))  # Debug information
#'   })
#'
#'   # Combine the plots based on plot_selection
#'   cli::cli_h2("Combining plots")
#'   combined_plot <- NULL
#'   tryCatch({
#'     if (plot_selection == "both") {
#'       if (is.null(p1) || is.null(p2)) {
#'         cli::cli_abort("Both plots should be created when plot_selection is 'both'")
#'       }
#'       combined_plot <- p1 + p2 +
#'         plot_layout(guides = "collect") +
#'         plot_annotation(
#'           title = if (!is.null(title)) paste("Policy Tree Results for", title) else NULL,
#'           caption = if (!is.null(original_df)) "* Original scale value" else NULL,
#'           tag_levels = 'A'
#'         ) &
#'         theme(
#'           plot.tag = element_text(size = 12, face = "bold"),
#'           legend.position = legend_position,
#'           plot.caption = element_text(hjust = 1, size = 12)  # Increase caption size
#'         )
#'       cli::cli_alert_success("Both plots combined")
#'     } else if (plot_selection == "p1") {
#'       if (is.null(p1)) {
#'         cli::cli_abort("Plot 1 should be created when plot_selection is 'p1'")
#'       }
#'       combined_plot <- p1 +
#'         plot_annotation(
#'           title = if (!is.null(title)) paste("Policy Tree Results for", title, "- Plot 1") else NULL,
#'           caption = if (!is.null(original_df)) "* Original scale value" else NULL,
#'           tag_levels = 'A'
#'         ) &
#'         theme(
#'           legend.position = legend_position,
#'           plot.caption = element_text(hjust = 1, size = 12)  # Increase caption size
#'         )
#'       cli::cli_alert_success("Plot 1 finalized")
#'     } else if (plot_selection == "p2") {
#'       if (is.null(p2)) {
#'         cli::cli_abort("Plot 2 should be created when plot_selection is 'p2'")
#'       }
#'       combined_plot <- p2 +
#'         plot_annotation(
#'           title = if (!is.null(title)) paste("Policy Tree Results for", title, "- Plot 2") else NULL,
#'           caption = if (!is.null(original_df)) "* Original scale value" else NULL,
#'           tag_levels = 'A'
#'         ) &
#'         theme(
#'           legend.position = legend_position,
#'           plot.caption = element_text(hjust = 1, size = 12)  # Increase caption size
#'         )
#'       cli::cli_alert_success("Plot 2 finalized")
#'     } else {
#'       cli::cli_abort("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#'     }
#'   }, error = function(e) {
#'     cli::cli_alert_danger("Error combining plots: {e$message}")
#'     print(class(p1))  # Debug information
#'     print(class(p2))  # Debug information
#'   })
#'
#'   if (is.null(combined_plot)) {
#'     cli::cli_alert_danger("Failed to create combined plot")
#'     return(NULL)
#'   }
#'
#'   cli::cli_alert_success("Margot plot policy tree created successfully 👍")
#'
#'   # Return the combined plot
#'   return(combined_plot)
#' }
#'
#'
#' #' @keywords internal
#' transform_var_name <- function(var_name, label_mapping, remove_tx_prefix, remove_z_suffix,
#'                                use_title_case, remove_underscores) {
#'   display_name <- var_name
#'
#'   # Remove 'model_' prefix if present (for model names)
#'   if (startsWith(display_name, "model_")) {
#'     display_name <- sub("^model_", "", display_name)
#'   }
#'
#'   # Apply label mapping first, if exists
#'   if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
#'     mapped_label <- label_mapping[[display_name]]
#'     cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
#'     return(mapped_label)
#'   }
#'
#'   # Else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
#'   if (startsWith(display_name, "t0_")) {
#'     t2_var <- sub("^t0_", "t2_", display_name)
#'     if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
#'       mapped_label <- label_mapping[[t2_var]]
#'       cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
#'       return(mapped_label)
#'     }
#'   }
#'
#'   # Else, apply transformations
#'   if (remove_tx_prefix) {
#'     display_name <- sub("^t[0-9]+_", "", display_name)
#'   }
#'   if (remove_z_suffix) {
#'     display_name <- sub("_z$", "", display_name)
#'   }
#'   if (remove_underscores) {
#'     display_name <- gsub("_", " ", display_name)
#'   }
#'
#'   if (use_title_case) {
#'     display_name <- tools::toTitleCase(display_name)
#'     # Replace "Nz" with "NZ"
#'     display_name <- gsub("Nz", "NZ", display_name)
#'   }
#'
#'   # Notify if transformed
#'   if (display_name != var_name) {
#'     cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
#'   }
#'
#'   return(display_name)
#' }
#'
#' # Helper function to get original scale split value
#' #' @keywords internal
#' get_original_split_value <- function(var_name, split_value, original_df) {
#'   if (is.null(original_df)) return(NULL)
#'
#'   orig_var <- var_name
#'   # If variable was z-transformed, assume original_df has the original variable without '_z'
#'   if (grepl("_z$", orig_var)) {
#'     orig_var <- sub("_z$", "", orig_var)
#'   }
#'
#'   if (!(orig_var %in% names(original_df))) {
#'     cli::cli_warn("Original variable '{orig_var}' not found in original_df. Skipping original scale value.")
#'     return(NULL)
#'   }
#'
#'   orig_data <- original_df[[orig_var]]
#'
#'   # Calculate mean and sd from original_df
#'   orig_mean <- mean(orig_data, na.rm = TRUE)
#'   orig_sd <- sd(orig_data, na.rm = TRUE)
#'
#'   # Back-transform z-score to original scale
#'   original_value <- orig_mean + split_value * orig_sd
#'
#'   return(round(original_value, 3))
#' }
# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = NULL,
#                                     point_alpha = 0.5,
#                                     theme_function = theme_classic,
#                                     title_size = 14,
#                                     subtitle_size = 11,
#                                     axis_title_size = 10,
#                                     legend_title_size = 10,
#                                     jitter_width = 0.3,
#                                     jitter_height = 0.3,
#                                     split_line_color = "darkgray",
#                                     split_line_alpha = 0.7,
#                                     split_line_type = "dashed",
#                                     split_line_linewidth = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     custom_action_names = NULL,
#                                     legend_position = "bottom",
#                                     plot_selection = "both",
#                                     remove_tx_prefix = TRUE,
#                                     remove_z_suffix = TRUE,
#                                     use_title_case = TRUE,
#                                     remove_underscores = TRUE,
#                                     label_mapping = NULL) {
#
#   cli::cli_h1("Margot Plot Policy Tree")
#
#   # Function to transform labels
#   transform_label <- function(label, use_mapping = TRUE) {
#     original_label <- label
#
#     # Apply mapping if available and requested
#     if (use_mapping && !is.null(label_mapping)) {
#       # Check for full model name in mapping
#       if (label %in% names(label_mapping)) {
#         mapped_label <- label_mapping[[label]]
#         cli::cli_alert_info("Applied label mapping: {original_label} -> {mapped_label}")
#         return(mapped_label)
#       }
#       # Check for model name without "model_" prefix in mapping
#       label_without_prefix <- sub("^model_", "", label)
#       if (label_without_prefix %in% names(label_mapping)) {
#         mapped_label <- label_mapping[[label_without_prefix]]
#         cli::cli_alert_info("Applied label mapping (without 'model_' prefix): {original_label} -> {mapped_label}")
#         return(mapped_label)
#       }
#     }
#
#     # Apply other transformations
#     if (remove_tx_prefix) {
#       label <- sub("^t[0-9]+_", "", label)
#     }
#     if (remove_z_suffix) {
#       label <- sub("_z$", "", label)
#     }
#     if (remove_underscores) {
#       label <- gsub("_", " ", label)
#     }
#     if (use_title_case) {
#       label <- tools::toTitleCase(label)
#       # correct NZ
#       label <- gsub("Nz", "NZ", label)
#     }
#     if (label != original_label) {
#       cli::cli_alert_info("Transformed label: {original_label} -> {label}")
#     }
#     return(label)
#   }
#
#   # Create title
#   title <- transform_label(model_name, use_mapping = TRUE)
#   cli::cli_alert_info("Using title: {title}")
#
#   # Ensure title is a character string or NULL
#   if (!is.character(title) && !is.null(title)) {
#     cli::cli_warn("Invalid title type. Setting title to NULL.")
#     title <- NULL
#   }
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     cli::cli_abort("Policy tree object not found for the specified model name.")
#   }
#
#   cli::cli_alert_success("Policy tree object extracted for model: {model_name}")
#
#   # Extract action names and number of actions from the policy tree object
#   action_names <- policy_tree_obj$action.names
#   n_actions <- policy_tree_obj$n.actions
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != n_actions) {
#       cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#     cli::cli_alert_info("Using custom action names")
#   }
#
#   # Transform action names
#   action_names <- sapply(action_names, transform_label, use_mapping = FALSE)
#   cli::cli_alert_success("Action names transformed")
#
#   # Define the Okabe-Ito palette, starting with blue and orange
#   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#
#   # If color_scale is not provided, create it based on the number of actions
#   if (is.null(color_scale)) {
#     # Create a named vector of colors
#     color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)
#     color_scale <- scale_colour_manual(values = color_vector)
#     cli::cli_alert_info("Created color scale using Okabe-Ito palette")
#   }
#
#   # Extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#   cli::cli_alert_success("Plot data extracted")
#
#   # Extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # Extract split variables and their values from the policy tree
#   policy_tree <- policy_tree_obj$nodes
#   x1_col <- policy_tree[[1]]$split_variable
#   x2_col <- policy_tree[[2]]$split_variable
#   x3_col <- policy_tree[[3]]$split_variable
#
#   # Extract split values
#   x1_split <- policy_tree[[1]]$split_value
#   x2_split <- policy_tree[[2]]$split_value
#   x3_split <- policy_tree[[3]]$split_value
#
#   # Use the actual column names from X_test and transform them
#   actual_columns <- names(X_test)
#   transformed_columns <- sapply(actual_columns, transform_label)
#   cli::cli_alert_success("Column names transformed")
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#   cli::cli_alert_success("Plot data frame created")
#
#   # Function to create a secondary axis with split value
#   create_secondary_axis <- function(primary_range, split_value) {
#     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#     secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#   }
#
#   # Create the base plot
#   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
#     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       color_scale +
#       labs(
#         x = x_lab,
#         y = y_lab,
#         subtitle = subtitle
#       ) +
#       theme_function() +
#       theme(
#         plot.title = element_text(size = title_size),
#         plot.subtitle = element_text(size = subtitle_size),
#         axis.title = element_text(size = axis_title_size),
#         legend.title = element_text(size = legend_title_size),
#         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#       )
#
#     # Add secondary axes with split values
#     p <- p +
#       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
#       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))
#
#     return(p)
#   }
#
#   # Create the individual plots based on plot_selection
#   cli::cli_h2("Creating individual plots")
#   p1 <- p2 <- NULL
#
#   tryCatch({
#     if (plot_selection %in% c("both", "p1")) {
#       p1 <- base_plot("x1", "x2", transformed_columns[x1_col], transformed_columns[x2_col],
#                       paste(transformed_columns[x1_col], "vs", transformed_columns[x2_col]),
#                       x1_split, x2_split)
#       cli::cli_alert_success("Plot 1 created")
#       print(class(p1))  # Debug information
#     }
#
#     if (plot_selection %in% c("both", "p2")) {
#       p2 <- base_plot("x1", "x3", transformed_columns[x1_col], transformed_columns[x3_col],
#                       paste(transformed_columns[x1_col], "vs", transformed_columns[x3_col]),
#                       x1_split, x3_split)
#       cli::cli_alert_success("Plot 2 created")
#       print(class(p2))  # Debug information
#     }
#   }, error = function(e) {
#     cli::cli_alert_danger("Error creating plots: {e$message}")
#     print(str(plot_df))  # Debug information
#     print(str(transformed_columns))  # Debug information
#   })
#
#   # Combine the plots based on plot_selection
#   cli::cli_h2("Combining plots")
#   combined_plot <- NULL
#   tryCatch({
#     if (plot_selection == "both") {
#       if (is.null(p1) || is.null(p2)) {
#         cli::cli_abort("Both plots should be created when plot_selection is 'both'")
#       }
#       combined_plot <- p1 + p2 +
#         patchwork::plot_layout(guides = "collect") &
#         patchwork::plot_annotation(
#           title = if (!is.null(title)) paste("Policy Tree Results for", title) else NULL,
#           tag_levels = 'A'
#         ) &
#         theme(
#           plot.tag = element_text(size = 12, face = "bold"),
#           legend.position = legend_position
#         )
#       cli::cli_alert_success("Both plots combined")
#     } else if (plot_selection == "p1") {
#       if (is.null(p1)) {
#         cli::cli_abort("Plot 1 should be created when plot_selection is 'p1'")
#       }
#       combined_plot <- p1 +
#         patchwork::plot_annotation(
#           title = if (!is.null(title)) paste("Policy Tree Results for", title, "- Plot 1") else NULL
#         ) &
#         theme(legend.position = legend_position)
#       cli::cli_alert_success("Plot 1 finalized")
#     } else if (plot_selection == "p2") {
#       if (is.null(p2)) {
#         cli::cli_abort("Plot 2 should be created when plot_selection is 'p2'")
#       }
#       combined_plot <- p2 +
#         patchwork::plot_annotation(
#           title = if (!is.null(title)) paste("Policy Tree Results for", title, "- Plot 2") else NULL
#         ) &
#         theme(legend.position = legend_position)
#       cli::cli_alert_success("Plot 2 finalized")
#     } else {
#       cli::cli_abort("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#     }
#   }, error = function(e) {
#     cli::cli_alert_danger("Error combining plots: {e$message}")
#     print(class(p1))  # Debug information
#     print(class(p2))  # Debug information
#   })
#
#   if (is.null(combined_plot)) {
#     cli::cli_alert_danger("Failed to create combined plot")
#     return(NULL)
#   }
#
#   cli::cli_alert_success("Margot plot policy tree created successfully \U0001F44D")
#
#   # Return the combined plot
#   return(combined_plot)
# }
# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = NULL,
#                                     point_alpha = 0.5,
#                                     theme_function = theme_classic,
#                                     title_size = 14,
#                                     subtitle_size = 11,
#                                     axis_title_size = 10,
#                                     legend_title_size = 10,
#                                     jitter_width = 0.3,
#                                     jitter_height = 0.3,
#                                     split_line_color = "darkgray",
#                                     split_line_alpha = 0.7,
#                                     split_line_type = "dashed",
#                                     split_line_linewidth = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     custom_action_names = NULL,
#                                     legend_position = "bottom",
#                                     plot_selection = "both",
#                                     remove_tx_prefix = TRUE,
#                                     remove_z_suffix = TRUE,
#                                     use_title_case = TRUE,
#                                     remove_underscores = TRUE) {
#
#   cli::cli_h1("Margot Plot Policy Tree")
#
#   # Function to transform labels
#   transform_label <- function(label) {
#     original_label <- label
#     if (remove_tx_prefix) {
#       label <- sub("^t[0-9]+_", "", label)
#     }
#     if (remove_z_suffix) {
#       label <- sub("_z$", "", label)
#     }
#     if (remove_underscores) {
#       label <- gsub("_", " ", label)
#     }
#     if (use_title_case) {
#       label <- tools::toTitleCase(label)
#       # correct NZ
#       label <- gsub("Nz", "NZ", label)
#     }
#     if (label != original_label) {
#       cli::cli_alert_info("Transformed label: {original_label} -> {label}")
#     }
#     return(label)
#   }
#
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     cli::cli_abort("Policy tree object not found for the specified model name.")
#   }
#
#   cli::cli_alert_success("Policy tree object extracted for model: {model_name}")
#
#   # Extract action names and number of actions from the policy tree object
#   action_names <- policy_tree_obj$action.names
#   n_actions <- policy_tree_obj$n.actions
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != n_actions) {
#       cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#     cli::cli_alert_info("Using custom action names")
#   }
#
#   # Transform action names
#   action_names <- sapply(action_names, transform_label)
#   cli::cli_alert_success("Action names transformed")
#
#   # Define the Okabe-Ito palette, starting with blue and orange
#   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#
#   # If color_scale is not provided, create it based on the number of actions
#   if (is.null(color_scale)) {
#     # Create a named vector of colors
#     color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)
#     color_scale <- scale_colour_manual(values = color_vector)
#     cli::cli_alert_info("Created color scale using Okabe-Ito palette")
#   }
#
#   # Extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#   cli::cli_alert_success("Plot data extracted")
#
#   # Extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # Extract split variables and their values from the policy tree
#   policy_tree <- policy_tree_obj$nodes
#   x1_col <- policy_tree[[1]]$split_variable
#   x2_col <- policy_tree[[2]]$split_variable
#   x3_col <- policy_tree[[3]]$split_variable
#
#   # Extract split values
#   x1_split <- policy_tree[[1]]$split_value
#   x2_split <- policy_tree[[2]]$split_value
#   x3_split <- policy_tree[[3]]$split_value
#
#   # Use the actual column names from X_test and transform them
#   actual_columns <- names(X_test)
#   transformed_columns <- sapply(actual_columns, transform_label)
#   cli::cli_alert_success("Column names transformed")
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#   cli::cli_alert_success("Plot data frame created")
#
#   # Function to create a secondary axis with split value
#   create_secondary_axis <- function(primary_range, split_value) {
#     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#     secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#   }
#
#   # Create the base plot
#   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
#     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       color_scale +
#       labs(
#         x = x_lab,
#         y = y_lab,
#         subtitle = subtitle
#       ) +
#       theme_function() +
#       theme(
#         plot.title = element_text(size = title_size),
#         plot.subtitle = element_text(size = subtitle_size),
#         axis.title = element_text(size = axis_title_size),
#         legend.title = element_text(size = legend_title_size),
#         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#       )
#
#     # Add secondary axes with split values
#     p <- p +
#       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
#       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))
#
#     return(p)
#   }
#
#   # Create the individual plots based on plot_selection
#   cli::cli_h2("Creating individual plots")
#   p1 <- p2 <- NULL
#
#   if (plot_selection %in% c("both", "p1")) {
#     p1 <- base_plot("x1", "x2", transformed_columns[x1_col], transformed_columns[x2_col],
#                     paste(transformed_columns[x1_col], "vs", transformed_columns[x2_col]),
#                     x1_split, x2_split)
#     cli::cli_alert_success("Plot 1 created")
#   }
#
#   if (plot_selection %in% c("both", "p2")) {
#     p2 <- base_plot("x1", "x3", transformed_columns[x1_col], transformed_columns[x3_col],
#                     paste(transformed_columns[x1_col], "vs", transformed_columns[x3_col]),
#                     x1_split, x3_split)
#     cli::cli_alert_success("Plot 2 created")
#   }
#
#   # Combine the plots based on plot_selection
#   cli::cli_h2("Combining plots")
#   if (plot_selection == "both") {
#     if (is.null(p1) || is.null(p2)) {
#       cli::cli_abort("Both plots should be created when plot_selection is 'both'")
#     }
#     combined_plot <- p1 + p2 +
#       patchwork::plot_layout(guides = "collect") &
#       patchwork::plot_annotation(
#         title = paste("Policy Tree Results for", transform_label(model_name)),
#         tag_levels = 'A'
#       ) &
#       theme(
#         plot.tag = element_text(size = 12, face = "bold"),
#         legend.position = legend_position
#       )
#     cli::cli_alert_success("Both plots combined")
#   } else if (plot_selection == "p1") {
#     if (is.null(p1)) {
#       cli::cli_abort("Plot 1 should be created when plot_selection is 'p1'")
#     }
#     combined_plot <- p1 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", transform_label(model_name), "- Plot 1")) &
#       theme(legend.position = legend_position)
#     cli::cli_alert_success("Plot 1 finalized")
#   } else if (plot_selection == "p2") {
#     if (is.null(p2)) {
#       cli::cli_abort("Plot 2 should be created when plot_selection is 'p2'")
#     }
#     combined_plot <- p2 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", transform_label(model_name), "- Plot 2")) &
#       theme(legend.position = legend_position)
#     cli::cli_alert_success("Plot 2 finalized")
#   } else {
#     cli::cli_abort("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#   }
#
#   cli::cli_alert_success("Margot plot policy tree created successfully \U0001F44D")
#
#   # Return the combined plot
#   return(combined_plot)
# }
# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = NULL,  # We'll set this inside the function
#                                     point_alpha = 0.5,
#                                     theme_function = theme_classic,
#                                     title_size = 14,
#                                     subtitle_size = 11,
#                                     axis_title_size = 10,
#                                     legend_title_size = 10,
#                                     jitter_width = 0.3,
#                                     jitter_height = 0.3,
#                                     split_line_color = "darkgray",
#                                     split_line_alpha = 0.7,
#                                     split_line_type = "dashed",
#                                     split_line_linewidth = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     custom_action_names = NULL,
#                                     legend_position = "bottom",
#                                     plot_selection = "both") {
#
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     stop("Policy tree object not found for the specified model name.")
#   }
#
#   # Extract action names from the policy tree object
#   action_names <- policy_tree_obj$action.names
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != length(action_names)) {
#       stop("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#   }
#
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     stop("Policy tree object not found for the specified model name.")
#   }
#
#   # Extract action names and number of actions from the policy tree object
#   action_names <- policy_tree_obj$action.names
#   n_actions <- policy_tree_obj$n.actions
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != n_actions) {
#       stop("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#   }
#
#   # Define the Okabe-Ito palette, starting with blue and orange
#   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#
#   # If color_scale is not provided, create it based on the number of actions
#   if (is.null(color_scale)) {
#     # Create a named vector of colors
#     color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)
#
#     color_scale <- scale_colour_manual(values = color_vector)
#   }
#
#
#   # Extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#
#   # Extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # Extract split variables and their values from the policy tree
#   policy_tree <- policy_tree_obj$nodes
#   x1_col <- policy_tree[[1]]$split_variable
#   x2_col <- policy_tree[[2]]$split_variable
#   x3_col <- policy_tree[[3]]$split_variable
#
#   # Extract split values
#   x1_split <- policy_tree[[1]]$split_value
#   x2_split <- policy_tree[[2]]$split_value
#   x3_split <- policy_tree[[3]]$split_value
#
#   # Use the actual column names from X_test
#   actual_columns <- names(X_test)
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#
#   # Function to create a secondary axis with split value
#   create_secondary_axis <- function(primary_range, split_value) {
#     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#     secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#   }
#
#   # Create the base plot
#   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
#     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       color_scale +
#       labs(
#         x = x_lab,
#         y = y_lab,
#         subtitle = subtitle
#       ) +
#       theme_function() +
#       theme(
#         plot.title = element_text(size = title_size),
#         plot.subtitle = element_text(size = subtitle_size),
#         axis.title = element_text(size = axis_title_size),
#         legend.title = element_text(size = legend_title_size),
#         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#       )
#
#     # Add secondary axes with split values
#     p <- p +
#       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
#       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))
#
#     return(p)
#   }
#
#   # Create the individual plots based on plot_selection
#   p1 <- if (plot_selection %in% c("both", "p1")) {
#     base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
#               x1_split, x2_split)
#   } else {
#     NULL
#   }
#
#   p2 <- if (plot_selection %in% c("both", "p2")) {
#     base_plot("x1", "x3", actual_columns[x1_col], actual_columns[x3_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x3_col]),
#               x1_split, x3_split)
#   } else {
#     NULL
#   }
#
#   # Combine the plots based on plot_selection
#   if (plot_selection == "both") {
#     combined_plot <- p1 + p2 +
#       patchwork::plot_layout(guides = "collect") &
#       patchwork::plot_annotation(
#         title = paste("Policy Tree Results for", model_name),
#         tag_levels = 'A'
#       ) &
#       theme(
#         plot.tag = element_text(size = 12, face = "bold"),
#         legend.position = legend_position
#       )
#   } else if (plot_selection == "p1") {
#     combined_plot <- p1 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 1")) &
#       theme(legend.position = legend_position)
#   } else if (plot_selection == "p2") {
#     combined_plot <- p2 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 2")) &
#       theme(legend.position = legend_position)
#   } else {
#     stop("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#   }
#
#   # Return the combined plot
#   return(combined_plot)
# }
# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = NULL,  # We'll set this inside the function
#                                     point_alpha = 0.5,
#                                     theme_function = theme_classic,
#                                     title_size = 14,
#                                     subtitle_size = 12,
#                                     axis_title_size = 10,
#                                     legend_title_size = 10,
#                                     jitter_width = 0.3,
#                                     jitter_height = 0.3,
#                                     split_line_color = "darkgray",
#                                     split_line_alpha = 0.7,
#                                     split_line_type = "dashed",
#                                     split_line_linewidth = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     custom_action_names = NULL,
#                                     legend_position = "bottom",
#                                     plot_selection = "both") {
#
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     stop("Policy tree object not found for the specified model name.")
#   }
#
#   # Extract action names from the policy tree object
#   action_names <- policy_tree_obj$action.names
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != length(action_names)) {
#       stop("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#   }
#
#   # Define the Okabe-Ito palette, starting with blue and orange
#   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#
#
#   # If color_scale is not provided, create it based on the number of action_names
#   if (is.null(color_scale)) {
#     num_colors_needed <- length(action_names)
#     colors_to_use <- okabe_ito_palette[1:min(num_colors_needed, length(okabe_ito_palette))]
#
#     # If we need more colors than in the palette, recycle them
#     if (num_colors_needed > length(okabe_ito_palette)) {
#       colors_to_use <- rep_len(colors_to_use, num_colors_needed)
#     }
#
#     color_scale <- scale_colour_manual(values = colors_to_use)
#   }
#
#
#   # Extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#
#   # Extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # Extract split variables and their values from the policy tree
#   policy_tree <- policy_tree_obj$nodes
#   x1_col <- policy_tree[[1]]$split_variable
#   x2_col <- policy_tree[[2]]$split_variable
#   x3_col <- policy_tree[[3]]$split_variable
#
#   # Extract split values
#   x1_split <- policy_tree[[1]]$split_value
#   x2_split <- policy_tree[[2]]$split_value
#   x3_split <- policy_tree[[3]]$split_value
#
#   # Use the actual column names from X_test
#   actual_columns <- names(X_test)
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#
#   # Function to create a secondary axis with split value
#   create_secondary_axis <- function(primary_range, split_value) {
#     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#     secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#   }
#
#   # Create the base plot
#   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
#     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       color_scale +
#       labs(
#         x = x_lab,
#         y = y_lab,
#         subtitle = subtitle
#       ) +
#       theme_function() +
#       theme(
#         plot.title = element_text(size = title_size),
#         plot.subtitle = element_text(size = subtitle_size),
#         axis.title = element_text(size = axis_title_size),
#         legend.title = element_text(size = legend_title_size),
#         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#       )
#
#     # Add secondary axes with split values
#     p <- p +
#       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
#       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))
#
#     return(p)
#   }
#
#   # Create the individual plots based on plot_selection
#   p1 <- if (plot_selection %in% c("both", "p1")) {
#     base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
#               x1_split, x2_split)
#   } else {
#     NULL
#   }
#
#   p2 <- if (plot_selection %in% c("both", "p2")) {
#     base_plot("x1", "x3", actual_columns[x1_col], actual_columns[x3_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x3_col]),
#               x1_split, x3_split)
#   } else {
#     NULL
#   }
#
#   # Combine the plots based on plot_selection
#   if (plot_selection == "both") {
#     combined_plot <- p1 + p2 +
#       patchwork::plot_layout(guides = "collect") &
#       patchwork::plot_annotation(
#         title = paste("Policy Tree Results for", model_name),
#         tag_levels = 'A'
#       ) &
#       theme(
#         plot.tag = element_text(size = 12, face = "bold"),
#         legend.position = legend_position
#       )
#   } else if (plot_selection == "p1") {
#     combined_plot <- p1 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 1")) &
#       theme(legend.position = legend_position)
#   } else if (plot_selection == "p2") {
#     combined_plot <- p2 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 2")) &
#       theme(legend.position = legend_position)
#   } else {
#     stop("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#   }
#
#   # Return the combined plot
#   return(combined_plot)
# }
