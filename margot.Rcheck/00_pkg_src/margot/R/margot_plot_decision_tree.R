#' Plot a Decision Tree from Margot Causal-Forest Results
#'
#' Creates a visualisation of a policy tree showing the decision rules
#' and assigned treatments from causal forest results.
#'
#' @param result_object A list returned by `margot_causal_forest()`
#' @param model_name Name of the model in the results to visualise
#' @param max_depth Maximum depth of the tree (1L or 2L)
#' @param original_df Optional dataframe with original data for showing untransformed values
#' @param x_padding Horizontal padding for the plot (proportion)
#' @param y_padding Vertical padding for the plot (proportion)
#' @param border_size Size of node borders in lines
#' @param text_size Size of text in plot elements
#' @param edge_label_offset Offset for edge labels from connecting lines
#' @param span_ratio Controls the aspect ratio of the plot
#' @param non_leaf_fill Colour for non-leaf nodes (decision nodes)
#' @param title Optional custom title for the plot
#' @param plot_margin Margins around the plot
#' @param remove_tx_prefix Whether to remove treatment prefixes from variable names
#' @param remove_z_suffix Whether to remove z-suffixes from variable names
#' @param use_title_case Whether to use title case for variable names
#' @param remove_underscores Whether to replace underscores with spaces in variable names
#' @param remove_action_label Whether to remove "Action:" prefix from leaf node labels
#' @param label_mapping Optional list for renaming variables in the display
#'
#' @importFrom dplyr case_when mutate
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot geom_segment geom_label geom_text aes scale_fill_identity coord_fixed scale_x_continuous scale_y_continuous theme_void theme element_text labs margin expansion
#' @importFrom grid unit
#' @importFrom cli cli_h1 cli_abort cli_alert_success cli_alert_info
#'
#' @return A ggplot object with the decision tree visualisation
#' @export
#' @keywords internal
#' Plot a Decision Tree from Margot Causal-Forest Results (robust labelling)
#' @param result_object A list returned by `margot_causal_forest()`
#' @param model_name Name of the model in the results to visualise
#' @param max_depth Maximum depth of the tree (1L or 2L)
#' @param original_df Optional dataframe with original data for showing untransformed values
#' @param x_padding Horizontal padding for the plot (proportion)
#' @param y_padding Vertical padding for the plot (proportion)
#' @param border_size Size of node borders in lines
#' @param text_size Size of text in plot elements
#' @param edge_label_offset Offset for edge labels from connecting lines
#' @param span_ratio Controls the aspect ratio of the plot
#' @param non_leaf_fill Colour for non-leaf nodes (decision nodes)
#' @param title Optional custom title for the plot
#' @param plot_margin Margins around the plot
#' @param remove_tx_prefix Whether to remove treatment prefixes from variable names
#' @param remove_z_suffix Whether to remove z-suffixes from variable names
#' @param use_title_case Whether to use title case for variable names
#' @param remove_underscores Whether to replace underscores with spaces in variable names
#' @param remove_action_label Whether to remove "Action:" prefix from leaf node labels
#' @param label_mapping Optional list for renaming variables in the display
#' @importFrom dplyr case_when mutate
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot geom_segment geom_label geom_text aes scale_fill_identity coord_fixed scale_x_continuous scale_y_continuous theme_void theme element_text labs margin expansion
#' @importFrom grid unit
#' @importFrom cli cli_h1 cli_abort cli_alert_success cli_alert_info
#' @export
margot_plot_decision_tree <- function(
    result_object,
    model_name         = NULL,
    max_depth          = 2L,
    original_df        = NULL,
    x_padding          = 0.12,
    y_padding          = 0.25,
    border_size        = .5,
    text_size          = 4,
    edge_label_offset  = 0.025,
    span_ratio         = 0.4,
    non_leaf_fill      = "lightyellow",
    title              = NULL,
    plot_margin        = grid::unit(c(1, 1, 1, 1), "cm"),
    remove_tx_prefix   = TRUE,
    remove_z_suffix    = TRUE,
    use_title_case     = TRUE,
    remove_underscores = TRUE,
    remove_action_label= TRUE,
    label_mapping      = NULL) {

  cli::cli_h1("Margot Plot Decision Tree")

  # safe wrappers for labelling
  tl <- function(x) tryCatch(
    transform_label(
      x, label_mapping,
      list(
        remove_tx_prefix   = remove_tx_prefix,
        remove_z_suffix    = remove_z_suffix,
        remove_underscores = remove_underscores,
        use_title_case     = use_title_case
      )
    ), error = function(e) x
  )
  tv <- function(x) tryCatch(
    transform_var_name(
      x, label_mapping,
      remove_tx_prefix, remove_z_suffix,
      use_title_case, remove_underscores
    ), error = function(e) x
  )

  # 1 fetch tree
  if (! max_depth %in% c(1L, 2L))
    cli::cli_abort("`max_depth` must be 1 or 2.")
  tag <- paste0("policy_tree_depth_", max_depth)
  if (! (is.list(result_object) && "results" %in% names(result_object)))
    cli::cli_abort("`result_object` must be the full list returned by `margot_causal_forest()`.")
  if (! model_name %in% names(result_object$results))
    cli::cli_abort("model `{model_name}` not found.")
  policy_tree_obj <- result_object$results[[model_name]][[tag]]
  if (is.null(policy_tree_obj))
    cli::cli_abort("`{tag}` object missing for model `{model_name}`.")
  cli::cli_alert_success("✔ Using decision tree at depth {max_depth} for model: {model_name}")

  # extract nodes
  nodes        <- policy_tree_obj$nodes
  columns      <- policy_tree_obj$columns
  action_names <- policy_tree_obj$action.names

  node_data <- tibble::tibble(
    id          = seq_along(nodes),
    is_leaf     = vapply(nodes, `[[`, logical(1), "is_leaf"),
    split_var   = vapply(nodes,
                         function(n) if (is.null(n$split_variable)) NA_character_ else columns[n$split_variable],
                         character(1)),
    split_val   = vapply(nodes,
                         function(n) if (is.null(n$split_value)) NA_real_ else n$split_value,
                         numeric(1)),
    action_id   = vapply(nodes,
                         function(n) if (is.null(n$action)) NA_integer_ else as.integer(n$action),
                         integer(1)),
    left_child  = vapply(nodes,
                         function(n) if (is.null(n$left_child)) NA_integer_ else as.integer(n$left_child),
                         integer(1)),
    right_child = vapply(nodes,
                         function(n) if (is.null(n$right_child)) NA_integer_ else as.integer(n$right_child),
                         integer(1)),
    x = NA_real_, y = NA_real_, label = NA_character_
  ) |>
    # keep literal action names for robustness
    dplyr::mutate(
      action_lbl = dplyr::case_when(
        !is_leaf ~ NA_character_,
        TRUE     ~ tolower(action_names[action_id])
      )
    )

  # 3 create labels
  for (i in seq_len(nrow(node_data))) {
    if (node_data$is_leaf[i]) {
      lbl <- tl(action_names[node_data$action_id[i]])
      if (! remove_action_label) lbl <- paste("Action:", lbl)
      node_data$label[i] <- lbl
    } else {
      var    <- node_data$split_var[i]
      val_s  <- round(node_data$split_val[i], 3)
      orig   <- get_original_value_plot(var, node_data$split_val[i], original_df)
      var_lb <- tv(var)
      node_data$label[i] <- if (!is.null(orig)) {
        sprintf("%s\n<= %s\n(%s)*", var_lb, val_s,
                format(orig, big.mark = ",", scientific = FALSE))
      } else {
        sprintf("%s\n<= %s", var_lb, val_s)
      }
    }
  }

  # 4 layout, edges, colours unchanged
  max_d      <- policy_tree_obj$depth
  assign_pos <- function(id, depth, xpos) {
    node_data$y[id] <<- max_d - depth + 1
    node_data$x[id] <<- xpos
    if (!is.na(node_data$left_child[id]))
      assign_pos(node_data$left_child[id], depth + 1, xpos - 1/(2^depth))
    if (!is.na(node_data$right_child[id]))
      assign_pos(node_data$right_child[id], depth + 1, xpos + 1/(2^depth))
  }
  assign_pos(1, 1, 0.5); cli::cli_alert_success("✔ Node positions calculated")

  edge_data <- purrr::map_dfr(seq_len(nrow(node_data)), function(i) {
    rows <- list()
    if (!is.na(node_data$left_child[i])) rows[[1]] <- tibble::tibble(
      x=node_data$x[i], y=node_data$y[i],
      xend=node_data$x[node_data$left_child[i]],
      yend=node_data$y[node_data$left_child[i]],
      edge_lab="True", hjust=1, vjust=.5)
    if (!is.na(node_data$right_child[i])) rows[[length(rows)+1]] <- tibble::tibble(
      x=node_data$x[i], y=node_data$y[i],
      xend=node_data$x[node_data$right_child[i]],
      yend=node_data$y[node_data$right_child[i]],
      edge_lab="False", hjust=0, vjust=.5)
    dplyr::bind_rows(rows)
  })
  cli::cli_alert_success("✔ Edge data created")

  edge_data <- edge_data |>
    dplyr::mutate(label_x = (x+xend)/2 + edge_label_offset*sign(xend-x),
                  label_y = (y+yend)/2)

  node_data <- node_data |>
    dplyr::mutate(
      fill_col = dplyr::case_when(
        is_leaf & action_lbl == "control"  ~ "#4f88c6",
        is_leaf & action_lbl == "treated"  ~ "#d8a739",
        TRUE                                ~ non_leaf_fill
      )
    )

  aspect <- ifelse(diff(range(node_data$y)) == 0,
                   1,
                   (diff(range(node_data$x))/diff(range(node_data$y))) * span_ratio)

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data=edge_data,
      ggplot2::aes(x=x,y=y,xend=xend,yend=yend)) +
    ggplot2::geom_label(
      data=node_data,
      ggplot2::aes(x=x,y=y,label=label,fill=fill_col),
      colour="black", size=text_size,
      label.padding=grid::unit(border_size,"lines")) +
    ggplot2::geom_text(
      data=edge_data,
      ggplot2::aes(x=label_x,y=label_y,label=edge_lab,hjust=hjust,vjust=vjust),
      size=text_size) +
    ggplot2::scale_fill_identity(guide="none") +
    ggplot2::coord_fixed(ratio=aspect) +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(x_padding,x_padding))) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(y_padding,y_padding))) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin  = plot_margin,
      plot.title   = ggplot2::element_text(hjust=.5,face="bold",margin=ggplot2::margin(b=20)),
      plot.caption = ggplot2::element_text(hjust=1,size=text_size+2)) +
    ggplot2::labs(
      title   = if (is.null(title)) tv(model_name) else tv(title),
      caption = if (!is.null(original_df)) "* original scale value" else NULL)

  cli::cli_alert_success("🎉 Plot created successfully")
  p
}

# fallback NULL-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# margot_plot_decision_tree <- function(
#     result_object,
#     model_name         = NULL,
#     max_depth          = 2L,
#     original_df        = NULL,
#     x_padding          = 0.12,
#     y_padding          = 0.25,
#     border_size        = .5,
#     text_size          = 4,
#     edge_label_offset  = 0.025,
#     span_ratio         = 0.4,
#     non_leaf_fill      = "lightyellow",
#     title              = NULL,
#     plot_margin        = grid::unit(c(1, 1, 1, 1), "cm"),
#     remove_tx_prefix   = TRUE,
#     remove_z_suffix    = TRUE,
#     use_title_case     = TRUE,
#     remove_underscores = TRUE,
#     remove_action_label= TRUE,
#     label_mapping      = NULL) {
#
#   cli::cli_h1("Margot Plot Decision Tree")
#
#   # ── 1 fetch tree ───────────────────────────────────────────────────────────
#   if (! max_depth %in% c(1L, 2L))
#     cli::cli_abort("`max_depth` must be 1 or 2.")
#   tag <- paste0("policy_tree_depth_", max_depth)
#   if (! (is.list(result_object) && "results" %in% names(result_object)))
#     cli::cli_abort("`result_object` must be the full list returned by `margot_causal_forest()`.")
#   if (! model_name %in% names(result_object$results))
#     cli::cli_abort("model `{model_name}` not found.")
#   policy_tree_obj <- result_object$results[[model_name]][[tag]]
#   if (is.null(policy_tree_obj))
#     cli::cli_abort("`{tag}` object missing for model `{model_name}`.")
#   cli::cli_alert_success("✔ Using decision tree at depth {max_depth} for model: {model_name}")
#
#   # helper to prettify labels -------------------------------------------------
#   tf_name <- function(x)
#     transform_var_name(x, label_mapping,
#                        remove_tx_prefix, remove_z_suffix,
#                        use_title_case, remove_underscores)
#
#   if (is.null(title)) {
#     title <- tf_name(model_name)
#     cli::cli_alert_info("ℹ Plot title set to: {title}")
#   }
#
#   # ── 2 extract nodes ────────────────────────────────────────────────────────
#   nodes        <- policy_tree_obj$nodes
#   columns      <- policy_tree_obj$columns
#   action_names <- policy_tree_obj$action.names
#
#   node_data <- tibble::tibble(
#     id          = seq_along(nodes),
#     is_leaf     = vapply(nodes, `[[`, logical(1), "is_leaf"),
#     split_var   = vapply(nodes,
#                          function(n) if (is.null(n$split_variable)) NA_character_
#                          else columns[n$split_variable],
#                          character(1)),
#     split_val   = vapply(nodes,
#                          function(n) if (is.null(n$split_value)) NA_real_
#                          else n$split_value,
#                          numeric(1)),
#     action_id   = vapply(nodes,
#                          function(n) {
#                            if (is.null(n$action)) NA_integer_
#                            else as.integer(n$action)        # <- fixed
#                          },
#                          integer(1)),
#     left_child  = vapply(nodes,
#                          function(n) {
#                            if (is.null(n$left_child)) NA_integer_
#                            else as.integer(n$left_child)    # robustness
#                          },
#                          integer(1)),
#     right_child = vapply(nodes,
#                          function(n) {
#                            if (is.null(n$right_child)) NA_integer_
#                            else as.integer(n$right_child)   # robustness
#                          },
#                          integer(1)),
#     x = NA_real_, y = NA_real_, label = NA_character_
#   ) |>
#     ## NEW: keep the literal label the tree supplies --------------------------
#   dplyr::mutate(
#     action_lbl = dplyr::case_when(
#       !is_leaf ~ NA_character_,
#       TRUE     ~ tolower(policy_tree_obj$action.names[action_id])
#     )
#   )
#
#   # ── 3 create labels (unchanged) ────────────────────────────────────────────
#   for (i in seq_len(nrow(node_data))) {
#     if (node_data$is_leaf[i]) {
#       lbl <- tf_name(action_names[node_data$action_id[i]])
#       if (! remove_action_label) lbl <- paste("Action:", lbl)
#       node_data$label[i] <- lbl
#     } else {
#       var   <- node_data$split_var[i]
#       val_s <- round(node_data$split_val[i], 3)
#       orig  <- get_original_value_plot(var, node_data$split_val[i], original_df)
#       var_lb<- tf_name(var)
#       node_data$label[i] <- if (!is.null(orig)) {
#         sprintf("%s\n<= %s\n(%s)*", var_lb, val_s,
#                 format(orig, big.mark = ",", scientific = FALSE))
#       } else {
#         sprintf("%s\n<= %s", var_lb, val_s)
#       }
#     }
#   }
#
#   # ── 4 layout, edges, colours (identical to previous version) ───────────────
#   max_d      <- policy_tree_obj$depth
#   assign_pos <- function(id, depth, xpos) {
#     node_data$y[id] <<- max_d - depth + 1
#     node_data$x[id] <<- xpos
#     if (!is.na(node_data$left_child[id]))
#       assign_pos(node_data$left_child[id], depth + 1, xpos - 1/(2^depth))
#     if (!is.na(node_data$right_child[id]))
#       assign_pos(node_data$right_child[id], depth + 1, xpos + 1/(2^depth))
#   }
#   assign_pos(1, 1, 0.5);  cli::cli_alert_success("✔ Node positions calculated")
#
#   edge_data <- purrr::map_dfr(seq_len(nrow(node_data)), function(i) {
#     rows <- list()
#     if (!is.na(node_data$left_child[i])) rows[[1]] <- tibble::tibble(
#       x=node_data$x[i], y=node_data$y[i],
#       xend=node_data$x[node_data$left_child[i]],
#       yend=node_data$y[node_data$left_child[i]],
#       edge_lab="True", hjust=1, vjust=.5)
#     if (!is.na(node_data$right_child[i])) rows[[length(rows)+1]] <- tibble::tibble(
#       x=node_data$x[i], y=node_data$y[i],
#       xend=node_data$x[node_data$right_child[i]],
#       yend=node_data$y[node_data$right_child[i]],
#       edge_lab="False", hjust=0, vjust=.5)
#     dplyr::bind_rows(rows)
#   })
#   cli::cli_alert_success("✔ Edge data created")
#
#   edge_data <- edge_data |>
#     dplyr::mutate(label_x = (x+xend)/2 + edge_label_offset*sign(xend-x),
#                   label_y = (y+yend)/2)
#
#   node_data <- node_data |>
#     dplyr::mutate(
#       fill_col = dplyr::case_when(
#         is_leaf & action_lbl == "control"  ~ "#4f88c6",  # blue
#         is_leaf & action_lbl == "treated"  ~ "#d8a739",  # orange
#         TRUE                               ~ non_leaf_fill
#       )
#     )
#
#   aspect <- ifelse(diff(range(node_data$y)) == 0,
#                    1,
#                    (diff(range(node_data$x))/diff(range(node_data$y))) * span_ratio)
#
#   p <- ggplot2::ggplot() +
#     ggplot2::geom_segment(
#       data=edge_data,
#       ggplot2::aes(x=x,y=y,xend=xend,yend=yend)) +
#     ggplot2::geom_label(
#       data=node_data,
#       ggplot2::aes(x=x,y=y,label=label,fill=fill_col),
#       colour="black", size=text_size,
#       label.padding=grid::unit(border_size,"lines")) +
#     ggplot2::geom_text(
#       data=edge_data,
#       ggplot2::aes(x=label_x,y=label_y,label=edge_lab,hjust=hjust,vjust=vjust),
#       size=text_size) +
#     ggplot2::scale_fill_identity(guide="none") +
#     ggplot2::coord_fixed(ratio=aspect) +
#     ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(x_padding,x_padding))) +
#     ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(y_padding,y_padding))) +
#     ggplot2::theme_void() +
#     ggplot2::theme(
#       plot.margin  = plot_margin,
#       plot.title   = ggplot2::element_text(hjust=.5,face="bold",margin=ggplot2::margin(b=20)),
#       plot.caption = ggplot2::element_text(hjust=1,size=text_size+2)) +
#     ggplot2::labs(
#       title   = title,
#       caption = if (!is.null(original_df)) "* original scale value" else NULL)
#
#   cli::cli_alert_success("🎉 Plot created successfully")
#   p
# }

# To develop
#' create a palette that always assigns control = orange, treated = blue
#'
#' @param actions numeric or character vector returned by the policy-tree
#' @param label_mapping optional list passed down from the main function
#' @param col_control  hex for control
#' @param col_treated  hex for treated
#' @return named character vector (names = display labels, values = hex codes)
# make_leaf_palette <- function(actions,
#                               label_mapping = NULL,
#                               col_control   = "#4f88c6",
#                               col_treated   = "#d8a739") {
#
#   # turn any numeric action codes into canonical strings -----------------------
#   action_label <- if (is.numeric(actions)) {
#     dplyr::case_when(
#       actions == 1 ~ "control",
#       actions == 2 ~ "treated",
#       TRUE         ~ paste0("action_", actions)
#     )
#   } else {
#     tolower(actions)
#   }
#
#   # pretty labels (allows user remapping, capitalisation, etc.) ----------------
#   display_label <- purrr::map_chr(
#     action_label,
#     transform_var_name,
#     label_mapping      = label_mapping,
#     remove_tx_prefix   = FALSE,
#     remove_z_suffix    = FALSE,
#     use_title_case     = TRUE,
#     remove_underscores = FALSE
#   )
#
#   # final palette table --------------------------------------------------------
#   palette_tbl <- tibble::tibble(
#     display_label = display_label,
#     colour        = dplyr::case_when(
#       action_label == "control" ~ col_control,
#       action_label == "treated" ~ col_treated,
#       TRUE                      ~ "#cccccc"
#     )
#   ) |>
#     dplyr::distinct(display_label, .keep_all = TRUE)
#
#   stats::setNames(palette_tbl$colour, palette_tbl$display_label)
# }


# original function with only 2L
#' #' Plot a Decision Tree from Margot Causal Forest Results
#' #'
#' #' This function creates a ggplot visualization of a decision tree from Margot Causal Forest results.
#' #' It handles label transformations, includes both standardized and original split values,
#' #' and provides informative feedback using cli messages.
#' #'
#' #' @param result_object A list containing the policy tree object or Margot results.
#' #' @param model_name Character string specifying the model name in the results object. Default is NULL.
#' #' @param original_df Optional dataframe with untransformed variables, used to display split values on the data scale.
#' #' @param x_padding Numeric value for horizontal padding of the plot. Default is 0.12.
#' #' @param y_padding Numeric value for vertical padding of the plot. Default is 0.12.
#' #' @param border_size Numeric value for the size of node borders. Default is 0.5.
#' #' @param edge_label_offset Numeric value for the offset of edge labels. Default is 0.025.
#' #' @param span_ratio Numeric value for the aspect ratio of the plot. Default is 0.4.
#' #' @param text_size Numeric value for the size of text in the plot. Default is 3.
#' #' @param non_leaf_fill Character string specifying the fill color for non-leaf nodes. Default is "lightyellow".
#' #' @param title Character string for the plot title. Default is NULL.
#' #' @param leaf_palette Vector of colors for leaf nodes. Default is NULL (uses Okabe-Ito palette).
#' #' @param remove_tx_prefix Logical indicating whether to remove "tx_" prefix from labels. Default is TRUE.
#' #' @param remove_z_suffix Logical indicating whether to remove "_z" suffix from labels. Default is TRUE.
#' #' @param use_title_case Logical indicating whether to convert labels to title case. Default is TRUE.
#' #' @param remove_underscores Logical indicating whether to remove underscores from labels. Default is TRUE.
#' #' @param remove_action_label Logical indicating whether to remove "Action: " prefix from leaf node labels in the final plot. Default is TRUE.
#' #' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names (with or without "model_" prefix),
#' #'        and values should be the desired display labels. Default is NULL.
#' #'
#' #' @return A ggplot object representing the decision tree.
#' #'
#' #' @import ggplot2
#' #' @import cli
#' #' @importFrom tools toTitleCase
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Assuming 'results' is your Margot results object and 'model_name' is the name of your model
#' #' tree_plot <- margot_plot_decision_tree(results, model_name = "my_model")
#' #' print(tree_plot)
#' #'
#' #' # To keep the "Action: " prefix in leaf node labels
#' #' tree_plot <- margot_plot_decision_tree(results, model_name = "my_model", remove_action_label = FALSE)
#' #' print(tree_plot)
#' #'
#' #' # Using custom label mapping and original_df for unstandardized values
#' #' label_mapping <- list(
#' #'   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
#' #'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
#' #' )
#' #' tree_plot <- margot_plot_decision_tree(results, model_name = "model_t2_env_not_env_efficacy_z",
#' #'                                        label_mapping = label_mapping,
#' #'                                        original_df = original_df)
#' #' print(tree_plot)
#' #' }
#' #'
#' margot_plot_decision_tree <- function(result_object,
#'                                       model_name = NULL,
#'                                       original_df = NULL,
#'                                       x_padding = 0.12,
#'                                       y_padding = 0.25,
#'                                       border_size = .5,
#'                                       text_size = 4,
#'                                       edge_label_offset = 0.025,
#'                                       span_ratio = 0.4,
#'                                       non_leaf_fill = "lightyellow",
#'                                       title = NULL,
#'                                       leaf_palette = NULL,
#'                                       plot_margin = unit(c(1, 1, 1, 1), "cm"),  # new parameter with old default
#'                                       remove_tx_prefix = TRUE,
#'                                       remove_z_suffix = TRUE,
#'                                       use_title_case = TRUE,
#'                                       remove_underscores = TRUE,
#'                                       remove_action_label = TRUE,
#'                                       label_mapping = NULL) {
#'
#'   cli::cli_h1("Margot Plot Decision Tree")
#'
#'   # Define the Okabe-Ito palette
#'   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73",
#'                          "#F0E442", "#0072B2", "#D55E00",
#'                          "#CC79A7", "#000000")
#'
#'   # Create title
#'   if (is.null(title) && !is.null(model_name)) {
#'     title <- transform_var_name(model_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
#'     cli::cli_alert_info("Using transformed model name as plot title: {title}")
#'   } else if (identical(title, "none")) {
#'     title <- NULL
#'     cli::cli_alert_info("No title will be displayed on the plot")
#'   } else if (!is.null(title)) {
#'     cli::cli_alert_info("Using provided title: {title}")
#'   }
#'
#'   # Ensure title is a character string or NULL
#'   if (!is.character(title) && !is.null(title)) {
#'     cli::cli_warn("Invalid title type. Setting title to NULL.")
#'     title <- NULL
#'   }
#'
#'   # Determine the correct policy tree object
#'   if (!is.null(model_name) && "results" %in% names(result_object)) {
#'     policy_tree_obj <- result_object$results[[model_name]]$policy_tree_depth_2
#'     if (is.null(policy_tree_obj)) {
#'       cli::cli_abort("Policy tree object not found for the specified model name.")
#'     }
#'     cli::cli_alert_success("Using policy tree from model: {model_name}")
#'   } else if ("nodes" %in% names(result_object)) {
#'     policy_tree_obj <- result_object
#'     cli::cli_alert_success("Using provided policy tree object")
#'   } else {
#'     cli::cli_abort("Invalid input. Please provide either a results object with a model name, or a policy tree object.")
#'   }
#'
#'   # Calculate node positions and create node data
#'   calculate_node_positions <- function(policy_tree) {
#'     cli::cli_h2("Calculating Node Positions")
#'     nodes <- policy_tree$nodes
#'     columns <- policy_tree$columns
#'     action_names <- policy_tree$action.names
#'
#'     node_data <- data.frame(
#'       id = integer(length(nodes)),
#'       is_leaf = logical(length(nodes)),
#'       split_variable = character(length(nodes)),
#'       split_value = numeric(length(nodes)),
#'       action = character(length(nodes)),
#'       left_child = integer(length(nodes)),
#'       right_child = integer(length(nodes)),
#'       x = numeric(length(nodes)),
#'       y = numeric(length(nodes)),
#'       label = character(length(nodes)),
#'       stringsAsFactors = FALSE
#'     )
#'
#'     for (i in seq_along(nodes)) {
#'       node_data$id[i] <- i
#'       node_data$is_leaf[i] <- nodes[[i]]$is_leaf
#'       node_data$split_variable[i] <- if (!is.null(nodes[[i]]$split_variable)) as.character(columns[nodes[[i]]$split_variable]) else NA_character_
#'       node_data$split_value[i] <- if (!is.null(nodes[[i]]$split_value)) as.numeric(nodes[[i]]$split_value) else NA_real_
#'       node_data$action[i] <- if (!is.null(nodes[[i]]$action)) as.character(action_names[nodes[[i]]$action]) else NA_character_
#'       node_data$left_child[i] <- if (!is.null(nodes[[i]]$left_child)) as.integer(nodes[[i]]$left_child) else NA_integer_
#'       node_data$right_child[i] <- if (!is.null(nodes[[i]]$right_child)) as.integer(nodes[[i]]$right_child) else NA_integer_
#'
#'       # Create label with transformed variables and include original value with asterisk
#'       if (node_data$is_leaf[i]) {
#'         # For leaf nodes, include the action
#'         action_label <- transform_var_name(node_data$action[i], label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
#'         if (!remove_action_label) {
#'           action_label <- paste("Action:", action_label)
#'         }
#'         node_data$label[i] <- action_label
#'       } else {
#'         # For split nodes, include both standardized and original split values
#'         split_var_name <- node_data$split_variable[i]
#'         split_var_label <- transform_var_name(split_var_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
#'         split_value <- round(node_data$split_value[i], 3)
#'         original_split <- get_original_value_plot(split_var_name, node_data$split_value[i], original_df)
#'
#'         if (!is.null(original_split)) {
#'           label_with_original <- paste0(
#'             split_var_label,
#'             "\n<= ", split_value,
#'             "\n(", format(original_split, scientific = FALSE, big.mark = ","), ")*"
#'           )
#'         } else {
#'           label_with_original <- paste0(
#'             split_var_label,
#'             "\n<= ", split_value
#'           )
#'         }
#'         node_data$label[i] <- label_with_original
#'       }
#'     }
#'
#'     # Assign positions based on tree depth and node order
#'     # This is a simple binary tree positioning
#'     max_depth <- policy_tree$depth
#'     assign_positions <- function(node_id, depth, x_pos) {
#'       node_data$y[node_id] <<- max_depth - depth + 1
#'       node_data$x[node_id] <<- x_pos
#'
#'       left_child <- node_data$left_child[node_id]
#'       right_child <- node_data$right_child[node_id]
#'
#'       if (!is.na(left_child)) {
#'         assign_positions(left_child, depth + 1, x_pos - 1 / (2 ^ depth))
#'       }
#'       if (!is.na(right_child)) {
#'         assign_positions(right_child, depth + 1, x_pos + 1 / (2 ^ depth))
#'       }
#'     }
#'     assign_positions(1, 1, 0.5)
#'
#'     cli::cli_alert_success("Node positions calculated")
#'     return(node_data)
#'   }
#'
#'   # Print policy tree structure with dual labels
#'   print_policy_tree <- function(policy_tree) {
#'     print_node <- function(node, depth = 0) {
#'       indent <- paste(rep("  ", depth), collapse = "")
#'       if (node$is_leaf) {
#'         action_label <- transform_var_name(policy_tree$action.names[node$action], label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
#'         if (!remove_action_label) {
#'           action_label <- paste("Action:", action_label)
#'         }
#'         cat(paste0(indent, "* ", action_label, "\n"))
#'       } else {
#'         # Get original split value if available
#'         split_var_name <- policy_tree$columns[node$split_variable]
#'         split_var_label <- transform_var_name(split_var_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case, remove_underscores)
#'         split_value <- node$split_value
#'         original_split <- get_original_value_plot(split_var_name, split_value, original_df)
#'
#'         if (!is.null(original_split)) {
#'           split_info <- paste0(
#'             split_var_label, " <= ", split_value,
#'             "\n(", format(original_split, scientific = FALSE, big.mark = ","), ")*"
#'           )
#'         } else {
#'           split_info <- paste0(
#'             split_var_label, " <= ", split_value
#'           )
#'         }
#'         cat(paste0(indent, "Split on: ", split_info, "\n"))
#'         if (!is.na(node$left_child)) {
#'           print_node(policy_tree$nodes[[node$left_child]], depth + 1)
#'         }
#'         if (!is.na(node$right_child)) {
#'           print_node(policy_tree$nodes[[node$right_child]], depth + 1)
#'         }
#'       }
#'     }
#'     cli::cli_h2("Policy Tree Structure")
#'     print_node(policy_tree$nodes[[1]])
#'   }
#'
#'   print_policy_tree(policy_tree_obj)
#'
#'   # Calculate node positions
#'   node_data <- calculate_node_positions(policy_tree_obj)
#'
#'   # Check for non-finite x or y
#'   if (any(!is.finite(node_data$x)) || any(!is.finite(node_data$y))) {
#'     cli::cli_abort("Non-finite x or y positions detected in node_data. Please check the node positioning logic.")
#'   }
#'
#'   # Create edge data
#'   cli::cli_h2("Creating Edge Data")
#'   edge_data <- data.frame(
#'     x = numeric(),
#'     y = numeric(),
#'     xend = numeric(),
#'     yend = numeric(),
#'     edge_label = character(),
#'     hjust = numeric(),
#'     vjust = numeric(),
#'     angle = numeric(),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   for (i in 1:nrow(node_data)) {
#'     if (!is.na(node_data$left_child[i])) {
#'       left_child <- node_data$left_child[i]
#'       edge_data <- rbind(edge_data, data.frame(
#'         x = node_data$x[i],
#'         y = node_data$y[i],
#'         xend = node_data$x[left_child],
#'         yend = node_data$y[left_child],
#'         edge_label = "True",
#'         hjust = 1,
#'         vjust = 0.5,
#'         angle = 0,
#'         stringsAsFactors = FALSE
#'       ))
#'     }
#'     if (!is.na(node_data$right_child[i])) {
#'       right_child <- node_data$right_child[i]
#'       edge_data <- rbind(edge_data, data.frame(
#'         x = node_data$x[i],
#'         y = node_data$y[i],
#'         xend = node_data$x[right_child],
#'         yend = node_data$y[right_child],
#'         edge_label = "False",
#'         hjust = 0,
#'         vjust = 0.5,
#'         angle = 0,
#'         stringsAsFactors = FALSE
#'       ))
#'     }
#'   }
#'
#'   cli::cli_alert_success("Edge data created")
#'
#'   # Adjust label positions
#'   edge_data$label_x <- edge_data$x + (edge_data$xend - edge_data$x) / 2
#'   edge_data$label_y <- edge_data$y + (edge_data$yend - edge_data$y) / 2
#'
#'   # Adjust label positions to be outside the branches
#'   edge_data$label_x <- edge_data$label_x +
#'     edge_label_offset * sign(edge_data$xend - edge_data$x)
#'
#'   # Use provided palette or default to Okabe-Ito
#'   if (is.null(leaf_palette)) {
#'     leaf_palette <- okabe_ito_palette
#'     cli::cli_alert_info("Using default Okabe-Ito color palette")
#'   } else {
#'     cli::cli_alert_info("Using custom color palette")
#'   }
#'
#'   # Create a mapping of actions to colors based on the policy tree's action order
#'   unique_actions <- unique(policy_tree_obj$action.names)
#'   action_colors <- setNames(
#'     leaf_palette[seq_along(unique_actions)],
#'     unique_actions
#'   )
#'
#'   # Assign colors to leaf nodes based on their action
#'   node_data$fill_color <- ifelse(
#'     node_data$is_leaf,
#'     action_colors[node_data$action],
#'     non_leaf_fill
#'   )
#'
#'   # Calculate aspect ratio based on the span_ratio
#'   y_range <- max(node_data$y) - min(node_data$y)
#'   x_range <- max(node_data$x) - min(node_data$x)
#'
#'   # Prevent division by zero
#'   if (y_range == 0) {
#'     aspect_ratio <- 1
#'     cli::cli_warn("y_range is zero. Setting aspect_ratio to 1.")
#'   } else {
#'     aspect_ratio <- (x_range / y_range) * span_ratio
#'   }
#'
#'   cli::cli_alert("Creating plot...")
#'
#'   p <- ggplot() +
#'     geom_segment(data = edge_data, aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_label(data = node_data, aes(x = x, y = y, label = label),
#'                size = text_size,
#'                fill = node_data$fill_color,
#'                label.padding = unit(border_size, "lines")) +
#'     geom_text(data = edge_data,
#'               aes(x = label_x, y = label_y, label = edge_label,
#'                   hjust = hjust, vjust = vjust),
#'               size = text_size) +
#'     theme_void() +
#'     coord_fixed(ratio = aspect_ratio) +
#'     scale_x_continuous(expand = expansion(mult = c(x_padding, x_padding))) +
#'     scale_y_continuous(expand = expansion(mult = c(y_padding, y_padding))) +
#'     theme(
#'       plot.margin = plot_margin,  # increase top margin from 1cm to 2.5cm
#'       plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)),  # add bottom margin to title
#'       plot.caption = element_text(hjust = 1, size = text_size + 2)
#'     ) +
#'     labs(
#'       title = title,
#'       caption = if (!is.null(original_df)) "* Original scale value" else NULL
#'     )
#'
#'   # Display a warning if original_df was provided but no original values were found
#'   if (!is.null(original_df)) {
#'     missing_originals <- node_data$label[grepl("\\*", node_data$label) & is.na(node_data$split_variable)]
#'     if (length(missing_originals) > 0) {
#'       cli::cli_warn("Some leaf nodes do not have original scale values available.")
#'     }
#'   }
#'
#'   cli::cli_alert_success("Plot created successfully 👍")
#'
#'   return(p)
#' }

