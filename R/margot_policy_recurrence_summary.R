#' Summarise outcome-wide recurrence in held-out policy-tree splits
#'
#' @description
#' Builds a descriptive outcome-wide recurrence table from
#' \code{margot_policy_tree_cv()}. The summary counts how often baseline
#' variables recur as root or any-node splits across outcomes. It is a
#' reporting diagnostic, not a formal multiplicity-adjusted test.
#'
#' @param policy_cv A \code{margot_policy_tree_cv} object.
#' @param selected_depth_only Logical; if \code{TRUE}, use the selected depth for
#'   each outcome when \code{policy_cv$depth_map} is available.
#' @param gain_weighted Logical; if \code{TRUE}, include a positive-held-out-gain
#'   weighted root-frequency column.
#'
#' @return A tibble with one row per variable.
#' @export
margot_policy_recurrence_summary <- function(policy_cv,
                                             selected_depth_only = TRUE,
                                             gain_weighted = TRUE) {
  # summarise recurring policy-tree split variables across outcomes.
  if (!is.list(policy_cv) || is.null(policy_cv$split_summary)) {
    stop("policy_cv must be a margot_policy_tree_cv-like object", call. = FALSE)
  }
  splits <- policy_cv$split_summary
  if (is.null(splits) || !nrow(splits)) return(tibble::tibble())
  values <- policy_cv$value_summary %||% data.frame()
  depth_map <- policy_cv$depth_map %||% integer()

  if (isTRUE(selected_depth_only) && length(depth_map)) {
    selected <- data.frame(
      model = names(depth_map),
      depth = as.integer(depth_map),
      stringsAsFactors = FALSE
    )
    splits <- dplyr::inner_join(splits, selected, by = c("model", "depth"))
    if (nrow(values)) {
      values <- dplyr::inner_join(values, selected, by = c("model", "depth"))
    }
  }
  if (!nrow(splits)) return(tibble::tibble())

  model_ids <- sort(unique(splits$model))
  variables <- sort(unique(splits$variable))

  root <- splits[splits$node_id == 1L, , drop = FALSE]
  root_rows <- lapply(variables, function(variable) {
    per_model <- vapply(model_ids, function(model) {
      x <- root$selection_frequency[root$model == model & root$variable == variable]
      if (length(x)) max(x, na.rm = TRUE) else 0
    }, numeric(1))
    top_model <- vapply(model_ids, function(model) {
      df <- root[root$model == model, , drop = FALSE]
      if (!nrow(df)) return(FALSE)
      max_freq <- max(df$selection_frequency, na.rm = TRUE)
      any(df$variable == variable & df$selection_frequency == max_freq)
    }, logical(1))
    any_node <- vapply(model_ids, function(model) {
      df <- splits[splits$model == model & splits$variable == variable, , drop = FALSE]
      if (!nrow(df)) return(0)
      pmin(1, sum(df$selection_frequency, na.rm = TRUE))
    }, numeric(1))
    gain_weights <- rep(1, length(model_ids))
    if (isTRUE(gain_weighted) && nrow(values) && "gain_vs_control_mean" %in% names(values)) {
      gain_lookup <- stats::setNames(pmax(0, values$gain_vs_control_mean), values$model)
      gain_weights <- unname(gain_lookup[model_ids])
      gain_weights[!is.finite(gain_weights)] <- 0
      if (sum(gain_weights) <= 0) gain_weights <- rep(1, length(model_ids))
    }
    label <- root$variable_label[root$variable == variable][1] %||% variable
    tibble::tibble(
      variable = variable,
      variable_label = label,
      n_outcomes = length(model_ids),
      n_outcomes_root_selected = sum(per_model > 0),
      n_outcomes_top_root = sum(top_model),
      mean_root_frequency = mean(per_model),
      gain_weighted_root_frequency = stats::weighted.mean(per_model, gain_weights),
      mean_any_node_frequency = mean(any_node)
    )
  })

  out <- dplyr::bind_rows(root_rows)
  out |>
    dplyr::arrange(
      dplyr::desc(.data$n_outcomes_top_root),
      dplyr::desc(.data$gain_weighted_root_frequency),
      dplyr::desc(.data$mean_root_frequency),
      .data$variable
    )
}
