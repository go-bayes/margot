#' Summarise policy-tree leaves with estimated action advantages and sample shares
#'
#' @description
#' Computes leaf-level summaries for a stored policy tree. Leaf advantages are
#' estimated from doubly robust action scores and are action conditional: treated
#' leaves report the estimated advantage of treatment relative to control,
#' whereas control leaves report the estimated advantage of control relative to
#' treatment.
#'
#' @param object A \code{margot_causal_forest()}-style object containing
#'   \code{results}, \code{covariates}, and optionally \code{weights}.
#' @param model_name Outcome/model name, with or without the \code{model_}
#'   prefix.
#' @param depth Integer policy-tree depth, usually \code{1} or \code{2}.
#' @param weights Optional evaluation weights. Defaults to
#'   \code{object$weights}.
#' @param digits Integer; rounding used in formatted labels.
#' @param label_mapping Optional named list used to label actions.
#'
#' @return A tibble with one row per leaf and columns for node id, action,
#'   unweighted count, weighted sample share, action-conditional estimated
#'   advantage, the legacy \code{estimated_gain} alias, and policy-value
#'   contributions.
#' @export
margot_policy_leaf_summary <- function(object,
                                       model_name,
                                       depth = 1L,
                                       weights = NULL,
                                       digits = 3L,
                                       label_mapping = NULL) {
  # compute action-conditional leaf summaries for one stored policy tree.
  if (!is.list(object) || is.null(object$results)) {
    stop("object must contain a 'results' list", call. = FALSE)
  }
  model_name <- .margot_leaf_resolve_model_name(object, model_name)
  model <- object$results[[model_name]]
  depth <- as.integer(depth)
  if (length(depth) != 1L || is.na(depth) || depth < 1L) {
    stop("depth must be a single positive integer", call. = FALSE)
  }
  tree <- model[[paste0("policy_tree_depth_", depth)]]
  if (is.null(tree)) {
    stop("stored policy tree not found for requested depth", call. = FALSE)
  }

  eval_data <- .policy_regret_evaluation_data(
    object = object,
    model = model,
    tree = tree,
    weights = weights %||% object$weights %||% NULL
  )
  if (is.null(eval_data)) {
    stop("could not build policy-tree evaluation data", call. = FALSE)
  }

  covariates <- eval_data$covariates
  dr_scores <- as.matrix(eval_data$dr_scores)
  if (ncol(dr_scores) != 2L) {
    stop(
      "margot_policy_leaf_summary() currently supports binary treatment ",
      "policy trees only; dr_scores must have exactly two action columns.",
      call. = FALSE
    )
  }
  eval_weights <- eval_data$weights
  leaf_ids <- .margot_policy_tree_leaf_ids(tree, covariates)
  if (length(leaf_ids) != nrow(dr_scores)) {
    stop("policy-tree leaf assignments do not align with scores", call. = FALSE)
  }
  keep <- is.finite(leaf_ids)
  if (!any(keep)) {
    stop("no rows could be assigned to policy-tree leaves", call. = FALSE)
  }
  leaf_ids <- leaf_ids[keep]
  dr_scores <- dr_scores[keep, , drop = FALSE]
  eval_weights <- if (!is.null(eval_weights)) eval_weights[keep] else NULL

  total_weight <- if (!is.null(eval_weights)) {
    sum(eval_weights[is.finite(eval_weights) & eval_weights > 0], na.rm = TRUE)
  } else {
    length(leaf_ids)
  }
  if (!is.finite(total_weight) || total_weight <= 0) {
    stop("evaluation weights are not usable", call. = FALSE)
  }

  effect <- as.numeric(dr_scores[, 2] - dr_scores[, 1])
  action_names <- tree$action.names %||% c("control", "treated")

  rows <- lapply(sort(unique(leaf_ids)), function(leaf_id) {
    idx <- which(leaf_ids == leaf_id)
    node <- tree$nodes[[leaf_id]]
    action_id <- as.integer(node$action)
    action_name <- action_names[[action_id]]
    w <- if (!is.null(eval_weights)) eval_weights[idx] else rep(1, length(idx))
    share <- sum(w, na.rm = TRUE) / total_weight
    effect_leaf <- effect[idx]
    estimated_advantage <- if (action_id == 2L) {
      .policy_regret_mean(effect_leaf, w)
    } else {
      .policy_regret_mean(-effect_leaf, w)
    }
    contrast <- if (action_id == 2L) "gain_vs_control" else "gain_vs_treatment"
    advantage_comparison <- if (action_id == 2L) {
      "treatment advantage vs control"
    } else {
      "control advantage vs treatment"
    }
    value_contribution_vs_control <- if (action_id == 2L) share * estimated_advantage else 0
    value_contribution_vs_treatment <- if (action_id == 1L) share * estimated_advantage else 0
    action_label <- .margot_leaf_label_action(action_name, label_mapping)
    tibble::tibble(
      node_id = leaf_id,
      action_id = action_id,
      action = action_name,
      action_label = action_label,
      contrast = contrast,
      advantage_comparison = advantage_comparison,
      n = length(idx),
      sample_share = share,
      estimated_advantage = estimated_advantage,
      estimated_gain = estimated_advantage,
      value_contribution_vs_control = value_contribution_vs_control,
      value_contribution_vs_treatment = value_contribution_vs_treatment,
      label = .margot_leaf_metric_label(
        action_label = action_label,
        contrast = contrast,
        estimated_advantage = estimated_advantage,
        sample_share = share,
        digits = digits
      )
    )
  })

  out <- dplyr::bind_rows(rows)
  attr(out, "model") <- model_name
  attr(out, "depth") <- depth
  attr(out, "estimand") <- "action-conditional estimated advantage from doubly robust scores"
  out
}

#' @keywords internal
.margot_leaf_resolve_model_name <- function(object, model_name) {
  # resolve model identifiers with or without the model_ prefix.
  if (missing(model_name) || is.null(model_name) || !length(model_name)) {
    stop("model_name is required", call. = FALSE)
  }
  model_name <- as.character(model_name[[1]])
  candidates <- unique(c(model_name, paste0("model_", model_name)))
  hit <- candidates[candidates %in% names(object$results)]
  if (!length(hit)) {
    stop("model not found in object$results", call. = FALSE)
  }
  hit[[1]]
}

#' @keywords internal
.margot_policy_tree_leaf_ids <- function(tree, covariates) {
  # route each row through the stored tree and return terminal node ids.
  if (is.null(tree$nodes) || is.null(tree$columns)) {
    stop("tree must contain nodes and columns", call. = FALSE)
  }
  missing_cols <- setdiff(tree$columns, colnames(covariates))
  if (length(missing_cols)) {
    stop("covariates are missing policy-tree columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  covariates <- as.data.frame(covariates)
  vapply(seq_len(nrow(covariates)), function(i) {
    node_id <- 1L
    repeat {
      node <- tree$nodes[[node_id]]
      if (isTRUE(node$is_leaf)) return(node_id)
      split_var <- tree$columns[[node$split_variable]]
      split_value <- covariates[[split_var]][[i]]
      if (!is.finite(split_value)) return(NA_integer_)
      node_id <- if (split_value <= node$split_value) {
        as.integer(node$left_child)
      } else {
        as.integer(node$right_child)
      }
    }
  }, integer(1))
}

#' @keywords internal
.margot_leaf_label_action <- function(action_name, label_mapping = NULL) {
  # transform action labels while keeping readable defaults.
  action_normalised <- tolower(as.character(action_name[[1]]))
  if (action_normalised %in% c("control", "untreated", "no treatment")) {
    return("Control")
  }
  if (action_normalised %in% c("treated", "treatment", "treat", "tx", "tx_1")) {
    return("Treatment")
  }
  label <- tryCatch(
    transform_label(
      action_name,
      label_mapping,
      list(
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        remove_underscores = TRUE,
        use_title_case = TRUE
      )
    ),
    error = function(e) action_name
  )
  label
}

#' @keywords internal
.margot_leaf_metric_label <- function(action_label,
                                      contrast,
                                      estimated_advantage,
                                      sample_share,
                                      digits = 3L) {
  # format a compact plot label for one policy-tree leaf.
  advantage_label <- if (is.finite(estimated_advantage)) {
    sprintf(paste0("%+.", digits, "f"), estimated_advantage)
  } else {
    "NA"
  }
  share_label <- if (is.finite(sample_share)) {
    sprintf("%.1f%%", 100 * sample_share)
  } else {
    "NA"
  }
  paste(action_label, paste0("adv: ", advantage_label),
        paste0("share: ", share_label), sep = "\n")
}
