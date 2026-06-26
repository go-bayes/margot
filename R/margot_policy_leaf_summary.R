#' Summarise policy-tree leaves with treatment-control contrasts and sample shares
#'
#' @description
#' Computes leaf-level summaries for a stored policy tree. Leaf contrasts are
#' estimated from doubly robust action scores and use a fixed signed comparison:
#' treatment minus control. Positive values favour treatment and negative values
#' favour control. The legacy action-conditional advantage columns are retained
#' as compatibility aliases.
#'
#' @details
#' Let \eqn{\Gamma_{ja}} denote the action score for observation \eqn{j} under
#' action \eqn{a}, let \eqn{L} denote a policy-tree leaf, and let
#' \eqn{E_L} denote the evaluation observations routed to that leaf. For binary
#' actions \eqn{C} and \eqn{T}, this helper reports the signed evaluation-sample
#' contrast
#' \deqn{
#'   \Delta_L =
#'   \frac{\sum_{j \in E_L} w_j\{\Gamma_{jT} - \Gamma_{jC}\}}
#'        {\sum_{j \in E_L} w_j}.
#' }
#' The selected action is reported separately. For the fitted tree, the selected
#' action is the action stored in the terminal node, learned on the training
#' observations \eqn{S_L} routed to that node:
#' \deqn{
#'   \pi(L) = \arg\max_{a \in \{C,T\}}
#'   \frac{\sum_{j \in S_L} w_j \Gamma_{ja}}{\sum_{j \in S_L} w_j}.
#' }
#' The reported \eqn{\Delta_L} is computed on the evaluation rows and does not
#' reselect the action from evaluation-row means.
#' Between-leaf differences in \eqn{\Delta_L} describe variation in magnitude.
#' They are not the decision rule used by the policy tree.
#'
#' @param object A \code{margot_causal_forest()}-style object containing
#'   \code{results}, \code{covariates}, and optionally \code{weights}.
#' @param model_name Outcome/model name, with or without the \code{model_}
#'   prefix.
#' @param depth Integer policy-tree depth, usually \code{1} or \code{2}.
#' @param weights Optional evaluation weights. Defaults to
#'   \code{object$weights}.
#' @param digits Integer; rounding used in formatted labels.
#' @param ci_level Numeric confidence level for approximate row-level score
#'   intervals. Defaults to \code{0.95}.
#' @param label_mapping Optional named list used to label actions.
#'
#' @return A tibble with one row per leaf and columns for node id, action,
#'   unweighted count, weighted sample share, signed treatment-control contrast,
#'   approximate interval columns, legacy action-conditional advantage aliases,
#'   selected-action metadata, and policy-value contributions.
#' @export
margot_policy_leaf_summary <- function(object,
                                       model_name,
                                       depth = 1L,
                                       weights = NULL,
                                       digits = 3L,
                                       ci_level = 0.95,
                                       label_mapping = NULL) {
  # compute signed leaf summaries for one stored policy tree.
  if (!is.list(object) || is.null(object$results)) {
    stop("object must contain a 'results' list", call. = FALSE)
  }
  model_name <- .margot_leaf_resolve_model_name(object, model_name)
  model <- object$results[[model_name]]
  depth <- as.integer(depth)
  if (length(depth) != 1L || is.na(depth) || depth < 1L) {
    stop("depth must be a single positive integer", call. = FALSE)
  }
  if (!is.numeric(ci_level) || length(ci_level) != 1L ||
      is.na(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be a single number between 0 and 1", call. = FALSE)
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

  action_columns <- .margot_policy_binary_action_columns(
    dr_scores = dr_scores,
    tree = tree,
    context = "margot_policy_leaf_summary()"
  )
  effect <- as.numeric(dr_scores[, action_columns$treatment] - dr_scores[, action_columns$control])
  action_names <- action_columns$action_names

  rows <- lapply(sort(unique(leaf_ids)), function(leaf_id) {
    idx <- which(leaf_ids == leaf_id)
    node <- tree$nodes[[leaf_id]]
    action_id <- as.integer(node$action)
    if (!is.finite(action_id) || action_id < 1L || action_id > length(action_names)) {
      return(NULL)
    }
    action_name <- action_names[[action_id]]
    w <- if (!is.null(eval_weights)) eval_weights[idx] else rep(1, length(idx))
    share <- sum(w, na.rm = TRUE) / total_weight
    effect_leaf <- effect[idx]
    tc_interval <- .margot_policy_leaf_interval(effect_leaf, w, ci_level)
    treatment_control_contrast <- tc_interval$estimate
    selected_treatment <- action_id == action_columns$treatment
    estimated_advantage <- if (selected_treatment) treatment_control_contrast else -treatment_control_contrast
    advantage_ci_low <- if (selected_treatment) tc_interval$ci_low else -tc_interval$ci_high
    advantage_ci_high <- if (selected_treatment) tc_interval$ci_high else -tc_interval$ci_low
    contrast <- if (selected_treatment) "gain_vs_control" else "gain_vs_treatment"
    score_contrast <- "treatment_minus_control"
    advantage_comparison <- if (selected_treatment) {
      "treatment advantage vs control"
    } else {
      "control advantage vs treatment"
    }
    value_contribution_vs_control <- if (selected_treatment) share * estimated_advantage else 0
    value_contribution_vs_treatment <- if (!selected_treatment) share * estimated_advantage else 0
    action_label <- .margot_leaf_label_action(action_name, label_mapping)
    tibble::tibble(
      node_id = leaf_id,
      action_id = action_id,
      action = action_name,
      action_label = action_label,
      contrast = contrast,
      score_contrast = score_contrast,
      advantage_comparison = advantage_comparison,
      n = length(idx),
      sample_share = share,
      treatment_control_contrast = treatment_control_contrast,
      estimated_treatment_contrast = treatment_control_contrast,
      treatment_control_se = tc_interval$se,
      treatment_control_ci_low = tc_interval$ci_low,
      treatment_control_ci_high = tc_interval$ci_high,
      treatment_control_n_eff = tc_interval$n_eff,
      estimated_advantage = estimated_advantage,
      estimated_advantage_se = tc_interval$se,
      estimated_advantage_ci_low = advantage_ci_low,
      estimated_advantage_ci_high = advantage_ci_high,
      estimated_gain = estimated_advantage,
      value_contribution_vs_control = value_contribution_vs_control,
      value_contribution_vs_treatment = value_contribution_vs_treatment,
      label = .margot_leaf_metric_label(
        action_label = action_label,
        treatment_control_contrast = treatment_control_contrast,
        sample_share = share,
        digits = digits
      )
    )
  })

  out <- dplyr::bind_rows(rows)
  out$selected_action <- out$action_label
  out$tc_score_contrast <- out$treatment_control_contrast
  out$score_interval <- .margot_policy_format_ci(
    low = out$treatment_control_ci_low,
    high = out$treatment_control_ci_high,
    digits = digits
  )
  out$sample_percent <- .margot_policy_format_percent(out$sample_share)
  out$direction <- ifelse(
    is.finite(out$treatment_control_contrast) & out$treatment_control_contrast > 0,
    "favours treatment",
    ifelse(
      is.finite(out$treatment_control_contrast) & out$treatment_control_contrast < 0,
      "favours control",
      "no directional contrast"
    )
  )
  out$n_selected_actions <- length(unique(stats::na.omit(out$selected_action)))
  out$uniform_selected_action <- out$n_selected_actions <= 1L
  attr(out, "model") <- model_name
  attr(out, "depth") <- depth
  attr(out, "estimand") <- "signed treatment-minus-control contrast from doubly robust scores"
  attr(out, "ci_level") <- ci_level
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
.margot_policy_binary_action_columns <- function(dr_scores,
                                                 tree = NULL,
                                                 context = "policy-tree reporting") {
  # resolve binary treatment/control action columns from score or tree names.
  if (ncol(dr_scores) != 2L) {
    stop(
      context,
      " currently supports binary treatment policy trees only; ",
      "dr_scores must have exactly two action columns.",
      call. = FALSE
    )
  }
  action_names <- .margot_policy_action_names(dr_scores, tree, context)
  identities <- .margot_policy_action_identity(action_names)
  control <- which(identities == "control")
  treatment <- which(identities == "treatment")
  if (length(control) != 1L || length(treatment) != 1L || identical(control, treatment)) {
    stop(
      context,
      " could not identify exactly one control action and one treatment ",
      "action from dr_scores column names or tree action names.",
      call. = FALSE
    )
  }
  list(
    control = control,
    treatment = treatment,
    action_names = as.character(action_names)
  )
}

#' @keywords internal
.margot_policy_action_names <- function(dr_scores,
                                        tree = NULL,
                                        context = "policy-tree reporting") {
  # choose the action-name source while checking for contradictory labels.
  column_names <- colnames(dr_scores)
  has_column_names <- length(column_names) == ncol(dr_scores) &&
    all(!is.na(column_names)) &&
    all(nzchar(column_names))
  tree_names <- tree$action.names %||% NULL
  has_tree_names <- length(tree_names) == ncol(dr_scores) &&
    all(!is.na(tree_names)) &&
    all(nzchar(tree_names))

  if (has_column_names && has_tree_names) {
    column_identity <- .margot_policy_action_identity(column_names)
    tree_identity <- .margot_policy_action_identity(tree_names)
    if (!anyNA(column_identity) &&
        !anyNA(tree_identity) &&
        !identical(column_identity, tree_identity)) {
      stop(
        context,
        " found conflicting dr_scores column names and tree action names.",
        call. = FALSE
      )
    }
  }
  if (has_column_names) return(as.character(column_names))
  if (has_tree_names) return(as.character(tree_names))
  stop(
    context,
    " requires dr_scores column names or tree action names that identify ",
    "control and treatment actions.",
    call. = FALSE
  )
}

#' @keywords internal
.margot_policy_action_identity <- function(action_names) {
  # map common binary-action labels to stable treatment/control identities.
  normalised <- .margot_policy_normalise_action_name(action_names)
  control_aliases <- c(
    "control", "untreated", "notreated", "notreatment", "none",
    "c", "0", "tx0", "unexposed", "comparison"
  )
  treatment_aliases <- c(
    "treated", "treatment", "treat", "tx", "tx1", "1",
    "t", "exposed", "exposure", "intervention"
  )
  ifelse(
    normalised %in% control_aliases,
    "control",
    ifelse(normalised %in% treatment_aliases, "treatment", NA_character_)
  )
}

#' @keywords internal
.margot_policy_normalise_action_name <- function(action_names) {
  # remove punctuation and case variation before action-label matching.
  gsub("[^[:alnum:]]+", "", tolower(trimws(as.character(action_names))))
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
                                      treatment_control_contrast,
                                      sample_share,
                                      digits = 3L) {
  # format a compact plot label for one policy-tree leaf.
  contrast_label <- if (is.finite(treatment_control_contrast)) {
    sprintf(paste0("%+.", digits, "f"), treatment_control_contrast)
  } else {
    "NA"
  }
  share_label <- if (is.finite(sample_share)) {
    sprintf("%.1f%%", 100 * sample_share)
  } else {
    "NA"
  }
  paste(action_label, paste0("T-C: ", contrast_label),
        paste0("share: ", share_label), sep = "\n")
}

#' @keywords internal
.margot_policy_leaf_interval <- function(x, weights = NULL, ci_level = 0.95) {
  # estimate a weighted mean and approximate interval for row-level scores.
  ok <- is.finite(x)
  if (!is.null(weights)) ok <- ok & is.finite(weights) & weights > 0
  x <- as.numeric(x[ok])
  weights <- if (!is.null(weights)) as.numeric(weights[ok]) else NULL
  if (!length(x)) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      n_eff = NA_real_
    ))
  }

  estimate <- .policy_regret_mean(x, weights)
  if (length(x) < 2L) {
    return(list(
      estimate = estimate,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      n_eff = length(x)
    ))
  }

  if (is.null(weights)) {
    n_eff <- length(x)
    se <- stats::sd(x, na.rm = TRUE) / sqrt(n_eff)
  } else {
    w_sum <- sum(weights, na.rm = TRUE)
    if (!is.finite(w_sum) || w_sum <= 0) {
      return(list(
        estimate = estimate,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        n_eff = NA_real_
      ))
    }
    w_norm <- weights / w_sum
    n_eff <- w_sum^2 / sum(weights^2, na.rm = TRUE)
    var_mean <- sum(w_norm^2 * (x - estimate)^2, na.rm = TRUE)
    if (is.finite(n_eff) && n_eff > 1) {
      var_mean <- var_mean * n_eff / (n_eff - 1)
    }
    se <- sqrt(var_mean)
  }

  z_value <- stats::qnorm(1 - ((1 - ci_level) / 2))
  list(
    estimate = estimate,
    se = se,
    ci_low = estimate - z_value * se,
    ci_high = estimate + z_value * se,
    n_eff = n_eff
  )
}
