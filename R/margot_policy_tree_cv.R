#' Cross-validated held-out policy-tree diagnostics
#'
#' @description
#' Learns shallow policy trees on training folds and evaluates their policy
#' values, selected split variables, split thresholds, and leaf-level signed
#' treatment-control contrasts on held-out folds.
#' The target is the performance of the policy-learning procedure, not the value
#' of a final full-sample display tree.
#'
#' @details
#' Let \eqn{\Gamma_{ja}} denote the action score for observation \eqn{j} under
#' action \eqn{a}. Policy-tree evaluation averages the score for the action
#' selected by the learned policy \eqn{\pi}. For binary actions \eqn{C} and
#' \eqn{T}, held-out summaries report value against all-control,
#' all-treatment, and best-constant baselines. Leaf summaries report the signed
#' held-out evaluation contrast \eqn{\Gamma_{jT} - \Gamma_{jC}} for observations
#' routed by trees learned on training folds. Selected actions are the actions
#' stored by those learned trees; held-out summaries do not reselect actions
#' from held-out means. Between-leaf differences describe variation in
#' score-contrast magnitude, not the policy decision rule itself.
#'
#' @param model_results A list returned by \code{margot_causal_forest()},
#'   \code{margot_policy_tree_stability()}, or a compatible object with
#'   \code{results}, \code{covariates}, and stored doubly robust action scores.
#' @param model_names Optional character vector of model names to process, with
#'   or without the \code{model_} prefix. Defaults to all models.
#' @param custom_covariates Optional character vector of covariates to use for
#'   policy trees.
#' @param exclude_covariates Optional character vector of covariate names or
#'   patterns to exclude.
#' @param covariate_mode Character. One of \code{"original"},
#'   \code{"custom"}, \code{"add"}, or \code{"all"}.
#' @param depths Integer vector containing 1, 2, or both. Character values
#'   \code{"1"}, \code{"2"}, and \code{"both"} are also accepted.
#' @param num_folds Integer. Number of folds per repeat. Default is 5.
#' @param n_repeats Integer. Number of repeated fold partitions. Default is 20.
#' @param weights Optional numeric vector of evaluation weights. If
#'   \code{NULL}, \code{model_results$weights} is used when available.
#' @param min_gain_for_depth_switch Numeric. Minimum held-out value gain required
#'   before depth two can be selected over depth one. Default is 0.01.
#' @param max_stability_loss_for_depth_switch Numeric. Maximum allowed loss in
#'   root-split stability before depth two is rejected. Default is 0.05.
#' @param label_mapping Optional named list mapping outcome and variable names to
#'   display labels.
#' @param seed Integer. Base seed for reproducible fold assignments.
#' @param tree_method Character. \code{"fastpolicytree"} or
#'   \code{"policytree"}.
#' @param verbose Logical. Print progress messages.
#'
#' @return A \code{margot_policy_tree_cv} list with fold-level held-out values,
#'   value summaries, split summaries, leaf summaries, threshold summaries,
#'   depth selection, and a named \code{depth_map} that can be passed to
#'   \code{margot_policy_workflow()} or
#'   \code{margot_policy_summary_compare_depths()}.
#'
#' @references
#' Athey, S., & Wager, S. (2021). Policy learning with observational data.
#' Econometrica, 89(1), 133-161.
#'
#' @export
margot_policy_tree_cv <- function(model_results,
                                  model_names = NULL,
                                  custom_covariates = NULL,
                                  exclude_covariates = NULL,
                                  covariate_mode = c("original", "custom", "add", "all"),
                                  depths = c(1L, 2L),
                                  num_folds = 5L,
                                  n_repeats = 20L,
                                  weights = NULL,
                                  min_gain_for_depth_switch = 0.01,
                                  max_stability_loss_for_depth_switch = 0.05,
                                  label_mapping = NULL,
                                  seed = 42L,
                                  tree_method = c("fastpolicytree", "policytree"),
                                  verbose = TRUE) {
  # evaluate policy-learning procedure on held-out folds and return summaries.
  if (!is.list(model_results) || is.null(model_results$results) || !is.list(model_results$results)) {
    stop("model_results must be a list with a 'results' element", call. = FALSE)
  }
  if (is.null(model_results$covariates)) {
    stop("model_results must contain covariates for held-out policy-tree CV", call. = FALSE)
  }

  covariate_mode <- match.arg(covariate_mode)
  tree_method <- match.arg(tree_method)
  actual_tree_method <- .get_tree_method(tree_method, verbose)
  depths <- .policy_cv_normalise_depths(depths)

  num_folds <- as.integer(num_folds)
  n_repeats <- as.integer(n_repeats)
  if (is.na(num_folds) || num_folds < 2L) {
    stop("num_folds must be an integer >= 2", call. = FALSE)
  }
  if (is.na(n_repeats) || n_repeats < 1L) {
    stop("n_repeats must be an integer >= 1", call. = FALSE)
  }
  if (!is.numeric(min_gain_for_depth_switch) || length(min_gain_for_depth_switch) != 1L ||
      is.na(min_gain_for_depth_switch)) {
    stop("min_gain_for_depth_switch must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(max_stability_loss_for_depth_switch) ||
      length(max_stability_loss_for_depth_switch) != 1L ||
      is.na(max_stability_loss_for_depth_switch) ||
      max_stability_loss_for_depth_switch < 0) {
    stop("max_stability_loss_for_depth_switch must be a non-negative numeric value", call. = FALSE)
  }

  model_names <- .policy_cv_resolve_model_names(model_results, model_names)

  if (isTRUE(verbose)) {
    cli::cli_h1("Held-out policy-tree cross-validation")
    cli::cli_alert_info("Processing {length(model_names)} model{?s}; folds={num_folds}; repeats={n_repeats}")
    cli::cli_alert_info("Tree method: {actual_tree_method}")
  }

  fold_rows <- list()
  split_rows <- list()
  leaf_rows <- list()

  for (model_name in model_names) {
    if (isTRUE(verbose)) cli::cli_h2("Processing {model_name}")
    model_result <- model_results$results[[model_name]]
    model_data <- .policy_cv_model_data(
      object = model_results,
      model = model_result,
      weights = weights
    )
    selected_vars <- .policy_cv_selected_vars(
      model_result = model_result,
      covariates = model_data$covariates,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      covariate_mode = covariate_mode,
      model_name = model_name,
      verbose = verbose
    )

    complete_rows <- stats::complete.cases(model_data$covariates[, selected_vars, drop = FALSE]) &
      stats::complete.cases(model_data$dr_scores)
    usable_idx <- which(complete_rows)
    if (length(usable_idx) < num_folds) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("{model_name}: fewer complete rows than folds; skipping")
      }
      next
    }

    for (repeat_id in seq_len(n_repeats)) {
      fold_id <- .policy_cv_make_folds(length(usable_idx), num_folds, seed + repeat_id)
      for (fold in seq_len(num_folds)) {
        test_pos <- usable_idx[fold_id == fold]
        train_pos <- usable_idx[fold_id != fold]
        if (length(test_pos) < 1L || length(train_pos) < 2L) next

        for (depth in depths) {
          tree <- tryCatch(
            .compute_policy_tree(
              model_data$covariates[train_pos, selected_vars, drop = FALSE],
              model_data$dr_scores[train_pos, , drop = FALSE],
              depth = depth,
              tree_method = actual_tree_method
            ),
            error = function(e) {
              if (isTRUE(verbose)) {
                cli::cli_alert_warning("{model_name} repeat {repeat_id} fold {fold} depth {depth}: {e$message}")
              }
              NULL
            }
          )
          if (is.null(tree)) next

          heldout <- .policy_cv_evaluate_tree(
            tree = tree,
            covariates = model_data$covariates[test_pos, , drop = FALSE],
            dr_scores = model_data$dr_scores[test_pos, , drop = FALSE],
            weights = if (!is.null(model_data$weights)) model_data$weights[test_pos] else NULL
          )
          if (is.null(heldout)) next

          fold_rows[[length(fold_rows) + 1L]] <- data.frame(
            model = model_name,
            outcome = gsub("^model_", "", model_name),
            outcome_label = .policy_cv_label(gsub("^model_", "", model_name), label_mapping),
            repeat_id = repeat_id,
            fold = fold,
            depth = depth,
            n_train = length(train_pos),
            n_eval = heldout$n_eval,
            coverage = heldout$coverage,
            value_policy = heldout$value_policy,
            value_control_all = heldout$value_control_all,
            value_treat_all = heldout$value_treat_all,
            value_best_constant = heldout$value_best_constant,
            best_constant_action = heldout$best_constant_action,
            gain_vs_control = heldout$gain_vs_control,
            gain_vs_treat = heldout$gain_vs_treat,
            gain_vs_best_constant = heldout$gain_vs_best_constant,
            n_selected_actions = heldout$n_selected_actions,
            uniform_selected_action = heldout$uniform_selected_action,
            stringsAsFactors = FALSE
          )

          split_info <- tryCatch(extract_tree_info(tree, tree$columns %||% selected_vars),
                                 error = function(e) NULL)
          split_df <- .policy_cv_split_rows(
            split_info = split_info,
            model_name = model_name,
            repeat_id = repeat_id,
            fold = fold,
            depth = depth,
            label_mapping = label_mapping
          )
          if (!is.null(split_df) && nrow(split_df)) {
            split_rows[[length(split_rows) + 1L]] <- split_df
          }

          leaf_df <- tryCatch(
            .policy_cv_leaf_rows(
              tree = tree,
              covariates = model_data$covariates[test_pos, , drop = FALSE],
              dr_scores = model_data$dr_scores[test_pos, , drop = FALSE],
              weights = if (!is.null(model_data$weights)) model_data$weights[test_pos] else NULL,
              model_name = model_name,
              repeat_id = repeat_id,
              fold = fold,
              depth = depth,
              label_mapping = label_mapping
            ),
            error = function(e) NULL
          )
          if (!is.null(leaf_df) && nrow(leaf_df)) {
            leaf_rows[[length(leaf_rows) + 1L]] <- leaf_df
          }
        }
      }
    }
  }

  fold_values <- if (length(fold_rows)) {
    do.call(rbind, fold_rows)
  } else {
    data.frame()
  }
  split_values <- if (length(split_rows)) {
    do.call(rbind, split_rows)
  } else {
    data.frame()
  }
  leaf_values <- if (length(leaf_rows)) {
    do.call(rbind, leaf_rows)
  } else {
    data.frame()
  }

  value_summary <- .policy_cv_value_summary(fold_values)
  split_summary <- .policy_cv_split_summary(split_values, fold_values)
  leaf_summary <- .policy_cv_leaf_summary(leaf_values)
  threshold_summary <- .policy_cv_threshold_summary(split_values)
  depth_selection <- .policy_cv_select_depths(
    value_summary = value_summary,
    split_summary = split_summary,
    min_gain_for_depth_switch = min_gain_for_depth_switch,
    max_stability_loss_for_depth_switch = max_stability_loss_for_depth_switch
  )
  depth_map <- if (nrow(depth_selection)) {
    stats::setNames(depth_selection$selected_depth, depth_selection$model)
  } else {
    stats::setNames(integer(), character())
  }

  out <- list(
    fold_values = fold_values,
    value_summary = value_summary,
    split_values = split_values,
    split_summary = split_summary,
    leaf_values = leaf_values,
    leaf_summary = leaf_summary,
    threshold_summary = threshold_summary,
    depth_selection = depth_selection,
    depth_map = depth_map,
    metadata = list(
      num_folds = num_folds,
      n_repeats = n_repeats,
      depths = depths,
      tree_method = actual_tree_method,
      seed = seed,
      min_gain_for_depth_switch = min_gain_for_depth_switch,
      max_stability_loss_for_depth_switch = max_stability_loss_for_depth_switch,
      estimand = "held-out evaluation of the policy-learning procedure"
    )
  )
  class(out) <- c("margot_policy_tree_cv", "list")
  out
}

#' @keywords internal
.policy_cv_normalise_depths <- function(depths) {
  # normalise depth input to the supported integer vector.
  if (is.character(depths)) {
    if (length(depths) == 1L && depths == "both") return(c(1L, 2L))
    depths <- suppressWarnings(as.integer(depths))
  } else {
    depths <- as.integer(depths)
  }
  if (!length(depths) || anyNA(depths) || any(!depths %in% c(1L, 2L))) {
    stop("depths must contain 1, 2, or both", call. = FALSE)
  }
  unique(depths)
}

#' @keywords internal
.policy_cv_resolve_model_names <- function(object, model_names = NULL) {
  # resolve optional bare outcome names to stored model ids.
  available <- names(object$results)
  if (is.null(available) || !length(available)) {
    stop("object$results must be a named list", call. = FALSE)
  }
  if (is.null(model_names)) return(available)
  model_names <- as.character(model_names)
  available_clean <- gsub("^model_", "", available)
  lookup <- stats::setNames(available, available)
  lookup <- c(lookup, stats::setNames(available, available_clean))
  lookup <- lookup[!duplicated(names(lookup))]
  unknown <- setdiff(model_names, names(lookup))
  if (length(unknown)) {
    stop("Unknown model name(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  unname(lookup[model_names])
}

#' @keywords internal
.policy_cv_model_data <- function(object, model, weights = NULL) {
  # align covariates, doubly robust scores, and optional weights to one row set.
  dr_scores <- model$dr_scores
  if (is.null(dr_scores)) dr_scores <- model$dr_scores_flipped
  if (is.null(dr_scores)) stop("model is missing dr_scores", call. = FALSE)
  dr_scores <- as.matrix(dr_scores)
  covariates <- as.data.frame(object$covariates)
  not_missing <- object$not_missing

  if (!is.null(not_missing) && is.logical(not_missing)) {
    if (length(not_missing) == nrow(covariates)) {
      not_missing <- which(not_missing)
    } else if (length(not_missing) == nrow(dr_scores) && nrow(covariates) == nrow(dr_scores)) {
      not_missing <- which(not_missing)
    } else {
      stop("logical not_missing cannot be aligned with covariates and dr_scores", call. = FALSE)
    }
  }

  if (nrow(covariates) == nrow(dr_scores)) {
    row_index <- seq_len(nrow(covariates))
  } else if (!is.null(not_missing) &&
             length(not_missing) == nrow(dr_scores) &&
             length(not_missing) > 0L &&
             max(not_missing) <= nrow(covariates)) {
    row_index <- not_missing
    covariates <- covariates[row_index, , drop = FALSE]
  } else if (nrow(covariates) > nrow(dr_scores)) {
    row_index <- seq_len(nrow(dr_scores))
    covariates <- covariates[row_index, , drop = FALSE]
  } else {
    stop("could not align covariates with dr_scores", call. = FALSE)
  }

  weights <- weights %||% object$weights %||% NULL
  eval_weights <- NULL
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (length(weights) == nrow(covariates)) {
      eval_weights <- weights
    } else if (length(weights) >= max(row_index)) {
      eval_weights <- weights[row_index]
    }
  }

  list(
    covariates = covariates,
    dr_scores = dr_scores,
    weights = eval_weights,
    row_index = row_index
  )
}

#' @keywords internal
.policy_cv_selected_vars <- function(model_result,
                                     covariates,
                                     custom_covariates,
                                     exclude_covariates,
                                     covariate_mode,
                                     model_name,
                                     verbose) {
  # choose policy-tree covariates using the same modes as stability analysis.
  all_covars <- colnames(covariates)
  if (covariate_mode == "all") {
    selected_vars <- all_covars
  } else if (covariate_mode == "add" && !is.null(custom_covariates)) {
    existing_top_vars <- model_result$top_vars
    selected_vars <- if (is.null(existing_top_vars)) custom_covariates else unique(c(existing_top_vars, custom_covariates))
  } else if (covariate_mode == "custom" && !is.null(custom_covariates)) {
    selected_vars <- custom_covariates
  } else {
    selected_vars <- model_result$top_vars
    if (is.null(selected_vars)) {
      stop("No top_vars found for ", model_name, " and no custom_covariates supplied", call. = FALSE)
    }
  }

  missing <- setdiff(selected_vars, all_covars)
  if (length(missing)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning("{model_name}: dropping unavailable covariate(s): {paste(missing, collapse = ', ')}")
    }
    selected_vars <- intersect(selected_vars, all_covars)
  }
  if (!is.null(exclude_covariates)) {
    selected_vars <- apply_covariate_exclusions(selected_vars, exclude_covariates, verbose)
  }
  if (!length(selected_vars)) {
    stop("No covariates remain for ", model_name, call. = FALSE)
  }
  selected_vars
}

#' @keywords internal
.policy_cv_make_folds <- function(n, num_folds, seed) {
  # make a balanced random fold vector for one repeat.
  set.seed(seed)
  sample(rep(seq_len(num_folds), length.out = n))
}

#' @keywords internal
.policy_cv_evaluate_tree <- function(tree, covariates, dr_scores, weights = NULL) {
  # evaluate one trained tree on held-out rows with aligned DR action scores.
  if (is.null(tree$columns) || !all(tree$columns %in% colnames(covariates))) return(NULL)
  X <- as.data.frame(covariates[, tree$columns, drop = FALSE])
  keep <- stats::complete.cases(X) & stats::complete.cases(dr_scores)
  if (!any(keep)) return(NULL)
  X <- X[keep, , drop = FALSE]
  dr_scores <- as.matrix(dr_scores[keep, , drop = FALSE])
  if (ncol(dr_scores) != 2L) return(NULL)
  action_columns <- tryCatch(
    .margot_policy_binary_action_columns(
      dr_scores = dr_scores,
      tree = tree,
      context = "margot_policy_tree_cv()"
    ),
    error = function(e) NULL
  )
  if (is.null(action_columns)) return(NULL)
  weights <- if (!is.null(weights)) weights[keep] else NULL
  if (nrow(X) < 1L || ncol(dr_scores) < 2L) return(NULL)

  actions <- tryCatch(stats::predict(tree, X, type = "action.id"), error = function(e) NULL)
  if (is.null(actions)) return(NULL)
  if (is.matrix(actions)) actions <- actions[, 1]
  actions <- .normalize_policy_actions(actions)
  ok_actions <- is.finite(actions) & actions >= 1L & actions <= ncol(dr_scores)
  if (!any(ok_actions)) return(NULL)
  actions <- actions[ok_actions]
  dr_scores <- dr_scores[ok_actions, , drop = FALSE]
  weights <- if (!is.null(weights)) weights[ok_actions] else NULL

  policy_score <- dr_scores[cbind(seq_along(actions), actions)]
  control_score <- dr_scores[, action_columns$control]
  treated_score <- dr_scores[, action_columns$treatment]

  value_policy <- .policy_cv_mean(policy_score, weights)
  value_control_all <- .policy_cv_mean(control_score, weights)
  value_treat_all <- .policy_cv_mean(treated_score, weights)
  constant_values <- c(value_control_all, value_treat_all)
  value_best_constant <- if (all(is.na(constant_values))) {
    NA_real_
  } else {
    max(constant_values, na.rm = TRUE)
  }
  best_constant_action <- if (all(is.na(constant_values))) {
    NA_character_
  } else if (isTRUE(all.equal(value_control_all, value_treat_all))) {
    "tie"
  } else if (!is.na(value_treat_all) &&
             (is.na(value_control_all) || value_treat_all > value_control_all)) {
    "treated"
  } else {
    "control"
  }
  n_selected_actions <- length(unique(stats::na.omit(actions)))

  list(
    n_eval = length(actions),
    coverage = .policy_cv_mean(actions == action_columns$treatment, weights),
    value_policy = value_policy,
    value_control_all = value_control_all,
    value_treat_all = value_treat_all,
    value_best_constant = value_best_constant,
    best_constant_action = best_constant_action,
    gain_vs_control = value_policy - value_control_all,
    gain_vs_treat = value_policy - value_treat_all,
    gain_vs_best_constant = value_policy - value_best_constant,
    n_selected_actions = n_selected_actions,
    uniform_selected_action = n_selected_actions <= 1L
  )
}

#' @keywords internal
.policy_cv_mean <- function(x, weights = NULL) {
  # compute a weighted mean after removing invalid rows.
  ok <- is.finite(x)
  if (!is.null(weights)) {
    ok <- ok & is.finite(weights) & weights > 0
    if (any(ok)) return(stats::weighted.mean(x[ok], weights[ok]))
  }
  mean(x[ok], na.rm = TRUE)
}

#' @keywords internal
.policy_cv_split_rows <- function(split_info, model_name, repeat_id, fold, depth, label_mapping = NULL) {
  # convert split information for one tree into a tabular audit trail.
  if (is.null(split_info) || is.null(split_info$splits) || !length(split_info$splits)) {
    return(NULL)
  }
  rows <- lapply(split_info$splits, function(split) {
    if (is.null(split$var_name)) return(NULL)
    data.frame(
      model = model_name,
      outcome = gsub("^model_", "", model_name),
      outcome_label = .policy_cv_label(gsub("^model_", "", model_name), label_mapping),
      repeat_id = repeat_id,
      fold = fold,
      depth = depth,
      node_id = split$node_id,
      variable = split$var_name,
      variable_label = .policy_cv_label(split$var_name, label_mapping),
      threshold = split$threshold,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) do.call(rbind, rows) else NULL
}

#' @keywords internal
.policy_cv_leaf_rows <- function(tree,
                                 covariates,
                                 dr_scores,
                                 weights = NULL,
                                 model_name,
                                 repeat_id,
                                 fold,
                                 depth,
                                 label_mapping = NULL) {
  # summarise signed leaf contrasts on held-out rows for one tree.
  if (is.null(tree$columns) || !all(tree$columns %in% colnames(covariates))) return(NULL)
  X <- as.data.frame(covariates[, tree$columns, drop = FALSE])
  keep <- stats::complete.cases(X) & stats::complete.cases(dr_scores)
  if (!any(keep)) return(NULL)
  X <- X[keep, , drop = FALSE]
  dr_scores <- as.matrix(dr_scores[keep, , drop = FALSE])
  action_columns <- tryCatch(
    .margot_policy_binary_action_columns(
      dr_scores = dr_scores,
      tree = tree,
      context = "margot_policy_tree_cv()"
    ),
    error = function(e) NULL
  )
  if (is.null(action_columns)) return(NULL)
  weights <- if (!is.null(weights)) weights[keep] else NULL
  leaf_ids <- .margot_policy_tree_leaf_ids(tree, X)
  ok <- is.finite(leaf_ids)
  if (!any(ok)) return(NULL)
  X <- X[ok, , drop = FALSE]
  dr_scores <- dr_scores[ok, , drop = FALSE]
  weights <- if (!is.null(weights)) weights[ok] else NULL
  leaf_ids <- leaf_ids[ok]
  total_weight <- if (!is.null(weights)) {
    sum(weights[is.finite(weights) & weights > 0], na.rm = TRUE)
  } else {
    length(leaf_ids)
  }
  if (!is.finite(total_weight) || total_weight <= 0) return(NULL)

  effect <- as.numeric(dr_scores[, action_columns$treatment] - dr_scores[, action_columns$control])
  action_names <- action_columns$action_names
  rows <- lapply(sort(unique(leaf_ids)), function(leaf_id) {
    idx <- which(leaf_ids == leaf_id)
    node <- tree$nodes[[leaf_id]]
    action_id <- as.integer(node$action)
    if (!is.finite(action_id) || action_id < 1L || action_id > length(action_names)) return(NULL)
    w <- if (!is.null(weights)) weights[idx] else rep(1, length(idx))
    share <- sum(w, na.rm = TRUE) / total_weight
    tc_interval <- .margot_policy_leaf_interval(effect[idx], w, ci_level = 0.95)
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
    data.frame(
      model = model_name,
      outcome = gsub("^model_", "", model_name),
      outcome_label = .policy_cv_label(gsub("^model_", "", model_name), label_mapping),
      repeat_id = repeat_id,
      fold = fold,
      depth = depth,
      node_id = leaf_id,
      action_id = action_id,
      action = action_names[[action_id]],
      action_label = .margot_leaf_label_action(action_names[[action_id]], label_mapping),
      contrast = contrast,
      score_contrast = score_contrast,
      advantage_comparison = advantage_comparison,
      n_eval_leaf = length(idx),
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
      value_contribution_vs_control = if (selected_treatment) share * estimated_advantage else 0,
      value_contribution_vs_treatment = if (!selected_treatment) share * estimated_advantage else 0,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) do.call(rbind, rows) else NULL
}

#' @keywords internal
.policy_cv_value_summary <- function(fold_values) {
  # summarise held-out policy values by model and depth.
  if (is.null(fold_values) || !nrow(fold_values)) return(data.frame())
  groups <- split(fold_values, interaction(fold_values$model, fold_values$depth, drop = TRUE))
  rows <- lapply(groups, function(df) {
    value_policy_mean <- stats::weighted.mean(df$value_policy, df$n_eval, na.rm = TRUE)
    value_control_all_mean <- stats::weighted.mean(df$value_control_all, df$n_eval, na.rm = TRUE)
    value_treat_all_mean <- stats::weighted.mean(df$value_treat_all, df$n_eval, na.rm = TRUE)
    constant_values <- c(value_control_all_mean, value_treat_all_mean)
    value_best_constant_mean <- if (all(is.na(constant_values))) {
      NA_real_
    } else {
      max(constant_values, na.rm = TRUE)
    }
    best_constant_action <- if (all(is.na(constant_values))) {
      NA_character_
    } else if (isTRUE(all.equal(value_control_all_mean, value_treat_all_mean))) {
      "tie"
    } else if (!is.na(value_treat_all_mean) &&
               (is.na(value_control_all_mean) || value_treat_all_mean > value_control_all_mean)) {
      "treated"
    } else {
      "control"
    }
    data.frame(
      model = df$model[1],
      outcome = df$outcome[1],
      outcome_label = df$outcome_label[1],
      depth = df$depth[1],
      n_folds = nrow(df),
      n_eval = sum(df$n_eval, na.rm = TRUE),
      coverage_mean = stats::weighted.mean(df$coverage, df$n_eval, na.rm = TRUE),
      value_policy_mean = value_policy_mean,
      value_control_all_mean = value_control_all_mean,
      value_treat_all_mean = value_treat_all_mean,
      value_best_constant_mean = value_best_constant_mean,
      best_constant_action = best_constant_action,
      gain_vs_control_mean = stats::weighted.mean(df$gain_vs_control, df$n_eval, na.rm = TRUE),
      gain_vs_control_sd = stats::sd(df$gain_vs_control, na.rm = TRUE),
      gain_vs_control_q025 = stats::quantile(df$gain_vs_control, 0.025, na.rm = TRUE, names = FALSE),
      gain_vs_control_q975 = stats::quantile(df$gain_vs_control, 0.975, na.rm = TRUE, names = FALSE),
      gain_vs_treat_mean = stats::weighted.mean(df$gain_vs_treat, df$n_eval, na.rm = TRUE),
      gain_vs_treat_sd = stats::sd(df$gain_vs_treat, na.rm = TRUE),
      gain_vs_best_constant_mean = value_policy_mean - value_best_constant_mean,
      gain_vs_best_constant_sd = stats::sd(df$gain_vs_best_constant, na.rm = TRUE),
      n_selected_actions_max = max(df$n_selected_actions, na.rm = TRUE),
      uniform_selected_action_all = all(df$uniform_selected_action),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$model, out$depth), , drop = FALSE]
}

#' @keywords internal
.policy_cv_split_summary <- function(split_values, fold_values) {
  # summarise split-variable frequencies by model, depth, node, and variable.
  if (is.null(split_values) || !nrow(split_values)) return(data.frame())
  total_trees <- unique(fold_values[, c("model", "depth", "repeat_id", "fold"), drop = FALSE])
  total_key <- split(total_trees, interaction(total_trees$model, total_trees$depth, drop = TRUE))
  totals <- vapply(total_key, nrow, integer(1))
  groups <- split(
    split_values,
    interaction(split_values$model, split_values$depth, split_values$node_id, split_values$variable, drop = TRUE)
  )
  rows <- lapply(groups, function(df) {
    total_name <- paste(df$model[1], df$depth[1], sep = ".")
    denom <- totals[[total_name]]
    if (is.null(denom) || is.na(denom) || denom < 1L) denom <- length(unique(paste(df$repeat_id, df$fold)))
    data.frame(
      model = df$model[1],
      outcome = df$outcome[1],
      outcome_label = df$outcome_label[1],
      depth = df$depth[1],
      node_id = df$node_id[1],
      variable = df$variable[1],
      variable_label = df$variable_label[1],
      n_selected = nrow(df),
      selection_frequency = nrow(df) / denom,
      threshold_mean = mean(df$threshold, na.rm = TRUE),
      threshold_sd = stats::sd(df$threshold, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$model, out$depth, out$node_id, -out$selection_frequency), , drop = FALSE]
}

#' @keywords internal
.policy_cv_leaf_summary <- function(leaf_values) {
  # summarise held-out leaf contrasts by model, depth, action, and contrast.
  if (is.null(leaf_values) || !nrow(leaf_values)) return(data.frame())
  if (!"score_contrast" %in% names(leaf_values)) {
    leaf_values$score_contrast <- "treatment_minus_control"
  }
  groups <- split(
    leaf_values,
    interaction(
      leaf_values$model,
      leaf_values$depth,
      leaf_values$action,
      leaf_values$contrast,
      leaf_values$score_contrast,
      drop = TRUE
    )
  )
  rows <- lapply(groups, function(df) {
    weights <- df$n_eval_leaf
    data.frame(
      model = df$model[1],
      outcome = df$outcome[1],
      outcome_label = df$outcome_label[1],
      depth = df$depth[1],
      action = df$action[1],
      action_label = df$action_label[1],
      contrast = df$contrast[1],
      score_contrast = df$score_contrast[1],
      advantage_comparison = df$advantage_comparison[1],
      n_leaves = nrow(df),
      n_eval_leaf = sum(df$n_eval_leaf, na.rm = TRUE),
      sample_share_mean = stats::weighted.mean(df$sample_share, weights, na.rm = TRUE),
      sample_share_sd = stats::sd(df$sample_share, na.rm = TRUE),
      treatment_control_contrast_mean = stats::weighted.mean(df$treatment_control_contrast, weights, na.rm = TRUE),
      treatment_control_contrast_sd = stats::sd(df$treatment_control_contrast, na.rm = TRUE),
      treatment_control_contrast_q025 = stats::quantile(df$treatment_control_contrast, 0.025, na.rm = TRUE, names = FALSE),
      treatment_control_contrast_q975 = stats::quantile(df$treatment_control_contrast, 0.975, na.rm = TRUE, names = FALSE),
      estimated_treatment_contrast_mean = stats::weighted.mean(df$estimated_treatment_contrast, weights, na.rm = TRUE),
      estimated_treatment_contrast_sd = stats::sd(df$estimated_treatment_contrast, na.rm = TRUE),
      estimated_advantage_mean = stats::weighted.mean(df$estimated_advantage, weights, na.rm = TRUE),
      estimated_advantage_sd = stats::sd(df$estimated_advantage, na.rm = TRUE),
      estimated_gain_mean = stats::weighted.mean(df$estimated_gain, weights, na.rm = TRUE),
      estimated_gain_sd = stats::sd(df$estimated_gain, na.rm = TRUE),
      value_contribution_vs_control_mean = mean(df$value_contribution_vs_control, na.rm = TRUE),
      value_contribution_vs_treatment_mean = mean(df$value_contribution_vs_treatment, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$model, out$depth, out$action), , drop = FALSE]
}

#' @keywords internal
.policy_cv_threshold_summary <- function(split_values) {
  # summarise threshold distributions for selected split variables.
  if (is.null(split_values) || !nrow(split_values)) return(data.frame())
  groups <- split(
    split_values,
    interaction(split_values$model, split_values$depth, split_values$node_id, split_values$variable, drop = TRUE)
  )
  rows <- lapply(groups, function(df) {
    data.frame(
      model = df$model[1],
      depth = df$depth[1],
      node_id = df$node_id[1],
      variable = df$variable[1],
      variable_label = df$variable_label[1],
      threshold_mean = mean(df$threshold, na.rm = TRUE),
      threshold_sd = stats::sd(df$threshold, na.rm = TRUE),
      threshold_q025 = stats::quantile(df$threshold, 0.025, na.rm = TRUE, names = FALSE),
      threshold_q500 = stats::quantile(df$threshold, 0.5, na.rm = TRUE, names = FALSE),
      threshold_q975 = stats::quantile(df$threshold, 0.975, na.rm = TRUE, names = FALSE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$model, out$depth, out$node_id, out$variable), , drop = FALSE]
}

#' @keywords internal
.policy_cv_select_depths <- function(value_summary,
                                     split_summary,
                                     min_gain_for_depth_switch,
                                     max_stability_loss_for_depth_switch) {
  # apply the depth rule: depth one unless depth two improves value and stability.
  if (is.null(value_summary) || !nrow(value_summary)) return(data.frame())
  models <- unique(value_summary$model)
  rows <- lapply(models, function(model_name) {
    sub <- value_summary[value_summary$model == model_name, , drop = FALSE]
    d1 <- sub[sub$depth == 1L, , drop = FALSE]
    d2 <- sub[sub$depth == 2L, , drop = FALSE]
    if (!nrow(d1) && !nrow(d2)) return(NULL)
    if (!nrow(d1)) {
      selected <- 2L
      reason <- "depth one unavailable"
      delta <- NA_real_
      stable_ok <- TRUE
    } else if (!nrow(d2)) {
      selected <- 1L
      reason <- "depth two unavailable"
      delta <- NA_real_
      stable_ok <- TRUE
    } else {
      delta <- d2$gain_vs_control_mean[1] - d1$gain_vs_control_mean[1]
      root1 <- .policy_cv_root_stability(split_summary, model_name, 1L)
      root2 <- .policy_cv_root_stability(split_summary, model_name, 2L)
      stability_loss <- root1 - root2
      stable_ok <- is.na(stability_loss) || stability_loss <= max_stability_loss_for_depth_switch
      selected <- if (is.finite(delta) && delta > min_gain_for_depth_switch && isTRUE(stable_ok)) 2L else 1L
      reason <- if (selected == 2L) {
        "depth two clears held-out value and stability thresholds"
      } else if (!isTRUE(stable_ok)) {
        "depth two loses too much root-split stability"
      } else if (is.finite(delta) && delta > 0) {
        "depth two gain below material-improvement threshold"
      } else {
        "depth one has equal or higher held-out value"
      }
    }
    data.frame(
      model = model_name,
      outcome = sub$outcome[1],
      outcome_label = sub$outcome_label[1],
      selected_depth = selected,
      pv_depth1 = if (nrow(d1)) d1$gain_vs_control_mean[1] else NA_real_,
      pv_depth2 = if (nrow(d2)) d2$gain_vs_control_mean[1] else NA_real_,
      depth2_minus_depth1 = if (exists("delta")) delta else NA_real_,
      depth1_root_stability = .policy_cv_root_stability(split_summary, model_name, 1L),
      depth2_root_stability = .policy_cv_root_stability(split_summary, model_name, 2L),
      stability_ok = stable_ok,
      reason = reason,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  } else {
    data.frame()
  }
}

#' @keywords internal
.policy_cv_root_stability <- function(split_summary, model_name, depth) {
  # return the largest root-variable selection frequency for one model/depth.
  if (is.null(split_summary) || !nrow(split_summary)) return(NA_real_)
  root <- split_summary[split_summary$model == model_name &
                          split_summary$depth == depth &
                          split_summary$node_id == 1L, , drop = FALSE]
  if (!nrow(root)) return(NA_real_)
  max(root$selection_frequency, na.rm = TRUE)
}

#' @keywords internal
.policy_cv_label <- function(x, label_mapping = NULL) {
  # apply labels consistently while falling back to raw names.
  tryCatch(.apply_label_stability(x, label_mapping), error = function(e) x)
}

#' Print held-out policy-tree CV results
#'
#' @param x A \code{margot_policy_tree_cv} object.
#' @param ... Additional arguments.
#' @return Invisibly returns \code{x}.
#' @export
print.margot_policy_tree_cv <- function(x, ...) {
  cat("Held-out Policy-tree Cross-validation\n")
  cat("=====================================\n\n")
  cat("Models:", length(x$depth_map), "\n")
  cat("Folds:", x$metadata$num_folds, "\n")
  cat("Repeats:", x$metadata$n_repeats, "\n")
  if (!is.null(x$depth_selection) && nrow(x$depth_selection)) {
    cat("\nDepth selection:\n")
    print(x$depth_selection[, c("model", "selected_depth", "depth2_minus_depth1", "reason"), drop = FALSE])
  }
  invisible(x)
}
