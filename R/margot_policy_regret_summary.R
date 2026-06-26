#' Summarise Policy-Tree Value and Oracle Regret
#'
#' @description
#' Computes policy-tree value contrasts from stored doubly robust action scores.
#' The summary compares a learned policy tree with universal control,
#' universal treatment, and an oracle that chooses the better action score for
#' each observation. The oracle is an optimistic benchmark for describing
#' remaining model-implied heterogeneity; it is not a deployable policy.
#'
#' @param object A list returned by \code{\link{margot_causal_forest}},
#'   \code{\link{margot_policy_tree}}, or a compatible policy-tree object with
#'   \code{results} and either top-level \code{covariates} or per-model
#'   \code{plot_data}.
#' @param model_names Optional character vector of model names to include, with
#'   or without the \code{model_} prefix. Defaults to all available models.
#' @param depths Integer vector of tree depths to evaluate. Defaults to
#'   \code{c(1, 2)}.
#' @param weights Optional numeric vector of evaluation weights. If \code{NULL},
#'   the function uses \code{object$weights} when available.
#' @param R Integer. Bootstrap replicates for uncertainty intervals. Defaults to
#'   499.
#' @param seed Integer or \code{NULL}. Seed for bootstrap reproducibility.
#' @param ci_level Numeric confidence level for bootstrap percentile intervals.
#'   Defaults to 0.95.
#' @param label_mapping Optional named character vector for display labels.
#'
#' @return A tibble with one row per model and depth, including policy value,
#'   universal-action values, oracle value, gains relative to control-all and
#'   treat-all, and regret relative to the oracle benchmark.
#'
#' @examples
#' \dontrun{
#' policy_regret_summary(causal_forest_results, depths = 2)
#' margot_policy_regret_summary(policy_tree_results, model_names = "model_y")
#' }
#'
#' @export
#' @importFrom stats predict quantile sd weighted.mean
margot_policy_regret_summary <- function(object,
                                         model_names = NULL,
                                         depths = c(1L, 2L),
                                         weights = NULL,
                                         R = 499L,
                                         seed = 42L,
                                         ci_level = 0.95,
                                         label_mapping = NULL) {
  # validate the minimal policy-tree result contract.
  if (!is.list(object) || is.null(object$results) || !is.list(object$results)) {
    stop("object must be a list with a 'results' element", call. = FALSE)
  }

  # keep depth handling explicit because absent trees are skipped quietly.
  depths <- as.integer(depths)
  if (!length(depths) || anyNA(depths) || any(!depths %in% c(1L, 2L))) {
    stop("depths must contain 1, 2, or both", call. = FALSE)
  }
  depths <- unique(depths)

  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R < 1L) {
    stop("R must be a positive integer", call. = FALSE)
  }
  R <- as.integer(R)

  if (!is.numeric(ci_level) || length(ci_level) != 1L ||
      is.na(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be a single number between 0 and 1", call. = FALSE)
  }

  model_names <- .policy_regret_resolve_model_names(object, model_names)
  weights <- weights %||% object$weights %||% NULL

  rows <- list()
  for (model_name in model_names) {
    model <- object$results[[model_name]]
    for (depth in depths) {
      tree <- model[[paste0("policy_tree_depth_", depth)]]
      if (is.null(tree)) next

      eval_data <- .policy_regret_evaluation_data(object, model, tree, weights)
      if (is.null(eval_data)) next

      rows[[length(rows) + 1L]] <- .policy_regret_one_tree(
        model = model,
        model_name = model_name,
        tree = tree,
        depth = depth,
        covariates = eval_data$covariates,
        dr_scores = eval_data$dr_scores,
        weights = eval_data$weights,
        R = R,
        seed = seed,
        ci_level = ci_level,
        label_mapping = label_mapping
      )
    }
  }

  out <- if (length(rows)) {
    dplyr::bind_rows(rows)
  } else {
    tibble::tibble()
  }

  class(out) <- c("margot_policy_regret_summary", class(out))
  out
}

#' @rdname margot_policy_regret_summary
#' @export
policy_regret_summary <- function(object,
                                  model_names = NULL,
                                  depths = c(1L, 2L),
                                  weights = NULL,
                                  R = 499L,
                                  seed = 42L,
                                  ci_level = 0.95,
                                  label_mapping = NULL) {
  # provide the user-facing shorthand requested in analysis scripts.
  margot_policy_regret_summary(
    object = object,
    model_names = model_names,
    depths = depths,
    weights = weights,
    R = R,
    seed = seed,
    ci_level = ci_level,
    label_mapping = label_mapping
  )
}

#' @keywords internal
.policy_regret_resolve_model_names <- function(object, model_names = NULL) {
  # resolve optional bare outcome names to stored model ids.
  available <- names(object$results)
  if (is.null(available) || !length(available)) {
    stop("object$results must be a named list", call. = FALSE)
  }

  if (is.null(model_names)) return(available)

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
.policy_regret_evaluation_data <- function(object, model, tree, weights = NULL) {
  # prefer full covariates when available so values use the complete fitted sample.
  dr_scores <- model$dr_scores %||% model$dr_scores_flipped %||% NULL
  if (is.null(dr_scores) || is.null(tree$columns)) return(NULL)
  dr_scores <- as.matrix(dr_scores)

  covariates <- object$covariates %||% NULL
  row_index <- NULL

  if (!is.null(covariates)) {
    if (nrow(covariates) == nrow(dr_scores)) {
      row_index <- seq_len(nrow(dr_scores))
    } else if (!is.null(object$not_missing) &&
               length(object$not_missing) == nrow(dr_scores) &&
               max(object$not_missing) <= nrow(covariates)) {
      covariates <- covariates[object$not_missing, , drop = FALSE]
      row_index <- object$not_missing
    } else if (!is.null(model$plot_data$test_indices) &&
               length(model$plot_data$test_indices) > 0L &&
               max(model$plot_data$test_indices) <= nrow(covariates)) {
      row_index <- model$plot_data$test_indices
      covariates <- covariates[row_index, , drop = FALSE]
      if (nrow(dr_scores) >= max(row_index)) {
        dr_scores <- dr_scores[row_index, , drop = FALSE]
      }
    }
  } else if (!is.null(model$plot_data$X_test_full)) {
    covariates <- model$plot_data$X_test_full
    row_index <- model$plot_data$test_indices %||% seq_len(nrow(covariates))
  } else if (!is.null(model$plot_data$X_test)) {
    covariates <- model$plot_data$X_test
    row_index <- model$plot_data$test_indices %||% seq_len(nrow(covariates))
  }

  if (is.null(covariates)) return(NULL)

  if (nrow(dr_scores) != nrow(covariates)) {
    if (!is.null(row_index) && nrow(dr_scores) >= max(row_index)) {
      dr_scores <- dr_scores[row_index, , drop = FALSE]
    } else {
      stop("could not align dr_scores with policy-tree covariates", call. = FALSE)
    }
  }

  eval_weights <- .policy_regret_align_weights(weights, row_index, nrow(covariates))
  keep <- stats::complete.cases(covariates[, tree$columns, drop = FALSE]) &
    stats::complete.cases(dr_scores)

  if (!any(keep)) return(NULL)

  list(
    covariates = covariates[keep, , drop = FALSE],
    dr_scores = dr_scores[keep, , drop = FALSE],
    weights = if (!is.null(eval_weights)) eval_weights[keep] else NULL
  )
}

#' @keywords internal
.policy_regret_align_weights <- function(weights, row_index, n) {
  # align optional weights to the rows used for tree evaluation.
  if (is.null(weights)) return(NULL)
  weights <- as.numeric(weights)

  if (length(weights) == n) return(weights)
  if (!is.null(row_index) && length(weights) >= max(row_index)) {
    aligned <- weights[row_index]
    if (length(aligned) == n) return(aligned)
  }

  warning("weights could not be aligned with the policy-regret evaluation rows; ignoring weights", call. = FALSE)
  NULL
}

#' @keywords internal
.policy_regret_one_tree <- function(model,
                                    model_name,
                                    tree,
                                    depth,
                                    covariates,
                                    dr_scores,
                                    weights,
                                    R,
                                    seed,
                                    ci_level,
                                    label_mapping) {
  # compute observed tree actions and compare them with universal and oracle rules.
  actions <- .normalize_policy_actions(stats::predict(
    tree,
    covariates[, tree$columns, drop = FALSE],
    type = "action.id"
  ))

  if (length(actions) != nrow(dr_scores)) {
    stop("policy predictions do not align with dr_scores", call. = FALSE)
  }
  if (anyNA(actions) || any(actions < 1L) || any(actions > ncol(dr_scores))) {
    stop("policy tree produced invalid action ids", call. = FALSE)
  }

  action_columns <- .margot_policy_binary_action_columns(
    dr_scores = dr_scores,
    tree = tree,
    context = "margot_policy_regret_summary()"
  )
  control_score <- dr_scores[, action_columns$control]
  treated_score <- dr_scores[, action_columns$treatment]
  policy_score <- dr_scores[cbind(seq_along(actions), actions)]
  oracle_score <- apply(dr_scores, 1L, max)
  oracle_action <- max.col(dr_scores, ties.method = "first")

  gain_control <- .policy_regret_bootstrap_mean(
    policy_score - control_score,
    weights,
    R = R,
    seed = if (is.null(seed)) NULL else seed + depth,
    ci_level = ci_level
  )
  gain_treat <- .policy_regret_bootstrap_mean(
    policy_score - treated_score,
    weights,
    R = R,
    seed = if (is.null(seed)) NULL else seed + 10L + depth,
    ci_level = ci_level
  )
  regret_oracle <- .policy_regret_bootstrap_mean(
    oracle_score - policy_score,
    weights,
    R = R,
    seed = if (is.null(seed)) NULL else seed + 20L + depth,
    ci_level = ci_level
  )

  outcome <- gsub("^model_", "", model_name)
  outcome_label <- suppressMessages(transform_var_name(outcome, label_mapping))
  split_variables <- .policy_regret_split_variables(tree)

  tibble::tibble(
    model = model_name,
    outcome = outcome,
    outcome_label = outcome_label,
    depth = as.integer(depth),
    n = length(actions),
    treat_share = .policy_regret_mean(actions == action_columns$treatment, weights),
    oracle_treat_share = .policy_regret_mean(oracle_action == action_columns$treatment, weights),
    oracle_disagreement = .policy_regret_mean(actions != oracle_action, weights),
    value_policy = .policy_regret_mean(policy_score, weights),
    value_control_all = .policy_regret_mean(control_score, weights),
    value_treat_all = .policy_regret_mean(treated_score, weights),
    value_oracle = .policy_regret_mean(oracle_score, weights),
    gain_vs_control = gain_control$estimate,
    gain_vs_control_se = gain_control$se,
    gain_vs_control_lower = gain_control$ci_lower,
    gain_vs_control_upper = gain_control$ci_upper,
    gain_vs_treat = gain_treat$estimate,
    gain_vs_treat_se = gain_treat$se,
    gain_vs_treat_lower = gain_treat$ci_lower,
    gain_vs_treat_upper = gain_treat$ci_upper,
    regret_vs_oracle = regret_oracle$estimate,
    regret_vs_oracle_se = regret_oracle$se,
    regret_vs_oracle_lower = regret_oracle$ci_lower,
    regret_vs_oracle_upper = regret_oracle$ci_upper,
    split_variables = paste(split_variables, collapse = "; ")
  )
}

#' @keywords internal
.policy_regret_mean <- function(x, weights = NULL) {
  # compute a weighted mean after dropping unusable rows.
  ok <- is.finite(x)
  if (!is.null(weights)) {
    ok <- ok & is.finite(weights) & weights > 0
    if (any(ok)) return(stats::weighted.mean(x[ok], weights[ok]))
  }
  mean(x[ok], na.rm = TRUE)
}

#' @keywords internal
.policy_regret_bootstrap_mean <- function(x,
                                          weights = NULL,
                                          R = 499L,
                                          seed = 42L,
                                          ci_level = 0.95) {
  # bootstrap the mean contrast because policy scores are row-level quantities.
  ok <- is.finite(x)
  if (!is.null(weights)) ok <- ok & is.finite(weights) & weights > 0
  x <- x[ok]
  weights <- if (!is.null(weights)) weights[ok] else NULL

  if (length(x) < 2L) {
    return(tibble::tibble(
      estimate = NA_real_,
      se = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    ))
  }

  if (!is.null(seed)) set.seed(seed)
  draws <- replicate(R, {
    idx <- sample.int(length(x), length(x), replace = TRUE)
    .policy_regret_mean(x[idx], if (!is.null(weights)) weights[idx] else NULL)
  })
  alpha <- (1 - ci_level) / 2

  tibble::tibble(
    estimate = .policy_regret_mean(x, weights),
    se = stats::sd(draws, na.rm = TRUE),
    ci_lower = stats::quantile(draws, alpha, names = FALSE, na.rm = TRUE),
    ci_upper = stats::quantile(draws, 1 - alpha, names = FALSE, na.rm = TRUE)
  )
}

#' @keywords internal
.policy_regret_split_variables <- function(tree) {
  # recover unique split variables from policytree's node representation.
  if (is.null(tree$nodes) || is.null(tree$columns)) return(character())
  split_variables <- vapply(
    tree$nodes,
    function(node) {
      if (isTRUE(node$is_leaf)) return(NA_character_)
      tree$columns[[node$split_variable]]
    },
    character(1)
  )
  unique(split_variables[!is.na(split_variables)])
}
