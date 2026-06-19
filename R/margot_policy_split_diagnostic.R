#' Repeated Split Diagnostic for Policy Trees
#'
#' Repeatedly fits policy trees on a training split and evaluates their action
#' summaries on the complementary split. This diagnostic describes instability in
#' the policy-learning procedure; it is not the displayed full-data tree.
#'
#' @param object A list returned by \code{\link{margot_causal_forest}} with
#'   saved \code{covariates}, \code{not_missing}, and per-model \code{dr_scores}.
#' @param model_names Optional model names, with or without the \code{model_}
#'   prefix. Defaults to all models.
#' @param depths Integer vector containing 1, 2, or both. Defaults to
#'   \code{c(1L, 2L)}.
#' @param n_splits Integer. Number of repeated train/test splits. Defaults to 50.
#' @param train_proportion Numeric in (0,1). Fraction assigned to the training
#'   split. Defaults to 0.5.
#' @param seed Integer or \code{NULL}. Seed for reproducibility. Defaults to 12345.
#' @param covariate_mode Character; one of "original", "custom", "add", or "all".
#'   Defaults to "original".
#' @param custom_covariates Optional character vector used with "custom" or "add".
#' @param exclude_covariates Optional character vector of exact names or patterns
#'   to exclude from the policy-tree covariates.
#' @param tree_method Character; "fastpolicytree" when available, otherwise
#'   "policytree".
#' @param weights Optional weights for held-out summaries. Defaults to
#'   \code{object$weights}.
#' @param verbose Logical; print progress messages. Defaults to TRUE.
#'
#' @return A tibble with one row per model, split, and depth. The returned object
#'   has class \code{margot_policy_split_diagnostic} and a \code{summary}
#'   attribute with mean and standard deviation of key metrics.
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom stats complete.cases predict sd
margot_policy_split_diagnostic <- function(object,
                                           model_names = NULL,
                                           depths = c(1L, 2L),
                                           n_splits = 50L,
                                           train_proportion = 0.5,
                                           seed = 12345,
                                           covariate_mode = c("original", "custom", "add", "all"),
                                           custom_covariates = NULL,
                                           exclude_covariates = NULL,
                                           tree_method = c("fastpolicytree", "policytree"),
                                           weights = NULL,
                                           verbose = TRUE) {
  # validate the saved-data contract needed for repeated split diagnostics.
  if (!is.list(object) || is.null(object$results) || is.null(object$covariates)) {
    stop("object must contain results and saved covariates", call. = FALSE)
  }
  if (!is.numeric(train_proportion) || length(train_proportion) != 1L ||
      train_proportion <= 0 || train_proportion >= 1) {
    stop("train_proportion must be a single number between 0 and 1", call. = FALSE)
  }
  n_splits <- as.integer(n_splits)
  if (is.na(n_splits) || n_splits < 1L) {
    stop("n_splits must be a positive integer", call. = FALSE)
  }
  depths <- unique(as.integer(depths))
  if (!length(depths) || anyNA(depths) || any(!depths %in% c(1L, 2L))) {
    stop("depths must contain 1, 2, or both", call. = FALSE)
  }

  covariate_mode <- match.arg(covariate_mode)
  tree_method <- match.arg(tree_method)
  actual_tree_method <- .get_tree_method(tree_method, verbose)
  model_names <- .policy_regret_resolve_model_names(object, model_names)
  weights <- weights %||% object$weights %||% NULL

  covariates <- object$covariates
  not_missing <- object$not_missing %||% which(stats::complete.cases(covariates))
  if (length(not_missing) < 2L) {
    stop("at least two complete cases are required", call. = FALSE)
  }

  if (verbose) {
    cli::cli_alert_info("Running repeated split policy-tree diagnostic for {length(model_names)} model{?s}")
  }

  rows <- list()
  pb <- if (verbose) {
    cli::cli_progress_bar(total = length(model_names) * n_splits * length(depths))
  } else {
    NULL
  }

  for (model_name in model_names) {
    model <- object$results[[model_name]]
    selected_vars <- .policy_split_selected_vars(
      model = model,
      covariates = covariates,
      covariate_mode = covariate_mode,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      verbose = verbose
    )
    dr_raw <- model$dr_scores %||% model$dr_scores_flipped
    if (is.null(dr_raw)) {
      stop("No dr_scores found for ", model_name, call. = FALSE)
    }
    dr_scores <- as.matrix(dr_raw)

    for (split_id in seq_len(n_splits)) {
      if (!is.null(seed)) set.seed(seed + split_id + as.integer(as.factor(model_name)))
      train_size <- floor(train_proportion * length(not_missing))
      train_size <- max(1L, min(length(not_missing) - 1L, train_size))
      train_idx <- sample(not_missing, train_size)
      eval_idx <- setdiff(not_missing, train_idx)

      for (depth in depths) {
        tree <- .compute_policy_tree(
          covariates[train_idx, selected_vars, drop = FALSE],
          dr_scores[train_idx, , drop = FALSE],
          depth = depth,
          tree_method = actual_tree_method
        )

        rows[[length(rows) + 1L]] <- .policy_split_one_row(
          model_name = model_name,
          split_id = split_id,
          depth = depth,
          tree = tree,
          covariates = covariates[eval_idx, , drop = FALSE],
          dr_scores = dr_scores[eval_idx, , drop = FALSE],
          weights = .policy_regret_align_weights(weights, eval_idx, length(eval_idx)),
          n_train = length(train_idx),
          eval_idx = eval_idx
        )

        if (verbose) cli::cli_progress_update(id = pb)
      }
    }
  }

  if (verbose) {
    cli::cli_progress_done(id = pb)
    cli::cli_alert_success("Repeated split diagnostic completed")
  }

  out <- if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble()
  attr(out, "summary") <- .policy_split_summary(out)
  class(out) <- c("margot_policy_split_diagnostic", class(out))
  out
}

#' @keywords internal
.policy_split_selected_vars <- function(model,
                                        covariates,
                                        covariate_mode,
                                        custom_covariates,
                                        exclude_covariates,
                                        verbose) {
  # resolve the covariates used for split diagnostics from existing policy-tree settings.
  all_covars <- colnames(covariates)
  if (identical(covariate_mode, "all")) {
    selected_vars <- all_covars
  } else if (identical(covariate_mode, "custom") && !is.null(custom_covariates)) {
    selected_vars <- custom_covariates
  } else if (identical(covariate_mode, "add") && !is.null(custom_covariates)) {
    selected_vars <- unique(c(model$top_vars %||% character(), custom_covariates))
  } else {
    selected_vars <- model$top_vars
  }
  if (is.null(selected_vars) || !length(selected_vars)) {
    stop("No policy-tree covariates available", call. = FALSE)
  }
  selected_vars <- intersect(selected_vars, all_covars)
  if (!is.null(exclude_covariates)) {
    selected_vars <- apply_covariate_exclusions(selected_vars, exclude_covariates, verbose)
  }
  if (!length(selected_vars)) {
    stop("No covariates remain after exclusions", call. = FALSE)
  }
  selected_vars
}

#' @keywords internal
.policy_split_one_row <- function(model_name,
                                  split_id,
                                  depth,
                                  tree,
                                  covariates,
                                  dr_scores,
                                  weights,
                                  n_train,
                                  eval_idx) {
  # evaluate a trained policy tree on a held-out split using stored DR scores.
  keep <- stats::complete.cases(covariates[, tree$columns, drop = FALSE]) &
    stats::complete.cases(dr_scores)
  covariates <- covariates[keep, , drop = FALSE]
  dr_scores <- dr_scores[keep, , drop = FALSE]
  weights <- if (!is.null(weights)) weights[keep] else NULL
  eval_idx <- eval_idx[keep]

  actions <- .normalize_policy_actions(stats::predict(
    tree,
    covariates[, tree$columns, drop = FALSE],
    type = "action.id"
  ))
  control_score <- dr_scores[, 1]
  treated_score <- dr_scores[, 2]
  policy_score <- dr_scores[cbind(seq_along(actions), actions)]
  uplift <- treated_score - control_score
  treated <- actions == 2L

  tibble::tibble(
    model = model_name,
    outcome = gsub("^model_", "", model_name),
    split_id = split_id,
    depth = as.integer(depth),
    n_train = n_train,
    n_eval = length(actions),
    eval_ids = list(eval_idx),
    treat_share = .policy_regret_mean(treated, weights),
    value_policy = .policy_regret_mean(policy_score, weights),
    gain_vs_control = .policy_regret_mean(policy_score - control_score, weights),
    gain_vs_treat = .policy_regret_mean(policy_score - treated_score, weights),
    avg_uplift_treated = if (any(treated)) .policy_regret_mean(uplift[treated], if (!is.null(weights)) weights[treated] else NULL) else NA_real_,
    split_variables = paste(.policy_regret_split_variables(tree), collapse = "; ")
  )
}

#' @keywords internal
.policy_split_summary <- function(x) {
  # summarise split-to-split variation for quick reporting.
  if (is.null(x) || !nrow(x)) return(tibble::tibble())
  x |>
    dplyr::group_by(model, outcome, depth) |>
    dplyr::summarise(
      n_splits = dplyr::n(),
      mean_gain_vs_control = mean(gain_vs_control, na.rm = TRUE),
      sd_gain_vs_control = stats::sd(gain_vs_control, na.rm = TRUE),
      mean_treat_share = mean(treat_share, na.rm = TRUE),
      sd_treat_share = stats::sd(treat_share, na.rm = TRUE),
      mean_uplift_treated = mean(avg_uplift_treated, na.rm = TRUE),
      sd_uplift_treated = stats::sd(avg_uplift_treated, na.rm = TRUE),
      .groups = "drop"
    )
}
