# validate complete finite inputs without dropping or reordering participants.
.policy_evaluation_inputs <- function(X, scores, weights, label) {
  if (!(is.matrix(X) || is.data.frame(X)) ||
      !(is.matrix(scores) || is.data.frame(scores))) {
    stop(label, " X and scores must be numeric matrices or data frames.", call. = FALSE)
  }
  X <- as.matrix(X)
  scores <- as.matrix(scores)
  if (!is.numeric(X) || !is.numeric(scores) || !nrow(X) || !ncol(X) ||
      nrow(X) != nrow(scores) || ncol(scores) != 2L ||
      any(!is.finite(X)) || any(!is.finite(scores))) {
    stop(label, " requires aligned, finite numeric X and binary action scores.", call. = FALSE)
  }
  for (nm in list(colnames(X), colnames(scores))) {
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
      stop(label, " requires unique non-empty column names.", call. = FALSE)
    }
  }
  if (is.null(weights)) weights <- rep(1, nrow(X))
  if (!is.numeric(weights) || length(weights) != nrow(X) ||
      any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0) ||
      !is.finite(sum(weights))) {
    stop(label, " weights must be finite, non-negative and aligned, with positive finite sum.", call. = FALSE)
  }
  list(X = X, scores = scores, weights = as.numeric(weights))
}

# verify immutable rule, threshold and stored evaluation summaries before adaptation.
.policy_evaluation_validate <- function(x) {
  if (!inherits(x, "margot_policy_tree_evaluation") ||
      !identical(x$rule_id, .margot_policy_rule_signature(x$tree))) {
    stop("Invalid fixed-rule evaluation identity.", call. = FALSE)
  }
  recorded <- x$integrity_signature
  x$integrity_signature <- NULL
  if (!is.character(recorded) || length(recorded) != 1L ||
      !identical(recorded, digest::digest(x, algo = "sha256"))) {
    stop("Fixed-rule evaluation integrity check failed; the stored object has changed.", call. = FALSE)
  }
  invisible(TRUE)
}

# calculate a paired weighted mean and conditional independent-record interval.
.policy_evaluation_interval <- function(score, weights, inference = TRUE) {
  n <- length(score)
  available <- n >= 2L && sum(weights) > 0 && sum(weights > 0) >= 2L
  estimate <- if (n && sum(weights) > 0) sum((weights / sum(weights)) * score) else NA_real_
  influence <- if (n && sum(weights) > 0) weights / sum(weights) * (score - estimate) else rep(NA_real_, n)
  se <- if (available && inference) sqrt(n / (n - 1) * sum(influence^2)) else NA_real_
  method <- "Independent-record paired weighted-score sandwich; fixed development rule and supplied nuisance scores"
  data.frame(estimate = estimate, se = se,
    lower = estimate - stats::qnorm(.975) * se,
    upper = estimate + stats::qnorm(.975) * se,
    interval_type = if (is.finite(se)) "pointwise" else "unavailable",
    interval_level = if (is.finite(se)) .95 else NA_real_,
    interval_method = if (is.finite(se)) method else "",
    unavailable_reason = if (is.finite(se)) "" else if (!inference) "Development summaries are selected descriptive quantities" else "Fewer than two positive-weight evaluation records",
    stringsAsFactors = FALSE)
}

# construct a native constant rule with the declared action and feature schema.
.policy_evaluation_constant_tree <- function(X, action_names, action) {
  structure(list(nodes = list(list(is_leaf = TRUE, action = as.integer(action))),
    `_tree_array` = matrix(c(-1, action, 0, 0), nrow = 1L), depth = 0L,
    n.actions = 2L, n.features = ncol(X), action.names = action_names,
    columns = colnames(X)), class = "policy_tree")
}

# summarise each fixed terminal group on its own declared partition.
.policy_evaluation_leaves <- function(tree, inputs, threshold, inference) {
  ids <- .margot_policy_tree_leaf_ids(tree, inputs$X)
  terminal <- which(vapply(tree$nodes, function(node) isTRUE(node$is_leaf), logical(1)))
  tau <- inputs$scores[, threshold$treatment_column] - inputs$scores[, threshold$control_column]
  leaves <- lapply(seq_along(terminal), function(j) {
    node <- terminal[j]
    keep <- ids == node
    w <- inputs$weights[keep]
    contrast <- .policy_evaluation_interval(tau[keep], w, inference)
    contrast$node_id <- node
    contrast$leaf_label <- paste("Leaf", j)
    contrast$net_effect <- contrast$estimate - threshold$value
    contrast$net_lower <- contrast$lower - threshold$value
    contrast$net_upper <- contrast$upper - threshold$value
    contrast$n <- sum(keep)
    contrast$weight_sum <- sum(w)
    contrast$weight_share <- sum(w) / sum(inputs$weights)
    contrast$unweighted_share <- sum(keep) / nrow(inputs$X)
    contrast$effective_n <- if (sum(w) > 0) 1 / sum((w / sum(w))^2) else NA_real_
    contrast$max_normalised_weight <- if (sum(w) > 0) max(w / sum(w)) else NA_real_
    contrast$assigned_action <- as.integer(tree$nodes[[node]]$action)
    contrast
  })
  list(leaves = do.call(rbind, leaves), leaf_ids = ids)
}

#' Learn a benefit-threshold rule and evaluate it on separate supplied scores
#'
#' @description
#' Learns a binary policy tree and constant comparator using development data only, then evaluates those fixed rules on a separate evaluation sample. This score-level interface performs no nuisance fitting, preprocessing, sample splitting or causal identification.
#'
#' @param development_X,evaluation_X Complete finite numeric feature matrices or data frames with identical named columns in the same order. Features must have been prepared within the declared development boundary.
#' @param development_scores,evaluation_scores Unweighted finite binary action-score matrices or data frames, with identical named columns in the same order. Rows must align with the corresponding features and weights. Scores must already share outcome units and orientation, with larger values preferred. Evaluation outcomes must not have contributed to development scores, feature preparation, threshold estimation or rule selection.
#' @param development_weights,evaluation_weights Optional finite non-negative analysis weights with positive finite sums. Applied exactly once. Defaults to equal weights. Weight estimation and population projection remain the caller's responsibility.
#' @param value_threshold Benefit threshold, either `"ate"` (the default for this new interface) or a finite numeric value. The development weighted mean treatment-minus-control action score defines the ATE reference. Its resolved value is carried unchanged into evaluation.
#' @param threshold_multiplier Finite multiplier for an ATE reference; see [margot_policy_value_threshold()].
#' @param depth Prespecified maximum depth, one or two. No depth selection uses evaluation data.
#' @param min_node_size Positive integer minimum development leaf count, default one. This is a computational constraint, not a guarantee of inferential support.
#' @param tree_method Requested engine, `"policytree"` (default) or `"fastpolicytree"`. The requested package must be installed. There is no engine fallback; the fast engine uses `strategy.datatype = 1`.
#' @param development_ids,evaluation_ids Optional unique, non-missing participant identifiers aligned with rows. Supply both or neither. Supplied identifiers must be disjoint. Without identifiers, participant separation cannot be verified; matrix equality is not an identity test.
#' @param treatment_column,control_column Optional action-column identities passed to [margot_policy_value_threshold()].
#' @param gain_margin Non-negative population-level gain margin saved for reporting, default 0.01 in the supplied outcome units. It does not change the treatment threshold, select the rule or define statistical significance.
#'
#' @details
#' The treatment reward is reduced by the development threshold before weighting. A tree is retained only when its development net value exceeds the development constant value beyond floating-point tolerance. Otherwise a constant rule is saved. Universal-action ties select control deterministically. This is training-objective simplification, not evidence of out-of-sample superiority. All prespecified evaluation comparisons are returned without choosing a winner using evaluation outcomes.
#'
#' For a paired score difference `D`, normalised weights `p`, and `n` independent evaluation records, the standard error is `sqrt(n/(n-1) * sum((p * (D - sum(p*D)))^2))`. Nominal pointwise 95% normal intervals condition on the learned rule, realised threshold, supplied nuisance scores and preparation. They do not account for training uncertainty, nuisance estimation bias, imputation, estimated weights, clustering, investigator outcome access or multiplicity. Independent evaluation rows and suitable nuisance-score conditions require separate justification. No claim of unconditional causal coverage is made. Leaf intervals refer to the unchanged development partition; a direct between-leaf comparison requires its own inference.
#'
#' @return A `margot_policy_tree_evaluation` list with the native fixed `tree`, its `rule_id`, resolved `threshold`, development-selected `constant`, partition identities and inference limitations. `development` stores descriptive leaves and values. `evaluation` stores original and net leaf contrasts, actions, weights, gross/cost/net values, all paired tree-minus-comparator intervals and the primary `value` row. Original scores are retained separately from net scores, and paired influence contributions are saved. The object is compatible with adapters to [margot_policy_reporting_data()].
#' @md
#' @export
margot_policy_tree_evaluate <- function(development_X, development_scores,
    evaluation_X, evaluation_scores, development_weights = NULL,
    evaluation_weights = NULL, value_threshold = "ate", threshold_multiplier = 1,
    depth = 1L, min_node_size = 1L, tree_method = "policytree",
    development_ids = NULL, evaluation_ids = NULL,
    treatment_column = NULL, control_column = NULL, gain_margin = .01) {
  development <- .policy_evaluation_inputs(development_X, development_scores, development_weights, "Development")
  evaluation <- .policy_evaluation_inputs(evaluation_X, evaluation_scores, evaluation_weights, "Evaluation")
  if (!identical(colnames(development$X), colnames(evaluation$X)) ||
      !identical(colnames(development$scores), colnames(evaluation$scores))) {
    stop("Development and evaluation column names and order must conform.", call. = FALSE)
  }
  if (nrow(evaluation$X) < 2L) stop("At least two evaluation records are required.", call. = FALSE)
  if (xor(is.null(development_ids), is.null(evaluation_ids))) stop("Supply both development_ids and evaluation_ids or neither.", call. = FALSE)
  if (!is.null(development_ids)) {
    for (pair in list(list(development_ids, nrow(development$X)), list(evaluation_ids, nrow(evaluation$X)))) {
      id <- pair[[1]]
      if (!is.atomic(id) || !is.null(dim(id)) || length(id) != pair[[2]] || anyNA(id) ||
          any(!nzchar(as.character(id))) || anyDuplicated(as.character(id))) {
        stop("Participant IDs must be unique non-missing values aligned with rows.", call. = FALSE)
      }
    }
    if (length(intersect(as.character(development_ids), as.character(evaluation_ids)))) {
      stop("Development and evaluation participant IDs must be disjoint.", call. = FALSE)
    }
  }
  if (!is.numeric(depth) || length(depth) != 1L || is.na(depth) || !depth %in% 1:2) stop("depth must be one or two.", call. = FALSE)
  min_node_size <- .resolve_policy_tree_min_node_size(min_node_size)
  if (min_node_size > nrow(development$X)) stop("min_node_size exceeds the development sample.", call. = FALSE)
  if (!is.numeric(gain_margin) || length(gain_margin) != 1L || !is.finite(gain_margin) || gain_margin < 0) stop("gain_margin must be finite and non-negative.", call. = FALSE)
  tree_method <- match.arg(tree_method, c("policytree", "fastpolicytree"))
  if (!requireNamespace(tree_method, quietly = TRUE)) stop("Requested tree engine ", tree_method, " is not installed; no fallback is permitted.", call. = FALSE)
  threshold <- margot_policy_value_threshold(development$scores, development$weights,
    value_threshold, threshold_multiplier, treatment_column, control_column)
  development$net_scores <- .policy_value_net_scores(development$scores, threshold)
  evaluation$net_scores <- .policy_value_net_scores(evaluation$scores, threshold)
  tolerance <- .policy_value_constant_tolerance(development$scores, development$weights)
  # resolve explicit action identities without relying on conventional score labels.
  constant_values <- colSums(development$net_scores * (development$weights / sum(development$weights)))
  constant_ids <- which(max(constant_values) - constant_values <= tolerance)
  constant_id <- if (threshold$control_column %in% constant_ids) threshold$control_column else constant_ids[1]
  constant <- list(action_id = as.integer(constant_id),
    action = if (constant_id == threshold$treatment_column) "treated" else "control",
    value_training = unname(constant_values[constant_id]), tie_tolerance = tolerance,
    training_tie = length(constant_ids) > 1L)
  # normalise weights for numerical stability; each score receives one weight.
  rewards <- development$net_scores * (development$weights / sum(development$weights))
  if (any(!is.finite(rewards))) stop("Net training rewards must be finite.", call. = FALSE)
  tree <- if (tree_method == "policytree") {
    policytree::policy_tree(development$X, rewards, depth = depth,
      min.node.size = min_node_size, verbose = FALSE)
  } else {
    fastpolicytree::fastpolicytree(development$X, rewards, depth = depth,
      min.node.size = min_node_size, strategy.datatype = 1)
  }
  development_actions <- as.integer(stats::predict(tree, development$X))
  if (length(development_actions) != nrow(development$X) || anyNA(development_actions) || any(!development_actions %in% 1:2)) stop("Engine returned invalid development actions.", call. = FALSE)
  tree_training_value <- sum(rewards[cbind(seq_len(nrow(rewards)), development_actions)])
  constant_selected <- tree_training_value - constant$value_training <= tolerance
  if (constant_selected) tree <- .policy_evaluation_constant_tree(development$X, colnames(development$scores), constant_id)
  rule_id <- .margot_policy_rule_signature(tree)
  qualification <- paste("Nominal pointwise inference conditional on the fixed development rule, realised threshold, supplied nuisance scores and preparation.",
    "No full nuisance, weight-estimation, imputation, training, cluster or multiplicity uncertainty is included; independent records and score validity require external justification.")
  partitions <- list(development = development, evaluation = evaluation)
  for (partition in names(partitions)) {
    item <- partitions[[partition]]
    item$actions <- as.integer(stats::predict(tree, item$X))
    if (length(item$actions) != nrow(item$X) || anyNA(item$actions) || any(!item$actions %in% 1:2)) stop("Engine returned invalid actions.", call. = FALSE)
    leaf <- .policy_evaluation_leaves(tree, item, threshold, partition == "evaluation")
    item$leaves <- leaf$leaves
    item$leaf_ids <- leaf$leaf_ids
    action_set <- list(tree = item$actions, development_constant = rep(constant_id, nrow(item$X)),
      universal_control = rep(threshold$control_column, nrow(item$X)),
      universal_treated = rep(threshold$treatment_column, nrow(item$X)))
    raw <- vapply(action_set, function(a) item$scores[cbind(seq_len(nrow(item$X)), a)], numeric(nrow(item$X)))
    net <- vapply(action_set, function(a) item$net_scores[cbind(seq_len(nrow(item$X)), a)], numeric(nrow(item$X)))
    p <- item$weights / sum(item$weights)
    item$values <- data.frame(policy = names(action_set), gross_value = colSums(raw * p),
      cost = vapply(action_set, function(a) sum(p * (a == threshold$treatment_column)) * threshold$value, numeric(1)),
      net_value = colSums(net * p), row.names = NULL)
    if (partition == "evaluation") {
      differences <- net[, "tree"] - net[, -1L, drop = FALSE]
      comparisons <- lapply(seq_len(ncol(differences)), function(j) {
        d <- .policy_evaluation_interval(differences[, j], item$weights)
        d$comparator_id <- colnames(differences)[j]
        d$comparator_label <- switch(d$comparator_id,
          development_constant = paste("Development-selected uniform", constant$action),
          universal_control = "Universal control", universal_treated = "Universal treatment")
        d$gain_margin <- gain_margin
        d
      })
      item$comparisons <- do.call(rbind, comparisons)
      item$value <- item$comparisons[item$comparisons$comparator_id == "development_constant", , drop = FALSE]
      item$paired_scores <- differences
      item$paired_influence <- sweep(differences, 2, colSums(differences * p)) * p
    }
    partitions[[partition]] <- item
  }
  metadata <- list(development_id = digest::digest(list(partition = "development", ids = development_ids,
      X = development$X, scores = development$scores, weights = development$weights), algo = "sha256"),
    evaluation_id = digest::digest(list(partition = "evaluation", ids = evaluation_ids,
      X = evaluation$X, scores = evaluation$scores, weights = evaluation$weights), algo = "sha256"),
    ids_verified_disjoint = !is.null(development_ids), development_ids = development_ids,
    evaluation_ids = evaluation_ids, requested_engine = tree_method, realised_engine = tree_method,
    requested_depth = depth, constant_selected = constant_selected,
    min_node_size = min_node_size, gain_margin = gain_margin,
    evaluation_mode = "independent_fixed_rule", nuisance_fitting = "none; caller-supplied scores")
  result <- structure(list(tree = tree, rule_id = rule_id, threshold = threshold, constant = constant,
    development = partitions$development, evaluation = partitions$evaluation,
    metadata = metadata, inference = list(qualification = qualification,
      method = "independent-record paired weighted-score sandwich", interval_level = .95)),
    class = "margot_policy_tree_evaluation")
  result$integrity_signature <- digest::digest(result, algo = "sha256")
  result
}
