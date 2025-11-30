# internal helper function for applying labels
#' @keywords internal
.apply_label_bootstrap <- function(var_name, label_mapping = NULL) {
  if (!is.null(label_mapping) && var_name %in% names(label_mapping)) {
    return(label_mapping[[var_name]])
  }
  # fall back to transform_var_name with silent mode
  suppressMessages(transform_var_name(var_name, label_mapping))
}

#' Bootstrap Analysis for Policy Trees
#'
#' @description
#' Performs bootstrap analysis of policy trees to assess stability and generate
#' consensus trees. By default, varies random seeds to create different train/test
#' splits rather than bootstrap resampling, as trees are highly sensitive to data
#' perturbations. Uses memory-efficient streaming approach to handle large datasets.
#'
#' @param model_results List returned by margot_causal_forest() or margot_flip_forests()
#' @param model_names Character vector of model names to analyze. NULL = all models.
#' @param custom_covariates Character vector of covariate names to use for policy trees.
#'   If NULL, uses the original top variables from the model.
#' @param exclude_covariates Character vector of covariate names or patterns to exclude.
#'   Supports exact matches and regex patterns (e.g., "_log" excludes all variables
#'   containing "_log").
#' @param covariate_mode Character string specifying how to handle covariates:
#'   "original" (use original top variables), "custom" (use only custom_covariates),
#'   "add" (add custom to existing), "all" (use all available covariates).
#' @param depth Numeric or character specifying which depth(s) to compute:
#'   1 for single split, 2 for two splits (default), or "both" for both depths.
#' @param n_bootstrap Integer. Number of bootstrap iterations (default 300).
#' @param vary_type Character. Type of variation: "split_only" (vary train/test split via seeds),
#'   "sample_only" (bootstrap resample), "both" (resample + split). Default is "split_only".
#' @param consensus_threshold Numeric. Minimum inclusion frequency for consensus (default 0.5).
#' @param train_proportion Numeric. Train/test split when vary_train_proportion = FALSE (default 0.5).
#' @param vary_train_proportion Logical. Whether to vary train proportion (default FALSE).
#' @param train_proportions Numeric vector. Proportions to cycle through when
#'   vary_train_proportion = TRUE (default c(0.4, 0.5, 0.6, 0.7)).
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param return_consensus_trees Logical. Return fitted consensus trees (default TRUE).
#' @param metaseed Integer. Master seed for reproducibility (default 12345).
#' @param parallel Logical. Use parallel processing (default FALSE).
#' @param n_cores Integer. Number of cores for parallel processing.
#' @param verbose Logical. Print progress messages (default TRUE).
#' @param seed Integer. Additional seed parameter for compatibility (default 12345).
#' @param tree_method Character string specifying the package to use: "fastpolicytree"
#'   (default) or "policytree". The fastpolicytree package provides ~10x faster
#'   computation, which is particularly beneficial for bootstrap analysis. Falls
#'   back to policytree if fastpolicytree is not installed.
#'
#' @return Object of class "margot_bootstrap_policy_tree" containing:
#' \itemize{
#'   \item results: List with consensus trees and bootstrap metrics per model
#'   \item summary_metrics: Variable importance and convergence diagnostics
#'   \item metadata: Bootstrap parameters and seeds used
#' }
#'
#' @details
#' The function uses a memory-efficient approach:
#' \itemize{
#'   \item Processes one tree at a time
#'   \item Extracts only essential split information
#'   \item Accumulates statistics without storing all trees
#'   \item Reconstructs single consensus trees for compatibility
#' }
#'
#' By default, the function varies random seeds to create different train/test splits
#' for each iteration rather than using bootstrap resampling. This is because decision
#' trees are highly sensitive to small data perturbations, and seed variation provides
#' a more realistic assessment of tree stability.
#'
#' @section Theoretical Background:
#' Policy trees inherit the instability of decision trees, where small changes in
#' the data can lead to completely different tree structures (Breiman, 1996). This
#' instability is particularly pronounced when predictors are correlated, as the tree
#' can arbitrarily choose between similar variables at split points. Athey and Wager's
#' (2021) policy learning framework acknowledges these challenges while providing
#' methods to extract robust insights despite the instability.
#'
#' The bootstrap analysis helps distinguish between:
#' \itemize{
#'   \item Fundamental instability due to weak or absent treatment effect heterogeneity
#'   \item Apparent instability due to correlated predictors that capture similar information
#'   \item Robust patterns that emerge consistently across different data samples
#' }
#'
#'
#' Use the companion functions `margot_assess_variable_correlation()` and
#' `margot_stability_diagnostics()` to better understand the sources of instability.
#'
#' Three types of variation are supported:
#' \itemize{
#'   \item "both": Varies both bootstrap sampling and train/test splits
#'   \item "sample_only": Only bootstrap resampling, fixed train/test split
#'   \item "split_only": Fixed sample, only varies train/test split
#' }
#'
#' @section Complete Workflow Example:
#' \preformatted{
#' # 1. Run causal forest (save data for correlation analysis)
#' cf_results <- margot_causal_forest(
#'   data = your_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   save_data = TRUE  # Important for correlation analysis
#' )
#'
#' # 2. Run bootstrap analysis to assess stability
#' boot_results <- margot_policy_tree_bootstrap(
#'   cf_results,
#'   n_bootstrap = 300,
#'   tree_method = "fastpolicytree"  # 10x faster if available
#' )
#'
#' # 3. Check variable correlations
#' cor_analysis <- margot_assess_variable_correlation(
#'   cf_results,  # Use original results, NOT boot_results
#'   "model_outcome1"
#' )
#'
#' # 4. Identify clusters of correlated variables
#' clusters <- margot_identify_variable_clusters(cor_analysis)
#'
#' # 5. Get comprehensive diagnostics
#' diagnostics <- margot_stability_diagnostics(
#'   bootstrap_results = boot_results,
#'   model_results = cf_results,
#'   model_name = "model_outcome1"
#' )
#'
#' # 6. Interpret results
#' interpretation <- margot_interpret_bootstrap(
#'   boot_results,
#'   "model_outcome1",
#'   include_theory = TRUE  # Include theoretical context
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Basic bootstrap with fixed train proportion
#' boot_results <- margot_policy_tree_bootstrap(
#'   causal_forest_results,
#'   n_bootstrap = 300
#' )
#'
#' # Vary train proportion with default values
#' boot_results <- margot_policy_tree_bootstrap(
#'   causal_forest_results,
#'   vary_train_proportion = TRUE
#' )
#'
#' # Custom train proportions
#' boot_results <- margot_policy_tree_bootstrap(
#'   causal_forest_results,
#'   vary_train_proportion = TRUE,
#'   train_proportions = c(0.3, 0.5, 0.7)
#' )
#'
#' # Use bootstrap resampling instead of seed variation
#' boot_results <- margot_policy_tree_bootstrap(
#'   causal_forest_results,
#'   vary_type = "sample_only",
#'   n_bootstrap = 300
#' )
#'
#' # Plot consensus tree
#' margot_plot_policy_tree(boot_results, "model_anxiety")
#'
#' # Get bootstrap summary
#' summary(boot_results)
#'
#' # Interpret results with theoretical context
#' interpretation <- margot_interpret_bootstrap(
#'   boot_results,
#'   "model_anxiety",
#'   format = "text"
#' )
#'
#' # Assess variable correlations (using original causal forest results)
#' cor_analysis <- margot_assess_variable_correlation(
#'   causal_forest_results, # NOT boot_results
#'   "model_anxiety"
#' )
#'
#' # Identify variable clusters
#' clusters <- margot_identify_variable_clusters(cor_analysis)
#'
#' # Run comprehensive stability diagnostics
#' diagnostics <- margot_stability_diagnostics(
#'   bootstrap_results = boot_results,
#'   model_results = causal_forest_results,
#'   model_name = "model_anxiety"
#' )
#' }
#'
#' @seealso
#' \code{\link{margot_policy_tree}} for computing policy trees without bootstrap
#' \code{\link{margot_assess_variable_correlation}} for correlation analysis
#' \code{\link{margot_stability_diagnostics}} for comprehensive diagnostics
#'
#' @references
#' Athey, S., & Wager, S. (2021). Policy learning with observational data.
#' Econometrica, 89(1), 133-161.
#'
#' Breiman, L. (1996). Bagging predictors. Machine Learning, 24(2), 123-140.
#'
#' Zhou, Z., Athey, S., & Wager, S. (2023). Offline multi-action policy learning:
#' Generalization and optimization. Operations Research, 71(1), 148-183.
#'
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom policytree policy_tree
#' @importFrom stats complete.cases
margot_policy_tree_bootstrap <- function(
    model_results,
    model_names = NULL,
    custom_covariates = NULL,
    exclude_covariates = NULL,
    covariate_mode = c("original", "custom", "add", "all"),
    depth = 2,
    n_bootstrap = 300,
    vary_type = c("split_only", "sample_only", "both"),
    consensus_threshold = 0.5,
    train_proportion = 0.5,
    vary_train_proportion = FALSE,
    train_proportions = c(0.4, 0.5, 0.6, 0.7),
    label_mapping = NULL,
    return_consensus_trees = TRUE,
    metaseed = 12345,
    parallel = FALSE,
    n_cores = NULL,
    verbose = TRUE,
    seed = 12345,
    tree_method = c("fastpolicytree", "policytree")) {
  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be output from margot_causal_forest() or similar")
  }

  vary_type <- match.arg(vary_type)
  covariate_mode <- match.arg(covariate_mode)
  tree_method <- match.arg(tree_method)

  # check tree method availability
  actual_tree_method <- .get_tree_method(tree_method, verbose)

  # convert depth parameter to match margot_policy_tree
  if (is.character(depth)) {
    if (depth == "both") {
      depths <- c(1, 2)
    } else {
      depths <- as.numeric(depth)
    }
  } else {
    depths <- depth
  }

  # validate train proportions
  if (vary_train_proportion) {
    if (any(train_proportions <= 0.2 | train_proportions >= 0.9)) {
      if (verbose) {
        cli::cli_alert_warning(
          "train_proportions outside [0.2, 0.9] may lead to unstable results"
        )
      }
    }
    if (length(train_proportions) < 2) {
      if (verbose) {
        cli::cli_alert_warning(
          "vary_train_proportion = TRUE but only one proportion provided"
        )
      }
    }
  }

  # check for required data
  if (is.null(model_results$covariates)) {
    stop("covariates not found in model_results. Ensure save_data = TRUE in margot_causal_forest()")
  }

  # determine models to process
  if (is.null(model_names)) {
    model_names <- names(model_results$results)
  } else {
    # add model_ prefix if needed
    model_names <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
  }

  if (verbose) {
    cli::cli_h1("Bootstrap Policy Tree Analysis")
    cli::cli_alert_info("Processing {length(model_names)} model{?s} with {n_bootstrap} bootstrap iterations")

    # determine actual variation type based on parameters
    actual_variation <- if (!vary_train_proportion && vary_type == "split_only") {
      "seed variation (fixed train proportion)"
    } else if (vary_train_proportion && vary_type == "split_only") {
      "seed + train proportion variation"
    } else {
      vary_type
    }
    cli::cli_alert_info("Variation type: {actual_variation}")
    cli::cli_alert_info("Tree method: {actual_tree_method}")
    cli::cli_alert_info("Covariate mode: {covariate_mode}")
    if (!is.null(exclude_covariates)) {
      cli::cli_alert_info("Excluding covariates matching: {paste(exclude_covariates, collapse = ', ')}")
    }
    if (!is.null(custom_covariates) && covariate_mode != "original") {
      cli::cli_alert_info("Using custom covariates: {paste(custom_covariates, collapse = ', ')}")
    }
    if (vary_train_proportion) {
      cli::cli_alert_info(
        "Varying train_proportion across: {paste(train_proportions, collapse = ', ')}"
      )
    } else {
      cli::cli_alert_info("Fixed train_proportion: {train_proportion}")
    }
  }

  # generate all seeds from metaseed
  all_seeds <- generate_bootstrap_seeds(metaseed, n_bootstrap)

  # determine actual train proportions for each iteration
  if (vary_train_proportion) {
    actual_train_props <- rep_len(train_proportions, n_bootstrap)
  } else {
    actual_train_props <- rep(train_proportion, n_bootstrap)
  }

  # get data
  covariates <- model_results$covariates
  not_missing <- model_results$not_missing
  if (is.null(not_missing)) {
    not_missing <- which(complete.cases(covariates))
  }

  # initialize output structure
  output <- list(
    results = list(),
    summary_metrics = list(),
    metadata = list(
      metaseed = metaseed,
      n_bootstrap = n_bootstrap,
      vary_type = vary_type,
      vary_train_proportion = vary_train_proportion,
      train_proportions = if (vary_train_proportion) train_proportions else train_proportion,
      consensus_threshold = consensus_threshold,
      tree_method = actual_tree_method,
      timestamp = Sys.time(),
      seeds_used = all_seeds
    )
  )

  # process each model
  for (model_name in model_names) {
    if (verbose) cli::cli_h2("Processing {model_name}")

    model_result <- model_results$results[[model_name]]

    # run bootstrap analysis for this model
    boot_result <- bootstrap_single_model(
      model_result = model_result,
      model_name = model_name,
      covariates = covariates,
      not_missing = not_missing,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      covariate_mode = covariate_mode,
      n_bootstrap = n_bootstrap,
      vary_type = vary_type,
      depths = depths,
      actual_train_props = actual_train_props,
      all_seeds = all_seeds,
      consensus_threshold = consensus_threshold,
      return_consensus_trees = return_consensus_trees,
      label_mapping = label_mapping,
      verbose = verbose,
      seed = seed,
      tree_method = actual_tree_method
    )

    output$results[[model_name]] <- boot_result
  }

  # compute summary metrics across all models
  output$summary_metrics <- compute_summary_metrics(output$results, verbose)

  if (verbose) cli::cli_alert_success("Bootstrap analysis completed")

  # set class for S3 methods
  class(output) <- c("margot_bootstrap_policy_tree", "margot_policy_tree", "list")

  return(output)
}

#' Generate bootstrap seeds from metaseed
#' @keywords internal
generate_bootstrap_seeds <- function(metaseed, n_bootstrap) {
  set.seed(metaseed)
  list(
    sample_seeds = sample.int(.Machine$integer.max, n_bootstrap),
    split_seeds = sample.int(.Machine$integer.max, n_bootstrap)
  )
}

#' Bootstrap analysis for a single model
#' @keywords internal
bootstrap_single_model <- function(
    model_result,
    model_name,
    covariates,
    not_missing,
    custom_covariates,
    exclude_covariates,
    covariate_mode,
    n_bootstrap,
    vary_type,
    depths,
    actual_train_props,
    all_seeds,
    consensus_threshold,
    return_consensus_trees,
    label_mapping,
    verbose,
    seed,
    tree_method) {
  # get DR scores (use flipped if available)
  dr_scores <- model_result$dr_scores
  if (is.null(dr_scores)) {
    dr_scores <- model_result$dr_scores_flipped
  }
  if (is.null(dr_scores)) {
    stop(paste("No dr_scores found for", model_name))
  }

  # determine covariates to use (same logic as margot_policy_tree)
  all_covars <- colnames(covariates)

  if (covariate_mode == "all") {
    selected_vars <- all_covars
    if (length(selected_vars) > 20 && verbose) {
      cli::cli_alert_warning(
        "Using all {length(selected_vars)} covariates for policy trees. This may be computationally intensive."
      )
    }
  } else if (covariate_mode == "add" && !is.null(custom_covariates)) {
    # combine existing top vars with custom
    existing_top_vars <- model_result$top_vars
    if (is.null(existing_top_vars)) {
      cli::cli_alert_warning("No top_vars found for {model_name}, using custom covariates only")
      selected_vars <- custom_covariates
    } else {
      selected_vars <- unique(c(existing_top_vars, custom_covariates))
    }
  } else if (covariate_mode == "custom" && !is.null(custom_covariates)) {
    selected_vars <- custom_covariates
  } else {
    # use original top vars
    selected_vars <- model_result$top_vars
    if (is.null(selected_vars)) {
      stop(paste("No top_vars found for", model_name, "and no custom_covariates specified"))
    }
  }

  # ensure selected covariates exist
  selected_vars <- intersect(selected_vars, all_covars)

  # apply exclusions
  if (!is.null(exclude_covariates)) {
    selected_vars <- apply_covariate_exclusions(selected_vars, exclude_covariates, verbose)
  }

  if (length(selected_vars) == 0) {
    stop(paste("No covariates remaining for", model_name, "after applying exclusions"))
  }

  # initialize accumulator
  accumulator <- create_accumulator(
    vars = selected_vars,
    n_actions = ncol(dr_scores)
  )

  # progress bar
  if (verbose) {
    pb <- cli::cli_progress_bar(
      total = n_bootstrap,
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_eta}"
    )
  }

  # bootstrap loop
  for (b in 1:n_bootstrap) {
    # get train indices based on vary_type
    boot_result <- get_bootstrap_indices(
      not_missing = not_missing,
      vary_type = vary_type,
      train_prop = actual_train_props[b],
      sample_seed = all_seeds$sample_seeds[b],
      split_seed = all_seeds$split_seeds[b]
    )

    # fit trees and extract info
    for (depth in depths) {
      if (depth == 1) {
        # depth-1 uses all available data
        tree <- .compute_policy_tree(
          covariates[boot_result$train_idx, selected_vars, drop = FALSE],
          dr_scores[boot_result$train_idx, ],
          depth = 1,
          tree_method = tree_method
        )
      } else {
        # depth-2 uses train subset
        tree <- .compute_policy_tree(
          covariates[boot_result$train_idx, selected_vars, drop = FALSE],
          dr_scores[boot_result$train_idx, ],
          depth = 2,
          tree_method = tree_method
        )
      }

      # extract and accumulate info
      tree_info <- extract_tree_info(tree, selected_vars)
      accumulator <- update_accumulator(accumulator, tree_info, depth)

      # clean up
      rm(tree)
    }

    if (verbose && b %% 10 == 0) {
      cli::cli_progress_update()
    }
  }

  if (verbose) cli::cli_progress_done()

  # compute consensus and metrics
  consensus_info <- compute_consensus_info(accumulator, n_bootstrap, consensus_threshold)

  # build output structure compatible with margot_causal_forest
  output <- model_result # start with original

  # add bootstrap metrics
  output$bootstrap_metrics <- consensus_info$metrics

  # optionally reconstruct consensus trees
  if (return_consensus_trees) {
    if (1 %in% depths) {
      output$policy_tree_depth_1 <- reconstruct_consensus_tree(
        consensus_info$consensus_splits$depth_1,
        dr_scores[not_missing, ],
        covariates[not_missing, selected_vars, drop = FALSE],
        depth = 1
      )
    }

    if (2 %in% depths) {
      # for depth-2, need train/test split for plot_data
      set.seed(all_seeds$split_seeds[1]) # use first split seed for consistency
      train_size <- floor(median(actual_train_props) * length(not_missing))
      train_idx <- sample(not_missing, train_size)
      test_idx <- setdiff(not_missing, train_idx)

      output$policy_tree_depth_2 <- reconstruct_consensus_tree(
        consensus_info$consensus_splits$depth_2,
        dr_scores[train_idx, ],
        covariates[train_idx, selected_vars, drop = FALSE],
        depth = 2
      )

      # create plot_data
      output$plot_data <- list(
        X_test = covariates[test_idx, selected_vars, drop = FALSE],
        X_test_full = covariates[test_idx, , drop = FALSE],
        predictions = predict(
          output$policy_tree_depth_2,
          covariates[test_idx, selected_vars, drop = FALSE]
        ),
        test_indices = test_idx
      )
    }
  }

  return(output)
}

#' Create accumulator for bootstrap statistics
#' @keywords internal
create_accumulator <- function(vars, n_actions) {
  n_vars <- length(vars)
  list(
    depth_1 = list(
      var_counts = integer(n_vars),
      threshold_sums = numeric(n_vars),
      threshold_sumsq = numeric(n_vars),
      action_matrix = matrix(0, n_vars, n_actions)
    ),
    depth_2 = list(
      node1_var_counts = integer(n_vars),
      node1_threshold_sums = numeric(n_vars),
      node1_threshold_sumsq = numeric(n_vars),
      node2_var_counts = integer(n_vars),
      node2_threshold_sums = numeric(n_vars),
      node2_threshold_sumsq = numeric(n_vars)
    ),
    var_names = vars
  )
}

#' Get bootstrap indices based on variation type
#' @keywords internal
get_bootstrap_indices <- function(not_missing, vary_type, train_prop, sample_seed, split_seed) {
  n_total <- length(not_missing)

  # step 1: bootstrap sampling (only if explicitly requested)
  if (vary_type %in% c("both", "sample_only")) {
    set.seed(sample_seed)
    boot_sample <- sample(not_missing, n_total, replace = TRUE)
  } else {
    boot_sample <- not_missing
  }

  # step 2: train/test split (always use the split seed for variation)
  set.seed(split_seed)

  train_size <- floor(train_prop * length(boot_sample))
  train_positions <- sample(length(boot_sample), train_size)

  list(
    train_idx = boot_sample[train_positions],
    test_idx = boot_sample[-train_positions]
  )
}

#' Extract essential information from a policy tree
#' @keywords internal
extract_tree_info <- function(tree, var_names) {
  nodes <- tree$nodes

  # extract split info for non-leaf nodes
  splits <- list()

  for (i in seq_along(nodes)) {
    node <- nodes[[i]]
    if (!node$is_leaf) {
      var_idx <- node$split_variable
      splits[[i]] <- list(
        node_id = i,
        var_name = var_names[var_idx],
        var_idx = var_idx,
        threshold = node$split_value,
        left_child = node$left_child,
        right_child = node$right_child
      )
    } else {
      splits[[i]] <- list(
        node_id = i,
        action = node$action
      )
    }
  }

  list(
    depth = tree$depth,
    splits = splits
  )
}

#' Update accumulator with tree information
#' @keywords internal
update_accumulator <- function(accumulator, tree_info, depth) {
  if (depth == 1) {
    # only one split at root
    split <- tree_info$splits[[1]]
    if (!is.null(split$var_idx)) {
      idx <- split$var_idx
      accumulator$depth_1$var_counts[idx] <- accumulator$depth_1$var_counts[idx] + 1
      accumulator$depth_1$threshold_sums[idx] <- accumulator$depth_1$threshold_sums[idx] + split$threshold
      accumulator$depth_1$threshold_sumsq[idx] <- accumulator$depth_1$threshold_sumsq[idx] + split$threshold^2
    }
  } else if (depth == 2) {
    # process nodes 1 and 2 (first level splits)
    for (node_id in 1:2) {
      if (node_id <= length(tree_info$splits)) {
        split <- tree_info$splits[[node_id]]
        if (!is.null(split$var_idx)) {
          idx <- split$var_idx
          if (node_id == 1) {
            accumulator$depth_2$node1_var_counts[idx] <- accumulator$depth_2$node1_var_counts[idx] + 1
            accumulator$depth_2$node1_threshold_sums[idx] <- accumulator$depth_2$node1_threshold_sums[idx] + split$threshold
            accumulator$depth_2$node1_threshold_sumsq[idx] <- accumulator$depth_2$node1_threshold_sumsq[idx] + split$threshold^2
          } else {
            accumulator$depth_2$node2_var_counts[idx] <- accumulator$depth_2$node2_var_counts[idx] + 1
            accumulator$depth_2$node2_threshold_sums[idx] <- accumulator$depth_2$node2_threshold_sums[idx] + split$threshold
            accumulator$depth_2$node2_threshold_sumsq[idx] <- accumulator$depth_2$node2_threshold_sumsq[idx] + split$threshold^2
          }
        }
      }
    }
  }

  accumulator
}

#' Compute consensus information from accumulator
#' @keywords internal
compute_consensus_info <- function(accumulator, n_bootstrap, consensus_threshold) {
  # depth-1 consensus
  d1_freqs <- accumulator$depth_1$var_counts / n_bootstrap
  d1_consensus_idx <- which.max(d1_freqs)

  # handle case where no depth-1 splits were made
  if (length(d1_consensus_idx) == 0 || all(d1_freqs == 0)) {
    d1_consensus <- list(
      variable = NA,
      frequency = 0,
      threshold_mean = NA,
      threshold_sd = NA
    )
  } else {
    d1_consensus <- list(
      variable = accumulator$var_names[d1_consensus_idx],
      frequency = d1_freqs[d1_consensus_idx],
      threshold_mean = if (accumulator$depth_1$var_counts[d1_consensus_idx] > 0) {
        accumulator$depth_1$threshold_sums[d1_consensus_idx] /
          accumulator$depth_1$var_counts[d1_consensus_idx]
      } else {
        NA
      },
      threshold_sd = if (accumulator$depth_1$var_counts[d1_consensus_idx] > 0) {
        sqrt(
          accumulator$depth_1$threshold_sumsq[d1_consensus_idx] /
            accumulator$depth_1$var_counts[d1_consensus_idx] -
            (accumulator$depth_1$threshold_sums[d1_consensus_idx] /
              accumulator$depth_1$var_counts[d1_consensus_idx])^2
        )
      } else {
        NA
      }
    )
  }

  # depth-2 consensus (nodes 1 and 2)
  d2_node1_freqs <- accumulator$depth_2$node1_var_counts / n_bootstrap
  d2_node1_idx <- which.max(d2_node1_freqs)

  d2_node2_freqs <- accumulator$depth_2$node2_var_counts / n_bootstrap
  d2_node2_idx <- which.max(d2_node2_freqs)

  d2_consensus <- list(
    node1 = list(
      variable = accumulator$var_names[d2_node1_idx],
      frequency = d2_node1_freqs[d2_node1_idx],
      threshold_mean = if (accumulator$depth_2$node1_var_counts[d2_node1_idx] > 0) {
        accumulator$depth_2$node1_threshold_sums[d2_node1_idx] /
          accumulator$depth_2$node1_var_counts[d2_node1_idx]
      } else {
        NA
      }
    ),
    node2 = list(
      variable = accumulator$var_names[d2_node2_idx],
      frequency = d2_node2_freqs[d2_node2_idx],
      threshold_mean = if (accumulator$depth_2$node2_var_counts[d2_node2_idx] > 0) {
        accumulator$depth_2$node2_threshold_sums[d2_node2_idx] /
          accumulator$depth_2$node2_var_counts[d2_node2_idx]
      } else {
        NA
      }
    )
  )

  # metrics
  metrics <- list(
    var_inclusion_freq = data.frame(
      variable = accumulator$var_names,
      depth_1_freq = d1_freqs,
      depth_2_node1_freq = d2_node1_freqs,
      depth_2_node2_freq = d2_node2_freqs
    ),
    consensus_strength = list(
      depth_1 = d1_consensus$frequency,
      depth_2 = mean(c(d2_consensus$node1$frequency, d2_consensus$node2$frequency))
    )
  )

  list(
    consensus_splits = list(
      depth_1 = d1_consensus,
      depth_2 = d2_consensus
    ),
    metrics = metrics
  )
}

#' Reconstruct a consensus tree from consensus splits
#' @keywords internal
reconstruct_consensus_tree <- function(consensus_splits, dr_scores, covariates, depth) {
  # for now, fit a tree with the consensus structure
  # in future, could construct manually

  # use the tree method to fit - it will find similar splits
  # note: this is a placeholder - ideally would use actual consensus method
  tree <- .compute_policy_tree(
    covariates,
    dr_scores,
    depth = depth,
    tree_method = "policytree" # use standard method for consensus
  )

  # TODO: implement manual tree construction to exactly match consensus splits

  tree
}

#' Compute summary metrics across all models
#' @keywords internal
compute_summary_metrics <- function(results, verbose) {
  # aggregate variable importance across models
  var_importance <- do.call(rbind, lapply(names(results), function(model_name) {
    metrics <- results[[model_name]]$bootstrap_metrics$var_inclusion_freq
    metrics$model <- model_name
    metrics
  }))

  # convergence diagnostics
  consensus_strengths <- sapply(results, function(r) {
    r$bootstrap_metrics$consensus_strength$depth_1
  })

  list(
    variable_importance = var_importance,
    convergence_diagnostics = list(
      mean_consensus_strength = mean(consensus_strengths),
      min_consensus_strength = min(consensus_strengths),
      all_above_threshold = all(consensus_strengths > 0.5)
    )
  )
}

#' Summary method for bootstrap policy tree results
#'
#' @param object Object of class "margot_bootstrap_policy_tree"
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @export
#' @method summary margot_bootstrap_policy_tree
summary.margot_bootstrap_policy_tree <- function(object, ...) {
  cli::cli_h1("Bootstrap Policy Tree Summary")

  # metadata
  cli::cli_h2("Bootstrap Configuration")
  cli::cli_alert_info("Number of bootstraps: {object$metadata$n_bootstrap}")
  cli::cli_alert_info("Variation type: {object$metadata$vary_type}")
  cli::cli_alert_info("Tree method: {object$metadata$tree_method}")
  cli::cli_alert_info("Train proportion(s): {paste(object$metadata$train_proportions, collapse = ', ')}")
  cli::cli_alert_info("Consensus threshold: {object$metadata$consensus_threshold}")
  cli::cli_alert_info("Metaseed: {object$metadata$metaseed}")

  # model summaries
  cli::cli_h2("Model Results")

  for (model_name in names(object$results)) {
    cli::cli_h3(model_name)

    metrics <- object$results[[model_name]]$bootstrap_metrics

    if (!is.null(metrics$consensus_strength)) {
      cli::cli_alert_info(
        "Consensus strength - Depth 1: {format(metrics$consensus_strength$depth_1, digits = 3)}, Depth 2: {format(metrics$consensus_strength$depth_2, digits = 3)}"
      )
    }

    # top variables by inclusion frequency
    if (!is.null(metrics$var_inclusion_freq)) {
      top_vars_d1 <- metrics$var_inclusion_freq[order(metrics$var_inclusion_freq$depth_1_freq, decreasing = TRUE), ]
      top_var_d1 <- top_vars_d1$variable[1]
      top_freq_d1 <- top_vars_d1$depth_1_freq[1]

      cli::cli_alert_info(
        "Most frequent depth-1 split: {top_var_d1} ({format(top_freq_d1 * 100, digits = 1)}%)"
      )
    }
  }

  # convergence diagnostics
  if (!is.null(object$summary_metrics$convergence_diagnostics)) {
    cli::cli_h2("Convergence Diagnostics")
    diag <- object$summary_metrics$convergence_diagnostics
    cli::cli_alert_info(
      "Mean consensus strength: {format(diag$mean_consensus_strength, digits = 3)}"
    )

    if (diag$all_above_threshold) {
      cli::cli_alert_success("All models achieved consensus above threshold")
    } else {
      cli::cli_alert_warning("Some models did not achieve strong consensus")
    }
  }

  # prepare summary object
  summary_obj <- list(
    n_models = length(object$results),
    n_bootstrap = object$metadata$n_bootstrap,
    vary_type = object$metadata$vary_type,
    convergence = object$summary_metrics$convergence_diagnostics
  )

  invisible(summary_obj)
}

#' Print method for bootstrap policy tree results
#'
#' @param x Object of class "margot_bootstrap_policy_tree"
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the object
#' @export
#' @method print margot_bootstrap_policy_tree
print.margot_bootstrap_policy_tree <- function(x, ...) {
  cat("Bootstrap Policy Tree Analysis\n")
  cat("==============================\n\n")

  # basic info
  cat("Models analyzed:", length(x$results), "\n")
  cat("Bootstrap iterations:", x$metadata$n_bootstrap, "\n")
  cat("Variation type:", x$metadata$vary_type, "\n")

  # consensus strength summary
  if (!is.null(x$summary_metrics$convergence_diagnostics)) {
    cat("\nConsensus strength:\n")
    cat("  Mean:", format(x$summary_metrics$convergence_diagnostics$mean_consensus_strength, digits = 3), "\n")
    cat("  Min:", format(x$summary_metrics$convergence_diagnostics$min_consensus_strength, digits = 3), "\n")
  }

  cat("\nUse summary() for detailed results\n")

  invisible(x)
}

#' Extract variable importance from bootstrap results
#'
#' @param object Object of class "margot_bootstrap_policy_tree"
#' @param model_name Optional model name to extract. If NULL, returns all.
#' @param depth Tree depth (1 or 2). If NULL, returns average across depths.
#'
#' @return Data frame of variable importance scores
#' @export
get_variable_importance <- function(object, model_name = NULL, depth = NULL) {
  UseMethod("get_variable_importance")
}

#' @export
#' @method get_variable_importance margot_bootstrap_policy_tree
get_variable_importance.margot_bootstrap_policy_tree <- function(object, model_name = NULL, depth = NULL) {
  if (!is.null(model_name) && !model_name %in% names(object$results)) {
    stop("Model '", model_name, "' not found in results")
  }

  # extract importance data
  if (!is.null(model_name)) {
    imp_data <- object$results[[model_name]]$bootstrap_metrics$var_inclusion_freq
    imp_data$model <- model_name
  } else {
    # combine all models
    imp_data <- object$summary_metrics$variable_importance
  }

  # filter by depth if specified
  if (!is.null(depth)) {
    if (depth == 1) {
      imp_data$importance <- imp_data$depth_1_freq
    } else if (depth == 2) {
      imp_data$importance <- rowMeans(imp_data[, c("depth_2_node1_freq", "depth_2_node2_freq")])
    } else {
      stop("depth must be 1 or 2")
    }
  } else {
    # average across all splits
    imp_data$importance <- rowMeans(imp_data[, c("depth_1_freq", "depth_2_node1_freq", "depth_2_node2_freq")])
  }

  # sort by importance
  imp_data <- imp_data[order(imp_data$importance, decreasing = TRUE), ]

  # clean up columns
  keep_cols <- c("variable", "importance", "model")
  imp_data[, intersect(names(imp_data), keep_cols)]
}

#' Get consensus tree information
#'
#' @param object Object of class "margot_bootstrap_policy_tree"
#' @param model_name Model name to extract
#' @param depth Tree depth (1 or 2)
#'
#' @return List with consensus split information
#' @export
get_consensus_info <- function(object, model_name, depth = 1) {
  UseMethod("get_consensus_info")
}

#' @export
#' @method get_consensus_info margot_bootstrap_policy_tree
get_consensus_info.margot_bootstrap_policy_tree <- function(object, model_name, depth = 1) {
  if (!model_name %in% names(object$results)) {
    stop("Model '", model_name, "' not found in results")
  }

  if (!depth %in% c(1, 2)) {
    stop("depth must be 1 or 2")
  }

  # extract consensus info
  if (depth == 1) {
    object$results[[model_name]]$bootstrap_metrics$consensus_splits$depth_1
  } else {
    object$results[[model_name]]$bootstrap_metrics$consensus_splits$depth_2
  }
}

#' Interpret Bootstrap Policy Tree Results
#'
#' @description
#' Provides a narrative interpretation of bootstrap policy tree results
#' suitable for inclusion in scientific manuscripts. The interpretation
#' acknowledges the inherent instability of decision trees while focusing
#' on robust patterns that emerge across bootstrap iterations.
#'
#' @param object Object of class "margot_bootstrap_policy_tree"
#' @param model_name Model name to interpret
#' @param depth Tree depth to interpret (1, 2, or "both")
#' @param stability_threshold Minimum frequency to consider a split "stable" (default 0.7)
#' @param format Output format: "text" for narrative prose or "technical" for detailed statistics
#' @param decimal_places Number of decimal places for statistics (default 1)
#' @param include_theory Logical: Include theoretical context about tree instability (default TRUE)
#' @param label_mapping Optional named list mapping variable names to labels. If NULL,
#'   uses automatic transformation via transform_var_name()
#'
#' @return Character string containing the interpretation
#' @export
margot_interpret_bootstrap <- function(
    object,
    model_name,
    depth = 2,
    stability_threshold = 0.7,
    format = c("text", "technical"),
    decimal_places = 1,
    include_theory = TRUE,
    label_mapping = NULL) {
  format <- match.arg(format)

  # validate inputs
  if (!inherits(object, "margot_bootstrap_policy_tree")) {
    stop("object must be of class 'margot_bootstrap_policy_tree'")
  }

  if (!model_name %in% names(object$results)) {
    stop("Model '", model_name, "' not found in results")
  }

  # extract model info
  model_result <- object$results[[model_name]]
  metrics <- model_result$bootstrap_metrics
  n_bootstrap <- object$metadata$n_bootstrap
  vary_type <- object$metadata$vary_type

  # get outcome name and label
  outcome_name <- gsub("model_", "", model_name)
  outcome_label <- .apply_label_bootstrap(outcome_name, label_mapping)
  model_label <- .apply_label_bootstrap(model_name, label_mapping)

  # build interpretation text
  interpretation <- character()

  # extract key statistics
  consensus_d1 <- metrics$consensus_strength$depth_1
  consensus_d2 <- metrics$consensus_strength$depth_2
  consensus_var_d1 <- metrics$consensus_splits$depth_1$variable
  consensus_freq_d1 <- metrics$consensus_splits$depth_1$frequency
  consensus_thresh_d1 <- metrics$consensus_splits$depth_1$threshold_mean
  thresh_sd_d1 <- metrics$consensus_splits$depth_1$threshold_sd

  # get label for consensus variable
  consensus_var_d1_label <- if (!is.null(consensus_var_d1) && !is.na(consensus_var_d1)) {
    .apply_label_bootstrap(consensus_var_d1, label_mapping)
  } else {
    consensus_var_d1
  }

  # check if this is a depth-2 only analysis
  var_freq_temp <- metrics$var_inclusion_freq
  is_depth2_only <- all(var_freq_temp$depth_1_freq == 0) && any(var_freq_temp$depth_2_node1_freq > 0 | var_freq_temp$depth_2_node2_freq > 0)

  if (is_depth2_only) {
    # handle depth-2 only analysis as a normal case
    return(interpret_depth2_only(
      object = object,
      model_name = model_name,
      outcome_name = outcome_name,
      outcome_label = outcome_label,
      metrics = metrics,
      n_bootstrap = n_bootstrap,
      stability_threshold = stability_threshold,
      decimal_places = decimal_places,
      format = format,
      include_theory = include_theory,
      label_mapping = label_mapping
    ))
  }

  # handle missing values for depth-1 analysis
  if (is.null(consensus_var_d1) || is.na(consensus_var_d1) || consensus_freq_d1 == 0) {
    # get the most frequent variable from the frequency table
    var_freq_temp <- var_freq_temp[order(var_freq_temp$depth_1_freq, decreasing = TRUE), ]
    if (nrow(var_freq_temp) > 0 && var_freq_temp$depth_1_freq[1] > 0) {
      consensus_var_d1 <- var_freq_temp$variable[1]
      consensus_freq_d1 <- var_freq_temp$depth_1_freq[1]
      consensus_var_d1_label <- .apply_label_bootstrap(consensus_var_d1, label_mapping)
    } else {
      # no depth-1 results available
      cli::cli_alert_warning("No depth-1 policy trees were successfully computed")
      return(invisible("No valid policy trees could be computed in bootstrap analysis"))
    }
  }

  # add theoretical context if requested
  if (include_theory) {
    interpretation <- c(interpretation, paste0(
      "Decision trees, including policy trees, are inherently sensitive to small ",
      "changes in the data due to their hierarchical splitting structure. ",
      "When predictors are correlated, the tree may arbitrarily choose between ",
      "similar variables at split points. This variability is expected and does not ",
      "necessarily indicate absence of signal, but rather reflects the challenge of ",
      "selecting among correlated predictors. "
    ))
  }

  # main stability finding
  if (consensus_d1 >= stability_threshold) {
    stability_desc <- "demonstrated notable consistency"
    confidence_statement <- paste0(
      "Despite the inherent instability of tree-based methods, ",
      consensus_var_d1_label, " emerged as the primary decision variable in ",
      sprintf("%.*f%%", decimal_places, consensus_freq_d1 * 100),
      " of bootstrap iterations, suggesting this variable (or variables highly ",
      "correlated with it) captures important treatment effect heterogeneity."
    )
  } else if (consensus_d1 >= 0.5) {
    stability_desc <- "showed moderate consistency"
    confidence_statement <- paste0(
      "The variable ", consensus_var_d1_label, " was selected most frequently (",
      sprintf("%.*f%%", decimal_places, consensus_freq_d1 * 100),
      " of iterations), though this falls below the ",
      sprintf("%.*f%%", decimal_places, stability_threshold * 100),
      " threshold. This suggests either moderate treatment effect heterogeneity ",
      "or the presence of multiple correlated variables that capture similar information."
    )
  } else {
    stability_desc <- "exhibited the expected variability"
    confidence_statement <- paste0(
      "No single variable dominated the splitting decisions, with ",
      consensus_var_d1_label, " appearing in only ",
      sprintf("%.*f%%", decimal_places, consensus_freq_d1 * 100),
      " of iterations. This pattern is consistent with either weak treatment effect ",
      "heterogeneity or multiple correlated predictors that split the signal across variables."
    )
  }

  # opening statement
  base_statement <- paste0(
    "Bootstrap analysis of the policy tree for ", outcome_label, " ",
    stability_desc, " across ", n_bootstrap, " iterations"
  )

  # add stability threshold info if non-default
  if (stability_threshold != 0.7) {
    base_statement <- paste0(
      base_statement,
      " (using a ", sprintf("%.*f%%", decimal_places, stability_threshold * 100),
      " stability threshold)"
    )
  }

  interpretation <- c(interpretation, paste0(
    base_statement, ". ",
    confidence_statement
  ))

  # threshold information if stable
  if (consensus_d1 >= 0.5 && !is.na(thresh_sd_d1) && thresh_sd_d1 > 0) {
    interpretation <- c(interpretation, paste0(
      " The consensus threshold for ", consensus_var_d1_label, " was ",
      sprintf("%.*f", decimal_places + 1, consensus_thresh_d1),
      " (SD = ", sprintf("%.*f", decimal_places + 1, thresh_sd_d1), ")."
    ))
  }

  # competing variables
  var_freq <- metrics$var_inclusion_freq
  var_freq <- var_freq[order(var_freq$depth_1_freq, decreasing = TRUE), ]
  competitors <- var_freq[var_freq$depth_1_freq > 0.2 & var_freq$variable != consensus_var_d1, ]

  if (nrow(competitors) > 0) {
    comp_text <- paste0(
      sapply(competitors$variable, function(v) .apply_label_bootstrap(v, label_mapping)), " (",
      sprintf("%.*f%%", decimal_places, competitors$depth_1_freq * 100),
      ")"
    )
    interpretation <- c(interpretation, paste0(
      " Other frequently selected variables included ",
      paste(comp_text, collapse = ", "),
      ". These variables may be correlated with ", consensus_var_d1_label,
      " or represent alternative ways to identify similar subgroups. ",
      "The distribution of selections across multiple variables is typical ",
      "when predictors are correlated and does not invalidate the findings."
    ))
  }

  # depth-2 assessment if requested
  if (depth %in% c(2, "both") && consensus_d1 >= 0.5) {
    avg_freq_d2 <- consensus_d2
    if (avg_freq_d2 < 0.5) {
      interpretation <- c(interpretation, paste0(
        " Secondary splits (depth-2) showed substantial variability across bootstrap samples,",
        " with average consensus of only ",
        sprintf("%.*f%%", decimal_places, avg_freq_d2 * 100),
        ", indicating that more complex treatment rules are not well-supported by the data."
      ))
    } else if (avg_freq_d2 >= stability_threshold) {
      interpretation <- c(interpretation, paste0(
        " Secondary splits also demonstrated stability (average consensus ",
        sprintf("%.*f%%", decimal_places, avg_freq_d2 * 100),
        "), supporting a more nuanced treatment assignment strategy."
      ))
    }
  }

  # clinical/practical implications
  if (consensus_d1 >= stability_threshold) {
    # get treatment recommendations from consensus tree
    if (!is.null(model_result$policy_tree_depth_1)) {
      tree_obj <- model_result$policy_tree_depth_1
      actions <- tree_obj$action.names

      # determine which action is recommended for each split
      root_node <- tree_obj$nodes[[1]]
      left_action <- actions[tree_obj$nodes[[root_node$left_child]]$action]
      right_action <- actions[tree_obj$nodes[[root_node$right_child]]$action]

      interpretation <- c(interpretation, paste0(
        " The consensus suggests considering ", tolower(left_action),
        " for individuals with lower values of ", consensus_var_d1_label,
        " (<= ", sprintf("%.*f", decimal_places + 1, consensus_thresh_d1),
        ") and ", tolower(right_action), " for those with higher values",
        " (> ", sprintf("%.*f", decimal_places + 1, consensus_thresh_d1), "). ",
        "However, practitioners should consider that other correlated variables ",
        "may serve as reasonable proxies, and validation in independent samples ",
        "is recommended before implementation."
      ))
    }
  } else {
    interpretation <- c(interpretation, paste0(
      " The high variability in selected variables suggests that either ",
      "treatment effect heterogeneity is limited for ", outcome_label,
      ", or that multiple correlated predictors capture similar information. ",
      "In such cases, uniform treatment assignment or additional research ",
      "to identify stronger effect modifiers may be warranted."
    ))
  }

  # technical details if requested
  if (format == "technical") {
    tech_details <- paste0(
      "\n\nTechnical details: Bootstrap procedure used ",
      switch(vary_type,
        "both" = "both observation resampling and train/test split variation",
        "sample_only" = "observation resampling with fixed train/test splits",
        "split_only" = "train/test split variation with fixed observations"
      ),
      ". Variable importance (bootstrap inclusion frequency) was: ",
      paste(
        sprintf(
          "%s (%.1f%%)", var_freq$variable[1:min(5, nrow(var_freq))],
          var_freq$depth_1_freq[1:min(5, nrow(var_freq))] * 100
        ),
        collapse = ", "
      ),
      "."
    )
    interpretation <- c(interpretation, tech_details)
  }

  # combine and return
  full_text <- paste(interpretation, collapse = "")

  # print to console
  cat(strwrap(full_text, width = 80), sep = "\n")

  # return invisibly
  invisible(full_text)
}

#' Interpret depth-2 only bootstrap results
#' @keywords internal
interpret_depth2_only <- function(object, model_name, outcome_name, outcome_label, metrics,
                                  n_bootstrap, stability_threshold,
                                  decimal_places, format, include_theory = TRUE,
                                  label_mapping = NULL) {
  interpretation <- character()

  # add theoretical context if requested
  if (include_theory) {
    interpretation <- c(interpretation, paste0(
      "Depth-2 policy trees are particularly sensitive to data perturbations due to ",
      "their hierarchical structure, where instability compounds at each split level. ",
      "The variability in selected variables often reflects correlations among predictors ",
      "rather than absence of signal. "
    ))
  }

  # get depth-2 statistics
  var_freq <- metrics$var_inclusion_freq
  var_freq <- var_freq[order(rowMeans(var_freq[, c("depth_2_node1_freq", "depth_2_node2_freq")]),
    decreasing = TRUE
  ), ]

  # get top variables and their frequencies
  top_vars <- head(var_freq, 5)
  top_var <- top_vars$variable[1]
  top_var_label <- .apply_label_bootstrap(top_var, label_mapping)

  # calculate average frequencies for top variables
  top_var_avg_freq <- mean(c(top_vars$depth_2_node1_freq[1], top_vars$depth_2_node2_freq[1]))

  # get consensus info for depth-2
  consensus_d2 <- metrics$consensus_strength$depth_2
  d2_splits <- metrics$consensus_splits$depth_2

  # determine stability
  if (consensus_d2 >= stability_threshold) {
    stability_desc <- "demonstrated notable consistency despite the added complexity"
  } else if (consensus_d2 >= 0.5) {
    stability_desc <- "showed moderate consistency"
  } else {
    stability_desc <- "exhibited the expected high variability"
  }

  # opening statement
  base_statement <- paste0(
    "Bootstrap analysis of depth-2 policy trees for ", outcome_label, " ",
    stability_desc, " across ", n_bootstrap, " iterations"
  )

  # add stability threshold info if non-default
  if (stability_threshold != 0.7) {
    base_statement <- paste0(
      base_statement,
      " (using a ", sprintf("%.*f%%", decimal_places, stability_threshold * 100),
      " stability threshold)"
    )
  }

  interpretation <- c(interpretation, paste0(base_statement, "."))

  # main finding about top variables
  if (consensus_d2 >= 0.5) {
    interpretation <- c(interpretation, paste0(
      " The most consistently selected variables were ",
      top_var_label, " (appearing in ",
      sprintf("%.*f%%", decimal_places, top_var_avg_freq * 100),
      " of nodes on average)"
    ))

    # add other top variables if they're frequent enough
    other_top_vars <- top_vars[2:min(3, nrow(top_vars)), ]
    if (nrow(other_top_vars) > 0) {
      other_freq <- rowMeans(other_top_vars[, c("depth_2_node1_freq", "depth_2_node2_freq")])
      if (any(other_freq > 0.2)) {
        other_vars_labeled <- sapply(
          other_top_vars$variable[other_freq > 0.2],
          function(v) .apply_label_bootstrap(v, label_mapping)
        )
        other_text <- paste0(
          other_vars_labeled, " (",
          sprintf("%.*f%%", decimal_places, other_freq[other_freq > 0.2] * 100),
          ")"
        )
        interpretation <- c(interpretation, paste0(
          ", followed by ",
          paste(other_text, collapse = " and ")
        ))
      }
    }
    interpretation <- c(interpretation, ".")

    # describe split patterns if available
    if (!is.null(d2_splits$node1) && !is.null(d2_splits$node2)) {
      if (!is.na(d2_splits$node1$variable) && d2_splits$node1$frequency >= 0.5) {
        interpretation <- c(interpretation, paste0(
          " The first split most frequently occurred on ",
          .apply_label_bootstrap(d2_splits$node1$variable, label_mapping), " (consensus ",
          sprintf("%.*f%%", decimal_places, d2_splits$node1$frequency * 100),
          ")"
        ))

        if (!is.na(d2_splits$node1$threshold_mean)) {
          interpretation <- c(interpretation, paste0(
            " at a threshold of ",
            sprintf("%.*f", decimal_places + 1, d2_splits$node1$threshold_mean)
          ))

          if (!is.na(d2_splits$node1$threshold_sd) && d2_splits$node1$threshold_sd > 0) {
            interpretation <- c(interpretation, paste0(
              " (SD = ", sprintf("%.*f", decimal_places + 1, d2_splits$node1$threshold_sd), ")"
            ))
          }
        }
        interpretation <- c(interpretation, ".")
      }
    }
  } else {
    # low stability case
    interpretation <- c(interpretation, paste0(
      " The most frequent variable (", top_var_label, ") appeared in only ",
      sprintf("%.*f%%", decimal_places, top_var_avg_freq * 100),
      " of nodes on average. This high variability is typical for depth-2 trees, ",
      "particularly when multiple correlated predictors compete for selection. ",
      "The lack of a dominant splitting pattern may indicate either limited ",
      "treatment effect heterogeneity or that the heterogeneity is distributed ",
      "across multiple correlated dimensions."
    ))
  }

  # practical implications
  model_result <- object$results[[model_name]]
  if (consensus_d2 >= stability_threshold && !is.null(model_result$policy_tree_depth_2)) {
    interpretation <- c(interpretation, paste0(
      " The depth-2 policy tree provides a nuanced treatment assignment strategy",
      " based on combinations of baseline characteristics, though users should",
      " validate these rules in independent samples before clinical implementation."
    ))
  } else if (consensus_d2 < 0.5) {
    interpretation <- c(interpretation, paste0(
      " The high variability in tree structure is common for depth-2 trees and ",
      "suggests caution in implementing complex treatment rules for ", outcome_label, "."
    ))
  }

  # technical details if requested
  if (format == "technical") {
    tech_details <- paste0(
      "\n\nTechnical details: Bootstrap used ", n_bootstrap, " iterations with ",
      object$metadata$vary_type, " variation. ",
      "Depth-2 variable inclusion frequencies (node1/node2): ",
      paste(
        paste0(
          var_freq$variable[1:min(5, nrow(var_freq))], " (",
          sprintf("%.*f", decimal_places, var_freq$depth_2_node1_freq[1:min(5, nrow(var_freq))] * 100),
          "/",
          sprintf("%.*f%%", decimal_places, var_freq$depth_2_node2_freq[1:min(5, nrow(var_freq))] * 100),
          ")"
        ),
        collapse = ", "
      ),
      "."
    )
    interpretation <- c(interpretation, tech_details)
  }

  # combine and return
  full_text <- paste(interpretation, collapse = "")

  # print to console
  cat(strwrap(full_text, width = 80), sep = "\n")

  # return invisibly
  invisible(full_text)
}
