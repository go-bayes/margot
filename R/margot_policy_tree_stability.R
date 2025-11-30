# internal helper function for applying labels
#' @keywords internal
.apply_label_stability <- function(var_name, label_mapping = NULL) {
  if (!is.null(label_mapping) && var_name %in% names(label_mapping)) {
    return(label_mapping[[var_name]])
  }
  # fall back to transform_var_name with silent mode; respect global option for acronym expansion
  expand <- isTRUE(getOption("margot.expand_acronyms", FALSE))
  suppressMessages(transform_var_name(var_name, label_mapping, expand_acronyms = expand))
}

#' Stability Analysis for Policy Trees
#'
#' @description
#' Performs stability analysis of policy trees to assess robustness and generate
#' consensus trees. By default, varies random seeds to create different train/test
#' splits to assess stability. Optionally supports bootstrap resampling for
#' traditional bootstrap analysis. Uses memory-efficient streaming approach to
#' handle large datasets.
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
#' @param n_iterations Integer. Number of stability iterations (default 300).
#' @param vary_type Character. Type of variation: "split_only" (vary train/test split via seeds),
#'   "bootstrap" (bootstrap resample), "both" (resample + split). Default is "split_only".
#' @param consensus_threshold Numeric. Minimum inclusion frequency for consensus (default 0.5).
#' @param train_proportion Numeric. Train/test split when vary_train_proportion = FALSE (default 0.5).
#' @param vary_train_proportion Logical. Whether to vary train proportion (default FALSE).
#' @param train_proportions Numeric vector. Proportions to cycle through when
#'   vary_train_proportion = TRUE (default c(0.4, 0.5, 0.6, 0.7)).
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param return_consensus_trees Logical. Return fitted consensus trees (default TRUE).
#' @param metaseed Integer. Master seed for reproducibility (default 12345).
#' @param parrallel Deprecated misspelling of `parallel`; retained for backwards compatibility.
#' @param parallel Logical. Use parallel processing (default FALSE).
#' @param n_cores Integer. Number of cores for parallel processing.
#' @param verbose Logical. Print progress messages (default TRUE).
#' @param seed Integer. Additional seed parameter for compatibility (default 12345).
#' @param tree_method Character string specifying the package to use: "fastpolicytree"
#'   (default) or "policytree". The fastpolicytree package provides ~10x faster
#'   computation, which is particularly beneficial for stability analysis. Falls
#'   back to policytree if fastpolicytree is not installed.
#' @param n_bootstrap Deprecated. Use n_iterations instead.
#' @param compute_policy_values Logical. If TRUE, compute bootstrap policy-value tests for
#'   each consensus tree and store results in `policy_value_depth_*` slots (default FALSE).
#' @param policy_value_R Integer ≥ 1. Number of bootstrap replicates for policy values when
#'   `compute_policy_values = TRUE` (default 499).
#' @param policy_value_seed Optional integer seed for policy-value bootstraps. Use NULL to
#'   inherit the ambient RNG state.
#' @param policy_value_baseline Contrast baseline for stored policy values when
#'   `compute_policy_values = TRUE`. One of "control_all" (default) or "treat_all".
#' @param future_globals_max_size Maximum size (in bytes) of globals that can be exported to
#'   parallel workers when `parallel = TRUE`. Defaults to 20 GiB. Increase this if large model
#'   objects trigger the `future.globals.maxSize` guard.
#'
#' @return Object of class "margot_stability_policy_tree" containing:
#' \itemize{
#'   \item results: List with consensus trees and stability metrics per model
#'   \item summary_metrics: Variable importance and convergence diagnostics
#'   \item metadata: Analysis parameters and seeds used
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
#' When `compute_policy_values = TRUE`, each consensus tree is also evaluated
#' against the specified baseline and bootstrap policy-value estimates are stored
#' in `policy_value_depth_1` / `policy_value_depth_2`. This allows downstream
#' reporters to reuse consistent estimates without rerunning the bootstrap.
#'
#' By default, the function varies random seeds to create different train/test splits
#' for each iteration. This assesses the stability of the policy tree structure
#' without the computational overhead and statistical assumptions of bootstrap
#' resampling. True bootstrap resampling can be enabled with vary_type = "bootstrap".
#'
#' @section Theoretical Background:
#' Policy trees inherit the instability of decision trees, where small changes in
#' the data can lead to completely different tree structures (Breiman, 1996). This
#' instability is particularly pronounced when predictors are correlated, as the tree
#' can arbitrarily choose between similar variables at split points. Athey and Wager's
#' (2021) policy learning framework acknowledges these challenges while providing
#' methods to extract robust insights despite the instability.
#'
#' The stability analysis helps distinguish between:
#' \itemize{
#'   \item Fundamental instability due to weak or absent treatment effect heterogeneity
#'   \item Apparent instability due to correlated predictors that capture similar information
#'   \item Robust patterns that emerge consistently across different data samples
#' }
#'
#' Use the companion functions `margot_assess_variable_correlation()` and
#' `margot_stability_diagnostics()` to better understand the sources of instability.
#'
#' Three types of variation are supported:
#' \itemize{
#'   \item "split_only": Fixed sample, only varies train/test split (default)
#'   \item "bootstrap": Bootstrap resampling with replacement
#'   \item "both": Varies both bootstrap sampling and train/test splits
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
#' # 2. Run stability analysis to assess robustness
#' stability_results <- margot_policy_tree_stability(
#'   cf_results,
#'   n_iterations = 300,
#'   tree_method = "fastpolicytree"  # 10x faster if available
#' )
#'
#' # 3. Check variable correlations
#' cor_analysis <- margot_assess_variable_correlation(
#'   cf_results,  # Use original results, NOT stability_results
#'   "model_outcome1"
#' )
#'
#' # 4. Identify clusters of correlated variables
#' clusters <- margot_identify_variable_clusters(cor_analysis)
#'
#' # 5. Get comprehensive diagnostics
#' diagnostics <- margot_stability_diagnostics(
#'   stability_results = stability_results,
#'   model_results = cf_results,
#'   model_name = "model_outcome1"
#' )
#'
#' # 6. Interpret results
#' interpretation <- margot_interpret_stability(
#'   stability_results,
#'   "model_outcome1",
#'   include_theory = TRUE  # Include theoretical context
#' )
#' }
#'
#' @examples
#' \dontrun{
#' # Basic stability analysis with fixed train proportion
#' stability_results <- margot_policy_tree_stability(
#'   causal_forest_results,
#'   n_iterations = 300
#' )
#'
#' # Vary train proportion with default values
#' stability_results <- margot_policy_tree_stability(
#'   causal_forest_results,
#'   vary_train_proportion = TRUE
#' )
#'
#' # Custom train proportions
#' stability_results <- margot_policy_tree_stability(
#'   causal_forest_results,
#'   vary_train_proportion = TRUE,
#'   train_proportions = c(0.3, 0.5, 0.7)
#' )
#'
#' # Use bootstrap resampling instead of seed variation
#' stability_results <- margot_policy_tree_stability(
#'   causal_forest_results,
#'   vary_type = "bootstrap",
#'   n_iterations = 300
#' )
#'
#' # Plot consensus tree
#' margot_plot_policy_tree(stability_results, "model_anxiety")
#'
#' # Get stability summary
#' summary(stability_results)
#'
#' # Interpret results with theoretical context
#' interpretation <- margot_interpret_stability(
#'   stability_results,
#'   "model_anxiety",
#'   format = "text"
#' )
#'
#' # Assess variable correlations (using original causal forest results)
#' cor_analysis <- margot_assess_variable_correlation(
#'   causal_forest_results, # NOT stability_results
#'   "model_anxiety"
#' )
#'
#' # Identify variable clusters
#' clusters <- margot_identify_variable_clusters(cor_analysis)
#'
#' # Run comprehensive stability diagnostics
#' diagnostics <- margot_stability_diagnostics(
#'   stability_results = stability_results,
#'   model_results = causal_forest_results,
#'   model_name = "model_anxiety"
#' )
#' }
#'
#' @seealso
#' \code{\link{margot_policy_tree}} for computing policy trees without stability analysis
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
margot_policy_tree_stability <- function(
    model_results,
    model_names = NULL,
    custom_covariates = NULL,
    exclude_covariates = NULL,
    covariate_mode = c("original", "custom", "add", "all"),
    depth = 2,
    n_iterations = 300,
    vary_type = c("split_only", "bootstrap", "both"),
    consensus_threshold = 0.5,
    train_proportion = 0.5,
    vary_train_proportion = FALSE,
    train_proportions = c(0.4, 0.5, 0.6, 0.7),
    label_mapping = NULL,
    return_consensus_trees = TRUE,
    metaseed = 12345,
    parrallel = NULL,
    parallel = FALSE,
    n_cores = NULL,
    verbose = TRUE,
    seed = 12345,
    tree_method = c("fastpolicytree", "policytree"),
    n_bootstrap = NULL, # deprecated parameter for backwards compatibility
    compute_policy_values = FALSE,
    policy_value_R = 499L,
    policy_value_seed = 42L,
    policy_value_baseline = c("control_all", "treat_all"),
    future_globals_max_size = 20 * 1024^3
    ) {
  # handle deprecated n_bootstrap parameter
  if (!is.null(n_bootstrap)) {
    if (!missing(n_iterations) && n_iterations != 300) {
      stop("Both n_iterations and n_bootstrap specified. Please use only n_iterations.")
    }
    cli::cli_alert_warning(
      "Parameter 'n_bootstrap' is deprecated. Please use 'n_iterations' instead."
    )
    n_iterations <- n_bootstrap
  }

  # backwards-compatible alias for misspelled argument
  if (!is.null(parrallel)) {
    cli::cli_alert_warning(
      "Argument 'parrallel' is deprecated; use 'parallel' instead."
    )
    parallel <- isTRUE(parrallel)
  }

  policy_value_baseline <- match.arg(policy_value_baseline)
  policy_value_R <- as.integer(policy_value_R)
  if (is.na(policy_value_R) || policy_value_R < 1L) {
    stop("policy_value_R must be a positive integer")
  }
  if (!is.null(policy_value_seed)) {
    policy_value_seed <- as.integer(policy_value_seed)
  }

  if (!is.null(future_globals_max_size)) {
    future_globals_max_size <- as.numeric(future_globals_max_size)
    if (is.na(future_globals_max_size) || future_globals_max_size <= 0) {
      stop("future_globals_max_size must be a positive number of bytes or NULL")
    }
  }

  if (isTRUE(compute_policy_values) && !isTRUE(return_consensus_trees)) {
    if (verbose) {
      cli::cli_alert_warning(
        "compute_policy_values = TRUE requires return_consensus_trees = TRUE; enabling consensus tree reconstruction."
      )
    }
    return_consensus_trees <- TRUE
  }

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
    cli::cli_h1("Policy Tree Stability Analysis")
    cli::cli_alert_info("Processing {length(model_names)} model{?s} with {n_iterations} iterations")

    # determine actual variation type based on parameters
    actual_variation <- if (!vary_train_proportion && vary_type == "split_only") {
      "train/test split variation"
    } else if (vary_train_proportion && vary_type == "split_only") {
      "train/test split + proportion variation"
    } else if (vary_type == "bootstrap") {
      "bootstrap resampling"
    } else {
      "bootstrap resampling + train/test splits"
    }
    cli::cli_alert_info("Analysis type: {actual_variation}")
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
  all_seeds <- generate_stability_seeds(metaseed, n_iterations)

  # determine actual train proportions for each iteration
  if (vary_train_proportion) {
    actual_train_props <- rep_len(train_proportions, n_iterations)
  } else {
    actual_train_props <- rep(train_proportion, n_iterations)
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
      n_iterations = n_iterations,
      vary_type = vary_type,
      vary_train_proportion = vary_train_proportion,
      train_proportions = if (vary_train_proportion) train_proportions else train_proportion,
      consensus_threshold = consensus_threshold,
      tree_method = actual_tree_method,
      compute_policy_values = compute_policy_values,
      policy_value_R = policy_value_R,
      policy_value_seed = policy_value_seed,
      policy_value_baseline = policy_value_baseline,
      future_globals_max_size = future_globals_max_size,
      timestamp = Sys.time(),
      seeds_used = all_seeds
    ),
    # preserve key data from original results
    outcome_vars = model_results$outcome_vars,
    not_missing = not_missing,
    data = model_results$data,
    covariates = covariates,
    weights = model_results$weights,
    W = model_results$W
  )

  # process each model (possibly in parallel across models)
  run_one_model <- function(model_name) {
    # Avoid oversubscription inside workers
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
      try(RhpcBLASctl::blas_set_num_threads(1), silent = TRUE)
    }
    Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
    if (verbose) cli::cli_h2("Processing {model_name}")
    model_result <- model_results$results[[model_name]]
    res <- stability_single_model(
      model_result = model_result,
      model_name = model_name,
      covariates = covariates,
      not_missing = not_missing,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      covariate_mode = covariate_mode,
      n_iterations = n_iterations,
      vary_type = vary_type,
      depths = depths,
      actual_train_props = actual_train_props,
      all_seeds = all_seeds,
      consensus_threshold = consensus_threshold,
      return_consensus_trees = return_consensus_trees,
      label_mapping = label_mapping,
      verbose = verbose,
      seed = seed,
      tree_method = actual_tree_method,
      compute_policy_values = compute_policy_values,
      policy_value_R = policy_value_R,
      policy_value_seed = policy_value_seed,
      policy_value_baseline = policy_value_baseline
    )
    list(name = model_name, result = res)
  }

  use_parallel <- isTRUE(parallel) && length(model_names) >= 3 && (n_iterations >= 200)
  results_list <- list()
  if (use_parallel && requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)) {
    # configure workers
    nc <- if (!is.null(n_cores)) as.integer(n_cores) else max(1L, parallel::detectCores(logical = TRUE) - 1L)
    if (verbose) cli::cli_alert_info("Running across models in parallel ({nc} workers)")
    old_plan <- NULL
    old_future_size <- getOption("future.globals.maxSize")
    try({ old_plan <- future::plan() }, silent = TRUE)
    future::plan(future::multisession, workers = nc)
    if (!is.null(future_globals_max_size)) {
      options(future.globals.maxSize = future_globals_max_size)
    }
    on.exit({
      if (!is.null(old_plan)) future::plan(old_plan) else future::plan(future::sequential)
      options(future.globals.maxSize = old_future_size)
    }, add = TRUE)
    results_list <- future.apply::future_lapply(model_names, run_one_model, future.seed = seed)
  } else {
    if (isTRUE(parallel) && !use_parallel && verbose) {
      cli::cli_alert_info("Parallel disabled (small job); running sequentially")
    }
    results_list <- lapply(model_names, run_one_model)
  }
  # assemble
  for (el in results_list) {
    output$results[[el$name]] <- el$result
  }

  # compute summary metrics across all models
  output$summary_metrics <- compute_stability_summary_metrics(output$results, verbose)

  # attach outcome vars for downstream helpers
  output$outcome_vars <- gsub("^model_", "", names(output$results))

  # optional compact policy-value summary if present
  output$policy_value_summary <- compute_policy_value_summary(output$results)

  if (verbose) cli::cli_alert_success("Stability analysis completed")

  # set class for S3 methods
  class(output) <- c("margot_stability_policy_tree", "margot_policy_tree", "list")

  return(output)
}

#' Generate stability seeds from metaseed
#' @keywords internal
generate_stability_seeds <- function(metaseed, n_iterations) {
  set.seed(metaseed)
  list(
    sample_seeds = sample.int(.Machine$integer.max, n_iterations),
    split_seeds = sample.int(.Machine$integer.max, n_iterations)
  )
}

#' Stability analysis for a single model
#' @keywords internal
stability_single_model <- function(
    model_result,
    model_name,
    covariates,
    not_missing,
    custom_covariates,
    exclude_covariates,
    covariate_mode,
    n_iterations,
    vary_type,
    depths,
    actual_train_props,
    all_seeds,
    consensus_threshold,
    return_consensus_trees,
    label_mapping,
    verbose,
    seed,
    tree_method,
    compute_policy_values,
    policy_value_R,
    policy_value_seed,
    policy_value_baseline) {
  # get DR scores (prefer original; flipped is for reporting)
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
  accumulator <- create_stability_accumulator(
    vars = selected_vars,
    n_actions = ncol(dr_scores)
  )

  # progress bar
  if (verbose) {
    pb <- cli::cli_progress_bar(
      total = n_iterations,
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_eta}"
    )
  }

  # stability loop
  for (iter in 1:n_iterations) {
    # get train indices based on vary_type
    indices <- get_stability_indices(
      not_missing = not_missing,
      vary_type = vary_type,
      train_prop = actual_train_props[iter],
      sample_seed = all_seeds$sample_seeds[iter],
      split_seed = all_seeds$split_seeds[iter]
    )

    # fit trees and extract info
    for (depth in depths) {
      if (depth == 1) {
        # depth-1 uses the sampled training subset
        tree <- .compute_policy_tree(
          covariates[indices$train_idx, selected_vars, drop = FALSE],
          dr_scores[indices$train_idx, ],
          depth = 1,
          tree_method = tree_method
        )
      } else {
        # depth-2 uses train subset
        tree <- .compute_policy_tree(
          covariates[indices$train_idx, selected_vars, drop = FALSE],
          dr_scores[indices$train_idx, ],
          depth = 2,
          tree_method = tree_method
        )
      }

      # extract and accumulate info
      tree_info <- extract_tree_info(tree, selected_vars)
      accumulator <- update_stability_accumulator(accumulator, tree_info, depth)

      # clean up
      rm(tree)
    }

    if (verbose && iter %% 10 == 0) {
      cli::cli_progress_update()
    }
  }

  if (verbose) cli::cli_progress_done()

  # compute consensus and metrics
  consensus_info <- compute_consensus_info(accumulator, n_iterations, consensus_threshold)

  # build output structure compatible with margot_causal_forest
  output <- model_result # start with original

  # add stability metrics (persist consensus splits for interpreters)
  output$stability_metrics <- consensus_info$metrics
  output$stability_metrics$consensus_splits <- consensus_info$consensus_splits

  # ensure key data is preserved
  if (is.null(output$Y)) output$Y <- model_result$Y
  if (is.null(output$W)) output$W <- model_result$W
  if (is.null(output$dr_scores)) output$dr_scores <- dr_scores
  if (is.null(output$dr_scores_flipped)) output$dr_scores_flipped <- model_result$dr_scores_flipped

  # establish a consistent train/test split for consensus reconstruction and evaluation
  if (length(not_missing) > 1L) {
    set.seed(all_seeds$split_seeds[1])
    train_prop_use <- median(actual_train_props)
    train_size <- floor(train_prop_use * length(not_missing))
    if (!is.finite(train_size)) train_size <- floor(0.5 * length(not_missing))
    train_size <- max(1L, min(length(not_missing) - 1L, train_size))
    if (train_size >= length(not_missing)) train_size <- length(not_missing) - 1L
    if (train_size < 1L) train_size <- 1L
    train_idx_consensus <- sample(not_missing, train_size)
    test_idx_consensus <- setdiff(not_missing, train_idx_consensus)
    if (!length(test_idx_consensus)) {
      # fallback: use a different sample for test set
      test_idx_consensus <- setdiff(not_missing, train_idx_consensus[1])
      if (!length(test_idx_consensus)) test_idx_consensus <- train_idx_consensus
    }
  } else {
    train_idx_consensus <- not_missing
    test_idx_consensus <- not_missing
  }

  # optionally reconstruct consensus trees
  if (return_consensus_trees) {
    if (1 %in% depths) {
      output$policy_tree_depth_1 <- reconstruct_consensus_tree(
        consensus_info$consensus_splits$depth_1,
        dr_scores[train_idx_consensus, ],
        covariates[train_idx_consensus, selected_vars, drop = FALSE],
        depth = 1
      )
    }

    if (2 %in% depths) {
      output$policy_tree_depth_2 <- reconstruct_consensus_tree(
        consensus_info$consensus_splits$depth_2,
        dr_scores[train_idx_consensus, ],
        covariates[train_idx_consensus, selected_vars, drop = FALSE],
        depth = 2
      )
    }
  }

  # ensure plot_data exists for evaluation even if only depth-1 was requested
  if (is.null(output$plot_data)) {
    preds <- NULL
    prediction_tree <- NULL
    if (!is.null(output$policy_tree_depth_2) && !is.null(output$policy_tree_depth_2$columns)) {
      prediction_tree <- output$policy_tree_depth_2
    } else if (!is.null(output$policy_tree_depth_1) && !is.null(output$policy_tree_depth_1$columns)) {
      prediction_tree <- output$policy_tree_depth_1
    }
    if (!is.null(prediction_tree)) {
      cols_use <- prediction_tree$columns
      if (is.null(cols_use)) {
        cols_use <- selected_vars
      } else {
        cols_use <- intersect(cols_use, colnames(covariates))
        if (!length(cols_use)) cols_use <- selected_vars
      }
      preds <- tryCatch(
        predict(prediction_tree, covariates[test_idx_consensus, cols_use, drop = FALSE]),
        error = function(e) NULL
      )
    }
    output$plot_data <- list(
      X_test = covariates[test_idx_consensus, selected_vars, drop = FALSE],
      X_test_full = covariates[test_idx_consensus, , drop = FALSE],
      predictions = preds,
      test_indices = test_idx_consensus
    )
  } else {
    # augment existing plot_data with consistent indices if missing
    if (is.null(output$plot_data$test_indices)) {
      output$plot_data$test_indices <- test_idx_consensus
    }
  }

  if (isTRUE(compute_policy_values)) {
    available_depths <- intersect(depths, c(1L, 2L))
    available_depths <- available_depths[vapply(available_depths, function(d) {
      !is.null(output[[paste0("policy_tree_depth_", d)]])
    }, logical(1))]

    if (length(available_depths)) {
      compute_pv <- NULL
      if (exists("margot_compute_policy_value", mode = "function")) {
        compute_pv <- margot_compute_policy_value
      } else {
        compute_pv <- tryCatch(
          getFromNamespace("margot_compute_policy_value", "margot"),
          error = function(e) NULL
        )
      }
      if (is.null(compute_pv)) {
        compute_pv <- .compute_policy_value_internal
      }

      for (idx in seq_along(available_depths)) {
        d <- available_depths[idx]
        seed_use <- if (is.null(policy_value_seed)) NULL else policy_value_seed + (idx - 1L)
        pv_res <- tryCatch(
          compute_pv(
            output,
            depth = d,
            R = policy_value_R,
            seed = seed_use,
            baseline = policy_value_baseline
          ),
          error = function(e) NULL
        )

        if (!is.null(pv_res) && is.list(pv_res)) {
          if (is.null(pv_res$p.value) || is.na(pv_res$p.value)) {
            if (!is.null(pv_res$std.err) && is.finite(pv_res$std.err) && pv_res$std.err > 0) {
              pv_res$p.value <- 2 * stats::pnorm(-abs(pv_res$estimate / pv_res$std.err))
            } else {
              pv_res$p.value <- NA_real_
            }
          }
          pv_res$baseline <- policy_value_baseline
          pv_res$contrast <- if (policy_value_baseline == "control_all") {
            "policy - control_all"
          } else {
            "policy - treat_all"
          }
          output[[paste0("policy_value_depth_", d)]] <- pv_res
        }
      }
    }
  }

  return(output)
}

#' Create accumulator for stability statistics
#' @keywords internal
create_stability_accumulator <- function(vars, n_actions) {
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

#' Get stability indices based on variation type
#' @keywords internal
get_stability_indices <- function(not_missing, vary_type, train_prop, sample_seed, split_seed) {
  n_total <- length(not_missing)

  # step 1: bootstrap sampling (only if explicitly requested)
  if (vary_type %in% c("both", "bootstrap")) {
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

#' Update accumulator with tree information for stability analysis
#' @keywords internal
update_stability_accumulator <- function(accumulator, tree_info, depth) {
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

#' Compute summary metrics across all models for stability analysis
#' @keywords internal
compute_stability_summary_metrics <- function(results, verbose) {
  # aggregate variable importance across models
  var_importance <- do.call(rbind, lapply(names(results), function(model_name) {
    metrics <- results[[model_name]]$stability_metrics$var_inclusion_freq
    metrics$model <- model_name
    metrics
  }))

  # convergence diagnostics
  consensus_strengths <- sapply(results, function(r) {
    r$stability_metrics$consensus_strength$depth_1
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

#' Summary method for stability policy tree results
#'
#' @param object Object of class "margot_stability_policy_tree"
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns a summary list
#' @export
#' @method summary margot_stability_policy_tree
summary.margot_stability_policy_tree <- function(object, ...) {
  dots <- list(...)
  pv_R <- if (!is.null(dots$policy_value_R)) as.integer(dots$policy_value_R) else 199L
  pv_seed <- if (!is.null(dots$policy_value_seed)) as.integer(dots$policy_value_seed) else 42L
  show_pv <- if (!is.null(dots$show_policy_value)) isTRUE(dots$show_policy_value) else TRUE
  cli::cli_h1("Policy Tree Stability Summary")

  # metadata
  cli::cli_h2("Analysis Configuration")
  cli::cli_alert_info("Number of iterations: {object$metadata$n_iterations}")
  cli::cli_alert_info("Variation type: {object$metadata$vary_type}")
  cli::cli_alert_info("Tree method: {object$metadata$tree_method}")
  cli::cli_alert_info("Train proportion(s): {paste(object$metadata$train_proportions, collapse = ', ')}")
  cli::cli_alert_info("Consensus threshold: {object$metadata$consensus_threshold}")
  cli::cli_alert_info("Metaseed: {object$metadata$metaseed}")

  # model summaries
  cli::cli_h2("Model Results")

  for (model_name in names(object$results)) {
    cli::cli_h3(model_name)

    metrics <- object$results[[model_name]]$stability_metrics

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

  # consensus policy value (depth 2) quick summary if evaluable
  if (show_pv) {
    cli::cli_h2("Consensus Policy Value (Depth 2)")
    pv_tbl <- tryCatch({
      margot_report_consensus_policy_value(object, depths = 2L, R = pv_R, seed = pv_seed, verbose = FALSE)
    }, error = function(e) NULL)

    if (!is.null(pv_tbl) && nrow(pv_tbl) > 0) {
      by_model <- split(pv_tbl, pv_tbl$model)
      for (nm in names(by_model)) {
        sub <- by_model[[nm]]
        treat_all <- sub[sub$contrast == "policy - treat_all", , drop = FALSE]
        control_all <- sub[sub$contrast == "policy - control_all", , drop = FALSE]
        out_label <- gsub("^model_", "", nm)
        if (nrow(treat_all)) {
          cli::cli_alert_info(
            "{out_label}: vs treat-all = {round(treat_all$estimate, 3)} [ {round(treat_all$ci_lo, 3)}, {round(treat_all$ci_hi, 3)} ]"
          )
        }
        if (nrow(control_all)) {
          cli::cli_alert_info(
            "{out_label}: vs control-all = {round(control_all$estimate, 3)} [ {round(control_all$ci_lo, 3)}, {round(control_all$ci_hi, 3)} ]"
          )
        }
      }
    } else {
      cli::cli_alert_info("No evaluable consensus trees; run margot_report_consensus_policy_value() or rerun with compute_policy_values = TRUE.")
    }
  }

  # prepare summary object
  summary_obj <- list(
    n_models = length(object$results),
    n_iterations = object$metadata$n_iterations,
    vary_type = object$metadata$vary_type,
    convergence = object$summary_metrics$convergence_diagnostics
  )

  invisible(summary_obj)
}

#' Compute policy value using DR scores for consensus trees (internal helper)
#' @keywords internal
.compute_policy_value_internal <- function(model,
                                           depth = 2L,
                                           R = 499L,
                                           seed = NULL,
                                           baseline = c("control_all", "treat_all")) {
  baseline <- match.arg(baseline)
  if (!is.null(seed)) set.seed(seed)

  tag <- paste0("policy_tree_depth_", depth)
  pol <- model[[tag]]
  if (is.null(pol)) return(NULL)

  dr <- model$dr_scores
  if (is.null(dr)) dr <- model$dr_scores_flipped
  if (is.null(dr)) return(NULL)

  pd <- model$plot_data
  full <- NULL
  if (!is.null(pd)) {
    if (!is.null(pd$X_test_full)) {
      full <- pd$X_test_full
    } else if (!is.null(pd$X_test)) {
      full <- pd$X_test
    }
  }
  if (is.null(full)) {
    return(NULL)
  }

  full <- as.data.frame(full)
  if (!all(pol$columns %in% colnames(full))) {
    missing_cols <- setdiff(pol$columns, colnames(full))
    stop(
      "Evaluation data missing columns required by the consensus tree: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  X_full <- as.data.frame(full[, pol$columns, drop = FALSE])
  test_idx <- suppressWarnings(as.integer(rownames(full)))
  drm <- as.matrix(dr)
  if (!is.null(test_idx) && all(!is.na(test_idx)) &&
      length(test_idx) == nrow(full) && max(test_idx) <= nrow(drm)) {
    drm_test <- drm[test_idx, , drop = FALSE]
  } else {
    take <- seq_len(min(nrow(full), nrow(drm)))
    drm_test <- drm[take, , drop = FALSE]
  }

  keep <- stats::complete.cases(X_full)
  if (!any(keep)) return(NULL)
  Xk <- X_full[keep, , drop = FALSE]
  drk <- drm_test[keep, , drop = FALSE]
  n <- nrow(Xk)
  if (n <= 1L) return(NULL)

  a_hat_full <- tryCatch(stats::predict(pol, X_full), error = function(e) NULL)
  if (is.null(a_hat_full)) return(NULL)
  if (is.matrix(a_hat_full)) a_hat_full <- a_hat_full[, 1]
  a_hat <- as.integer(a_hat_full[keep])

  pick <- function(a, mat) {
    if (length(a) && min(a, na.rm = TRUE) == 0L) a <- a + 1L
    mat[cbind(seq_along(a), a)]
  }

  treat_mean <- mean(drk[, 2])
  control_mean <- mean(drk[, 1])
  base_val <- if (baseline == "control_all") control_mean else treat_mean
  pv_hat <- mean(pick(a_hat, drk)) - base_val

  if (!is.null(seed)) set.seed(seed)
  reps <- replicate(R, {
    idx <- sample.int(n, n, TRUE)
    a_bs <- tryCatch(stats::predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
    if (is.null(a_bs)) return(NA_real_)
    if (is.matrix(a_bs)) a_bs <- a_bs[, 1]
    mean(pick(a_bs, drk[idx, , drop = FALSE])) - base_val
  })
  reps <- reps[is.finite(reps)]
  se <- if (length(reps)) stats::sd(reps) else NA_real_
  p_val <- if (!is.na(se) && is.finite(se) && se > 0) {
    2 * stats::pnorm(-abs(pv_hat / se))
  } else {
    NA_real_
  }

  structure(
    list(
      estimate = pv_hat,
      std.err = se,
      p.value = p_val,
      n_eval = n
    ),
    class = "policy_value_test"
  )
}

#' Print method for stability policy tree results
#'
#' @param x Object of class "margot_stability_policy_tree"
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the object
#' @export
#' @method print margot_stability_policy_tree
print.margot_stability_policy_tree <- function(x, ...) {
  cat("Policy Tree Stability Analysis\n")
  cat("==============================\n\n")

  # basic info
  cat("Models analyzed:", length(x$results), "\n")
  cat("Stability iterations:", x$metadata$n_iterations, "\n")
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

#' Interpret Policy Tree Stability Results
#'
#' @description
#' Provides a narrative interpretation of policy tree stability results
#' suitable for inclusion in scientific manuscripts. The interpretation
#' acknowledges the inherent instability of decision trees while focusing
#' on robust patterns that emerge across iterations.
#'
#' @param object Object of class "margot_stability_policy_tree"
#' @param model_name Model name to interpret
#' @param depth Tree depth to interpret (1, 2, or "both")
#' @param stability_threshold Minimum frequency to consider a split "stable" (default 0.7)
#' @param format Output format: "text" for narrative prose or "technical" for detailed statistics
#' @param decimal_places Number of decimal places for statistics (default 1)
#' @param include_theory Logical: Include theoretical context about tree instability (default TRUE)
#' @param label_mapping Optional named list mapping variable names to labels. If NULL,
#'   uses automatic transformation via transform_var_name()
#' @param include_ci Logical: If TRUE and format = "technical", include simple confidence
#'   intervals for selection frequencies (Wilson interval) and, when available, for
#'   threshold means (normal approx). These quantify iteration-level variability; for
#'   sample uncertainty prefer vary_type = "bootstrap".
#' @param ci_level Numeric: Confidence level for intervals (default 0.95)
#'
#' @return Character string containing the interpretation
#' @export
margot_interpret_stability <- function(
    object,
    model_name,
    depth = 2,
    stability_threshold = 0.7,
    format = c("text", "technical"),
    decimal_places = 1,
    include_theory = TRUE,
    label_mapping = NULL,
    include_ci = FALSE,
    ci_level = 0.95) {
  format <- match.arg(format)

  # validate inputs
  if (!inherits(object, "margot_stability_policy_tree")) {
    stop("object must be of class 'margot_stability_policy_tree'")
  }

  if (!model_name %in% names(object$results)) {
    stop("Model '", model_name, "' not found in results")
  }

  # extract model info
  model_result <- object$results[[model_name]]
  metrics <- model_result$stability_metrics
  n_iterations <- object$metadata$n_iterations
  vary_type <- object$metadata$vary_type

  # get outcome name and label
  outcome_name <- gsub("model_", "", model_name)
  outcome_label <- .apply_label_stability(outcome_name, label_mapping)
  model_label <- .apply_label_stability(model_name, label_mapping)

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
    .apply_label_stability(consensus_var_d1, label_mapping)
  } else {
    consensus_var_d1
  }

  # check if this is a depth-2 only analysis
  var_freq_temp <- metrics$var_inclusion_freq
  is_depth2_only <- all(var_freq_temp$depth_1_freq == 0) && any(var_freq_temp$depth_2_node1_freq > 0 | var_freq_temp$depth_2_node2_freq > 0)

  if (is_depth2_only) {
    # handle depth-2 only analysis as a normal case
    return(interpret_depth2_only_stability(
      object = object,
      model_name = model_name,
      outcome_name = outcome_name,
      outcome_label = outcome_label,
      metrics = metrics,
      n_iterations = n_iterations,
      stability_threshold = stability_threshold,
      decimal_places = decimal_places,
      format = format,
      include_theory = include_theory,
      label_mapping = label_mapping,
      include_ci = include_ci,
      ci_level = ci_level
    ))
  }

  # handle missing values for depth-1 analysis
  if (is.null(consensus_var_d1) || is.na(consensus_var_d1) || consensus_freq_d1 == 0) {
    # get the most frequent variable from the frequency table
    var_freq_temp <- var_freq_temp[order(var_freq_temp$depth_1_freq, decreasing = TRUE), ]
    if (nrow(var_freq_temp) > 0 && var_freq_temp$depth_1_freq[1] > 0) {
      consensus_var_d1 <- var_freq_temp$variable[1]
      consensus_freq_d1 <- var_freq_temp$depth_1_freq[1]
      consensus_var_d1_label <- .apply_label_stability(consensus_var_d1, label_mapping)
    } else {
      # no depth-1 results available
      cli::cli_alert_warning("No depth-1 policy trees were successfully computed")
      return(invisible("No valid policy trees could be computed in stability analysis"))
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

  # describe analysis type
  analysis_type <- if (vary_type == "split_only") {
    "train/test split variation"
  } else if (vary_type == "bootstrap") {
    "bootstrap resampling"
  } else {
    "combined bootstrap and split variation"
  }

  # main stability finding
  if (consensus_d1 >= stability_threshold) {
    stability_desc <- "demonstrated notable consistency"
    confidence_statement <- paste0(
      "Despite the inherent instability of tree-based methods, ",
      consensus_var_d1_label, " emerged as the primary decision variable in ",
      sprintf("%.*f%%", decimal_places, consensus_freq_d1 * 100),
      " of iterations, suggesting this variable (or variables highly ",
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
    "Stability analysis of the policy tree for ", outcome_label, " ",
    stability_desc, " across ", n_iterations, " iterations using ", analysis_type
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
      sapply(competitors$variable, function(v) .apply_label_stability(v, label_mapping)), " (",
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
        " Secondary splits (depth-2) showed substantial variability across iterations,",
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
    # optional CI helper for proportions (Wilson interval)
    .prop_ci <- function(p, n, level = 0.95) {
      if (is.na(p) || is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
      z <- stats::qnorm(1 - (1 - level) / 2)
      denom <- 1 + z^2 / n
      center <- (p + z^2 / (2 * n)) / denom
      rad <- (z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2)))) / denom
      lo <- max(0, center - rad)
      hi <- min(1, center + rad)
      c(lo, hi)
    }

    # CI text for depth-1 consensus selection and threshold mean (if available)
    ci_text <- ""
    if (isTRUE(include_ci)) {
      # CI for selection frequency
      ci_p <- .prop_ci(consensus_freq_d1, n_iterations, ci_level)
      if (all(!is.na(ci_p))) {
        ci_text <- paste0(
          " Consensus freq CI: ",
          sprintf("%.*f%%", decimal_places, ci_p[1] * 100),
          "–",
          sprintf("%.*f%%", decimal_places, ci_p[2] * 100),
          "."
        )
      }

      # CI for threshold mean if SD present and at least 2 selections
      if (!is.na(consensus_thresh_d1) && !is.na(thresh_sd_d1) && thresh_sd_d1 > 0) {
        n_sel <- max(1L, round(consensus_freq_d1 * n_iterations))
        if (n_sel >= 2L) {
          z <- stats::qnorm(1 - (1 - ci_level) / 2)
          se_th <- thresh_sd_d1 / sqrt(n_sel)
          th_lo <- consensus_thresh_d1 - z * se_th
          th_hi <- consensus_thresh_d1 + z * se_th
          ci_text <- paste0(
            ci_text,
            " Threshold CI: [",
            sprintf("%.*f", decimal_places + 1, th_lo), ", ",
            sprintf("%.*f", decimal_places + 1, th_hi), "]"
          )
        }
      }
    }

    tech_details <- paste0(
      "\n\nTechnical details: Stability analysis used ",
      switch(vary_type,
        "both" = "both bootstrap resampling and train/test split variation",
        "bootstrap" = "bootstrap resampling with replacement",
        "split_only" = "train/test split variation without resampling"
      ),
      ". Variable importance (inclusion frequency) was: ",
      paste(
        sprintf(
          "%s (%.1f%%)", var_freq$variable[1:min(5, nrow(var_freq))],
          var_freq$depth_1_freq[1:min(5, nrow(var_freq))] * 100
        ),
        collapse = ", "
      ),
      ".",
      if (nzchar(ci_text)) paste0(" ", ci_text) else ""
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

#' Interpret depth-2 only stability results
#' @keywords internal
interpret_depth2_only_stability <- function(object, model_name, outcome_name, outcome_label, metrics,
                                            n_iterations, stability_threshold,
                                            decimal_places, format, include_theory = TRUE,
                                            label_mapping = NULL,
                                            include_ci = FALSE,
                                            ci_level = 0.95) {
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
  top_var_label <- .apply_label_stability(top_var, label_mapping)

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
    "Stability analysis of depth-2 policy trees for ", outcome_label, " ",
    stability_desc, " across ", n_iterations, " iterations"
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
          function(v) .apply_label_stability(v, label_mapping)
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
          .apply_label_stability(d2_splits$node1$variable, label_mapping), " (consensus ",
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
      "suggests caution in implementing complex treatment rules for ", outcome_label,
      ". While uniform treatment assignment avoids unstable personalization."
    ))
  }

  # technical details if requested
  if (format == "technical") {
    # optional CI for the top variable's average frequency across nodes
    ci_text <- ""
    if (isTRUE(include_ci) && nrow(top_vars) > 0) {
      p1 <- top_vars$depth_2_node1_freq[1]
      p2 <- top_vars$depth_2_node2_freq[1]
      z <- stats::qnorm(1 - (1 - ci_level) / 2)
      se_avg <- sqrt((p1 * (1 - p1) / n_iterations + p2 * (1 - p2) / n_iterations) / 4)
      lo <- max(0, top_var_avg_freq - z * se_avg)
      hi <- min(1, top_var_avg_freq + z * se_avg)
      ci_text <- paste0(
        " Top variable avg freq CI: ",
        sprintf("%.*f%%", decimal_places, lo * 100),
        "–",
        sprintf("%.*f%%", decimal_places, hi * 100),
        "."
      )
    }

    tech_details <- paste0(
      "\n\nTechnical details: Stability analysis used ", n_iterations, " iterations with ",
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
      ".",
      ci_text
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

#' Deprecated: Bootstrap Analysis for Policy Trees
#'
#' @description
#' This function is deprecated. Please use \code{\link{margot_policy_tree_stability}} instead.
#'
#' @param ... Arguments passed to margot_policy_tree_stability
#'
#' @return Result from margot_policy_tree_stability
#' @export
margot_policy_tree_bootstrap <- function(...) {
  .Deprecated("margot_policy_tree_stability",
    msg = paste0(
      "margot_policy_tree_bootstrap() is deprecated.\n",
      "Please use margot_policy_tree_stability() instead.\n",
      "Note: change 'n_bootstrap' parameter to 'n_iterations'"
    )
  )

  # extract arguments
  args <- list(...)

  # rename n_bootstrap to n_iterations if present
  if ("n_bootstrap" %in% names(args)) {
    args$n_iterations <- args$n_bootstrap
    args$n_bootstrap <- NULL
  }

  # rename sample_only to bootstrap if present
  if ("vary_type" %in% names(args) && args$vary_type == "sample_only") {
    args$vary_type <- "bootstrap"
  }

  # call the new function
  do.call(margot_policy_tree_stability, args)
}

#' Report Policy Value for Consensus Trees
#'
#' Computes and reports how much better the consensus policy trees perform
#' than (a) treat-all (ATE baseline) and (b) treat-none (universal control),
#' with 95% confidence intervals from bootstrap standard errors.
#'
#' @param object Object of class "margot_stability_policy_tree" produced by
#'   [margot_policy_tree_stability()]. Must contain consensus policy trees in
#'   `policy_tree_depth_1` and/or `policy_tree_depth_2`, plus `plot_data` and
#'   `dr_scores`/`dr_scores_flipped`.
#' @param model_names Optional character vector of model names to include
#'   (with or without `model_` prefix). Default: all.
#' @param depths Integer vector of depths to evaluate (default `c(1, 2)`).
#' @param R Integer ≥ 199; number of bootstrap replicates (default 499).
#' @param seed Integer or NULL; RNG seed for reproducibility (default 42).
#' @param ci_level Confidence level for intervals (default 0.95).
#' @param verbose Logical; print progress (default TRUE).
#'
#' @return A data.frame with one row per model × depth × contrast containing
#'   estimate, standard error, and confidence interval.
#'
#' @export
margot_report_consensus_policy_value <- function(object,
                                                model_names = NULL,
                                                depths = c(1L, 2L),
                                                R = 499L,
                                                seed = 42L,
                                                ci_level = 0.95,
                                                include_treated_only = FALSE,
                                                label_mapping = NULL,
                                                verbose = TRUE) {
  if (!inherits(object, "margot_stability_policy_tree")) {
    stop("object must be of class 'margot_stability_policy_tree'")
  }

  if (is.null(model_names)) {
    model_names <- names(object$results)
  } else {
    model_names <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
  }

  missing <- setdiff(model_names, names(object$results))
  if (length(missing)) stop("Models not found in results: ", paste(missing, collapse = ", "))

  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  out_rows <- list()

  # resolve internal helper robustly (works if user sourced a single file)
  compute_pv <- NULL
  if (exists("margot_compute_policy_value", mode = "function")) {
    compute_pv <- margot_compute_policy_value
  } else {
    compute_pv <- tryCatch(getFromNamespace("margot_compute_policy_value", "margot"), error = function(e) NULL)
  }
  # local fallback implementation if helper not found
  if (is.null(compute_pv)) {
    compute_pv <- .compute_policy_value_internal
  }

  for (mn in model_names) {
    if (verbose) {
      out_name <- gsub("^model_", "", mn)
      out_disp <- .apply_label_stability(out_name, label_mapping)
      cli::cli_alert_info("Evaluating consensus trees for {.var {out_disp}}")
    }
    m <- object$results[[mn]]
    for (d in depths) {
      tag <- paste0("policy_tree_depth_", d)
      if (is.null(m[[tag]])) next

      # evaluate both baselines
      for (base in c("treat_all", "control_all")) {
        pv <- compute_pv(m, depth = d, R = R, seed = seed, baseline = base)
        est <- pv$estimate
        se  <- pv$std.err
        lo <- est - z * se
        hi <- est + z * se
        # optionally compute treated-only metrics
        avg_uplift_treated <- NA_real_
        coverage_treated <- NA_real_
        if (isTRUE(include_treated_only)) {
          pol <- m[[tag]]
          dr <- m$dr_scores
          pd <- m$plot_data
          full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
          if (!is.null(full) && !is.null(pol) && !is.null(dr)) {
            X <- full[, pol$columns, drop = FALSE]
            keep <- stats::complete.cases(X)
            Xk <- X[keep, , drop = FALSE]
            drk <- dr[keep, , drop = FALSE]
            if (nrow(Xk) > 0) {
              a_hat <- predict(pol, Xk)
              treat_mask <- a_hat == 2L
              if (any(treat_mask)) {
                avg_uplift_treated <- mean(drk[treat_mask, 2] - drk[treat_mask, 1])
                coverage_treated <- mean(treat_mask)
                # bootstrap CI for avg uplift among treated
                reps_u <- replicate(R, {
                  idx <- sample.int(nrow(Xk), nrow(Xk), TRUE)
                  a_bs <- predict(pol, Xk[idx, , drop = FALSE])
                  tm <- a_bs == 2L
                  if (any(tm)) mean(drk[idx, , drop = FALSE][tm, 2] - drk[idx, , drop = FALSE][tm, 1]) else NA_real_
                })
                reps_u <- reps_u[!is.na(reps_u)]
                if (length(reps_u) > 1) {
                  se_u <- stats::sd(reps_u)
                  lo_u <- avg_uplift_treated - z * se_u
                  hi_u <- avg_uplift_treated + z * se_u
                } else {
                  se_u <- NA_real_
                  lo_u <- NA_real_
                  hi_u <- NA_real_
                }
              }
            }
          }
        }
        out_rows[[length(out_rows) + 1L]] <- data.frame(
          model = mn,
          outcome = gsub("^model_", "", mn),
          outcome_label = .apply_label_stability(gsub("^model_", "", mn), label_mapping),
          depth = d,
          contrast = if (base == "treat_all") "policy - treat_all" else "policy - control_all",
          estimate = est,
          std_err = se,
          ci_lo = lo,
          ci_hi = hi,
          avg_uplift_treated = avg_uplift_treated,
          avg_uplift_treated_se = if (exists("se_u")) se_u else NA_real_,
          avg_uplift_treated_ci_lo = if (exists("lo_u")) lo_u else NA_real_,
          avg_uplift_treated_ci_hi = if (exists("hi_u")) hi_u else NA_real_,
          coverage_treated = coverage_treated,
          n_eval = pv$n_eval,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(out_rows)) {
    if (verbose) cli::cli_alert_warning("No consensus trees available to evaluate")
    return(invisible(NULL))
  }

  res <- do.call(rbind, out_rows)
  rownames(res) <- NULL
  res
}

#' Manuscript-ready table for consensus policy value
#'
#' Produces a compact table (data.frame) summarizing the consensus policy value
#' for each model and selected depth, relative to both treat-all and control-all
#' baselines, with 95% confidence intervals.
#'
#' @param object Stability result from [margot_policy_tree_stability()].
#' @param model_names Optional outcomes to include (with or without `model_`).
#' @param depth Integer; default 2.
#' @param R,seed,ci_level Passed to [margot_report_consensus_policy_value()].
#' @export
margot_table_consensus_policy_value <- function(object,
                                               model_names = NULL,
                                               depth = 2L,
                                               R = 499L,
                                               seed = 42L,
                                               ci_level = 0.95,
                                               report_df = NULL,
                                               label_mapping = NULL) {
  # If caller provided a precomputed report, prefer it for consistency and efficiency
  tbl <- report_df
  if (is.null(tbl)) {
    tbl <- margot_report_consensus_policy_value(
      object,
      model_names = model_names,
      depths = depth,
      R = R,
      seed = seed,
      ci_level = ci_level,
      verbose = FALSE
    )
  } else {
    # Optionally filter the provided report to the selected models/depth
    if (!is.null(model_names)) {
      keep_models <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
      tbl <- tbl[tbl$model %in% keep_models, , drop = FALSE]
    }
    if (!is.null(depth)) {
      tbl <- tbl[tbl$depth %in% depth, , drop = FALSE]
    }
  }

  if (is.null(tbl) || !nrow(tbl)) return(tbl)

  # apply label mapping for display if provided or if outcome_label present
  if (!is.null(label_mapping)) {
    tbl$outcome <- vapply(tbl$outcome, function(x) .apply_label_stability(x, label_mapping), character(1))
  } else if ("outcome_label" %in% names(tbl)) {
    tbl$outcome <- tbl$outcome_label
  }

  # order columns and add a concise CI string
  tbl$ci <- paste0("[", round(tbl$ci_lo, 3), ", ", round(tbl$ci_hi, 3), "]")
  tbl$estimate <- round(tbl$estimate, 3)
  tbl$std_err <- round(tbl$std_err, 3)
  # include treated-only metrics if present
  has_treated <- all(c("avg_uplift_treated", "coverage_treated") %in% names(tbl))
  if (has_treated) {
    tbl$avg_uplift_treated <- round(tbl$avg_uplift_treated, 3)
    tbl$coverage_treated <- round(100 * tbl$coverage_treated, 1)
    if (all(c("avg_uplift_treated_ci_lo", "avg_uplift_treated_ci_hi") %in% names(tbl))) {
      tbl$avg_uplift_treated_ci <- paste0(
        "[", round(tbl$avg_uplift_treated_ci_lo, 3), ", ", round(tbl$avg_uplift_treated_ci_hi, 3), "]"
      )
      tbl <- tbl[, c("outcome", "depth", "contrast", "estimate", "std_err", "ci", "n_eval",
                     "avg_uplift_treated", "avg_uplift_treated_ci", "coverage_treated")]
      names(tbl)[names(tbl) == "coverage_treated"] <- "coverage_treated_pct"
    } else {
      tbl <- tbl[, c("outcome", "depth", "contrast", "estimate", "std_err", "ci", "n_eval",
                     "avg_uplift_treated", "coverage_treated")]
      names(tbl)[names(tbl) == "coverage_treated"] <- "coverage_treated_pct"
    }
  } else {
    tbl <- tbl[, c("outcome", "depth", "contrast", "estimate", "std_err", "ci", "n_eval")]
  }
  rownames(tbl) <- NULL
  tbl
}

#' Batch Interpret Policy Tree Stability Results
#'
#' @description
#' Processes multiple models from a stability analysis and provides narrative
#' interpretations for each. Returns a named list of interpretations suitable
#' for batch reporting.
#'
#' @param object Object of class "margot_stability_policy_tree"
#' @param model_names Optional character vector of model names to interpret.
#'   If NULL (default), interprets all models in the results.
#' @param depth Tree depth to interpret (1, 2, or "both")
#' @param stability_threshold Minimum frequency to consider a split "stable" (default 0.7)
#' @param format Output format: "text" for narrative prose or "technical" for detailed statistics
#' @param decimal_places Number of decimal places for statistics (default 1)
#' @param include_theory Logical: Include theoretical context about tree instability (default TRUE).
#'   Note: theory is only included for the first model to avoid repetition.
#' @param label_mapping Optional named list mapping variable names to labels
#' @param verbose Logical: Print progress messages (default TRUE)
#' @param combine Logical: If TRUE, combines all interpretations into a single text (default FALSE)
#' @param save_to_file Optional file path to save combined interpretations as text file
#' @param include_ci Logical: If TRUE and format = "technical", include simple CIs in output
#' @param ci_level Numeric: Confidence level for intervals (default 0.95)
#'
#' @return Named list where each element contains the interpretation for a model.
#'   If combine = TRUE, returns a single character string with all interpretations.
#'
#' @examples
#' \dontrun{
#' # Interpret all models
#' all_interpretations <- margot_interpret_stability_batch(
#'   stability_results,
#'   format = "technical",
#'   stability_threshold = 0.5,
#'   label_mapping = label_mapping_all
#' )
#'
#' # Interpret specific models only
#' selected_interpretations <- margot_interpret_stability_batch(
#'   stability_results,
#'   model_names = c("model_depression", "model_anxiety"),
#'   format = "text"
#' )
#'
#' # Get combined output for reporting
#' combined_text <- margot_interpret_stability_batch(
#'   stability_results,
#'   combine = TRUE,
#'   save_to_file = "stability_interpretations.txt"
#' )
#' }
#'
#' @export
margot_interpret_stability_batch <- function(
    object,
    model_names = NULL,
    depth = 2,
    stability_threshold = 0.7,
    format = c("text", "technical"),
    decimal_places = 1,
    include_theory = TRUE,
    label_mapping = NULL,
    verbose = TRUE,
    combine = FALSE,
    save_to_file = NULL,
    include_ci = FALSE,
    ci_level = 0.95) {
  format <- match.arg(format)

  # validate inputs
  if (!inherits(object, "margot_stability_policy_tree")) {
    stop("object must be of class 'margot_stability_policy_tree'")
  }

  # determine which models to process
  if (is.null(model_names)) {
    model_names <- names(object$results)
  } else {
    # add model_ prefix if needed
    model_names <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    # check all models exist
    missing <- setdiff(model_names, names(object$results))
    if (length(missing) > 0) {
      stop("Models not found in results: ", paste(missing, collapse = ", "))
    }
  }

  if (verbose) {
    cli::cli_h1("Batch Stability Interpretation")
    cli::cli_alert_info("Processing {length(model_names)} model{?s}")
  }

  # initialize results list
  interpretations <- list()

  # process each model
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]

    if (verbose) {
      cli::cli_h2("Model {i}/{length(model_names)}: {gsub('model_', '', model_name)}")
    }

    # suppress individual console output
    interpretation_text <- capture.output(
      {
        margot_interpret_stability(
          object = object,
          model_name = model_name,
          depth = depth,
          stability_threshold = stability_threshold,
          format = format,
          decimal_places = decimal_places,
          include_theory = include_theory && i == 1, # only include theory for first model
          label_mapping = label_mapping,
          include_ci = include_ci,
          ci_level = ci_level
        )
      },
      type = "output"
    )

    # the function returns invisibly, so we need to capture it differently
    actual_text <- margot_interpret_stability(
      object = object,
      model_name = model_name,
      depth = depth,
      stability_threshold = stability_threshold,
      format = format,
      decimal_places = decimal_places,
      include_theory = include_theory && i == 1,
      label_mapping = label_mapping,
      include_ci = include_ci,
      ci_level = ci_level
    )

    interpretations[[model_name]] <- actual_text
  }

  if (verbose) {
    cli::cli_alert_success("Completed interpreting {length(interpretations)} model{?s}")
  }

  # combine if requested
  if (combine) {
    divider <- paste0("\n\n", paste(rep("=", 80), collapse = ""), "\n\n")

    combined_text <- paste(
      sapply(names(interpretations), function(model_name) {
        model_label <- .apply_label_stability(gsub("model_", "", model_name), label_mapping)
        header <- paste0("Model: ", model_label, "\n", paste(rep("-", nchar(model_label) + 7), collapse = ""), "\n\n")
        paste0(header, interpretations[[model_name]])
      }),
      collapse = divider
    )

    # save to file if requested
    if (!is.null(save_to_file)) {
      tryCatch(
        {
          writeLines(combined_text, save_to_file)
          if (verbose) {
            cli::cli_alert_success("Interpretations saved to {save_to_file}")
          }
        },
        error = function(e) {
          cli::cli_alert_danger("Failed to save to file: {e$message}")
        }
      )
    }

    return(combined_text)
  }

  # return named list
  return(interpretations)
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

#' Compute consensus information from accumulator
#' @keywords internal
compute_consensus_info <- function(accumulator, n_iterations, consensus_threshold) {
  # depth-1 consensus
  d1_freqs <- accumulator$depth_1$var_counts / n_iterations
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
  d2_node1_freqs <- accumulator$depth_2$node1_var_counts / n_iterations
  d2_node1_idx <- which.max(d2_node1_freqs)

  d2_node2_freqs <- accumulator$depth_2$node2_var_counts / n_iterations
  d2_node2_idx <- which.max(d2_node2_freqs)

  d2_consensus <- list(
    node1 = {
      n1c <- accumulator$depth_2$node1_var_counts[d2_node1_idx]
      mean1 <- if (n1c > 0) accumulator$depth_2$node1_threshold_sums[d2_node1_idx] / n1c else NA
      sd1 <- if (n1c > 1) {
        m2 <- accumulator$depth_2$node1_threshold_sumsq[d2_node1_idx] / n1c
        sqrt(max(0, m2 - mean1^2))
      } else NA
      list(
        variable = accumulator$var_names[d2_node1_idx],
        frequency = d2_node1_freqs[d2_node1_idx],
        threshold_mean = mean1,
        threshold_sd = sd1
      )
    },
    node2 = {
      n2c <- accumulator$depth_2$node2_var_counts[d2_node2_idx]
      mean2 <- if (n2c > 0) accumulator$depth_2$node2_threshold_sums[d2_node2_idx] / n2c else NA
      sd2 <- if (n2c > 1) {
        m2 <- accumulator$depth_2$node2_threshold_sumsq[d2_node2_idx] / n2c
        sqrt(max(0, m2 - mean2^2))
      } else NA
      list(
        variable = accumulator$var_names[d2_node2_idx],
        frequency = d2_node2_freqs[d2_node2_idx],
        threshold_mean = mean2,
        threshold_sd = sd2
      )
    }
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

#' Compute compact policy-value summary if available
#' @keywords internal
compute_policy_value_summary <- function(results) {
  z <- stats::qnorm(1 - 0.05 / 2)
  rows <- lapply(names(results), function(nm) {
    m <- results[[nm]]
    out <- list()
    for (d in c(1L, 2L)) {
      tag <- paste0("policy_value_depth_", d)
      pv <- m[[tag]]
      if (!is.null(pv) && is.list(pv)) {
        est <- pv$estimate
        se  <- pv$std.err
        lo <- est - z * se
        hi <- est + z * se
        out[[length(out) + 1L]] <- data.frame(
          model = nm,
          depth = d,
          baseline = pv$baseline %||% NA_character_,
          contrast = pv$contrast %||% NA_character_,
          estimate = est,
          std_err = se,
          ci_lo = lo,
          ci_hi = hi,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(out)) do.call(rbind, out) else NULL
  })
  do.call(rbind, rows)
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

#' Deprecated: Interpret Bootstrap Results
#'
#' @description
#' This function is deprecated. Please use \code{\link{margot_interpret_stability}} instead.
#'
#' @param ... Arguments passed to margot_interpret_stability
#'
#' @return Result from margot_interpret_stability
#' @export
margot_interpret_bootstrap <- function(...) {
  .Deprecated("margot_interpret_stability",
    msg = "margot_interpret_bootstrap() is deprecated. Please use margot_interpret_stability() instead."
  )

  margot_interpret_stability(...)
}
