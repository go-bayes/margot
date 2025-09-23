# -----------------------------------------------------------------------------
# helper: %||%
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------
# helper: compute_qini_improved
# -----------------------------------------------------------------------------

#' Compute QINI Curves with Improved DR Scores (Internal)
#' @keywords internal
compute_qini_improved <- function(Y, W, X, tau_hat, weights = NULL,
                                  seed = 12345, n_bootstrap = 200,
                                  verbose = TRUE) {
  tryCatch(
    {
      if (verbose) cli::cli_alert_info("Computing QINI curves with evaluation forest approach")

      # ensure proper formats
      Y <- as.vector(Y)
      W <- as.vector(W)
      tau_hat <- as.vector(tau_hat)

      # fit evaluation forest on test data for consistent DR scores
      if (verbose) cli::cli_alert_info("Fitting evaluation forest for DR scores")

      set.seed(seed)
      eval_forest <- grf::causal_forest(
        X = X,
        Y = Y,
        W = W,
        sample.weights = weights,
        num.trees = 2000
      )

      # get DR scores from evaluation forest
      DR_scores <- grf::get_scores(eval_forest, drop = TRUE)

      # compute ATE from evaluation forest for reference
      ate_eval <- grf::average_treatment_effect(eval_forest)

      if (verbose) {
        cli::cli_alert_info("Evaluation forest ATE: {round(ate_eval['estimate'], 4)}")
        # add diagnostic info
        if (length(tau_hat) > 0) {
          cli::cli_alert_info("tau_hat range: [{round(min(tau_hat), 4)}, {round(max(tau_hat), 4)}]")
          cli::cli_alert_info("tau_hat SD: {round(sd(tau_hat), 4)}")
        }
      }

      # compute CATE-based QINI
      if (verbose) cli::cli_alert_info("Computing CATE-based QINI curve")

      qini_cate <- tryCatch(
        {
          maq::maq(
            reward = as.matrix(tau_hat),
            cost = matrix(1, length(tau_hat), 1),
            DR.scores = DR_scores,
            R = n_bootstrap,
            seed = seed
          )
        },
        error = function(e) {
          if (verbose) cli::cli_alert_warning("Failed to compute CATE QINI: {e$message}")
          NULL
        }
      )

      # compute no-priority baseline
      if (verbose) cli::cli_alert_info("Computing no-priority baseline")

      qini_baseline <- tryCatch(
        {
          maq::maq(
            reward = as.matrix(tau_hat),
            cost = matrix(1, length(tau_hat), 1),
            DR.scores = DR_scores,
            target.with.covariates = FALSE,
            R = n_bootstrap,
            seed = seed
          )
        },
        error = function(e) {
          if (verbose) cli::cli_alert_warning("Failed to compute baseline: {e$message}")
          NULL
        }
      )

      if (is.null(qini_baseline)) {
        if (verbose) cli::cli_alert_warning("Creating fallback baseline")
        # create minimal baseline
        qini_baseline <- list(
          "_path" = list(
            spend = c(0, 1),
            gain = c(0, mean(tau_hat))
          )
        )
      }

      # check if CATE computation failed
      if (is.null(qini_cate)) {
        if (verbose) cli::cli_alert_warning("CATE QINI computation failed")
        return(NULL)
      }

      # extract paths
      cate_path <- qini_cate[["_path"]]
      baseline_path <- qini_baseline[["_path"]]

      # create qini_data
      if (!is.null(cate_path) && length(cate_path$spend) > 0) {
        cate_df <- data.frame(
          proportion = cate_path$spend,
          gain = cate_path$gain,
          curve = "cate"
        )
      } else {
        if (verbose) cli::cli_alert_warning("CATE path is empty")
        return(NULL)
      }

      if (!is.null(baseline_path) && length(baseline_path$spend) > 0) {
        baseline_df <- data.frame(
          proportion = baseline_path$spend,
          gain = baseline_path$gain,
          curve = "ate"
        )
      } else {
        # create minimal baseline
        baseline_df <- data.frame(
          proportion = c(0, 1),
          gain = c(0, mean(tau_hat)),
          curve = "ate"
        )
      }

      qini_data <- rbind(cate_df, baseline_df)

      # return results
      list(
        qini_data = qini_data,
        qini_objects = list(cate = qini_cate, ate = qini_baseline),
        ate_evaluation_forest = round(ate_eval, 3) # This is already a named vector, not a list
      )
    },
    error = function(e) {
      if (verbose) cli::cli_alert_warning("Error in compute_qini_improved: {e$message}")
      NULL
    }
  )
}

# -----------------------------------------------------------------------------
# helper: compute_qini_curves_binary
# -----------------------------------------------------------------------------

#' Compute Qini Curves for Binary Treatments
#' @keywords internal
compute_qini_curves_binary <- function(tau_hat, Y, W, weights = NULL, seed = 12345, verbose = TRUE, treatment_cost = 1) {
  tryCatch(
    {
      if (verbose) cli::cli_alert_info("computing Qini curves …")
      tau_hat <- as.vector(tau_hat)
      treatment <- as.factor(W)
      IPW_scores <- maq::get_ipw_scores(Y, treatment)

      # use modern maq API with named parameters
      cate_qini <- maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(treatment_cost, length(tau_hat), 1),
        DR.scores = IPW_scores,
        R = 200,
        sample.weights = weights,
        seed = seed
      )

      # try maq with target.with.covariates = FALSE first (maq_no_covariates approach)
      ate_qini <- tryCatch(
        {
          maq::maq(
            reward = as.matrix(tau_hat),
            cost = matrix(treatment_cost, length(tau_hat), 1),
            DR.scores = IPW_scores,
            target.with.covariates = FALSE,
            R = 200,
            sample.weights = weights,
            seed = seed
          )
        },
        error = function(e) {
          if (verbose) cli::cli_alert_warning("Failed to generate baseline with maq (no covariates), falling back to constant rewards: {e$message}")
          # fallback to constant rewards
          maq::maq(
            reward = matrix(rep(mean(tau_hat), length(tau_hat)), ncol = 1),
            cost = matrix(treatment_cost, length(tau_hat), 1),
            DR.scores = IPW_scores,
            R = 200,
            sample.weights = weights,
            seed = seed
          )
        }
      )

      qini_objs <- list(cate = cate_qini, ate = ate_qini)
      max_idx <- max(sapply(qini_objs, function(q) length(q[["_path"]]$gain)))
      if (!max_idx) {
        return(NULL)
      }
      if (verbose) cli::cli_alert_info("Extracting qini data for curves: {paste(names(qini_objs), collapse = ', ')}")
      qini_dat <- purrr::map2_dfr(
        qini_objs, names(qini_objs),
        ~ extract_qini_data_binary(.x, .y, max_idx, verbose)
      )
      if (!nrow(qini_dat)) {
        return(NULL)
      }
      list(qini_data = qini_dat, qini_objects = qini_objs)
    },
    error = function(e) {
      if (verbose) cli::cli_alert_warning("qini error: {e$message}")
      NULL
    }
  )
}

#' Extract Qini Data for Binary Treatment Plotting
#' @keywords internal
extract_qini_data_binary <- function(qini_obj, name, max_index, verbose = TRUE) {
  # use actual gain values for all curves
  gain <- qini_obj[["_path"]]$gain

  if (is.null(gain) || length(gain) == 0) {
    if (verbose) cli::cli_alert_warning("no gain data found for curve {name}")
    return(NULL)
  }

  # handle length differences
  if (length(gain) < max_index) {
    # pad with last value
    gain <- c(gain, rep(tail(gain, 1), max_index - length(gain)))
  } else if (length(gain) > max_index) {
    # interpolate to max_index points
    indices <- round(seq(1, length(gain), length.out = max_index))
    gain <- gain[indices]
  }

  # ensure we have exactly max_index points
  gain <- gain[1:max_index]
  proportion <- seq(0, 1, length.out = max_index)

  data.frame(proportion = proportion, gain = gain, curve = name)
}

#' Inspect qini diagnostics for one or several models
#'
#' @param model_results list returned by `margot_causal_forest()` **with**
#'        `save_models = TRUE, save_data = TRUE`.
#' @param model_names  optional character vector of outcome names
#'        (with or without the `model_` prefix).  default = *all*.
#' @param test_prop    fraction of trimmed rows to allocate to the
#'        validation/Qini test set.  default 0.5.
#' @param propensity_bounds numeric length-2 vector giving the lower
#'        and upper trimming thresholds for `forest$W.hat`.  default
#'        c(0.05, 0.95).
#' @param seed integer for reproducibility.
#'
#' @return a tibble of diagnostics (class `"margot_qini_diag"`).
#' @export
margot_inspect_qini <- function(model_results,
                                model_names = NULL,
                                test_prop = 0.5,
                                propensity_bounds = c(0.05, 0.95),
                                seed = 2025) {
  stopifnot(
    is.list(model_results),
    "full_models" %in% names(model_results),
    "data" %in% names(model_results)
  )

  # make sure tidyverse is on board
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("policytree", quietly = TRUE)

  if (is.null(model_names)) {
    model_names <- names(model_results$full_models)
  }
  model_names <- ifelse(grepl("^model_", model_names),
    model_names,
    paste0("model_", model_names)
  )

  set.seed(seed)

  purrr::map_dfr(model_names, function(mn) {
    forest <- model_results$full_models[[mn]]
    if (is.null(forest)) {
      return(NULL)
    }

    outcome_name <- sub("^model_", "", mn)

    Y <- model_results$data[[outcome_name]]
    W <- forest$W
    eh <- forest$W.hat
    th <- predict(forest)$predictions # tau_hat
    dr <- policytree::double_robust_scores(forest) # n x 2 matrix
    dr <- dr[, 2] - dr[, 1] # treated – control

    keep <- which(!is.na(Y) &
      eh > propensity_bounds[1] &
      eh < propensity_bounds[2])

    n_trimmed <- length(Y) - length(keep)
    if (length(keep) < 10) {
      return(tibble::tibble(
        model = mn,
        outcome = outcome_name,
        note = "too little data after trimming"
      ))
    }

    idx_test <- sample(keep, size = floor(test_prop * length(keep)))
    ord <- order(-th[idx_test]) # best-to-worst ranking
    gain <- cumsum(dr[idx_test][ord])

    tibble::tibble(
      model = mn,
      outcome = outcome_name,
      sd_tau = sd(th[idx_test]),
      min_gain = min(gain),
      final_gain = gain[length(gain)],
      n_test = length(idx_test),
      n_trimmed = n_trimmed
    )
  }) %>% dplyr::arrange(dplyr::desc(abs(final_gain)))
}

#' Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Qini Cross-Validation
#'
#' This function runs multiple GRF causal forest models with enhanced features. In addition to estimating
#' causal effects, it can compute the Rank-Weighted Average Treatment Effect (RATE) for each model. It also
#' gives you the option to train a separate "Qini forest" on a subset of data and compute Qini curves on
#' held-out data, thereby avoiding in-sample optimism in the Qini plots.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modelled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A vector of binary treatment assignments.
#' @param weights A vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#'   Note: Direct computation of RATE, QINI, and policy trees within this function may be
#'   deprecated in future versions. Use margot_rate(), margot_qini(), and margot_policy_tree() instead.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 15.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is TRUE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of data to use for
#'   training set, or NULL. Used for policy trees and QINI evaluation when compute_marginal_only=FALSE.
#'   Must be NULL when compute_marginal_only=TRUE. Default is 0.5.
#' @param train_prop Deprecated. Use train_proportion instead.
#' @param qini_train_prop Deprecated. Use train_proportion instead.
#' @param compute_conditional_means Logical indicating whether to compute conditional means using
#'   \code{policytree::conditional_means()}. These represent expected outcomes under each treatment arm. Default is TRUE.
#' @param compute_marginal_only Logical indicating whether to compute only marginal (average)
#'   treatment effects, skipping heterogeneity metrics (QINI curves, RATE, policy trees).
#'   When TRUE, train_proportion must be NULL. When FALSE (default), heterogeneity
#'   metrics are computed using train/test splits.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#' @param qini_treatment_cost Scalar treatment cost per unit for QINI calculations. Default 1.
#'   Lower values (e.g., 0.2) represent cheap treatments creating steeper QINI curves;
#'   higher values (e.g., 5) represent expensive treatments creating shallower curves.
#' @param seed Integer. Random seed for reproducibility of train/test splits for policy trees
#'   and QINI evaluation. Default is 12345.
#' @param use_train_test_split Logical. If TRUE (default), uses a consistent train/test split where:
#'   - The main causal forest is trained on all data (following GRF best practices for honesty)
#'   - All reported results (ATE, E-values, combined_table) are computed on the TEST SET
#'   - Policy trees and QINI also use the same train/test split
#'   - All-data results are stored in split_info for reference
#'   If FALSE, the main forest uses all data for estimation and policy trees/QINI get their own splits.
#' @param flip_outcomes Character vector or list specifying outcomes to flip (reverse score).
#'   - Character vector: applies default flip_method and flip_scale_bounds to all listed outcomes
#'   - List: allows per-outcome specification, e.g., list(anxiety = list(method = "ordinal", scale_bounds = c(1, 7)))
#'   Flipped outcomes are appended with "_r" suffix in results.
#' @param flip_method Character string specifying default inversion method when flip_outcomes is provided:
#'   - "zscore": Simple negation (default). Assumes data is already standardized (e.g., outcomes ending in "_z").
#'   - "ordinal": Invert on ordinal scale using bounds. Use for Likert scales or bounded measures.
#' @param flip_scale_bounds Numeric vector of length 2 [min, max] or named list of bounds per outcome.
#'   Required when flip_method = "ordinal" unless bounds should be inferred from data.
#'   Example: c(1, 7) for a 7-point Likert scale, or list(anxiety = c(1, 7), depression = c(0, 21)).
#'
#' @return A list containing:
#'   * `results` - per-outcome diagnostics and objects
#'   * `combined_table` - combined e-value table across outcomes
#'   * `outcome_vars` - vector of outcome names
#'   * `not_missing` - indices of complete-case rows
#'   * (`data`, `covariates`, `weights`, `W`) when `save_data = TRUE`
#'   * `full_models` when `save_models = TRUE`
#'   * `flip_info` when `flip_outcomes` is used - contains mapping and metadata for flipped outcomes
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect variable_importance best_linear_projection
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @export
margot_causal_forest <- function(data, outcome_vars, covariates, W, weights,
                                 grf_defaults = list(),
                                 save_data = FALSE,
                                 compute_rate = TRUE,
                                 top_n_vars = 15,
                                 save_models = TRUE,
                                 train_proportion = 0.5,
                                 train_prop = NULL, # deprecated - use train_proportion
                                 qini_train_prop = NULL, # deprecated - use train_proportion
                                 compute_conditional_means = TRUE,
                                 compute_marginal_only = FALSE, # renamed and inverted
                                 verbose = TRUE,
                                 qini_treatment_cost = 1,
                                 seed = 12345,
                                 use_train_test_split = TRUE, # changed default to TRUE
                                 flip_outcomes = NULL,
                                 flip_method = "zscore",
                                 flip_scale_bounds = NULL) {
  # value:
  #   • plot_data – a list with elements
  #       • X_test       – test matrix restricted to `top_vars`
  #       • X_test_full  – full-width test matrix (all covariates)
  #       • predictions  – policy-tree action predictions

  # --- basic dimension checks ---
  n_rows <- nrow(covariates)
  if (verbose) cli::cli_alert_info(paste("number of rows in covariates:", n_rows))

  if (length(W) != n_rows) {
    stop("length of w does not match number of rows in covariates")
  }
  if (!is.null(weights)) {
    if (length(weights) != n_rows) {
      stop("length of weights does not match number of rows in covariates")
    }
  }
  for (outcome in outcome_vars) {
    if (nrow(as.matrix(data[[outcome]])) != n_rows) {
      stop("number of rows in outcome ", outcome, " does not match number of rows in covariates")
    }
  }
  # handle deprecated parameters
  if (!is.null(train_prop)) {
    cli::cli_alert_warning("Parameter 'train_prop' is deprecated. Please use 'train_proportion' instead.")
    # only override if train_proportion hasn't been explicitly set
    if (missing(train_proportion)) {
      train_proportion <- train_prop
    }
  }

  if (!is.null(qini_train_prop)) {
    cli::cli_alert_warning("Parameter 'qini_train_prop' is deprecated. Please use 'train_proportion' instead.")
    # only override if train_proportion hasn't been explicitly set
    if (missing(train_proportion)) {
      train_proportion <- qini_train_prop
    }
  }

  # validate parameter combinations
  if (compute_marginal_only && !is.null(train_proportion)) {
    stop("When compute_marginal_only = TRUE, train_proportion must be NULL (no splitting needed for marginal effects only)")
  }

  if (!compute_marginal_only && is.null(train_proportion)) {
    stop("When compute_marginal_only = FALSE, train_proportion must be specified for heterogeneity evaluation")
  }

  # validate train_proportion value if provided
  if (!is.null(train_proportion)) {
    if (train_proportion <= 0 || train_proportion >= 1) {
      stop("train_proportion must be between 0 and 1 (exclusive)")
    }
  }

  if (verbose) cli::cli_alert_info("starting margot_causal_forest function")

  # validate and prepare flip specifications if provided
  flip_spec <- NULL
  if (!is.null(flip_outcomes)) {
    if (verbose) cli::cli_alert_info("Validating flip specifications")
    flip_spec <- .validate_flip_spec(
      flip_outcomes = flip_outcomes,
      outcome_vars = outcome_vars,
      flip_method = flip_method,
      flip_scale_bounds = flip_scale_bounds
    )
    if (length(flip_spec) == 0) {
      cli::cli_alert_warning("No valid outcomes to flip found")
      flip_spec <- NULL
    } else if (verbose) {
      cli::cli_alert_success("Will flip {length(flip_spec)} outcome{?s}: {paste(names(flip_spec), collapse = ', ')}")
    }
  }

  # --- apply flipping transformations before computation ---
  flip_info <- NULL
  if (!is.null(flip_spec) && length(flip_spec) > 0) {
    if (verbose) cli::cli_alert_info("Applying outcome flipping transformations")
    
    # create a copy of data to avoid modifying the original
    data_processed <- data
    
    # track flipping information
    flip_info <- list()
    
    # apply flipping to each specified outcome
    for (outcome in names(flip_spec)) {
      spec <- flip_spec[[outcome]]
      
      # apply inversion
      flipped_values <- margot_invert_measure(
        x = data_processed[[outcome]],
        method = spec$method,
        scale_bounds = spec$scale_bounds
      )
      
      # create new column name with _r suffix
      flipped_name <- paste0(outcome, "_r")
      data_processed[[flipped_name]] <- flipped_values
      
      # remove original column to avoid confusion
      data_processed[[outcome]] <- NULL
      
      # store flip info
      flip_info[[flipped_name]] <- list(
        original_name = outcome,
        method = spec$method,
        scale_bounds = spec$scale_bounds
      )
      
      if (verbose) {
        original_mean <- mean(data[[outcome]], na.rm = TRUE)
        flipped_mean <- mean(flipped_values, na.rm = TRUE)
        cli::cli_alert_success("Flipped {outcome} -> {flipped_name} (mean: {round(original_mean, 3)} -> {round(flipped_mean, 3)})")
      }
    }
    
    # update outcome_vars to use flipped names
    outcome_vars <- sapply(outcome_vars, function(v) {
      if (v %in% names(flip_spec)) {
        return(paste0(v, "_r"))
      }
      return(v)
    })
    
    # use processed data
    data <- data_processed
  }

  # --- identify complete cases ---
  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[full %in% not_missing]
  if (length(not_missing) == 0) stop("no complete cases in covariates")
  if (verbose) cli::cli_alert_info(paste("number of complete cases:", length(not_missing)))

  # handle compute_marginal_only parameter
  if (compute_marginal_only) {
    if (verbose) {
      cli::cli_alert_info("compute_marginal_only = TRUE: Computing only marginal (average) treatment effects")
      cli::cli_alert_info("Skipping heterogeneity analysis (QINI, RATE, policy trees)")
    }
  } else {
    if (verbose) {
      cli::cli_alert_info("Computing full heterogeneity analysis with train_proportion = {train_proportion}")
    }
  }

  # --- determine train/test split if requested ---
  split_info <- NULL
  if (!compute_marginal_only && use_train_test_split) {
    if (verbose) cli::cli_alert_info("using train/test split for policy trees and QINI evaluation")
    n_non_missing <- length(not_missing)
    train_size <- floor(train_proportion * n_non_missing)
    if (train_size < 1) stop("train_proportion too low: resulting train_size is less than 1")

    set.seed(seed)
    global_train_indices <- sample(not_missing, train_size)
    global_test_indices <- setdiff(not_missing, global_train_indices)

    split_info <- list(
      use_train_test_split = TRUE,
      train_indices = global_train_indices,
      test_indices = global_test_indices,
      train_proportion = train_proportion
    )

    if (verbose) {
      cli::cli_alert_info(paste(
        "train set size:", length(global_train_indices),
        "| test set size:", length(global_test_indices)
      ))
    }
  }

  results <- list()
  full_models <- list()

  if (verbose) cli::cli_alert_info("running models for each outcome variable")
  pb <- cli::cli_progress_bar(
    total = length(outcome_vars),
    format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )

  for (outcome in outcome_vars) {
    model_name <- paste0("model_", outcome)
    Y <- as.matrix(data[[outcome]])

    tryCatch(
      {
        # --- 1) fit the main causal forest using all rows ---
        model <- do.call(
          grf::causal_forest,
          c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults)
        )

        # compute results based on whether we're using train/test split
        if (use_train_test_split && !compute_marginal_only) {
          # when using train/test split, main results are computed on TEST SET
          ate_test <- grf::average_treatment_effect(model, subset = global_test_indices)
          custom_table_test <- margot::margot_model_evalue(
            data.frame(estimate = ate_test[["estimate"]], std.err = ate_test[["std.err"]]),
            scale = "RD",
            new_name = outcome
          )

          # main results use test set
          results[[model_name]] <- list(
            ate = round(ate_test, 3),
            test_calibration = round(grf::test_calibration(model), 3),
            custom_table = custom_table_test,
            tau_hat = predict(model)$predictions
          )

          # also compute and store all-data results for reference
          ate_all <- grf::average_treatment_effect(model)
          custom_table_all <- margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL)

          results[[model_name]]$split_info <- list(
            use_train_test_split = TRUE,
            train_indices = global_train_indices,
            test_indices = global_test_indices,
            train_proportion = train_proportion,
            ate_all_data = round(ate_all, 3),
            custom_table_all_data = custom_table_all
          )
        } else {
          # standard behavior: compute on all data
          ate_all <- grf::average_treatment_effect(model)
          custom_table <- margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL)

          results[[model_name]] <- list(
            ate = round(ate_all, 3),
            test_calibration = round(grf::test_calibration(model), 3),
            custom_table = custom_table,
            tau_hat = predict(model)$predictions
          )
        }

        tau_hat <- results[[model_name]]$tau_hat

        if (!compute_marginal_only && compute_rate) {
          results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
          attr(results[[model_name]]$rate_result, "target") <- "AUTOC"
          results[[model_name]]$rate_qini <- grf::rank_average_treatment_effect(model, tau_hat, target = "QINI")
          attr(results[[model_name]]$rate_qini, "target") <- "QINI"
        } else if (compute_marginal_only) {
          # create empty structure for pipeline compatibility
          results[[model_name]]$rate_result <- list(
            estimate = NA,
            std.err = NA,
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
          results[[model_name]]$rate_qini <- list(
            estimate = NA,
            std.err = NA,
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
        }

        # --- compute conditional means if requested ---
        if (compute_conditional_means && save_models) {
          if (compute_marginal_only) {
            # create empty structure for pipeline compatibility
            results[[model_name]]$conditional_means <- list(
              computed = FALSE,
              reason = "compute_marginal_only set to TRUE"
            )
          } else {
            if (requireNamespace("policytree", quietly = TRUE)) {
              tryCatch(
                {
                  results[[model_name]]$conditional_means <- policytree::conditional_means(model)
                  if (verbose) cli::cli_alert_info("computed conditional means for {outcome}")
                },
                error = function(e) {
                  if (verbose) cli::cli_alert_warning("could not compute conditional means for {outcome}: {e$message}")
                  results[[model_name]]$conditional_means <- NULL
                }
              )
            } else {
              if (verbose) cli::cli_alert_warning("policytree package not available for conditional means")
            }
          }
        }

        # --- 2) compute doubly robust scores and policy trees ---
        results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)

        # --- 3) variable importance and best linear projection ---
        varimp <- grf::variable_importance(model)
        ranked_vars <- order(varimp, decreasing = TRUE)
        # if covariates have no column names, use numeric indices
        if (is.null(colnames(covariates))) {
          top_vars <- ranked_vars[1:top_n_vars]
        } else {
          top_vars <- colnames(covariates)[ranked_vars[1:top_n_vars]]
        }
        results[[model_name]]$top_vars <- top_vars

        results[[model_name]]$blp_top <- grf::best_linear_projection(
          model,
          covariates[, top_vars, drop = FALSE],
          target.sample = "all"
        )

        # --- 4) compute policy trees using top_vars ---
        if (compute_marginal_only) {
          # create placeholder for pipeline compatibility
          results[[model_name]]$policy_tree_depth_1 <- list(
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
        }

        # determine train/test indices for policy trees
        if (!compute_marginal_only) {
          if (use_train_test_split) {
            # use global train/test split
            train_indices <- global_train_indices
            test_indices <- global_test_indices
          } else {
            # create new split specific to this model
            n_non_missing <- length(not_missing)
            train_size <- floor(train_proportion * n_non_missing)
            if (train_size < 1) stop("train_proportion too low: resulting train_size is less than 1")
            set.seed(seed + as.integer(factor(model_name)))
            train_indices <- sample(not_missing, train_size)
            test_indices <- setdiff(not_missing, train_indices)
          }

          if (length(test_indices) < 1) stop("test set is empty after splitting for policy tree")
        }

        if (!compute_marginal_only) {
          results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(
            covariates[train_indices, top_vars, drop = FALSE],
            results[[model_name]]$dr_scores[train_indices, ],
            depth = 1
          )
        }

        if (!compute_marginal_only) {
          policy_tree_model <- policytree::policy_tree(
            covariates[train_indices, top_vars, drop = FALSE],
            results[[model_name]]$dr_scores[train_indices, ],
            depth = 2
          )
          results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

          X_test <- covariates[test_indices, top_vars, drop = FALSE]
          X_test_full <- covariates[test_indices, , drop = FALSE]
          predictions <- predict(policy_tree_model, X_test)

          results[[model_name]]$plot_data <- list(
            X_test       = X_test,
            X_test_full  = X_test_full, # <- NEW
            predictions  = predictions,
            test_indices = test_indices
          )
        } else {
          # create placeholder for pipeline compatibility
          results[[model_name]]$policy_tree_depth_2 <- list(
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
          results[[model_name]]$plot_data <- list(
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
        }

        # --- 4) compute qini curves ---
        if (!compute_marginal_only) {
          # Always use honest evaluation for QINI
          if (verbose) cli::cli_alert_info("performing train/test split for honest QINI evaluation")

          # determine train/test indices for QINI
          if (use_train_test_split) {
            # use global train/test split
            qini_train_idxs <- global_train_indices
            qini_test_idxs <- global_test_indices
          } else {
            # create new split specific to QINI
            qini_n <- length(not_missing)
            qini_train_size <- floor(train_proportion * qini_n) # use train_proportion
            if (qini_train_size < 1 || qini_train_size >= qini_n) {
              stop("invalid train_proportion: results in empty train or test set for qini evaluation")
            }
            set.seed(seed + as.integer(factor(model_name)) + 1000) # different seed component for qini
            qini_train_idxs <- sample(not_missing, qini_train_size)
            qini_test_idxs <- setdiff(not_missing, qini_train_idxs)
          }

          if (length(qini_test_idxs) < 1) stop("qini test set is empty")
          if (verbose) cli::cli_alert_info("fitting separate forest for qini on qini-train subset")
          qini_model <- do.call(
            grf::causal_forest,
            c(
              list(
                X = covariates[qini_train_idxs, , drop = FALSE],
                Y = Y[qini_train_idxs],
                W = W[qini_train_idxs],
                sample.weights = if (!is.null(weights)) weights[qini_train_idxs] else NULL
              ),
              grf_defaults
            )
          )
          qini_tau_hat <- predict(qini_model, newdata = covariates[qini_test_idxs, , drop = FALSE])$predictions
          if (verbose) cli::cli_alert_info("computing QINI curves on test subset with improved DR scores")
          qini_result <- compute_qini_improved(
            Y = Y[qini_test_idxs],
            W = W[qini_test_idxs],
            X = covariates[qini_test_idxs, , drop = FALSE],
            tau_hat = qini_tau_hat,
            weights = if (!is.null(weights)) weights[qini_test_idxs] else NULL,
            seed = seed,
            n_bootstrap = 200,
            verbose = verbose
          )
          if (!is.null(qini_result)) {
            results[[model_name]]$qini_data <- qini_result$qini_data
            results[[model_name]]$qini_objects <- qini_result$qini_objects
            # store the evaluation forest ATE
            if (!is.null(qini_result$ate_evaluation_forest)) {
              results[[model_name]]$ate_evaluation_forest <- qini_result$ate_evaluation_forest
            }
            # store metadata about qini generation
            results[[model_name]]$qini_metadata <- list(
              n_test = length(qini_test_idxs),
              n_train = length(qini_train_idxs),
              test_indices = qini_test_idxs,
              honest_qini = TRUE, # always use honest evaluation
              baseline_method = "grf_standard" # new GRF-standard approach
            )
          } else {
            if (verbose) cli::cli_alert_warning(crayon::yellow(paste("unable to compute QINI curves for", outcome)))
          }
        } else { # compute_marginal_only
          # create empty structure for pipeline compatibility
          results[[model_name]]$qini_data <- data.frame(
            proportion = numeric(0),
            gain = numeric(0),
            curve = character(0)
          )
          results[[model_name]]$qini_objects <- list(
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE"
          )
          results[[model_name]]$qini_metadata <- list(
            computed = FALSE,
            reason = "compute_marginal_only set to TRUE",
            can_compute_later = TRUE
          )
        }

        # --- 5) save the main model object if requested ---
        if (save_models) {
          full_models[[model_name]] <- model
        }
      },
      error = function(e) {
        if (verbose) cli::cli_alert_danger(crayon::red(paste("error in model for", outcome, ":", e$message)))
      }
    )

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # filter out NULL results from errors
  valid_results <- Filter(Negate(is.null), results)

  if (length(valid_results) == 0) {
    stop("All models failed. Check your data and parameters.")
  }

  combined_table <- do.call(rbind, lapply(valid_results, function(x) x$custom_table))
  rownames(combined_table) <- gsub("model_", "", names(valid_results))

  if (verbose) cli::cli_alert_success(crayon::green("model runs completed successfully"))

  output <- list(
    results = valid_results,
    combined_table = combined_table,
    outcome_vars = outcome_vars,
    not_missing = not_missing
  )

  # add computation status and parameters
  output$computation_status <- list(
    heterogeneity_computed = !compute_marginal_only,
    parameters_saved = TRUE,
    version = packageVersion("margot")
  )

  output$computation_params <- list(
    compute_marginal_only = compute_marginal_only,
    train_proportion = train_proportion,
    compute_rate = compute_rate,
    qini_treatment_cost = qini_treatment_cost,
    seed = seed,
    top_n_vars = top_n_vars,
    use_train_test_split = use_train_test_split,
    compute_conditional_means = compute_conditional_means
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
    output$W <- W
    if (verbose) cli::cli_alert_info("data, covariates, weights, and W saved in output")
  }
  if (save_models) {
    output$full_models <- full_models
    if (verbose) cli::cli_alert_info("full model objects saved in output")
  }
  
  # add flip info if any outcomes were flipped
  if (!is.null(flip_info)) {
    output$flip_info <- flip_info
    if (verbose) cli::cli_alert_info("flip information saved in output")
  }

  if (verbose) cli::cli_alert_success("margot_causal_forest function completed successfully \U0001F44D")

  return(output)
}


#' #' Run Multiple Generalised Random Forest (GRF) Causal Forest Models in Parallel
#' #'
#' #' Parallelised, diagnostic‑rich variant of `margot_causal_forest()`.  Each
#' #' outcome‑specific forest is estimated in its own R worker via **future**.  All
#' #' the `cli` messages and checks from the sequential original are preserved, so
#' #' you still get the same granular reporting (dimension checks, Qini status,
#' #' warnings, etc.).  Live progress bars are emitted with **progressr** using a
#' #' `cli` handler.
#' #'
#' #' @inheritParams margot_causal_forest
#' #' @param n_cores integer. number of parallel workers (default = all cores − 1).
#' #'
#' #' @return list with elements:
#' #'   * `results`         – per‑outcome diagnostics and objects
#' #'   * `combined_table`  – rbind‑ed e‑value table across outcomes
#' #'   * `outcome_vars`    – vector of (successful) outcome names
#' #'   * `not_missing`     – indices of complete‑case rows
#' #'   * (`data`, `covariates`, `weights`, `W`) when `save_data = TRUE`
#' #'   * `full_models` when `save_models = TRUE`
#' #'
#' #' @details Messages produced inside workers are captured by **future** and
#' #'   dispatched to the master session.  Progress bars update in real time.  To
#' #'   silence progress, call `progressr::handlers("off")` before running.
#' #'
#' #' @importFrom future plan multisession availableCores
#' #' @importFrom future.apply future_lapply
#' #' @importFrom progressr with_progress progressor handlers
#' #' @importFrom grf causal_forest average_treatment_effect test_calibration
#' #'   rank_average_treatment_effect variable_importance best_linear_projection
#' #' @importFrom policytree double_robust_scores policy_tree
#' #' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_h3
#' #'   cli_progress_bar cli_progress_update cli_progress_done
#' #' @importFrom purrr map walk
#' #' @name margot_causal_forest_parallel
#' #' @keywords internal
#' margot_causal_forest_parallel  <- function(data, outcome_vars, covariates, W, weights,
#'                                            grf_defaults    = list(),
#'                                            save_data       = FALSE,
#'                                            compute_rate    = TRUE,
#'                                            top_n_vars      = 15,
#'                                            save_models     = TRUE,
#'                                            train_proportion= 0.5,
#'                                            qini_split      = TRUE,
#'                                            train_prop      = 0.5,
#'                                            qini_train_prop = NULL,  # deprecated
#'                                            compute_conditional_means = TRUE,
#'                                            n_cores         = future::availableCores() - 1,
#'                                            verbose         = TRUE,
#'                                            qini_treatment_cost = 1,
#'                                            seed            = 12345) {
#'   # dimension checks
#'   n_rows <- nrow(covariates)
#'   if (verbose) cli::cli_alert_info(paste0("rows in covariates: ", n_rows))
#'   if (length(W) != n_rows) stop("length of W does not match covariates rows")
#'   if (!is.null(weights) && length(weights) != n_rows) stop("length of weights does not match")
#'   for (o in outcome_vars) {
#'     if (nrow(as.matrix(data[[o]])) != n_rows)
#'       stop("rows in outcome ", o, " do not match covariates")
#'   }
#'   # handle deprecated qini_train_prop parameter
#'   if (!is.null(qini_train_prop)) {
#'     if (verbose) cli::cli_alert_warning("Parameter 'qini_train_prop' is deprecated. Please use 'train_prop' instead.")
#'     train_prop <- qini_train_prop
#'   }
#'
#'   if (qini_split && (train_prop <= 0 || train_prop >= 1))
#'     stop("train_prop must be in (0,1)")
#'   if (verbose) cli::cli_alert_info("starting margot_causal_forest()")
#'
#'   # complete cases
#'   not_missing <- which(complete.cases(covariates))
#'   if (length(not_missing)==0) stop("no complete cases in covariates")
#'   if (verbose) cli::cli_alert_info(paste0("complete cases: ", length(not_missing)))
#'
#'   # parallel setup
#'   future::plan(future::multisession, workers = n_cores)
#'   if (verbose) cli::cli_alert_info(paste0("using ", n_cores, " workers"))
#'
#'   # parallel loop with CLI logging
#'   results_list <- future.apply::future_lapply(outcome_vars, function(outcome) {
#'     t0 <- Sys.time()
#'     cli::cli_alert_info(paste0("↗ starting ", outcome))
#'
#'     res <- tryCatch({
#'       # fit forest
#'       model <- do.call(grf::causal_forest,
#'                        c(list(X = covariates,
#'                               Y = as.matrix(data[[outcome]]),
#'                               W = W,
#'                               sample.weights = weights),
#'                          grf_defaults))
#'       # core metrics
#'       ate    <- round(grf::average_treatment_effect(model),3)
#'       calib  <- round(grf::test_calibration(model),3)
#'       table  <- margot::margot_model_evalue(model, scale="RD",
#'                                             new_name=outcome, subset=NULL)
#'       tau    <- predict(model)$predictions
#'
#'       # RATE
#'       if (compute_rate) {
#'         rate     <- grf::rank_average_treatment_effect(model, tau)
#'         attr(rate, "target") <- "AUTOC"
#'         rate_qini<- grf::rank_average_treatment_effect(model, tau, target="QINI")
#'         attr(rate_qini, "target") <- "QINI"
#'       }
#'       # conditional means
#'       cond_means <- NULL
#'       if (compute_conditional_means) {
#'         if (requireNamespace("policytree", quietly = TRUE)) {
#'           tryCatch({
#'             cond_means <- policytree::conditional_means(model)
#'             if (verbose) cli::cli_alert_info("computed conditional means for {outcome}")
#'           }, error = function(e) {
#'             if (verbose) cli::cli_alert_warning("could not compute conditional means for {outcome}: {e$message}")
#'           })
#'         }
#'       }
#'       # DR scores
#'       dr   <- policytree::double_robust_scores(model)
#'       # var importance + BLP
#'       vi     <- grf::variable_importance(model)
#'       ord    <- order(vi, decreasing=TRUE)
#'       top    <- if(is.null(colnames(covariates))) ord[1:top_n_vars] else colnames(covariates)[ord[1:top_n_vars]]
#'       blp    <- grf::best_linear_projection(model, covariates[,top,drop=FALSE], target.sample="all")
#'       # policy trees using top vars
#'       # depth-1 using top vars (not all covariates)
#'       pt1    <- policytree::policy_tree(covariates[not_missing,top,drop=FALSE],
#'                                       dr[not_missing,], depth=1)
#'       # depth-2 + plot data
#'       set.seed(seed + as.integer(factor(outcome)))
#'       train_idx <- sample(not_missing, floor(train_proportion*length(not_missing)))
#'       pt2    <- policytree::policy_tree(covariates[train_idx,top,drop=FALSE], dr[train_idx,], depth=2)
#'       test_idx<- setdiff(not_missing, train_idx)
#'       plotd  <- list(X_test=covariates[test_idx,top,drop=FALSE],
#'                      X_test_full=covariates[test_idx,,drop=FALSE],
#'                      predictions=predict(pt2, covariates[test_idx,top,drop=FALSE]))
#'       # Qini
#'       if (!qini_split) {
#'         qres <- compute_qini_curves_binary(tau, as.matrix(data[[outcome]]), W,
#'                                           weights = weights, seed = seed, verbose = verbose, treatment_cost = qini_treatment_cost)
#'       } else {
#'         set.seed(seed + as.integer(factor(outcome)) + 1000)  # different seed component for qini
#'         qtr <- sample(not_missing, floor(train_prop*length(not_missing)))
#'         qte <- setdiff(not_missing, qtr)
#'         qmod<- do.call(grf::causal_forest,
#'                        c(list(X=covariates[qtr,,drop=FALSE],
#'                               Y=as.matrix(data[[outcome]])[qtr],
#'                               W=W[qtr], sample.weights=weights[qtr]),
#'                          grf_defaults))
#'         qtau<- predict(qmod, newdata=covariates[qte,,drop=FALSE])$predictions
#'         qres <- compute_qini_curves_binary(qtau, as.matrix(data[[outcome]])[qte], W[qte],
#'                                           weights = if(!is.null(weights)) weights[qte] else NULL,
#'                                           seed = 42, verbose = verbose, treatment_cost = qini_treatment_cost)
#'       }
#'       qdata   <- if(!is.null(qres)) qres$qini_data else NULL
#'       qobjs   <- if(!is.null(qres)) qres$qini_objects else NULL
#'
#'       out <- list(
#'         ate=ate, test_calibration=calib, custom_table=table, tau_hat=tau,
#'         conditional_means=cond_means,
#'         rate_result=if(compute_rate) rate else NULL,
#'         rate_qini=if(compute_rate) rate_qini else NULL,
#'         dr_scores=dr, policy_tree_depth_1=pt1,
#'         top_vars=top, blp_top=blp,
#'         policy_tree_depth_2=pt2, plot_data=plotd,
#'         qini_data=qdata, qini_objects=qobjs,
#'         model=if(save_models) model else NULL
#'       )
#'       dt <- difftime(Sys.time(), t0, units="secs")
#'       cli::cli_alert_success(sprintf("✓ worker finished %s (%.1f s)", outcome, as.numeric(dt)))
#'       out
#'     }, error=function(e) {
#'       cli::cli_alert_danger(paste0("✗ error in ", outcome, ": ", e$message))
#'       NULL
#'     })
#'     res
#'   })
#'   names(results_list) <- paste0("model_", outcome_vars)
#'
#'   # drop failures
#'   ok <- !vapply(results_list, is.null, logical(1))
#'   if (!all(ok)) {
#'     cli::cli_alert_warning(paste(sum(!ok), "outcome(s) failed and were dropped"))
#'     results_list <- results_list[ok]
#'     outcome_vars <- outcome_vars[ok]
#'   }
#'
#'   # combine
#'   combined_table <- do.call(rbind, lapply(results_list, `[[`, "custom_table"))
#'   rownames(combined_table) <- gsub("model_", "", names(results_list))
#'   if (verbose) cli::cli_alert_success("all model runs completed")
#'
#'   output <- list(
#'     results=results_list,
#'     combined_table=combined_table,
#'     outcome_vars=outcome_vars,
#'     not_missing=not_missing
#'   )
#'   if (save_data) {
#'     output$data<-data; output$covariates<-covariates; output$weights<-weights; output$W<-W
#'     if (verbose) cli::cli_alert_info("raw data/covariates/weights/W saved")
#'   }
#'   if (save_models) {
#'     output$full_models<-purrr::map(results_list, `[[`, "model")
#'     if (verbose) cli::cli_alert_info("full model objects saved")
#'   }
#'   if (verbose) cli::cli_alert_success("margot_causal_forest() completed")
#'   output
#' }
