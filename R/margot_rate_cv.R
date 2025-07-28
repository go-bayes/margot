#' Cross-Validation Test for Treatment Effect Heterogeneity
#'
#' @description
#' Performs uncorrelated sequential cross-validation to test for treatment effect
#' heterogeneity using the approach recommended by the grf package (Wager 2024). 
#' This method provides robust statistical testing that avoids overfitting by using 
#' sequential, non-overlapping training sets.
#'
#' @param model_results Output from `margot_causal_forest()` or `margot_flip_forests()`
#' @param num_folds Integer. Number of cross-validation folds (default 5). Must be at least 3.
#' @param target Character vector. RATE targets to test: "AUTOC", "QINI", or 
#'   c("AUTOC", "QINI") for both (default). When both are specified, results
#'   are combined with adjusted p-values accounting for testing both metrics.
#' @param model_names Character vector. Specific models to test (default NULL tests all)
#' @param use_full_data Logical. If TRUE and data is available, refit forests on full data 
#'   rather than using the training split (default TRUE)
#' @param seed Integer or NULL. Random seed for reproducibility. If NULL, defaults to 12345
#' @param verbose Logical. Print progress messages (default TRUE)
#' @param parallel Logical. Use parallel processing for multiple models (default FALSE).
#'   Note: Parallel processing requires the margot package to be installed (not just loaded
#'   with devtools::load_all()). If the package is not installed, the function will fall
#'   back to sequential processing with a warning.
#' @param n_cores Integer. Number of cores for parallel processing when parallel = TRUE 
#'   (default all cores - 1)
#' @param future_globals_maxSize Numeric. Maximum allowed size for exporting globals to 
#'   parallel workers in GiB. Default is 22. Set to Inf to disable the limit.
#' @param alpha Numeric. Significance level for hypothesis tests (default 0.05).
#'   When using Bonferroni correction, consider alpha = 0.2 due to its conservative nature.
#' @param adjust Character. Multiple testing adjustment method. Only "bonferroni" or "none" 
#'   are valid for cross-validation due to the martingale aggregation method. Default is "none".
#'   Other methods like BH/BY/FDR are not statistically appropriate for CV aggregation.
#' @param label_mapping Optional named list for custom label mappings. Keys should be model names
#'   (with or without "model_" prefix), and values should be the desired display labels.
#' @param ... Additional arguments passed to grf::causal_forest
#'
#' @return A list containing:
#'   \item{cv_results}{Data frame with CV test results for each model}
#'   \item{interpretation}{Character string interpreting the results}
#'   \item{significant_models}{Character vector of models with significant heterogeneity}
#'   \item{method_details}{List with details about the CV procedure}
#'
#' @details
#' The function implements the sequential cross-validation approach from 
#' Wager (2024) and Nie & Wager (2021). This method:
#' - Splits data into K folds
#' - Trains on folds 1 to k-1, tests on fold k
#' - Aggregates t-statistics using the martingale property
#' - Provides valid p-values for heterogeneity testing
#'
#' The null hypothesis is that there is no treatment effect heterogeneity
#' (all individuals have the same treatment effect).
#'
#' ## Multiple Testing Correction
#' 
#' Due to the martingale aggregation method used in CV, only Bonferroni correction
#' or no correction are statistically valid. Methods like Benjamini-Hochberg (BH)
#' or false discovery rate (FDR) control are not appropriate because they assume
#' independent p-values, which is violated by the sequential CV structure.
#' 
#' Bonferroni correction is very conservative, especially with noisy heterogeneous
#' treatment effect models. We recommend using alpha = 0.2 when applying Bonferroni
#' correction to maintain reasonable statistical power.
#'
#' For memory efficiency with large model objects, the function pre-extracts
#' only the necessary data for each model before parallel processing. However,
#' parallel processing is currently experimental and disabled by default due to
#' potential memory issues with large model objects. If you want to try parallel
#' processing, set parallel = TRUE and consider increasing the future.globals.maxSize
#' option with: options(future.globals.maxSize = 15 * 1024^3) # 15 GB
#'
#' @examples
#' \dontrun{
#' # Run causal forest
#' cf_results <- margot_causal_forest(
#'   data = mydata,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   covariates = covariates,
#'   W = treatment,
#'   save_data = TRUE  # Important for CV
#' )
#'
#' # Test for heterogeneity with cross-validation (both targets by default)
#' cv_results <- margot_rate_cv(
#'   model_results = cf_results,
#'   num_folds = 5,
#'   alpha = 0.2,  # recommended with Bonferroni
#'   adjust = "bonferroni"
#' )
#'
#' # With custom labels
#' label_mapping <- list(
#'   "model_happiness" = "Happiness",
#'   "model_depression" = "Depression"
#' )
#' cv_results <- margot_rate_cv(
#'   model_results = cf_results,
#'   label_mapping = label_mapping
#' )
#'
#' # Test single target only
#' cv_results_autoc <- margot_rate_cv(
#'   model_results = cf_results,
#'   target = "AUTOC"
#' )
#'
#' # View results
#' cat(cv_results$interpretation)
#' 
#' # Create visualization
#' plot <- margot_plot_cv_results(cv_results)
#' }
#'
#' @references
#' Wager, S. (2024). Sequential validation for heterogeneous treatment effects. 
#' arXiv preprint arXiv:2401.06818.
#' 
#' Nie, X., & Wager, S. (2021). Quasi-oracle estimation of heterogeneous treatment effects.
#' Biometrika, 108(2), 299-319.
#'
#' @export
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom grf causal_forest get_scores rank_average_treatment_effect.fit
#' @importFrom stats pnorm p.adjust
#' @importFrom tibble tibble
margot_rate_cv <- function(model_results,
                          num_folds = 5,
                          target = c("AUTOC", "QINI"),
                          model_names = NULL,
                          use_full_data = TRUE,
                          seed = 12345,
                          verbose = TRUE,
                          parallel = FALSE,
                          n_cores = future::availableCores() - 1,
                          future_globals_maxSize = 22,
                          alpha = 0.05,
                          adjust = "none",
                          label_mapping = NULL,
                          ...) {
  
  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- 12345
  }
  set.seed(seed)
  
  # Input validation
  if (!is.list(model_results) || 
      !all(c("results", "outcome_vars") %in% names(model_results))) {
    stop("model_results must be output from margot_causal_forest() or margot_flip_forests()")
  }
  
  if (num_folds < 3) {
    stop("num_folds must be at least 3 for valid cross-validation")
  }
  
  # handle target parameter - can be single value or vector
  if (length(target) > 1) {
    # default behavior - test both
    target <- c("AUTOC", "QINI")
    test_both <- TRUE
  } else {
    target <- match.arg(target, c("AUTOC", "QINI"))
    test_both <- FALSE
  }
  
  # validate adjust parameter for CV
  if (!adjust %in% c("none", "bonferroni")) {
    stop("For cross-validation, only 'bonferroni' or 'none' are valid for the adjust parameter. ",
         "Other methods like BH/BY/FDR are not appropriate due to the martingale aggregation method.")
  }
  
  # suggest higher alpha for bonferroni
  if (adjust == "bonferroni" && alpha == 0.05 && verbose) {
    cli::cli_alert_info(
      "Using Bonferroni correction with alpha = 0.05. Due to its conservative nature with noisy models, ",
      "consider using alpha = 0.2 for more reasonable power."
    )
  }
  
  # Determine which models to test
  if (is.null(model_names)) {
    model_names <- names(model_results$results)
  }
  
  # Check if we have the necessary data
  has_full_data <- !is.null(model_results$data) && 
                   !is.null(model_results$covariates) && 
                   !is.null(model_results$W)
  
  if (use_full_data && !has_full_data) {
    if (verbose) {
      cli::cli_alert_warning(
        "Full data not available. Set save_data = TRUE in margot_causal_forest() for best CV results."
      )
      cli::cli_alert_info("Proceeding with available forest objects only.")
    }
    use_full_data <- FALSE
  }
  
  if (verbose) {
    cli::cli_h2("Cross-Validation Heterogeneity Testing")
    cli::cli_alert_info("Testing {length(model_names)} models with {num_folds}-fold CV")
    if (test_both) {
      cli::cli_alert_info("Targets: AUTOC and QINI (both)")
    } else {
      cli::cli_alert_info("Target: {target}")
    }
  }
  
  # Pre-extract data for each model to avoid sending large objects to workers
  if (verbose) cli::cli_alert_info("Pre-extracting data for {length(model_names)} models")
  
  model_data_list <- list()
  
  # Progress bar for data extraction
  pb_extract_id <- NULL
  if (verbose && length(model_names) > 1) {
    pb_extract_id <- cli::cli_progress_bar("Extracting model data", total = length(model_names))
  }
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    
    # Update progress
    if (verbose && length(model_names) > 1 && !is.null(pb_extract_id)) {
      cli::cli_progress_update(id = pb_extract_id)
    }
    
    # Get the model
    if (!is.null(model_results$full_models) && 
        model_name %in% names(model_results$full_models)) {
      forest <- model_results$full_models[[model_name]]
    } else {
      if (verbose) {
        cli::cli_alert_warning("Forest object not found for {model_name}")
      }
      model_data_list[[i]] <- NULL
      next
    }
    
    # Extract data for CV
    if (use_full_data && has_full_data) {
      # Use full data
      outcome_var <- gsub("^model_", "", model_name)
      Y <- as.matrix(model_results$data[[outcome_var]])
      X <- model_results$covariates
      W <- model_results$W
      weights <- model_results$weights
      
      # Handle missing data
      not_missing <- model_results$not_missing
      if (!is.null(not_missing)) {
        Y <- Y[not_missing, , drop = FALSE]
        X <- X[not_missing, , drop = FALSE]
        W <- W[not_missing]
        if (!is.null(weights)) weights <- weights[not_missing]
      }
      
      grf_defaults <- list()
      if (!is.null(forest$tunable.params)) {
        grf_defaults <- forest$tunable.params
      }
      
    } else {
      # Extract from forest object
      Y <- forest$Y.orig
      X <- forest$X.orig
      W <- forest$W.orig
      weights <- forest$sample.weights
      grf_defaults <- forest$tunable.params
    }
    
    # Store extracted data
    model_data_list[[i]] <- list(
      model_name = model_name,
      X = X,
      Y = Y,
      W = W,
      weights = weights,
      grf_defaults = grf_defaults,
      seed = seed + i
    )
  }
  
  # Remove NULL entries
  model_data_list <- model_data_list[!sapply(model_data_list, is.null)]
  
  # Close progress bar
  if (verbose && length(model_names) > 1 && !is.null(pb_extract_id)) {
    cli::cli_progress_done(id = pb_extract_id)
  }
  
  if (length(model_data_list) == 0) {
    stop("No valid models found for cross-validation testing")
  }
  
  # Set up parallel processing if requested
  if (parallel && length(model_data_list) > 1) {
    # Check if margot package is installed (needed for parallel processing)
    if (!requireNamespace("margot", quietly = TRUE)) {
      if (verbose) {
        cli::cli_alert_warning(
          "Parallel processing requires the margot package to be installed. ",
          "Use devtools::install() or set parallel = FALSE. ",
          "Falling back to sequential processing."
        )
      }
      parallel <- FALSE
    } else {
      # Save current globals.maxSize
      old_maxSize <- getOption("future.globals.maxSize")
      
      # Set new limit (convert GiB to bytes) BEFORE creating the plan
      new_maxSize <- if (is.infinite(future_globals_maxSize)) {
        Inf
      } else {
        future_globals_maxSize * 1024^3
      }
      options(future.globals.maxSize = new_maxSize)
      
      # Ensure we restore the original setting on exit
      on.exit({
        options(future.globals.maxSize = old_maxSize)
        # Reset to sequential plan
        future::plan(future::sequential)
      }, add = TRUE)
      
      # Set up parallel plan AFTER setting memory limit
      future::plan(future::multisession, workers = n_cores)
      
      if (verbose) {
        cli::cli_alert_info("Using {n_cores} cores for parallel processing")
        cli::cli_alert_info("Memory limit for globals: {if (is.infinite(future_globals_maxSize)) 'unlimited' else paste0(future_globals_maxSize, ' GiB')}")
      }
    }
  }
  
  # Run CV for each model and target combination
  if (test_both) {
    # test both AUTOC and QINI
    cv_list <- list()
    total_tests <- length(model_data_list) * 2  # Testing both AUTOC and QINI
    current_test <- 0
    
    for (tgt in c("AUTOC", "QINI")) {
      # Initialize timing
      start_time <- NULL
      
      if (verbose) {
        cli::cli_alert_info("Running CV for {tgt}...")
        if (parallel) {
          cli::cli_alert_info("Processing {length(model_data_list)} models in parallel (progress updates not available during parallel execution)")
          # Add timing information
          start_time <- Sys.time()
        }
      }
      
      # Progress bar for non-parallel processing
      if (verbose && !parallel && length(model_data_list) > 1) {
        pb_id <- cli::cli_progress_bar(
          format = "Testing {tgt}: {cli::pb_current}/{cli::pb_total} models [{cli::pb_percent}] {cli::pb_bar} ETA: {cli::pb_eta}",
          total = length(model_data_list),
          clear = FALSE
        )
      }
      
      # Counter for progress updates
      model_counter <- 0
      
      # wrap in tryCatch for better error handling
      cv_list_target <- tryCatch({
        if (parallel) {
          # For parallel, explicitly export the function and data
          future.apply::future_lapply(
            seq_along(model_data_list), 
            function(idx) {
          model_data <- model_data_list[[idx]]
          if (verbose && !parallel) {
            # Update progress bar
            if (length(model_data_list) > 1) {
              cli::cli_progress_update(id = pb_id)
            }
          }
          
          # Run sequential CV
          cv_result <- rate_sequential_cv(
            X = model_data$X,
            Y = model_data$Y,
            W = model_data$W,
            weights = model_data$weights,
            num_folds = num_folds,
            target = tgt,
            grf_defaults = model_data$grf_defaults,
            seed = model_data$seed,
            verbose = FALSE,  # No verbose output in parallel workers
            model_name = model_data$model_name,
            ...
          )
          
          cv_result$model_name <- model_data$model_name
          cv_result$target <- tgt
          cv_result
        },
            future.globals = list(
              rate_sequential_cv = rate_sequential_cv,
              model_data_list = model_data_list,
              num_folds = num_folds,
              tgt = tgt,
              verbose = FALSE
            ),
            future.packages = c("margot", "grf", "cli"),
            future.stdout = FALSE,  # Suppress stdout from workers
            future.seed = TRUE
          )
        } else {
          # For sequential, use regular lapply
          lapply(seq_along(model_data_list), function(idx) {
            model_data <- model_data_list[[idx]]
            if (verbose) {
              # Update progress bar
              if (length(model_data_list) > 1) {
                cli::cli_progress_update(id = pb_id)
              }
            }
            
            # Run sequential CV
            cv_result <- rate_sequential_cv(
              X = model_data$X,
              Y = model_data$Y,
              W = model_data$W,
              weights = model_data$weights,
              num_folds = num_folds,
              target = tgt,
              grf_defaults = model_data$grf_defaults,
              seed = model_data$seed,
              verbose = FALSE,
              model_name = model_data$model_name,
              ...
            )
            
            cv_result$model_name <- model_data$model_name
            cv_result$target <- tgt
            cv_result
          })
        }
      }, error = function(e) {
        if (grepl("globals.*exceeds", e$message)) {
          stop(paste0(
            "Memory limit exceeded for parallel processing. ",
            "Try increasing future_globals_maxSize (current: ", future_globals_maxSize, " GiB) ",
            "or set parallel = FALSE. Original error: ", e$message
          ))
        } else {
          stop(e)
        }
      })
      
      # Close progress bar or show timing for parallel
      if (verbose) {
        if (!parallel && length(model_data_list) > 1) {
          cli::cli_progress_done(id = pb_id)
        } else if (parallel && !is.null(start_time)) {
          # Show elapsed time for parallel processing
          elapsed <- difftime(Sys.time(), start_time, units = "secs")
          cli::cli_alert_success("Completed {tgt} testing in {round(elapsed, 1)} seconds")
        }
      }
      
      cv_list <- c(cv_list, cv_list_target)
    }
  } else {
    # test single target
    # Initialize timing
    start_time <- NULL
    
    if (verbose) {
      cli::cli_alert_info("Running CV for {target}...")
      if (parallel) {
        cli::cli_alert_info("Processing {length(model_data_list)} models in parallel (progress updates not available during parallel execution)")
        # Add timing information
        start_time <- Sys.time()
      }
    }
    
    # Progress bar for non-parallel processing
    if (verbose && !parallel && length(model_data_list) > 1) {
      pb_id <- cli::cli_progress_bar(
        format = "Testing {target}: {cli::pb_current}/{cli::pb_total} models [{cli::pb_percent}] {cli::pb_bar} ETA: {cli::pb_eta}",
        total = length(model_data_list),
        clear = FALSE
      )
    }
    
    # wrap in tryCatch for better error handling
    cv_list <- tryCatch({
      if (parallel) {
        # For parallel, explicitly export the function and data
        future.apply::future_lapply(
          seq_along(model_data_list), 
          function(idx) {
        model_data <- model_data_list[[idx]]
        
        if (verbose && !parallel) {
          # Update progress bar
          if (length(model_data_list) > 1) {
            cli::cli_progress_update(id = pb_id)
          }
        }
        
        # Run sequential CV
        cv_result <- rate_sequential_cv(
          X = model_data$X,
          Y = model_data$Y,
          W = model_data$W,
          weights = model_data$weights,
          num_folds = num_folds,
          target = target,
          grf_defaults = model_data$grf_defaults,
          seed = model_data$seed,
          verbose = FALSE,  # No verbose output in parallel workers
          model_name = model_data$model_name,
          ...
        )
        
        cv_result$model_name <- model_data$model_name
        cv_result$target <- target
        cv_result
      },
          future.globals = list(
            rate_sequential_cv = rate_sequential_cv,
            model_data_list = model_data_list,
            num_folds = num_folds,
            target = target,
            verbose = FALSE
          ),
          future.packages = c("grf", "cli"),
          future.seed = TRUE
        )
      } else {
        # For sequential, use regular lapply
        lapply(seq_along(model_data_list), function(idx) {
          model_data <- model_data_list[[idx]]
          if (verbose) {
            # Update progress bar
            if (length(model_data_list) > 1) {
              cli::cli_progress_update(id = pb_id)
            }
          }
          
          # Run sequential CV
          cv_result <- rate_sequential_cv(
            X = model_data$X,
            Y = model_data$Y,
            W = model_data$W,
            weights = model_data$weights,
            num_folds = num_folds,
            target = target,
            grf_defaults = model_data$grf_defaults,
            seed = model_data$seed,
            verbose = FALSE,
            model_name = model_data$model_name,
            ...
          )
          
          cv_result$model_name <- model_data$model_name
          cv_result$target <- target
          cv_result
        })
      }
    }, error = function(e) {
      # Close progress bar on error
      if (verbose && !parallel && length(model_data_list) > 1) {
        cli::cli_progress_done(id = pb_id)
      }
      if (grepl("globals.*exceeds", e$message)) {
        stop(paste0(
          "Memory limit exceeded for parallel processing. ",
          "Try increasing future_globals_maxSize (current: ", future_globals_maxSize, " GiB) ",
          "or set parallel = FALSE. Original error: ", e$message
        ))
      } else {
        stop(e)
      }
    })
    
    # Close progress bar or show timing for parallel
    if (verbose) {
      if (!parallel && length(model_data_list) > 1) {
        cli::cli_progress_done(id = pb_id)
      } else if (parallel && !is.null(start_time)) {
        # Show elapsed time for parallel processing
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cli::cli_alert_success("Completed {target} testing in {round(elapsed, 1)} seconds")
      }
    }
  }
  
  # Results should already be filtered, but double-check
  cv_list <- cv_list[!sapply(cv_list, is.null)]
  
  # Compile results
  raw_p_values <- sapply(cv_list, `[[`, "p_value")
  
  # Apply multiple testing adjustment (only bonferroni or none for CV)
  adjusted_p_values <- if (adjust == "none") {
    raw_p_values
  } else if (adjust == "bonferroni") {
    pmin(raw_p_values * length(raw_p_values), 1)  # bonferroni correction
  } else {
    stop("Invalid adjust parameter")
  }
  
  cv_results <- tibble::tibble(
    model = sapply(cv_list, `[[`, "model_name"),
    target = sapply(cv_list, `[[`, "target"),
    p_value_raw = raw_p_values,
    p_value = adjusted_p_values,
    t_statistic = sapply(cv_list, `[[`, "t_statistic"),
    significant = adjusted_p_values < alpha
  )
  
  # apply label mapping if provided
  if (!is.null(label_mapping)) {
    cv_results <- cv_results %>%
      dplyr::mutate(
        model_label = dplyr::case_when(
          model %in% names(label_mapping) ~ label_mapping[model],
          paste0("model_", model) %in% names(label_mapping) ~ label_mapping[paste0("model_", model)],
          gsub("^model_", "", model) %in% names(label_mapping) ~ label_mapping[gsub("^model_", "", model)],
          TRUE ~ model
        )
      )
  } else {
    cv_results$model_label <- cv_results$model
  }
  
  # Identify significant models (using adjusted p-values)
  if (test_both) {
    # for combined results, a model is significant if either AUTOC or QINI is significant
    significant_models <- unique(cv_results$model[cv_results$significant])
  } else {
    significant_models <- cv_results$model[cv_results$significant]
  }
  
  # Create interpretation
  interpretation <- create_cv_interpretation(
    cv_results = cv_results,
    alpha = alpha,
    adjust = adjust,
    num_folds = num_folds,
    target = target
  )
  
  # Return results
  structure(
    list(
      cv_results = cv_results,
      interpretation = interpretation,
      significant_models = significant_models,
      method_details = list(
        num_folds = num_folds,
        target = if (test_both) c("AUTOC", "QINI") else target,
        alpha = alpha,
        adjust = adjust,
        use_full_data = use_full_data,
        seed = seed,
        test_both = test_both
      ),
      fold_details = cv_list
    ),
    class = "margot_cv_results"
  )
}


#' Sequential Cross-Validation for RATE
#'
#' Internal function implementing the uncorrelated sequential cross-validation
#' approach from Nie & Wager (2021). This function is exported for technical
#' reasons (parallel processing) but should not be called directly by users.
#'
#' @export
#' @keywords internal
#' @importFrom grf causal_forest get_scores rank_average_treatment_effect.fit
#' @importFrom stats predict
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
rate_sequential_cv <- function(X, Y, W, weights = NULL, 
                              num_folds = 5, 
                              target = "AUTOC",
                              grf_defaults = list(),
                              seed = 12345,
                              verbose = FALSE,
                              model_name = NULL,
                              ...) {
  set.seed(seed)
  n <- nrow(X)
  
  # Create fold assignments
  fold_id <- sample(rep(1:num_folds, length.out = n))
  samples_by_fold <- split(seq_len(n), fold_id)
  
  # Fit nuisance forest on full data for doubly robust scores
  nuisance_forest <- do.call(grf::causal_forest,
                            c(list(X = X, Y = Y, W = W, 
                                  sample.weights = weights),
                              grf_defaults,
                              list(...)))
  DR_scores <- grf::get_scores(nuisance_forest)
  
  # Initialize storage for t-statistics
  t_statistics <- numeric(num_folds - 1)
  fold_sizes <- numeric(num_folds - 1)
  
  # Progress tracking for sequential CV
  if (verbose && num_folds > 3) {
    model_str <- if (!is.null(model_name)) paste0(" for ", model_name) else ""
    pb_cv_id <- cli::cli_progress_bar(
      format = "CV folds{model_str}: {k-1}/{num_folds-1} [{cli::pb_percent}] {cli::pb_bar}",
      total = num_folds - 1,
      clear = TRUE
    )
  }
  
  # Sequential cross-validation
  for (k in 2:num_folds) {
    # Update progress
    if (verbose && num_folds > 3) {
      cli::cli_progress_update(id = pb_cv_id)
    }
    train_idx <- unlist(samples_by_fold[1:(k-1)])
    test_idx <- samples_by_fold[[k]]
    
    # Train CATE forest on cumulative training data
    cate_forest <- do.call(grf::causal_forest,
                          c(list(X = X[train_idx, , drop = FALSE], 
                                Y = Y[train_idx, drop = FALSE], 
                                W = W[train_idx],
                                sample.weights = if (!is.null(weights)) weights[train_idx] else NULL),
                            grf_defaults,
                            list(...)))
    
    # Predict on test fold
    cate_hat_test <- predict(cate_forest, 
                            X[test_idx, , drop = FALSE])$predictions
    
    # Compute RATE on test fold using DR scores
    rate_fold <- grf::rank_average_treatment_effect.fit(
      DR.scores = DR_scores[test_idx],
      priorities = cate_hat_test,
      target = target,
      R = 200  # Number of bootstrap replicates
    )
    
    t_statistics[k-1] <- rate_fold$estimate / rate_fold$std.err
    fold_sizes[k-1] <- length(test_idx)
  }
  
  # Close progress bar
  if (verbose && num_folds > 3) {
    cli::cli_progress_done(id = pb_cv_id)
  }
  
  # Aggregate t-statistics using martingale property
  aggregated_t <- sum(t_statistics) / sqrt(num_folds - 1)
  p_value <- 2 * pnorm(-abs(aggregated_t))
  
  list(
    p_value = p_value,
    t_statistic = aggregated_t,
    fold_t_statistics = t_statistics,
    fold_sizes = fold_sizes,
    num_folds = num_folds,
    target = target
  )
}


#' Create Interpretation for CV Results
#'
#' @keywords internal
create_cv_interpretation <- function(cv_results, alpha, adjust, num_folds, target) {
  # check if we have both targets
  test_both <- length(unique(cv_results$target)) > 1
  
  if (test_both) {
    # count unique models (each model tested with both targets)
    n_models <- length(unique(cv_results$model))
    n_tests <- nrow(cv_results)
    
    # get significant models by target
    sig_autoc <- unique(cv_results$model[cv_results$significant & cv_results$target == "AUTOC"])
    sig_qini <- unique(cv_results$model[cv_results$significant & cv_results$target == "QINI"])
    sig_both <- intersect(sig_autoc, sig_qini)
    sig_either <- union(sig_autoc, sig_qini)
    n_significant <- length(sig_either)
  } else {
    n_models <- nrow(cv_results)
    n_tests <- n_models
    n_significant <- sum(cv_results$significant)
    target_str <- target
  }
  
  # Format adjustment method name
  adjust_name <- switch(adjust,
    "bonferroni" = "Bonferroni",
    "none" = "no adjustment",
    adjust  # default to the provided name
  )
  
  interpretation <- paste0(
    "# Cross-Validation Heterogeneity Test Results\n\n",
    sprintf("Tested %d models using %d-fold sequential cross-validation.\n", 
            n_models, num_folds)
  )
  
  if (test_both) {
    interpretation <- paste0(
      interpretation,
      "Target metrics: AUTOC and QINI (both tested)\n",
      sprintf("Total tests: %d (each model tested with both metrics)\n", n_tests)
    )
  } else {
    interpretation <- paste0(
      interpretation,
      sprintf("Target metric: %s\n", target)
    )
  }
  
  if (adjust == "bonferroni") {
    interpretation <- paste0(
      interpretation,
      sprintf("Significance level: α = %.3f\n", alpha),
      sprintf("Multiple testing correction: %s\n\n", adjust_name)
    )
  } else {
    interpretation <- paste0(
      interpretation,
      sprintf("Multiple testing correction: %s\n\n", adjust_name)
    )
  }
  
  if (n_significant > 0) {
    interpretation <- paste0(
      interpretation,
      sprintf("## Significant Heterogeneity Found\n\n")
    )
    
    if (test_both) {
      interpretation <- paste0(
        interpretation,
        sprintf("%d out of %d models show statistically significant treatment effect heterogeneity:\n\n", 
                n_significant, n_models)
      )
      
      # show breakdown by target
      if (length(sig_both) > 0) {
        interpretation <- paste0(
          interpretation,
          sprintf("### Models significant for both AUTOC and QINI (%d):\n", length(sig_both))
        )
        for (m in sig_both) {
          # get results for both targets
          autoc_row <- cv_results[cv_results$model == m & cv_results$target == "AUTOC", ]
          qini_row <- cv_results[cv_results$model == m & cv_results$target == "QINI", ]
          model_label <- if ("model_label" %in% names(cv_results)) {
            unique(cv_results$model_label[cv_results$model == m])[1]
          } else {
            gsub("^model_", "", m)
          }
          # check if either shows negative heterogeneity
          if (autoc_row$t_statistic[1] < 0 || qini_row$t_statistic[1] < 0) {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: AUTOC p = %.4f (t = %.3f), QINI p = %.4f (t = %.3f) ⚠️ **CAUTION: Shows negative heterogeneity**\n", 
                      model_label, 
                      autoc_row$p_value[1], autoc_row$t_statistic[1],
                      qini_row$p_value[1], qini_row$t_statistic[1])
            )
          } else {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: AUTOC p = %.4f (t = %.3f), QINI p = %.4f (t = %.3f)\n", 
                      model_label, 
                      autoc_row$p_value[1], autoc_row$t_statistic[1],
                      qini_row$p_value[1], qini_row$t_statistic[1])
            )
          }
        }
        interpretation <- paste0(interpretation, "\n")
      }
      
      # AUTOC only
      autoc_only <- setdiff(sig_autoc, sig_qini)
      if (length(autoc_only) > 0) {
        interpretation <- paste0(
          interpretation,
          sprintf("### Models significant for AUTOC only (%d):\n", length(autoc_only))
        )
        for (m in autoc_only) {
          row <- cv_results[cv_results$model == m & cv_results$target == "AUTOC", ]
          model_label <- if ("model_label" %in% names(cv_results)) {
            unique(cv_results$model_label[cv_results$model == m])[1]
          } else {
            gsub("^model_", "", m)
          }
          # Check for negative t-statistic
          if (row$t_statistic[1] < 0) {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: p = %.4f (t = %.3f) ⚠️ **CAUTION: Shows negative heterogeneity**\n", 
                      model_label, row$p_value[1], row$t_statistic[1])
            )
          } else {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: p = %.4f (t = %.3f)\n", 
                      model_label, row$p_value[1], row$t_statistic[1])
            )
          }
        }
        interpretation <- paste0(interpretation, "\n")
      }
      
      # QINI only
      qini_only <- setdiff(sig_qini, sig_autoc)
      if (length(qini_only) > 0) {
        interpretation <- paste0(
          interpretation,
          sprintf("### Models significant for QINI only (%d):\n", length(qini_only))
        )
        for (m in qini_only) {
          row <- cv_results[cv_results$model == m & cv_results$target == "QINI", ]
          model_label <- if ("model_label" %in% names(cv_results)) {
            unique(cv_results$model_label[cv_results$model == m])[1]
          } else {
            gsub("^model_", "", m)
          }
          # Check for negative t-statistic
          if (row$t_statistic[1] < 0) {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: p = %.4f (t = %.3f) ⚠️ **CAUTION: Shows negative heterogeneity**\n", 
                      model_label, row$p_value[1], row$t_statistic[1])
            )
          } else {
            interpretation <- paste0(
              interpretation,
              sprintf("- **%s**: p = %.4f (t = %.3f)\n", 
                      model_label, row$p_value[1], row$t_statistic[1])
            )
          }
        }
        interpretation <- paste0(interpretation, "\n")
      }
    } else {
      # single target case
      interpretation <- paste0(
        interpretation,
        sprintf("%d out of %d models show statistically significant treatment effect heterogeneity:\n\n", 
                n_significant, n_models)
      )
      
      # List significant models
      sig_models <- cv_results[cv_results$significant, ]
      for (i in 1:nrow(sig_models)) {
        model_label <- if ("model_label" %in% names(sig_models)) {
          sig_models$model_label[i]
        } else {
          gsub("^model_", "", sig_models$model[i])
        }
        # Check for negative t-statistic
        if (sig_models$t_statistic[i] < 0) {
          interpretation <- paste0(
            interpretation,
            sprintf("- **%s**: p = %.4f (t = %.3f) ⚠️ **CAUTION: Shows negative heterogeneity**\n", 
                    model_label, sig_models$p_value[i], sig_models$t_statistic[i])
          )
        } else {
          interpretation <- paste0(
            interpretation,
            sprintf("- **%s**: p = %.4f (t = %.3f)\n", 
                    model_label, sig_models$p_value[i], sig_models$t_statistic[i])
          )
        }
      }
    }
    
    interpretation <- paste0(
      interpretation,
      "\n## Recommendation\n\n",
      "These models show robust evidence of treatment effect heterogeneity and are ",
      "good candidates for personalized treatment strategies.\n"
    )
    
  } else {
    interpretation <- paste0(
      interpretation,
      "## No Significant Heterogeneity Found\n\n",
      "None of the tested models show statistically significant treatment effect heterogeneity.\n\n",
      "## Recommendation\n\n",
      "The evidence suggests that treatment effects are relatively homogeneous across individuals. ",
      "A uniform treatment policy may be appropriate.\n"
    )
  }
  
  # Add technical note
  interpretation <- paste0(
    interpretation,
    "\n## Technical Note\n\n",
    "P-values are computed using the sequential cross-validation method ",
    "(Wager 2024; Nie & Wager 2021), which provides valid inference by ",
    "ensuring independence across folds through sequential training.\n"
  )
  
  interpretation
}


#' Convert CV Results to Rate Results Format
#'
#' Internal function to convert CV results to the format expected by
#' margot_interpret_heterogeneity.
#'
#' @keywords internal
convert_cv_to_rate_results <- function(cv_results, flipped_outcomes = NULL) {
  
  # extract model names that passed CV test
  significant_models <- cv_results$significant_models
  
  # format model names - ensure they have "model_" prefix
  significant_models <- ifelse(
    grepl("^model_", significant_models),
    significant_models,
    paste0("model_", significant_models)
  )
  
  # all tested models
  all_models <- ifelse(
    grepl("^model_", cv_results$cv_results$model),
    cv_results$cv_results$model,
    paste0("model_", cv_results$cv_results$model)
  )
  
  # models that failed the test (negative evidence)
  excluded_models <- setdiff(all_models, significant_models)
  
  # models with inconclusive results (p-value > alpha but not negative)
  # for CV, we treat non-significant as inconclusive rather than negative
  inconclusive_models <- all_models[!cv_results$cv_results$significant]
  
  # create rate_results structure matching margot_interpret_rate output
  list(
    # CV uses the same target for both, so duplicate for consistency
    autoc_model_names = significant_models,
    qini_model_names = significant_models,
    
    # excluded models - those with clearly negative evidence
    excluded_autoc = character(0),  # CV doesn't produce negative results per se
    excluded_qini = character(0),
    excluded_either = character(0),  # No models excluded for negative evidence in CV
    
    # inconclusive models 
    not_excluded_autoc_model_names = inconclusive_models,
    not_excluded_qini_model_names = inconclusive_models,
    
    # flipped outcomes handling
    autoc_flipped = if (!is.null(flipped_outcomes)) {
      intersect(significant_models, paste0("model_", flipped_outcomes))
    } else character(0),
    
    qini_flipped = if (!is.null(flipped_outcomes)) {
      intersect(significant_models, paste0("model_", flipped_outcomes))
    } else character(0),
    
    # summary counts
    n_positive_autoc = length(significant_models),
    n_positive_qini = length(significant_models),
    n_tested = length(all_models),
    
    # method info
    method = "cross_validation",
    num_folds = cv_results$method_details$num_folds,
    target = cv_results$method_details$target,
    alpha = cv_results$method_details$alpha,
    adjust = cv_results$method_details$adjust,
    interpretation = cv_results$interpretation
  )
}


#' Print method for margot_cv_results
#'
#' @param x A margot_cv_results object
#' @param ... Additional arguments (not used)
#' @export
#' @keywords internal
print.margot_cv_results <- function(x, ...) {
  cat(x$interpretation)
  invisible(x)
}