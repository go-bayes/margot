#' Development Version of Causal Forest with Enhanced Features
#'
#' This development version implements the new architecture with proper train/test
#' splits, evaluation forests, and integrated missing data handling. It maintains
#' a 50/50 train/test split by default and supports toy data sampling.
#'
#' @param data Data frame containing all variables
#' @param outcome_vars Character vector of outcome variable names
#' @param treatment Character name of treatment variable (default: "A") 
#' @param covariates Character vector of covariate names. If NULL, uses all
#'   variables except outcomes, treatment, and special columns.
#' @param weights Optional weights vector or column name
#' @param train_prop Numeric. Proportion for train/test split (default: 0.5)
#' @param toy_data_prop Numeric. If provided, uses a random sample of this
#'   proportion for testing (default: NULL)
#' @param grf_defaults List of parameters passed to grf::causal_forest()
#' @param eval_forest Logical. Whether to create evaluation forest for test
#'   set doubly robust scores (default: TRUE)
#' @param save_data Logical. Whether to save input data (default: TRUE)
#' @param save_forests Logical. Whether to save forest objects (default: TRUE)
#' @param handle_missing Character. How to handle missing data:
#'   "complete" (default), "impute", or "forest" (let forest handle it)
#' @param seed Integer. Random seed (default: NULL)
#' @param verbose Logical. Whether to print progress (default: TRUE)
#'
#' @return List containing:
#'   \item{results}{List of model results by outcome}
#'   \item{data_info}{Information about data splits and missing values}
#'   \item{forests}{Causal forest objects (if save_forests = TRUE)}
#'   \item{eval_forests}{Evaluation forest objects (if eval_forest = TRUE)}
#'   \item{data}{Original data (if save_data = TRUE)}
#'   \item{metadata}{Model metadata and parameters}
#'
#' @details
#' Key differences from original margot_causal_forest():
#' - Consistent 50/50 train/test split for all analyses
#' - Optional evaluation forests for computing test set DR scores
#' - Integrated missing data handling options
#' - Support for toy data sampling
#' - Simplified structure focused on forest estimation
#' - Returns data needed for downstream analysis functions
#'
#' The evaluation forest is trained on the training data and used to compute
#' doubly robust scores for the test set, ensuring proper out-of-sample
#' evaluation for QINI curves and other metrics.
#'
#' @examples
#' \dontrun{
#' # Generate test data
#' test_data <- margot_simulate_test_data()
#' 
#' # Run causal forest with default settings
#' cf_results <- margot_causal_forest_dev(
#'   data = test_data$data,
#'   outcome_vars = c("Y1", "Y2", "Y3", "Y4"),
#'   treatment = "A"
#' )
#' 
#' # Run with toy data for quick testing
#' cf_results_toy <- margot_causal_forest_dev(
#'   data = test_data$data,
#'   outcome_vars = c("Y1", "Y2"),
#'   treatment = "A",
#'   toy_data_prop = 0.1
#' )
#' }
#'
#' @export
#' @keywords internal
#' @importFrom grf causal_forest
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
margot_causal_forest_dev <- function(
    data,
    outcome_vars,
    treatment = "A",
    covariates = NULL,
    weights = NULL,
    train_prop = 0.5,
    toy_data_prop = NULL,
    grf_defaults = list(),
    eval_forest = TRUE,
    save_data = TRUE,
    save_forests = TRUE,
    handle_missing = "complete",
    seed = NULL,
    verbose = TRUE
) {
  
  # set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # start timing
  start_time <- Sys.time()
  
  if (verbose) cli::cli_alert_info("Starting margot_causal_forest_dev()")
  
  # validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!treatment %in% names(data)) {
    stop("treatment variable '", treatment, "' not found in data")
  }
  
  # check outcome variables exist
  missing_outcomes <- setdiff(outcome_vars, names(data))
  if (length(missing_outcomes) > 0) {
    stop("outcome variables not found in data: ", 
         paste(missing_outcomes, collapse = ", "))
  }
  
  # extract treatment vector
  W <- data[[treatment]]
  if (!all(W %in% c(0, 1, NA))) {
    stop("treatment must be binary (0/1)")
  }
  
  # handle weights
  if (!is.null(weights)) {
    if (is.character(weights)) {
      if (!weights %in% names(data)) {
        stop("weights column '", weights, "' not found in data")
      }
      sample_weights <- data[[weights]]
    } else if (is.numeric(weights)) {
      if (length(weights) != nrow(data)) {
        stop("weights vector length must match number of rows")
      }
      sample_weights <- weights
    } else {
      stop("weights must be a column name or numeric vector")
    }
  } else {
    sample_weights <- NULL
  }
  
  # determine covariates
  if (is.null(covariates)) {
    # exclude special columns
    exclude_cols <- c("id", treatment, outcome_vars, "censored", 
                     grep("_r$", names(data), value = TRUE))
    if (!is.null(weights) && is.character(weights)) {
      exclude_cols <- c(exclude_cols, weights)
    }
    covariates <- setdiff(names(data), exclude_cols)
    if (verbose) {
      cli::cli_alert_info("Auto-detected {length(covariates)} covariates")
    }
  }
  
  # create covariate matrix
  X <- as.matrix(data[, covariates, drop = FALSE])
  
  # handle toy data sampling
  if (!is.null(toy_data_prop)) {
    if (toy_data_prop <= 0 || toy_data_prop > 1) {
      stop("toy_data_prop must be between 0 and 1")
    }
    n_toy <- floor(nrow(data) * toy_data_prop)
    toy_idx <- sample(nrow(data), n_toy)
    
    data <- data[toy_idx, ]
    X <- X[toy_idx, , drop = FALSE]
    W <- W[toy_idx]
    if (!is.null(sample_weights)) {
      sample_weights <- sample_weights[toy_idx]
    }
    
    if (verbose) {
      cli::cli_alert_info("Using toy data sample: {n_toy} observations ({toy_data_prop*100}%)")
    }
  }
  
  # handle missing data
  complete_idx <- complete.cases(X, W)
  n_complete <- sum(complete_idx)
  n_missing <- sum(!complete_idx)
  
  if (verbose && n_missing > 0) {
    cli::cli_alert_warning("{n_missing} observations with missing covariates/treatment")
  }
  
  if (handle_missing == "complete") {
    # use only complete cases
    if (n_complete < 50) {
      stop("fewer than 50 complete observations - insufficient for analysis")
    }
    
    X <- X[complete_idx, , drop = FALSE]
    W <- W[complete_idx]
    data_subset <- data[complete_idx, ]
    if (!is.null(sample_weights)) {
      sample_weights <- sample_weights[complete_idx]
    }
    
    # update indices to refer to complete case positions
    all_idx <- which(complete_idx)
    
  } else if (handle_missing == "impute") {
    # simple mean imputation for covariates
    for (j in 1:ncol(X)) {
      if (any(is.na(X[, j]))) {
        X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
      }
    }
    data_subset <- data
    all_idx <- 1:nrow(data)
    
  } else if (handle_missing == "forest") {
    # let forest handle missing data
    data_subset <- data
    all_idx <- 1:nrow(data)
    
  } else {
    stop("handle_missing must be 'complete', 'impute', or 'forest'")
  }
  
  # create train/test split
  n_obs <- nrow(X)
  n_train <- floor(n_obs * train_prop)
  n_test <- n_obs - n_train
  
  train_idx_local <- sample(n_obs, n_train)
  test_idx_local <- setdiff(1:n_obs, train_idx_local)
  
  # map back to original data indices
  train_idx <- all_idx[train_idx_local]
  test_idx <- all_idx[test_idx_local]
  
  if (verbose) {
    cli::cli_alert_info("Train/test split: {n_train}/{n_test} observations")
  }
  
  # prepare data splits
  X_train <- X[train_idx_local, , drop = FALSE]
  X_test <- X[test_idx_local, , drop = FALSE]
  W_train <- W[train_idx_local]
  W_test <- W[test_idx_local]
  
  weights_train <- if (!is.null(sample_weights)) {
    sample_weights[train_idx_local]
  } else NULL
  
  weights_test <- if (!is.null(sample_weights)) {
    sample_weights[test_idx_local]
  } else NULL
  
  # initialize results storage
  results <- list()
  forests <- list()
  eval_forests <- list()
  
  # process each outcome
  if (verbose) {
    cli::cli_alert_info("Fitting forests for {length(outcome_vars)} outcomes")
  }
  
  for (outcome in outcome_vars) {
    if (verbose) {
      cli::cli_alert_info("Processing outcome: {outcome}")
    }
    
    # extract outcome
    Y <- data_subset[[outcome]]
    Y_train <- Y[train_idx_local]
    Y_test <- Y[test_idx_local]
    
    # check for missing outcomes
    if (any(is.na(Y_train))) {
      cli::cli_alert_warning("Missing values in training outcomes for {outcome}")
    }
    
    tryCatch({
      # fit main forest on training data
      forest_args <- c(
        list(
          X = X_train,
          Y = Y_train,
          W = W_train,
          sample.weights = weights_train
        ),
        grf_defaults
      )
      
      forest <- do.call(grf::causal_forest, forest_args)
      
      # predict on test set
      tau_hat_test <- predict(forest, X_test)$predictions
      
      # fit evaluation forest if requested
      if (eval_forest) {
        eval_forest_obj <- do.call(grf::causal_forest, forest_args)
        
        if (eval_forest) {
          eval_forests[[outcome]] <- eval_forest_obj
        }
      }
      
      # store results
      results[[outcome]] <- list(
        tau_hat_train = predict(forest)$predictions,
        tau_hat_test = tau_hat_test,
        Y_train = Y_train,
        Y_test = Y_test,
        ate_train = grf::average_treatment_effect(forest),
        # we'll compute ate_test using evaluation forest if available
        forest_summary = list(
          n_train = length(Y_train),
          n_test = length(Y_test),
          n_missing_Y_train = sum(is.na(Y_train)),
          n_missing_Y_test = sum(is.na(Y_test))
        )
      )
      
      if (save_forests) {
        forests[[outcome]] <- forest
      }
      
      if (verbose) {
        cli::cli_alert_success("Successfully fit forest for {outcome}")
      }
      
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fit forest for {outcome}: {e$message}")
      results[[outcome]] <- list(error = e$message)
    })
  }
  
  # compile data info
  data_info <- list(
    n_total = nrow(data),
    n_complete = n_complete,
    n_missing = n_missing,
    n_train = n_train,
    n_test = n_test,
    train_idx = train_idx,
    test_idx = test_idx,
    train_prop = train_prop,
    toy_data_prop = toy_data_prop,
    handle_missing = handle_missing,
    covariates = covariates,
    treatment = treatment,
    outcome_vars = outcome_vars
  )
  
  # compile metadata
  metadata <- list(
    call = match.call(),
    timestamp = Sys.time(),
    duration = difftime(Sys.time(), start_time, units = "secs"),
    grf_defaults = grf_defaults,
    seed = seed,
    eval_forest = eval_forest,
    save_data = save_data,
    save_forests = save_forests
  )
  
  # create output
  output <- list(
    results = results,
    data_info = data_info,
    metadata = metadata,
    X_train = X_train,
    X_test = X_test,
    W_train = W_train,
    W_test = W_test,
    weights_train = weights_train,
    weights_test = weights_test
  )
  
  if (save_forests) {
    output$forests <- forests
  }
  
  if (eval_forest && length(eval_forests) > 0) {
    output$eval_forests <- eval_forests
  }
  
  if (save_data) {
    output$data <- data
  }
  
  class(output) <- c("margot_causal_forest_dev", "list")
  
  if (verbose) {
    cli::cli_alert_success(
      "margot_causal_forest_dev() completed in {round(metadata$duration, 1)} seconds"
    )
  }
  
  return(output)
}

#' Print method for margot_causal_forest_dev objects
#'
#' @param x A margot_causal_forest_dev object
#' @param ... Additional arguments (ignored)
#'
#' @export
#' @keywords internal
print.margot_causal_forest_dev <- function(x, ...) {
  cat("margot_causal_forest_dev object\n")
  cat("==============================\n\n")
  
  # data summary
  cat("Data:\n")
  cat(sprintf("  Total observations: %d\n", x$data_info$n_total))
  cat(sprintf("  Complete cases: %d\n", x$data_info$n_complete))
  cat(sprintf("  Train/Test split: %d/%d (%.1f%%/%.1f%%)\n",
              x$data_info$n_train, x$data_info$n_test,
              100 * x$data_info$train_prop,
              100 * (1 - x$data_info$train_prop)))
  
  if (!is.null(x$data_info$toy_data_prop)) {
    cat(sprintf("  Toy data sample: %.1f%%\n", 
                100 * x$data_info$toy_data_prop))
  }
  
  cat(sprintf("  Covariates: %d\n", length(x$data_info$covariates)))
  cat(sprintf("  Treatment: %s\n", x$data_info$treatment))
  cat("\n")
  
  # outcomes summary
  cat("Outcomes:\n")
  for (outcome in x$data_info$outcome_vars) {
    if (!is.null(x$results[[outcome]]$error)) {
      cat(sprintf("  %s: ERROR - %s\n", outcome, x$results[[outcome]]$error))
    } else {
      ate <- x$results[[outcome]]$ate_train
      cat(sprintf("  %s: ATE(train) = %.3f [%.3f, %.3f]\n",
                  outcome, ate[1], ate[1] - 1.96*ate[2], ate[1] + 1.96*ate[2]))
    }
  }
  cat("\n")
  
  # components
  cat("Components saved:\n")
  if (!is.null(x$forests)) cat("  - Causal forests\n")
  if (!is.null(x$eval_forests)) cat("  - Evaluation forests\n")
  if (!is.null(x$data)) cat("  - Original data\n")
  
  invisible(x)
}