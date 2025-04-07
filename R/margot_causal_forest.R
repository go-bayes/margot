# Ensure necessary packages are loaded (or use explicit :: syntax)
# library(grf)
# library(policytree)
# library(cli)
# library(crayon)
# library(purrr)


# --- Helper Functions (ensure these are defined before main function) ---

# Helper for null or default operator
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# Helper function for clipping scores
clip_scores <- function(score, clip_val) {
  if (!is.null(clip_val) && clip_val > 0 && clip_val < 0.5) {
    return(pmax(clip_val, pmin(score, 1 - clip_val)))
  } else {
    return(score)
  }
}

#' Extract Qini Data for Binary Treatment Plotting
#' (Modified slightly to handle potential non-finite max_gain more gracefully)
#' @keywords internal
extract_qini_data_binary <- function(qini_obj, name, max_index, verbose = TRUE) { # Added verbose pass-through
  if (name == "ate") {
    # Ensure max_gain is finite, default to 0 if not (e.g., if all gains were -Inf)
    raw_gain_path <- qini_obj[["_path"]]$gain
    finite_gains <- raw_gain_path[is.finite(raw_gain_path)]
    if (length(finite_gains) == 0) {
      if(verbose) cli::cli_alert_warning("ATE curve has non-finite gain path, setting max_gain to 0.")
      max_gain <- 0
    } else {
      max_gain <- max(finite_gains, na.rm = TRUE)
    }

    proportion <- seq(0, 1, length.out = max_index)
    gain <- proportion * max_gain
  } else if (name == "cate") {
    gain <- qini_obj[["_path"]]$gain
    # Replace non-finite with NA for padding/truncation
    gain[!is.finite(gain)] <- NA
    gain_length <- length(gain)

    if (gain_length < max_index) {
      # Pad with the last *finite* value or NA if none exist
      last_finite_val <- tail(stats::na.omit(gain), 1) # Use stats::na.omit explicitly
      if(length(last_finite_val) == 0) last_finite_val <- NA # Pad with NA if no finite values
      gain <- c(gain, rep(last_finite_val, max_index - gain_length))
    } else if (gain_length > max_index) {
      gain <- gain[1:max_index]
    }
    proportion <- seq_len(max_index) / max_index
  } else {
    if (verbose) cli::cli_alert_warning(paste("Unknown curve type:", name))
    return(NULL)
  }
  # Ensure output columns exist even if gain is NA/Inf
  out_df <- data.frame(
    proportion = proportion,
    gain = as.numeric(gain), # Ensure numeric type
    curve = name
  )
  return(out_df)
}


#' Compute Qini Curves for Binary Treatments
#' (Revised version with propensity score input and checks)
#' @keywords internal
compute_qini_curves_binary <- function(tau_hat, Y, W,
                                       propensity_scores = NULL, # New argument
                                       verbose = TRUE) {
  tryCatch({
    if (verbose) {
      cli::cli_alert_info("--- Entering compute_qini_curves_binary ---")
      cli::cli_alert_info(paste("Input tau_hat length:", length(tau_hat)))
      cli::cli_alert_info(paste("Input Y class:", class(Y)))
      cli::cli_alert_info(paste("Input Y length:", length(Y)))
      cli::cli_alert_info(paste("Input W length:", length(W)))
      if (!is.null(propensity_scores)) {
        cli::cli_alert_info(paste("Using provided propensity scores. Length:", length(propensity_scores)))
        if(length(propensity_scores) != length(Y)) stop("Length of provided propensity_scores does not match length of Y.")
        print(summary(propensity_scores)) # Good to see summary of scores used
      } else {
        cli::cli_alert_info("Propensity scores not provided, will be estimated internally by get_ipw_scores.")
      }
    }

    tau_hat <- as.vector(tau_hat)
    # Ensure Y is a vector
    Y <- as.vector(Y)
    treatment <- as.factor(W)
    cost <- 1 # Assuming unit cost

    # Calculate IPW scores using potentially provided propensity scores
    IPW_scores <- maq::get_ipw_scores(Y, treatment, propensity.score = propensity_scores)

    # Check IPW_scores *after* calculation
    if (any(!is.finite(IPW_scores))) {
      cli::cli_alert_warning("Non-finite values detected in IPW_scores AFTER calculation. Check inputs/clipping.")
      # Decide how to handle: return NULL? Stop? For now, issue warning and return NULL
      return(NULL)
    } else {
      if(verbose) cli::cli_alert_info("IPW scores calculated successfully.")
    }

    # Compute CATE Qini
    cate_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)

    # Compute ATE Qini
    current_mean_tau <- mean(tau_hat, na.rm = TRUE)
    if (!is.finite(current_mean_tau)) {
      cli::cli_alert_warning("Mean tau_hat is not finite! Skipping ATE Qini.")
      ate_qini <- NULL # Cannot compute if mean is non-finite
    } else {
      if (verbose) cli::cli_alert_info(paste("Mean tau_hat for ATE maq:", current_mean_tau))
      ate_qini <- maq::maq(rep(current_mean_tau, length(tau_hat)), cost, IPW_scores, R = 200)
    }

    qini_objects <- list(cate = cate_qini, ate = ate_qini)
    # Remove NULL objects if ATE calculation failed
    qini_objects <- qini_objects[!sapply(qini_objects, is.null)]

    if (length(qini_objects) == 0) {
      if(verbose) cli::cli_alert_warning("No valid Qini objects could be computed.")
      return(NULL)
    }

    # --- Extract data ---
    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain))
        return(0)
      # Handle potential -Inf in gain path before taking length/max
      finite_gain <- qini_obj[["_path"]]$gain[is.finite(qini_obj[["_path"]]$gain)]
      if (length(finite_gain) == 0) return(0)
      length(qini_obj[["_path"]]$gain) # Use original length for indexing
    }))

    if (max_index == 0) {
      if (verbose) cli::cli_alert_warning("All qini objects have empty or non-finite gain paths. Returning NULL.")
      return(NULL)
    }

    # Use tryCatch around map2_dfr as an extra precaution
    qini_data <- tryCatch({
      purrr::map2_dfr(qini_objects, names(qini_objects),
                      ~ extract_qini_data_binary(.x, .y, max_index, verbose = verbose))
    }, error = function(e) {
      if(verbose) cli::cli_alert_warning(paste("Error during Qini data extraction:", e$message))
      NULL
    })


    if (is.null(qini_data) || nrow(qini_data) == 0) {
      if (verbose) cli::cli_alert_warning("Extracted qini data is empty or extraction failed. Returning NULL.")
      return(NULL)
    }

    if (verbose) cli::cli_alert_info("--- Exiting compute_qini_curves_binary successfully ---")
    return(list(qini_data = qini_data, qini_objects = qini_objects))

  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning(paste("--- Error captured in compute_qini_curves_binary:", e$message, "---"))
      # Consider adding traceback() here if errors persist
      # traceback()
    }
    return(NULL)
  })
}


# --- Main Function ---

#' Run Multiple Generalized Random Forest (GRF) Causal Forest Models
#' (Revised Version with Robustness Fixes)
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modelled.
#' @param covariates A matrix of covariates to be used in the GRF models.
#' @param W A vector of binary treatment assignments.
#' @param weights Optional vector of weights for the observations.
#' @param grf_defaults A list of default parameters for the GRF models.
#' @param save_data Logical indicating whether to save data, covariates, and weights. Default is FALSE.
#' @param compute_rate Logical indicating whether to compute RATE for each model. Default is TRUE.
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 15.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is TRUE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for
#'   training policy trees. Default is 0.7.
#' @param qini_split Logical indicating whether to do a separate train/test split exclusively for the Qini
#'   calculation. Default is TRUE.
#' @param qini_test_prop Proportion of data to use for the Qini test set (if \code{qini_split=TRUE}). Default is 0.5.
#' @param propensity_clip Numeric value for clipping propensity scores (e.g., 0.01).
#'  Propensity scores p will be clipped to [propensity_clip, 1 - propensity_clip].
#'  Set to NULL or 0 to disable clipping. Default is 0.01.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A list containing model results, a combined table (potentially NULL if errors occurred), and other relevant information.
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect variable_importance best_linear_projection probability_forest
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom crayon bold green red yellow
#' @importFrom purrr map2_dfr
#' @importFrom stats na.omit # Added for explicit na.omit call
#' @export
margot_causal_forest <- function(data, outcome_vars, covariates, W, weights = NULL, # Added default NULL for weights
                                 grf_defaults = list(),
                                 save_data = FALSE,
                                 compute_rate = TRUE,
                                 top_n_vars = 15,
                                 save_models = TRUE,
                                 train_proportion = 0.7,
                                 qini_split = TRUE,
                                 qini_test_prop = 0.5,
                                 propensity_clip = 0.01, # Default clipping
                                 verbose = TRUE) {

  # --- basic dimension checks ---
  n_rows <- nrow(covariates)
  if (verbose) cli::cli_alert_info(paste("Number of rows in covariates:", n_rows))

  if (length(W) != n_rows) {
    stop("Length of W does not match number of rows in covariates")
  }
  if (!is.null(weights)) {
    if (length(weights) != n_rows) {
      stop("Length of weights does not match number of rows in covariates")
    }
  }
  # Check outcomes exist and match length AFTER handling missing covariates
  # Moved this check after 'not_missing' is defined

  if (qini_split) {
    if (qini_test_prop <= 0 || qini_test_prop >= 1) {
      stop("qini_test_prop must be between 0 and 1 (exclusive) when qini_split is TRUE")
    }
  }

  if (verbose) cli::cli_alert_info("Starting margot_causal_forest function")

  # --- identify complete cases in COVARIATES ---
  not_missing_covariates <- which(complete.cases(covariates))
  if (length(not_missing_covariates) == 0) stop("No complete cases in covariates")
  if (verbose) cli::cli_alert_info(paste("Number of covariate complete cases:", length(not_missing_covariates)))

  # --- Check outcome lengths and identify overall complete cases ---
  complete_case_indices <- not_missing_covariates
  for (outcome in outcome_vars) {
    if(!outcome %in% names(data)) stop("Outcome variable '", outcome, "' not found in data frame.")
    outcome_vec <- data[[outcome]]
    if (length(outcome_vec) != n_rows) {
      stop("Length of outcome ", outcome, " does not match number of rows in covariates")
    }
    # Further restrict to cases where outcome is not NA
    complete_case_indices <- complete_case_indices[which(!is.na(outcome_vec[complete_case_indices]))]
  }
  # Also check W and weights (if provided) for missingness among remaining cases
  complete_case_indices <- complete_case_indices[which(!is.na(W[complete_case_indices]))]
  if(!is.null(weights)) {
    complete_case_indices <- complete_case_indices[which(!is.na(weights[complete_case_indices]))]
  }

  if (length(complete_case_indices) == 0) stop("No observations with complete covariates, outcomes, W, and weights.")
  if (verbose) cli::cli_alert_info(paste("Number of overall complete cases for analysis:", length(complete_case_indices)))


  results <- list()
  full_models <- list()

  # --- main loop ---
  if (verbose) cli::cli_alert_info("Running models for each outcome variable")
  pb <- cli::cli_progress_bar(total = length(outcome_vars),
                              format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (outcome in outcome_vars) {
    model_name <- paste0("model_", outcome)
    # Use only complete cases for this specific outcome (already filtered above)
    current_complete_indices <- complete_case_indices # Start with overall complete cases
    # Check for NA in this specific outcome again (should be redundant but safe)
    current_complete_indices <- current_complete_indices[which(!is.na(data[[outcome]][current_complete_indices]))]

    if (length(current_complete_indices) < 10) { # Add a minimum size check
      if(verbose) cli::cli_alert_warning(paste("Skipping", outcome, "- fewer than 10 complete cases."))
      results[[model_name]] <- list(error_message = "Skipped: Too few complete cases.")
      cli::cli_progress_update()
      next # Skip to next outcome
    }

    Y_vector <- as.vector(data[[outcome]][current_complete_indices])
    Y_matrix <- as.matrix(data[[outcome]][current_complete_indices, drop=FALSE])
    X_subset <- covariates[current_complete_indices, , drop = FALSE]
    W_subset <- W[current_complete_indices]
    weights_subset <- if (!is.null(weights)) weights[current_complete_indices] else NULL


    # Initialize results element to ensure it exists even if tryCatch fails early
    results[[model_name]] <- list(error_message = NULL, custom_table = NULL) # Ensure custom_table exists

    tryCatch({
      # --- 1) fit the main causal forest using ONLY complete cases for this run ---
      if (verbose) cli::cli_alert_info(paste("Fitting main causal forest for", outcome, "on", length(current_complete_indices), "cases."))
      model <- do.call(grf::causal_forest,
                       c(list(X = X_subset, Y = Y_matrix, W = W_subset, sample.weights = weights_subset),
                         grf_defaults))

      # --- Assign results incrementally ---
      # Use tryCatch around ATE calculation due to potential warnings/errors
      ate_result <- tryCatch({
        grf::average_treatment_effect(model)
      }, warning = function(w) {
        if(verbose) cli::cli_alert_warning(paste("Warning during ATE calculation for", outcome, ":", w$message))
        invokeRestart("muffleWarning") # Continue processing after warning
        grf::average_treatment_effect(model) # Return the value despite warning
      }, error = function(e) {
        if(verbose) cli::cli_alert_danger(paste("Error during ATE calculation for", outcome, ":", e$message))
        NA # Return NA if error occurs
      })
      results[[model_name]]$ate <- round(ate_result, 3)

      results[[model_name]]$test_calibration <- round(grf::test_calibration(model), 3) # Assume this is less problematic

      # Assign custom_table (assuming margot_model_evalue exists and is robust)
      results[[model_name]]$custom_table <- tryCatch({
        margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL)
      }, error = function(e) {
        if(verbose) cli::cli_alert_warning(paste("Failed to generate custom_table for", outcome, ":", e$message))
        NULL # Set to NULL if it fails
      })

      results[[model_name]]$tau_hat <- predict(model)$predictions # Get predictions on the training data (complete_indices)

      # --- RATE ---
      if (compute_rate) {
        results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, results[[model_name]]$tau_hat)
        results[[model_name]]$rate_qini <- grf::rank_average_treatment_effect(model, results[[model_name]]$tau_hat, target = "QINI")
      }

      # --- Policy Tree related ---
      results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)

      # --- Variable Importance & BLP ---
      varimp <- grf::variable_importance(model)
      ranked_vars <- order(varimp, decreasing = TRUE)
      if (is.null(colnames(X_subset))) {
        top_vars_indices <- ranked_vars[1:min(top_n_vars, ncol(X_subset))]
        top_vars_names <- as.character(top_vars_indices) # Use index as name if no colnames
        X_subset_top <- X_subset[, top_vars_indices, drop = FALSE]
      } else {
        top_vars_names <- colnames(X_subset)[ranked_vars[1:min(top_n_vars, ncol(X_subset))]]
        X_subset_top <- X_subset[, top_vars_names, drop = FALSE]
      }
      results[[model_name]]$top_vars <- top_vars_names

      results[[model_name]]$blp_top <- grf::best_linear_projection(
        model,
        X_subset_top, # Use the subsetted covariates with top vars
        target.sample = "all"
      )

      # --- Policy Trees require splitting the *current complete indices* ---
      n_current <- length(current_complete_indices)
      train_size_pt <- floor(train_proportion * n_current)
      if (train_size_pt < 1 || train_size_pt >= n_current) stop("train_proportion results in invalid split for policy tree.")
      train_indices_pt_relative <- sample(n_current, train_size_pt) # Indices relative to current subset
      test_indices_pt_relative <- setdiff(seq_len(n_current), train_indices_pt_relative)

      # Policy Tree Depth 1 (using all current data - check policytree docs if split needed)
      # Assuming policy_tree can be trained on all available DR scores
      results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(
        X_subset, # All current covariates
        results[[model_name]]$dr_scores, # All current DR scores
        depth = 1
      )

      # Policy Tree Depth 2 (using split)
      policy_tree_model_d2 <- policytree::policy_tree(
        X_subset[train_indices_pt_relative, top_vars_names, drop = FALSE], # Train covariates
        results[[model_name]]$dr_scores[train_indices_pt_relative, ], # Train DR scores
        depth = 2
      )
      results[[model_name]]$policy_tree_depth_2 <- policy_tree_model_d2

      # Predictions for plotting policy tree (on test portion of current indices)
      if (length(test_indices_pt_relative) > 0) {
        X_test_pt <- X_subset[test_indices_pt_relative, top_vars_names, drop = FALSE]
        predictions_pt <- predict(policy_tree_model_d2, X_test_pt)
        results[[model_name]]$plot_data <- list(
          X_test = X_test_pt,
          predictions = predictions_pt
        )
      } else {
        results[[model_name]]$plot_data <- NULL # No test data for plot
      }


      # --- 4) compute qini curves ---
      clipped_pscores_qini <- NULL # Initialize

      if (!qini_split) {
        if (verbose) cli::cli_alert_info("Computing Qini in-sample. Estimating and clipping propensity scores.")
        # Use all current complete data for Qini
        X_qini <- X_subset
        W_qini <- W_subset
        tau_hat_qini <- results[[model_name]]$tau_hat # Tau-hat from main model
        Y_qini <- Y_vector # Outcome vector for current complete cases

        p_forest <- grf::probability_forest(X_qini, as.factor(W_qini), num.threads = grf_defaults$num.threads %||% 1)
        pscores_qini <- predict(p_forest, X_qini)$predictions
        clipped_pscores_qini <- clip_scores(pscores_qini, propensity_clip)
        if (verbose) print(summary(clipped_pscores_qini))

        qini_result <- compute_qini_curves_binary(
          tau_hat = tau_hat_qini,
          Y = Y_qini,
          W = W_qini,
          propensity_scores = clipped_pscores_qini,
          verbose = verbose
        )

      } else { # qini_split == TRUE
        if (verbose) cli::cli_alert_info("Performing separate train/test split for Qini evaluation.")
        # Split the *current complete indices* for Qini train/test
        n_qini <- length(current_complete_indices)
        qini_train_size <- floor((1 - qini_test_prop) * n_qini)

        if (qini_train_size < 2 || (n_qini - qini_train_size) < 2) { # Ensure both sets are non-trivial
          stop("qini_test_prop results in empty or too small train/test set for Qini evaluation.")
        }
        qini_train_indices_relative <- sample(n_qini, qini_train_size)
        qini_test_indices_relative <- setdiff(seq_len(n_qini), qini_train_indices_relative)

        # Get actual indices if needed, but relative indices are often sufficient
        # qini_train_idxs_abs <- current_complete_indices[qini_train_indices_relative]
        # qini_test_idxs_abs <- current_complete_indices[qini_test_indices_relative]

        # Subsets for Qini forest training and p-score estimation
        X_train_qini <- X_subset[qini_train_indices_relative, , drop = FALSE]
        Y_train_qini_matrix <- Y_matrix[qini_train_indices_relative, , drop = FALSE]
        W_train_qini <- W_subset[qini_train_indices_relative]
        weights_train_qini <- if(!is.null(weights_subset)) weights_subset[qini_train_indices_relative] else NULL

        # Subsets for Qini evaluation (testing)
        X_test_qini <- X_subset[qini_test_indices_relative, , drop = FALSE]
        Y_test_qini <- Y_vector[qini_test_indices_relative] # Vector outcome for compute_qini
        W_test_qini <- W_subset[qini_test_indices_relative]

        # Fit separate causal forest for CATE estimates on test set
        if (verbose) cli::cli_alert_info(paste("Fitting separate forest for Qini CATE on", length(qini_train_indices_relative) ,"qini-train cases."))
        qini_model <- do.call(grf::causal_forest,
                              c(list(X = X_train_qini,
                                     Y = Y_train_qini_matrix,
                                     W = W_train_qini,
                                     sample.weights = weights_train_qini),
                                grf_defaults))
        qini_tau_hat <- predict(qini_model, newdata = X_test_qini)$predictions

        # Estimate propensity scores on Qini-train, predict on Qini-test
        if (verbose) cli::cli_alert_info("Estimating propensity scores on qini-train, predicting on qini-test.")
        p_forest_qini <- grf::probability_forest(X_train_qini, as.factor(W_train_qini), num.threads = grf_defaults$num.threads %||% 1)
        pscores_qini_test <- predict(p_forest_qini, X_test_qini)$predictions
        clipped_pscores_qini <- clip_scores(pscores_qini_test, propensity_clip)
        if (verbose) print(summary(clipped_pscores_qini))

        # Compute Qini curves on test set using estimates/scores from test set
        if (verbose) cli::cli_alert_info(paste("Computing Qini curves on", length(qini_test_indices_relative), "qini-test cases."))
        qini_result <- compute_qini_curves_binary(
          tau_hat = qini_tau_hat,              # CATE estimates for test set
          Y = Y_test_qini,                     # Outcome for test set
          W = W_test_qini,                     # Treatment for test set
          propensity_scores = clipped_pscores_qini, # Clipped p-scores for test set
          verbose = verbose
        )
      } # End qini_split else block

      # Store Qini results
      if (!is.null(qini_result)) {
        results[[model_name]]$qini_data <- qini_result$qini_data
        results[[model_name]]$qini_objects <- qini_result$qini_objects
      } else {
        if (verbose) cli::cli_alert_warning(crayon::yellow(paste("Unable to compute binary Qini curves for", outcome)))
        results[[model_name]]$qini_data <- NULL # Ensure it's NULL if failed
        results[[model_name]]$qini_objects <- NULL
      }


      # --- 5) save the main model object if requested ---
      if (save_models) {
        full_models[[model_name]] <- model
      }

      # If everything succeeded, clear potential error message
      results[[model_name]]$error_message <- NULL

    }, error = function(e) {
      # Assign error message to pre-initialized list element
      results[[model_name]]$error_message <<- paste("Error processing", outcome, ":", e$message)
      if (verbose) {
        cli::cli_alert_danger(crayon::red(results[[model_name]]$error_message))
        # Simple traceback alternative: show the call stack
        cli::cli_alert_info("Traceback (simplified):")
        print(sys.calls())
        # Alternatively use traceback() but it can be very long
        # traceback()
      }
    }) # End tryCatch

    cli::cli_progress_update()
  } # End for loop over outcomes

  cli::cli_progress_done()

  # --- Combine results and return ---
  if (verbose) cli::cli_alert_info("Combining results...")

  # Filter results to include only those where custom_table was successfully created
  valid_results_for_table <- Filter(function(x) !is.null(x$custom_table) && is.data.frame(x$custom_table), results)

  if (length(valid_results_for_table) > 0) {
    combined_table <- tryCatch({
      # Ensure consistent column names/types before rbind if necessary
      # This assumes margot_model_evalue produces consistent outputs
      bound_list <- lapply(valid_results_for_table, function(x) x$custom_table)
      # Check consistency here if needed
      # e.g., all_names <- unique(unlist(lapply(bound_list, names)))
      # bound_list_aligned <- lapply(bound_list, function(df) { # Add missing cols with NA })
      do.call(rbind, bound_list)
    }, error = function(e) {
      if(verbose) cli::cli_alert_warning(paste("Failed to create combined_table:", e$message))
      NULL # Return NULL if rbind fails
    })

    if(!is.null(combined_table)) {
      # Set rownames based on the names of the valid results used
      rownames(combined_table) <- gsub("model_", "", names(valid_results_for_table))
    }
  } else {
    if(verbose) cli::cli_alert_warning("No valid results found with 'custom_table' to create combined_table.")
    combined_table <- NULL # Set to NULL if no valid results
  }

  if (verbose) cli::cli_alert_success(crayon::green("Model runs loop completed."))

  # *** Define output object reliably ***
  # It will contain results for all attempted outcomes, including errors
  output <- list(
    results = results,
    combined_table = combined_table, # Might be NULL if creation failed
    outcome_vars = outcome_vars,
    complete_case_indices = complete_case_indices # Store the overall complete cases used
  )

  if (save_data) {
    # Save the original full data, covariates, weights if requested
    output$data_input <- data
    output$covariates_input <- covariates
    output$weights_input <- weights
    if (verbose) cli::cli_alert_info("Input data, covariates, and weights saved in output")
  }
  if (save_models) {
    output$full_models <- full_models # Contains models only for successful runs
    if (verbose) cli::cli_alert_info("Full model objects saved in output")
  }

  if (verbose) cli::cli_alert_success("margot_causal_forest function completed successfully \U0001F44D")

  return(output)
}
