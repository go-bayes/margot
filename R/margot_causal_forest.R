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
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 15.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is TRUE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for
#'   training policy trees. Default is 0.7.
#' @param qini_split Logical indicating whether to do a separate train/test split exclusively for the Qini
#'   calculation. Default is TRUE (i.e., Qini is computed out-of-sample).
#' @param qini_train_prop Proportion of data to use for the Qini training set (if \code{qini_split=TRUE}). Default is 0.7.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A list containing model results, a combined table, and other relevant information.
#'
#' @importFrom grf causal_forest average_treatment_effect test_calibration rank_average_treatment_effect variable_importance best_linear_projection
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom crayon bold green red yellow
#'
#' @export
margot_causal_forest <- function(data, outcome_vars, covariates, W, weights,
                                 grf_defaults = list(),
                                 save_data = FALSE,
                                 compute_rate = TRUE,
                                 top_n_vars = 15,
                                 save_models = TRUE,
                                 train_proportion = 0.7,
                                 qini_split = TRUE,
                                 qini_train_prop = 0.7,  # renamed from qini_test_prop to qini_train_prop
                                 verbose = TRUE) {

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
  if (qini_split) {
    if (qini_train_prop <= 0 || qini_train_prop >= 1) {
      stop("qini_train_prop must be between 0 and 1 (exclusive) when qini_split is TRUE")
    }
  }

  if (verbose) cli::cli_alert_info("starting margot_causal_forest function")

  # --- identify complete cases ---
  not_missing <- which(complete.cases(covariates))
  full <- seq_len(nrow(covariates))
  full <- full[full %in% not_missing]
  if (length(not_missing) == 0) stop("no complete cases in covariates")
  if (verbose) cli::cli_alert_info(paste("number of complete cases:", length(not_missing)))

  results <- list()
  full_models <- list()

  if (verbose) cli::cli_alert_info("running models for each outcome variable")
  pb <- cli::cli_progress_bar(total = length(outcome_vars),
                              format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")

  for (outcome in outcome_vars) {
    model_name <- paste0("model_", outcome)
    Y <- as.matrix(data[[outcome]])

    tryCatch({
      # --- 1) fit the main causal forest using all rows ---
      model <- do.call(grf::causal_forest,
                       c(list(X = covariates, Y = Y, W = W, sample.weights = weights), grf_defaults))

      results[[model_name]] <- list(
        ate = round(grf::average_treatment_effect(model), 3),
        test_calibration = round(grf::test_calibration(model), 3),
        custom_table = margot::margot_model_evalue(model, scale = "RD", new_name = outcome, subset = NULL),
        tau_hat = predict(model)$predictions
      )

      tau_hat <- results[[model_name]]$tau_hat

      if (compute_rate) {
        results[[model_name]]$rate_result <- grf::rank_average_treatment_effect(model, tau_hat)
        results[[model_name]]$rate_qini   <- grf::rank_average_treatment_effect(model, tau_hat, target = "QINI")
      }

      # --- 2) compute doubly robust scores and policy trees ---
      results[[model_name]]$dr_scores <- policytree::double_robust_scores(model)
      results[[model_name]]$policy_tree_depth_1 <- policytree::policy_tree(
        covariates[full, , drop = FALSE],
        results[[model_name]]$dr_scores[full, ],
        depth = 1
      )

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

      n_non_missing <- length(not_missing)
      train_size <- floor(train_proportion * n_non_missing)
      if (train_size < 1) stop("train_proportion too low: resulting train_size is less than 1")
      train_indices <- sample(not_missing, train_size)

      policy_tree_model <- policytree::policy_tree(
        covariates[train_indices, top_vars, drop = FALSE],
        results[[model_name]]$dr_scores[train_indices, ],
        depth = 2
      )
      results[[model_name]]$policy_tree_depth_2 <- policy_tree_model

      test_indices <- setdiff(not_missing, train_indices)
      if (length(test_indices) < 1) stop("test set is empty after splitting for policy tree")
      X_test <- covariates[test_indices, top_vars, drop = FALSE]
      predictions <- predict(policy_tree_model, X_test)
      results[[model_name]]$plot_data <- list(
        X_test = X_test,
        predictions = predictions
      )

      # --- 4) compute qini curves ---
      if (!qini_split) {
        if (verbose) cli::cli_alert_info("computing binary qini curves in-sample")
        qini_result <- compute_qini_curves_binary(tau_hat, Y, W, verbose = verbose)
        if (!is.null(qini_result)) {
          results[[model_name]]$qini_data <- qini_result$qini_data
          results[[model_name]]$qini_objects <- qini_result$qini_objects
        } else {
          if (verbose) cli::cli_alert_warning(crayon::yellow(paste("unable to compute binary qini curves for", outcome)))
        }
      } else {
        if (verbose) cli::cli_alert_info("performing separate train/test split for qini evaluation")
        qini_n <- length(not_missing)
        qini_train_size <- floor(qini_train_prop * qini_n)  # modified to use qini_train_prop directly
        if (qini_train_size < 1 || qini_train_size >= qini_n) {
          stop("invalid qini_train_prop: results in empty train or test set for qini evaluation")
        }
        qini_train_idxs <- sample(not_missing, qini_train_size)
        qini_test_idxs  <- setdiff(not_missing, qini_train_idxs)
        if (length(qini_test_idxs) < 1) stop("qini test set is empty")
        if (verbose) cli::cli_alert_info("fitting separate forest for qini on qini-train subset")
        qini_model <- do.call(grf::causal_forest,
                              c(list(X = covariates[qini_train_idxs, , drop = FALSE],
                                     Y = Y[qini_train_idxs],
                                     W = W[qini_train_idxs],
                                     sample.weights = weights[qini_train_idxs]),
                                grf_defaults))
        qini_tau_hat <- predict(qini_model, newdata = covariates[qini_test_idxs, , drop = FALSE])$predictions
        if (verbose) cli::cli_alert_info("computing qini curves on qini-test subset")
        qini_result <- compute_qini_curves_binary(qini_tau_hat, Y[qini_test_idxs], W[qini_test_idxs], verbose = verbose)
        results[[model_name]]$qini_data <- qini_result$qini_data
        results[[model_name]]$qini_objects <- qini_result$qini_objects
      }

      # --- 5) save the main model object if requested ---
      if (save_models) {
        full_models[[model_name]] <- model
      }

    }, error = function(e) {
      if (verbose) cli::cli_alert_danger(crayon::red(paste("error in model for", outcome, ":", e$message)))
    })

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  combined_table <- do.call(rbind, lapply(results, function(x) x$custom_table))
  rownames(combined_table) <- gsub("model_", "", rownames(combined_table))

  if (verbose) cli::cli_alert_success(crayon::green("model runs completed successfully"))

  output <- list(
    results = results,
    combined_table = combined_table,
    outcome_vars = outcome_vars,
    not_missing = not_missing
  )

  if (save_data) {
    output$data <- data
    output$covariates <- covariates
    output$weights <- weights
    if (verbose) cli::cli_alert_info("data, covariates, and weights saved in output")
  }
  if (save_models) {
    output$full_models <- full_models
    if (verbose) cli::cli_alert_info("full model objects saved in output")
  }

  if (verbose) cli::cli_alert_success("margot_causal_forest function completed successfully \U0001F44D")

  return(output)
}

#' Compute Qini Curves for Binary Treatments
#'
#' @description
#' Computes Qini curves for binary treatments using the maq package. This function
#' calculates both Conditional Average Treatment Effect (CATE) and Average Treatment
#' Effect (ATE) Qini curves.
#'
#' @param tau_hat Numeric vector of estimated treatment effects.
#' @param Y Vector or matrix of observed outcomes.
#' @param W Vector of treatment assignments (binary).
#' @param verbose Logical; if TRUE, print diagnostic information during execution.
#'
#' @return A list containing two elements:
#'   \item{qini_data}{A data frame containing Qini curve data for plotting.}
#'   \item{qini_objects}{A list of maq objects for CATE and ATE Qini curves.}
#'   Returns NULL if an error occurs or if the resulting Qini data is empty.
#'
#' @importFrom maq get_ipw_scores maq
#' @importFrom purrr map2_dfr
#' @importFrom cli cli_alert_info cli_alert_warning
#'
#' @keywords internal
compute_qini_curves_binary <- function(tau_hat, Y, W, verbose = TRUE) {
  tryCatch({
    if (verbose) {
      cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
      cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
      cli::cli_alert_info(paste("y class:", class(Y)))
      cli::cli_alert_info(paste("y dimensions:", paste(dim(Y), collapse = "x")))
      cli::cli_alert_info(paste("w class:", class(W)))
      cli::cli_alert_info(paste("w length:", length(W)))
    }

    tau_hat <- as.vector(tau_hat)
    treatment <- as.factor(W)
    IPW_scores <- maq::get_ipw_scores(Y, treatment)
    cost <- 1

    if (verbose) {
      cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
      cli::cli_alert_info(paste("cost length:", length(cost)))
      cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))
    }

    cate_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
    ate_qini <- maq::maq(rep(mean(tau_hat), length(tau_hat)), cost, IPW_scores, R = 200)
    qini_objects <- list(cate = cate_qini, ate = ate_qini)

    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain))
        return(0)
      length(qini_obj[["_path"]]$gain)
    }))
    if (max_index == 0) {
      if (verbose) cli::cli_alert_warning("all qini objects have empty gain. returning NULL.")
      return(NULL)
    }

    qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects),
                                 ~ extract_qini_data_binary(.x, .y, max_index))
    if (nrow(qini_data) == 0) {
      if (verbose) cli::cli_alert_warning("extracted qini data is empty. returning NULL.")
      return(NULL)
    }
    return(list(qini_data = qini_data, qini_objects = qini_objects))
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning(paste("error in compute_qini_curves_binary:", e$message))
    return(NULL)
  })
}

#' Extract Qini Data for Binary Treatment Plotting
#'
#' @description
#' Extracts Qini curve data from a Qini object for binary treatments and prepares it for plotting.
#'
#' @param qini_obj A Qini object.
#' @param name Name of the curve type (either "ate" or "cate").
#' @param max_index Maximum index to extend the curve to.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A data frame with extracted Qini data.
#'
#' @keywords internal
extract_qini_data_binary <- function(qini_obj, name, max_index, verbose = TRUE) {
  if (name == "ate") {
    max_gain <- max(qini_obj[["_path"]]$gain, na.rm = TRUE)
    proportion <- seq(0, 1, length.out = max_index)
    gain <- proportion * max_gain
  } else if (name == "cate") {
    gain <- qini_obj[["_path"]]$gain
    gain_length <- length(gain)
    if (gain_length < max_index) {
      gain <- c(gain, rep(tail(gain, 1), max_index - gain_length))
    } else if (gain_length > max_index) {
      gain <- gain[1:max_index]
    }
    proportion <- seq_len(max_index) / max_index
  } else {
    if (verbose) cli::cli_alert_warning(paste("unknown curve type:", name))
    return(NULL)
  }
  data.frame(
    proportion = proportion,
    gain = gain,
    curve = name
  )
}
