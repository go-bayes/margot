#' Run Multiple Generalised Random Forest (GRF) Causal Forest Models in Parallel
#'
#' Parallelised, diagnostic‑rich variant of `margot_causal_forest()`.  Each
#' outcome‑specific forest is estimated in its own R worker via **future**.  All
#' the `cli` messages and checks from the sequential original are preserved, so
#' you still get the same granular reporting (dimension checks, Qini status,
#' warnings, etc.).  Live progress bars are emitted with **progressr** using a
#' `cli` handler.
#'
#' @inheritParams margot_causal_forest
#' @param n_cores integer. number of parallel workers (default = all cores − 1).
#'
#' @return list with elements:
#'   * `results`         – per‑outcome diagnostics and objects
#'   * `combined_table`  – rbind‑ed e‑value table across outcomes
#'   * `outcome_vars`    – vector of (successful) outcome names
#'   * `not_missing`     – indices of complete‑case rows
#'   * (`data`, `covariates`, `weights`, `W`) when `save_data = TRUE`
#'   * `full_models` when `save_models = TRUE`
#'
#' @details Messages produced inside workers are captured by **future** and
#'   dispatched to the master session.  Progress bars update in real time.  To
#'   silence progress, call `progressr::handlers("off")` before running.
#'
#' @importFrom future plan multisession availableCores
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor handlers
#' @importFrom grf causal_forest average_treatment_effect test_calibration
#'   rank_average_treatment_effect variable_importance best_linear_projection
#' @importFrom policytree double_robust_scores policy_tree
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_h3
#'   cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom purrr map walk
#' @name margot_causal_forest_parallel
#' @keywords internal
margot_causal_forest_parallel  <- function(data, outcome_vars, covariates, W, weights,
                                           grf_defaults    = list(),
                                           save_data       = FALSE,
                                           compute_rate    = TRUE,
                                           top_n_vars      = 15,
                                           save_models     = TRUE,
                                           train_proportion= 0.7,
                                           qini_split      = TRUE,
                                           train_prop      = 0.5,
                                           qini_train_prop = NULL,  # deprecated
                                           compute_conditional_means = TRUE,
                                           n_cores         = future::availableCores() - 1,
                                           verbose         = TRUE,
                                           qini_treatment_cost = 1) {
  # dimension checks
  n_rows <- nrow(covariates)
  if (verbose) cli::cli_alert_info(paste0("rows in covariates: ", n_rows))
  if (length(W) != n_rows) stop("length of W does not match covariates rows")
  if (!is.null(weights) && length(weights) != n_rows) stop("length of weights does not match")
  for (o in outcome_vars) {
    if (nrow(as.matrix(data[[o]])) != n_rows)
      stop("rows in outcome ", o, " do not match covariates")
  }
  # handle deprecated qini_train_prop parameter
  if (!is.null(qini_train_prop)) {
    if (verbose) cli::cli_alert_warning("Parameter 'qini_train_prop' is deprecated. Please use 'train_prop' instead.")
    train_prop <- qini_train_prop
  }
  
  if (qini_split && (train_prop <= 0 || train_prop >= 1))
    stop("train_prop must be in (0,1)")
  if (verbose) cli::cli_alert_info("starting margot_causal_forest()")

  # complete cases
  not_missing <- which(complete.cases(covariates))
  if (length(not_missing)==0) stop("no complete cases in covariates")
  if (verbose) cli::cli_alert_info(paste0("complete cases: ", length(not_missing)))

  # parallel setup
  future::plan(future::multisession, workers = n_cores)
  if (verbose) cli::cli_alert_info(paste0("using ", n_cores, " workers"))

  # parallel loop with CLI logging
  results_list <- future.apply::future_lapply(outcome_vars, function(outcome) {
    t0 <- Sys.time()
    cli::cli_alert_info(paste0("↗ starting ", outcome))

    res <- tryCatch({
      # fit forest
      model <- do.call(grf::causal_forest,
                       c(list(X = covariates,
                              Y = as.matrix(data[[outcome]]),
                              W = W,
                              sample.weights = weights),
                         grf_defaults))
      # core metrics
      ate    <- round(grf::average_treatment_effect(model),3)
      calib  <- round(grf::test_calibration(model),3)
      table  <- margot::margot_model_evalue(model, scale="RD",
                                            new_name=outcome, subset=NULL)
      tau    <- predict(model)$predictions

      # RATE
      if (compute_rate) {
        rate     <- grf::rank_average_treatment_effect(model, tau)
        attr(rate, "target") <- "AUTOC"
        rate_qini<- grf::rank_average_treatment_effect(model, tau, target="QINI")
        attr(rate_qini, "target") <- "QINI"
      }
      # conditional means
      cond_means <- NULL
      if (compute_conditional_means) {
        if (requireNamespace("policytree", quietly = TRUE)) {
          tryCatch({
            cond_means <- policytree::conditional_means(model)
            if (verbose) cli::cli_alert_info("computed conditional means for {outcome}")
          }, error = function(e) {
            if (verbose) cli::cli_alert_warning("could not compute conditional means for {outcome}: {e$message}")
          })
        }
      }
      # DR scores + policy tree 1
      dr   <- policytree::double_robust_scores(model)
      pt1  <- policytree::policy_tree(covariates[not_missing,,drop=FALSE],
                                      dr[not_missing,], depth=1)
      # var importance + BLP
      vi     <- grf::variable_importance(model)
      ord    <- order(vi, decreasing=TRUE)
      top    <- if(is.null(colnames(covariates))) ord[1:top_n_vars] else colnames(covariates)[ord[1:top_n_vars]]
      blp    <- grf::best_linear_projection(model, covariates[,top,drop=FALSE], target.sample="all")
      # policy tree 2 + plot data
      train_idx <- sample(not_missing, floor(train_proportion*length(not_missing)))
      pt2    <- policytree::policy_tree(covariates[train_idx,top,drop=FALSE], dr[train_idx,], depth=2)
      test_idx<- setdiff(not_missing, train_idx)
      plotd  <- list(X_test=covariates[test_idx,top,drop=FALSE],
                     X_test_full=covariates[test_idx,,drop=FALSE],
                     predictions=predict(pt2, covariates[test_idx,top,drop=FALSE]))
      # Qini
      if (!qini_split) {
        qres <- compute_qini_curves_binary(tau, as.matrix(data[[outcome]]), W, 
                                          weights = weights, seed = 42, verbose = verbose, treatment_cost = qini_treatment_cost)
      } else {
        qtr <- sample(not_missing, floor(train_prop*length(not_missing)))
        qte <- setdiff(not_missing, qtr)
        qmod<- do.call(grf::causal_forest,
                       c(list(X=covariates[qtr,,drop=FALSE],
                              Y=as.matrix(data[[outcome]])[qtr],
                              W=W[qtr], sample.weights=weights[qtr]),
                         grf_defaults))
        qtau<- predict(qmod, newdata=covariates[qte,,drop=FALSE])$predictions
        qres <- compute_qini_curves_binary(qtau, as.matrix(data[[outcome]])[qte], W[qte], 
                                          weights = if(!is.null(weights)) weights[qte] else NULL, 
                                          seed = 42, verbose = verbose, treatment_cost = qini_treatment_cost)
      }
      qdata   <- if(!is.null(qres)) qres$qini_data else NULL
      qobjs   <- if(!is.null(qres)) qres$qini_objects else NULL

      out <- list(
        ate=ate, test_calibration=calib, custom_table=table, tau_hat=tau,
        conditional_means=cond_means,
        rate_result=if(compute_rate) rate else NULL,
        rate_qini=if(compute_rate) rate_qini else NULL,
        dr_scores=dr, policy_tree_depth_1=pt1,
        top_vars=top, blp_top=blp,
        policy_tree_depth_2=pt2, plot_data=plotd,
        qini_data=qdata, qini_objects=qobjs,
        model=if(save_models) model else NULL
      )
      dt <- difftime(Sys.time(), t0, units="secs")
      cli::cli_alert_success(sprintf("✓ worker finished %s (%.1f s)", outcome, as.numeric(dt)))
      out
    }, error=function(e) {
      cli::cli_alert_danger(paste0("✗ error in ", outcome, ": ", e$message))
      NULL
    })
    res
  })
  names(results_list) <- paste0("model_", outcome_vars)

  # drop failures
  ok <- !vapply(results_list, is.null, logical(1))
  if (!all(ok)) {
    cli::cli_alert_warning(paste(sum(!ok), "outcome(s) failed and were dropped"))
    results_list <- results_list[ok]
    outcome_vars <- outcome_vars[ok]
  }

  # combine
  combined_table <- do.call(rbind, lapply(results_list, `[[`, "custom_table"))
  rownames(combined_table) <- gsub("model_", "", names(results_list))
  if (verbose) cli::cli_alert_success("all model runs completed")

  output <- list(
    results=results_list,
    combined_table=combined_table,
    outcome_vars=outcome_vars,
    not_missing=not_missing
  )
  if (save_data) {
    output$data<-data; output$covariates<-covariates; output$weights<-weights; output$W<-W
    if (verbose) cli::cli_alert_info("raw data/covariates/weights/W saved")
  }
  if (save_models) {
    output$full_models<-purrr::map(results_list, `[[`, "model")
    if (verbose) cli::cli_alert_info("full model objects saved")
  }
  if (verbose) cli::cli_alert_success("margot_causal_forest() completed")
  output
}


# -----------------------------------------------------------------------------
# helper: compute_qini_curves_binary
# -----------------------------------------------------------------------------

#' Compute Qini Curves for Binary Treatments
#' @keywords internal
compute_qini_curves_binary <- function(tau_hat, Y, W, weights = NULL, seed = NULL, verbose = TRUE, treatment_cost = 1) {
  tryCatch({
    if (verbose) cli::cli_alert_info("computing Qini curves …")
    tau_hat    <- as.vector(tau_hat)
    treatment  <- as.factor(W)
    IPW_scores <- maq::get_ipw_scores(Y, treatment)
    
    # use modern maq API with named parameters
    cate_qini  <- maq::maq(
      reward = as.matrix(tau_hat),
      cost = matrix(treatment_cost, length(tau_hat), 1),
      DR.scores = IPW_scores,
      R = 200,
      sample.weights = weights,
      seed = seed
    )
    
    # try maq with target.with.covariates = FALSE first (maq_no_covariates approach)
    ate_qini <- tryCatch({
      maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(treatment_cost, length(tau_hat), 1),
        DR.scores = IPW_scores,
        target.with.covariates = FALSE,
        R = 200,
        sample.weights = weights,
        seed = seed
      )
    }, error = function(e) {
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
    })
    
    qini_objs  <- list(cate = cate_qini, ate = ate_qini)
    max_idx    <- max(sapply(qini_objs, function(q) length(q[["_path"]]$gain)))
    if (!max_idx) return(NULL)
    if (verbose) cli::cli_alert_info("Extracting qini data for curves: {paste(names(qini_objs), collapse = ', ')}")
    qini_dat   <- purrr::map2_dfr(qini_objs, names(qini_objs),
                                  ~ extract_qini_data_binary(.x, .y, max_idx, verbose))
    if (!nrow(qini_dat)) return(NULL)
    list(qini_data = qini_dat, qini_objects = qini_objs)
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning("qini error: {e$message}")
    NULL
  })
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
                                model_names        = NULL,
                                test_prop          = 0.5,
                                propensity_bounds  = c(0.05, 0.95),
                                seed               = 2025) {

  stopifnot(is.list(model_results),
            "full_models" %in% names(model_results),
            "data"        %in% names(model_results))

  # make sure tidyverse is on board
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("policytree", quietly = TRUE)

  if (is.null(model_names))
    model_names <- names(model_results$full_models)
  model_names <- ifelse(grepl("^model_", model_names),
                        model_names,
                        paste0("model_", model_names))

  set.seed(seed)

  purrr::map_dfr(model_names, function(mn) {

    forest <- model_results$full_models[[mn]]
    if (is.null(forest))
      return(NULL)

    outcome_name <- sub("^model_", "", mn)

    Y  <- model_results$data[[outcome_name]]
    W  <- forest$W
    eh <- forest$W.hat
    th <- predict(forest)$predictions               # tau_hat
    dr <- policytree::double_robust_scores(forest)  # n x 2 matrix
    dr <- dr[, 2] - dr[, 1]                         # treated – control

    keep <- which(!is.na(Y) &
                    eh > propensity_bounds[1] &
                    eh < propensity_bounds[2])

    n_trimmed <- length(Y) - length(keep)
    if (length(keep) < 10)
      return(tibble::tibble(model      = mn,
                            outcome    = outcome_name,
                            note       = "too little data after trimming"))

    idx_test <- sample(keep, size = floor(test_prop * length(keep)))
    ord      <- order(-th[idx_test])                # best-to-worst ranking
    gain     <- cumsum(dr[idx_test][ord])

    tibble::tibble(model       = mn,
                   outcome     = outcome_name,
                   sd_tau      = sd(th[idx_test]),
                   min_gain    = min(gain),
                   final_gain  = gain[length(gain)],
                   n_test      = length(idx_test),
                   n_trimmed   = n_trimmed)
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
#' @param top_n_vars Integer specifying the number of top variables to use for additional computations. Default is 15.
#' @param save_models Logical indicating whether to save the full GRF model objects. Default is TRUE.
#' @param train_proportion Numeric value between 0 and 1 indicating the proportion of non-missing data to use for
#'   training policy trees. Default is 0.7.
#' @param qini_split Logical indicating whether to do a separate train/test split exclusively for the Qini
#'   calculation. Default is TRUE (i.e., Qini is computed out-of-sample).
#' @param train_prop Proportion of data to use for the training set when qini_split=TRUE. Default is 0.5.
#' @param qini_train_prop Deprecated. Use train_prop instead. If provided, will override train_prop with a warning.
#' @param compute_conditional_means Logical indicating whether to compute conditional means using 
#'   \code{policytree::conditional_means()}. These represent expected outcomes under each treatment arm. Default is TRUE.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#' @param qini_treatment_cost Scalar treatment cost per unit for QINI calculations. Default 1.
#'   Lower values (e.g., 0.2) represent cheap treatments creating steeper QINI curves;
#'   higher values (e.g., 5) represent expensive treatments creating shallower curves.
#'
#' @return A list containing:
#'   * `results` - per-outcome diagnostics and objects
#'   * `combined_table` - combined e-value table across outcomes
#'   * `outcome_vars` - vector of outcome names
#'   * `not_missing` - indices of complete-case rows
#'   * (`data`, `covariates`, `weights`, `W`) when `save_data = TRUE`
#'   * `full_models` when `save_models = TRUE`
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
                                 train_proportion = 0.7,
                                 qini_split = TRUE,
                                 train_prop = 0.5,
                                 qini_train_prop = NULL,  # deprecated parameter
                                 compute_conditional_means = TRUE,
                                 verbose = TRUE,
                                 qini_treatment_cost = 1) {

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
  # handle deprecated qini_train_prop parameter
  if (!is.null(qini_train_prop)) {
    cli::cli_alert_warning("Parameter 'qini_train_prop' is deprecated. Please use 'train_prop' instead.")
    train_prop <- qini_train_prop
  }
  
  if (qini_split) {
    if (train_prop <= 0 || train_prop >= 1) {
      stop("train_prop must be between 0 and 1 (exclusive) when qini_split is TRUE")
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
        attr(results[[model_name]]$rate_result, "target") <- "AUTOC"
        results[[model_name]]$rate_qini   <- grf::rank_average_treatment_effect(model, tau_hat, target = "QINI")
        attr(results[[model_name]]$rate_qini, "target") <- "QINI"
      }

      # --- compute conditional means if requested ---
      if (compute_conditional_means && save_models) {
        if (requireNamespace("policytree", quietly = TRUE)) {
          tryCatch({
            results[[model_name]]$conditional_means <- policytree::conditional_means(model)
            if (verbose) cli::cli_alert_info("computed conditional means for {outcome}")
          }, error = function(e) {
            if (verbose) cli::cli_alert_warning("could not compute conditional means for {outcome}: {e$message}")
            results[[model_name]]$conditional_means <- NULL
          })
        } else {
          if (verbose) cli::cli_alert_warning("policytree package not available for conditional means")
        }
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

      X_test_full <- covariates[test_indices, , drop = FALSE]

      predictions <- predict(policy_tree_model, X_test)

      results[[model_name]]$plot_data <- list(
        X_test       = X_test,
        X_test_full  = X_test_full,   # <- NEW
        predictions  = predictions
      )

      # --- 4) compute qini curves ---
      if (!qini_split) {
        if (verbose) cli::cli_alert_info("computing binary qini curves in-sample")
        qini_result <- compute_qini_curves_binary(tau_hat, Y, W, weights = weights, seed = 42, verbose = verbose, treatment_cost = qini_treatment_cost)
        if (!is.null(qini_result)) {
          results[[model_name]]$qini_data <- qini_result$qini_data
          results[[model_name]]$qini_objects <- qini_result$qini_objects
          # store metadata about qini generation
          results[[model_name]]$qini_metadata <- list(
            n_test = length(not_missing),
            n_train = 0,  # no separate train set
            test_indices = not_missing,
            qini_split = FALSE,
            baseline_method = "maq_no_covariates"  # compute_qini_curves_binary now uses maq_no_covariates with fallback
          )
        } else {
          if (verbose) cli::cli_alert_warning(crayon::yellow(paste("unable to compute binary qini curves for", outcome)))
        }
      } else {
        if (verbose) cli::cli_alert_info("performing separate train/test split for qini evaluation")
        qini_n <- length(not_missing)
        qini_train_size <- floor(train_prop * qini_n)  # use train_prop
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
        qini_result <- compute_qini_curves_binary(qini_tau_hat, Y[qini_test_idxs], W[qini_test_idxs], 
                                                 weights = if(!is.null(weights)) weights[qini_test_idxs] else NULL,
                                                 seed = 42, verbose = verbose, treatment_cost = qini_treatment_cost)
        results[[model_name]]$qini_data <- qini_result$qini_data
        results[[model_name]]$qini_objects <- qini_result$qini_objects
        # store metadata about qini generation
        results[[model_name]]$qini_metadata <- list(
          n_test = length(qini_test_idxs),
          n_train = length(qini_train_idxs),
          test_indices = qini_test_idxs,
          qini_split = TRUE,
          baseline_method = "maq_no_covariates"  # compute_qini_curves_binary now uses maq_no_covariates with fallback
        )
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
    output$W <- W
    if (verbose) cli::cli_alert_info("data, covariates, weights, and W saved in output")
  }
  if (save_models) {
    output$full_models <- full_models
    if (verbose) cli::cli_alert_info("full model objects saved in output")
  }

  if (verbose) cli::cli_alert_success("margot_causal_forest function completed successfully \U0001F44D")

  return(output)
}
