# the shared-ratio seam is validated against this exact internal lmtp interface
margot_lmtp_shared_lmtp_version <- "1.5.4"

# return one internal lmtp object after enforcing the validated package version
margot_lmtp_internal <- function(name, call = rlang::caller_env()) {
  installed <- as.character(utils::packageVersion("lmtp"))
  if (!identical(installed, margot_lmtp_shared_lmtp_version)) {
    cli::cli_abort(
      c(
        "Density-ratio reuse is not validated for the installed {.pkg lmtp} version.",
        "x" = "Installed {.val {installed}}; validated {.val {margot_lmtp_shared_lmtp_version}}.",
        "i" = "Use the legacy path or validate the shared path against this version before enabling reuse."
      ),
      class = "margot_error_lmtp_conformity",
      call = call
    )
  }

  object <- tryCatch(
    utils::getFromNamespace(name, "lmtp"),
    error = function(e) NULL
  )
  if (is.null(object)) {
    object <- get0(name, envir = asNamespace("lmtp"), inherits = TRUE)
  }
  if (is.null(object)) {
    cli::cli_abort(
      c(
        "The validated {.pkg lmtp} internal interface is unavailable.",
        "x" = "Missing {.field {name}} from {.pkg lmtp} {.val {installed}}."
      ),
      class = "margot_error_lmtp_conformity",
      call = call
    )
  }
  object
}

# build one ordinary lmtp task for an outcome and the common design inputs
margot_lmtp_make_task <- function(data, trt, outcome, baseline, time_vary,
                                  cens, compete, shift, shifted, k, mtp,
                                  outcome_type, id, bounds, folds, weights) {
  variable_names <- c(
    unlist(trt), outcome, unlist(time_vary), baseline, cens, compete, id
  )
  shifted_data <- margot_lmtp_internal("make_shifted")(
    data[, variable_names], trt, cens, shift, shifted
  )
  task_generator <- margot_lmtp_internal("LmtpTask")
  task_generator$new(
    data = data,
    shifted = shifted_data,
    A = trt,
    Y = outcome,
    L = time_vary,
    W = baseline,
    C = cens,
    D = compete,
    k = k,
    id = id,
    outcome_type = outcome_type,
    bounds = bounds,
    folds = folds,
    weights = weights
  )
}

# verify that an outcome task has the same participants and nuisance inputs as the source task
margot_lmtp_assert_shared_task <- function(task, source_task, shared_variables,
                                           outcome, call = rlang::caller_env()) {
  problems <- character()
  if (!identical(as.character(task$id), as.character(source_task$id))) {
    problems <- c(problems, "participant identifiers or row order")
  }
  if (!identical(task$time_horizon, source_task$time_horizon)) {
    problems <- c(problems, "time horizon")
  }
  if (!identical(task$weights, source_task$weights)) {
    problems <- c(problems, "analysis weights")
  }

  shared_variables <- unique(shared_variables[!is.na(shared_variables) & nzchar(shared_variables)])
  shared_variables <- intersect(shared_variables, names(source_task$natural))
  if (length(shared_variables) > 0L) {
    if (!identical(
      as.data.frame(task$natural[, shared_variables, drop = FALSE]),
      as.data.frame(source_task$natural[, shared_variables, drop = FALSE])
    )) {
      problems <- c(problems, "observed nuisance inputs")
    }
    if (!identical(
      as.data.frame(task$shifted[, shared_variables, drop = FALSE]),
      as.data.frame(source_task$shifted[, shared_variables, drop = FALSE])
    )) {
      problems <- c(problems, "policy-shifted nuisance inputs")
    }
  }

  if (length(problems) > 0L) {
    cli::cli_abort(
      c(
        "Outcome {.val {outcome}} cannot reuse the common density-ratio fit.",
        "x" = "Mismatched: {problems}."
      ),
      class = "margot_error_density_ratio_identity",
      call = call
    )
  }
  invisible(task)
}

# fit the policy-specific density ratios once and return the ratios with their learner fits
margot_lmtp_fit_density_ratios <- function(task, learners_trt, mtp, control,
                                           progress_bar) {
  margot_lmtp_internal("cf_density_ratios")(
    task = task,
    learners = learners_trt,
    mtp = mtp,
    control = control,
    pb = progress_bar
  )
}

# fit one outcome regression from common ratios and reconstruct an ordinary lmtp model
margot_lmtp_fit_sdr_outcome <- function(task, density_fit, learners_outcome,
                                        control, progress_bar, shift_label) {
  regressions <- margot_lmtp_internal("cf_sdr")(
    task = task,
    density_ratios = density_fit$density_ratios,
    learners = learners_outcome,
    control = control,
    progress_bar = progress_bar
  )
  margot_lmtp_internal("theta_dr")(
    task = task,
    sequential_regressions = list(
      natural = regressions$natural,
      shifted = regressions$shifted
    ),
    density_ratios = density_fit$density_ratios,
    fits_m = regressions$fits,
    fits_r = density_fit$fits,
    shift = shift_label,
    is_sdr = TRUE
  )
}

# restore an exact RNG state before each outcome-specific stochastic regression
margot_lmtp_restore_seed <- function(seed_state) {
  if (!is.integer(seed_state)) {
    cli::cli_abort(
      "The shared density-ratio stage returned no reusable random-number state.",
      class = "margot_error_lmtp_conformity"
    )
  }
  assign(".Random.seed", seed_state, envir = .GlobalEnv)
  invisible(seed_state)
}

# run one policy-specific ratio fit followed by compatible outcome-specific SDR fits
margot_lmtp_sdr_shared <- function(
    data,
    trt,
    outcomes,
    baseline = NULL,
    time_vary = NULL,
    cens = NULL,
    compete = NULL,
    shift = NULL,
    shifted = NULL,
    k = Inf,
    mtp = TRUE,
    outcome_type = c("binomial", "continuous", "survival"),
    id = NULL,
    bounds = NULL,
    learners_outcome = "SL.glm",
    learners_trt = "SL.glm",
    folds = 10,
    weights = NULL,
    control = lmtp::lmtp_control()) {
  margot_lmtp_internal("LmtpTask")
  if (!is.character(outcomes) || length(outcomes) < 1L || anyNA(outcomes) || any(!nzchar(outcomes))) {
    cli::cli_abort(
      "{.arg outcomes} must contain at least one outcome column name.",
      class = "margot_error_invalid_input"
    )
  }
  if (!is.null(shifted)) {
    cli::cli_abort(
      "Density-ratio reuse does not yet support a precomputed {.arg shifted} data object.",
      class = "margot_error_unsupported_estimator"
    )
  }
  outcome_type <- match.arg(outcome_type)
  if (identical(outcome_type, "survival") || !is.null(compete)) {
    cli::cli_abort(
      "Density-ratio reuse currently supports continuous and binomial SDR outcomes without competing events.",
      class = "margot_error_unsupported_estimator"
    )
  }

  assert_not_data_table <- margot_lmtp_internal("assert_not_data_table")
  assert_outcome_types <- margot_lmtp_internal("assert_outcome_types")
  assert_subset <- margot_lmtp_internal("assert_subset")
  assert_numeric <- margot_lmtp_internal("assert_numeric")
  check_trt_type <- margot_lmtp_internal("check_trt_type")
  assert_not_data_table(data)
  required <- c(unlist(trt), outcomes, unlist(time_vary), baseline, cens, compete, id)
  assert_subset(required, names(data))
  for (outcome in outcomes) {
    assert_outcome_types(data, outcome, outcome_type)
  }
  assert_numeric(
    bounds,
    len = 2,
    unique = TRUE,
    sorted = TRUE,
    finite = TRUE,
    null.ok = TRUE
  )
  check_trt_type(data, unlist(trt), mtp)

  source_task <- margot_lmtp_make_task(
    data = data,
    trt = trt,
    outcome = outcomes[[1L]],
    baseline = baseline,
    time_vary = time_vary,
    cens = cens,
    compete = compete,
    shift = shift,
    shifted = shifted,
    k = k,
    mtp = mtp,
    outcome_type = outcome_type,
    id = id,
    bounds = bounds,
    folds = folds,
    weights = weights
  )
  progress_bar <- progressr::progressor(
    steps = source_task$time_horizon * folds * (length(outcomes) + 1L)
  )
  density_fit <- margot_lmtp_fit_density_ratios(
    task = source_task,
    learners_trt = learners_trt,
    mtp = mtp,
    control = control,
    progress_bar = progress_bar
  )
  post_ratio_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  common_folds <- source_task$folds
  shared_variables <- c(
    unlist(trt), unlist(time_vary), baseline, cens, compete, id, "..i..lmtp_id"
  )
  shift_label <- paste(deparse(shift), collapse = " ")

  models <- lapply(outcomes, function(outcome) {
    task <- if (identical(outcome, outcomes[[1L]])) {
      source_task
    } else {
      margot_lmtp_make_task(
        data = data,
        trt = trt,
        outcome = outcome,
        baseline = baseline,
        time_vary = time_vary,
        cens = cens,
        compete = compete,
        shift = shift,
        shifted = shifted,
        k = k,
        mtp = mtp,
        outcome_type = outcome_type,
        id = id,
        bounds = bounds,
        folds = folds,
        weights = weights
      )
    }
    margot_lmtp_assert_shared_task(
      task = task,
      source_task = source_task,
      shared_variables = shared_variables,
      outcome = outcome
    )
    task$folds <- common_folds
    margot_lmtp_restore_seed(post_ratio_seed)
    model <- margot_lmtp_fit_sdr_outcome(
      task = task,
      density_fit = density_fit,
      learners_outcome = learners_outcome,
      control = control,
      progress_bar = progress_bar,
      shift_label = shift_label
    )
    attr(model, "margot_density_ratio_source") <- outcomes[[1L]]
    attr(model, "margot_density_ratio_reused") <- !identical(outcome, outcomes[[1L]])
    model
  })
  names(models) <- outcomes
  attr(models, "margot_density_ratio_fit_count") <- 1L
  attr(models, "margot_lmtp_version") <- margot_lmtp_shared_lmtp_version
  models
}

# wrap a shift without mutating the caller's closure and provide common exposure aliases
margot_lmtp_prepare_shift <- function(shift, trt) {
  if (!is.function(shift)) {
    return(shift)
  }
  parent_env <- environment(shift)
  if (is.null(parent_env)) {
    parent_env <- baseenv()
  }
  wrap_env <- new.env(parent = parent_env)
  if (is.character(trt) && length(trt) == 1L && nzchar(trt)) {
    wrap_env$t0_name_exposure <- trt
    wrap_env$exposure_name <- trt
    wrap_env$A <- trt
  }
  environment(shift) <- wrap_env
  shift
}

# attach measured exposure waves in the form used by existing Margot summaries
margot_lmtp_attach_exposure_by_wave <- function(model, data, trt) {
  if (!is.character(trt) || !all(trt %in% names(data))) {
    return(model)
  }
  ratio_waves <- ncol(model$density_ratios)
  if (!is.numeric(ratio_waves) || length(ratio_waves) != 1L || ratio_waves < 1L) {
    return(model)
  }
  exposure <- as.matrix(data[, trt, drop = FALSE])
  if (ncol(exposure) >= ratio_waves) {
    exposure <- exposure[, seq_len(ratio_waves), drop = FALSE]
  }
  if (nrow(exposure) == nrow(model$density_ratios)) {
    model$exposure_by_wave <- exposure
  }
  model
}

# run every policy-specific shared fit and return the legacy per-cell task records
margot_lmtp_run_shared_tasks <- function(
    data,
    outcome_vars,
    trt,
    shift_functions,
    lmtp_defaults,
    mtp_by_arm,
    seed,
    save_output,
    checkpoint_dir,
    progress = c("cli", "progressr", "none")) {
  progress <- match.arg(progress)
  total_tasks <- length(outcome_vars) * length(shift_functions)
  pb_id <- NULL
  if (identical(progress, "cli")) {
    pb_id <- cli::cli_progress_bar(
      total = total_tasks,
      format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
    on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
  }

  results <- lapply(names(shift_functions), function(shift_name) {
    shift <- margot_lmtp_prepare_shift(shift_functions[[shift_name]], trt)
    shared_args <- c(
      list(data = data, trt = trt, outcomes = outcome_vars, shift = shift),
      lmtp_defaults
    )
    if (!is.null(mtp_by_arm)) {
      shared_args$mtp <- unname(mtp_by_arm[[shift_name]])
    }
    if (!is.null(seed)) {
      set.seed(seed)
    }

    fitted <- do.call(margot_lmtp_sdr_shared, shared_args)
    lapply(outcome_vars, function(outcome) {
      result <- list(
        outcome = outcome,
        shift_name = shift_name,
        model_name = paste0(outcome, "_", shift_name),
        success = TRUE,
        model = NULL,
        error = NULL
      )
      result$model <- margot_lmtp_attach_exposure_by_wave(
        fitted[[outcome]], data = data, trt = trt
      )
      attr(result$model, "margot_density_ratio_fit_id") <- shift_name
      if (save_output && !is.null(checkpoint_dir)) {
        checkpoint_file <- paste0(outcome, "_", shift_name, ".rds")
        checkpoint_path <- file.path(checkpoint_dir, checkpoint_file)
        checkpoint_obj <- list(
          model = result$model,
          outcome = outcome,
          shift_name = shift_name,
          timestamp = Sys.time()
        )
        saveRDS(checkpoint_obj, file = checkpoint_path, compress = TRUE)
        result$checkpoint_path <- checkpoint_path
        cli::cli_alert_success("Saved checkpoint: {.file {checkpoint_file}}")
      }
      if (!is.null(pb_id)) {
        try(
          cli::cli_progress_update(
            id = pb_id,
            inc = 1,
            status = if (result$success) {
              sprintf("Completed %s - %s", outcome, shift_name)
            } else {
              sprintf("Error in %s - %s", outcome, shift_name)
            }
          ),
          silent = TRUE
        )
      }
      result
    })
  })
  unlist(results, recursive = FALSE)
}
