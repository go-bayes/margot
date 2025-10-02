#' Batch Process LMTP Models
#'
#' This function runs multiple Longitudinal Modified Treatment Policy (LMTP) models for specified outcome variables,
#' calculates contrasts, creates evaluation tables, and optionally saves the complete output.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modeled.
#' @param trt A character string specifying the treatment variable.
#' @param shift_functions A list of shift functions to be applied. Each function should take `data` and `trt` as arguments.
#' @param include_null_shift Logical, whether to include a null shift. Default is TRUE.
#' @param lmtp_model_type The LMTP model function to use. Default is lmtp_tmle.
#' @param contrast_type Type of contrasts to compute: "pairwise" or "null". Default is "pairwise".
#' @param contrast_scale Scale for contrasts: "additive", "rr", or "or". Default is "additive".
#' @param lmtp_defaults A list of default parameters for the LMTP models.
#' @param n_cores Total number of CPU cores to budget for the batch run. Default is detectCores() - 1 (includes efficiency cores on Apple Silicon, so set manually if you want to cap at performance cores).
#' @param models_in_parallel Optional cap on how many LMTP models to run at once. Defaults to floor(n_cores / cv_workers).
#' @param cv_workers Number of workers consumed internally by each LMTP fit (usually the cross-validation folds). Defaults to future::nbrOfWorkers().
#' @param save_output Logical, whether to save the complete output. Default is FALSE.
#' @param save_path The directory path to save the output. Default is "push_mods" in the current working directory.
#' @param base_filename The base filename for saving the output. Default is "lmtp_output".
#' @param use_timestamp Logical, whether to include a timestamp in the filename. Default is FALSE.
#' @param prefix Optional prefix to add to the saved output filename. Default is NULL.
#'
#' @return A list containing:
#'   \item{models}{A list of all LMTP models for each outcome and shift function.}
#'   \item{contrasts}{A list of contrasts computed for each outcome.}
#'   \item{individual_tables}{A list of individual tables for each contrast and outcome.}
#'   \item{combined_tables}{A list of combined tables for each contrast type across all outcomes.}
#'
#' @examples
#' \dontrun{
#' # Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders
#'
#' # Define shift functions
#' gain_function <- function(data, trt) {
#'   data[[trt]] + 1
#' }
#'
#' loss_function <- function(data, trt) {
#'   pmax(data[[trt]] - 1, 0)
#' }
#'
#' # Run LMTP analysis
#' result <- margot_lmtp(
#'   data = my_data,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   trt = "treatment",
#'   shift_functions = list(gain = gain_function, loss = loss_function),
#'   lmtp_defaults = list(
#'     baseline = c("confounder1", "confounder2"),
#'     time_vary = c("time_var1", "time_var2"),
#'     outcome_type = "continuous"
#'   ),
#'   save_output = TRUE,
#'   save_path = here::here("output", "lmtp_results"),
#'   prefix = "my_study"
#' )
#' }
#'
#' @import lmtp
#' @import margot
#' @import parallel
#' @import doParallel
#' @import SuperLearner
#' @import cli
#' @import progressr
#'
#' @export
margot_lmtp <- function(
    data,
    outcome_vars,
    trt,
    shift_functions = list(),
    include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_tmle,
    contrast_type = c("pairwise", "null"),
    contrast_scale = c("additive", "rr", "or"),
    lmtp_defaults = list(),
    n_cores = parallel::detectCores() - 1,
    models_in_parallel = NULL,
    cv_workers = NULL,
    save_output = FALSE,
    save_path = here::here("push_mods"),
    base_filename = "lmtp_output",
    use_timestamp = FALSE,
    prefix = NULL,
    manage_future_plan = FALSE,
    progress = c("cli", "progressr", "none")) {
  # Load required packages
  library(cli)
  library(progressr)

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)

  # ensure outcome_vars is always a character vector
  if (!is.character(outcome_vars)) {
    cli::cli_alert_danger("outcome_vars must be a character vector")
    stop("outcome_vars must be a character vector")
  }

  # add null shift if requested and capture shift names
  if (include_null_shift && !("null" %in% names(shift_functions))) {
    shift_functions <- c(shift_functions, list(null = NULL))
  }
  shift_names <- names(shift_functions)

  # preflight validation: check all required variables exist in data
  cli::cli_h2("Preflight Checks")

  # check treatment variable (if it's a character vector of column names)
  if (is.character(trt)) {
    missing_trt <- setdiff(trt, names(data))
    if (length(missing_trt) > 0) {
      cli::cli_alert_danger("Treatment variable{?s} not found in data: {.val {missing_trt}}")
      cli::cli_text("Available variables: {paste(head(names(data), 20), collapse = ', ')}...")
      stop("Treatment variable not found in data", call. = FALSE)
    }
    cli::cli_alert_success("Treatment variable{?s} found: {.val {trt}}")
  } else {
    cli::cli_alert_info("Treatment specified as non-character (indices or list) - skipping validation")
  }

  # check outcome variables
  missing_outcomes <- setdiff(outcome_vars, names(data))
  if (length(missing_outcomes) > 0) {
    cli::cli_alert_danger(
      "Outcome variable{?s} not found in data: {.val {missing_outcomes}}"
    )
    cli::cli_text("Available variables: {paste(names(data), collapse = ', ')}")
    stop("Missing outcome variables in data", call. = FALSE)
  }
  cli::cli_alert_success(
    "All {length(outcome_vars)} outcome variable{?s} found"
  )

  # check baseline variables (if specified)
  if (!is.null(lmtp_defaults$baseline)) {
    missing_baseline <- setdiff(lmtp_defaults$baseline, names(data))
    if (length(missing_baseline) > 0) {
      cli::cli_alert_danger(
        "Baseline variable{?s} not found in data: {.val {missing_baseline}}"
      )
      cli::cli_text("Available variables: {paste(names(data), collapse = ', ')}")
      stop("Missing baseline variables in data", call. = FALSE)
    }
    cli::cli_alert_success(
      "All {length(lmtp_defaults$baseline)} baseline variable{?s} found"
    )
  }

  # check time-varying variables (if specified)
  if (!is.null(lmtp_defaults$time_vary)) {
    # time_vary can be a list of vectors, so flatten it first
    if (is.list(lmtp_defaults$time_vary)) {
      time_vary_vars <- unique(unlist(lmtp_defaults$time_vary))
    } else {
      time_vary_vars <- lmtp_defaults$time_vary
    }
    missing_time_vary <- setdiff(time_vary_vars, names(data))
    if (length(missing_time_vary) > 0) {
      cli::cli_alert_danger(
        "Time-varying variable{?s} not found in data: {.val {missing_time_vary}}"
      )
      cli::cli_text("Available variables: {paste(names(data), collapse = ', ')}")
      stop("Missing time-varying variables in data", call. = FALSE)
    }
    cli::cli_alert_success(
      "All {length(time_vary_vars)} time-varying variable{?s} found"
    )
  }

  # check cens variables (if specified)
  if (!is.null(lmtp_defaults$cens)) {
    # cens can be a vector
    missing_cens <- setdiff(lmtp_defaults$cens, names(data))
    if (length(missing_cens) > 0) {
      cli::cli_alert_danger(
        "Censoring variable{?s} not found in data: {.val {missing_cens}}"
      )
      cli::cli_text("Available variables: {paste(names(data), collapse = ', ')}")
      stop("Missing censoring variables in data", call. = FALSE)
    }
    cli::cli_alert_success(
      "All {length(lmtp_defaults$cens)} censoring variable{?s} found"
    )
  }

  # check for null contrast compatibility
  if (contrast_type == "null" && !include_null_shift && !("null" %in% shift_names)) {
    cli::cli_alert_danger(
      "contrast_type = 'null' requires a null shift, but include_null_shift = FALSE and no 'null' shift provided"
    )
    stop("Cannot compute null contrasts without null shift", call. = FALSE)
  }

  cli::cli_alert_success("All preflight checks passed")

  # check if SuperLearner library is specified, if not, default to "SL.ranger"
  if (is.null(lmtp_defaults$learners_trt)) {
    lmtp_defaults$learners_trt <- "SL.ranger"
  }
  if (is.null(lmtp_defaults$learners_outcome)) {
    lmtp_defaults$learners_outcome <- "SL.ranger"
  }

  # derive concurrency settings
  total_tasks <- length(outcome_vars) * length(shift_functions)
  total_cores <- n_cores
  if (!is.numeric(total_cores) || !is.finite(total_cores)) {
    total_cores <- parallel::detectCores() - 1
  }
  total_cores <- max(1L, as.integer(total_cores))

  inferred_cv_workers <- cv_workers
  if (is.null(inferred_cv_workers)) {
    inferred_cv_workers <- tryCatch(future::nbrOfWorkers(), error = function(e) 1L)
  }
  if (!is.numeric(inferred_cv_workers) || !is.finite(inferred_cv_workers)) {
    inferred_cv_workers <- 1L
  }
  inferred_cv_workers <- max(1L, as.integer(inferred_cv_workers))

  inferred_models_in_parallel <- models_in_parallel
  if (is.null(inferred_models_in_parallel)) {
    inferred_models_in_parallel <- floor(total_cores / inferred_cv_workers)
  }
  if (!is.numeric(inferred_models_in_parallel) || !is.finite(inferred_models_in_parallel)) {
    inferred_models_in_parallel <- 1L
  }
  inferred_models_in_parallel <- max(1L, as.integer(inferred_models_in_parallel))
  inferred_models_in_parallel <- min(inferred_models_in_parallel, total_tasks)

  # Initialise results lists
  all_models <- vector("list", length(outcome_vars))
  names(all_models) <- outcome_vars
  all_contrasts <- list()
  all_tables <- list()

  # Progress mode
  progress <- match.arg(progress)
  # Progress is managed locally; do not modify global handlers here.

  # CLI setup
  cli::cli_h1("Starting LMTP Analysis")
  if (isTRUE(manage_future_plan)) {
    cli::cli_alert_info(
      sprintf(
        "Scheduling %d LMTP fits (%d outcomes x %d shifts) with up to %d concurrent model(s) reserving ~%d worker(s) each.",
        total_tasks,
        length(outcome_vars),
        length(shift_functions),
        inferred_models_in_parallel,
        inferred_cv_workers
      )
    )
  } else {
    cli::cli_alert_info(
      sprintf(
        "Scheduling %d LMTP fits (%d outcomes x %d shifts); parallelism is controlled by your future::plan().",
        total_tasks,
        length(outcome_vars),
        length(shift_functions)
      )
    )
  }

  # Build task grid
  task_grid <- expand.grid(
    outcome = outcome_vars,
    shift_name = names(shift_functions),
    stringsAsFactors = FALSE
  )

  # create checkpoint directory if saving output
  checkpoint_dir <- NULL
  if (save_output) {
    checkpoint_dir <- file.path(
      save_path,
      "checkpoints",
      paste0(
        ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
    )

    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_info("Checkpoints \u2192 {.path {checkpoint_dir}}")
  }

  # Optionally manage the future plan internally (nested outer × inner). By default,
  # do not touch the user's plan and rely on their external configuration.
  if (isTRUE(manage_future_plan)) {
    # set options BEFORE creating nested futures to avoid worker limit errors
    options(mc.cores = total_cores)
    options(parallelly.maxWorkers.localhost = total_cores)

    # prevent underlying libraries (ranger, BLAS, OpenMP) from spawning extra threads
    # this ensures we use exactly the cores we specify, no more
    old_omp <- Sys.getenv("OMP_NUM_THREADS", unset = NA)
    old_openblas <- Sys.getenv("OPENBLAS_NUM_THREADS", unset = NA)
    old_mkl <- Sys.getenv("MKL_NUM_THREADS", unset = NA)

    Sys.setenv(OMP_NUM_THREADS = 1)
    Sys.setenv(OPENBLAS_NUM_THREADS = 1)
    Sys.setenv(MKL_NUM_THREADS = 1)

    old_plan <- future::plan()
    # always shut down workers when done, even if old_plan wasn't sequential
    on.exit({
      future::plan(future::sequential)
      gc()  # garbage collect to clean up

      # restore thread settings
      if (is.na(old_omp)) Sys.unsetenv("OMP_NUM_THREADS") else Sys.setenv(OMP_NUM_THREADS = old_omp)
      if (is.na(old_openblas)) Sys.unsetenv("OPENBLAS_NUM_THREADS") else Sys.setenv(OPENBLAS_NUM_THREADS = old_openblas)
      if (is.na(old_mkl)) Sys.unsetenv("MKL_NUM_THREADS") else Sys.setenv(MKL_NUM_THREADS = old_mkl)
    }, add = TRUE)

    outer_strategy <- if (inferred_models_in_parallel > 1L) {
      future::tweak(future::multisession, workers = inferred_models_in_parallel)
    } else {
      future::sequential
    }

    inner_strategy <- if (inferred_cv_workers > 1L) {
      # use I() to tell future we know what we're doing with nested parallelization
      future::tweak(future::multisession, workers = I(inferred_cv_workers))
    } else {
      future::sequential
    }

    combined_plan <- list(outer_strategy)
    if (!identical(inner_strategy, future::sequential)) {
      combined_plan <- c(combined_plan, list(inner_strategy))
    }
    future::plan(combined_plan, substitute = FALSE)
  } else {
    # when manage_future_plan = FALSE, respect user's external future plan
    # models run sequentially via lapply, but each model can use parallel CV

    # detect current plan to inform user
    current_plan_info <- tryCatch({
      plan_list <- future::plan("list")
      n_workers <- future::nbrOfWorkers()
      list(workers = n_workers, plan = class(plan_list[[1]])[1])
    }, error = function(e) {
      list(workers = 1, plan = "unknown")
    })

    cli::cli_alert_info(
      "Running {total_tasks} LMTP fit{?s} sequentially (one at a time)."
    )
    cli::cli_alert_info(
      "Each model will use your future plan for internal CV: {.strong {current_plan_info$plan}} with {.strong {current_plan_info$workers}} worker{?s}"
    )

    if (current_plan_info$workers == 1 || current_plan_info$plan == "sequential") {
      cli::cli_alert_warning(
        "LMTP internal parallelization {.emph disabled}. Set future::plan(multisession, workers = 5) before margot_lmtp() to enable parallel CV."
      )
    } else {
      cli::cli_alert_success(
        "LMTP internal parallelization {.emph enabled} - each model uses {current_plan_info$workers} parallel worker{?s} for cross-validation"
      )
    }
  }

  if (identical(progress, "progressr")) {
    task_results <- with_progress({
      p <- progressor(steps = total_tasks)
      prog <- function(msg) {
        # Avoid spurious progressr warnings if the progressor has closed
        try(suppressWarnings(p(msg)), silent = TRUE)
      }
      worker_fun <- function(task_idx) {
      # If we're running with a nested future plan (manage_future_plan = TRUE),
      # ensure inner futures (CV workers) aren't blocked by parallelly's 300% cap
      # inside this worker process. Raise mc.cores and local maxWorkers just for
      # the duration of this task.
      if (isTRUE(manage_future_plan)) {
        .old_opts <- options()
        on.exit(options(.old_opts), add = TRUE)
        inner_needed <- max(1L, as.integer(cv_workers %||% 1L))
        # use total_cores from parent environment, not getOption defaults
        options(mc.cores = total_cores)
        options(parallelly.maxWorkers.localhost = total_cores)
      }
      
      outcome <- task_grid$outcome[[task_idx]]
      shift_name <- task_grid$shift_name[[task_idx]]
      shift <- shift_functions[[shift_name]]

      # Wrap shift function to carry treatment-name globals some users reference in closures
      if (is.function(shift)) {
        # create a child environment so we don't mutate user's function
        parent_env <- environment(shift)
        if (is.null(parent_env)) parent_env <- baseenv()
        wrap_env <- new.env(parent = parent_env)
        # If trt is a single character name, provide common aliases used in shift closures
        if (is.character(trt) && length(trt) == 1L && nzchar(trt)) {
          wrap_env$t0_name_exposure <- trt
          wrap_env$exposure_name <- trt
          wrap_env$A <- trt
        }
        environment(shift) <- wrap_env
      }

      result <- list(
        outcome = outcome,
        shift_name = shift_name,
        model_name = paste0(outcome, "_", shift_name),
        success = FALSE,
        model = NULL,
        error = NULL
      )

      lmtp_args <- c(
        list(data = data, trt = trt, outcome = outcome, shift = shift),
        lmtp_defaults
      )

      res <- tryCatch({
        model <- do.call(lmtp_model_type, lmtp_args)
        result$success <- TRUE
        result$model <- model

        # checkpoint: save immediately after successful fit
        if (save_output && !is.null(checkpoint_dir)) {
          checkpoint_file <- paste0(outcome, "_", shift_name, ".qs")
          checkpoint_path <- file.path(checkpoint_dir, checkpoint_file)

          checkpoint_obj <- list(
            model = model,
            outcome = outcome,
            shift_name = shift_name,
            timestamp = Sys.time()
          )

          qs::qsave(x = checkpoint_obj, file = checkpoint_path, preset = "high", nthreads = 1)

          result$checkpoint_path <- checkpoint_path
          cli::cli_alert_success("Saved checkpoint: {.file {checkpoint_file}}")
        }

        result
      }, error = function(e) {
        error_msg <- conditionMessage(e)
        result$error <- error_msg

        # display error immediately
        cli::cli_alert_danger(
          "Model failed: {.val {outcome}} - {.val {shift_name}}"
        )
        cli::cli_text("{.emph {error_msg}}")

        # save error log
        if (save_output && !is.null(checkpoint_dir)) {
          error_file <- paste0("ERROR_", outcome, "_", shift_name, ".txt")
          writeLines(
            c(
              paste("Error:", error_msg),
              paste("Time:", Sys.time()),
              paste("Model:", outcome, "-", shift_name),
              "",
              "Full traceback:",
              paste(capture.output(print(e)), collapse = "\n")
            ),
            file.path(checkpoint_dir, error_file)
          )
          cli::cli_text("Error log saved: {.file {error_file}}")
        }

        result
      })

        if (res$success) {
          prog(sprintf("Completed %s - %s", outcome, shift_name))
        } else {
          prog(sprintf("Error in %s - %s", outcome, shift_name))
        }

        res
      }

      if (isTRUE(manage_future_plan)) {
        future.apply::future_lapply(seq_len(total_tasks), worker_fun, future.seed = TRUE)
      } else {
        lapply(seq_len(total_tasks), worker_fun)
      }
    })
  } else {
    # CLI progress bar or none
    pb_id <- NULL
    if (identical(progress, "cli")) {
      pb_id <- cli::cli_progress_bar(total = total_tasks, format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}")
    }
    prog <- function(msg) {
      if (!is.null(pb_id)) {
        # Best-effort update; ignore errors
        try(cli::cli_progress_update(id = pb_id, inc = 1, status = msg), silent = TRUE)
      }
    }
    worker_fun <- function(task_idx) {
      if (isTRUE(manage_future_plan)) {
        .old_opts <- options()
        on.exit(options(.old_opts), add = TRUE)
        inner_needed <- max(1L, as.integer(cv_workers %||% 1L))
        # use total_cores from parent environment, not getOption defaults
        options(mc.cores = total_cores)
        options(parallelly.maxWorkers.localhost = total_cores)
      }

      outcome <- task_grid$outcome[[task_idx]]
      shift_name <- task_grid$shift_name[[task_idx]]
      shift <- shift_functions[[shift_name]]

      if (is.function(shift)) {
        parent_env <- environment(shift)
        if (is.null(parent_env)) parent_env <- baseenv()
        wrap_env <- new.env(parent = parent_env)
        if (is.character(trt) && length(trt) == 1L && nzchar(trt)) {
          wrap_env$t0_name_exposure <- trt
          wrap_env$exposure_name <- trt
          wrap_env$A <- trt
        }
        environment(shift) <- wrap_env
      }

      result <- list(
        outcome = outcome,
        shift_name = shift_name,
        model_name = paste0(outcome, "_", shift_name),
        success = FALSE,
        model = NULL,
        error = NULL
      )

      lmtp_args <- c(
        list(data = data, trt = trt, outcome = outcome, shift = shift),
        lmtp_defaults
      )

      res <- tryCatch({
        model <- do.call(lmtp_model_type, lmtp_args)
        result$success <- TRUE
        result$model <- model

        # checkpoint: save immediately after successful fit
        if (save_output && !is.null(checkpoint_dir)) {
          checkpoint_file <- paste0(outcome, "_", shift_name, ".qs")
          checkpoint_path <- file.path(checkpoint_dir, checkpoint_file)

          checkpoint_obj <- list(
            model = model,
            outcome = outcome,
            shift_name = shift_name,
            timestamp = Sys.time()
          )

          qs::qsave(x = checkpoint_obj, file = checkpoint_path, preset = "high", nthreads = 1)

          result$checkpoint_path <- checkpoint_path
          cli::cli_alert_success("Saved checkpoint: {.file {checkpoint_file}}")
        }

        result
      }, error = function(e) {
        error_msg <- conditionMessage(e)
        result$error <- error_msg

        # display error immediately
        cli::cli_alert_danger(
          "Model failed: {.val {outcome}} - {.val {shift_name}}"
        )
        cli::cli_text("{.emph {error_msg}}")

        # save error log
        if (save_output && !is.null(checkpoint_dir)) {
          error_file <- paste0("ERROR_", outcome, "_", shift_name, ".txt")
          writeLines(
            c(
              paste("Error:", error_msg),
              paste("Time:", Sys.time()),
              paste("Model:", outcome, "-", shift_name),
              "",
              "Full traceback:",
              paste(capture.output(print(e)), collapse = "\n")
            ),
            file.path(checkpoint_dir, error_file)
          )
          cli::cli_text("Error log saved: {.file {error_file}}")
        }

        result
      })

      if (res$success) {
        prog(sprintf("Completed %s - %s", outcome, shift_name))
      } else {
        prog(sprintf("Error in %s - %s", outcome, shift_name))
      }

      res
    }

    if (isTRUE(manage_future_plan)) {
      task_results <- future.apply::future_lapply(seq_len(total_tasks), worker_fun, future.seed = TRUE)
    } else {
      task_results <- lapply(seq_len(total_tasks), worker_fun)
    }
    if (!is.null(pb_id)) {
      try(cli::cli_progress_done(id = pb_id), silent = TRUE)
    }
  }
  # Restore user's plan only if we changed it here
  if (isTRUE(manage_future_plan)) {
    future::plan(old_plan, substitute = FALSE)
  }

  # run models for each outcome and process downstream outputs
  for (outcome in outcome_vars) {
    cli::cli_h2("Processing outcome: {.val {outcome}}")

    outcome_results <- Filter(function(x) identical(x$outcome, outcome), task_results)
    if (length(outcome_results) > 1) {
      shift_order <- match(vapply(outcome_results, function(x) x$shift_name, character(1)), shift_names)
      outcome_results <- outcome_results[order(shift_order)]
    }

    outcome_models <- list()
    for (res in outcome_results) {
      if (isTRUE(res$success)) {
        outcome_models[[res$model_name]] <- res$model
        cli::cli_alert_success("Completed model for {.val {outcome}} with shift {.val {res$shift_name}}")
      } else {
        cli::cli_alert_danger("Error in model for {.val {outcome}} with shift {.val {res$shift_name}}: {res$error}")
      }
    }

    all_models[[outcome]] <- outcome_models

    # contrasts
    cli::cli_h3("Calculating contrasts")
    contrasts <- list()
    model_names <- names(outcome_models)

    if (length(model_names) == 0) {
      cli::cli_alert_warning("No models available for contrasts for {.val {outcome}}")
      all_contrasts[[outcome]] <- contrasts
      all_tables[[outcome]] <- list()
      next
    }

    if (contrast_type == "null") {
      null_model_name <- grep("null", model_names, value = TRUE)

      # validation: check if null model exists
      if (length(null_model_name) == 0) {
        cli::cli_alert_danger(
          "No null model found for {.val {outcome}}. Cannot compute null contrasts."
        )
        cli::cli_text("Available models: {paste(model_names, collapse = ', ')}")
        all_contrasts[[outcome]] <- list()
        all_tables[[outcome]] <- list()
        next
      }

      # validation: handle multiple null models
      if (length(null_model_name) > 1) {
        cli::cli_alert_warning(
          "Multiple null models found. Using first: {null_model_name[1]}"
        )
        null_model_name <- null_model_name[1]
      }

      null_model <- outcome_models[[null_model_name]]

      # validation: check null model is valid
      if (is.null(null_model)) {
        cli::cli_alert_danger(
          "Null model {null_model_name} exists but is NULL. Cannot compute contrasts."
        )
        all_contrasts[[outcome]] <- list()
        all_tables[[outcome]] <- list()
        next
      }

      for (model_name in model_names) {
        if (!grepl("null", model_name, fixed = TRUE)) {
          contrast_name <- paste0(model_name, "_vs_null")
          tryCatch(
            {
              contrast <- lmtp::lmtp_contrast(outcome_models[[model_name]], ref = null_model, type = contrast_scale)
              contrasts[[contrast_name]] <- contrast
              cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
            },
            error = function(e) {
              cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
            }
          )
        }
      }
    } else {
      for (i in 1:(length(model_names) - 1)) {
        for (j in (i + 1):length(model_names)) {
          contrast_name <- paste0(model_names[i], "_vs_", model_names[j])
          tryCatch(
            {
              contrast <- lmtp::lmtp_contrast(outcome_models[[model_names[i]]], ref = outcome_models[[model_names[j]]], type = contrast_scale)
              contrasts[[contrast_name]] <- contrast
              cli::cli_alert_success("Completed contrast: {.val {contrast_name}}")
            },
            error = function(e) {
              cli::cli_alert_danger("Error in contrast {.val {contrast_name}} for {.val {outcome}}: {e$message}")
            }
          )
        }
      }
    }

    all_contrasts[[outcome]] <- contrasts

    # tables
    cli::cli_h3("Creating tables")
    tables <- list()
    for (contrast_name in names(contrasts)) {
      tryCatch(
        {
          scale <- if (contrast_scale == "additive") "RD" else "RR"
          table <- margot::margot_lmtp_evalue(contrasts[[contrast_name]], scale = scale, new_name = outcome)
          tables[[contrast_name]] <- table
          cli::cli_alert_success("Created table for contrast: {.val {contrast_name}}")
        },
        error = function(e) {
          cli::cli_alert_danger("Error in creating table for contrast {.val {contrast_name}}, outcome {.val {outcome}}: {e$message}")
        }
      )
    }

    all_tables[[outcome]] <- tables
  }

  # combine tables across outcomes
  cli::cli_h2("Combining tables across outcomes")
  combined_tables <- list()

  if (contrast_type == "null") {
    for (shift_name in shift_names) {
      if (shift_name != "null") {
        contrast_name <- paste0("combined_outcomes_", shift_name, "_vs_null")
        combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
          table_name <- paste0(outcome, "_", shift_name, "_vs_null")
          if (!is.null(all_tables[[outcome]][[table_name]])) {
            all_tables[[outcome]][[table_name]]
          } else {
            cli::cli_alert_warning(paste("Table not found for:", table_name))
            NULL
          }
        }))
        if (!is.null(combined_table) && nrow(combined_table) > 0) {
          combined_tables[[contrast_name]] <- combined_table
          cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
        } else {
          cli::cli_alert_warning(paste("No data for combined table:", contrast_name))
        }
      }
    }
  } else {
    for (i in 1:(length(shift_names) - 1)) {
      for (j in (i + 1):length(shift_names)) {
        contrast_name <- paste0("combined_outcomes_", shift_names[i], "_vs_", shift_names[j])
        combined_table <- do.call(rbind, lapply(outcome_vars, function(outcome) {
          table_name <- paste0(outcome, "_", shift_names[i], "_vs_", outcome, "_", shift_names[j])
          if (!is.null(all_tables[[outcome]][[table_name]])) {
            all_tables[[outcome]][[table_name]]
          } else {
            cli::cli_alert_warning(paste("Table not found for:", table_name))
            NULL
          }
        }))
        if (!is.null(combined_table) && nrow(combined_table) > 0) {
          combined_tables[[contrast_name]] <- combined_table
          cli::cli_alert_success("Created combined table: {.val {contrast_name}}")
        } else {
          cli::cli_alert_warning(paste("No data for combined table:", contrast_name))
        }
      }
    }
  }

  # Prepare the complete output
  complete_output <- list(
    models = all_models,
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  )

  # Save complete output if save_output is TRUE
  if (save_output) {
    cli::cli_alert_info("Saving complete output...")
    tryCatch(
      {
        if (use_timestamp) {
          output_filename <- paste0(
            ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
            base_filename, "_",
            format(Sys.time(), "%Y%m%d_%H%M%S")
          )
        } else {
          output_filename <- paste0(
            ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
            base_filename
          )
        }

        margot::here_save_qs(
          obj = complete_output,
          name = output_filename,
          dir_path = save_path,
          preset = "high",
          nthreads = 1
        )
        cli::cli_alert_success("Complete output saved successfully")
      },
      error = function(e) {
        cli::cli_alert_danger(paste("Failed to save complete output:", e$message))
      }
    )
  }

  cli::cli_alert_success("Analysis complete \U0001F44D")

  if (isTRUE(manage_future_plan)) {
    cli::cli_alert_info("Shutting down parallel workers...")
  }

  return(complete_output)
}
