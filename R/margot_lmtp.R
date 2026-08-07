#' Batch Process LMTP Models
#'
#' This function runs multiple Longitudinal Modified Treatment Policy (LMTP) models for specified outcome variables,
#' calculates contrasts, creates evaluation tables, and optionally saves
#' checkpoints and the complete output as `.rds` files.
#'
#' @section Design and execution:
#' `margot_lmtp()` executes an LMTP analysis. A study's causal question, causal
#' estimand, identification assumptions, policy rationale, and decision rules
#' belong in its protocol rather than this software call. Keeping those design
#' commitments outside the estimator prevents a later computational improvement
#' from changing the scientific workflow.
#'
#' Supplying `estimator_spec` locks the execution settings. The `lmtp` call is
#' then built from the specification's `call_arguments` — the exposure at each node, the
#' baseline and time-varying covariates, the censoring and competing-event
#' indicators, the outcome and its model, the identifier, the folds, the bounds,
#' the registered learner library, the analysis-weight column, and the cap — and any conflicting user
#' argument errors with a condition of class
#' `margot_error_estimator_spec_conflict` that names the conflict. The specification
#' supplies the whole `lmtp_defaults` list, so any entry supplied alongside it —
#' one the specification fixes, or one the derived list would drop — raises that condition
#' rather than passing in silence. Margot re-verifies the specification's
#' content hash on entry, so an object edited after creation is refused.
#'
#' @section Scheduling modes:
#' The shared density-ratio route (`reuse_density_ratios = TRUE`) runs in one of
#' two modes, selected by `manage_future_plan` and the caller's `future` plan.
#'
#' *Fold-parallel* (`manage_future_plan = FALSE`) fits each policy and each
#' outcome in turn, and the caller's plan parallelises the cross-fitting folds
#' inside `lmtp`.
#'
#' *Task-parallel* (`manage_future_plan = TRUE`) schedules one density task for
#' each policy-specific ratio-fit identity and, as each density task resolves,
#' one outcome task for every terminal outcome. Each task keeps its own folds
#' sequential. When the current plan is already an explicit multi-worker plan —
#' `future::multisession`, or a `future::cluster` plan spanning several machines
#' — Margot schedules over it and never alters it. Otherwise Margot opens a local
#' `multisession` pool sized from `models_in_parallel` and `n_cores`, and
#' restores the caller's plan exactly on success and on error. That default local
#' pool counts performance cores alone on Apple Silicon, since a task that
#' assumes one performance core cannot use an efficiency core, and an explicit
#' `models_in_parallel` overrides the cap. Every worker
#' reports its R, `lmtp`, and `margot` versions and a fingerprint of the shared
#' path's internals before any task is dispatched; an inconsistent fleet is
#' refused with a condition of class `margot_error_worker_ineligible`. Because
#' nested worker pools remain deferred, `cv_workers` above one is refused with
#' `margot_error_nested_parallel_unsupported`. The coordinator transports the
#' recorded random-number state to each worker, so a deterministic fixture
#' reproduces the fold-parallel route bit for bit.
#'
#' Task-parallel scheduling requires an explicit `seed`, and refuses `seed =
#' NULL` with `margot_error_task_seed_required`. The sequential route lets each
#' policy continue the previous policy's random-number state, which concurrent
#' policies cannot reproduce. Given a seed, the mode leaves the caller's
#' random-number state at exactly the state `set.seed(seed)` produces, on every
#' exit path and whatever the worker count, resolution order, or checkpoint
#' availability. That contract differs from the sequential route, which leaves
#' whatever state its last fit reached; both routes change the caller's state,
#' and neither preserves the state at entry. The identity is built from that
#' full state and from `RNGkind()`, so the same integer seed under a different
#' generator is a different identity.
#'
#' Shift functions travel to workers inside the task payload, so each shift must
#' be self-contained: it may read its arguments and its own captured values, and
#' must not depend on objects that exist only in the caller's global
#' environment. The realised policy-shifted values enter the task identity, so a
#' shift whose captured values change receives a new identity. A shift that
#' draws random numbers is refused with
#' `margot_error_stochastic_shift_unsupported`, because its realised values
#' would depend on when each task ran; a stochastic policy needs a registered
#' scheduling-independent design, which this mode does not yet provide.
#'
#' With `save_output = TRUE`, each policy-specific density result is written once
#' to an immutable, identity-keyed checkpoint under
#' `<save_path>/checkpoints/density`. A later call with the same inputs reads and
#' verifies that checkpoint instead of refitting; the reuse is reported in
#' `ratio_checkpoint_reuse_count` and does not increase `ratio_fit_count`. A
#' checkpoint carries the density ratios, the treatment and censoring learner
#' fits, the common fold map, and the post-density random-number state alone;
#' every terminal-outcome task is built afresh from the current call's data, so
#' changed outcome values are always analysed. A corrupt or mismatched
#' checkpoint refuses with `margot_error_density_checkpoint_invalid`, and two
#' distinct stored results under one identity refuse with
#' `margot_error_density_checkpoint_conflict`, rather than being refitted over.
#'
#' The eligibility probe compares R, platform, `margot`, `lmtp`, and a fixed set
#' of learner-package versions, together with a fingerprint over the shared
#' path's `margot` and `lmtp` internals. It cannot fingerprint an arbitrary
#' user-registered `SuperLearner` wrapper or its transitive dependencies, so a
#' fleet that registers its own learners must keep those packages aligned by
#' other means.
#'
#' @section Stage-split execution:
#' The shared route fits one treatment-and-censoring density stage per policy and
#' then one outcome regression per terminal outcome, so `K` outcomes cost
#' `G + sum_k Q_k` rather than `sum_k (G + Q_k)`. `stages` lets the two halves run
#' in separate calls, so positivity can be assessed before any outcome model is
#' fitted.
#'
#' `stages = "density"` runs the coordinator preflight and the density stage
#' alone, writes the density checkpoints, and returns an object of class
#' `margot_lmtp_density_stage` carrying the per-policy density-ratio matrices,
#' the task records, the identities and result fingerprints, and diagnostics from
#' [margot_lmtp_positivity()] and [margot_lmtp_overlap()]. Margot supplies the
#' assessment artefacts alone: no threshold is applied and no pass-or-fail
#' verdict is recorded, because both belong to the investigator's registered
#' protocol.
#'
#' `stages = "outcome"` fits the outcome stages and requires every policy's
#' density result to resolve from a verified checkpoint. A policy without one
#' refuses with `margot_error_density_checkpoint_required` rather than refitting
#' the exposure and censoring models.
#'
#' @section Outcome recovery:
#' With `save_output = TRUE`, the task route also writes each fitted outcome
#' model once, keyed by its outcome-task fingerprint, under
#' `<save_path>/checkpoints/outcomes`. A later call whose task fingerprint
#' matches reuses that model instead of refitting, so an interrupted run resumes
#' where it stopped; a changed outcome column, learner, control, or density
#' result yields a different fingerprint and a fresh fit. The per-run checkpoint
#' directory holds a hard link to the same single copy, so
#' [margot_lmtp_restore_checkpoints()] keeps working on a run directory without a
#' second copy of every model. Reuse is reported in
#' `outcome_checkpoint_reuse_count` alongside `outcome_fit_count`.
#'
#' @section Thread discipline:
#' Task-parallel scheduling assumes one thread per worker. Cap the native
#' libraries in the launcher, before R starts, by exporting
#' `OMP_NUM_THREADS=1`, `OPENBLAS_NUM_THREADS=1`, `VECLIB_MAXIMUM_THREADS=1`,
#' `MKL_NUM_THREADS=1`, and `RCPP_PARALLEL_NUM_THREADS=1`.
#'
#' Learner wrappers must do the same: `SL.ranger` with `num.threads = 1` and
#' `SL.xgboost` with `nthread = 1`. Margot does not administer machines, so an
#' uncapped learner will oversubscribe the performance cores that the outer task
#' budget assumes.
#'
#' @details
#' For very large datasets or models with many time points, parallel processing may not improve performance
#' as much as expected. This is because LMTP models can be memory-bound rather than CPU-bound when working
#' with large data. In such cases, memory pressure and data copying between workers may offset the benefits
#' of parallelization. Consider using fewer cores or sequential processing for very large models if you
#' experience performance degradation.
#'
#' @param data A data frame containing all necessary variables.
#' @param outcome_vars A character vector of outcome variable names to be modelled. Optional when `estimator_spec` is supplied, which locks it.
#' @param trt A character string specifying the treatment variable. Optional when `estimator_spec` is supplied, which locks it.
#' @param shift_functions A list of shift functions to be applied. Each function should take `data` and `trt` as arguments.
#' @param include_null_shift Logical, whether to include a null shift. Default is TRUE.
#' @param lmtp_model_type The LMTP model function to use. Default is lmtp_tmle.
#' @param contrast_type Type of contrasts to compute: "pairwise" or "null". Default is "pairwise".
#' @param contrast_scale Scale for contrasts: "additive", "rr", or "or". Default is "additive".
#' @param lmtp_defaults A list of default parameters for the LMTP models. Must be empty when `estimator_spec` is supplied, which builds the whole list from the specification.
#' @param n_cores Total number of CPU cores to budget for the batch run. Default is detectCores() - 1 (includes efficiency cores on Apple Silicon, so set manually if you want to cap at performance cores).
#' @param models_in_parallel Optional cap on how many LMTP models to run at once. Defaults to floor(n_cores / cv_workers).
#' @param cv_workers Number of workers consumed internally by each LMTP fit (usually the cross-validation folds). Defaults to future::nbrOfWorkers().
#' @param save_output Logical, whether to save per-model checkpoints and the
#'   complete output. Saved artefacts are written as `.rds` files. Default is
#'   FALSE.
#' @param save_path The directory path to save the output. Default is
#'   "push_mods" in the current working directory. A run that keeps checkpoints
#'   beyond itself — any `stages` other than `"all"`, or `save_output = TRUE` on
#'   the task-parallel shared route — must supply this argument explicitly and
#'   errors with `margot_error_save_path_required` otherwise, because where those
#'   artefacts live is the investigator's decision rather than Margot's.
#' @param base_filename The base filename for saving the output. Default is "lmtp_output".
#' @param use_timestamp Logical, whether to include a timestamp in the filename. Default is FALSE.
#' @param prefix Optional prefix to add to the saved output filename. Default is NULL.
#' @param manage_future_plan Logical, whether Margot schedules the outer model
#'   futures. Default is FALSE. On the independent route (`reuse_density_ratios
#'   = FALSE`), TRUE sets up nested futures (outer loop for models, inner loop
#'   for cross-validation) and restores the caller's plan on exit. On the shared
#'   density-ratio route (`reuse_density_ratios = TRUE`), TRUE selects
#'   task-parallel scheduling: see the "Scheduling modes" section. When FALSE,
#'   models run one at a time and each fit uses the caller's external
#'   `future::plan()` for parallel cross-fitting.
#' @param progress Progress reporting method: "cli" (default CLI progress bar), "progressr" (use progressr package handlers), or "none" (no progress reporting).
#' @param seed Optional single whole number seeding every stochastic step: the
#'   RNG at entry, each model fit, and the parallel streams. Default NULL leaves
#'   the RNG untouched. When `estimator_spec` is supplied the seed comes from the
#'   locked specification, and supplying a different one errors.
#' @param stages Which stages of the shared density-ratio route to execute:
#'   `"all"` (default) fits the density stage and every outcome stage in one
#'   call; `"density"` fits the policy-specific density-ratio stage alone and
#'   returns its diagnostics for positivity assessment; `"outcome"` fits the
#'   outcome stages from density checkpoints already written. Anything other than
#'   `"all"` requires `reuse_density_ratios = TRUE`, `manage_future_plan = TRUE`,
#'   and `save_output = TRUE`. See the "Stage-split execution" section.
#' @param estimator_spec Optional locked `margot_lmtp_estimator_spec` object from
#'   [margot_lmtp_estimator_spec()]. When supplied, the `lmtp` call
#'   is built from the specification and every conflicting user argument
#'   errors.
#' @param reuse_density_ratios Logical. When `TRUE`, sequentially doubly robust
#'   fits sharing one policy-specific nuisance identity fit the treatment and
#'   censoring density ratios once and reuse them across `outcome_vars`. The
#'   returned models, contrasts, and tables retain the existing Margot
#'   structure. Default is `FALSE` while the opt-in path is validated.
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
    outcome_vars = NULL,
    trt = NULL,
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
    progress = c("cli", "progressr", "none"),
    seed = NULL,
    reuse_density_ratios = FALSE,
    stages = c("all", "density", "outcome"),
    estimator_spec = NULL) {
  # Load required packages
  library(cli)
  library(progressr)

  contrast_type <- match.arg(contrast_type)
  contrast_scale <- match.arg(contrast_scale)
  stages <- match.arg(stages)
  # where cross-run artefacts are stored is the investigator's decision, so a
  # mode that writes or reads them cannot fall back on the default location
  save_path_supplied <- !missing(save_path)
  if (!is.logical(reuse_density_ratios) || length(reuse_density_ratios) != 1L ||
      is.na(reuse_density_ratios)) {
    cli::cli_abort("{.arg reuse_density_ratios} must be `TRUE` or `FALSE`.")
  }
  if (!identical(stages, "all")) {
    if (!isTRUE(reuse_density_ratios) || !isTRUE(manage_future_plan)) {
      cli::cli_abort(
        c(
          "Stage-split execution belongs to the task-parallel shared route.",
          "x" = "Received {.arg stages} = {.val {stages}}.",
          "i" = "Set {.code reuse_density_ratios = TRUE} and {.code manage_future_plan = TRUE}."
        ),
        class = "margot_error_unsupported_stage_split"
      )
    }
    if (!isTRUE(save_output)) {
      # a stage that writes nothing cannot hand its work to the next stage
      cli::cli_abort(
        c(
          "Stage-split execution requires {.code save_output = TRUE}.",
          "x" = "With {.arg stages} = {.val {stages}} the stages exchange work through checkpoints.",
          "i" = "Set {.code save_output = TRUE} and a {.arg save_path} both stages can read."
        ),
        class = "margot_error_unsupported_stage_split"
      )
    }
  }
  # density-ratio reuse selects its scheduler from the existing plan argument:
  # fold-parallel when the caller keeps their own plan, task-parallel when Margot
  # is asked to manage outer model futures
  shared_scheduler <- if (isTRUE(reuse_density_ratios) && isTRUE(manage_future_plan)) {
    "task"
  } else {
    "sequential"
  }
  # the task route's density and outcome checkpoints outlive the run that wrote
  # them, and a stage split reads what an earlier call stored; both need a
  # storage root the caller chose
  if (!save_path_supplied &&
      (!identical(stages, "all") ||
        (isTRUE(save_output) && identical(shared_scheduler, "task")))) {
    cli::cli_abort(
      c(
        "This run keeps checkpoints that outlive it, so it needs an explicit {.arg save_path}.",
        "x" = "No {.arg save_path} was supplied, and Margot does not choose a storage root for you.",
        "i" = "Pass the directory these artefacts belong in; where they live is your decision."
      ),
      class = "margot_error_save_path_required"
    )
  }
  # the legacy nested-plan branch governs the independent route alone
  manage_legacy_plan <- isTRUE(manage_future_plan) && !isTRUE(reuse_density_ratios)

  # the locked specification, where one is supplied, is authoritative over every
  # modelling argument it fixes; a conflicting user argument errors by name
  mtp_by_arm <- NULL
  if (!is.null(estimator_spec)) {
    supplied <- c(
      if (!missing(outcome_vars) && !is.null(outcome_vars)) "outcome_vars",
      if (!missing(trt) && !is.null(trt)) "trt",
      if (!missing(lmtp_model_type)) "lmtp_model_type",
      if (!is.null(seed)) "seed"
    )
    if (!missing(include_null_shift) && isTRUE(include_null_shift)) {
      supplied <- c(supplied, "include_null_shift")
    }
    from_spec <- margot_lmtp_args_from_spec(
      estimator_spec = estimator_spec,
      trt = trt,
      outcome_vars = outcome_vars,
      lmtp_defaults = lmtp_defaults,
      lmtp_model_type = lmtp_model_type,
      seed = seed,
      shift_functions = shift_functions,
      supplied = supplied,
      data = data
    )
    trt <- from_spec$trt
    outcome_vars <- from_spec$outcome_vars
    lmtp_defaults <- from_spec$lmtp_defaults
    lmtp_model_type <- from_spec$lmtp_model_type
    seed <- from_spec$seed
    mtp_by_arm <- from_spec$mtp_by_arm
    include_null_shift <- FALSE
    cli::cli_alert_info(
      "Building the {.pkg lmtp} call from the locked Margot estimator specification at seed {.val {seed}}."
    )
  }
  if (isTRUE(reuse_density_ratios) && !identical(lmtp_model_type, lmtp::lmtp_sdr)) {
    cli::cli_abort(
      c(
        "Density-ratio reuse currently supports {.fn lmtp::lmtp_sdr} alone.",
        "i" = "Use the legacy path for another estimator."
      ),
      class = "margot_error_unsupported_estimator"
    )
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      cli::cli_abort("{.arg seed} must be a single whole number.")
    }
    seed <- as.integer(seed)
    set.seed(seed)
  }

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
  if (isTRUE(reuse_density_ratios) && identical(shared_scheduler, "task")) {
    cli::cli_alert_info(
      sprintf(
        "Scheduling %d common ratio task(s) and %d outcome task(s) concurrently; each task keeps its cross-fitting folds sequential.",
        length(shift_functions),
        total_tasks
      )
    )
  } else if (isTRUE(reuse_density_ratios)) {
    cli::cli_alert_info(
      sprintf(
        "Scheduling %d common ratio stage(s) and %d outcome stage(s); cross-fitting parallelism is controlled by your future::plan().",
        length(shift_functions),
        total_tasks
      )
    )
  } else if (isTRUE(manage_future_plan)) {
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

  # create checkpoint directory if saving output. The run identifier carries
  # sub-second time and the process id, so two calls that start in the same
  # second cannot share a directory; it consumes no random numbers.
  checkpoint_dir <- NULL
  density_checkpoint_dir <- NULL
  outcome_checkpoint_dir <- NULL
  if (save_output) {
    run_id <- paste0(
      format(Sys.time(), "%Y%m%d_%H%M%OS3"),
      "_p", Sys.getpid()
    )
    checkpoint_dir <- file.path(
      save_path,
      "checkpoints",
      paste0(
        ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
        run_id
      )
    )

    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_info("Checkpoints \u2192 {.path {checkpoint_dir}}")
    if (identical(shared_scheduler, "task")) {
      # density and outcome checkpoints are fingerprint-keyed and immutable, so
      # they live beside the per-run directory and a later run resumes from them
      density_checkpoint_dir <- file.path(save_path, "checkpoints", "density")
      dir.create(density_checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
      outcome_checkpoint_dir <- file.path(save_path, "checkpoints", "outcomes")
      dir.create(outcome_checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Optionally manage the future plan internally (nested outer × inner). By default,
  # do not touch the user's plan and rely on their external configuration.
  if (manage_legacy_plan) {
    # record the caller's complete plan stack and the options this branch
    # overwrites, including whether each option was set at all, and restore both
    # through one exit handler that runs on success and on error
    old_plan <- future::plan("list")
    old_options <- list(
      mc.cores = getOption("mc.cores"),
      parallelly.maxWorkers.localhost = getOption("parallelly.maxWorkers.localhost")
    )
    on.exit(
      {
        future::plan(old_plan, substitute = FALSE)
        options(old_options)
      },
      add = TRUE
    )

    # set options BEFORE creating nested futures to avoid worker limit errors
    options(mc.cores = total_cores)
    options(parallelly.maxWorkers.localhost = total_cores)

    cli::cli_alert_info("Using {total_cores} core{?s} for parallel processing")

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
  } else if (identical(shared_scheduler, "task")) {
    # the task scheduler resolves its own worker pool and reports it there
    cli::cli_alert_info(
      "Each task keeps its cross-fitting folds sequential; cap every learner and native library at one thread per worker."
    )
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

    if (isTRUE(reuse_density_ratios)) {
      cli::cli_alert_info(
        "Running {length(shift_functions)} policy-specific fit-once batch{?es} sequentially."
      )
    } else {
      cli::cli_alert_info(
        "Running {total_tasks} LMTP fit{?s} sequentially (one at a time)."
      )
    }
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

  if (isTRUE(reuse_density_ratios)) {
    cli::cli_alert_info(
      "Fitting {length(shift_functions)} common density-ratio stage{?s} for {length(outcome_vars)} outcome{?s}."
    )
    task_results <- margot_lmtp_run_shared_tasks(
      data = data,
      outcome_vars = outcome_vars,
      trt = trt,
      shift_functions = shift_functions,
      lmtp_defaults = lmtp_defaults,
      mtp_by_arm = mtp_by_arm,
      seed = seed,
      save_output = save_output,
      checkpoint_dir = checkpoint_dir,
      progress = progress,
      scheduler = shared_scheduler,
      n_cores = total_cores,
      models_in_parallel = inferred_models_in_parallel,
      models_in_parallel_supplied = !is.null(models_in_parallel),
      cv_workers = cv_workers,
      density_checkpoint_dir = density_checkpoint_dir,
      outcome_checkpoint_dir = outcome_checkpoint_dir,
      estimator_spec_hash = if (!is.null(estimator_spec)) estimator_spec$content_hash else NULL,
      stages = stages
    )
    if (inherits(task_results, "margot_lmtp_density_stage")) {
      # the density stage returns its diagnostics; no outcome model exists yet
      cli::cli_alert_success("Density stage complete for {length(shift_functions)} polic{?y/ies}.")
      if (save_output) {
        output_path <- file.path(
          save_path,
          paste0(
            ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
            base_filename, "_density_stage.rds"
          )
        )
        tryCatch(
          saveRDS(task_results, file = output_path, compress = TRUE),
          error = function(e) {
            cli::cli_alert_danger(paste("Failed to save the density stage:", e$message))
          }
        )
      }
      return(task_results)
    }
  } else if (identical(progress, "progressr")) {
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
      # the locked specification fixes mtp per arm; the exploratory path leaves it alone
      if (!is.null(mtp_by_arm)) {
        lmtp_args$mtp <- unname(mtp_by_arm[[shift_name]])
      }
      # seed each fit so the run reproduces independently of task scheduling
      if (!is.null(seed)) set.seed(seed)

      res <- tryCatch({
        model <- do.call(lmtp_model_type, lmtp_args)
        # Attach exposure-by-wave for downstream policy-rate summaries when possible
        # Best-effort: only if `trt` is a character vector of column names and
        # dimensions align with the model's density ratios.
        try({
          dr_cols <- try(ncol(model$density_ratios), silent = TRUE)
          if (!inherits(dr_cols, "try-error") && is.numeric(dr_cols) && dr_cols > 0) {
            if (is.character(trt)) {
              if (all(trt %in% colnames(data))) {
                exp_mat <- as.matrix(data[, trt, drop = FALSE])
                # If there are more exposure columns than waves used, keep the first ones
                if (ncol(exp_mat) >= dr_cols) {
                  exp_mat <- exp_mat[, seq_len(dr_cols), drop = FALSE]
                }
                # Basic sanity: same number of rows
                if (nrow(exp_mat) == nrow(model$density_ratios)) {
                  model$exposure_by_wave <- exp_mat
                }
              }
            }
          }
        }, silent = TRUE)
        result$success <- TRUE
        result$model <- model

        # checkpoint: save immediately after successful fit
        if (save_output && !is.null(checkpoint_dir)) {
          checkpoint_file <- paste0(outcome, "_", shift_name, ".rds")
          checkpoint_path <- file.path(checkpoint_dir, checkpoint_file)

          checkpoint_obj <- list(
            model = model,
            outcome = outcome,
            shift_name = shift_name,
            timestamp = Sys.time()
          )

          saveRDS(checkpoint_obj, file = checkpoint_path, compress = TRUE)

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
        future.apply::future_lapply(
          seq_len(total_tasks), worker_fun,
          future.seed = if (is.null(seed)) TRUE else seed
        )
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
      # the locked specification fixes mtp per arm; the exploratory path leaves it alone
      if (!is.null(mtp_by_arm)) {
        lmtp_args$mtp <- unname(mtp_by_arm[[shift_name]])
      }
      # seed each fit so the run reproduces independently of task scheduling
      if (!is.null(seed)) set.seed(seed)

      res <- tryCatch({
        model <- do.call(lmtp_model_type, lmtp_args)
        result$success <- TRUE
        result$model <- model

        # checkpoint: save immediately after successful fit
        if (save_output && !is.null(checkpoint_dir)) {
          checkpoint_file <- paste0(outcome, "_", shift_name, ".rds")
          checkpoint_path <- file.path(checkpoint_dir, checkpoint_file)

          checkpoint_obj <- list(
            model = model,
            outcome = outcome,
            shift_name = shift_name,
            timestamp = Sys.time()
          )

          saveRDS(checkpoint_obj, file = checkpoint_path, compress = TRUE)

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
      task_results <- future.apply::future_lapply(
        seq_len(total_tasks), worker_fun,
        future.seed = if (is.null(seed)) TRUE else seed
      )
    } else {
      task_results <- lapply(seq_len(total_tasks), worker_fun)
    }
    if (!is.null(pb_id)) {
      try(cli::cli_progress_done(id = pb_id), silent = TRUE)
    }
  }
  # the caller's plan and options are restored by the single on.exit handler
  # registered where this branch changed them

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
  }

  finalized <- margot_lmtp_finalize_outputs(
    all_models = all_models,
    outcome_vars = outcome_vars,
    shift_names = shift_names,
    contrast_type = contrast_type,
    contrast_scale = contrast_scale,
    quiet = FALSE
  )
  all_models <- finalized$models
  all_contrasts <- finalized$contrasts
  all_tables <- finalized$individual_tables
  combined_tables <- finalized$combined_tables

  # Prepare the complete output
  complete_output <- list(
    models = all_models,
    contrasts = all_contrasts,
    individual_tables = all_tables,
    combined_tables = combined_tables
  )
  if (isTRUE(reuse_density_ratios)) {
    # the density-fit count is observed from the returned task records rather
    # than declared from the number of policies, and a verified checkpoint read
    # counts as a reuse rather than as another fit
    density_records <- attr(task_results, "margot_lmtp_density_records") %||% list()
    observed_ratio_fits <- sum(vapply(
      density_records,
      function(record) {
        isTRUE(record$success) && !identical(record$density_source, "checkpoint")
      },
      logical(1)
    ))
    observed_ratio_reuses <- sum(vapply(
      density_records,
      function(record) {
        isTRUE(record$success) && identical(record$density_source, "checkpoint")
      },
      logical(1)
    ))
    # outcome models are observed the same way: a verified checkpoint read is a
    # reuse, and everything else this call completed is a fresh fit
    outcome_sources <- vapply(
      task_results,
      function(result) result$task_record$model_source %||% "fit",
      character(1)
    )
    observed_outcome_fits <- sum(outcome_sources != "checkpoint")
    observed_outcome_reuses <- sum(outcome_sources == "checkpoint")
    attr(complete_output, "margot_density_ratio_reuse") <- list(
      enabled = TRUE,
      ratio_fit_count = as.integer(observed_ratio_fits),
      ratio_checkpoint_reuse_count = as.integer(observed_ratio_reuses),
      outcome_fit_count = as.integer(observed_outcome_fits),
      outcome_checkpoint_reuse_count = as.integer(observed_outcome_reuses),
      scheduler = shared_scheduler,
      worker_count = as.integer(attr(task_results, "margot_lmtp_worker_count") %||% 1L),
      density_records = density_records,
      legacy_ratio_fit_count = length(outcome_vars) * length(shift_functions),
      outcomes = outcome_vars,
      policies = shift_names,
      lmtp_version = margot_lmtp_shared_lmtp_version
    )
  }

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

        output_path <- file.path(save_path, paste0(output_filename, ".rds"))
        saveRDS(complete_output, file = output_path, compress = TRUE)
        cli::cli_alert_success("Complete output saved successfully: {.file {basename(output_path)}}")
      },
      error = function(e) {
        cli::cli_alert_danger(paste("Failed to save complete output:", e$message))
      }
    )
  }

  cli::cli_alert_success("Analysis complete")

  if (manage_legacy_plan) {
    cli::cli_alert_info("Shutting down parallel workers...")
  }

  return(complete_output)
}
