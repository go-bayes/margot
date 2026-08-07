# Task-parallel scheduling for the shared density-ratio path.
#
# The coordinator owns every workflow decision. It resolves the estimator
# arguments, fingerprints each policy-specific ratio-fit identity, builds and
# verifies every outcome task, schedules one Stage 1 density task per identity,
# launches that policy's Stage 2 outcome tasks as soon as its density task
# resolves, writes every checkpoint, and assembles the results in the original
# outcome x policy order through the unchanged Margot output assembly. Worker
# topology, scheduling order, and object transport are execution provenance
# alone; the caller must supply a seed, so scheduling cannot reach the numbers.
#
# Worker contract. The stage entry points below travel to workers as function
# objects, so their bodies move with the future while their enclosing
# environment resolves to the `margot` namespace already present on the worker.
# They must therefore call only long-standing `margot` internals; any new helper
# they need is written inside their own body or passed in the payload. The
# Stage 0 eligibility probe refuses a worker whose fingerprint over those
# internals, over the pinned `lmtp` internals, or over the registered learner
# packages differs from the coordinator's, so a stale or patched worker fails
# closed rather than returning quietly different numbers.

# hash any transport payload with the estimator contract's fingerprint primitive
margot_lmtp_task_fingerprint <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 2L)
}

# encode a policy or outcome name for use in a file path. An ordinary name is
# returned unchanged, so checkpoint names match the sequential route's scheme; an
# altered name carries a hash of the original, so two names that sanitise to the
# same text cannot claim one path
margot_lmtp_safe_path_name <- function(x) {
  vapply(
    as.character(x),
    function(name) {
      encoded <- gsub("[^A-Za-z0-9._-]", "_", name)
      if (identical(encoded, name)) {
        return(encoded)
      }
      paste0(encoded, "-", substr(margot_lmtp_task_fingerprint(name), 1L, 12L))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# report one process's result-affecting environment; runs on the coordinator and,
# transported as an object, on every candidate worker
margot_lmtp_worker_report <- function() {
  # deparse without source references so a source-kept development copy and an
  # installed copy of identical code fingerprint identically
  render <- function(object) {
    if (is.null(object)) {
      return(NA_character_)
    }
    control <- c("keepInteger", "keepNA")
    if (inherits(object, "R6ClassGenerator")) {
      return(paste(
        c(
          names(object$public_fields),
          unlist(lapply(
            object$public_methods,
            function(method) deparse(body(method), control = control)
          ))
        ),
        collapse = "\n"
      ))
    }
    if (!is.function(object)) {
      return(paste(deparse(object, control = control), collapse = "\n"))
    }
    paste(
      c(
        deparse(formals(object), control = control),
        deparse(body(object), control = control)
      ),
      collapse = "\n"
    )
  }
  # the shared-path internals a task worker executes, listed here rather than in
  # a package object so this function stays self-contained when it is transported
  margot_internals <- c(
    "margot_lmtp_internal",
    "margot_lmtp_make_task",
    "margot_lmtp_assert_shared_task",
    "margot_lmtp_fit_density_ratios",
    "margot_lmtp_fit_sdr_outcome",
    "margot_lmtp_restore_seed"
  )
  # the pinned lmtp internals that build folds and fit the nuisance models; a
  # locally patched lmtp 1.5.4 differs here even though its version string agrees
  lmtp_internals <- c(
    "cf_density_ratios", "cf_sdr", "theta_dr", "make_shifted", "LmtpTask"
  )
  margot_namespace <- asNamespace("margot")
  margot_sources <- lapply(margot_internals, function(name) {
    render(get0(name, envir = margot_namespace, inherits = FALSE))
  })
  names(margot_sources) <- margot_internals
  lmtp_namespace <- tryCatch(asNamespace("lmtp"), error = function(e) NULL)
  lmtp_sources <- lapply(lmtp_internals, function(name) {
    if (is.null(lmtp_namespace)) {
      return(NA_character_)
    }
    render(get0(name, envir = lmtp_namespace, inherits = TRUE))
  })
  names(lmtp_sources) <- lmtp_internals
  learner_packages <- c(
    "SuperLearner", "ranger", "xgboost", "glmnet", "future", "progressr", "digest"
  )
  learner_versions <- vapply(
    learner_packages,
    function(package) {
      if (requireNamespace(package, quietly = TRUE)) {
        as.character(utils::packageVersion(package))
      } else {
        NA_character_
      }
    },
    character(1)
  )
  list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    host = unname(Sys.info()[["nodename"]]),
    process_id = Sys.getpid(),
    lmtp_version = as.character(utils::packageVersion("lmtp")),
    margot_version = as.character(utils::packageVersion("margot")),
    learner_versions = learner_versions,
    source_fingerprint = digest::digest(
      list(margot = margot_sources, lmtp = lmtp_sources),
      algo = "sha256",
      serialize = TRUE,
      serializeVersion = 2L
    )
  )
}

# fingerprint the code that produces a task result: the worker-resolvable
# internals plus the two stage bodies, which travel with every future
margot_lmtp_code_fingerprint <- function() {
  control <- c("keepInteger", "keepNA")
  report <- margot_lmtp_worker_report()
  margot_lmtp_task_fingerprint(list(
    environment = report$source_fingerprint,
    # a learner package can change the fit while margot and lmtp stand still, so
    # its version belongs in every identity a checkpoint is keyed by
    learner_versions = report$learner_versions,
    density_stage = paste(
      deparse(body(margot_lmtp_density_stage), control = control),
      collapse = "\n"
    ),
    outcome_stage = paste(
      deparse(body(margot_lmtp_outcome_stage), control = control),
      collapse = "\n"
    )
  ))
}

# stage 1: fit one policy-specific density-ratio process inside a worker
margot_lmtp_density_stage <- function(payload) {
  started_at <- Sys.time()
  # inner cross-fitting folds stay sequential in this slice; nested fleet mode is deferred
  previous_plan <- future::plan(future::sequential)
  on.exit(future::plan(previous_plan, substitute = FALSE), add = TRUE)
  # the coordinator's captured state, not set.seed(): an outer future seeded with
  # future.seed = TRUE leaves the worker on L'Ecuyer-CMRG, and seeding again here
  # would seed the wrong generator
  assign(".Random.seed", payload$seed_state, envir = .GlobalEnv)

  args <- payload$args
  source_task <- margot_lmtp_make_task(
    data = args$data,
    trt = args$trt,
    outcome = args$outcomes[[1L]],
    baseline = args$baseline,
    time_vary = args$time_vary,
    cens = args$cens,
    compete = args$compete,
    shift = args$shift,
    # the coordinator realised this policy once; the worker reuses those values
    # rather than running the user's closure again
    shifted = payload$shifted,
    k = args$k,
    mtp = args$mtp,
    outcome_type = args$outcome_type,
    id = args$id,
    bounds = args$bounds,
    folds = args$folds,
    weights = args$weights
  )
  # a worker-local progressor is a no-op; the coordinator alone renders progress
  progress_bar <- progressr::progressor(
    steps = source_task$time_horizon * args$folds * (length(args$outcomes) + 1L)
  )
  density_fit <- margot_lmtp_fit_density_ratios(
    task = source_task,
    learners_trt = args$learners_trt,
    mtp = args$mtp,
    control = args$control,
    progress_bar = progress_bar
  )
  post_ratio_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  fingerprint <- function(x) {
    digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 2L)
  }
  # the result fingerprint covers the whole immutable Stage 1 result that Stage 2
  # and the reconstructed lmtp model consume, not the ratios alone
  result_fingerprint <- fingerprint(list(
    density_ratios = density_fit$density_ratios,
    fits = density_fit$fits,
    folds = source_task$folds,
    post_ratio_seed = post_ratio_seed
  ))
  list(
    stage = "density",
    shift_name = payload$shift_name,
    identity = payload$identity,
    success = TRUE,
    density_source = "fit",
    density_fit = density_fit,
    source_task = source_task,
    folds = source_task$folds,
    folds_fingerprint = fingerprint(source_task$folds),
    post_ratio_seed = post_ratio_seed,
    result_fingerprint = result_fingerprint,
    fits_fingerprint = fingerprint(density_fit$fits),
    lmtp_version = as.character(utils::packageVersion("lmtp")),
    margot_version = as.character(utils::packageVersion("margot")),
    code_fingerprint = payload$code_fingerprint,
    host = unname(Sys.info()[["nodename"]]),
    process_id = Sys.getpid(),
    started_at = started_at,
    elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  )
}

# stage 2: fit one outcome regression from an immutable density result. The
# coordinator has already built and verified the task; this worker re-verifies
# the transported objects and then does nothing between restoring the recorded
# random-number state and fitting
margot_lmtp_outcome_stage <- function(payload) {
  started_at <- Sys.time()
  previous_plan <- future::plan(future::sequential)
  on.exit(future::plan(previous_plan, substitute = FALSE), add = TRUE)
  fingerprint <- function(x) {
    digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 2L)
  }

  task <- payload$task
  problems <- character()
  realised_result <- fingerprint(list(
    density_ratios = payload$density_ratios,
    fits = payload$density_fits,
    folds = payload$folds,
    post_ratio_seed = payload$post_ratio_seed
  ))
  if (!identical(realised_result, payload$result_fingerprint)) {
    problems <- c(problems, "policy-specific density-ratio result")
  }
  if (!identical(fingerprint(payload$folds), payload$folds_fingerprint)) {
    problems <- c(problems, "cross-fitting fold map")
  }
  if (!identical(task$folds, payload$folds)) {
    problems <- c(problems, "fold map carried by the outcome task")
  }
  realised_task <- fingerprint(list(
    result_fingerprint = payload$result_fingerprint,
    outcome = payload$outcome,
    id = as.character(task$id),
    weights = task$weights,
    outcome_values = task$natural[[payload$outcome]],
    time_horizon = task$time_horizon,
    learners_outcome = payload$learners_outcome,
    control = payload$control,
    folds_fingerprint = payload$folds_fingerprint,
    estimator_spec_hash = payload$estimator_spec_hash
  ))
  if (!identical(realised_task, payload$task_fingerprint)) {
    problems <- c(problems, "outcome task identity")
  }
  if (length(problems) > 0L) {
    cli::cli_abort(
      c(
        "Outcome {.val {payload$outcome}} cannot reuse the common density-ratio fit.",
        "x" = "Mismatched: {problems}."
      ),
      class = "margot_error_density_ratio_identity"
    )
  }

  progress_bar <- progressr::progressor(steps = payload$progress_steps)
  margot_lmtp_restore_seed(payload$post_ratio_seed)
  model <- margot_lmtp_fit_sdr_outcome(
    task = task,
    density_fit = list(
      density_ratios = payload$density_ratios,
      fits = payload$density_fits
    ),
    learners_outcome = payload$learners_outcome,
    control = payload$control,
    progress_bar = progress_bar,
    shift_label = payload$shift_label
  )
  attr(model, "margot_density_ratio_source") <- payload$source_outcome
  attr(model, "margot_density_ratio_reused") <-
    !identical(payload$outcome, payload$source_outcome)

  list(
    stage = "outcome",
    outcome = payload$outcome,
    shift_name = payload$shift_name,
    identity = payload$identity,
    task_fingerprint = payload$task_fingerprint,
    result_fingerprint = payload$result_fingerprint,
    success = TRUE,
    model = model,
    host = unname(Sys.info()[["nodename"]]),
    process_id = Sys.getpid(),
    started_at = started_at,
    elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  )
}

# fill the shared-fit call arguments the sequential route would resolve from its
# own formals, so both routes fit from one identical argument set
margot_lmtp_task_resolve_args <- function(shared_args, call = rlang::caller_env()) {
  formal_arguments <- formals(margot_lmtp_sdr_shared)
  unknown <- setdiff(names(shared_args), names(formal_arguments))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "The shared density-ratio fit received unknown argument{?s}.",
        "x" = "Unused: {.arg {unknown}}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  resolved <- lapply(names(formal_arguments), function(name) {
    if (name %in% names(shared_args)) {
      return(shared_args[[name]])
    }
    default <- formal_arguments[[name]]
    if (identical(default, quote(expr = ))) {
      return(NULL)
    }
    eval(default, envir = asNamespace("margot"))
  })
  names(resolved) <- names(formal_arguments)
  resolved$outcome_type <- match.arg(
    resolved$outcome_type,
    c("binomial", "continuous", "survival")
  )
  resolved$shifted <- NULL
  resolved
}

# replicate the sequential route's input validation on the coordinator
margot_lmtp_task_preflight <- function(args, call = rlang::caller_env()) {
  margot_lmtp_internal("LmtpTask", call = call)
  outcomes <- args$outcomes
  if (!is.character(outcomes) || length(outcomes) < 1L || anyNA(outcomes) ||
      any(!nzchar(outcomes))) {
    cli::cli_abort(
      "{.arg outcomes} must contain at least one outcome column name.",
      class = "margot_error_invalid_input",
      call = call
    )
  }
  if (identical(args$outcome_type, "survival") || !is.null(args$compete)) {
    cli::cli_abort(
      "Density-ratio reuse currently supports continuous and binomial SDR outcomes without competing events.",
      class = "margot_error_unsupported_estimator",
      call = call
    )
  }
  assert_not_data_table <- margot_lmtp_internal("assert_not_data_table", call = call)
  assert_outcome_types <- margot_lmtp_internal("assert_outcome_types", call = call)
  assert_subset <- margot_lmtp_internal("assert_subset", call = call)
  assert_numeric <- margot_lmtp_internal("assert_numeric", call = call)
  check_trt_type <- margot_lmtp_internal("check_trt_type", call = call)
  assert_not_data_table(args$data)
  required <- c(
    unlist(args$trt), outcomes, unlist(args$time_vary), args$baseline,
    args$cens, args$compete, args$id
  )
  assert_subset(required, names(args$data))
  for (outcome in outcomes) {
    assert_outcome_types(args$data, outcome, args$outcome_type)
  }
  assert_numeric(
    args$bounds,
    len = 2, unique = TRUE, sorted = TRUE, finite = TRUE, null.ok = TRUE
  )
  check_trt_type(args$data, unlist(args$trt), args$mtp)
  invisible(TRUE)
}

# the variable set one lmtp task is built from, in the order
# margot_lmtp_make_task() assembles it
margot_lmtp_task_variables <- function(args, outcome) {
  c(
    unlist(args$trt), outcome, unlist(args$time_vary),
    args$baseline, args$cens, args$compete, args$id
  )
}

# realise one policy's shifted nuisance inputs exactly once. The realised frame
# is transported to the density worker and reused for every outcome task, so the
# user's shift closure runs once per policy for the whole call; its trt and
# censoring columns enter the identity, so two closures with one deparsed body
# but different captured bindings receive different identities. A realisation
# failure is a preflight failure, never an identity value, and a shift that draws
# random numbers is reported so the coordinator can refuse it
margot_lmtp_realise_shift <- function(args, shift_name, call = rlang::caller_env()) {
  if (is.null(args$shift)) {
    return(list(frame = NULL, values = NULL, consumed_rng = FALSE))
  }
  before <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  frame <- tryCatch(
    margot_lmtp_internal("make_shifted")(
      args$data[, margot_lmtp_task_variables(args, args$outcomes[[1L]])],
      args$trt, args$cens, args$shift, NULL
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Margot could not apply the policy {.val {shift_name}} to the analysis data.",
          "x" = "{conditionMessage(e)}"
        ),
        class = "margot_error_shift_realisation_failed",
        parent = e,
        call = call
      )
    }
  )
  after <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  values <- NULL
  if (!is.null(frame)) {
    keep <- intersect(c(unlist(args$trt), args$cens), names(frame))
    values <- as.data.frame(frame)[, keep, drop = FALSE]
  }
  list(frame = frame, values = values, consumed_rng = !identical(before, after))
}

# derive one outcome's shifted frame from the single realised frame. A policy
# alters the exposure and censoring columns alone, so the outcome column is
# carried over from the analysis data; the policy is therefore never applied a
# second time. A policy that read a terminal outcome would not be reproduced
# here, and such a policy already contradicts the fit-once ratio identity
margot_lmtp_shifted_for_outcome <- function(realised, args, outcome) {
  if (is.null(realised)) {
    return(NULL)
  }
  source_outcome <- args$outcomes[[1L]]
  frame <- realised
  if (!identical(outcome, source_outcome)) {
    frame[[source_outcome]] <- NULL
    frame[[outcome]] <- args$data[[outcome]]
  }
  frame[, margot_lmtp_task_variables(args, outcome), drop = FALSE]
}

# fingerprint the result-affecting inputs of one policy-specific density task;
# terminal-outcome values are deliberately excluded, so perturbing an outcome
# leaves the identity unchanged
margot_lmtp_density_identity <- function(args, shift_name, shift_label,
                                         realised_shift, seed_state, rng_kind,
                                         code_fingerprint,
                                         estimator_spec_hash = NULL) {
  shared_variables <- unique(c(
    unlist(args$trt), unlist(args$time_vary), args$baseline, args$cens,
    args$compete, args$id
  ))
  shared_variables <- intersect(shared_variables, names(args$data))
  margot_lmtp_task_fingerprint(list(
    estimator = "lmtp::lmtp_sdr",
    trt = args$trt,
    outcomes = args$outcomes,
    shift_name = shift_name,
    shift = shift_label,
    realised_shift = realised_shift,
    mtp = args$mtp,
    k = args$k,
    outcome_type = args$outcome_type,
    id = args$id,
    bounds = args$bounds,
    folds = args$folds,
    learners_trt = args$learners_trt,
    control = args$control,
    weights = args$weights,
    # the full state the fit starts from, not the bare seed: the same integer
    # produces different draws under a different generator
    seed_state = seed_state,
    rng_kind = rng_kind,
    shared_data = args$data[, shared_variables, drop = FALSE],
    lmtp_version = margot_lmtp_shared_lmtp_version,
    margot_version = as.character(utils::packageVersion("margot")),
    code_fingerprint = code_fingerprint,
    estimator_spec_hash = estimator_spec_hash
  ))
}

# fingerprint one outcome task over everything its worker will consume, so the
# worker can recompute it and refuse a payload altered in transit
margot_lmtp_outcome_task_fingerprint <- function(task, outcome, result_fingerprint,
                                                 folds_fingerprint, learners_outcome,
                                                 control, estimator_spec_hash = NULL) {
  margot_lmtp_task_fingerprint(list(
    result_fingerprint = result_fingerprint,
    outcome = outcome,
    id = as.character(task$id),
    weights = task$weights,
    outcome_values = task$natural[[outcome]],
    time_horizon = task$time_horizon,
    learners_outcome = learners_outcome,
    control = control,
    folds_fingerprint = folds_fingerprint,
    estimator_spec_hash = estimator_spec_hash
  ))
}

# read one sysctl core count, returning NA when the query is unavailable or
# returns anything but a single positive whole number
margot_lmtp_sysctl_cores <- function(name) {
  reported <- tryCatch(
    suppressWarnings(system2("sysctl", c("-n", name), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (!is.character(reported) || length(reported) < 1L) {
    return(NA_integer_)
  }
  value <- suppressWarnings(as.integer(trimws(reported[[1L]])))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    return(NA_integer_)
  }
  value
}

# count the fast cores a local task pool may use; Apple Silicon reports its
# efficiency cores alongside its performance cores, and an efficiency core must
# not count toward a pool whose tasks each assume one performance core
margot_lmtp_performance_cores <- function() {
  fallback <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (!is.numeric(fallback) || !is.finite(fallback) || fallback < 1) {
    fallback <- 1L
  }
  fallback <- as.integer(fallback)
  darwin <- identical(unname(Sys.info()[["sysname"]]), "Darwin")
  apple_silicon <- grepl("^(arm|aarch)", R.version$arch)
  if (!darwin || !apple_silicon) {
    return(fallback)
  }
  # perflevel0 is the performance cluster; perflevel1 is the efficiency cluster
  reported <- margot_lmtp_sysctl_cores("hw.perflevel0.logicalcpu")
  if (is.na(reported)) {
    return(fallback)
  }
  reported
}

# size a coordinator-owned local pool. An explicit models_in_parallel comes from
# a caller who knows their machine and wins outright; otherwise the derived size
# is capped at the performance cores available
margot_lmtp_task_pool_size <- function(models_in_parallel, n_cores, task_budget,
                                       models_in_parallel_supplied = FALSE) {
  size <- models_in_parallel %||% n_cores %||% 1L
  if (!is.numeric(size) || !is.finite(size)) {
    size <- 1L
  }
  size <- max(1L, min(as.integer(size), max(1L, as.integer(task_budget))))
  if (!isTRUE(models_in_parallel_supplied)) {
    size <- max(1L, min(size, margot_lmtp_performance_cores()))
  }
  size
}

# choose the worker pool: schedule over an explicit caller pool and never alter
# it, otherwise open a local multisession pool the coordinator owns
margot_lmtp_resolve_task_pool <- function(models_in_parallel, n_cores, task_budget,
                                          models_in_parallel_supplied = FALSE) {
  plan_list <- tryCatch(future::plan("list"), error = function(e) NULL)
  workers <- tryCatch(future::nbrOfWorkers(), error = function(e) 1L)
  if (!is.numeric(workers) || !is.finite(workers)) {
    workers <- 1L
  }
  workers <- as.integer(workers)
  caller_pool <- !is.null(plan_list) && inherits(plan_list[[1L]], "cluster") && workers >= 2L
  if (caller_pool) {
    return(list(owned = FALSE, workers = workers, kind = class(plan_list[[1L]])[1L]))
  }
  size <- margot_lmtp_task_pool_size(
    models_in_parallel = models_in_parallel,
    n_cores = n_cores,
    task_budget = task_budget,
    models_in_parallel_supplied = models_in_parallel_supplied
  )
  if (size < 2L) {
    # a single-worker budget schedules the same graph in this process
    return(list(owned = FALSE, workers = 1L, kind = "sequential"))
  }
  future::plan(future::multisession, workers = size)
  list(owned = TRUE, workers = size, kind = "multisession")
}

# return the cluster object behind the current plan when the backend exposes it,
# so every node can be probed exactly once
margot_lmtp_plan_cluster <- function() {
  strategy <- tryCatch(future::plan("list")[[1L]], error = function(e) NULL)
  if (is.null(strategy)) {
    return(NULL)
  }
  backend <- attr(strategy, "backend")
  cluster <- NULL
  if (!is.null(backend)) {
    cluster <- tryCatch(backend[["workers"]], error = function(e) NULL)
  }
  if (!inherits(cluster, "cluster")) {
    cluster <- tryCatch(eval(formals(strategy)$workers), error = function(e) NULL)
  }
  if (!inherits(cluster, "cluster")) {
    return(NULL)
  }
  cluster
}

# refuse a fleet whose workers do not share the coordinator's result-affecting
# environment. Where the backend exposes its cluster, every node is called
# exactly once; otherwise membership is established from distinct host and
# process identifiers, and a pool that cannot show complete membership is refused
margot_lmtp_probe_workers <- function(pool, register = NULL, waves = 5L,
                                      deadline_seconds = 30, call = rlang::caller_env()) {
  if (pool$workers < 2L) {
    return(invisible(NULL))
  }
  expected <- margot_lmtp_worker_report()
  report_worker <- margot_lmtp_worker_report
  cluster <- margot_lmtp_plan_cluster()
  if (inherits(cluster, "cluster") && length(cluster) >= pool$workers) {
    reports <- parallel::clusterCall(cluster, report_worker)
  } else {
    # a future backend does not promise one probe per worker, so membership is
    # collected over bounded repeated waves before a healthy pool is refused
    reports <- list()
    members <- character()
    deadline <- Sys.time() + deadline_seconds
    expired <- FALSE
    for (wave in seq_len(waves)) {
      probes <- lapply(seq_len(pool$workers), function(index) {
        probe <- future::future(
          report_worker(),
          seed = TRUE,
          globals = list(report_worker = report_worker)
        )
        if (is.function(register)) {
          register(paste0("probe-", wave, "-", index), probe)
        }
        probe
      })
      for (index in seq_along(probes)) {
        # the deadline binds while a probe is collected, not only between waves;
        # an unresponsive worker cannot hold the coordinator indefinitely
        while (!future::resolved(probes[[index]]) && Sys.time() < deadline) {
          Sys.sleep(0.05)
        }
        if (!future::resolved(probes[[index]])) {
          expired <- TRUE
          break
        }
        report <- future::value(probes[[index]])
        if (is.function(register)) {
          register(paste0("probe-", wave, "-", index), NULL)
        }
        member <- paste(report$host, report$process_id, sep = "|")
        if (!member %in% members) {
          members <- c(members, member)
          reports[[length(reports) + 1L]] <- report
        }
      }
      if (expired || length(members) >= pool$workers || Sys.time() > deadline) {
        break
      }
    }
    # outstanding probes stay in the coordinator's registry, which cancels and
    # drains them as the refusal below unwinds
    if (expired || length(members) < pool$workers) {
      cli::cli_abort(
        c(
          "Margot could not establish the complete membership of the worker pool within its discovery deadline.",
          "x" = "{pool$workers} worker{?s} configured; {length(members)} distinct worker{?s} answered within {waves} probe wave{?s} and {deadline_seconds} second{?s}.",
          "i" = "Use a plan whose backend exposes its cluster, so every node can be probed exactly once."
        ),
        class = "margot_error_worker_ineligible",
        call = call
      )
    }
  }

  fields <- c(
    "r_version", "platform", "lmtp_version", "margot_version",
    "learner_versions", "source_fingerprint"
  )
  for (report in reports) {
    if (!identical(report$lmtp_version, margot_lmtp_shared_lmtp_version)) {
      cli::cli_abort(
        c(
          "A worker does not carry the validated {.pkg lmtp} version.",
          "x" = "Worker {report$host}:{report$process_id} reports {.val {report$lmtp_version}}; validated {.val {margot_lmtp_shared_lmtp_version}}."
        ),
        class = "margot_error_worker_ineligible",
        call = call
      )
    }
    mismatched <- fields[vapply(
      fields,
      function(field) !identical(report[[field]], expected[[field]]),
      logical(1)
    )]
    if (length(mismatched) > 0L) {
      cli::cli_abort(
        c(
          "A worker does not share this session's result-affecting environment.",
          "x" = "Worker {report$host}:{report$process_id} differs in: {mismatched}.",
          "i" = "Install the current {.pkg margot}, {.pkg lmtp}, and learner packages on every worker before scheduling tasks."
        ),
        class = "margot_error_worker_ineligible",
        call = call
      )
    }
  }
  invisible(reports)
}

# re-raise a worker or coordinator failure, keeping any Margot error class and
# attaching the complete task ledger so the failure remains inspectable
margot_lmtp_task_abort <- function(condition, records, stage, label,
                                   call = rlang::caller_env()) {
  margot_classes <- grep("^margot_error_", class(condition), value = TRUE)
  cli::cli_abort(
    c(
      "The {stage} task for {.val {label}} failed; the shared route stops rather than refitting independently.",
      "x" = "{conditionMessage(condition)}"
    ),
    class = c(margot_classes, "margot_error_task_worker_failure"),
    parent = condition,
    margot_task_records = records,
    margot_task_stage = stage,
    call = call
  )
}

# install one .rds file atomically. An existing destination is verified rather
# than overwritten, so a duplicate completion reuses identical content and a
# mismatch refuses
margot_lmtp_write_rds_atomic <- function(object, path, compare = NULL,
                                         call = rlang::caller_env()) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  # a duplicate completion differs in its write timestamp alone, so the caller
  # may name the fields that decide whether two writes carry the same result
  comparable <- function(x) {
    if (is.null(compare)) {
      return(margot_lmtp_task_fingerprint(x))
    }
    margot_lmtp_task_fingerprint(x[compare])
  }
  if (file.exists(path)) {
    stored <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(stored) && identical(comparable(stored), comparable(object))) {
      return(path)
    }
    cli::cli_abort(
      c(
        "An existing checkpoint does not match the result Margot is writing.",
        "x" = "{.file {basename(path)}} already exists with different content.",
        "i" = "Remove or move the earlier checkpoint before rerunning."
      ),
      class = "margot_error_checkpoint_conflict",
      call = call
    )
  }
  scratch <- tempfile(pattern = "margot-checkpoint-", tmpdir = directory, fileext = ".rds")
  # no scratch file survives a serialisation, disk, or rename failure
  on.exit(unlink(scratch), add = TRUE)
  saveRDS(object, file = scratch, compress = TRUE)
  written <- tryCatch(readRDS(scratch), error = function(e) NULL)
  if (is.null(written) || !identical(comparable(written), comparable(object))) {
    cli::cli_abort(
      c(
        "Margot could not serialise the checkpoint {.file {basename(path)}}.",
        "x" = "The written file did not read back as the object Margot proposed."
      ),
      class = "margot_error_checkpoint_conflict",
      call = call
    )
  }
  if (!file.rename(scratch, path)) {
    cli::cli_abort(
      "Margot could not install the checkpoint {.file {basename(path)}}.",
      class = "margot_error_checkpoint_conflict",
      call = call
    )
  }
  # rename can replace a destination another writer installed after the check
  # above, so the installed file is read back and compared; an exact match is a
  # duplicate completion, and any difference is a conflict
  installed <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(installed) || !identical(comparable(installed), comparable(object))) {
    cli::cli_abort(
      c(
        "An installed checkpoint does not match the result Margot wrote.",
        "x" = "{.file {basename(path)}} changed while Margot was installing it.",
        "i" = "Another process is writing the same checkpoint; rerun once it has finished."
      ),
      class = "margot_error_checkpoint_conflict",
      call = call
    )
  }
  path
}

# name one immutable density checkpoint by its identity and full result
# fingerprint; the identity leads so a restart can find it before reading it
margot_lmtp_density_checkpoint_path <- function(directory, identity, result_fingerprint) {
  file.path(directory, paste0("density_", identity, "_", result_fingerprint, ".rds"))
}

# store the complete Stage 1 payload that Stage 2 and a later restart require
margot_lmtp_write_density_checkpoint <- function(record, directory,
                                                 call = rlang::caller_env()) {
  payload <- list(
    schema = "margot_density_checkpoint_1",
    shift_name = record$shift_name,
    identity = record$identity,
    result_fingerprint = record$result_fingerprint,
    fits_fingerprint = record$fits_fingerprint,
    folds_fingerprint = record$folds_fingerprint,
    code_fingerprint = record$code_fingerprint,
    # a checkpoint carries the density result alone; the terminal-outcome tasks
    # of a later analysis must be built from that call's own data
    density_fit = record$density_fit,
    folds = record$folds,
    post_ratio_seed = record$post_ratio_seed,
    lmtp_version = record$lmtp_version,
    margot_version = record$margot_version,
    timestamp = Sys.time()
  )
  margot_lmtp_write_rds_atomic(
    payload,
    margot_lmtp_density_checkpoint_path(directory, record$identity, record$result_fingerprint),
    compare = c(
      "schema", "shift_name", "identity", "result_fingerprint",
      "fits_fingerprint", "folds_fingerprint", "code_fingerprint"
    ),
    call = call
  )
}

# recover one verified density result. A missing checkpoint returns NULL and the
# policy is fitted; a corrupt or mismatched checkpoint refuses rather than being
# silently refitted over
margot_lmtp_read_density_checkpoint <- function(directory, identity, shift_name,
                                                call = rlang::caller_env()) {
  if (is.null(directory) || !dir.exists(directory)) {
    return(NULL)
  }
  candidates <- list.files(
    directory,
    pattern = paste0("^density_", identity, "_[0-9a-f]+\\.rds$"),
    full.names = TRUE
  )
  if (length(candidates) < 1L) {
    return(NULL)
  }
  # a complete identity is deterministic, so one identity has one result; several
  # distinct results mean something result-affecting escaped the identity
  fingerprints <- unique(sub("\\.rds$", "", sub("^density_[0-9a-f]+_", "", basename(candidates))))
  if (length(fingerprints) > 1L) {
    cli::cli_abort(
      c(
        "Policy {.val {shift_name}} has more than one stored density result under one identity.",
        "x" = "{length(fingerprints)} distinct result fingerprints share the identity {.val {substr(identity, 1L, 12L)}}.",
        "i" = "Remove the stale files from the density checkpoint directory; a complete identity must determine one result."
      ),
      class = "margot_error_density_checkpoint_conflict",
      call = call
    )
  }
  path <- sort(candidates)[[1L]]
  stored <- tryCatch(readRDS(path), error = function(e) NULL)
  invalid <- function(reason) {
    cli::cli_abort(
      c(
        "The density checkpoint for policy {.val {shift_name}} cannot be reused.",
        "x" = "{reason}",
        "i" = "Remove {.file {basename(path)}} to refit the policy-specific density ratios."
      ),
      class = "margot_error_density_checkpoint_invalid",
      call = call
    )
  }
  if (is.null(stored) || !is.list(stored) ||
      !identical(stored$schema, "margot_density_checkpoint_1")) {
    invalid("The stored object is unreadable or was not written by Margot.")
  }
  if (!identical(stored$identity, identity)) {
    invalid("The stored ratio-fit identity differs from the identity requested.")
  }
  realised <- margot_lmtp_task_fingerprint(list(
    density_ratios = stored$density_fit$density_ratios,
    fits = stored$density_fit$fits,
    folds = stored$folds,
    post_ratio_seed = stored$post_ratio_seed
  ))
  if (!identical(realised, stored$result_fingerprint)) {
    invalid("The stored result does not match its recorded fingerprint.")
  }
  if (!identical(margot_lmtp_task_fingerprint(stored$folds), stored$folds_fingerprint)) {
    invalid("The stored fold map does not match its recorded fingerprint.")
  }
  list(
    stage = "density",
    shift_name = shift_name,
    identity = identity,
    success = TRUE,
    density_source = "checkpoint",
    density_fit = stored$density_fit,
    folds = stored$folds,
    folds_fingerprint = stored$folds_fingerprint,
    post_ratio_seed = stored$post_ratio_seed,
    result_fingerprint = stored$result_fingerprint,
    fits_fingerprint = stored$fits_fingerprint,
    code_fingerprint = stored$code_fingerprint,
    lmtp_version = stored$lmtp_version,
    margot_version = stored$margot_version,
    checkpoint_path = path,
    host = unname(Sys.info()[["nodename"]]),
    process_id = Sys.getpid(),
    started_at = stored$timestamp,
    elapsed_seconds = 0
  )
}

# strip the transported objects from a record before it enters a ledger or the
# public reuse attribute
margot_lmtp_task_record_summary <- function(record) {
  record[setdiff(
    names(record),
    c("density_fit", "source_task", "folds", "post_ratio_seed", "model")
  )]
}

# name one outcome checkpoint by the fingerprint of the task that produced it, so
# a later call finds it by identity rather than by outcome and policy names
margot_lmtp_outcome_checkpoint_path <- function(directory, task_fingerprint) {
  file.path(directory, paste0("outcome_", task_fingerprint, ".rds"))
}

# store one fitted outcome model once. The fingerprint-keyed file is the single
# copy; the per-run checkpoint name is a hard link to it where the filesystem
# allows one, so margot_lmtp_restore_checkpoints() keeps working on the run
# directory without a second multi-hundred-megabyte write
margot_lmtp_write_outcome_checkpoint <- function(model, outcome, shift_name,
                                                 task_fingerprint, directory,
                                                 run_path = NULL,
                                                 call = rlang::caller_env()) {
  payload <- list(
    schema = "margot_outcome_checkpoint_1",
    model = model,
    outcome = outcome,
    shift_name = shift_name,
    task_fingerprint = task_fingerprint,
    # provenance for the written object. An S7 estimate does not re-digest
    # identically after a serialisation round trip, so reuse is verified by the
    # task fingerprint rather than by comparing this value to a restored model
    model_fingerprint = margot_lmtp_task_fingerprint(model),
    timestamp = Sys.time()
  )
  # the task fingerprint in the filename is the identity; two independent fits of
  # one identity carry captured environments that do not digest alike, so the
  # stored model fingerprint is provenance rather than an equality predicate
  path <- margot_lmtp_write_rds_atomic(
    payload,
    margot_lmtp_outcome_checkpoint_path(directory, task_fingerprint),
    compare = c("schema", "task_fingerprint", "outcome", "shift_name"),
    call = call
  )
  list(
    path = path,
    run_path = run_path,
    linked = margot_lmtp_link_run_checkpoint(path, run_path)
  )
}

# give the run directory its name-keyed entry without a second copy of the model
margot_lmtp_link_run_checkpoint <- function(path, run_path) {
  if (is.null(run_path) || file.exists(run_path)) {
    return(NA)
  }
  dir.create(dirname(run_path), recursive = TRUE, showWarnings = FALSE)
  linked <- isTRUE(suppressWarnings(file.link(path, run_path)))
  if (!linked) {
    # a filesystem without hard links keeps its own copy rather than losing the
    # run directory's restoration contract
    file.copy(path, run_path, overwrite = FALSE)
  }
  linked
}

# recover one verified outcome model. A missing checkpoint returns NULL and the
# outcome is fitted; a stored record whose identity fields disagree with the task
# refuses rather than being reused
margot_lmtp_read_outcome_checkpoint <- function(directory, task_fingerprint, outcome,
                                                shift_name, call = rlang::caller_env()) {
  if (is.null(directory) || !dir.exists(directory)) {
    return(NULL)
  }
  path <- margot_lmtp_outcome_checkpoint_path(directory, task_fingerprint)
  if (!file.exists(path)) {
    return(NULL)
  }
  stored <- tryCatch(readRDS(path), error = function(e) NULL)
  invalid <- function(reason) {
    cli::cli_abort(
      c(
        "The stored outcome model for {.val {outcome}} - {.val {shift_name}} cannot be reused.",
        "x" = "{reason}",
        "i" = "Remove {.file {basename(path)}} to refit this outcome."
      ),
      class = "margot_error_outcome_checkpoint_invalid",
      call = call
    )
  }
  if (is.null(stored) || !is.list(stored) ||
      !identical(stored$schema, "margot_outcome_checkpoint_1")) {
    invalid("The stored object is unreadable or was not written by Margot.")
  }
  if (!identical(stored$task_fingerprint, task_fingerprint) ||
      !identical(stored$outcome, outcome) ||
      !identical(stored$shift_name, shift_name)) {
    invalid("The stored task identity differs from the task requested.")
  }
  if (is.null(stored$model)) {
    invalid("The stored record carries no model.")
  }
  list(model = stored$model, checkpoint_path = path, timestamp = stored$timestamp)
}

# assemble the density-stage return object. Every diagnostic comes from Margot's
# existing positivity machinery, given a models-shaped view of the density
# ratios; no threshold and no pass-or-fail verdict is decided here
margot_lmtp_density_stage_result <- function(density_records, outcome_vars, policies,
                                             ledger, density_checkpoint_dir,
                                             worker_count) {
  density_ratios <- lapply(density_records, function(record) record$density_fit$density_ratios)
  names(density_ratios) <- policies
  source_outcome <- outcome_vars[[1L]]
  view <- list(models = list())
  view$models[[source_outcome]] <- lapply(density_ratios, function(ratios) {
    list(density_ratios = ratios)
  })
  names(view$models[[source_outcome]]) <- paste0(source_outcome, "_", policies)
  attempt <- function(expr) {
    tryCatch(expr, error = function(e) list(unavailable = conditionMessage(e)))
  }
  diagnostics <- list(
    positivity = attempt(margot_lmtp_positivity(view, verbose = FALSE)),
    overlap = attempt(margot_lmtp_overlap(view, plot = FALSE, verbose = FALSE))
  )
  structure(
    list(
      stage = "density",
      density_ratios = density_ratios,
      diagnostics = diagnostics,
      records = lapply(density_records, margot_lmtp_task_record_summary),
      task_records = ledger,
      identities = vapply(density_records, function(record) record$identity, character(1)),
      result_fingerprints = vapply(
        density_records, function(record) record$result_fingerprint, character(1)
      ),
      outcomes = outcome_vars,
      policies = policies,
      checkpoint_dir = density_checkpoint_dir,
      worker_count = worker_count,
      lmtp_version = margot_lmtp_shared_lmtp_version
    ),
    class = c("margot_lmtp_density_stage", "list")
  )
}

#' Print a Margot LMTP density-stage result
#'
#' @param x A `margot_lmtp_density_stage` object, returned by [margot_lmtp()]
#'   with `stages = "density"`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.margot_lmtp_density_stage <- function(x, ...) {
  cli::cli_h2("Margot LMTP density stage")
  cli::cli_text("Policies: {.val {x$policies}}")
  cli::cli_text("Terminal outcomes awaiting their outcome stage: {.val {x$outcomes}}")
  cli::cli_text("Density-ratio results recorded: {length(x$density_ratios)}")
  if (!is.null(x$checkpoint_dir)) {
    cli::cli_text("Density checkpoints: {.path {x$checkpoint_dir}}")
  }
  if (is.data.frame(x$diagnostics$positivity$overall)) {
    cli::cli_text("Positivity summary rows: {nrow(x$diagnostics$positivity$overall)}")
  }
  cli::cli_alert_info(
    "Assess overlap against your registered thresholds, then run the outcome stage with {.code stages = \"outcome\"}."
  )
  invisible(x)
}

# schedule the shared density-ratio path as an explicit two-stage task graph
margot_lmtp_run_shared_tasks_task <- function(
    data,
    outcome_vars,
    trt,
    shift_functions,
    lmtp_defaults,
    mtp_by_arm,
    seed,
    save_output,
    checkpoint_dir,
    progress = c("cli", "progressr", "none"),
    n_cores = NULL,
    models_in_parallel = NULL,
    models_in_parallel_supplied = FALSE,
    cv_workers = NULL,
    density_checkpoint_dir = NULL,
    outcome_checkpoint_dir = NULL,
    estimator_spec_hash = NULL,
    stages = c("all", "density", "outcome"),
    call = rlang::caller_env()) {
  progress <- match.arg(progress)
  stages <- match.arg(stages)
  policies <- names(shift_functions)
  policy_count <- length(policies)
  outcome_count <- length(outcome_vars)
  total_tasks <- outcome_count * policy_count

  # nested fleet mode is deferred: every task keeps its inner folds sequential
  if (!is.null(cv_workers) && is.numeric(cv_workers) && length(cv_workers) == 1L &&
      is.finite(cv_workers) && as.integer(cv_workers) > 1L) {
    cli::cli_abort(
      c(
        "Task-parallel density-ratio reuse keeps every task's cross-fitting folds sequential.",
        "x" = "Received {.arg cv_workers} = {.val {as.integer(cv_workers)}}.",
        "i" = "Use {.code cv_workers = 1} here; nested worker pools remain deferred."
      ),
      class = "margot_error_nested_parallel_unsupported",
      call = call
    )
  }
  # concurrent policies cannot reproduce the sequential route's policy-to-policy
  # random-number chain, so this mode requires an explicit seed
  if (is.null(seed)) {
    cli::cli_abort(
      c(
        "Task-parallel density-ratio reuse requires an explicit seed.",
        "x" = "With {.arg seed = NULL} each policy would continue the previous policy's random-number state, which concurrent policies cannot reproduce.",
        "i" = "Supply {.arg seed}, or an {.arg estimator_spec}, which always carries one."
      ),
      class = "margot_error_task_seed_required",
      call = call
    )
  }

  # every state transition enters one ledger, which travels with any failure
  ledger <- new.env(parent = emptyenv())
  ledger$entries <- list()
  note <- function(key, entry) {
    ledger$entries[[key]] <- entry
    invisible(entry)
  }
  ledger_snapshot <- function() unname(ledger$entries)

  # outstanding futures are drained before a failure returns, so no task keeps
  # running on a caller-owned cluster after Margot has stopped
  live <- new.env(parent = emptyenv())
  live$futures <- list()
  register_future <- function(key, value) {
    live$futures[[key]] <- value
    invisible(value)
  }
  release_future <- function(key) {
    live$futures[[key]] <- NULL
    invisible(NULL)
  }
  drain_futures <- function() {
    for (pending in live$futures) {
      try(
        {
          if (!future::resolved(pending)) {
            try(future::cancel(pending), silent = TRUE)
          }
          future::value(pending)
        },
        silent = TRUE
      )
    }
    live$futures <- list()
    invisible(NULL)
  }
  # Registered before any future exists and before an owned pool is opened, so
  # outstanding work is cancelled and drained on every exit path. Exit handlers
  # run in registration order under `after = TRUE`, which every handler below
  # states explicitly: this drain must run before the random-number state is
  # installed and before an owned pool's plan is restored, because restoring the
  # plan can shut its workers down under still-running tasks.
  on.exit(try(drain_futures(), silent = TRUE), add = TRUE, after = TRUE)
  fail <- function(condition, stage, label) {
    try(drain_futures(), silent = TRUE)
    margot_lmtp_task_abort(
      condition = condition,
      records = ledger_snapshot(),
      stage = stage,
      label = label,
      call = call
    )
  }
  guard <- function(expr, stage, label) {
    tryCatch(expr, error = function(e) fail(e, stage, label))
  }
  # mark the ledger entry for one task as failed before the whole call stops
  guard_task <- function(expr, key, stage, label) {
    tryCatch(
      expr,
      error = function(e) {
        entry <- ledger$entries[[key]] %||% list(stage = stage)
        entry$success <- FALSE
        entry$state <- "failed"
        entry$error <- conditionMessage(e)
        note(key, entry)
        fail(e, stage, label)
      }
    )
  }

  # Stage 0: coordinator preflight
  code_fingerprint <- guard(margot_lmtp_code_fingerprint(), "preflight", "code fingerprint")
  policy_arguments <- guard(
    {
      resolved <- lapply(policies, function(shift_name) {
        shift <- margot_lmtp_prepare_shift(shift_functions[[shift_name]], trt)
        shared_args <- c(
          list(data = data, trt = trt, outcomes = outcome_vars, shift = shift),
          lmtp_defaults
        )
        if (!is.null(mtp_by_arm)) {
          shared_args$mtp <- unname(mtp_by_arm[[shift_name]])
        }
        margot_lmtp_task_resolve_args(shared_args, call = call)
      })
      names(resolved) <- policies
      resolved
    },
    "preflight", "estimator arguments"
  )
  for (shift_name in policies) {
    guard(
      margot_lmtp_task_preflight(policy_arguments[[shift_name]], call = call),
      "preflight", shift_name
    )
  }
  shift_labels <- vapply(
    policy_arguments,
    function(args) paste(deparse(args$shift), collapse = " "),
    character(1)
  )
  # every policy's fit starts from this exact state, so the identity is built
  # from it rather than from the bare seed, and the caller's post-call state is
  # this state whatever the scheduling topology or checkpoint availability
  set.seed(seed)
  seed_state <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  rng_kind <- RNGkind()
  # runs after the drain, before the plan restoration
  on.exit(assign(".Random.seed", seed_state, envir = .GlobalEnv), add = TRUE, after = TRUE)

  realised_shifts <- guard(
    lapply(policies, function(shift_name) {
      margot_lmtp_realise_shift(
        args = policy_arguments[[shift_name]],
        shift_name = shift_name,
        call = call
      )
    }),
    "preflight", "policy realisation"
  )
  names(realised_shifts) <- policies
  stochastic <- policies[vapply(realised_shifts, function(x) isTRUE(x$consumed_rng), logical(1))]
  if (length(stochastic) > 0L) {
    cli::cli_abort(
      c(
        "Task-parallel density-ratio reuse requires deterministic policy functions.",
        "x" = "Polic{?y/ies} {.val {stochastic}} drew random numbers when applied to the analysis data.",
        "i" = "A stochastic policy needs a registered scheduling-independent design, which this mode does not yet provide; use the fold-parallel route."
      ),
      class = "margot_error_stochastic_shift_unsupported",
      call = call
    )
  }
  identities <- guard(
    vapply(
      policies,
      function(shift_name) {
        margot_lmtp_density_identity(
          args = policy_arguments[[shift_name]],
          shift_name = shift_name,
          shift_label = shift_labels[[shift_name]],
          realised_shift = realised_shifts[[shift_name]]$values,
          seed_state = seed_state,
          rng_kind = rng_kind,
          code_fingerprint = code_fingerprint,
          estimator_spec_hash = estimator_spec_hash
        )
      },
      character(1)
    ),
    "preflight", "task identities"
  )

  old_plan <- future::plan("list")
  pool <- guard(
    margot_lmtp_resolve_task_pool(
      models_in_parallel = models_in_parallel,
      n_cores = n_cores,
      task_budget = total_tasks,
      models_in_parallel_supplied = models_in_parallel_supplied
    ),
    "preflight", "worker pool"
  )
  if (isTRUE(pool$owned)) {
    # the caller's plan is restored exactly on success and on error, and only
    # after the drain above has cleared this pool's outstanding work
    on.exit(future::plan(old_plan, substitute = FALSE), add = TRUE, after = TRUE)
  }
  guard(
    margot_lmtp_probe_workers(pool, register = register_future, call = call),
    "eligibility probe", pool$kind
  )
  cli::cli_alert_info(
    "Scheduling {policy_count} density task{?s} and {total_tasks} outcome task{?s} over {pool$workers} worker{?s} ({pool$kind})."
  )

  pb_id <- NULL
  if (identical(progress, "cli")) {
    pb_id <- cli::cli_progress_bar(
      total = total_tasks,
      format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
    on.exit(try(cli::cli_progress_done(id = pb_id), silent = TRUE), add = TRUE)
  }

  density_stage <- margot_lmtp_density_stage
  outcome_stage <- margot_lmtp_outcome_stage
  launch_stage <- function(stage_function, payload) {
    future::future(
      stage_function(payload),
      seed = TRUE,
      globals = list(stage_function = stage_function, payload = payload)
    )
  }

  density_records <- vector("list", policy_count)
  outcome_futures <- lapply(seq_len(policy_count), function(index) vector("list", outcome_count))
  outcome_restored <- lapply(seq_len(policy_count), function(index) vector("list", outcome_count))
  outcome_launched <- rep(FALSE, policy_count)

  # build one verified outcome task on the coordinator. The assertion is
  # deterministic, so verifying it here keeps the Stage 2 payload small without
  # moving any random-number-consuming work
  launch_outcome_tasks <- function(policy_index) {
    record <- density_records[[policy_index]]
    args <- policy_arguments[[policy_index]]
    source_outcome <- args$outcomes[[1L]]
    shared_variables <- c(
      unlist(args$trt), unlist(args$time_vary), args$baseline, args$cens,
      args$compete, args$id, "..i..lmtp_id"
    )
    build_task <- function(outcome) {
      margot_lmtp_make_task(
        data = args$data,
        trt = args$trt,
        outcome = outcome,
        baseline = args$baseline,
        time_vary = args$time_vary,
        cens = args$cens,
        compete = args$compete,
        shift = args$shift,
        # the policy was realised once in preflight; every task reuses it
        shifted = margot_lmtp_shifted_for_outcome(
          realised_shifts[[policy_index]]$frame, args, outcome
        ),
        k = args$k,
        mtp = args$mtp,
        outcome_type = args$outcome_type,
        id = args$id,
        bounds = args$bounds,
        folds = args$folds,
        weights = args$weights
      )
    }
    # a resumed density result carries no task, so every outcome task, including
    # the source outcome's, is built from this call's own data; a fitted result
    # keeps this call's own source task, which preserves it byte for byte
    resumed <- identical(record$density_source, "checkpoint")
    source_task <- guard(
      {
        built <- if (resumed) build_task(source_outcome) else record$source_task
        if (!resumed && !identical(built$folds, record$folds)) {
          cli::cli_abort(
            c(
              "Policy {.val {policies[[policy_index]]}} cannot reuse the common density-ratio fit.",
              "x" = "Mismatched: the fold map recorded with the density result."
            ),
            class = "margot_error_density_ratio_identity",
            call = call
          )
        }
        built$folds <- record$folds
        built
      },
      "outcome", policies[[policy_index]]
    )
    progress_steps <- source_task$time_horizon * args$folds * (outcome_count + 1L)
    for (outcome_index in seq_len(outcome_count)) {
      outcome <- outcome_vars[[outcome_index]]
      label <- paste0(outcome, " - ", policies[[policy_index]])
      task <- guard(
        {
          built <- if (identical(outcome, source_outcome)) {
            source_task
          } else {
            build_task(outcome)
          }
          margot_lmtp_assert_shared_task(
            task = built,
            source_task = source_task,
            shared_variables = shared_variables,
            outcome = outcome
          )
          built$folds <- record$folds
          built
        },
        "outcome", label
      )
      key <- paste0("outcome-", policy_index, "-", outcome_index)
      task_fingerprint <- guard_task(
        margot_lmtp_outcome_task_fingerprint(
          task = task,
          outcome = outcome,
          result_fingerprint = record$result_fingerprint,
          folds_fingerprint = record$folds_fingerprint,
          learners_outcome = args$learners_outcome,
          control = args$control,
          estimator_spec_hash = estimator_spec_hash
        ),
        key, "outcome", label
      )
      # a completed outcome survives an interrupted run: a stored model whose
      # task fingerprint matches is reused, and no worker is launched
      restored <- guard_task(
        margot_lmtp_read_outcome_checkpoint(
          directory = outcome_checkpoint_dir,
          task_fingerprint = task_fingerprint,
          outcome = outcome,
          shift_name = policies[[policy_index]],
          call = call
        ),
        key, "outcome", label
      )
      if (!is.null(restored)) {
        outcome_restored[[policy_index]][[outcome_index]] <<- list(
          stage = "outcome",
          outcome = outcome,
          shift_name = policies[[policy_index]],
          identity = record$identity,
          task_fingerprint = task_fingerprint,
          result_fingerprint = record$result_fingerprint,
          success = TRUE,
          model_source = "checkpoint",
          model = restored$model,
          checkpoint_path = restored$checkpoint_path,
          host = unname(Sys.info()[["nodename"]]),
          process_id = Sys.getpid(),
          started_at = restored$timestamp,
          elapsed_seconds = 0
        )
        note(key, list(
          stage = "outcome",
          outcome = outcome,
          shift_name = policies[[policy_index]],
          identity = record$identity,
          task_fingerprint = task_fingerprint,
          success = TRUE,
          model_source = "checkpoint",
          state = "restored"
        ))
        next
      }
      note(key, list(
        stage = "outcome",
        outcome = outcome,
        shift_name = policies[[policy_index]],
        identity = record$identity,
        task_fingerprint = task_fingerprint,
        success = NA,
        model_source = "fit",
        state = "launched"
      ))
      launched <- guard_task(
        launch_stage(outcome_stage, list(
          outcome = outcome,
          shift_name = policies[[policy_index]],
          source_outcome = source_outcome,
          identity = record$identity,
          task_fingerprint = task_fingerprint,
          shift_label = shift_labels[[policy_index]],
          task = task,
          density_ratios = record$density_fit$density_ratios,
          density_fits = record$density_fit$fits,
          folds = record$folds,
          folds_fingerprint = record$folds_fingerprint,
          post_ratio_seed = record$post_ratio_seed,
          result_fingerprint = record$result_fingerprint,
          learners_outcome = args$learners_outcome,
          control = args$control,
          estimator_spec_hash = estimator_spec_hash,
          progress_steps = progress_steps
        )),
        key, "outcome", label
      )
      register_future(key, launched)
      outcome_futures[[policy_index]][[outcome_index]] <<- launched
    }
    outcome_launched[[policy_index]] <<- TRUE
    invisible(NULL)
  }

  accept_density <- function(policy_index, record) {
    key <- paste0("density-", policy_index)
    guard_task(
      {
        if (!identical(record$shift_name, policies[[policy_index]]) ||
            !identical(record$identity, identities[[policy_index]])) {
          cli::cli_abort(
            "A density task returned a record for a different policy identity.",
            class = "margot_error_task_identity_mismatch",
            call = call
          )
        }
      },
      key, "density", policies[[policy_index]]
    )
    density_records[[policy_index]] <<- record
    note(key, margot_lmtp_task_record_summary(record))
    if (save_output && !is.null(density_checkpoint_dir) &&
        identical(record$density_source, "fit")) {
      # the fit succeeded whatever its checkpoint write does, so the write keeps
      # its own state rather than marking a completed fit as failed
      path <- tryCatch(
        margot_lmtp_write_density_checkpoint(record, density_checkpoint_dir, call = call),
        error = function(e) {
          entry <- ledger$entries[[key]]
          entry$checkpoint_state <- "failed"
          entry$checkpoint_error <- conditionMessage(e)
          note(key, entry)
          fail(e, "density checkpoint", policies[[policy_index]])
        }
      )
      density_records[[policy_index]]$checkpoint_path <<- path
      note(key, margot_lmtp_task_record_summary(density_records[[policy_index]]))
    }
    # the density stage stops here: the investigator assesses positivity before
    # any terminal outcome is fitted
    if (!identical(stages, "density")) {
      launch_outcome_tasks(policy_index)
    }
  }

  # Stage 1: reuse a verified density checkpoint where one exists, otherwise
  # launch every remaining density future before collecting any of them
  density_futures <- vector("list", policy_count)
  for (policy_index in seq_len(policy_count)) {
    shift_name <- policies[[policy_index]]
    cached <- guard(
      margot_lmtp_read_density_checkpoint(
        directory = density_checkpoint_dir,
        identity = identities[[policy_index]],
        shift_name = shift_name,
        call = call
      ),
      "density", shift_name
    )
    if (!is.null(cached)) {
      density_records[[policy_index]] <- cached
      next
    }
    if (identical(stages, "outcome")) {
      # the outcome stage never refits the shared exposure and censoring stage
      fail(
        rlang::error_cnd(
          class = "margot_error_density_checkpoint_required",
          message = paste0(
            "No verified density checkpoint is available for policy '", shift_name,
            "'. Run the density stage for this policy before its outcome stage; ",
            "the outcome stage does not refit the exposure and censoring models."
          )
        ),
        "density", shift_name
      )
    }
    key <- paste0("density-", policy_index)
    note(key, list(
      stage = "density",
      shift_name = shift_name,
      identity = identities[[policy_index]],
      success = NA,
      state = "launched"
    ))
    density_futures[[policy_index]] <- guard_task(
      launch_stage(density_stage, list(
        shift_name = shift_name,
        identity = identities[[policy_index]],
        seed_state = seed_state,
        code_fingerprint = code_fingerprint,
        shifted = realised_shifts[[policy_index]]$frame,
        args = policy_arguments[[policy_index]]
      )),
      key, "density", shift_name
    )
    register_future(key, density_futures[[policy_index]])
  }

  # a checkpoint hit needs no density worker, so its outcome tasks start at once
  for (policy_index in seq_len(policy_count)) {
    if (is.null(density_futures[[policy_index]]) && !outcome_launched[[policy_index]]) {
      accept_density(policy_index, density_records[[policy_index]])
    }
  }

  # Stage 2: launch each remaining policy's outcome tasks as its density resolves
  pending <- which(!vapply(density_futures, is.null, logical(1)))
  while (length(pending) > 0L) {
    progressed <- FALSE
    for (policy_index in pending) {
      shift_name <- policies[[policy_index]]
      resolved <- guard_task(
        future::resolved(density_futures[[policy_index]]),
        paste0("density-", policy_index), "density", shift_name
      )
      if (!isTRUE(resolved)) {
        next
      }
      record <- guard_task(
        future::value(density_futures[[policy_index]]),
        paste0("density-", policy_index), "density", shift_name
      )
      release_future(paste0("density-", policy_index))
      progressed <- TRUE
      pending <- setdiff(pending, policy_index)
      accept_density(policy_index, record)
    }
    if (!progressed && length(pending) > 0L) {
      Sys.sleep(0.05)
    }
  }

  if (identical(stages, "density")) {
    return(margot_lmtp_density_stage_result(
      density_records = density_records,
      outcome_vars = outcome_vars,
      policies = policies,
      ledger = ledger_snapshot(),
      density_checkpoint_dir = density_checkpoint_dir,
      worker_count = pool$workers
    ))
  }

  # Stage 3: deterministic assembly in the original outcome x policy order
  results <- list()
  for (policy_index in seq_len(policy_count)) {
    shift_name <- policies[[policy_index]]
    for (outcome_index in seq_len(outcome_count)) {
      outcome <- outcome_vars[[outcome_index]]
      label <- paste0(outcome, " - ", shift_name)
      key <- paste0("outcome-", policy_index, "-", outcome_index)
      restored <- outcome_restored[[policy_index]][[outcome_index]]
      outcome_record <- if (!is.null(restored)) {
        restored
      } else {
        guard_task(
          future::value(outcome_futures[[policy_index]][[outcome_index]]),
          key, "outcome", label
        )
      }
      release_future(key)
      expected <- ledger$entries[[key]]
      guard_task(
        {
          if (!identical(outcome_record$outcome, outcome) ||
              !identical(outcome_record$shift_name, shift_name) ||
              !identical(outcome_record$identity, identities[[policy_index]]) ||
              !identical(outcome_record$task_fingerprint, expected$task_fingerprint)) {
            cli::cli_abort(
              paste0(
                "An outcome task returned a record for a different grid cell than ",
                outcome, " - ", shift_name, "."
              ),
              class = "margot_error_task_identity_mismatch",
              call = call
            )
          }
        },
        key, "outcome", label
      )
      note(key, margot_lmtp_task_record_summary(outcome_record))
      result <- guard_task(
        {
          assembled <- list(
            outcome = outcome,
            shift_name = shift_name,
            model_name = paste0(outcome, "_", shift_name),
            success = TRUE,
            model = NULL,
            error = NULL
          )
          assembled$model <- margot_lmtp_attach_exposure_by_wave(
            outcome_record$model,
            data = data,
            trt = trt
          )
          attr(assembled$model, "margot_density_ratio_fit_id") <- shift_name
          assembled$task_record <- margot_lmtp_task_record_summary(outcome_record)
          assembled
        },
        key, "outcome", label
      )
      # the coordinator writes every checkpoint; workers never do. The model is
      # written once, keyed by its task fingerprint, and the run directory's
      # name-keyed entry is a link to that single copy
      if (save_output && !is.null(outcome_checkpoint_dir)) {
        checkpoint_file <- paste0(
          margot_lmtp_safe_path_name(outcome), "_",
          margot_lmtp_safe_path_name(shift_name), ".rds"
        )
        run_path <- if (!is.null(checkpoint_dir)) {
          file.path(checkpoint_dir, checkpoint_file)
        } else {
          NULL
        }
        written <- guard_task(
          if (identical(outcome_record$model_source, "checkpoint")) {
            # a restored model is already stored under this identity; only the
            # run directory's link is missing, and rewriting it would install a
            # second copy of a model this call never fitted
            list(
              path = outcome_record$checkpoint_path,
              run_path = run_path,
              linked = margot_lmtp_link_run_checkpoint(
                outcome_record$checkpoint_path, run_path
              )
            )
          } else {
            margot_lmtp_write_outcome_checkpoint(
              model = result$model,
              outcome = outcome,
              shift_name = shift_name,
              task_fingerprint = outcome_record$task_fingerprint,
              directory = outcome_checkpoint_dir,
              run_path = run_path,
              call = call
            )
          },
          key, "outcome", label
        )
        result$checkpoint_path <- written$run_path %||% written$path
        result$outcome_checkpoint_path <- written$path
        cli::cli_alert_success("Saved checkpoint: {.file {checkpoint_file}}")
      }
      if (!is.null(pb_id)) {
        try(
          cli::cli_progress_update(
            id = pb_id,
            inc = 1,
            status = sprintf("Completed %s - %s", outcome, shift_name)
          ),
          silent = TRUE
        )
      }
      results[[length(results) + 1L]] <- result
    }
  }

  summaries <- lapply(density_records, margot_lmtp_task_record_summary)
  names(summaries) <- policies
  attr(results, "margot_lmtp_density_records") <- summaries
  attr(results, "margot_lmtp_task_records") <- ledger_snapshot()
  attr(results, "margot_lmtp_scheduler") <- "task"
  attr(results, "margot_lmtp_worker_count") <- pool$workers
  results
}
