#' Fit SDR from verified, previously estimated components
#'
#' Use with [margot_lmtp()] through its `lmtp_model_type` argument. The caller
#' supplies node density ratios, the realised fold map and the RNG procedure.
#' No exposure or observation model is fitted and no additional cap is applied.
#' The caller remains responsible for the provenance and scientific validity of
#' the components. Input identities are checked before outcome fitting.
#'
#' The current implementation supports continuous outcomes and requires a unique,
#' nonmissing `id` column. It uses the internal interface of `lmtp` 1.5.4;
#' other versions are rejected by the shared interface guard. At least two
#' outer folds must be supplied, and the active future plan must provide exactly
#' one worker per fold. This function does not change the future plan.
#' Exposure waves may be nonconsecutive, and each node may include covariates
#' from multiple earlier surveys; the caller declares when each covariate enters.
#'
#' @param data Analysis frame, including one continuous outcome and `id`.
#' @param trt Ordered exposure column names.
#' @param outcome Outcome column name.
#' @param shift Registered shift function.
#' @param components List containing `frame` (all non-outcome analysis columns),
#'   `history`, `baseline`, `time_vary`, `trt`, `cens`, `density_ratios`, `folds`, `weights`, `seed`,
#'   `rng_kind`, `shift_source_sha256` and `shifted_exposures`.
#' @param baseline Baseline covariate names, identical to `components$baseline`.
#' @param time_vary List of covariates entering each node, identical to
#'   `components$time_vary`, including node order and within-node variable order.
#' @param learners_outcome Outcome learner library.
#' @param bounds Explicit outcome limits on the analysis scale.
#' @param control An `lmtp_control` object with `.trim = 1`.
#' @param progress_bar Optional native SDR progress callback.
#' @param learners_trt Ignored; exposure components are already supplied.
#' @return An ordinary `lmtp` fit with supplied folds, histories, bounds and RNG
#'   provenance attached.
#' @md
#' @export
margot_lmtp_sdr_from_components <- function(data, trt, outcome, shift,
    components, baseline, time_vary, learners_outcome, bounds,
    control = lmtp::lmtp_control(.trim = 1),
    progress_bar = function(...) invisible(NULL), learners_trt = NULL) {
  x <- components
  stopifnot(is.data.frame(data), is.data.frame(x$frame), is.function(shift),
    is.character(outcome), length(outcome) == 1L, !outcome %in% names(x$frame),
    identical(sort(names(data)), sort(c(names(x$frame), outcome))),
    identical(data[names(x$frame)], x$frame), identical(trt, x$trt),
    identical(baseline, x$baseline), identical(time_vary, x$time_vary),
    setequal(unique(c(baseline, unlist(time_vary))), x$history),
    identical(digest::digest(shift, algo = "sha256"), x$shift_source_sha256),
    identical(lapply(trt, function(node) shift(x$frame, node)), x$shifted_exposures),
    is.matrix(x$density_ratios), identical(dim(x$density_ratios), c(nrow(data), length(trt))),
    all(is.finite(x$density_ratios)), all(x$density_ratios >= 0),
    length(x$weights) == nrow(data), all(is.finite(x$weights)), all(x$weights > 0),
    is.numeric(bounds), length(bounds) == 2L, all(is.finite(bounds)), bounds[1L] < bounds[2L],
    all(is.na(data[[outcome]]) | (data[[outcome]] >= bounds[1L] & data[[outcome]] <= bounds[2L])),
    is.list(control), identical(as.numeric(control$.trim), 1),
    is.function(progress_bar), length(x$cens) == length(trt),
    is.numeric(x$seed), length(x$seed) == 1L, is.finite(x$seed),
    is.character(x$rng_kind), length(x$rng_kind) == 3L)
  ids <- x$frame$id
  stopifnot(length(ids) == nrow(data), !anyNA(ids), !anyDuplicated(ids),
    is.list(x$folds), length(x$folds) >= 2L)
  validation <- integer()
  for (fold in x$folds) {
    stopifnot(setequal(names(fold), c("training_set", "validation_set")))
    for (rows in fold) stopifnot(is.numeric(rows), length(rows) > 0L,
      !anyNA(rows), all(rows == as.integer(rows)), !anyDuplicated(rows),
      all(rows >= 1L & rows <= nrow(data)))
    stopifnot(setequal(fold$training_set, setdiff(seq_len(nrow(data)), fold$validation_set)))
    validation <- c(validation, fold$validation_set)
  }
  stopifnot(identical(sort(as.integer(validation)), seq_len(nrow(data))),
    future::nbrOfWorkers() == length(x$folds))
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  do.call(RNGkind, as.list(x$rng_kind))
  set.seed(x$seed)
  task <- margot_lmtp_make_task(data, trt, outcome, baseline, time_vary, x$cens,
    compete = NULL, shift = shift, shifted = NULL, k = Inf, mtp = TRUE,
    outcome_type = "continuous", id = "id", bounds = bounds,
    folds = length(x$folds), weights = x$weights)
  task$folds <- x$folds
  expected_weights <- if (abs(mean(x$weights) - 1) < sqrt(.Machine$double.eps)) x$weights else x$weights / mean(x$weights)
  stopifnot(identical(as.character(task$id), as.character(ids)), identical(task$weights, expected_weights))
  histories <- lapply(seq_along(trt), function(node) task$vars$history("L", node + 1L))
  for (node in seq_along(trt)) stopifnot(setequal(histories[[node]],
    c(baseline, trt[seq_len(node)], unlist(time_vary[seq_len(node)]))))
  regression_seed <- get(".Random.seed", envir = .GlobalEnv)
  fit <- margot_lmtp_fit_sdr_outcome(task,
    list(density_ratios = x$density_ratios, fits = list(provenance = "supplied components; no nuisance refit")),
    learners_outcome, control, progress_bar, "supplied components")
  stopifnot(identical(fit$density_ratios, x$density_ratios))
  fit$outcome_history <- histories
  fit$bounds_realised <- bounds
  fit$rng_record <- list(kind = x$rng_kind, seed = x$seed, regression_start = regression_seed)
  fit$folds_supplied <- x$folds
  fit$weight_identity <- list(supplied = x$weights, internal = task$weights,
    normalised_by_engine = !identical(x$weights, task$weights))
  fit
}
