#' Save and reuse verified LMTP task results
#'
#' A caller defines each task's complete scientific and runtime identity. A
#' completed result is reused only after checking that identity and the payload
#' checksum. Computation and installation hold exclusive local ownership.
#' An existing ownership directory requires inspection; locks never expire
#' automatically. These functions do not define estimators or schedule workers.
#'
#' Identities are compared in canonical form (see
#' [margot_lmtp_checkpoint_identity()]), so attributes that record where an
#' input came from, such as the file path RcppTOML attaches to a parsed study
#' file, do not change a task's identity.
#'
#' @param directory Private directory for these tasks.
#' @param key Unique task name using lowercase letters, digits and underscores.
#' @param identity Nonempty list identifying all result-affecting inputs,
#'   ordered participants, histories, folds, weights, settings and code/builds.
#'   Environments, closures and external pointers are refused; pass a
#'   fingerprint of a function instead of the function.
#' @param compute Zero-argument function computing a missing task.
#' @param identities Named list of identities for the complete expected task set.
#' @param load For `inventory`, whether to return each verified value. The
#'   default `FALSE` verifies every file but returns metadata only (path and
#'   checksums), so an inventory never holds the saved results in memory.
#' @return `read` returns a verified record or `NULL`; `compute` returns a
#'   record containing `value`, `path`, `reused` and checksums; `inventory`
#'   returns completion status, a task table and verified records, which hold
#'   `value` only when `load = TRUE`.
#' @md
#' @name margot_lmtp_checkpoints
NULL

# sourceable orchestration helper; scientific identities belong to the study caller.
# reuse margot's verified atomic writer under exclusive local ownership.
margot_lmtp_checkpoint_hash <- function(object) digest::digest(object, algo = "sha256")

# attributes that describe content; every other attribute is dropped.
margot_lmtp_identity_attributes <- c("names", "dim", "dimnames", "class", "levels", "row.names", "tzone")

#' Canonical form of a checkpoint identity
#'
#' Returns `identity` with every attribute other than names, dimensions,
#' dimension names, class, factor levels, row names and time zone removed,
#' recursively. Attributes such as the absolute file path that RcppTOML attaches
#' to a parsed TOML file therefore do not enter a checkpoint identity, and the
#' same inputs read from two locations give the same identity. Environments,
#' closures, external pointers and other reference objects are refused, because
#' their serialised form depends on the R process rather than on the inputs;
#' identify a function by a fingerprint of its source instead.
#'
#' @param identity A list identifying a checkpoint task.
#' @return The canonical identity.
#' @examples
#' study <- structure(list(seed = 42L), class = c("toml", "list"), file = "/a/study.toml")
#' identical(margot_lmtp_checkpoint_identity(list(study = study)),
#'   margot_lmtp_checkpoint_identity(list(study = structure(study, file = "/b/study.toml"))))
#' @export
margot_lmtp_checkpoint_identity <- function(identity) {
  stopifnot(is.list(identity), length(identity) > 0L)
  canonical <- function(x, path) {
    if (is.environment(x) || is.function(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode", "promise")) {
      stop("A checkpoint identity cannot hold ", typeof(x), " objects (at ", path,
        "); pass a fingerprint instead.", call. = FALSE)
    }
    if (is.list(x) && length(x)) {
      labels <- if (is.null(names(x))) seq_along(x) else names(x)
      for (i in seq_along(x)) {
        value <- canonical(x[[i]], paste0(path, "/", labels[[i]]))
        if (!is.null(value)) x[[i]] <- value
      }
    }
    kept <- attributes(x)[intersect(names(attributes(x)), margot_lmtp_identity_attributes)]
    attributes(x) <- if (length(kept)) kept else NULL
    x
  }
  canonical(identity, "identity")
}

margot_lmtp_checkpoint_path <- function(directory, key) {
  stopifnot(is.character(directory), length(directory) == 1L, !is.na(directory), nzchar(directory),
    is.character(key), length(key) == 1L, !is.na(key), grepl("^[a-z][a-z0-9_]*$", key))
  file.path(directory, paste0(key, ".rds"))
}

# read and verify one record without unserialising its payload.
margot_lmtp_checkpoint_verify <- function(directory, key, identity) {
  identity <- margot_lmtp_checkpoint_identity(identity)
  path <- margot_lmtp_checkpoint_path(directory, key)
  if (!file.exists(path)) return(NULL)
  record <- readRDS(path)
  stopifnot(is.list(record), identical(record$schema, "mlmtp_verified_checkpoint_v1"),
    identical(record$key, key), identical(record$identity, identity),
    identical(record$identity_sha256, margot_lmtp_checkpoint_hash(identity)), is.raw(record$payload),
    identical(record$payload_sha256, margot_lmtp_checkpoint_hash(record$payload)))
  list(record = record, path = path)
}

#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_read <- function(directory, key, identity) {
  stopifnot(is.list(identity), length(identity) > 0L)
  checked <- margot_lmtp_checkpoint_verify(directory, key, identity)
  if (is.null(checked)) return(NULL)
  list(value = unserialize(checked$record$payload), path = checked$path, reused = TRUE,
    identity_sha256 = checked$record$identity_sha256, payload_sha256 = checked$record$payload_sha256)
}

# ownership covers lookup, computation and installation; competing callers fail before fitting.
#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_compute <- function(directory, key, identity, compute) {
  stopifnot(is.function(compute), is.list(identity), length(identity) > 0L)
  identity <- margot_lmtp_checkpoint_identity(identity)
  path <- margot_lmtp_checkpoint_path(directory, key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(path, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Checkpoint ownership exists. Inspect the active or interrupted writer before recovery: ", lock,
      call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  saveRDS(list(pid = Sys.getpid(), host = unname(Sys.info()["nodename"]),
    started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), key = key,
    identity_sha256 = margot_lmtp_checkpoint_hash(identity)), file.path(lock, "owner.rds"))
  existing <- margot_lmtp_checkpoint_read(directory, key, identity)
  if (!is.null(existing)) return(existing)
  value <- compute()
  payload <- serialize(value, NULL, version = 3L)
  # the computed value is released before the read-back, so two full copies are never held.
  rm(value)
  record <- list(schema = "mlmtp_verified_checkpoint_v1", key = key, identity = identity,
    identity_sha256 = margot_lmtp_checkpoint_hash(identity), payload = payload,
    payload_sha256 = margot_lmtp_checkpoint_hash(payload))
  rm(payload)
  writer <- margot_lmtp_write_rds_atomic
  writer(record, path)
  expected <- record$payload_sha256
  rm(record)
  installed <- margot_lmtp_checkpoint_read(directory, key, identity)
  stopifnot(identical(installed$payload_sha256, expected))
  installed$reused <- FALSE
  installed
}

# completion is reconciled against the caller's full task set, never inferred from existing files.
#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_inventory <- function(directory, identities, load = FALSE) {
  stopifnot(is.list(identities), length(identities) > 0L, !is.null(names(identities)),
    !anyDuplicated(names(identities)), isTRUE(load) || isFALSE(load))
  paths <- vapply(names(identities), function(key) margot_lmtp_checkpoint_path(directory, key), character(1))
  if (dir.exists(directory)) {
    unexpected <- setdiff(list.files(directory, pattern = "[.]rds$", full.names = FALSE), basename(paths))
    if (length(unexpected)) stop("Unexpected checkpoint keys: ", paste(unexpected, collapse = ", "), call. = FALSE)
  }
  records <- lapply(names(identities), function(key) {
    if (load) return(margot_lmtp_checkpoint_read(directory, key, identities[[key]]))
    checked <- margot_lmtp_checkpoint_verify(directory, key, identities[[key]])
    if (is.null(checked)) return(NULL)
    list(path = checked$path, reused = TRUE, identity_sha256 = checked$record$identity_sha256,
      payload_sha256 = checked$record$payload_sha256)
  })
  saved <- !vapply(records, is.null, logical(1))
  owned <- dir.exists(paste0(paths, ".lock"))
  list(complete = all(saved) && !any(owned),
    tasks = data.frame(key = names(identities), verified = saved, writer_ownership = owned),
    records = setNames(records, names(identities)))
}

#' Check that an object holds no fitted learner
#'
#' Walks `x` (lists and their elements, recursively) and stops if it reaches a
#' fitted learner object: a SuperLearner ensemble (including lmtp's
#' `lmtp_ensemble`), a ranger forest, an xgboost booster or a glmnet fit. Use it
#' at each stage boundary of an analysis to confirm that results kept in memory
#' or saved for reporting hold estimates and diagnostics, not fitted models.
#'
#' @param x Object to check.
#' @param what Label used in the error message.
#' @return `x`, invisibly, when no fitted learner is found.
#' @examples
#' margot_lmtp_assert_lean(list(estimate = 1, weights = c(a = 0.5, b = 0.5)))
#' @export
margot_lmtp_assert_lean <- function(x, what = deparse(substitute(x))) {
  heavy <- c("SuperLearner", "ranger", "xgb.Booster", "glmnet", "cv.glmnet")
  found <- character()
  walk <- function(object, path) {
    if (length(found) >= 5L) return(invisible(NULL))
    if (inherits(object, heavy)) {
      found <<- c(found, paste0(path, " (", class(object)[1L], ")"))
      return(invisible(NULL))
    }
    if (is.list(object) && length(object)) {
      labels <- if (is.null(names(object))) seq_along(object) else names(object)
      for (i in seq_along(object)) walk(object[[i]], paste0(path, "$", labels[[i]]))
    }
    invisible(NULL)
  }
  walk(x, what)
  if (length(found)) {
    stop("Fitted learner objects are held in ", what, ": ", paste(found, collapse = "; "),
      if (length(found) >= 5L) " (first five shown)" else "", call. = FALSE)
  }
  invisible(x)
}
