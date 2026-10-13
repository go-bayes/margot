#' Save and reuse verified LMTP task results
#'
#' A caller defines each task's complete scientific and runtime identity. A
#' completed result is reused only after checking that identity and the payload
#' checksum. Computation and installation hold exclusive local ownership.
#' An existing ownership directory requires inspection; locks never expire
#' automatically. These functions do not define estimators or schedule workers.
#'
#' @param directory Private directory for these tasks.
#' @param key Unique task name using lowercase letters, digits and underscores.
#' @param identity Nonempty list identifying all result-affecting inputs,
#'   ordered participants, histories, folds, weights, settings and code/builds.
#' @param compute Zero-argument function computing a missing task.
#' @param identities Named list of identities for the complete expected task set.
#' @return `read` returns a verified record or `NULL`; `compute` returns a
#'   record containing `value`, `path`, `reused` and checksums; `inventory`
#'   returns completion status, a task table and verified records.
#' @md
#' @name margot_lmtp_checkpoints
NULL

# sourceable orchestration helper; scientific identities belong to the study caller.
# reuse margot's verified atomic writer under exclusive local ownership.
margot_lmtp_checkpoint_hash <- function(object) digest::digest(object, algo = "sha256")

margot_lmtp_checkpoint_path <- function(directory, key) {
  stopifnot(is.character(directory), length(directory) == 1L, !is.na(directory), nzchar(directory),
    is.character(key), length(key) == 1L, !is.na(key), grepl("^[a-z][a-z0-9_]*$", key))
  file.path(directory, paste0(key, ".rds"))
}

#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_read <- function(directory, key, identity) {
  stopifnot(is.list(identity), length(identity) > 0L)
  path <- margot_lmtp_checkpoint_path(directory, key)
  if (!file.exists(path)) return(NULL)
  record <- readRDS(path)
  stopifnot(is.list(record), identical(record$schema, "mlmtp_verified_checkpoint_v1"),
    identical(record$key, key), identical(record$identity, identity),
    identical(record$identity_sha256, margot_lmtp_checkpoint_hash(identity)), is.raw(record$payload),
    identical(record$payload_sha256, margot_lmtp_checkpoint_hash(record$payload)))
  list(value = unserialize(record$payload), path = path, reused = TRUE,
    identity_sha256 = record$identity_sha256, payload_sha256 = record$payload_sha256)
}

# ownership covers lookup, computation and installation; competing callers fail before fitting.
#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_compute <- function(directory, key, identity, compute) {
  stopifnot(is.function(compute), is.list(identity), length(identity) > 0L)
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
  record <- list(schema = "mlmtp_verified_checkpoint_v1", key = key, identity = identity,
    identity_sha256 = margot_lmtp_checkpoint_hash(identity), payload = payload,
    payload_sha256 = margot_lmtp_checkpoint_hash(payload))
  writer <- margot_lmtp_write_rds_atomic
  writer(record, path)
  installed <- margot_lmtp_checkpoint_read(directory, key, identity)
  stopifnot(identical(installed$payload_sha256, record$payload_sha256))
  installed$reused <- FALSE
  installed
}

# completion is reconciled against the caller's full task set, never inferred from existing files.
#' @rdname margot_lmtp_checkpoints
#' @export
margot_lmtp_checkpoint_inventory <- function(directory, identities) {
  stopifnot(is.list(identities), length(identities) > 0L, !is.null(names(identities)),
    !anyDuplicated(names(identities)))
  paths <- vapply(names(identities), function(key) margot_lmtp_checkpoint_path(directory, key), character(1))
  if (dir.exists(directory)) {
    unexpected <- setdiff(list.files(directory, pattern = "[.]rds$", full.names = FALSE), basename(paths))
    if (length(unexpected)) stop("Unexpected checkpoint keys: ", paste(unexpected, collapse = ", "), call. = FALSE)
  }
  records <- lapply(names(identities), function(key) margot_lmtp_checkpoint_read(directory, key, identities[[key]]))
  saved <- !vapply(records, is.null, logical(1))
  owned <- dir.exists(paste0(paths, ".lock"))
  list(complete = all(saved) && !any(owned),
    tasks = data.frame(key = names(identities), verified = saved, writer_ownership = owned),
    records = setNames(records, names(identities)))
}
