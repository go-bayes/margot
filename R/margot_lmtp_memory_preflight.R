# pre-launch memory checks for lmtp runs: a clean host, a measured estimate and stop rules.

#' Measure an object's size in memory and serialised
#'
#' Returns the in-memory size reported by [utils::object.size()] and the length of the object's serialised form, `length(serialize(x, NULL, version = 3L))`, which is the form `margot_lmtp_checkpoint_compute()` writes. Use it on objects built from generated fixtures at the study's number of participants and variables, so that [margot_lmtp_memory_preflight()] receives measured sizes rather than guesses. `object.size()` does not count the contents of environments and counts shared memory once for each reference, so it can understate an object that holds environments; the serialised length includes environment contents.
#'
#' @param x Object to measure.
#' @return A named numeric vector with `memory_bytes` and `serialised_bytes`.
#' @examples
#' margot_lmtp_object_size(matrix(stats::rnorm(1000), ncol = 10))
#' @seealso [margot_lmtp_memory_preflight()]
#' @md
#' @export
margot_lmtp_object_size <- function(x) {
  c(memory_bytes = as.numeric(utils::object.size(x)),
    serialised_bytes = as.numeric(length(serialize(x, NULL, version = 3L))))
}

#' Check memory before launching an LMTP run
#'
#' `margot_lmtp_memory_preflight()` checks, before a run is launched, that the host is clean and that a measured memory estimate fits within an administrative ceiling. It returns a receipt and stops with a classed error when a rule fails. It reads the host and lists processes; it never stops, signals or kills a process, and it does not set memory limits or monitor a running job.
#'
#' **Clean host.** The receipt records physical memory, free plus inactive memory, memory pressure and swap in use, and lists other R processes. On macOS these come from `sysctl hw.memsize`, `vm_stat`, `memory_pressure -Q` and `sysctl vm.swapusage`; on Linux from `/proc/meminfo`, `/proc/swaps` and, when present, `/proc/pressure/memory`. Processes are listed with `ps` on both. An R process is one whose executable is `R`, `Rscript`, `Rterm` or `rsession`; a parallelly or future PSOCK worker is an R process whose command line runs `workRSOCK`. A parallelly worker is launched detached, so its owner is read from the `parallelly.parent=<pid>` tag on its command line; any other process is owned by its parent. A worker is orphaned when its owner is process 1 or no longer exists. The current process, its descendants and the parallelly workers it owns are excluded; its ancestors are not, so an R process that launched this one counts as another R process. Workers started by base `parallel::makePSOCKcluster()` carry no owner tag and may be reported as orphaned even when the current process started them, so run the preflight before creating workers or setting a `future` plan. On another platform, or when a command fails, the affected readings are recorded as unavailable and are not estimated.
#'
#' **Measured estimate.** The estimate, in bytes, is
#'
#' `coordinator_bytes + (2 * largest_task_value_bytes + 2 * largest_task_payload_bytes) + workers * (worker_data_bytes + worker_fold_fit_bytes)`.
#'
#' The middle term is the transient peak of the largest single task when its checkpoint is computed: the computed value, its serialised payload, the re-read raw record (taken as the payload size) and the unserialised copy of the value (taken as the value size) may be held at once. The last term is one copy of the data and one outer fold's fit for each of the `workers` workers. Every size is supplied by the caller and should be measured with [margot_lmtp_object_size()] on lean-loaded checkpoints and on full fits from a generated fixture at the study's n and p. The formula is an accounting of known copies, not a validated bound: it omits the observed process-tree peak, temporary allocations inside learners and the memory of other processes.
#'
#' **Stop rules.** The preflight refuses when (1) the estimate exceeds `ceiling` times physical memory; (2) another R process or an orphaned worker is present, unless `allow_other_r = TRUE` with a reason; or (3) swap in use exceeds `swap_limit_bytes`. A rule whose reading is unavailable fails, because an unmeasured host cannot be shown to be clean; an unavailable process listing is accepted only under `allow_other_r = TRUE`. The default `ceiling = 0.70` is a provisional administrative ceiling, not a validated bound.
#'
#' @param coordinator_bytes Measured bytes held by the coordinating process: its data and the lean results it keeps.
#' @param largest_task_value_bytes Measured in-memory bytes of the largest single task's computed value.
#' @param largest_task_payload_bytes Measured serialised bytes of that value.
#' @param workers Number of workers, `K`, each holding one copy of the data and one outer fold's fit.
#' @param worker_data_bytes Measured bytes of the data each worker holds.
#' @param worker_fold_fit_bytes Measured bytes of one outer fold's full fit on a worker.
#' @param ceiling Largest permitted fraction of physical memory for the estimate. The default 0.70 is a provisional administrative ceiling, not a validated bound.
#' @param swap_limit_bytes Largest permitted swap in use, in bytes. The default is 1 GiB (`2^30` bytes).
#' @param allow_other_r Whether to proceed when other R processes or orphaned workers are present. Requires `other_r_reason`.
#' @param other_r_reason A non-empty reason, recorded in the receipt, for allowing other R processes.
#' @param receipt_path Optional path without extension. When given, the receipt is written to `<receipt_path>.rds` and a readable `<receipt_path>.txt`, whether the preflight passes or refuses. The receipt records the host name, process identifiers and command lines, so keep it in private storage.
#' @return A `margot_lmtp_memory_preflight` receipt, a list with `passed`, `created_utc`, `system` (host name, platform, process identifier, R and margot versions), `host` (memory and swap readings in bytes, pressure and the readings that were unavailable), `processes` (a data frame of other R processes with their parent, owner and role, or `NULL` when the listing is unavailable), `estimate` (inputs, components, total and formula), `rules` (one row per stop rule with its status and detail) and `settings`, and, when `receipt_path` is given, `files`. On refusal the function stops with an error of class `margot_lmtp_memory_preflight_error` whose `receipt` element holds the same receipt.
#' @examples
#' \dontrun{
#' fit_size <- margot_lmtp_object_size(fixture_fit)
#' margot_lmtp_memory_preflight(
#'   coordinator_bytes = margot_lmtp_object_size(prepared)[["memory_bytes"]],
#'   largest_task_value_bytes = fit_size[["memory_bytes"]],
#'   largest_task_payload_bytes = fit_size[["serialised_bytes"]],
#'   workers = 5,
#'   worker_data_bytes = margot_lmtp_object_size(prepared)[["memory_bytes"]],
#'   worker_fold_fit_bytes = fold_fit_bytes,
#'   receipt_path = "private/receipts/preflight-outcomes"
#' )
#' }
#' @seealso [margot_lmtp_object_size()], [margot_lmtp_assert_lean()]
#' @md
#' @export
margot_lmtp_memory_preflight <- function(coordinator_bytes,
                                         largest_task_value_bytes,
                                         largest_task_payload_bytes,
                                         workers,
                                         worker_data_bytes,
                                         worker_fold_fit_bytes,
                                         ceiling = 0.70,
                                         swap_limit_bytes = 2^30,
                                         allow_other_r = FALSE,
                                         other_r_reason = NULL,
                                         receipt_path = NULL) {
  bytes <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
      stop("`", name, "` must be one non-negative, finite number of bytes.", call. = FALSE)
    }
    as.numeric(x)
  }
  inputs <- list(
    coordinator_bytes = bytes(coordinator_bytes, "coordinator_bytes"),
    largest_task_value_bytes = bytes(largest_task_value_bytes, "largest_task_value_bytes"),
    largest_task_payload_bytes = bytes(largest_task_payload_bytes, "largest_task_payload_bytes"),
    worker_data_bytes = bytes(worker_data_bytes, "worker_data_bytes"),
    worker_fold_fit_bytes = bytes(worker_fold_fit_bytes, "worker_fold_fit_bytes")
  )
  if (!is.numeric(workers) || length(workers) != 1L || is.na(workers) || workers < 1 ||
      workers != round(workers)) {
    stop("`workers` must be one positive whole number.", call. = FALSE)
  }
  inputs$workers <- as.integer(workers)
  if (!is.numeric(ceiling) || length(ceiling) != 1L || is.na(ceiling) || ceiling <= 0 || ceiling > 1) {
    stop("`ceiling` must be one number in (0, 1].", call. = FALSE)
  }
  swap_limit_bytes <- bytes(swap_limit_bytes, "swap_limit_bytes")
  if (!isTRUE(allow_other_r) && !isFALSE(allow_other_r)) {
    stop("`allow_other_r` must be TRUE or FALSE.", call. = FALSE)
  }
  if (allow_other_r && (!is.character(other_r_reason) || length(other_r_reason) != 1L ||
      is.na(other_r_reason) || !nzchar(trimws(other_r_reason)))) {
    stop("`allow_other_r = TRUE` requires a non-empty `other_r_reason`.", call. = FALSE)
  }
  if (!is.null(receipt_path) && (!is.character(receipt_path) || length(receipt_path) != 1L ||
      is.na(receipt_path) || !nzchar(receipt_path))) {
    stop("`receipt_path` must be NULL or one path.", call. = FALSE)
  }

  estimate <- margot_lmtp_memory_estimate(inputs)
  sysname <- margot_lmtp_preflight_sysname()
  host <- margot_lmtp_preflight_host(sysname)
  processes <- margot_lmtp_preflight_other_r(margot_lmtp_preflight_processes(sysname), Sys.getpid())

  rules <- list()
  limit <- ceiling * host$physical_bytes
  rules$memory_ceiling <- if (is.na(host$physical_bytes)) {
    c("unavailable", "physical memory could not be read")
  } else {
    c(if (estimate$total_bytes > limit) "fail" else "pass",
      sprintf("estimate %s; ceiling %s (%.2f of %s physical)", margot_lmtp_format_bytes(estimate$total_bytes),
        margot_lmtp_format_bytes(limit), ceiling, margot_lmtp_format_bytes(host$physical_bytes)))
  }
  rules$other_r <- if (is.null(processes)) {
    if (allow_other_r) c("allowed", paste0("process listing unavailable; allowed: ", other_r_reason))
    else c("unavailable", "the process listing could not be read")
  } else {
    found <- nrow(processes)
    orphaned <- sum(processes$role == "orphaned_worker")
    detail <- sprintf("%d other R process(es), of which %d orphaned worker(s)", found, orphaned)
    if (!found) c("pass", detail)
    else if (allow_other_r) c("allowed", paste0(detail, "; allowed: ", other_r_reason))
    else c("fail", detail)
  }
  rules$swap <- if (is.na(host$swap_used_bytes)) {
    c("unavailable", "swap in use could not be read")
  } else {
    c(if (host$swap_used_bytes > swap_limit_bytes) "fail" else "pass",
      sprintf("swap in use %s; limit %s", margot_lmtp_format_bytes(host$swap_used_bytes),
        margot_lmtp_format_bytes(swap_limit_bytes)))
  }
  rules <- data.frame(rule = names(rules), status = vapply(rules, `[[`, "", 1L),
    detail = vapply(rules, `[[`, "", 2L), stringsAsFactors = FALSE, row.names = NULL)

  receipt <- structure(list(
    schema = "margot_lmtp_memory_preflight_v1",
    passed = all(rules$status %in% c("pass", "allowed")),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    system = list(nodename = unname(Sys.info()[["nodename"]]), sysname = sysname, pid = Sys.getpid(),
      r_version = R.version.string, margot_version = as.character(utils::packageVersion("margot"))),
    host = host,
    processes = processes,
    estimate = estimate,
    rules = rules,
    settings = list(ceiling = ceiling, swap_limit_bytes = swap_limit_bytes, allow_other_r = allow_other_r,
      other_r_reason = other_r_reason)
  ), class = "margot_lmtp_memory_preflight")

  if (!is.null(receipt_path)) {
    dir.create(dirname(receipt_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(receipt, paste0(receipt_path, ".rds"))
    writeLines(utils::capture.output(print(receipt)), paste0(receipt_path, ".txt"))
    receipt$files <- paste0(receipt_path, c(".rds", ".txt"))
  }
  if (!receipt$passed) {
    failed <- rules[!rules$status %in% c("pass", "allowed"), , drop = FALSE]
    stop(structure(class = c("margot_lmtp_memory_preflight_error", "error", "condition"), list(
      message = paste0("Memory preflight refused the launch: ",
        paste0(failed$rule, " (", failed$status, ": ", failed$detail, ")", collapse = "; "), "."),
      call = NULL, receipt = receipt)))
  }
  receipt
}

# the stated formula, kept separate so it can be tested on supplied sizes.
margot_lmtp_memory_estimate <- function(inputs) {
  components <- c(
    coordinator = inputs$coordinator_bytes,
    largest_task = 2 * inputs$largest_task_value_bytes + 2 * inputs$largest_task_payload_bytes,
    workers = inputs$workers * (inputs$worker_data_bytes + inputs$worker_fold_fit_bytes)
  )
  list(inputs = inputs, components = components, total_bytes = sum(components),
    formula = paste("coordinator_bytes + (largest_task_value_bytes + largest_task_payload_bytes",
      "+ re-read record [= largest_task_payload_bytes] + unserialised copy [= largest_task_value_bytes])",
      "+ workers * (worker_data_bytes + worker_fold_fit_bytes)"))
}

margot_lmtp_format_bytes <- function(x) {
  if (is.na(x)) return("unavailable")
  sprintf("%.2f GiB", x / 2^30)
}

# platform name; a separate function so tests can stand in another platform.
margot_lmtp_preflight_sysname <- function() tolower(unname(Sys.info()[["sysname"]]))

# run a system command, returning its output lines or NULL when it is missing or fails.
margot_lmtp_preflight_command <- function(command, args = character()) {
  if (!nzchar(Sys.which(command))) return(NULL)
  out <- tryCatch(suppressWarnings(system2(command, args, stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL)
  if (is.null(out) || !is.null(attr(out, "status"))) return(NULL)
  out
}

margot_lmtp_preflight_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
}

# memory and swap readings, in bytes; unreadable values are NA and named in `unavailable`.
margot_lmtp_preflight_host <- function(sysname = margot_lmtp_preflight_sysname()) {
  host <- list(source = sysname, physical_bytes = NA_real_, free_bytes = NA_real_, inactive_bytes = NA_real_,
    free_inactive_bytes = NA_real_, swap_total_bytes = NA_real_, swap_used_bytes = NA_real_,
    pressure = NA_character_)
  if (identical(sysname, "darwin")) {
    memsize <- margot_lmtp_preflight_command("sysctl", c("-n", "hw.memsize"))
    if (length(memsize)) host$physical_bytes <- suppressWarnings(as.numeric(trimws(memsize[[1L]])))
    vm <- margot_lmtp_parse_vm_stat(margot_lmtp_preflight_command("vm_stat"))
    host[names(vm)] <- vm
    swap <- margot_lmtp_parse_swapusage(margot_lmtp_preflight_command("sysctl", "vm.swapusage"))
    host[names(swap)] <- swap
    pressure <- margot_lmtp_preflight_command("memory_pressure", "-Q")
    line <- grep("free percentage", pressure, value = TRUE)
    if (length(line)) host$pressure <- trimws(line[[1L]])
  } else if (identical(sysname, "linux")) {
    meminfo <- margot_lmtp_parse_meminfo(margot_lmtp_preflight_read("/proc/meminfo"))
    host[names(meminfo)] <- meminfo
    swaps <- margot_lmtp_parse_proc_swaps(margot_lmtp_preflight_read("/proc/swaps"))
    host[names(swaps)] <- swaps
    pressure <- margot_lmtp_preflight_read("/proc/pressure/memory")
    if (length(pressure)) host$pressure <- paste(trimws(pressure), collapse = "; ")
  }
  if (!is.na(host$free_bytes) && !is.na(host$inactive_bytes)) {
    host$free_inactive_bytes <- host$free_bytes + host$inactive_bytes
  }
  readings <- setdiff(names(host), "source")
  host$unavailable <- readings[vapply(host[readings], is.na, logical(1))]
  host
}

# macOS vm_stat: free and inactive pages times the reported page size.
margot_lmtp_parse_vm_stat <- function(lines) {
  if (!length(lines)) return(list())
  page <- suppressWarnings(as.numeric(sub(".*page size of ([0-9]+) bytes.*", "\\1",
    grep("page size of", lines, value = TRUE)[1L])))
  pages <- function(label) {
    line <- grep(paste0("^", label, ":"), lines, value = TRUE)
    if (!length(line)) return(NA_real_)
    suppressWarnings(as.numeric(gsub("[^0-9]", "", sub("^[^:]*:", "", line[[1L]]))))
  }
  if (is.na(page)) return(list())
  list(free_bytes = pages("Pages free") * page, inactive_bytes = pages("Pages inactive") * page)
}

# macOS `sysctl vm.swapusage`: "total = 2048.00M  used = 361.69M  free = ...".
margot_lmtp_parse_swapusage <- function(lines) {
  if (!length(lines)) return(list())
  value <- function(label) {
    m <- regmatches(lines[[1L]], regexec(paste0(label, " = ([0-9.]+)([KMGT])"), lines[[1L]]))[[1L]]
    if (length(m) != 3L) return(NA_real_)
    as.numeric(m[[2L]]) * 1024^match(m[[3L]], c("K", "M", "G", "T"))
  }
  list(swap_total_bytes = value("total"), swap_used_bytes = value("used"))
}

# linux /proc/meminfo, in kB.
margot_lmtp_parse_meminfo <- function(lines) {
  if (!length(lines)) return(list())
  field <- function(label) {
    line <- grep(paste0("^", label, ":"), lines, value = TRUE)
    if (!length(line)) return(NA_real_)
    suppressWarnings(as.numeric(gsub("[^0-9]", "", line[[1L]]))) * 1024
  }
  swap_total <- field("SwapTotal")
  list(physical_bytes = field("MemTotal"), free_bytes = field("MemFree"), inactive_bytes = field("Inactive"),
    swap_total_bytes = swap_total, swap_used_bytes = swap_total - field("SwapFree"))
}

# linux /proc/swaps: header, then one device per line with size and used in kB.
margot_lmtp_parse_proc_swaps <- function(lines) {
  if (!length(lines) || !grepl("^Filename", lines[[1L]])) return(list())
  rows <- strsplit(trimws(lines[-1L]), "[[:space:]]+")
  rows <- rows[lengths(rows) >= 4L]
  if (!length(rows)) return(list(swap_total_bytes = 0, swap_used_bytes = 0))
  number <- function(i) suppressWarnings(as.numeric(vapply(rows, `[[`, "", i)))
  list(swap_total_bytes = sum(number(3L)) * 1024, swap_used_bytes = sum(number(4L)) * 1024)
}

# process table (pid, ppid, command) or NULL when it cannot be read; overridden in tests.
margot_lmtp_preflight_processes <- function(sysname = margot_lmtp_preflight_sysname()) {
  if (!sysname %in% c("darwin", "linux")) return(NULL)
  out <- margot_lmtp_preflight_command("ps", c("-A", "-o", "pid=", "-o", "ppid=", "-o", "args="))
  if (is.null(out)) return(NULL)
  m <- regmatches(out, regexec("^[[:space:]]*([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+(.*)$", out))
  m <- m[lengths(m) == 4L]
  data.frame(pid = as.integer(vapply(m, `[[`, "", 2L)), ppid = as.integer(vapply(m, `[[`, "", 3L)),
    command = vapply(m, `[[`, "", 4L), stringsAsFactors = FALSE)
}

# other R processes, excluding `self` and its descendants, with their role. a parallelly
# worker is launched detached (its parent is process 1), so its owner is read from the
# `parallelly.parent=<pid>` tag on its command line; other processes are owned by their parent.
margot_lmtp_preflight_other_r <- function(processes, self) {
  if (is.null(processes)) return(NULL)
  stopifnot(is.data.frame(processes), all(c("pid", "ppid", "command") %in% names(processes)))
  empty <- data.frame(pid = integer(), ppid = integer(), owner_pid = integer(), role = character(),
    command = character(), stringsAsFactors = FALSE)
  if (!nrow(processes)) return(empty)
  # executable name: the first token of the command line, without its directory.
  executable <- basename(sub("[[:space:]].*$", "", sub("^[[:space:]]+", "", processes$command)))
  is_r <- executable %in% c("R", "Rscript", "Rterm", "rsession")
  tagged <- grepl("parallelly[.]parent=[0-9]+", processes$command)
  owner <- processes$ppid
  owner[tagged] <- as.integer(sub(".*parallelly[.]parent=([0-9]+).*", "\\1", processes$command[tagged]))
  own <- as.integer(self)
  repeat {
    children <- processes$pid[owner %in% own & !processes$pid %in% own]
    if (!length(children)) break
    own <- c(own, children)
  }
  keep <- is_r & !processes$pid %in% own
  if (!any(keep)) return(empty)
  found <- processes[keep, c("pid", "ppid", "command"), drop = FALSE]
  found$owner_pid <- owner[keep]
  worker <- grepl("workRSOCK", found$command, fixed = TRUE)
  orphaned <- worker & (found$owner_pid == 1L | !found$owner_pid %in% processes$pid)
  found$role <- ifelse(orphaned, "orphaned_worker", ifelse(worker, "worker", "r_process"))
  rownames(found) <- NULL
  found[, c("pid", "ppid", "owner_pid", "role", "command")]
}

#' @rdname margot_lmtp_memory_preflight
#' @param x A `margot_lmtp_memory_preflight` receipt.
#' @param ... Unused.
#' @md
#' @export
print.margot_lmtp_memory_preflight <- function(x, ...) {
  gib <- margot_lmtp_format_bytes
  cat("LMTP memory preflight: ", if (x$passed) "passed" else "refused", "\n", sep = "")
  cat("Created: ", x$created_utc, "; host ", x$system$nodename, " (", x$system$sysname, "), pid ",
    x$system$pid, "\n", sep = "")
  cat(x$system$r_version, "; margot ", x$system$margot_version, "\n\n", sep = "")
  cat("Host\n")
  cat("  physical memory:       ", gib(x$host$physical_bytes), "\n", sep = "")
  cat("  free plus inactive:    ", gib(x$host$free_inactive_bytes), "\n", sep = "")
  cat("  swap in use:           ", gib(x$host$swap_used_bytes), " of ", gib(x$host$swap_total_bytes), "\n", sep = "")
  cat("  memory pressure:       ", if (is.na(x$host$pressure)) "unavailable" else x$host$pressure, "\n", sep = "")
  if (length(x$host$unavailable)) cat("  unavailable readings:  ", paste(x$host$unavailable, collapse = ", "), "\n", sep = "")
  cat("\nOther R processes\n")
  if (is.null(x$processes)) {
    cat("  process listing unavailable\n")
  } else if (!nrow(x$processes)) {
    cat("  none\n")
  } else {
    for (i in seq_len(nrow(x$processes))) {
      p <- x$processes[i, ]
      cat("  ", p$pid, " (parent ", p$ppid, ", ", p$role, "): ", substr(p$command, 1L, 120L), "\n", sep = "")
    }
  }
  e <- x$estimate
  cat("\nEstimate: ", gib(e$total_bytes), "\n", sep = "")
  cat("  coordinator:           ", gib(e$components[["coordinator"]]), "\n", sep = "")
  cat("  largest task (transient): ", gib(e$components[["largest_task"]]), "\n", sep = "")
  cat("  ", e$inputs$workers, " worker(s):           ", gib(e$components[["workers"]]), "\n", sep = "")
  cat("  formula: ", e$formula, "\n", sep = "")
  cat("\nRules\n")
  for (i in seq_len(nrow(x$rules))) {
    cat("  ", x$rules$rule[[i]], ": ", x$rules$status[[i]], " (", x$rules$detail[[i]], ")\n", sep = "")
  }
  invisible(x)
}
