# generated sizes and mocked host readings only; nothing here fits a model.

gib <- 2^30

small_sizes <- function(...) {
  utils::modifyList(list(coordinator_bytes = 1 * gib, largest_task_value_bytes = 0.5 * gib,
    largest_task_payload_bytes = 0.25 * gib, workers = 4, worker_data_bytes = 0.5 * gib,
    worker_fold_fit_bytes = 1 * gib), list(...))
}

clean_host <- function(...) {
  utils::modifyList(list(source = "darwin", physical_bytes = 64 * gib, free_bytes = 30 * gib,
    inactive_bytes = 10 * gib, free_inactive_bytes = 40 * gib, swap_total_bytes = 2 * gib,
    swap_used_bytes = 0, pressure = "System-wide memory free percentage: 80%", unavailable = character()),
    list(...))
}

self_only <- function(...) {
  data.frame(pid = c(1L, 500L, Sys.getpid(), 9001L), ppid = c(0L, 1L, 500L, Sys.getpid()),
    command = c("/sbin/launchd", "-zsh", "/usr/lib/R/bin/exec/R --no-echo --file=run.R",
      "/usr/lib/R/bin/exec/R --no-echo -e workRSOCK <- tryCatch(parallel:::.workRSOCK) MASTER=localhost PORT=1"),
    stringsAsFactors = FALSE)
}

run_preflight <- function(..., host = clean_host(), processes = self_only()) {
  local_mocked_bindings(
    margot_lmtp_preflight_sysname = function() "darwin",
    margot_lmtp_preflight_host = function(sysname) host,
    margot_lmtp_preflight_processes = function(sysname) processes,
    .package = "margot"
  )
  do.call(margot_lmtp_memory_preflight, utils::modifyList(small_sizes(), list(...)))
}

refusal <- function(expr) {
  tryCatch({
    expr
    NULL
  }, margot_lmtp_memory_preflight_error = function(e) e)
}

test_that("object size reports memory and serialised bytes", {
  x <- matrix(stats::rnorm(2000), ncol = 20)
  size <- margot_lmtp_object_size(x)
  expect_named(size, c("memory_bytes", "serialised_bytes"))
  expect_equal(size[["memory_bytes"]], as.numeric(utils::object.size(x)))
  expect_equal(size[["serialised_bytes"]], length(serialize(x, NULL, version = 3L)))
})

test_that("the estimate follows the stated formula on supplied sizes", {
  receipt <- run_preflight()
  e <- receipt$estimate
  expect_equal(e$components[["coordinator"]], 1 * gib)
  # 2 * max(value, payload) + 10 * payload = 2 * 0.5 + 10 * 0.25 GiB.
  expect_equal(e$components[["largest_task"]], 3.5 * gib)
  expect_equal(e$largest_task_live_bytes, max(0.75 * gib, 1 * gib))
  expect_equal(e$components[["workers"]], 4 * (0.5 * gib + 1 * gib))
  expect_equal(e$total_bytes, 1 * gib + 3.5 * gib + 6 * gib)
  expect_match(e$formula, "10 * largest_task_payload_bytes", fixed = TRUE)
  expect_match(e$basis, "object.size", fixed = TRUE)
  expect_true(receipt$passed)
  expect_identical(receipt$rules$status, c("pass", "pass", "pass"))
  expect_identical(receipt$warnings, character())
  # a value held in an environment measures near zero in memory; its copies are taken at payload size.
  env_value <- run_preflight(largest_task_value_bytes = 0)$estimate
  expect_equal(env_value$components[["largest_task"]], 12 * 0.25 * gib)
  big_value <- run_preflight(largest_task_value_bytes = 2 * gib)$estimate
  expect_equal(big_value$components[["largest_task"]], 4 * gib + 2.5 * gib)
  expect_equal(big_value$largest_task_live_bytes, 2.25 * gib)
  # concurrent writes multiply the largest-task term.
  three <- run_preflight(concurrent_tasks = 3L)$estimate
  expect_equal(three$components[["largest_task"]], 3 * 3.5 * gib)
  expect_equal(three$inputs$concurrent_tasks, 3L)
  expect_error(run_preflight(concurrent_tasks = 0), "concurrent_tasks")
})

test_that("the largest-task term covers the heap peak of a checkpoint write", {
  skip_on_cran()
  # the value's data sit in an environment, so object.size() sees almost nothing of them.
  make_value <- function() {
    holder <- new.env()
    holder$x <- stats::rnorm(2e7)
    list(holder = holder)
  }
  set.seed(20261104)
  size <- margot_lmtp_object_size(make_value())
  expect_lt(size[["memory_bytes"]], 1e4)
  expect_gt(size[["serialised_bytes"]], 1.5e8)
  directory <- withr::local_tempdir()
  # load and compile the checkpoint code first so its own allocations are not counted.
  margot_lmtp_checkpoint_compute(directory, "warm", list(k = 0L), function() list(a = 1))
  before <- gc(reset = TRUE)
  result <- margot_lmtp_checkpoint_compute(directory, "task", list(k = 1L), make_value)
  after <- gc()
  # vector-heap growth in bytes: "max used" cells after, less cells in use before, at 8 bytes a cell.
  # columns by name: gc() omits its limit column when no memory limit is set.
  peak <- (after["Vcells", "max used"] - before["Vcells", "used"]) * 8
  rm(result)
  term <- margot_lmtp_memory_estimate(list(coordinator_bytes = 0, largest_task_value_bytes = size[["memory_bytes"]],
    largest_task_payload_bytes = size[["serialised_bytes"]], worker_data_bytes = 0, worker_fold_fit_bytes = 0,
    workers = 1L, concurrent_tasks = 1L))$components[["largest_task"]]
  expect_lte(peak, term)
  # at least the live copies were held at once, more than the earlier term of twice value plus
  # twice payload, which object.size() reduced to about two payloads here.
  expect_gt(peak, 2 * size[["memory_bytes"]] + 2 * size[["serialised_bytes"]])
})

test_that("an estimate above free plus inactive memory is a warning, not a refusal", {
  receipt <- run_preflight(host = clean_host(free_inactive_bytes = 4 * gib))
  expect_true(receipt$passed)
  expect_length(receipt$warnings, 1L)
  expect_match(receipt$warnings, "free plus inactive")
  expect_output(print(receipt), "Warnings")
})

test_that("the estimate is refused above the ceiling", {
  # 64 GiB physical, ceiling 0.70 = 44.8 GiB; 10 workers of 4.5 GiB plus 4.5 GiB exceed it.
  err <- refusal(run_preflight(workers = 10, worker_data_bytes = 0.5 * gib, worker_fold_fit_bytes = 4 * gib))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  expect_match(conditionMessage(err), "memory_ceiling")
  expect_false(err$receipt$passed)
  expect_identical(err$receipt$rules$status[err$receipt$rules$rule == "memory_ceiling"], "fail")
  # a higher ceiling is the caller's explicit choice.
  expect_true(run_preflight(workers = 10, worker_fold_fit_bytes = 4 * gib, ceiling = 0.9)$passed)
  expect_error(run_preflight(ceiling = 1.5), "ceiling")
})

test_that("other R processes and orphaned workers are refused unless allowed with a reason", {
  others <- rbind(self_only(), data.frame(
    pid = c(7001L, 7002L, 7003L), ppid = c(1L, 7001L, 1L),
    command = c("/Library/Frameworks/R.framework/Resources/bin/exec/R --no-echo --file=other.R",
      "/Library/Frameworks/R.framework/Resources/bin/exec/R -e workRSOCK MASTER=localhost",
      "/usr/local/bin/Rscript -e parallel:::.workRSOCK() MASTER=localhost PORT=11000"),
    stringsAsFactors = FALSE))
  err <- refusal(run_preflight(processes = others))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  p <- err$receipt$processes
  # the current process and its own worker (pid 9001) are excluded.
  expect_setequal(p$pid, c(7001L, 7002L, 7003L))
  expect_identical(p$role[match(c(7001L, 7002L, 7003L), p$pid)], c("r_process", "worker", "orphaned_worker"))
  expect_identical(err$receipt$rules$status[err$receipt$rules$rule == "other_r"], "fail")

  expect_error(run_preflight(processes = others, allow_other_r = TRUE), "other_r_reason")
  expect_error(run_preflight(processes = others, allow_other_r = TRUE, other_r_reason = " "), "other_r_reason")
  allowed <- run_preflight(processes = others, allow_other_r = TRUE, other_r_reason = "monitored sibling run")
  expect_true(allowed$passed)
  expect_identical(allowed$rules$status[allowed$rules$rule == "other_r"], "allowed")
  expect_match(allowed$rules$detail[allowed$rules$rule == "other_r"], "monitored sibling run")
  expect_identical(allowed$settings$other_r_reason, "monitored sibling run")
  # the separate override for an unreadable listing does not cover other R processes.
  expect_s3_class(refusal(run_preflight(processes = others, allow_unreadable_processes = TRUE,
    unreadable_processes_reason = "not needed")), "margot_lmtp_memory_preflight_error")

  # a worker whose parent no longer exists is orphaned even if not reparented to process 1.
  lost <- rbind(self_only(), data.frame(pid = 8001L, ppid = 4242L,
    command = "/usr/lib/R/bin/exec/R -e workRSOCK", stringsAsFactors = FALSE))
  expect_identical(refusal(run_preflight(processes = lost))$receipt$processes$role, "orphaned_worker")

  # parallelly workers are detached (parent 1); their owner is read from the parallelly.parent tag.
  tag <- function(owner) paste0("/usr/lib/R/bin/exec/R -e try(cat(Sys.getpid(),file='/tmp/worker.rank=1.parallelly.parent=",
    owner, ".1a2b.pid')) -e workRSOCK<-tryCatch(parallel:::.workRSOCK);workRSOCK() --args MASTER=localhost")
  detached <- rbind(self_only(), data.frame(pid = c(8101L, 8102L, 8103L, 7001L), ppid = c(1L, 1L, 1L, 1L),
    command = c(tag(Sys.getpid()), tag(7001L), tag(4343L), "/usr/lib/R/bin/exec/R --file=sibling.R"),
    stringsAsFactors = FALSE))
  p <- refusal(run_preflight(processes = detached))$receipt$processes
  # the current process's own detached worker (8101) is excluded.
  expect_setequal(p$pid, c(8102L, 8103L, 7001L))
  expect_identical(p$owner_pid[match(c(8102L, 8103L), p$pid)], c(7001L, 4343L))
  expect_identical(p$role[match(c(8102L, 8103L, 7001L), p$pid)], c("worker", "orphaned_worker", "r_process"))

  # a worker tagged with the current pid but older than the current process is a stale orphan
  # from an earlier process with the same identifier.
  aged <- self_only()
  aged$elapsed <- c(9e5, 9e5, 100, 50)
  reused <- rbind(aged, data.frame(pid = 8201L, ppid = 1L, elapsed = 5000, command = tag(Sys.getpid()),
    stringsAsFactors = FALSE))
  p <- refusal(run_preflight(processes = reused))$receipt$processes
  expect_identical(p$pid, 8201L)
  expect_identical(p$role, "orphaned_worker")
  # the same tag on a worker younger than the current process is the current process's own.
  reused$elapsed[reused$pid == 8201L] <- 30
  expect_true(run_preflight(processes = reused)$passed)
})

test_that("an R ancestor of the current process is counted with its own role", {
  family <- data.frame(pid = c(1L, 400L, 450L, Sys.getpid()), ppid = c(0L, 1L, 400L, 450L),
    rss = c(1000, 204800, 1000, 51200),
    command = c("/sbin/launchd", "/usr/lib/R/bin/exec/R --file=launcher.R", "sh -c Rscript run.R",
      "/usr/lib/R/bin/exec/R --file=run.R"), stringsAsFactors = FALSE)
  err <- refusal(run_preflight(processes = family))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  p <- err$receipt$processes
  expect_identical(p$pid, 400L)
  expect_identical(p$role, "ancestor")
  # resident memory is reported in bytes from ps's kilobytes.
  expect_equal(p$rss_bytes, 204800 * 1024)
  expect_match(err$receipt$rules$detail[err$receipt$rules$rule == "other_r"], "1 ancestor")
  expect_true(run_preflight(processes = family, allow_other_r = TRUE, other_r_reason = "launched by an R driver")$passed)
})

test_that("an unreadable process listing needs its own override", {
  err <- refusal(run_preflight(processes = NULL))
  expect_identical(err$receipt$rules$status[err$receipt$rules$rule == "other_r"], "unavailable")
  # allowing other R processes does not cover an unreadable listing.
  err <- refusal(run_preflight(processes = NULL, allow_other_r = TRUE, other_r_reason = "sibling"))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  expect_error(run_preflight(processes = NULL, allow_unreadable_processes = TRUE), "unreadable_processes_reason")
  allowed <- run_preflight(processes = NULL, allow_unreadable_processes = TRUE,
    unreadable_processes_reason = "ps is unavailable in this container")
  expect_true(allowed$passed)
  expect_match(allowed$rules$detail[allowed$rules$rule == "other_r"], "container")
})

test_that("swap in use above the limit is refused", {
  err <- refusal(run_preflight(host = clean_host(swap_used_bytes = 1.5 * gib)))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  expect_match(conditionMessage(err), "swap")
  expect_true(run_preflight(host = clean_host(swap_used_bytes = 1.5 * gib), swap_limit_bytes = 2 * gib)$passed)
})

test_that("an unsupported platform records unavailable readings and refuses", {
  local_mocked_bindings(margot_lmtp_preflight_sysname = function() "windows", .package = "margot")
  err <- refusal(do.call(margot_lmtp_memory_preflight, small_sizes()))
  expect_s3_class(err, "margot_lmtp_memory_preflight_error")
  r <- err$receipt
  expect_identical(r$system$sysname, "windows")
  expect_true(is.na(r$host$physical_bytes))
  expect_true(all(c("physical_bytes", "swap_used_bytes", "free_inactive_bytes", "pressure") %in% r$host$unavailable))
  expect_null(r$processes)
  expect_identical(r$rules$status, c("unavailable", "unavailable", "unavailable"))
  # the listing override covers the unreadable listing but not the unmeasured memory or swap.
  err <- refusal(do.call(margot_lmtp_memory_preflight, c(small_sizes(), list(allow_unreadable_processes = TRUE,
    unreadable_processes_reason = "no process listing on this platform"))))
  expect_identical(err$receipt$rules$status, c("unavailable", "allowed", "unavailable"))
})

test_that("the receipt is written as rds and text, whether passed or refused", {
  dir <- withr::local_tempdir()
  receipt <- run_preflight(receipt_path = file.path(dir, "pass"))
  expect_true(all(file.exists(file.path(dir, c("pass.rds", "pass.txt")))))
  saved <- readRDS(file.path(dir, "pass.rds"))
  expect_s3_class(saved, "margot_lmtp_memory_preflight")
  expect_identical(saved$estimate, receipt$estimate)
  expect_true(any(grepl("LMTP memory preflight: passed", readLines(file.path(dir, "pass.txt")))))
  refusal(run_preflight(host = clean_host(swap_used_bytes = 5 * gib), receipt_path = file.path(dir, "refused")))
  expect_true(any(grepl("refused", readLines(file.path(dir, "refused.txt")))))
  expect_false(readRDS(file.path(dir, "refused.rds"))$passed)
})

test_that("inputs are validated", {
  expect_error(run_preflight(coordinator_bytes = -1), "coordinator_bytes")
  expect_error(run_preflight(largest_task_value_bytes = NA_real_), "largest_task_value_bytes")
  expect_error(run_preflight(worker_fold_fit_bytes = "1"), "worker_fold_fit_bytes")
  expect_error(run_preflight(workers = 0), "workers")
  expect_error(run_preflight(workers = 2.5), "workers")
  expect_error(run_preflight(allow_other_r = NA), "allow_other_r")
})

test_that("macOS and Linux readings parse into bytes", {
  vm <- margot_lmtp_parse_vm_stat(c("Mach Virtual Memory Statistics: (page size of 16384 bytes)",
    "Pages free:                                   172199.", "Pages active:      646967.",
    "Pages inactive:                               674975."))
  expect_equal(vm$free_bytes, 172199 * 16384)
  expect_equal(vm$inactive_bytes, 674975 * 16384)
  swap <- margot_lmtp_parse_swapusage("vm.swapusage: total = 2048.00M  used = 361.69M  free = 1686.31M  (encrypted)")
  expect_equal(swap$swap_total_bytes, 2048 * 1024^2)
  expect_equal(swap$swap_used_bytes, 361.69 * 1024^2)
  meminfo <- margot_lmtp_parse_meminfo(c("MemTotal:       65536000 kB", "MemFree:        20000000 kB",
    "MemAvailable:   40000000 kB", "Inactive:       10000000 kB", "SwapTotal:       2097148 kB",
    "SwapFree:        2000000 kB"))
  expect_equal(meminfo$physical_bytes, 65536000 * 1024)
  expect_equal(meminfo$inactive_bytes, 10000000 * 1024)
  expect_equal(meminfo$swap_used_bytes, (2097148 - 2000000) * 1024)
  swaps <- margot_lmtp_parse_proc_swaps(c("Filename\t\t\t\tType\t\tSize\t\tUsed\t\tPriority",
    "/swap.img                               file\t\t2097148\t\t97148\t\t-2"))
  expect_equal(swaps$swap_used_bytes, 97148 * 1024)
  expect_equal(margot_lmtp_parse_proc_swaps("Filename Type Size Used Priority")$swap_used_bytes, 0)
  expect_identical(margot_lmtp_parse_vm_stat(NULL), list())
  gig <- margot_lmtp_parse_swapusage("vm.swapusage: total = 4.00G  used = 1.50G  free = 2.50G  (encrypted)")
  expect_equal(gig$swap_total_bytes, 4 * 1024^3)
  expect_equal(gig$swap_used_bytes, 1.5 * 1024^3)
  expect_equal(margot_lmtp_parse_etime(c("00:07", "12:34", "01:02:03", "2-03:04:05", "bad")),
    c(7, 754, 3723, 2 * 86400 + 3 * 3600 + 4 * 60 + 5, NA))
})

test_that("Linux readings come from the real /proc", {
  skip_if_not(margot_lmtp_preflight_sysname() == "linux", "reads /proc on Linux only")
  host <- margot_lmtp_preflight_host("linux")
  expect_gt(host$physical_bytes, 0)
  expect_gt(host$free_inactive_bytes, 0)
  expect_false(is.na(host$swap_used_bytes))
  expect_false("physical_bytes" %in% host$unavailable)
  processes <- margot_lmtp_preflight_processes("linux")
  expect_true(Sys.getpid() %in% processes$pid)
})

test_that("the current platform produces a complete receipt", {
  skip_on_cran()
  sysname <- margot_lmtp_preflight_sysname()
  skip_if_not(sysname %in% c("darwin", "linux"), "preflight readings are implemented for macOS and Linux")
  # the host may hold other R processes or swap, so read the receipt from either outcome.
  receipt <- tryCatch(do.call(margot_lmtp_memory_preflight, small_sizes(workers = 1)),
    margot_lmtp_memory_preflight_error = function(e) e$receipt)
  expect_s3_class(receipt, "margot_lmtp_memory_preflight")
  expect_named(receipt, c("schema", "passed", "created_utc", "system", "host", "processes", "estimate",
    "rules", "warnings", "settings"))
  expect_identical(receipt$system$sysname, sysname)
  expect_true(receipt$host$physical_bytes > 0)
  expect_true(receipt$host$free_inactive_bytes > 0)
  expect_false(is.na(receipt$host$swap_used_bytes))
  expect_s3_class(receipt$processes, "data.frame")
  expect_false(Sys.getpid() %in% receipt$processes$pid)
  expect_true("rss_bytes" %in% names(receipt$processes))
  listing <- margot_lmtp_preflight_processes(sysname)
  expect_true(all(c("rss", "elapsed") %in% names(listing)))
  expect_false(is.na(listing$elapsed[listing$pid == Sys.getpid()]))
  expect_setequal(receipt$rules$rule, c("memory_ceiling", "other_r", "swap"))
  expect_output(print(receipt), "LMTP memory preflight")
})
