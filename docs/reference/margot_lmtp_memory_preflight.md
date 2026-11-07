# Check memory before launching an LMTP run

`margot_lmtp_memory_preflight()` checks, before a run is launched, that
the host is clean and that a measured memory estimate fits within an
administrative ceiling. It returns a receipt and stops with a classed
error when a rule fails. It reads the host and lists processes; it never
stops, signals or kills a process, and it does not set memory limits or
monitor a running job.

## Usage

``` r
margot_lmtp_memory_preflight(
  coordinator_bytes,
  largest_task_value_bytes,
  largest_task_payload_bytes,
  workers,
  worker_data_bytes,
  worker_fold_fit_bytes,
  concurrent_tasks = 1L,
  ceiling = 0.7,
  swap_limit_bytes = 2^30,
  allow_other_r = FALSE,
  other_r_reason = NULL,
  allow_unreadable_processes = FALSE,
  unreadable_processes_reason = NULL,
  receipt_path = NULL
)

# S3 method for class 'margot_lmtp_memory_preflight'
print(x, ...)
```

## Arguments

- coordinator_bytes:

  Measured bytes held by the coordinating process: its data and the lean
  results it keeps.

- largest_task_value_bytes:

  Measured in-memory bytes of the largest single task's computed value.

- largest_task_payload_bytes:

  Measured serialised bytes of that value.

- workers:

  Number of workers, `K`, each holding one copy of the data and one
  outer fold's fit.

- worker_data_bytes:

  Measured bytes of the data each worker holds.

- worker_fold_fit_bytes:

  Measured bytes of one outer fold's full fit on a worker.

- concurrent_tasks:

  Number of task results that can be written at the same time, for
  example `K` on the task-parallel route. The largest-task term is
  multiplied by it. The default is 1.

- ceiling:

  Largest permitted fraction of physical memory for the estimate. The
  default 0.70 is a provisional administrative ceiling, not a validated
  bound.

- swap_limit_bytes:

  Largest permitted swap in use, in bytes. The default is 1 GiB (`2^30`
  bytes).

- allow_other_r:

  Whether to proceed when other R processes, R ancestors or orphaned
  workers are present. Requires `other_r_reason`.

- other_r_reason:

  A non-empty reason, recorded in the receipt, for allowing other R
  processes.

- allow_unreadable_processes:

  Whether to proceed when the process listing cannot be read. Requires
  `unreadable_processes_reason`.

- unreadable_processes_reason:

  A non-empty reason, recorded in the receipt, for proceeding without a
  process listing.

- receipt_path:

  Optional path without extension. When given, the receipt is written to
  `<receipt_path>.rds` and a readable `<receipt_path>.txt`, whether the
  preflight passes or refuses. The receipt records the host name,
  process identifiers and command lines, so keep it in private storage.

- x:

  A `margot_lmtp_memory_preflight` receipt.

- ...:

  Unused.

## Value

A `margot_lmtp_memory_preflight` receipt, a list with `passed`,
`created_utc`, `system` (host name, platform, process identifier, R and
margot versions), `host` (memory and swap readings in bytes, pressure
and the readings that were unavailable), `processes` (a data frame of
other R processes with their parent, owner, role and resident bytes, or
`NULL` when the listing is unavailable), `estimate` (inputs, components,
total, formula and its basis), `rules` (one row per stop rule with its
status and detail), `warnings` (a character vector, empty when there are
none) and `settings`, and, when `receipt_path` is given, `files`. On
refusal the function stops with an error of class
`margot_lmtp_memory_preflight_error` whose `receipt` element holds the
same receipt.

## Details

**Clean host.** The receipt records physical memory, free plus inactive
memory, memory pressure and swap in use, and lists other R processes
with their resident memory. On macOS these come from
`/usr/sbin/sysctl hw.memsize`, `/usr/bin/vm_stat`,
`/usr/bin/memory_pressure -Q` and `/usr/sbin/sysctl vm.swapusage`; on
Linux from `/proc/meminfo`, `/proc/swaps` and, when present,
`/proc/pressure/memory`. Processes are listed with `/bin/ps` on both. A
command found at another location on the search path is used when the
standard path is absent.

An R process is one whose executable is `R`, `Rscript`, `Rterm` or
`rsession`. R embedded in another executable, such as an IDE or notebook
kernel or radian running under Python, is not detected. A parallelly or
future PSOCK worker is an R process whose command line runs `workRSOCK`.
A parallelly worker is launched detached, so its owner is read from the
`parallelly.parent=<pid>` tag on its command line; any other process is
owned by its parent. A tagged owner is accepted only if it is running
and started no later than the worker, so a stale tag naming a process
identifier that has since been reused, including the current one, does
not make an orphan look owned. A worker is orphaned when it has no
running owner. The current process, its descendants and the workers it
owns are excluded. Its R ancestors are reported with the role `ancestor`
and count as other R processes, so an R process that launched this one
must be allowed explicitly. Workers started by base
[`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html)
carry no owner tag and may be reported as orphaned even when the current
process started them, so run the preflight before creating workers or
setting a `future` plan. On another platform, or when a command fails,
the affected readings are recorded as unavailable and are not estimated.

**Measured estimate.** The estimate, in bytes, is

`coordinator_bytes + concurrent_tasks * (2 * max(largest_task_value_bytes, largest_task_payload_bytes) + 10 * largest_task_payload_bytes) + workers * (worker_data_bytes + worker_fold_fit_bytes)`.

The middle term bounds the growth of R's heap while the largest task's
checkpoint is written by
[`margot_lmtp_checkpoint_compute()`](https://go-bayes.github.io/margot/reference/margot_lmtp_checkpoints.md).
It counts every allocation of the write as if none were released: the
computed value, its serialised payload and the payload's checksum
serialisation; in the atomic writer, the read-back of the scratch file,
the read-back of the installed file and four fingerprint serialisations;
and, in the final read, the re-read record, its checksum serialisation
and the unserialised value. Each of the two copies of the value is taken
as the larger of its in-memory and serialised sizes, because
[`object.size()`](https://rdrr.io/r/utils/object.size.html) misses data
held in environments. The bound does not depend on when R collects
garbage; it leaves out the write's small allocations (identities,
checksums, lock records and function frames), about a megabyte in tests,
which matter only for small tasks. Fewer copies are live at once: after
the value is released, the writer holds the record, two read-backs and a
fingerprint serialisation together, `max(value + payload, 4 * payload)`,
which is recorded in the receipt as `largest_task_live_bytes`. On
generated values of 50 to 300 MB, R's maximum heap use during the write
rose by 4 to 5 payloads, so the bound is conservative by a factor of
about 2 to 3; a single fixed multiple of the payload was exceeded at
some sizes because collection timing varies. The payload carries most of
the term, so measure `serialised_bytes` with
[`margot_lmtp_object_size()`](https://go-bayes.github.io/margot/reference/margot_lmtp_object_size.md).
`concurrent_tasks` is the number of task results that can be written at
the same time, for example `K` on the task-parallel route. The last term
is one copy of the data and one outer fold's fit for each of the
`workers` workers.

Every size is supplied by the caller and should be measured with
[`margot_lmtp_object_size()`](https://go-bayes.github.io/margot/reference/margot_lmtp_object_size.md)
on lean-loaded checkpoints and on full fits from a generated fixture at
the study's n and p. The formula is an accounting of known copies, not a
validated bound: it omits the observed process-tree peak, temporary
allocations inside learners and the memory of other processes.

**Stop rules.** The preflight refuses when (1) the estimate exceeds
`ceiling` times physical memory; (2) another R process, an R ancestor or
an orphaned worker is present, unless `allow_other_r = TRUE` with a
reason; (3) the process listing cannot be read, unless
`allow_unreadable_processes = TRUE` with a reason; or (4) swap in use
exceeds `swap_limit_bytes`. It also refuses when physical memory or swap
in use cannot be read, because an unmeasured host cannot be shown to be
clean; there is no override for those. The default `ceiling = 0.70` is a
provisional administrative ceiling, not a validated bound. An estimate
above free plus inactive memory is recorded as a warning in the receipt,
not a refusal, because the operating system can usually reclaim other
memory.

## See also

[`margot_lmtp_object_size()`](https://go-bayes.github.io/margot/reference/margot_lmtp_object_size.md),
[`margot_lmtp_assert_lean()`](https://go-bayes.github.io/margot/reference/margot_lmtp_assert_lean.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit_size <- margot_lmtp_object_size(fixture_fit)
margot_lmtp_memory_preflight(
  coordinator_bytes = margot_lmtp_object_size(prepared)[["memory_bytes"]],
  largest_task_value_bytes = fit_size[["memory_bytes"]],
  largest_task_payload_bytes = fit_size[["serialised_bytes"]],
  workers = 5,
  worker_data_bytes = margot_lmtp_object_size(prepared)[["memory_bytes"]],
  worker_fold_fit_bytes = fold_fit_bytes,
  receipt_path = "private/receipts/preflight-outcomes"
)
} # }
```
