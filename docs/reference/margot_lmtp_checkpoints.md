# Save and reuse verified LMTP task results

A caller defines each task's complete scientific and runtime identity. A
completed result is reused only after checking that identity and the
payload checksum. Computation and installation hold exclusive local
ownership. An existing ownership directory requires inspection; locks
never expire automatically. These functions do not define estimators or
schedule workers.

## Usage

``` r
margot_lmtp_checkpoint_read(directory, key, identity)

margot_lmtp_checkpoint_compute(directory, key, identity, compute)

margot_lmtp_checkpoint_inventory(directory, identities, load = TRUE)
```

## Arguments

- directory:

  Private directory for these tasks.

- key:

  Unique task name using lowercase letters, digits and underscores.

- identity:

  Nonempty list identifying all result-affecting inputs, ordered
  participants, histories, folds, weights, settings and code/builds.
  Environments, closures and external pointers are refused; pass a
  fingerprint of a function instead of the function.

- compute:

  Zero-argument function computing a missing task.

- identities:

  Named list of identities for the complete expected task set.

- load:

  For `inventory`, whether to return each verified value. With
  `load = FALSE`, every file is still verified but records hold metadata
  only (path and checksums), so the inventory does not hold the saved
  results in memory. The default `TRUE` keeps the earlier behaviour.

## Value

`read` returns a verified record or `NULL`; `compute` returns a record
containing `value`, `path`, `reused` and checksums; `inventory` returns
completion status, a task table and verified records, which hold `value`
only when `load = TRUE`.

## Details

Identities are compared in canonical form (see
[`margot_lmtp_checkpoint_identity()`](https://go-bayes.github.io/margot/reference/margot_lmtp_checkpoint_identity.md)),
so attributes that record where an input came from, such as the file
path RcppTOML attaches to a parsed study file, do not change a task's
identity.
