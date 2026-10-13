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

margot_lmtp_checkpoint_inventory(directory, identities)
```

## Arguments

- directory:

  Private directory for these tasks.

- key:

  Unique task name using lowercase letters, digits and underscores.

- identity:

  Nonempty list identifying all result-affecting inputs, ordered
  participants, histories, folds, weights, settings and code/builds.

- compute:

  Zero-argument function computing a missing task.

- identities:

  Named list of identities for the complete expected task set.

## Value

`read` returns a verified record or `NULL`; `compute` returns a record
containing `value`, `path`, `reused` and checksums; `inventory` returns
completion status, a task table and verified records.
