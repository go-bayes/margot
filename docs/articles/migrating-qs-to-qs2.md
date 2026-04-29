# Migrating from \`qs\` to \`qs2\`

## Why this matters

Earlier versions of margot used the `qs` package to write fast,
compressed serialisations of fitted models, plot lists, lmtp results,
and other analysis artefacts. Many lab projects accumulated archives of
`.qs` files this way.

`qs` was archived from CRAN in 2025 because it depends on R-internal C
symbols (`LEVELS`, `OBJECT`, `ENCLOS`, `Rf_allocSExp`, …) that **R 4.6
removed**. As a result `qs` cannot be built or installed on R 4.6+ —
even from GitHub source.

margot 1.0.318 switches to **`qs2`** (the same author’s actively
maintained successor) and ships two migration tools so existing `.qs`
archives remain accessible.

## What changed in margot

- `here_save_qs(obj, name, dir_path)` now writes **`.qs2`** files via
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html).
- `here_read_qs(name, dir_path)` looks for `<name>.qs2` first and falls
  back to `<name>.qs` (legacy) when both are present and the optional
  `qs` package is installed.
- `here_save_arrow(obj, name)` now round-trips arbitrary R objects — not
  just data frames. Lists, fitted models, ggplots, and result objects
  are wrapped in a single-row “margot envelope” parquet.
- `qs` is now a `Suggests`, not an `Imports`. Fresh installs of margot
  on R 4.6+ no longer fail.

These changes are backward-compatible: pipelines that previously called
`here_save_qs(out, "models")` and then `here_read_qs("models")` continue
to work with no code changes; the file extension on disk goes from `.qs`
to `.qs2` for new saves.

## Decision tree

    You have legacy .qs files you need to read.
    │
    ├── Do you have `qs` installed and working?  ────  yes  ──>  margot_convert_qs_dir(dir)
    │
    ├── Do you have an R 4.5 environment available  ───  yes  ──>  in that R session:
    │   (rig, separate install, colleague's laptop)?              install qs, then
    │                                                             margot_convert_qs_dir(dir)
    │
    └── Are you on R 4.6+ with no qs?  ───────────────  yes  ──>  margot_convert_qs_dir_docker(dir)
                                                                  (needs Docker)

The third branch is what most users will hit.

## Path A: direct conversion (machines that still have `qs`)

If [`library(qs)`](https://rdrr.io/r/base/library.html) works on your
machine, the simplest route is a single call:

``` r

margot::margot_convert_qs_dir(
  "~/path/to/your/cache/dir",
  delete_qs = FALSE   # leave originals while you spot-check
)
```

This walks the directory recursively, reads each `.qs` with
`qs::qread()`, writes a `.qs2` sibling with
[`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html), and does
an [`identical()`](https://rdrr.io/r/base/identical.html) read-verify.
Originals are kept by default. Re-run with `delete_qs = TRUE` once
you’re satisfied.

## Path B: conversion via Docker (R 4.6+ users)

If you are on R 4.6+ and `qs` will not install, run the conversion
inside a Docker container. margot does the heavy lifting; you only need
a working Docker engine on your machine.

### Prerequisites at a glance

|  | What you need | How to check |
|----|----|----|
| 1 | A working Docker engine | `docker info` shows a “Server” section |
| 2 | `margot` \>= 1.0.318 in your R session | `packageVersion("margot") >= "1.0.318"` |
| 3 | `qs2` in your host R session | [`requireNamespace("qs2")`](https://github.com/qsbase/qs2) |
| 4 | ~2 GB free disk (image + libs + converted files) | – |

`qs2` is a hard `Imports` of margot, so a fresh
`install.packages("margot")` or a `pak::pak("go-bayes/margot")` will
pull it automatically. If you upgraded margot in place from a much older
version, run `install.packages("qs2")` once.

You do **not** need `qs`, R 4.5, Rcpp, or any compiler installed on your
host. Everything that needs `qs` runs inside the container.

### One-time Docker setup

#### macOS

The lightest option is **Colima** (a small Linux VM that the standard
Docker CLI talks to). It is free and CLI-only:

``` bash
# install Homebrew first if you do not have it: https://brew.sh
brew install docker colima
colima start --memory 6 --cpu 4
```

Verify Docker is reachable before continuing:

``` bash
docker info | head -10
```

You should see lines such as `Server Version: 29.x.x` and
`Operating System: Ubuntu 24.04`. If the output ends with “failed to
connect to the docker API”, run `colima start` again.

**Docker Desktop** also works if you prefer a GUI app. Download from
<https://www.docker.com/products/docker-desktop>, install, launch the
app, and wait for the whale icon in the menu bar to settle. Note that
Docker Desktop requires a paid subscription for organisations above
certain size thresholds; Colima has no such restriction.

#### Linux

Most distributions package Docker directly. On Debian/Ubuntu:

``` bash
sudo apt-get update
sudo apt-get install docker.io
sudo systemctl start docker
sudo usermod -aG docker $USER   # log out and back in for this to apply
```

On Fedora/RHEL: `sudo dnf install docker`. Then verify with
`docker info`.

#### Windows

Install **Docker Desktop for Windows** (with the WSL 2 backend
recommended): <https://www.docker.com/products/docker-desktop>. After
installation, restart and open Docker Desktop once so the engine starts.
Then verify with `docker info` from a terminal.

### Run the migration

From your normal R 4.6 session:

``` r

margot::margot_convert_qs_dir_docker(
  "~/path/to/your/cache/dir"
)
```

The first run pulls the `rocker/r-ver:4.5` image (~600 MB) and compiles
`qs` 0.27.3 from a pinned **2024-12-01 Posit Package Manager snapshot**
so the build is reproducible across machines and dates. Subsequent runs
reuse the cached image and library and finish quickly.

Per-file output looks like:

    ℹ starting docker container (rocker/r-ver:4.5); first run pulls the image and compiles qs.
    ℹ mounted: /Users/you/path/to/cache -> /data
    found 12 .qs file(s) under /data
    ok     project-a/models_binary.qs
    ok     project-a/policy_workflow.qs
    skip   project-b/old.qs (.qs2 exists)
    …
    converted: 11, skipped: 1, failed: 0
    ✔ conversion finished. spot-check a .qs2 file before re-running with delete_qs = TRUE.

### Spot-check, then delete

After the first run, read one of the new `.qs2` files in your normal
session to confirm the migration:

``` r

obj <- qs2::qs_read("~/path/to/your/cache/dir/something.qs2")
str(obj, max.level = 1)
```

Once you’re satisfied, run again with `delete_qs = TRUE` to remove the
originals:

``` r

margot::margot_convert_qs_dir_docker(
  "~/path/to/your/cache/dir",
  delete_qs = TRUE
)
```

The container performs an
[`identical()`](https://rdrr.io/r/base/identical.html) read-verify
*before* any deletion, so the safety net stays in place even with
`delete_qs = TRUE`.

## Caveats

- **Bind-mount scope.** Colima and Docker Desktop on macOS share `~/`
  into the VM by default. If your archive lives outside `~/` (e.g. on an
  external volume), either move the work into `~/` first or extend the
  Colima mount with `colima start --mount /Volumes/External:w`.
- **Memory.** Default Colima allocates 2 GiB of RAM, which can OOM on
  very large `qs` files (the deserialised object lives in memory).
  `colima start --memory 6 --cpu 4` is a sensible baseline; users with
  multi-gigabyte caches may want more.
- **Rocker image size.** The first pull is ~600 MB. Once cached locally
  it’s reused for every subsequent migration run on that machine.

## After migration

Your day-to-day code does not need to change. New saves go to `.qs2` and
read back the same way:

``` r

margot::here_save_qs(my_models, "models", dir_path = "cache")  # writes models.qs2
my_models <- margot::here_read_qs("models", dir_path = "cache") # reads models.qs2
```

If you also want to migrate to the parquet-backed format (e.g. to share
result objects with non-R tooling), use the arrow wrappers, which now
round-trip arbitrary R objects:

``` r

margot::here_save_arrow(my_models, "models", dir_path = "cache")
my_models <- margot::here_read_arrow("models", dir_path = "cache")
```

The arrow envelope writes `.parquet` files, so non-R tools see a single
row of metadata; full fidelity is preserved when read back through
[`here_read_arrow()`](https://go-bayes.github.io/margot/reference/here_read_arrow.md).

## Reporting issues

If
[`margot_convert_qs_dir_docker()`](https://go-bayes.github.io/margot/reference/margot_convert_qs_dir_docker.md)
fails on a specific archive, please open an issue at
<https://github.com/go-bayes/margot/issues> with:

- the per-file `FAIL` line(s) printed by the function,
- the host R version (`R.version.string`),
- the Docker engine in use (`docker info | head -20`),
- the size of the failing `.qs` file (`file.info(path)$size`).

The most common failure mode is an out-of-memory kill inside the
container; the fix is usually `colima stop && colima start --memory 8`
(or higher), then re-run.
