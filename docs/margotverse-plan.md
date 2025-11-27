# summary —————————————————————

# this script is a strategic blueprint for evolving the original `margot`

# monolith into a coherent ecosystem – the **margotverse** – composed of

# lean, interoperable packages that share a common grammar, data contract,

# and build infrastructure. like pruning a venerable but over‑grown vine,

# we keep only the branches that bear fruit, graft them onto stronger

# rootstock, and train new shoots along a tidy trellis. the end‑state is

# a stack that is easier to test, maintain, and extend, while retaining

# continuity for users and the lab.

# guiding principles —————————————————-

# 1. **explicit dependencies** – favour minimal imports and optional

# ‘suggests’; manage them per‑package with {renv}.

# 2. **single‑responsibility packages** – each repo does one thing

# extremely well, a la tidyverse design philosophy.

# 3. **shared core objects** – a light s3 class (`margot_panel`) defines

# panel data; helpers live in `margot.core`. all other packages import

# but do **not** modify this class.

# 4. **pipelines over scripts** – long simulations and estimation workflows

# live in *targets* pipelines for reproducibility and lazy re‑builds.

# 

# 5. **continuous documentation** – each package gets its own {pkgdown}

# site; a meta‑site at `margotverse.org` aggregates reference indices

# via a pkgdown “articles” collection.

# 6. **literate results** – reporting is powered by {boilerplate} +

# Quarto templates so manuscripts remain executable.

# high‑level package map ———————————————–

margotverse_map \<- list( margot.core = c(“panel data class”,
“coercion + validation”), margot.wrangle = c(“long‑to‑wide”, “missing
data tags”), margot.sim = c(“data‑generating mechanisms”, “diagnostic
plots”), margot.lmtp = c(“batch lmtp estimators”, “assumption checks”),
\# margot.grf = c(“batch causal & policy forests”, “effect
heterogeneity”), \# margot.viz = c(“graphs, tables, text”, “tidy
presentation”), margot.report= c(“boilerplate templates”, “quarto
helpers”), margotverse = c(“meta‑package umbrella”, “imports all above”)
)

# helper: create a skeleton sub‑package ———————————

setup_subpackage \<- function(path, licence = “MIT”, open = FALSE) { \#
creates a bare package using usethis ——————————- if
(!requireNamespace(“usethis”, quietly = TRUE)) stop(“install usethis
first”) usethis::create_package(path, open = open) \#
usethis::use_mit_license(“Joseph A.Bulbulia”) usethis::use_git()
usethis::use_github(repo_spec = paste0(“go-bayes/”, basename(path))) \#
initialise renv for isolated deps ———————————- renv::init(bare = TRUE)
\# \# add standard ci, pkgdown ——————————————-
usethis::use_github_action_check_standard() usethis::use_pkgdown() }

# helper: declare common imports —————————————

add_core_dep \<- function(pkg_path) { desc \<- file.path(pkg_path,
“DESCRIPTION”) usethis::use_package(“margot.core”, type = “Imports”, pkg
= pkg_path) usethis::use_package(“rlang”, type = “Imports”, pkg =
pkg_path) }

# define the shared s3 class ——————————————-

\#’ construct a margot_panel object \#’ @param data a data.frame in long
format \#’ @param id unit identifier \#’ @param time time index \#’
@return an object of class margot_panel margot_panel \<- function(data,
id, time) { stopifnot(all(c(id, time) %in% names(data))) structure(
data, id_col = id, time_col = time, class = c(“margot_panel”,
class(data)) ) }

# method: convert to wide ———————————————-

as_wide.margot_panel \<- function(x, keep_ids = TRUE) {
tidyr::pivot_wider( x, id_cols = attr(x, “id_col”), names_from = attr(x,
“time_col”), values_fn = dplyr::first ) }

# targets pipeline template ——————————————–

# inside each analysis repo create \_targets.R

write_targets_template \<- function(path = “analysis/\_targets.R”) {
lines \<- c( “library(targets)”, “library(margot.lmtp)”, \# bring your
estimators “tar_option_set(packages = c(‘margot.core’,
‘margot.wrangle’))”, ““,”list(“,” tar_target(raw,
readr::read_csv(‘data/panel.csv’)),“,” tar_target(panel,
margot_panel(raw, id = ‘participant’, time = ‘wave’)),“,”
tar_target(wide, as_wide.margot_panel(panel)),“,” tar_target(lmtp_res,
batch_lmtp(wide, spec_file = ‘analysis/spec.yml’))“,”)” )
dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, path) } \# batch wrappers ——————————————————–
batch_lmtp \<- function(data, spec_file) { \# iterate over interventions
defined in yaml, fit lmtp in parallel – spec \<-
yaml::read_yaml(spec_file) future.apply::future_lapply(
spec\$interventions, \\pol) lmtp::lmtp_tmle(data = data, shift =
pol\$fn, A = pol$`A, Y = pol`$Y) ) }

batch_grf \<- function(data, treat, outcome, covars) { forest \<-
grf::causal_forest( X = data\[covars\], Y = data\[\[outcome\]\], W =
data\[\[treat\]\] ) grf::predict(forest)\$predictions }

# reporting blueprint —————————————————

# quarto + boilerplate integration:

# in `analysis/report.qmd` embed:

# 

# `{r} # library(boilerplate) # bp_render( # template = "methods.lmtp", # variables = list(estimator = "TMLE", learners = "SuperLearner") # ) #`

# the template string lives in `inst/templates/methods.lmtp.json`.

# quarto renders narrative + tables automatically.

# meta‑package assembly ————————————————-

build_margotverse \<- function() { if (!requireNamespace(“usethis”,
quietly = TRUE)) stop(“usethis needed”)
usethis::create_package(“margotverse”, open = FALSE) \#
usethis::use_description(fields = list( Title = “The margotverse”,
Description = “A family of packages for longitudinal causal inference.”
)) \# import each child package but expose nothing; user attaches
meta‑pkg for (pkg in names(margotverse_map)\[-length(margotverse_map)\])
{ usethis::use_package(pkg, type = “Imports”, pkg = “margotverse”) } \#
convenience re‑exports usethis::use_package(“magrittr”, type =
“Imports”, pkg = “margotverse”) cat(“margotverse skeleton created –
remember to add a pkgdown site.”) }

# dependency hygiene —————————————————-

# run this occasionally in each repo to drop unused imports ———–

renv_clean \<- function() renv::dependencies() \#

# cultural notes ——————————————————–

# \* avoid loading the whole margotverse within the low‑level packages;

# echo the tidyverse decision to keep dependencies acyclic.

# \* expose only one main verb per package (e.g., `sim_*` in margot.sim).

# \* dedicate a `zzz.R` that sets generic options but does **not** attach

# any non‑strict dependency.

# \* publish each package’s site via GitHub‑Pages; aggregate reference

# tabs with custom navbar links in the umbrella site.

# execution ————————————————————-

# run these interactively to scaffold the ecosystem ——————–

# setup_subpackage(“margot.core”)

# setup_subpackage(“margot.wrangle”)

# setup_subpackage(“margot.sim”)

# setup_subpackage(“margot.lmtp”)

# setup_subpackage(“margot.grf”)

# setup_subpackage(“margot.viz”)

# setup_subpackage(“margot.report”)

# build_margotverse()

# when we next `devtools::load_all()` inside a child package, renv will

# capture the exact versions, and targets will orchestrate the heavy

# lifting – leaving us free to cultivate new methodological vines

# without tangling the trellis.
