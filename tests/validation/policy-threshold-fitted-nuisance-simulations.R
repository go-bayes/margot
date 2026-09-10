#!/usr/bin/env Rscript
# assess the complete development-score helper and fixed-rule evaluator on simulated data.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else file.path(tempdir(), "policy-threshold-fitted-nuisance")
replications <- if (length(args) >= 2L) as.integer(args[[2]]) else 100L
workers <- if (length(args) >= 3L) as.integer(args[[3]]) else 2L
if (is.na(replications) || replications < 2L || is.na(workers) || !workers %in% 1:2) stop("Use at least two replications and one or two workers.")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1])
package_dir <- normalizePath(file.path(dirname(script), "../.."))
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)
}
devtools::load_all(package_dir, quiet = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cells <- as.matrix(expand.grid(x = c(0, 1), z = c(0, 1)))
cell_propensity <- .25 + .25 * cells[, "x"] + .25 * cells[, "z"]
cell_weight <- c(1, 2, 2, 4)
cell_mu0 <- .2 * cells[, "x"] - .15 * cells[, "z"] + .1 * cells[, "x"] * cells[, "z"]
scenarios <- list(constant_zero = rep(0, 4), constant_positive = rep(.05, 4),
  same_sign = c(.02, .08, .02, .08), sign_reversal = c(-.04, .06, -.04, .06))
fractions <- c(.6, .7, .8)
forest_args <- list(num.trees = 300L, min.node.size = 20L)
plan <- c("# Fitted-nuisance simulation design fixed before execution", "",
  paste("Replications per effect/split combination:", replications),
  "Four effect scenarios: constant zero, constant +.05, effects .02/.08 according to x, and effects -.04/+.06 according to x. Two independent binary features yield four equally prevalent source cells.",
  "Total sample size 1,000 per replication. Development fractions .6, .7, .8; .7 remains the primary predeclared analysis and the others are descriptive sensitivity settings. No ratio will be selected because its observed coverage looks best.",
  "Propensity e(x,z)=.25+.25*x+.25*z, hence .25 to .75. Baseline target weights on the four cells are (1,2,2,4); target probabilities are those weights divided by nine. Outcome Y=.2*x-.15*z+.1*x*z+A*tau(x)+Normal(0,.15^2). All weights and baseline features are known, without imputation or observation models.",
  "Each replication calls margot_policy_development_scores with three fitted forests, 300 trees, minimum node size 20 and one native thread. Development action scores use out-of-bag predictions; evaluation predictions use only development-trained models. The subsequent margot_policy_tree_evaluate call uses the development weighted ATE reference, exact policytree depth one and minimum leaf size 25.",
  "Evaluate exact conditional true net gain of the realised development rule versus its development-selected constant by summing (pi(x)-a_constant)*(tau(x)-phi_development) over the four known target cells. This removes reference-grid simulation error and does not compare estimates against an oracle rule's value.",
  "Report pointwise coverage with Monte Carlo SE, bias, error SD, mean reported SE, positive lower intervals and false superiority where exact true gain <=0. Track fitted nuisance RMSE, propensity range, root and assignment patterns, all warnings and errors. Failures remain in the attempted replication count and are saved rather than silently discarded.",
  "Constant effects can yield nonzero conditional net gains when the estimated ATE threshold differs from the population ATE; a positive policy gain alone does not identify effect heterogeneity.",
  "Outcome generation seeds: 20260911+100000*effect_index+replication. The same simulated cohort supports the three prespecified split sensitivities; this shared-cohort dependence is irrelevant to within-cell replication MCSE but prohibits treating cells as independent pooled replications. Forest seeds additionally add 10000*split_index. R workers are at most two, each with one native thread.",
  "This checks fitted nuisance estimation for a simple correctly represented observational DGP with bounded propensity and moderate known weights. It does not establish coverage for sequential missingness, imputation, estimated calibration or observation weights, clustered data, severe positivity problems, richer covariates, investigator outcome access or multiplicity.")
writeLines(plan, file.path(output_dir, "design.md"))
source_hashes <- vapply(c("R/margot_policy_action_scores.R", "R/margot_policy_tree_evaluate.R",
  "R/margot_policy_value_threshold.R", "tests/validation/policy-threshold-fitted-nuisance-simulations.R"),
  function(p) digest::digest(file.path(package_dir, p), file = TRUE, algo = "sha256"), character(1))

# fit development-only nuisances and compare the evaluated rule with exact cell truth.
run_replication <- function(i, j, k) {
  recorded_warnings <- character()
  tryCatch(withCallingHandlers({
    set.seed(20260911L + 100000L * i + k)
    cell <- sample.int(4L, 1000L, replace = TRUE)
    X <- cells[cell, , drop = FALSE]
    tau <- scenarios[[i]]
    W <- rbinom(1000L, 1L, cell_propensity[cell])
    Y <- cell_mu0[cell] + W * tau[cell] + rnorm(1000L, sd = .15)
    weights <- cell_weight[cell]
    d <- seq_len(as.integer(1000 * fractions[j]))
    e <- setdiff(seq_len(1000L), d)
    nuisance <- margot_policy_development_scores(X[d, , drop = FALSE], Y[d], W[d],
      X[e, , drop = FALSE], Y[e], W[e], development_weights = weights[d],
      forest_args = forest_args, seed = 20260911L + 100000L * i + 10000L * j + k,
      num_threads = 1L, save_models = FALSE)
    fit <- margot_policy_tree_evaluate(X[d, , drop = FALSE], nuisance$development_scores,
      X[e, , drop = FALSE], nuisance$evaluation_scores,
      development_weights = weights[d], evaluation_weights = weights[e],
      development_ids = d, evaluation_ids = e, value_threshold = "ate",
      depth = 1L, min_node_size = 25L, tree_method = "policytree")
    action <- as.integer(predict(fit$tree, cells)) - 1L
    constant <- fit$constant$action_id - 1L
    truth <- sum(cell_weight / sum(cell_weight) * (action - constant) * (tau - fit$threshold$value))
    value <- fit$evaluation$value
    root <- if (isTRUE(fit$tree$nodes[[1]]$is_leaf)) "constant" else colnames(cells)[fit$tree$nodes[[1]]$split_variable]
    target_ate <- sum(cell_weight / sum(cell_weight) * tau)
    pred <- nuisance$evaluation_predictions
    data.frame(scenario = names(scenarios)[i], development_fraction = fractions[j],
      primary_split = fractions[j] == .7, replication = k, status = "success", error = "",
      warnings = paste(unique(recorded_warnings), collapse = " | "),
      target_ate = target_ate, threshold = fit$threshold$value,
      estimate = value$estimate, truth = truth, se = value$se,
      lower = value$lower, upper = value$upper,
      covered = value$lower <= truth + 1e-12 & value$upper >= truth - 1e-12,
      ci_positive = value$lower > 1e-12, nonpositive_truth = truth <= 1e-12,
      false_superiority = truth <= 1e-12 & value$lower > 1e-12,
      constant_rule = fit$metadata$constant_selected,
      assignment = paste0(action, collapse = ""), root = root,
      target_treated_share = sum(cell_weight / sum(cell_weight) * action),
      threshold_error = fit$threshold$value - target_ate,
      propensity_rmse = sqrt(mean((pred$propensity - cell_propensity[cell[e]])^2)),
      outcome_mean_rmse = sqrt(mean((pred$outcome_mean - cell_mu0[cell[e]] - cell_propensity[cell[e]] * tau[cell[e]])^2)),
      treatment_effect_rmse = sqrt(mean((pred$treatment_effect - tau[cell[e]])^2)),
      min_propensity = min(pred$propensity), max_propensity = max(pred$propensity))
  }, warning = function(w) {
    recorded_warnings <<- c(recorded_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(err) {
    list(scenario = names(scenarios)[i], development_fraction = fractions[j], replication = k,
      status = "failure", error = conditionMessage(err), warnings = paste(unique(recorded_warnings), collapse = " | "))
  })
}

started <- Sys.time()
rows <- list()
failures <- list()
for (i in seq_along(scenarios)) {
  for (j in seq_along(fractions)) {
    batch <- parallel::mclapply(seq_len(replications), function(k) run_replication(i, j, k),
      mc.cores = workers, mc.preschedule = TRUE, mc.set.seed = FALSE)
    for (r in batch) {
      if (is.data.frame(r)) rows[[length(rows) + 1L]] <- r else failures[[length(failures) + 1L]] <- as.data.frame(r)
    }
    message(names(scenarios)[i], "/", fractions[j], ": ", replications, " attempted; cumulative failures ", length(failures))
    saveRDS(list(rows = rows, failures = failures), file.path(output_dir, "checkpoint.rds"))
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_dir, "replications.csv"), row.names = FALSE)
if (length(failures)) write.csv(do.call(rbind, failures), file.path(output_dir, "failures.csv"), row.names = FALSE)
warning_rows <- results[nzchar(results$warnings), ]
write.csv(warning_rows, file.path(output_dir, "warnings.csv"), row.names = FALSE)

# summarise independent replications separately for each prespecified split.
groups <- split(results, interaction(results$scenario, results$development_fraction, drop = TRUE))
summary <- do.call(rbind, lapply(groups, function(d) {
  n <- nrow(d)
  coverage <- mean(d$covered)
  eligible <- sum(d$nonpositive_truth)
  data.frame(scenario = d$scenario[1], development_fraction = d$development_fraction[1],
    primary_split = d$primary_split[1], attempted = replications, n = n,
    coverage = coverage, coverage_mcse = sqrt(coverage * (1 - coverage) / n),
    nominal_mcse = sqrt(.95 * .05 / n), z_from_nominal = (coverage - .95) / sqrt(.95 * .05 / n),
    bias = mean(d$estimate - d$truth), error_sd = sd(d$estimate - d$truth), mean_se = mean(d$se),
    ci_positive = mean(d$ci_positive), nonpositive_truth_n = eligible,
    false_superiority_n = sum(d$false_superiority),
    false_superiority_rate = if (eligible) sum(d$false_superiority) / eligible else NA_real_,
    constant_rule_fraction = mean(d$constant_rule),
    distinct_assignments = length(unique(d$assignment)),
    mean_treated_share = mean(d$target_treated_share), treated_share_sd = sd(d$target_treated_share),
    threshold_bias = mean(d$threshold_error), propensity_rmse = mean(d$propensity_rmse),
    outcome_mean_rmse = mean(d$outcome_mean_rmse), treatment_effect_rmse = mean(d$treatment_effect_rmse),
    min_propensity = min(d$min_propensity), max_propensity = max(d$max_propensity),
    warning_replications = sum(nzchar(d$warnings)))
}))
rownames(summary) <- NULL
write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)
assignments <- as.data.frame(with(results, table(scenario, development_fraction, assignment)))
write.csv(assignments[assignments$Freq > 0, ], file.path(output_dir, "assignment-frequency.csv"), row.names = FALSE)
roots <- as.data.frame(with(results, table(scenario, development_fraction, root)))
write.csv(roots[roots$Freq > 0, ], file.path(output_dir, "root-frequency.csv"), row.names = FALSE)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
saveRDS(list(design = plan, summary = summary, results = results, failures = failures,
  session = sessionInfo(), source_sha256 = source_hashes, elapsed_seconds = elapsed),
  file.path(output_dir, "simulation-results.rds"))
report <- c("# Fitted-nuisance fixed-rule simulation results", "",
  paste("Completed:", nrow(results), "replications; failures:", length(failures), "; replications with warnings:", nrow(warning_rows)),
  paste("Elapsed seconds:", round(elapsed, 1)), "",
  "The 70/30 split is primary. The other ratios are descriptive sensitivity settings, not a selection competition.", "",
  "| Scenario | Development fraction | Coverage | MCSE | Bias | Mean SE | Positive lower CI | False superiority / nonpositive truth |",
  "|---|---:|---:|---:|---:|---:|---:|---:|")
for (i in seq_len(nrow(summary))) {
  d <- summary[i, ]
  report <- c(report, sprintf("| %s | %.1f | %.3f | %.3f | %.5f | %.5f | %.3f | %d / %d |",
    d$scenario, d$development_fraction, d$coverage, d$coverage_mcse, d$bias, d$mean_se,
    d$ci_positive, d$false_superiority_n, d$nonpositive_truth_n))
}
report <- c(report, "", "These are scenario-specific operating characteristics against each realised rule's exact conditional target. This simple known-weight DGP does not validate Christina's missingness, imputation or estimated population-weight uncertainty. A positive gain with an estimated ATE threshold does not itself demonstrate heterogeneity.")
writeLines(report, file.path(output_dir, "summary.md"))
print(summary, row.names = FALSE)
if (length(failures)) stop("Failures were retained; inspect failures.csv before interpreting completed replications.")
