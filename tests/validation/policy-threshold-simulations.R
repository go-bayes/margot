#!/usr/bin/env Rscript
# evaluate fixed-rule paired intervals against analytically known realised-rule values.
# all observations are simulated; no empirical data or fitted nuisance models enter.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else file.path(tempdir(), "policy-threshold-simulations")
replications <- if (length(args) >= 2L) as.integer(args[[2]]) else 400L
reference_mode <- if (length(args) >= 3L) args[[3]] else "ate"
if (!reference_mode %in% c("ate", "known_constant")) stop("Reference mode must be ate or known_constant.")
if (is.na(replications) || replications < 2L) stop("At least two replications are required.")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1])
package_dir <- normalizePath(file.path(dirname(script), "../.."))
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1L)
  RhpcBLASctl::omp_set_num_threads(1L)
}
devtools::load_all(package_dir, quiet = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# four equally prevalent source cells give exact target-weighted integration.
cells <- as.matrix(expand.grid(x = c(0, 1), z = c(0, 1)))
scenarios <- list(constant_zero = rep(0, 4), constant_positive = rep(.05, 4),
  same_sign = c(.02, .08, .02, .08), sign_reversal = c(-.04, .06, -.04, .06),
  negative_ate = c(-.08, -.02, -.08, -.02))
if (reference_mode == "known_constant") scenarios <- scenarios[c("constant_zero", "constant_positive")]
weight_scenarios <- list(equal = rep(1, 4), nonuniform = c(1, 2, 4, 8))
plan <- c("# Simulation design fixed before execution", "",
  paste("Replications per effect/weight combination:", replications),
  paste("Threshold mode:", reference_mode, "(ate estimates the reference in development; known_constant supplies the exact true homogeneous effect as the fixed threshold)."),
  "ATE mode includes five effect patterns; known_constant mode includes the two constant-effect patterns. Both use two weight patterns; development n=300, evaluation n=200; fixed depth-one exact policytree with minimum development leaf count 25.",
  "Source covariates are two independent Bernoulli(0.5) variables; the four source cells have probability 1/4. Weight patterns are (1,1,1,1) and (1,2,4,8), attached to baseline cells; the target probabilities are weights divided by their sum.",
  "Treatment A is Bernoulli(0.5), independent of baseline. Outcome Y = 0.1*x - 0.05*z + A*tau(x,z) + Normal(0,0.1^2). Known nuisance functions define unweighted action scores Gamma_a=m_a+I(A=a)/p_a*(Y-m_a). Development and evaluation observations are independent.",
  "For each development fit, calculate its exact conditional target gain by integrating (pi(x)-a_constant)*(tau(x)-phi_development) over the four target cells. This is the realised fitted rule's target, never the oracle's value. Paired intervals are assessed against that target with no reference-population Monte Carlo error.",
  "Primary summaries: coverage, its Monte Carlo standard error, Monte Carlo z discrepancy from 95% (descriptive, no pass/fail criterion), estimated-minus-true gain bias, empirical error SD, average reported SE, CI-positive frequency, and false superiority among replications with true conditional gain <=0. Assignment-pattern and root-frequency tables describe training variability separately.",
  "The known_constant benchmark fixes phi to the true constant effect, making every realised policy and comparator have exactly zero true net gain. Because the sample ATE threshold is random, constant causal effects do not imply zero true net gain for every realised threshold and rule. CI positivity alone is not a false heterogeneity claim; the false-superiority event additionally requires true conditional gain <=0. Report constant-effect patterns transparently.",
  "The simulation only examines oracle-nuisance, independent-record, moderate-weight scenarios. It does not validate fitted nuisance models, missingness/imputation, sequential observation weights, clustering, multiplicity, or complete empirical-pipeline coverage.",
  "Seeds: 20260911 + 100000*effect_index + 10000*weight_index + replication_index. One native thread. All five effect patterns, both weight patterns and every replication are retained, including failures. No outcome-based scenario selection.")
writeLines(plan, file.path(output_dir, "design.md"))

# generate independent randomized records and oracle AIPW action scores.
generate_partition <- function(n, tau_cells, weights_cells) {
  cell <- sample.int(4L, n, replace = TRUE)
  X <- cells[cell, , drop = FALSE]
  A <- rbinom(n, 1L, .5)
  mu0 <- .1 * X[, "x"] - .05 * X[, "z"]
  mu1 <- mu0 + tau_cells[cell]
  error <- rnorm(n, sd = .1)
  scores <- cbind(control = mu0 + (1 - A) * error / .5,
    treated = mu1 + A * error / .5)
  list(X = X, scores = scores, weights = weights_cells[cell])
}

# learn one rule and compare its interval with its exact conditional target.
run_replication <- function(scenario, weight_name, i, j, k) {
  set.seed(20260911L + 100000L * i + 10000L * j + k)
  tau <- scenarios[[scenario]]
  w <- weight_scenarios[[weight_name]]
  d <- generate_partition(300L, tau, w)
  e <- generate_partition(200L, tau, w)
  fit <- margot_policy_tree_evaluate(d$X, d$scores, e$X, e$scores,
    development_weights = d$weights, evaluation_weights = e$weights,
    min_node_size = 25L, depth = 1L, tree_method = "policytree",
    value_threshold = if (reference_mode == "ate") "ate" else sum(w / sum(w) * tau),
    development_ids = paste0("development-", seq_len(300)),
    evaluation_ids = paste0("evaluation-", seq_len(200)))
  action <- as.integer(predict(fit$tree, cells)) - 1L
  constant <- fit$constant$action_id - 1L
  truth <- sum(w / sum(w) * (action - constant) * (tau - fit$threshold$value))
  value <- fit$evaluation$value
  root <- if (isTRUE(fit$tree$nodes[[1]]$is_leaf)) "constant" else colnames(cells)[fit$tree$nodes[[1]]$split_variable]
  data.frame(reference_mode = reference_mode, scenario = scenario, weights = weight_name, replication = k,
    target_ate = sum(w / sum(w) * tau), threshold = fit$threshold$value,
    estimate = value$estimate, truth = truth, se = value$se,
    lower = value$lower, upper = value$upper,
    covered = value$lower <= truth + 1e-12 & value$upper >= truth - 1e-12,
    ci_positive = value$lower > 1e-12,
    nonpositive_truth = truth <= 1e-12,
    false_superiority = truth <= 1e-12 & value$lower > 1e-12,
    constant_rule = fit$metadata$constant_selected,
    assignment = paste0(action, collapse = ""), root = root,
    target_treated_share = sum(w / sum(w) * action),
    threshold_error = fit$threshold$value - sum(w / sum(w) * tau))
}

started <- Sys.time()
rows <- list()
failures <- list()
index <- 0L
for (i in seq_along(scenarios)) {
  for (j in seq_along(weight_scenarios)) {
    scenario <- names(scenarios)[i]
    weight_name <- names(weight_scenarios)[j]
    for (k in seq_len(replications)) {
      result <- tryCatch(run_replication(scenario, weight_name, i, j, k), error = identity)
      index <- index + 1L
      if (inherits(result, "error")) {
        failures[[length(failures) + 1L]] <- data.frame(scenario = scenario,
          weights = weight_name, replication = k, error = conditionMessage(result))
      } else rows[[length(rows) + 1L]] <- result
    }
    message(scenario, "/", weight_name, ": ", replications, " replications complete")
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_dir, "replications.csv"), row.names = FALSE)
if (length(failures)) write.csv(do.call(rbind, failures), file.path(output_dir, "failures.csv"), row.names = FALSE)

# summarise independent replication indicators without treating leaves as replicates.
groups <- split(results, interaction(results$scenario, results$weights, drop = TRUE))
summary <- do.call(rbind, lapply(groups, function(d) {
  coverage <- mean(d$covered)
  n <- nrow(d)
  eligible <- sum(d$nonpositive_truth)
  data.frame(reference_mode = reference_mode, scenario = d$scenario[1], weights = d$weights[1], n = n,
    coverage = coverage, coverage_mcse = sqrt(coverage * (1 - coverage) / n),
    nominal_mcse = sqrt(.95 * .05 / n), z_from_nominal = (coverage - .95) / sqrt(.95 * .05 / n),
    bias = mean(d$estimate - d$truth), error_sd = sd(d$estimate - d$truth),
    mean_se = mean(d$se), ci_positive = mean(d$ci_positive),
    nonpositive_truth_n = eligible, false_superiority_n = sum(d$false_superiority),
    false_superiority_rate = if (eligible) sum(d$false_superiority) / eligible else NA_real_,
    constant_rule_fraction = mean(d$constant_rule),
    nonconstant_coverage = if (any(!d$constant_rule)) mean(d$covered[!d$constant_rule]) else NA_real_,
    distinct_assignments = length(unique(d$assignment)),
    mean_treated_share = mean(d$target_treated_share), treated_share_sd = sd(d$target_treated_share),
    threshold_bias = mean(d$threshold_error))
}))
rownames(summary) <- NULL
write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)
assignments <- as.data.frame(with(results, table(scenario, weights, assignment)))
write.csv(assignments[assignments$Freq > 0, ], file.path(output_dir, "assignment-frequency.csv"), row.names = FALSE)
roots <- as.data.frame(with(results, table(scenario, weights, root)))
write.csv(roots[roots$Freq > 0, ], file.path(output_dir, "root-frequency.csv"), row.names = FALSE)
saveRDS(list(design = plan, summary = summary, results = results, failures = failures,
  session = sessionInfo(), source_sha256 = vapply(c("R/margot_policy_tree_evaluate.R", "R/margot_policy_value_threshold.R", "tests/validation/policy-threshold-simulations.R"), function(p) digest::digest(file.path(package_dir, p), file = TRUE, algo = "sha256"), character(1)), elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))),
  file.path(output_dir, "simulation-results.rds"))
report <- c("# Fixed-rule interval simulation results", "",
  paste("Reference mode:", reference_mode),
  paste("Completed:", nrow(results), "replications; failures:", length(failures)),
  paste("Elapsed seconds:", round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)),
  "", "| Scenario | Weights | Coverage | MCSE | Bias | Mean SE | Positive lower CI | False superiority / nonpositive truth | Assignment patterns |",
  "|---|---|---:|---:|---:|---:|---:|---:|---:|")
for (i in seq_len(nrow(summary))) {
  d <- summary[i, ]
  report <- c(report, sprintf("| %s | %s | %.3f | %.3f | %.5f | %.5f | %.3f | %d / %d | %d |",
    d$scenario, d$weights, d$coverage, d$coverage_mcse, d$bias, d$mean_se,
    d$ci_positive, d$false_superiority_n, d$nonpositive_truth_n, d$distinct_assignments))
}
report <- c(report, "", "These are scenario-specific descriptive operating characteristics, not a coverage guarantee or an empirical acceptance decision. The design and inferential limitations are recorded in design.md. In ate mode, constant-effect scenarios can have a nonzero conditional true gain because the realised development ATE reference differs from the population ATE. In known_constant mode all true net gains equal zero. A positive gain alone does not demonstrate effect heterogeneity.")
writeLines(report, file.path(output_dir, "summary.md"))
print(summary, row.names = FALSE)
if (length(failures)) stop("Simulation failures were retained; inspect failures.csv.")
