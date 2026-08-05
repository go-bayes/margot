# Run a complete synthetic perfectionism analysis through Margot's fit-once SDR path.

# simulate the baseline, exposure, outcomes, and analysis weights used by the example
simulate_perfectionism_example <- function(n = 160L, seed = 3031L) {
  set.seed(seed)
  data <- data.frame(
    id = seq_len(n),
    baseline_perfectionism = stats::rnorm(n),
    baseline_wellbeing = stats::rnorm(n),
    perfectionism = pmin(7, pmax(1, stats::rnorm(n, mean = 4, sd = 1))),
    analysis_weight = stats::runif(n, min = 0.7, max = 1.3)
  )
  data$analysis_weight <- data$analysis_weight / mean(data$analysis_weight)
  data$wellbeing <- 0.35 * data$baseline_wellbeing -
    0.20 * data$perfectionism + 0.15 * data$baseline_perfectionism + stats::rnorm(n)
  data$distress <- -0.20 * data$baseline_wellbeing +
    0.25 * data$perfectionism + 0.20 * data$baseline_perfectionism + stats::rnorm(n)
  data
}

# lower perfectionism by half a scale point while respecting its 1-to-7 range
lower_perfectionism <- function(data, trt) {
  pmin(7, pmax(1, data[[trt]] - 0.5))
}

# execute the weighted two-outcome analysis and return its table, plot, and fit counts
run_perfectionism_lmtp_example <- function(n = 160L, seed = 3031L) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- simulate_perfectionism_example(n = n, seed = seed)
  specification <- margot::margot_lmtp_estimator_spec(
    trt = "perfectionism",
    outcomes = c("wellbeing", "distress"),
    policies = c(lower = TRUE, null = TRUE),
    seed = seed,
    baseline = c("baseline_perfectionism", "baseline_wellbeing"),
    outcome_type = "continuous",
    id = "id",
    folds = 2L,
    learner_profile = "glm",
    weight_column = "analysis_weight"
  )
  analysis_arguments <- list(
    data = data,
    shift_functions = list(lower = lower_perfectionism, null = NULL),
    contrast_type = "null",
    contrast_scale = "additive",
    estimator_spec = specification,
    n_cores = 1L,
    progress = "none"
  )
  shared_timing <- system.time({
    fit <- do.call(
      margot::margot_lmtp,
      c(analysis_arguments, list(reuse_density_ratios = TRUE))
    )
  })
  independent_timing <- system.time({
    independent_fit <- do.call(
      margot::margot_lmtp,
      c(analysis_arguments, list(reuse_density_ratios = FALSE))
    )
  })
  table <- fit$combined_tables$combined_outcomes_lower_vs_null
  plot <- margot::margot_plot(
    table,
    type = "RD",
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )
  reuse <- attr(fit, "margot_density_ratio_reuse")
  stopifnot(
    identical(names(fit), c("models", "contrasts", "individual_tables", "combined_tables")),
    identical(reuse$ratio_fit_count, 2L),
    identical(reuse$legacy_ratio_fit_count, 4L),
    identical(fit$combined_tables, independent_fit$combined_tables),
    inherits(plot$plot, "ggplot")
  )
  list(
    fit = fit,
    table = table,
    plot = plot$plot,
    ratio_fits = reuse$ratio_fit_count,
    independent_ratio_fits = reuse$legacy_ratio_fit_count,
    shared_elapsed_seconds = unname(shared_timing[["elapsed"]]),
    independent_elapsed_seconds = unname(independent_timing[["elapsed"]])
  )
}

perfectionism_example <- run_perfectionism_lmtp_example()
print(perfectionism_example$table)
cat(sprintf(
  "Density-ratio fits: shared %d; independent %d. Elapsed seconds: shared %.3f; independent %.3f.\n",
  perfectionism_example$ratio_fits,
  perfectionism_example$independent_ratio_fits,
  perfectionism_example$shared_elapsed_seconds,
  perfectionism_example$independent_elapsed_seconds
))
if (interactive()) {
  print(perfectionism_example$plot)
}
