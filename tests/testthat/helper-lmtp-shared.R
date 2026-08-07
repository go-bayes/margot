# Fixtures shared by the sequential and task-parallel shared-ratio tests.

# simulate a two-outcome LMTP frame with one common nuisance identity
make_shared_ratio_data <- function(n = 120L, seed = 44L) {
  set.seed(seed)
  data <- data.frame(
    id = seq_len(n),
    baseline = stats::rnorm(n),
    exposure = stats::rnorm(n)
  )
  data$perfectionism <- 0.30 * data$exposure + 0.20 * data$baseline + stats::rnorm(n)
  data$distress <- -0.20 * data$exposure + 0.40 * data$baseline + stats::rnorm(n)
  data
}

# apply the deterministic exposure reduction used in the shared-ratio tests
shared_ratio_shift_down <- function(data, trt) {
  data[[trt]] - 0.15
}

# supply the common deterministic SDR settings used by both execution paths
shared_ratio_defaults <- function() {
  list(
    baseline = "baseline",
    outcome_type = "continuous",
    id = "id",
    folds = 2L,
    learners_trt = c("SL.glm", "SL.mean"),
    learners_outcome = c("SL.glm", "SL.mean")
  )
}

# extract the numerical influence-function properties from one lmtp model
shared_ratio_numerics <- function(model) {
  list(
    density_ratios = model$density_ratios,
    estimate = attr(model$estimate, "x", exact = TRUE),
    standard_error = attr(model$estimate, "std_error", exact = TRUE),
    influence_function = attr(model$estimate, "eif", exact = TRUE),
    confidence_interval = as.numeric(model$estimate@conf_int)
  )
}
