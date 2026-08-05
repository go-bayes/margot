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

test_that("shared SDR ratios reproduce independent lmtp fits exactly", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data()
  outcomes <- c("perfectionism", "distress")
  defaults <- shared_ratio_defaults()

  legacy <- lapply(outcomes, function(outcome) {
    set.seed(3031L)
    do.call(
      lmtp::lmtp_sdr,
      c(
        list(
          data = data,
          trt = "exposure",
          outcome = outcome,
          shift = shared_ratio_shift_down
        ),
        defaults
      )
    )
  })
  set.seed(3031L)
  shared <- do.call(
    margot_lmtp_sdr_shared,
    c(
      list(
        data = data,
        trt = "exposure",
        outcomes = outcomes,
        shift = shared_ratio_shift_down
      ),
      defaults
    )
  )

  expect_identical(names(shared), outcomes)
  expect_identical(attr(shared, "margot_density_ratio_fit_count"), 1L)
  for (i in seq_along(outcomes)) {
    expect_s3_class(shared[[i]], "lmtp")
    expect_identical(shared_ratio_numerics(shared[[i]]), shared_ratio_numerics(legacy[[i]]))
  }
})

test_that("shared SDR restores stochastic outcome-learning state", {
  skip_if_not_installed("ranger")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 100L, seed = 51L)
  outcomes <- c("perfectionism", "distress")
  defaults <- shared_ratio_defaults()
  defaults$learners_trt <- c("SL.glm", "SL.ranger")
  defaults$learners_outcome <- c("SL.glm", "SL.ranger")

  legacy <- lapply(outcomes, function(outcome) {
    set.seed(3031L)
    do.call(
      lmtp::lmtp_sdr,
      c(
        list(
          data = data,
          trt = "exposure",
          outcome = outcome,
          shift = shared_ratio_shift_down
        ),
        defaults
      )
    )
  })
  set.seed(3031L)
  shared <- do.call(
    margot_lmtp_sdr_shared,
    c(
      list(
        data = data,
        trt = "exposure",
        outcomes = outcomes,
        shift = shared_ratio_shift_down
      ),
      defaults
    )
  )

  for (i in seq_along(outcomes)) {
    expect_identical(shared_ratio_numerics(shared[[i]]), shared_ratio_numerics(legacy[[i]]))
  }
})

test_that("margot_lmtp fit-once output reaches the unchanged plot contract", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 140L, seed = 77L)
  ratio_fit_count <- 0L
  original_fit <- margot_lmtp_fit_density_ratios
  testthat::local_mocked_bindings(
    margot_lmtp_fit_density_ratios = function(...) {
      ratio_fit_count <<- ratio_fit_count + 1L
      original_fit(...)
    },
    .package = "margot"
  )
  arguments <- list(
    data = data,
    outcome_vars = c("perfectionism", "distress"),
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = "null",
    contrast_scale = "additive",
    lmtp_defaults = shared_ratio_defaults(),
    n_cores = 1L,
    progress = "none",
    seed = 3031L
  )

  shared <- do.call(margot_lmtp, c(arguments, list(reuse_density_ratios = TRUE)))
  expect_identical(ratio_fit_count, 2L)
  expect_identical(
    names(shared),
    c("models", "contrasts", "individual_tables", "combined_tables")
  )
  expect_true(all(vapply(
    unlist(shared$models, recursive = FALSE),
    inherits,
    logical(1L),
    what = "lmtp"
  )))
  reuse <- attr(shared, "margot_density_ratio_reuse")
  expect_identical(reuse$ratio_fit_count, 2L)
  expect_identical(reuse$legacy_ratio_fit_count, 4L)

  table_name <- "combined_outcomes_down_vs_null"
  expect_true(table_name %in% names(shared$combined_tables))
  table <- shared$combined_tables[[table_name]]
  expect_identical(
    names(table),
    c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound")
  )
  plotted <- margot_plot(
    table,
    type = "RD",
    include_coefficients = FALSE,
    options = list(use_title_case = FALSE)
  )
  expect_s3_class(plotted$plot, "ggplot")
  expect_equal(nrow(plotted$plot$data), 2L)
  multi_plot <- margot_plot_multi(
    tables = list(primary = table, repeated = table),
    type = "RD",
    include_coefficients = FALSE
  )
  expect_s3_class(multi_plot$plot, "patchwork")

  learner_report <- margot_report_lmtp_learners(
    shared,
    outcome = "perfectionism",
    include_plot = FALSE
  )
  expect_true(nrow(learner_report$summary_table) > 0L)
  expect_true(nrow(learner_report$learner_data) > 0L)

  recombined <- margot_lmtp_combine_and_contrast(
    shared,
    contrasts = list(c("down", "null")),
    keep_models = TRUE,
    quiet = TRUE
  )
  expect_identical(
    names(recombined),
    c("models", "contrasts", "individual_tables", "combined_tables")
  )
  expect_true(all(vapply(
    unlist(recombined$models, recursive = FALSE),
    inherits,
    logical(1L),
    what = "lmtp"
  )))

  legacy <- do.call(margot_lmtp, c(arguments, list(reuse_density_ratios = FALSE)))
  expect_identical(shared$combined_tables, legacy$combined_tables)
  expect_identical(
    lapply(shared$models, names),
    lapply(legacy$models, names)
  )
  expect_no_error(margot_lmtp_overlap(shared, plot = FALSE, verbose = FALSE))
  expect_no_error(margot_bind_models(shared, shared, quiet = TRUE))
})

test_that("a Margot specification drives weighted fit-once estimation", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 90L, seed = 91L)
  data$analysis_weight <- seq(0.75, 1.25, length.out = nrow(data))
  specification <- margot_lmtp_estimator_spec(
    trt = "exposure",
    outcomes = c("perfectionism", "distress"),
    policies = c(down = TRUE, null = TRUE),
    seed = 3031L,
    baseline = "baseline",
    outcome_type = "continuous",
    id = "id",
    folds = 2L,
    learner_profile = "glm",
    weight_column = "analysis_weight"
  )

  fitted <- margot_lmtp(
    data = data,
    shift_functions = list(down = shared_ratio_shift_down, null = NULL),
    contrast_type = "null",
    estimator_spec = specification,
    reuse_density_ratios = TRUE,
    n_cores = 1L,
    progress = "none"
  )

  expect_identical(
    names(fitted),
    c("models", "contrasts", "individual_tables", "combined_tables")
  )
  expect_identical(attr(fitted, "margot_density_ratio_reuse")$ratio_fit_count, 2L)
  expect_s3_class(
    margot_plot(
      fitted$combined_tables$combined_outcomes_down_vs_null,
      type = "RD",
      include_coefficients = FALSE
    )$plot,
    "ggplot"
  )
})

test_that("fit-once checkpoints restore through the existing Margot path", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data(n = 80L, seed = 61L)
  output_dir <- withr::local_tempdir()

  fitted <- margot_lmtp(
    data = data,
    outcome_vars = c("perfectionism", "distress"),
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = "null",
    lmtp_defaults = shared_ratio_defaults(),
    n_cores = 1L,
    progress = "none",
    seed = 3031L,
    reuse_density_ratios = TRUE,
    save_output = TRUE,
    save_path = output_dir,
    base_filename = "shared-ratio-test"
  )
  checkpoint_dirs <- list.dirs(
    file.path(output_dir, "checkpoints"),
    recursive = FALSE,
    full.names = TRUE
  )
  expect_length(checkpoint_dirs, 1L)
  restored <- margot_lmtp_restore_checkpoints(
    checkpoint_dir = checkpoint_dirs[[1L]],
    outcome_vars = c("perfectionism", "distress"),
    contrast_type = "null",
    quiet = TRUE
  )

  expect_identical(restored$combined_tables, fitted$combined_tables)
  expect_true(all(vapply(
    unlist(restored$models, recursive = FALSE),
    inherits,
    logical(1L),
    what = "lmtp"
  )))
})

test_that("terminal outcome changes cannot alter shared density ratios", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data()
  altered <- data
  altered$distress <- rev(altered$distress) + 10
  arguments <- c(
    list(
      trt = "exposure",
      outcomes = c("perfectionism", "distress"),
      shift = shared_ratio_shift_down
    ),
    shared_ratio_defaults()
  )

  set.seed(3031L)
  original <- do.call(margot_lmtp_sdr_shared, c(list(data = data), arguments))
  set.seed(3031L)
  changed <- do.call(margot_lmtp_sdr_shared, c(list(data = altered), arguments))

  expect_identical(original$perfectionism$density_ratios, changed$perfectionism$density_ratios)
  expect_identical(original$distress$density_ratios, changed$distress$density_ratios)
})

test_that("unsupported shared-ratio paths refuse explicitly", {
  data <- make_shared_ratio_data(n = 60L)
  expect_error(
    margot_lmtp(
      data = data,
      outcome_vars = c("perfectionism", "distress"),
      trt = "exposure",
      shift_functions = list(down = shared_ratio_shift_down),
      lmtp_model_type = lmtp::lmtp_tmle,
      lmtp_defaults = shared_ratio_defaults(),
      reuse_density_ratios = TRUE,
      progress = "none"
    ),
    class = "margot_error_unsupported_estimator"
  )
  expect_error(
    do.call(
      margot_lmtp_sdr_shared,
      c(
        list(
          data = data,
          trt = "exposure",
          outcomes = c("perfectionism", "distress"),
          shift = shared_ratio_shift_down,
          shifted = data
        ),
        shared_ratio_defaults()
      )
    ),
    class = "margot_error_unsupported_estimator"
  )
})
