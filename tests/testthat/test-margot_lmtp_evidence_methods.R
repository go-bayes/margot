# software tests for the print and plot methods of the LMTP evidence reports

evidence_fixture <- function() {
  observed <- matrix(
    c(1, 1, 1,
      1, 0, 0,
      1, 1, 0,
      1, NA, NA),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("T10-T11", "T11-T12", "T12-T13"))
  )
  probabilities <- matrix(
    c(0.9, 0.8, 0.7,
      0.8, 0.4, 0.3,
      0.95, 0.85, 0.5,
      0.7, 0.2, 0.1),
    nrow = 4,
    byrow = TRUE
  )
  factors <- matrix(
    c(1.1, 1.2, 1.3,
      1.0, 0, 0,
      0.9, 1.1, 0,
      1.2, 0, 0),
    nrow = 4,
    byrow = TRUE
  )
  censoring <- margot_lmtp_censoring_report(
    observed = observed,
    baseline_weights = c(1, 2, 1, 2),
    fitted_probabilities = probabilities,
    censoring_factors = factors,
    joint_ratios = factors * 1.5,
    policy_id = "policy_a",
    learner_specification = data.frame(learner = "SL.glm")
  )
  projection <- margot_target_projection_report(
    source = data.frame(
      age = c(20, 30, 40, 50),
      group = c("a", "a", "b", "b"),
      region = c("north", "north", "south", "south")
    ),
    target = data.frame(
      age = c(30, 40, 50, 60),
      group = c("a", "b", "b", "c"),
      region = c("north", "south", "south", "east")
    ),
    variables = c("age", "group", "region"),
    projection_weights = c(0.5, 0.5, 1.5, 1.5),
    strata = "region",
    harmonisation = "shared synthetic coding"
  )
  ratios <- matrix(
    c(1, 2,
      0, NA,
      2, 0.5,
      1, 1),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("T11", "T12"))
  )
  weights <- margot_lmtp_analysis_weight_report(
    baseline_weights = c(1, 2, 1, 2),
    joint_ratios = ratios,
    regularised_joint_ratios = pmin(ratios, 1.5),
    policy_id = "policy_a"
  )
  list(censoring = censoring, projection = projection, weights = weights)
}

draw <- function(graph) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(graph)
  invisible(TRUE)
}

test_that("censoring report prints retention, fitted summaries and zero causes", {
  f <- evidence_fixture()
  text <- utils::capture.output(result <- print(f$censoring))
  expect_identical(result, f$censoring)
  expect_true(any(grepl("Observed retention", text)))
  expect_true(any(grepl("T11-T12\\s+2\\s+50.00", text)))
  expect_true(any(grepl("Fitted continued-observation probability", text)))
  expect_true(any(grepl("Joint exposure-and-censoring ratio", text)))
  expect_true(any(grepl("zero_factor_after_non_observation", text)))
  expect_true(any(grepl("Learner specification: aggregate table with 1 row", text)))
  expect_false(any(grepl("verdict:|pass|fail", text)))
  bare <- utils::capture.output(print(margot_lmtp_censoring_report(c(1, 0, 1))))
  expect_true(any(grepl("none recorded", bare)))
})

test_that("projection report prints categorical and numeric balance and unrepresented strata", {
  f <- evidence_fixture()
  text <- utils::capture.output(print(f$projection))
  expect_true(any(grepl("Categorical balance", text)))
  expect_true(any(grepl("Numeric balance", text)))
  expect_true(any(grepl("age\\s+35.00\\s+40.00\\s+45.00", text)))
  expect_true(any(grepl("1 without source representation:", text)))
  expect_true(any(grepl("^\\s+east\\s+1\\s+25.00", text)))
  expect_true(any(grepl("Kish effective sample size", text)))
  expect_true(any(grepl("Harmonisation: shared synthetic coding", text)))
})

test_that("analysis-weight report prints nodes, concentration, causes and regularisation", {
  f <- evidence_fixture()
  text <- utils::capture.output(print(f$weights))
  expect_true(any(grepl("Full analysis weight by node", text)))
  expect_true(any(grepl("regularised\\s+T12", text)))
  expect_true(any(grepl("Concentration at the final node \\(T12\\)", text)))
  expect_true(any(grepl("joint_ratio_zero_at_T11\\s+1", text)))
  expect_true(any(grepl("^Regularisation: raw final weight sum", text)))
})

test_that("evidence report prints one line per component with structural reasons", {
  f <- evidence_fixture()
  complete <- margot_lmtp_evidence_report(f$censoring, f$projection, list(policy_a = f$weights))
  text <- utils::capture.output(print(complete))
  expect_true(any(grepl("^censoring_report\\s+3 transitions; rows observed 100.00% to 25.00%", text)))
  expect_true(any(grepl("^projection_report\\s+variables age, group, region", text)))
  expect_true(any(grepl("strata without source representation: 1 of 3", text)))
  expect_true(any(grepl("^analysis_weight_reports\\s+1 policy; policy_a: final-node Kish/n", text)))
  partial <- margot_lmtp_evidence_report(
    censoring_report = f$censoring,
    missing_reasons = c(
      projection_report = "target population is the realised source population",
      analysis_weight_reports = "route stopped before a policy was realised"
    )
  )
  text <- utils::capture.output(print(partial))
  expect_true(any(grepl("^projection_report\\s+unavailable: target population is the realised", text)))
  expect_true(any(grepl("^analysis_weight_reports\\s+unavailable: route stopped", text)))
})

test_that("plot methods return drawable ggplot or patchwork objects", {
  f <- evidence_fixture()
  censoring <- plot(f$censoring)
  expect_s3_class(censoring, "patchwork")
  expect_true(draw(censoring))
  expect_s3_class(plot(margot_lmtp_censoring_report(c(1, 0, 1))), "ggplot")
  both <- plot(f$projection)
  expect_s3_class(both, "patchwork")
  expect_true(draw(both))
  balance <- plot(f$projection, what = "balance")
  expect_s3_class(balance, "ggplot")
  expect_true(draw(balance))
  built <- ggplot2::ggplot_build(balance)$data[[2L]]
  expect_equal(nrow(built), 2L * nrow(f$projection$balance))
  weights <- plot(f$weights)
  expect_s3_class(weights, "ggplot")
  expect_true(draw(weights))
  expect_equal(nrow(ggplot2::ggplot_build(weights)$data[[1L]]), 3L * nrow(f$weights$weight_summary))
  complete <- margot_lmtp_evidence_report(f$censoring, f$projection, list(policy_a = f$weights))
  stacked <- plot(complete)
  expect_s3_class(stacked, "patchwork")
  expect_true(draw(stacked))
  partial <- margot_lmtp_evidence_report(
    projection_report = f$projection,
    missing_reasons = c(
      censoring_report = "no transition realised",
      analysis_weight_reports = "no policy realised"
    )
  )
  expect_s3_class(plot(partial), "patchwork")
  empty <- margot_lmtp_evidence_report(missing_reasons = c(
    censoring_report = "a", projection_report = "b", analysis_weight_reports = "c"
  ))
  expect_error(plot(empty), "no available component")
})
