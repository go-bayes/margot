# software tests for nonbinding LMTP evidence reports

test_that("censoring report separates retention, fitted probabilities, and factors", {
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

  report <- margot_lmtp_censoring_report(
    observed = observed,
    baseline_weights = c(1, 2, 1, 2),
    fitted_probabilities = probabilities,
    censoring_factors = factors,
    joint_ratios = factors * 1.5,
    policy_id = "policy_a",
    learner_specification = data.frame(learner = "SL.glm")
  )

  expect_s3_class(report, "margot_lmtp_censoring_report")
  expect_equal(report$retention$n_observed, c(4, 2, 1))
  expect_equal(report$retention$pct_observed_unweighted, c(100, 50, 25))
  expect_equal(report$retention$pct_observed_weighted, 100 * c(1, 2 / 6, 1 / 6))
  expect_equal(nrow(report$fitted_probability_summary), 3)
  expect_equal(nrow(report$censoring_factor_summary), 3)
  expect_equal(nrow(report$joint_ratio_summary), 3)
  expect_equal(
    report$exact_zero_causes$n[
      report$exact_zero_causes$wave == "T11-T12" &
        report$exact_zero_causes$component == "censoring_factor" &
        report$exact_zero_causes$cause == "zero_factor_after_non_observation"
    ],
    2
  )
  expect_identical(report$metadata$decision_role, "descriptive_nonbinding")
  expect_false(any(grepl("verdict|status|threshold|pass|fail", names(report$retention))))
})

test_that("joint-zero causes distinguish unavailable censoring factors", {
  report <- margot_lmtp_censoring_report(
    observed = c(1, 1, 0),
    censoring_factors = c(1, NA, 0),
    joint_ratios = c(0, 0, 0)
  )
  causes <- report$exact_zero_causes
  count <- function(cause) causes$n[causes$cause == cause]

  expect_equal(count("zero_joint_ratio_with_positive_censoring_factor"), 1)
  expect_equal(
    count("zero_joint_ratio_with_missing_or_non_finite_censoring_factor"),
    1
  )
  expect_equal(count("zero_joint_ratio_with_zero_censoring_factor"), 1)
})

test_that("projection report describes numeric, categorical, and stratum overlap", {
  source <- data.frame(
    age = c(20, 30, 40, 50),
    group = c("a", "a", "b", "b"),
    region = c("north", "north", "south", "south")
  )
  target <- data.frame(
    age = c(30, 40, 50, 60),
    group = c("a", "b", "b", "c"),
    region = c("north", "south", "south", "east")
  )
  report <- margot_target_projection_report(
    source = source,
    target = target,
    variables = c("age", "group", "region"),
    projection_weights = c(0.5, 0.5, 1.5, 1.5),
    strata = "region",
    harmonisation = "shared synthetic coding"
  )

  expect_s3_class(report, "margot_target_projection_report")
  age <- report$balance[report$balance$variable == "age", ]
  expect_equal(age$source_value, 35)
  expect_equal(age$projected_source_value, 40)
  expect_equal(age$target_value, 45)
  expect_true(any(report$balance$variable == "group" & report$balance$level == "c"))
  east <- report$stratum_representation[report$stratum_representation$stratum == "east", ]
  expect_false(east$represented_in_source)
  expect_equal(east$target_weight_share, 0.25)
  expect_equal(report$weight_summary$n_zero, 0)
  expect_identical(report$metadata$decision_role, "descriptive_nonbinding")
  expect_false(any(grepl("verdict|status|threshold|pass|fail", names(report$balance))))
})

test_that("analysis-weight report retains zeros in concentration summaries", {
  baseline <- c(1, 2, 1, 2)
  ratios <- matrix(
    c(1, 2,
      0, NA,
      2, 0.5,
      1, 1),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("T11", "T12"))
  )
  regularised <- pmin(ratios, 1.5)
  report <- margot_lmtp_analysis_weight_report(
    baseline_weights = baseline,
    joint_ratios = ratios,
    regularised_joint_ratios = regularised,
    policy_id = "policy_a"
  )

  expect_s3_class(report, "margot_lmtp_analysis_weight_report")
  final_raw <- report$weight_summary[
    report$weight_summary$stage == "raw" & report$weight_summary$wave == "T12",
  ]
  expect_equal(final_raw$n_zero, 1)
  expect_equal(final_raw$n_missing, 0)
  expect_equal(final_raw$sum_weights, 5)
  expect_equal(final_raw$kish_effective_n, 25 / 9)
  expect_equal(final_raw$kish_fraction_of_all_rows, (25 / 9) / 4)
  expect_true(any(
    report$exact_zero_and_missing_causes$cause == "joint_ratio_zero_at_T11" &
      report$exact_zero_and_missing_causes$n == 1
  ))
  expect_true(is.data.frame(report$regularisation_comparison))
  expect_true(report$metadata$zero_weights_included)
  expect_identical(report$metadata$decision_role, "descriptive_nonbinding")
})

test_that("evidence report requires reasons for structurally missing objects", {
  censoring <- margot_lmtp_censoring_report(c(1, 0, 1))
  projection <- margot_target_projection_report(
    source = data.frame(x = c(0, 1)),
    target = data.frame(x = c(0, 1)),
    variables = "x"
  )
  weights <- margot_lmtp_analysis_weight_report(
    baseline_weights = c(1, 1),
    joint_ratios = matrix(c(1, 1), ncol = 1)
  )

  complete <- margot_lmtp_evidence_report(censoring, projection, weights)
  expect_s3_class(complete, "margot_lmtp_evidence_report")
  expect_true(all(complete$manifest$available))
  expect_true(all(complete$manifest$decision_role == "descriptive_nonbinding"))

  expect_error(
    margot_lmtp_evidence_report(censoring_report = censoring),
    "Every missing report requires a structural reason"
  )
  expect_error(
    margot_lmtp_evidence_report(
      missing_reasons = c(
        censoring_report = "",
        projection_report = "not applicable",
        analysis_weight_reports = "not realised"
      )
    ),
    "non-empty structural reason"
  )
  expect_error(
    margot_lmtp_evidence_report(
      censoring_report = censoring,
      missing_reasons = c(
        censoring_report = "stale reason",
        projection_report = "not applicable",
        analysis_weight_reports = "not realised"
      )
    ),
    "available report"
  )
  partial <- margot_lmtp_evidence_report(
    censoring_report = censoring,
    missing_reasons = c(
      projection_report = "target population is the realised source population",
      analysis_weight_reports = "route stopped before a policy was realised"
    )
  )
  expect_false(partial$manifest$available[partial$manifest$object == "projection_report"])
})
