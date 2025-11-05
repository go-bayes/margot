make_base_lmtp_output <- function() {
  list(
    models = list(
      outcome_a = list(
        outcome_a_shift_zero = list(id = "a_zero"),
        outcome_a_ipsi_05 = list(id = "a_ipsi_05"),
        outcome_a_null = list(id = "a_null")
      ),
      outcome_b = list(
        outcome_b_shift_zero = list(id = "b_zero"),
        outcome_b_null = list(id = "b_null")
      )
    ),
    contrasts = list(),
    individual_tables = list(),
    combined_tables = list()
  )
}

with_lmtp_mocks <- function(code) {
  testthat::local_mocked_bindings(
    lmtp_contrast = function(model, ref, type) {
      list(
        contrast = paste0(model$id, "_vs_", ref$id),
        type = type
      )
    },
    .package = "lmtp"
  )

  testthat::local_mocked_bindings(
    margot_lmtp_evalue = function(contrast, scale, new_name) {
      data.frame(
        check.names = FALSE,
        `E[Y(1)]-E[Y(0)]` = 0.1,
        `2.5 %` = 0.0,
        `97.5 %` = 0.2,
        E_Value = 1.1,
        E_Val_bound = 1.0,
        row.names = new_name
      )
    },
    .package = "margot"
  )

  force(code)
}

test_that("margot_lmtp_combine_and_contrast integrates standalone checkpoint models", {
  base_lmtp_output <- make_base_lmtp_output()

  standalone_checkpoint <- list(
    model = list(id = "b_ipsi_05"),
    outcome = "outcome_b",
    shift_name = "ipsi_05"
  )

  with_lmtp_mocks({
    combined <- margot_lmtp_combine_and_contrast(
      base_lmtp_output,
      standalone_checkpoint,
      auto_pairwise = TRUE,
      include_null_contrasts = TRUE,
      quiet = TRUE
    )

    expect_true("combined_tables" %in% names(combined))
    expect_true("combined_outcomes_ipsi_05_vs_null" %in% names(combined$combined_tables))

    ipsi_vs_null <- combined$combined_tables$combined_outcomes_ipsi_05_vs_null
    expect_equal(nrow(ipsi_vs_null), 2L)
    expect_setequal(rownames(ipsi_vs_null), c("outcome_a", "outcome_b"))
  })
})

test_that("duplicate_policy='overwrite' replaces existing models", {
  base_lmtp_output <- make_base_lmtp_output()

  duplicate_checkpoint <- list(
    model = list(id = "dup_ipsi"),
    outcome = "outcome_a",
    shift_name = "ipsi_05"
  )

  with_lmtp_mocks({
    combined <- margot_lmtp_combine_and_contrast(
      base_lmtp_output,
      duplicate_checkpoint,
      contrasts = list(c("ipsi_05", "null")),
      include_null_contrasts = FALSE,
      duplicate_policy = "overwrite",
      quiet = TRUE
    )

    contrast_name <- "outcome_a_ipsi_05_vs_outcome_a_null"
    result <- combined$contrasts$outcome_a[[contrast_name]]
    expect_match(result$contrast, "^dup_ipsi_vs_a_null$")
  })
})

test_that("duplicate_policy='skip' retains original models", {
  base_lmtp_output <- make_base_lmtp_output()

  duplicate_checkpoint <- list(
    model = list(id = "dup_ipsi"),
    outcome = "outcome_a",
    shift_name = "ipsi_05"
  )

  with_lmtp_mocks({
    combined <- margot_lmtp_combine_and_contrast(
      base_lmtp_output,
      duplicate_checkpoint,
      contrasts = list(c("ipsi_05", "null")),
      include_null_contrasts = FALSE,
      duplicate_policy = "skip",
      quiet = TRUE
    )

    contrast_name <- "outcome_a_ipsi_05_vs_outcome_a_null"
    result <- combined$contrasts$outcome_a[[contrast_name]]
    expect_match(result$contrast, "^a_ipsi_05_vs_a_null$")
  })
})

test_that("duplicate_policy='error' aborts on duplicates", {
  base_lmtp_output <- make_base_lmtp_output()

  duplicate_checkpoint <- list(
    model = list(id = "dup_ipsi"),
    outcome = "outcome_a",
    shift_name = "ipsi_05"
  )

  with_lmtp_mocks({
    expect_error(
      margot_lmtp_combine_and_contrast(
        base_lmtp_output,
        duplicate_checkpoint,
        contrasts = list(c("ipsi_05", "null")),
        include_null_contrasts = FALSE,
        duplicate_policy = "error",
        quiet = TRUE
      ),
      "Duplicate model encountered"
    )
  })
})

test_that("keep_models retains merged models with canonical naming", {
  base_lmtp_output <- make_base_lmtp_output()

  standalone_checkpoint <- list(
    model = list(id = "b_ipsi_05"),
    outcome = "outcome_b",
    shift_name = "ipsi_05"
  )

  with_lmtp_mocks({
    combined <- margot_lmtp_combine_and_contrast(
      base_lmtp_output,
      standalone_checkpoint,
      auto_pairwise = FALSE,
      contrasts = list(c("ipsi_05", "null")),
      keep_models = TRUE,
      quiet = TRUE
    )

    expect_true(is.list(combined$models))
    expect_true("outcome_a" %in% names(combined$models))
    expect_true("outcome_b" %in% names(combined$models))

    outcome_a_models <- combined$models$outcome_a
    expect_setequal(
      names(outcome_a_models),
      paste0("outcome_a_", c("shift_zero", "ipsi_05", "null"))
    )

    outcome_b_models <- combined$models$outcome_b
    expect_true("outcome_b_ipsi_05" %in% names(outcome_b_models))
    expect_true(inherits(outcome_b_models[["outcome_b_ipsi_05"]], "list"))
  })
})
