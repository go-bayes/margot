test_that("deprecated here_save_qs and here_read_qs round-trip via qs2", {
  skip_if_not_installed("qs2")

  tmp_dir <- tempdir()
  obj <- list(
    df = data.frame(x = 1:5, y = letters[1:5]),
    fit = stats::lm(mpg ~ wt, data = mtcars),
    text = "hello"
  )

  expect_warning(
    margot::here_save_qs(obj, "qs_roundtrip", tmp_dir, quiet = TRUE),
    "deprecated"
  )
  expect_true(file.exists(file.path(tmp_dir, "qs_roundtrip.qs2")))

  expect_warning(
    back <- margot::here_read_qs("qs_roundtrip", dir_path = tmp_dir, quiet = TRUE),
    "deprecated"
  )
  expect_equal(back$df, obj$df)
  expect_equal(coef(back$fit), coef(obj$fit))
  expect_identical(back$text, obj$text)
})

test_that("here_read_qs falls back to legacy .qs when qs is installed", {
  skip_if_not_installed("qs")

  tmp_dir <- tempdir()
  obj <- data.frame(a = 1:3)

  legacy_path <- file.path(tmp_dir, "legacy_only.qs")
  qs::qsave(obj, legacy_path)
  on.exit(unlink(legacy_path), add = TRUE)

  expect_warning(
    back <- margot::here_read_qs("legacy_only", dir_path = tmp_dir, quiet = TRUE),
    "deprecated"
  )
  expect_equal(back, obj)
})

test_that("here_read_qs errors usefully for legacy .qs when qs is missing", {
  if (requireNamespace("qs", quietly = TRUE)) {
    skip("{qs} is installed")
  }

  tmp_dir <- tempdir()
  legacy_path <- file.path(tmp_dir, "legacy_only.qs")
  writeLines("legacy placeholder", legacy_path)
  on.exit(unlink(legacy_path), add = TRUE)

  expect_warning(
    expect_error(
      margot::here_read_qs("legacy_only", dir_path = tmp_dir, quiet = TRUE),
      "optional 'qs' package is not installed",
      fixed = TRUE
    ),
    "deprecated"
  )
})

test_that("here_read_qs prefers .qs2 over legacy .qs when both exist", {
  skip_if_not_installed("qs2")

  tmp_dir <- tempdir()
  qs2_obj <- list(via = "qs2")

  suppressWarnings(margot::here_save_qs(qs2_obj, "dual", tmp_dir, quiet = TRUE))
  writeLines("legacy placeholder", file.path(tmp_dir, "dual.qs"))
  on.exit(unlink(file.path(tmp_dir, c("dual.qs", "dual.qs2"))), add = TRUE)

  expect_warning(
    back <- margot::here_read_qs("dual", dir_path = tmp_dir, quiet = TRUE),
    "deprecated"
  )
  expect_identical(back, qs2_obj)
})

test_that("here_read_qs errors usefully when neither .qs2 nor .qs exists", {
  expect_warning(
    expect_error(
      margot::here_read_qs("nope", dir_path = tempdir(), quiet = TRUE),
      "tried .qs2 and .qs",
      fixed = TRUE
    ),
    "deprecated"
  )
})
