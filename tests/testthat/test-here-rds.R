test_that("here_save and here_read round-trip non-tabular R objects", {
  tmp_dir <- tempfile("margot-rds-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  object <- list(
    estimate = 0.31,
    interval = c(0.21, 0.41),
    fit = stats::lm(mpg ~ wt, data = mtcars)
  )

  path <- margot::here_save(
    object,
    "non_tabular_roundtrip",
    dir_path = tmp_dir,
    quiet = TRUE
  )
  restored <- margot::here_read(
    "non_tabular_roundtrip",
    dir_path = tmp_dir,
    quiet = TRUE
  )

  expect_identical(path, file.path(tmp_dir, "non_tabular_roundtrip.rds"))
  expect_true(file.exists(path))
  expect_equal(restored$estimate, object$estimate)
  expect_equal(restored$interval, object$interval)
  expect_equal(stats::coef(restored$fit), stats::coef(object$fit))
})

test_that("RDS helpers round-trip a data frame", {
  tmp_dir <- tempfile("margot-rds-tabular-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  tabular <- data.frame(x = 1:3)
  path <- margot::here_save(tabular, "tabular", dir_path = tmp_dir, quiet = TRUE)
  restored <- margot::here_read("tabular", dir_path = tmp_dir, quiet = TRUE)

  expect_true(file.exists(path))
  expect_equal(restored, tabular)
})
