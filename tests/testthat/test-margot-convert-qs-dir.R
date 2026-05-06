test_that("margot_convert_qs_dir returns cleanly for empty directories", {
  tmp_dir <- file.path(tempdir(), "qs_dir_empty")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- margot::margot_convert_qs_dir(tmp_dir, quiet = TRUE)
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("path", "status", "message"))
})

test_that("margot_convert_qs_dir converts .qs files in place to .qs2", {
  skip_if_not_installed("qs")
  skip_if_not_installed("qs2")

  tmp_dir <- file.path(tempdir(), "qs_dir_convert")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qs::qsave(list(a = 1:5), file.path(tmp_dir, "one.qs"))
  qs::qsave(data.frame(x = 1:3, y = 4:6), file.path(tmp_dir, "two.qs"))

  result <- margot::margot_convert_qs_dir(tmp_dir, quiet = TRUE)

  expect_equal(sort(result$status), c("converted", "converted"))
  expect_true(file.exists(file.path(tmp_dir, "one.qs2")))
  expect_true(file.exists(file.path(tmp_dir, "two.qs2")))
  expect_true(file.exists(file.path(tmp_dir, "one.qs")))
  expect_equal(qs2::qs_read(file.path(tmp_dir, "one.qs2")), list(a = 1:5))
})

test_that("margot_convert_qs_dir skips existing .qs2 unless overwrite = TRUE", {
  skip_if_not_installed("qs")
  skip_if_not_installed("qs2")

  tmp_dir <- file.path(tempdir(), "qs_dir_overwrite")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qs::qsave(list(v = "from_qs"), file.path(tmp_dir, "thing.qs"))
  qs2::qs_save(list(v = "stale_qs2"), file.path(tmp_dir, "thing.qs2"))

  res <- margot::margot_convert_qs_dir(tmp_dir, quiet = TRUE)
  expect_equal(res$status, "skipped")
  expect_identical(qs2::qs_read(file.path(tmp_dir, "thing.qs2"))$v, "stale_qs2")

  res2 <- margot::margot_convert_qs_dir(tmp_dir, overwrite = TRUE, quiet = TRUE)
  expect_equal(res2$status, "converted")
  expect_identical(qs2::qs_read(file.path(tmp_dir, "thing.qs2"))$v, "from_qs")
})

test_that("margot_convert_qs_dir delete_qs removes the legacy file after verify", {
  skip_if_not_installed("qs")
  skip_if_not_installed("qs2")

  tmp_dir <- file.path(tempdir(), "qs_dir_delete")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  qs::qsave(list(a = 1L), file.path(tmp_dir, "gone.qs"))

  margot::margot_convert_qs_dir(tmp_dir, delete_qs = TRUE, quiet = TRUE)

  expect_false(file.exists(file.path(tmp_dir, "gone.qs")))
  expect_true(file.exists(file.path(tmp_dir, "gone.qs2")))
})

test_that("margot_convert_qs_dir errors usefully when qs is missing", {
  if (requireNamespace("qs", quietly = TRUE)) {
    skip("{qs} is installed")
  }

  tmp_dir <- file.path(tempdir(), "qs_dir_convert_no_qs")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("legacy placeholder", file.path(tmp_dir, "one.qs"))

  expect_error(
    margot::margot_convert_qs_dir(tmp_dir, quiet = TRUE),
    "Package 'qs' is required",
    fixed = TRUE
  )
})
