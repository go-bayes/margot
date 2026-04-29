test_that("here_save_arrow and here_read_arrow round-trip parquet", {
  skip_if_not_installed("arrow")

  tmp_dir <- tempdir()
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  margot::here_save_arrow(df, "arrow_roundtrip", dir_path = tmp_dir, quiet = TRUE)
  read_df <- margot::here_read_arrow("arrow_roundtrip", dir_path = tmp_dir, quiet = TRUE)

  expect_true(file.exists(file.path(tmp_dir, "arrow_roundtrip.parquet")))
  expect_equal(as.data.frame(read_df), df)
})

test_that("here_save_arrow round-trips a list via the qs2 envelope", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("qs2")

  tmp_dir <- tempdir()
  obj <- list(
    estimate = 0.31,
    ci = c(0.21, 0.41),
    fit = stats::lm(mpg ~ wt, data = mtcars),
    df = head(mtcars)
  )

  margot::here_save_arrow(obj, "arrow_list_roundtrip", dir_path = tmp_dir, quiet = TRUE)
  back <- margot::here_read_arrow("arrow_list_roundtrip", dir_path = tmp_dir, quiet = TRUE)

  expect_type(back, "list")
  expect_equal(back$estimate, obj$estimate)
  expect_equal(back$ci, obj$ci)
  expect_equal(coef(back$fit), coef(obj$fit))
  expect_equal(back$df, obj$df)
})

test_that("here_save_arrow round-trips a fitted lm directly", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("qs2")

  tmp_dir <- tempdir()
  fit <- stats::lm(mpg ~ wt + cyl, data = mtcars)

  margot::here_save_arrow(fit, "arrow_lm_roundtrip", dir_path = tmp_dir, quiet = TRUE)
  back <- margot::here_read_arrow("arrow_lm_roundtrip", dir_path = tmp_dir, quiet = TRUE)

  expect_s3_class(back, "lm")
  expect_equal(coef(back), coef(fit))
})
