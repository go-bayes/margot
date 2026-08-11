test_that("here_save_arrow and here_read_arrow round-trip parquet", {
  skip_if_not_installed("arrow")

  tmp_dir <- tempdir()
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  margot::here_save_arrow(df, "arrow_roundtrip", dir_path = tmp_dir, quiet = TRUE)
  read_df <- margot::here_read_arrow("arrow_roundtrip", dir_path = tmp_dir, quiet = TRUE)

  expect_true(file.exists(file.path(tmp_dir, "arrow_roundtrip.parquet")))
  expect_equal(as.data.frame(read_df), df)
})

test_that("here_save_arrow rejects non-tabular objects", {
  skip_if_not_installed("arrow")

  tmp_dir <- tempdir()
  expect_error(
    margot::here_save_arrow(list(value = 1), "arrow_list", dir_path = tmp_dir, quiet = TRUE),
    "Use `here_save\\(\\)` for non-tabular R objects"
  )
  expect_false(file.exists(file.path(tmp_dir, "arrow_list.parquet")))
})
