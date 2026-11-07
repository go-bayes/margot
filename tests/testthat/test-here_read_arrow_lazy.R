test_that("lazy Parquet reading defers row collection and preserves eager defaults", {
  path <- withr::local_tempdir()
  original <- data.frame(id = 1:4, wave = c("T10", "T16", "T16", "T10"),
    score = c(2, NA, 4, 5))
  arrow::write_parquet(original, file.path(path, "example.parquet"))
  eager <- here_read_arrow("example", path, quiet = TRUE)
  expect_equal(as.data.frame(eager), original)
  table <- here_read_arrow("example", path, quiet = TRUE, as_data_frame = FALSE)
  expect_s3_class(table, "Table")
  testthat::local_mocked_bindings(read_parquet = function(...) stop("eager reader called"),
    .package = "arrow")
  dataset <- here_read_arrow("example", path, quiet = TRUE, lazy = TRUE)
  expect_s3_class(dataset, "Dataset")
  flags <- dataset |>
    dplyr::filter(.data$wave == "T16") |>
    dplyr::transmute(id = .data$id, available = !is.na(.data$score)) |>
    dplyr::collect()
  expect_equal(as.data.frame(flags), data.frame(id = 2:3, available = c(FALSE, TRUE)))
  expect_error(here_read_arrow("example", path, quiet = TRUE, lazy = TRUE,
    as_data_frame = FALSE), "Leave additional arguments empty")
  expect_error(here_read_arrow("example", path, quiet = TRUE, lazy = NA))
  dir.create(file.path(path, "directory.parquet"))
  expect_error(here_read_arrow("directory", path, quiet = TRUE, lazy = TRUE),
    "requires one Parquet file")
})
