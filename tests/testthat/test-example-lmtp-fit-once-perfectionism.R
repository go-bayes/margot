# The public perfectionism example is executable and reaches Margot's plotting surface.

test_that("the public perfectionism example runs end to end", {
  example_environment <- new.env(parent = globalenv())
  example_path <- system.file(
    "examples", "lmtp-fit-once-perfectionism.R",
    package = "margot",
    mustWork = TRUE
  )
  expect_output(
    sys.source(example_path, envir = example_environment),
    "wellbeing|distress"
  )
  example <- example_environment$perfectionism_example
  expect_identical(example$ratio_fits, 2L)
  expect_identical(example$independent_ratio_fits, 4L)
  expect_gt(example$shared_elapsed_seconds, 0)
  expect_gt(example$independent_elapsed_seconds, 0)
  expect_s3_class(example$plot, "ggplot")
  expect_equal(nrow(example$plot$data), 2L)
})
