test_that("margot_convert_qs_dir_docker errors helpfully when docker is missing", {
  testthat::skip_if(Sys.which("docker") != "", "docker is on PATH; this test runs only when it isn't")
  td <- tempfile("docker_missing_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  expect_warning(
    expect_error(
      margot::margot_convert_qs_dir_docker(td),
      "Docker Desktop"
    ),
    "deprecated"
  )
})

test_that("margot_convert_qs_dir_docker errors when dir_path does not exist", {
  expect_warning(
    expect_error(
      margot::margot_convert_qs_dir_docker("/this/path/does/not/exist"),
      "Directory not found"
    ),
    "deprecated"
  )
})

test_that("margot_convert_qs_dir_docker round-trips a real file end-to-end", {
  # full integration test: requires docker on PATH (with a working daemon
  # such as Colima or Docker Desktop) and pulls/uses rocker/r-ver:4.5. The
  # first run also compiles qs from the pinned Posit PPM snapshot, so allow
  # several minutes. Opt in via env var so it does not run by default.
  testthat::skip_if(Sys.getenv("MARGOT_TEST_DOCKER", "") != "1",
                    "set MARGOT_TEST_DOCKER=1 to run the full docker round-trip test")
  testthat::skip_if(Sys.which("docker") == "", "docker not on PATH")
  testthat::skip_if_not_installed("qs2")

  # we cannot create a .qs file from this R session without qs installed.
  # so this test is a manual integration: it expects an existing .qs file
  # at the path below (override via env var).
  src_dir <- Sys.getenv("MARGOT_TEST_QS_DIR", "")
  testthat::skip_if(src_dir == "", "set MARGOT_TEST_QS_DIR to a directory containing .qs files")
  testthat::skip_if(!dir.exists(src_dir), sprintf("%s does not exist", src_dir))

  qs_files <- list.files(src_dir, pattern = "\\.qs$", recursive = TRUE, full.names = TRUE)
  testthat::skip_if(length(qs_files) == 0L, sprintf("no .qs files under %s", src_dir))

  expect_warning(
    status <- margot::margot_convert_qs_dir_docker(src_dir, quiet = TRUE),
    "deprecated"
  )
  expect_equal(status, 0L)

  # at least one .qs2 sibling should now exist and read back as something
  qs2_siblings <- sub("\\.qs$", ".qs2", qs_files)
  expect_true(any(file.exists(qs2_siblings)))

  obj <- qs2::qs_read(qs2_siblings[file.exists(qs2_siblings)][[1]])
  expect_true(!is.null(obj))
})
