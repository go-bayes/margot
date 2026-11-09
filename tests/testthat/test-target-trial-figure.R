reduce_by_one <- function(a) pmax(1, a - 1)

decile_people <- list(A = c(8, 8, 9, 9, 9), B = c(7, 7, 5, 5, 4), C = c(1, 1, 1, 1, 1))

decile_arms <- list(
  list(name = "No shift", descriptor = "deciles left unassigned", shift = NULL),
  list(name = "Reduction", descriptor = "one decile less deprived, floor of 1", shift = reduce_by_one)
)

draw_deciles <- function(assignment_times = 14, people = decile_people, arms = decile_arms, ...) {
  margot_target_trial_figure(
    observed = people,
    times = 10:14,
    assignment_times = assignment_times,
    outcome_time = 15,
    arms = arms,
    exposure_label = "Deprivation decile",
    unit = "decile",
    scale = 1:10,
    scale_ends = c("least", "most"),
    outcome_label = c("meaning and", "satisfaction"),
    title = "Reducing deprivation",
    ...
  )
}

test_that("shift functions act on observed values at assignment times only", {
  values <- draw_deciles(assignment_times = 12:14)$values
  reduction <- values[values$arm == "Reduction", ]

  expect_equal(reduction$shifted[!reduction$assignment_time], reduction$observed[!reduction$assignment_time])
  expect_equal(reduction$shifted[reduction$participant == "A"], c(8, 8, 8, 8, 8))
  expect_equal(reduction$shifted[reduction$participant == "B"], c(7, 7, 4, 4, 3))
  # the floor returns the observed value
  expect_equal(reduction$shifted[reduction$participant == "C"], rep(1, 5))

  no_shift <- values[values$arm == "No shift", ]
  expect_equal(no_shift$shifted, no_shift$observed)
})

test_that("one assignment time draws a shift, a held value and an unassigned arm", {
  tikz <- draw_deciles()$tikz

  expect_true(any(grepl("\\ottoshift{personA}{(\\tD,9)}{(\\tE,9)}{(\\tE,8)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\ottoshift{personB}{(\\tD,5)}{(\\tE,4)}{(\\tE,3)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\ottohold{personC}{(\\tD,1)}{(\\tE,1)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\ottohistory{personA}{\\tA/8, \\tB/8, \\tC/9, \\tD/9, \\tE/9}", tikz, fixed = TRUE)))
  expect_true(any(grepl("The shift function acts on observed deciles at Time 14.", tikz, fixed = TRUE)))
})

test_that("later assignment times shift the observed value, not the earlier shifted value", {
  tikz <- draw_deciles(assignment_times = 12:14)$tikz

  expect_true(any(grepl("\\ottoshift{personB}{(\\tB,7)}{(\\tC,5)}{(\\tC,4)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\ottoshiftobs{personB}{(\\tD,5)}{(\\tD,4)}{(\\tE,4)}{(\\tE,3)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\ottoholdnext[5.9pt]{personC}{(\\tC,1)}{(\\tD,1)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("Shift functions act on observed deciles, from Time 14 back.", tikz, fixed = TRUE)))
})

test_that("a value returned unchanged after a shifted value keeps the dashed line", {
  people <- list(A = c(5, 5, 5, 2, 1))
  tikz <- draw_deciles(assignment_times = 13:14, people = people)$tikz

  expect_true(any(grepl("\\ottoshift{personA}{(\\tC,5)}{(\\tD,2)}{(\\tD,1)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\draw[assignpath, personA, shorten <=4.7pt] (\\tD,1) -- (\\tE,1);", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\node[heldring=personA] at (\\tE,1) {};", tikz, fixed = TRUE)))
})

test_that("half-point values are written without rounding", {
  figure <- margot_target_trial_figure(
    observed = list(A = c(4, 3.5, 3.5)),
    times = 13:15,
    assignment_times = 15,
    outcome_time = 16,
    arms = decile_arms,
    exposure_label = "Social dominance orientation",
    unit = "score",
    scale = 1:7,
    outcome_label = "warmth",
    title = "Reducing SDO"
  )

  expect_true(any(grepl("\\ottoshift{personA}{(\\tB,3.5)}{(\\tC,3.5)}{(\\tC,2.5)}", figure$tikz, fixed = TRUE)))
})

test_that("a specification that cannot be drawn is refused", {
  expect_error(draw_deciles(assignment_times = 12), class = "margot_error_target_trial_spec")
  expect_error(draw_deciles(assignment_times = 10:14), class = "margot_error_target_trial_spec")
  expect_error(
    draw_deciles(people = list(A = c(8, 8, 9, 9))),
    class = "margot_error_target_trial_spec"
  )
  expect_error(
    draw_deciles(people = list(A = c(8, 8, NA, 9, 9))),
    class = "margot_error_target_trial_spec"
  )
  expect_error(
    draw_deciles(people = c(decile_people, list(D = rep(3, 5)))),
    class = "margot_error_target_trial_spec"
  )
  expect_error(draw_deciles(build = TRUE), class = "margot_error_target_trial_spec")
})

test_that("a shift function that leaves the scale is refused", {
  arms <- decile_arms
  arms[[2]]$shift <- function(a) a - 1

  expect_error(draw_deciles(arms = arms), class = "margot_error_target_trial_shift")
})

test_that("the source is written with its style file", {
  directory <- withr::local_tempdir()
  file <- file.path(directory, "figure.tex")
  figure <- draw_deciles(file = file)

  expect_true(file.exists(file))
  expect_true(file.exists(file.path(directory, "margot-target-trial.sty")))
  expect_equal(readLines(file), figure$tikz)
})

test_that("the figure builds when pdflatex is available", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pdflatex")), "pdflatex is not available")
  directory <- withr::local_tempdir()
  figure <- draw_deciles(assignment_times = 12:14, file = file.path(directory, "figure.tex"), build = TRUE)

  expect_true(file.exists(file.path(directory, "figure.pdf")))
})
