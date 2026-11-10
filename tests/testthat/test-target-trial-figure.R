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

test_that("a shifted value after an unchanged one leaves the edge of its ring", {
  people <- list(A = c(5, 5, 5, 1, 5))
  tikz <- draw_deciles(assignment_times = 13:14, people = people)$tikz

  expect_true(any(grepl("\\ottohold{personA}{(\\tC,5)}{(\\tD,1)}", tikz, fixed = TRUE)))
  expect_true(any(grepl("\\draw[assignpath, personA, shorten <=5.9pt] (\\tD,1) -- (\\tE,4);", tikz, fixed = TRUE)))
  expect_true(any(grepl("The same hypothetical participant in each arm", tikz, fixed = TRUE)))
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

test_that("columns can stand for several survey times and carry their own labels", {
  labels <- c("$H_{10}$", "$(L_{11}, A_{11})$", "$(L_{12\\text{--}13}, A_{14})$", "$(L_{14}, A_{15})$", "$Y_{16}$")
  draw_gap <- function(time_labels = NULL) {
    margot_target_trial_figure(
      observed = list(A = c(2, 1, 0, 0), B = c(3, 2, 3, 2)),
      times = c(10, 11, 14, 15),
      assignment_times = 14:15,
      outcome_time = 16,
      arms = list(
        list(name = "Zero", descriptor = "set to 0", shift = function(a) 0),
        list(name = "At least four", descriptor = "raised to 4", shift = function(a) pmax(a, 4))
      ),
      exposure_label = "Services in the past month",
      unit = "count",
      scale = 0:7,
      outcome_label = "meaning",
      title = "Attendance",
      time_labels = time_labels
    )$tikz
  }

  tikz <- draw_gap(labels)
  # the survey times keep their numbers, evenly spaced across the gap
  expect_true(any(grepl("\\tA/10,\\tB/11,\\tC/14,\\tD/15,\\endmid/16", tikz, fixed = TRUE)))
  expect_true(any(grepl(
    "\\node[note, align=center, anchor=north] at (\\tC,{\\bottom-\\labeldrop}) {$(L_{12\\text{--}13}, A_{14})$};", tikz,
    fixed = TRUE
  )))
  expect_true(any(grepl("at (\\endmid,{\\bottom-\\labeldrop}) {$Y_{16}$};", tikz, fixed = TRUE)))
  expect_false(any(grepl("{unassigned}", tikz, fixed = TRUE)))

  # without labels the period words are drawn; a label for the outcome time is optional
  expect_true(any(grepl("{unassigned}", draw_gap(), fixed = TRUE)))
  expect_false(any(grepl("\\endmid,{\\bottom-\\labeldrop}", draw_gap(labels[1:4]), fixed = TRUE)))
  # a stacked label takes its second line's room from the foot of the panel
  stacked <- draw_gap(c("$H_{10}$", "$L_{11}$\\\\$A_{11}$", "$L_{12,13}$\\\\$A_{14}$", "$L_{14}$\\\\$A_{15}$"))
  expect_true(any(grepl("\\def\\yorigin{2.16}", stacked, fixed = TRUE)))
  expect_true(any(grepl("\\def\\yorigin{1.82}", tikz, fixed = TRUE)))
  expect_error(draw_gap(labels[1:3]), class = "margot_error_target_trial_spec")
  expect_error(draw_gap(1:4), class = "margot_error_target_trial_spec")
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
  # an endpoint region that leaves no room for the survey times
  expect_error(draw_deciles(outcome_width = 6), class = "margot_error_target_trial_spec")
  expect_error(
    draw_deciles(people = list(A = rep(3, 5), A = rep(4, 5))),
    class = "margot_error_target_trial_spec"
  )
  same_names <- decile_arms
  same_names[[1]]$name <- "Reduction"
  expect_error(draw_deciles(arms = same_names), class = "margot_error_target_trial_spec")
  expect_error(
    draw_deciles(notes = list(list(text = "x", arm = 2, participant = "Z", time = 13, value = 2))),
    class = "margot_error_target_trial_spec"
  )
})

test_that("a scale too fine or too crowded to draw is refused or flagged", {
  draw_scale <- function(scale, value) {
    margot_target_trial_figure(
      observed = list(A = rep(value, 2)), times = 1:2, assignment_times = 2, outcome_time = 3,
      arms = list(
        list(name = "No shift", descriptor = "", shift = NULL),
        list(name = "Same", descriptor = "", shift = function(a) a)
      ),
      exposure_label = "x", unit = "value", scale = scale, outcome_label = "y", title = "t"
    )
  }

  expect_error(draw_scale(seq(0, 0.0009, by = 0.0001), 0.0003), class = "margot_error_target_trial_spec")
  # a narrow scale far from zero would put its origin outside what tex can hold
  expect_error(draw_scale(c(9999, 10000), 9999), class = "margot_error_target_trial_spec")
  # a large scale keeps the height of one exposure unit instead of rounding it to zero
  large <- draw_scale(seq(0, 10000, by = 2000), 2000)$tikz
  expect_true(any(grepl("\\def\\yunit{0.000468}", large, fixed = TRUE)))
  expect_warning(draw_scale(0:12, 3), "0.52 cm")
})

test_that("a shift function that leaves the scale is refused", {
  arms <- decile_arms
  arms[[2]]$shift <- function(a) a - 1

  expect_error(draw_deciles(arms = arms), class = "margot_error_target_trial_shift")

  # the function sees one observed value at a time
  arms[[2]]$shift <- function(a) pmax(1, a - stats::sd(a))
  expect_error(
    draw_deciles(assignment_times = 12:14, arms = arms),
    class = "margot_error_target_trial_shift"
  )
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
  # a space in the name checks that file names reach the shell quoted
  figure <- draw_deciles(assignment_times = 12:14, file = file.path(directory, "my figure.tex"), build = TRUE)

  expect_true(file.exists(file.path(directory, "my figure.pdf")))
  if (nzchar(Sys.which("pdftoppm"))) {
    expect_true(any(basename(figure$files) == "my figure.png"))
  }

  # a tex error reaches the user as the build error, with the log's braces intact
  expect_error(
    draw_deciles(subtitle = "Bad \\nosuchmacro{} subtitle", file = file.path(directory, "bad.tex"), build = TRUE),
    class = "margot_error_target_trial_build"
  )
})
