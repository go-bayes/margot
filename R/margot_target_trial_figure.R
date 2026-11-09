#' Draw a Target-Trial Figure for a Shift Analysis
#'
#' Writes the TikZ source of a two-panel figure that shows what each arm of a modified treatment policy analysis does to the same hypothetical participants. Exposure is on the vertical axis and survey times are on the horizontal axis. The function applies each arm's shift function to the participants' observed values at every assignment time, so the drawn values come from the analysis code and are never typed by hand.
#'
#' The figure is an estimation figure: at every assignment time the shift function acts on the observed value, which is how sequential estimators of modified treatment policies use the data. A filled dot is an observed value. An open ring is a shifted value, with an arrow from the faded observed value. A ring around a dot marks a value that the shift function returns unchanged. A pale column marks each assignment time.
#'
#' The participants are hypothetical. Supply trajectories that show the cases a reader needs to see, such as a participant at a bound of the scale, and never supply records of real participants.
#'
#' This first version draws two arms, up to three participants and assignment times that form the last surveyed times before the outcome. Times without a usable exposure measurement, sideways spreading of marks that share a value, the theory figure and arm SWIGs are not yet drawn.
#'
#' @param observed A named list with one numeric vector per hypothetical participant, holding that participant's observed exposure at each element of `times`. The names label the participants, for example `A`, `B` and `C`. At most three participants are drawn.
#' @param times Survey times shown on the horizontal axis, in increasing order. The first is baseline.
#' @param assignment_times The times at which the arms assign exposure. They must be the last elements of `times`, and baseline cannot be an assignment time.
#' @param outcome_time The time at which the outcomes are measured, shown in the endpoint region.
#' @param arms A list of two arms, the comparator first. Each arm is a list with `name` (the panel heading), `descriptor` (a short phrase after the heading) and `shift`. `shift` is a function that takes a numeric vector of observed values and returns the shifted values, or `NULL` for an arm that leaves exposure unassigned.
#' @param exposure_label Title of the vertical axis.
#' @param unit Singular word for one exposure value, used in the legend and footer, for example `"decile"`, `"score"` or `"count"`.
#' @param scale Exposure values labelled on the vertical axis, in increasing order.
#' @param scale_ends Optional character vector of two words placed beside the lowest and highest scale values, for example `c("least", "most")`.
#' @param outcome_label Character vector with one element per line of the outcome description.
#' @param title Figure title.
#' @param subtitle Line under the title.
#' @param footer Line at the foot of the figure. The default states that the shift function acts on observed values and that censoring weights address attrition.
#' @param notes Optional list of annotations. Each is a list with `text`, `arm` (1 or 2), `time` and `value` (the position, in axis units; `time` may fall between survey times), and optionally `participant` (a name in `observed`, which sets the colour), `anchor` (a TikZ anchor, default `"south east"`) and `align` (default `"right"`).
#' @param outcome_width Width of the endpoint region in centimetres. Widen it for a long outcome description.
#' @param file Optional path of the `.tex` file to write. The style file `margot-target-trial.sty` is copied beside it.
#' @param build If `TRUE`, run `pdflatex` on `file` and, when `pdftoppm` is available, write a 300 dpi PNG.
#'
#' @details Text arguments are placed in the TikZ source as given, so they may contain LaTeX and must escape characters that LaTeX treats specially.
#'
#' The canvas is 160 mm by 90 mm. A scale with more than ten labelled values leaves less than 0.52 cm per step, and the function warns because a one-step arrow is then hard to see when projected.
#'
#' @return Invisibly, a list with `tikz` (the source as a character vector of lines), `values` (a data frame with one row per arm, participant and time, giving the observed value, the value drawn after the shift function and whether the time is an assignment time) and `files` (the paths written).
#'
#' @examples
#' reduce <- function(a) pmax(1, a - 1)
#' figure <- margot_target_trial_figure(
#'   observed = list(A = c(8, 8, 9, 9, 9), B = c(7, 7, 5, 5, 4), C = c(1, 1, 1, 1, 1)),
#'   times = 10:14,
#'   assignment_times = 14,
#'   outcome_time = 15,
#'   arms = list(
#'     list(name = "No shift", descriptor = "Time 14 decile left unassigned", shift = NULL),
#'     list(name = "Reduction", descriptor = "one decile less deprived, floor of 1", shift = reduce)
#'   ),
#'   exposure_label = "Deprivation decile",
#'   unit = "decile",
#'   scale = 1:10,
#'   scale_ends = c("least", "most"),
#'   outcome_label = c("meaning and", "satisfaction", "(7 measures)"),
#'   title = "Estimated shift analysis: reducing deprivation at Time 14"
#' )
#' figure$values
#'
#' @export
margot_target_trial_figure <- function(observed,
                                       times,
                                       assignment_times,
                                       outcome_time,
                                       arms,
                                       exposure_label,
                                       unit,
                                       scale,
                                       scale_ends = NULL,
                                       outcome_label,
                                       title,
                                       subtitle = "The same three hypothetical participants in each arm",
                                       footer = NULL,
                                       notes = NULL,
                                       outcome_width = 1.83,
                                       file = NULL,
                                       build = FALSE) {
  target_trial_check_inputs(
    observed, times, assignment_times, outcome_time, arms, scale, scale_ends, notes, file, build
  )
  values <- target_trial_values(observed, times, assignment_times, arms, scale)
  geometry <- target_trial_geometry(times, assignment_times, scale, outcome_width, values)
  if (is.null(footer)) {
    footer <- target_trial_footer(arms, unit, assignment_times)
  }
  tikz <- target_trial_document(
    values, observed, times, assignment_times, outcome_time, arms, exposure_label, unit,
    scale, scale_ends, outcome_label, title, subtitle, footer, notes, geometry
  )
  files <- character()
  if (!is.null(file)) {
    files <- target_trial_write(tikz, file, build)
  }
  invisible(list(tikz = tikz, values = values, files = files))
}

# stop with a classed condition when the specification cannot be drawn
target_trial_check_inputs <- function(observed, times, assignment_times, outcome_time, arms,
                                      scale, scale_ends, notes, file, build) {
  stop_spec <- function(message) {
    cli::cli_abort(message, class = "margot_error_target_trial_spec", call = NULL, .envir = parent.frame())
  }
  if (!is.list(observed) || is.null(names(observed)) || any(names(observed) == "")) {
    stop_spec("{.arg observed} must be a named list with one numeric vector per participant.")
  }
  if (length(observed) > 3) {
    stop_spec("At most three hypothetical participants can be drawn.")
  }
  if (!is.numeric(times) || length(times) < 2 || is.unsorted(times, strictly = TRUE)) {
    stop_spec("{.arg times} must hold at least two survey times in increasing order.")
  }
  for (person in names(observed)) {
    trajectory <- observed[[person]]
    if (!is.numeric(trajectory) || length(trajectory) != length(times)) {
      stop_spec("Participant {person} needs one observed value for each element of {.arg times}.")
    }
    if (anyNA(trajectory)) {
      stop_spec("Participant {person} has a missing value. Times without a usable measurement are not yet drawn.")
    }
  }
  n_assigned <- length(assignment_times)
  if (n_assigned < 1 || n_assigned >= length(times) ||
    !isTRUE(all.equal(as.numeric(assignment_times), as.numeric(utils::tail(times, n_assigned))))) {
    stop_spec("{.arg assignment_times} must be the last elements of {.arg times} and cannot include baseline.")
  }
  if (!is.numeric(outcome_time) || length(outcome_time) != 1 || outcome_time <= max(times)) {
    stop_spec("{.arg outcome_time} must be one time after the last element of {.arg times}.")
  }
  if (!is.list(arms) || length(arms) != 2) {
    stop_spec("{.arg arms} must be a list of two arms, the comparator first.")
  }
  for (arm in arms) {
    if (!is.list(arm) || !all(c("name", "descriptor") %in% names(arm))) {
      stop_spec("Each arm needs a {.field name} and a {.field descriptor}.")
    }
    if (!is.null(arm$shift) && !is.function(arm$shift)) {
      stop_spec("An arm's {.field shift} must be a function or {.code NULL}.")
    }
  }
  if (!is.numeric(scale) || length(scale) < 2 || is.unsorted(scale, strictly = TRUE)) {
    stop_spec("{.arg scale} must hold at least two exposure values in increasing order.")
  }
  if (!is.null(scale_ends) && length(scale_ends) != 2) {
    stop_spec("{.arg scale_ends} must give two words: one for the lowest and one for the highest scale value.")
  }
  outside <- vapply(observed, function(x) any(x < min(scale) | x > max(scale)), logical(1))
  if (any(outside)) {
    stop_spec("Observed values for {names(observed)[outside]} fall outside {.arg scale}.")
  }
  for (note in notes) {
    if (!all(c("text", "arm", "time", "value") %in% names(note)) || !note$arm %in% 1:2) {
      stop_spec("Each note needs {.field text}, {.field arm} (1 or 2), {.field time} and {.field value}.")
    }
  }
  if (isTRUE(build) && is.null(file)) {
    stop_spec("{.arg build} needs a {.arg file} to build.")
  }
  invisible(TRUE)
}

# apply each arm's shift function to the observed values at the assignment times
target_trial_values <- function(observed, times, assignment_times, arms, scale) {
  assigned <- times %in% assignment_times
  rows <- list()
  for (arm_index in seq_along(arms)) {
    arm <- arms[[arm_index]]
    for (person in names(observed)) {
      trajectory <- as.numeric(observed[[person]])
      shifted <- trajectory
      if (!is.null(arm$shift)) {
        result <- arm$shift(trajectory[assigned])
        if (!is.numeric(result) || length(result) != sum(assigned) || anyNA(result) ||
          any(result < min(scale) | result > max(scale))) {
          cli::cli_abort(
            "The shift function of arm {.val {arm$name}} must return one value within {.arg scale} for each observed value; check it for participant {person}.",
            class = "margot_error_target_trial_shift", call = NULL
          )
        }
        shifted[assigned] <- result
      }
      rows[[length(rows) + 1]] <- data.frame(
        arm = arm$name,
        participant = person,
        time = times,
        assignment_time = assigned,
        observed = trajectory,
        shifted = shifted,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# positions in centimetres across a panel and in exposure units up it
target_trial_geometry <- function(times, assignment_times, scale, outcome_width, values) {
  panel_width <- 6.55
  n_times <- length(times)
  # the panel is 4.836 cm tall above the lowest gridline. a value near the top of the
  # scale would run through the time-zero label, so the label then gets its own headroom
  top_margin <- 0.156
  y_unit <- (4.836 - top_margin) / (max(scale) - min(scale))
  if (any(c(values$observed, values$shifted) > max(scale) - 0.3 / y_unit)) {
    top_margin <- 0.50
    y_unit <- (4.836 - top_margin) / (max(scale) - min(scale))
  }
  if (y_unit * min(diff(scale)) < 0.52 - 1e-8) {
    cli::cli_warn("The scale leaves less than 0.52 cm per labelled step; a one-step arrow may be hard to see when projected.")
  }
  end_low <- panel_width - outcome_width
  first_x <- if (n_times == 2) 1.00 else 0.55
  column_half <- 0.38
  for (pass in 1:2) {
    last_x <- end_low - 0.11 - column_half
    step <- (last_x - first_x) / (n_times - 1)
    column_half <- min(0.38, 0.42 * step)
  }
  list(
    panel_width = panel_width,
    left_x = 1.90,
    right_x = 8.90,
    y_unit = y_unit,
    y_origin = 1.82 - min(scale) * y_unit,
    bottom = min(scale) - 0.364 / y_unit,
    top = max(scale) + top_margin / y_unit,
    time_x = first_x + step * (seq_len(n_times) - 1),
    time_zero = first_x + min(0.42 * step, 0.55),
    column_half = column_half,
    end_low = end_low,
    end_mid = (end_low + panel_width) / 2,
    # distances below the axis, converted from centimetres to exposure units
    tick_drop = 0.104 / y_unit,
    brace_drop = 0.572 / y_unit,
    label_drop = 0.676 / y_unit,
    rule_drop = 0.99 / y_unit
  )
}

target_trial_footer <- function(arms, unit, assignment_times) {
  n_shifts <- sum(!vapply(arms, function(arm) is.null(arm$shift), logical(1)))
  last_time <- target_trial_number(max(assignment_times))
  acts <- if (length(assignment_times) > 1) {
    paste0("Shift functions act on observed ", unit, "s, from Time ", last_time, " back.")
  } else if (n_shifts > 1) {
    paste0("Both shift functions act on observed ", unit, "s at Time ", last_time, ".")
  } else {
    paste0("The shift function acts on observed ", unit, "s at Time ", last_time, ".")
  }
  paste(acts, "Inverse probability of censoring weights address attrition.")
}

# print a number without trailing zeros
target_trial_number <- function(x) {
  vapply(
    round(x, 3),
    function(value) format(value, trim = TRUE, scientific = FALSE, drop0trailing = TRUE),
    character(1)
  )
}

# the tex macro that holds the horizontal position of the i-th survey time
target_trial_time_macro <- function(i) {
  paste0("\\t", LETTERS[i])
}

target_trial_point <- function(i, value) {
  paste0("(", target_trial_time_macro(i), ",", target_trial_number(value), ")")
}

# tikz for one participant in one arm
target_trial_person <- function(colour, observed, shifted, assigned, has_shift) {
  n_times <- length(observed)
  same <- function(a, b) isTRUE(all.equal(a, b))
  pairs <- function(index) {
    paste0(target_trial_time_macro(index), "/", target_trial_number(observed[index]), collapse = ", ")
  }
  if (!has_shift) {
    return(c(
      paste0("  \\ottohistory{", colour, "}{", pairs(seq_len(n_times)), "}"),
      paste0("  \\ottoconnect{", colour, "}{", target_trial_time_macro(n_times), "}{",
        target_trial_number(observed[n_times]), "}{\\endlo}")
    ))
  }
  first <- which(assigned)[1]
  lines <- paste0("  \\ottohistory{", colour, "}{", pairs(seq_len(first - 1)), "}")
  for (i in which(assigned)) {
    held <- same(observed[i], shifted[i])
    previous_observed <- target_trial_point(i - 1, observed[i - 1])
    here_observed <- target_trial_point(i, observed[i])
    here_shifted <- target_trial_point(i, shifted[i])
    if (i == first) {
      lines <- c(lines, if (held) {
        paste0("  \\ottohold{", colour, "}{", previous_observed, "}{", here_observed, "}")
      } else {
        paste0("  \\ottoshift{", colour, "}{", previous_observed, "}{", here_observed, "}{", here_shifted, "}")
      })
      next
    }
    previous_ring <- target_trial_point(i - 1, shifted[i - 1])
    previous_held <- same(observed[i - 1], shifted[i - 1])
    if (!held) {
      lines <- c(lines, paste0(
        "  \\ottoshiftobs{", colour, "}{", previous_observed, "}{", previous_ring, "}{",
        here_observed, "}{", here_shifted, "}"
      ))
    } else if (previous_held) {
      lines <- c(lines, paste0("  \\ottoholdnext[5.9pt]{", colour, "}{", previous_ring, "}{", here_observed, "}"))
    } else {
      # the previous value was shifted and this one is returned unchanged:
      # the dashed line still leaves a shifted value
      lines <- c(
        lines,
        paste0("  \\draw[natpath, ", colour, ", draw opacity=0.4] ", previous_observed, " -- ", here_observed, ";"),
        paste0("  \\draw[assignpath, ", colour, ", shorten <=4.7pt] ", previous_ring, " -- ", here_observed, ";"),
        paste0("  \\node[heldring=", colour, "] at ", here_observed, " {};"),
        paste0("  \\node[helddot=", colour, "] at ", here_observed, " {};")
      )
    }
  }
  c(lines, paste0(
    "  \\ottoconnect{", colour, "}{", target_trial_time_macro(n_times), "}{",
    target_trial_number(shifted[n_times]), "}{\\endlo}"
  ))
}

# the frame shared by both panels: endpoint region, columns, gridlines, axis and time labels
target_trial_frame <- function(times, assignment_times, outcome_time, scale, outcome_label, geometry) {
  n_times <- length(times)
  assigned <- which(times %in% assignment_times)
  unassigned <- setdiff(seq_len(n_times)[-1], assigned)
  macro <- target_trial_time_macro
  last_macro <- macro(n_times)
  columns <- paste0(macro(assigned), collapse = ",")
  time_labels <- paste0(
    c(paste0(macro(seq_len(n_times)), "/", target_trial_number(times)),
      paste0("\\endmid/", target_trial_number(outcome_time))),
    collapse = ","
  )
  grid_values <- paste0(target_trial_number(scale), collapse = ",")
  lines <- c(
    "% panel frame shared by both arms",
    "\\newcommand{\\panelframe}{%",
    "  \\fill[endregion] (\\endlo,\\bottom) rectangle (\\panelw,\\top);",
    "  \\draw[regionedge] (\\endlo,\\bottom) -- (\\endlo,\\top);",
    paste0("  \\foreach \\x in {", columns, "}"),
    "    {\\fill[assigncol] ({\\x-\\colhalf},\\bottom) rectangle ({\\x+\\colhalf},\\top);}",
    paste0("  \\foreach \\d in {", grid_values, "} {\\draw[gridline] (0,\\d) -- ({", last_macro, "+\\colhalf},\\d);}"),
    "  \\draw[axis] (0,\\bottom) -- (\\panelw,\\bottom);",
    "  \\draw[timezero] (\\tzero,\\bottom) -- (\\tzero,\\top);",
    "  \\node[note, anchor=north west, fill=paper, inner sep=1.5pt]",
    "    at ({\\tzero+0.04},{\\top-\\tickdrop/2}) {time zero};",
    paste0("  \\foreach \\x/\\w in {", time_labels, "}"),
    "    {\\node[font=\\ottolabel, anchor=north] at (\\x,{\\bottom-\\tickdrop}) {\\w};}"
  )
  if (length(unassigned) >= 3) {
    lines <- c(
      lines,
      "  \\node[note, anchor=north] at (\\tA,{\\bottom-\\labeldrop}) {baseline};",
      paste0("  \\draw[periodbrace] ({", macro(min(unassigned)), "-0.25},{\\bottom-\\bracedrop}) -- ({",
        macro(max(unassigned)), "+0.25},{\\bottom-\\bracedrop});"),
      paste0("  \\node[note, anchor=north] at ({0.5*(", macro(min(unassigned)), "+", macro(max(unassigned)),
        ")},{\\bottom-\\labeldrop}) {unassigned};")
    )
  } else if (length(unassigned) >= 1) {
    # too little room to centre both labels: set them either side of time zero
    lines <- c(
      lines,
      "  \\draw[timezero] (\\tzero,\\bottom) -- (\\tzero,{\\bottom-\\ruledrop});",
      "  \\node[note, anchor=north east] at ({\\tzero-0.08},{\\bottom-\\labeldrop}) {baseline};",
      paste0("  \\draw[periodbrace] ({", macro(min(unassigned)), "-0.25},{\\bottom-\\bracedrop}) -- ({",
        macro(max(unassigned)), "+0.25},{\\bottom-\\bracedrop});"),
      "  \\node[note, anchor=north west] at ({\\tzero+0.08},{\\bottom-\\labeldrop}) {unassigned};"
    )
  } else {
    lines <- c(lines, "  \\node[note, anchor=north] at (\\tA,{\\bottom-\\labeldrop}) {baseline};")
  }
  c(
    lines,
    "  \\node[font=\\ottolabel\\bfseries, anchor=north] at (\\endmid,{\\top-\\tickdrop}) {Outcomes};",
    "  \\node[note, align=center, anchor=north] at (\\endmid,{\\top-6.25*\\tickdrop})",
    paste0("    {", paste(outcome_label, collapse = "\\\\"), "};"),
    "}"
  )
}

# one arm's panel: heading, frame, participants, notes and letter labels
target_trial_panel <- function(arm_index, origin_macro, arm, values, observed, times, notes, geometry) {
  colours <- paste0("person", LETTERS[seq_along(observed)])
  lines <- c(
    paste0("% --- panel ", arm_index, ": ", arm$name, " ", strrep("-", 40)),
    paste0("\\node[anchor=base west] at (", origin_macro, ",6.98)"),
    paste0("  {{\\ottopanel ", arm$name, "}\\hspace{0.7em}{\\ottosmall\\color{muted}", arm$descriptor, "}};"),
    paste0("\\begin{scope}[shift={(", origin_macro, ",\\yorigin)}, y=\\yunit cm]"),
    "  \\panelframe"
  )
  for (p in seq_along(observed)) {
    person_rows <- values[values$arm == arm$name & values$participant == names(observed)[p], ]
    lines <- c(lines, target_trial_person(
      colours[p], person_rows$observed, person_rows$shifted, person_rows$assignment_time, !is.null(arm$shift)
    ))
  }
  for (note in notes) {
    if (note$arm != arm_index) next
    colour <- if (is.null(note$participant)) "muted" else colours[match(note$participant, names(observed))]
    x <- stats::approx(times, geometry$time_x, xout = note$time, rule = 2)$y
    anchor <- if (is.null(note$anchor)) "south east" else note$anchor
    align <- if (is.null(note$align)) "right" else note$align
    lines <- c(
      lines,
      paste0("  \\node[font=\\ottosmall, text=", colour, ", align=", align, ", anchor=", anchor, ", inner sep=1pt]"),
      paste0("    at (", target_trial_number(x), ",", target_trial_number(note$value), ") {", note$text, "};")
    )
  }
  first_values <- vapply(observed, function(x) x[1], numeric(1))
  labels <- paste0(colours, "/", names(observed), "/", target_trial_number(first_values), collapse = ", ")
  c(
    lines,
    paste0("  \\foreach \\c/\\n/\\d in {", labels, "}"),
    "    {\\node[personlabel, text=\\c] at ({\\tA-0.10},\\d) {\\n};}",
    "\\end{scope}",
    ""
  )
}

# assemble the standalone document
target_trial_document <- function(values, observed, times, assignment_times, outcome_time, arms,
                                  exposure_label, unit, scale, scale_ends, outcome_label, title,
                                  subtitle, footer, notes, geometry) {
  number <- target_trial_number
  n_times <- length(times)
  definition <- function(name, value, comment) {
    sprintf("%-22s %% %s", paste0("\\def\\", name, "{", number(value), "}"), comment)
  }
  time_role <- ifelse(times %in% assignment_times, ", assignment time", "")
  time_role[1] <- ", baseline"
  end_word_x <- if (max(nchar(number(scale))) >= 2) -0.66 else -0.50
  scale_labels <- paste0(number(scale), collapse = ",")
  axis <- c(
    "% --- shared vertical axis -------------------------------------------------",
    "\\begin{scope}[shift={(\\leftx,\\yorigin)}, y=\\yunit cm]",
    paste0("  \\foreach \\d in {", scale_labels, "}"),
    "    {\\node[font=\\ottolabel, anchor=east] at (-0.10,\\d) {\\d};}"
  )
  if (!is.null(scale_ends)) {
    axis <- c(
      axis,
      paste0("  \\node[note, anchor=east] at (", number(end_word_x), ",", number(max(scale)), ") {", scale_ends[2], "};"),
      paste0("  \\node[note, anchor=east] at (", number(end_word_x), ",", number(min(scale)), ") {", scale_ends[1], "};")
    )
  }
  axis <- c(
    axis,
    "  \\node[font=\\ottolabel, anchor=north east] at (-0.10,{\\bottom-\\tickdrop}) {Time};",
    "  \\node[rotate=90, font=\\ottolabel\\bfseries, anchor=center]",
    paste0("    at (-1.45,", number(mean(range(scale))), ") {", exposure_label, "};"),
    "\\end{scope}",
    ""
  )
  c(
    "% written by margot::margot_target_trial_figure(); the drawn values come from the",
    "% supplied shift functions. edit the specification and regenerate, or adjust",
    "% positions below by hand.",
    "\\documentclass[border=0pt]{standalone}",
    "\\usepackage{margot-target-trial}",
    "",
    "% --- panel geometry (cm across; one y unit = one exposure unit) ---------",
    definition("yunit", geometry$y_unit, "cm per exposure unit"),
    definition("yorigin", geometry$y_origin, "page height of exposure value 0"),
    definition("panelw", geometry$panel_width, "panel width"),
    definition("leftx", geometry$left_x, "left panel origin"),
    definition("rightx", geometry$right_x, "right panel origin"),
    vapply(seq_len(n_times), function(i) {
      definition(paste0("t", LETTERS[i]), geometry$time_x[i], paste0("Time ", number(times[i]), time_role[i]))
    }, character(1)),
    definition("colhalf", geometry$column_half, "half-width of an assignment column"),
    definition("tzero", geometry$time_zero, "time zero, after baseline"),
    definition("endlo", geometry$end_low, "endpoint region"),
    definition("endmid", geometry$end_mid, "centre of the endpoint region"),
    definition("bottom", geometry$bottom, "axis, in exposure units"),
    definition("top", geometry$top, "top of the panel"),
    definition("tickdrop", geometry$tick_drop, "time labels below the axis"),
    definition("bracedrop", geometry$brace_drop, "brace below the axis"),
    definition("labeldrop", geometry$label_drop, "period labels below the axis"),
    definition("ruledrop", geometry$rule_drop, "time-zero rule below the axis"),
    "",
    target_trial_frame(times, assignment_times, outcome_time, scale, outcome_label, geometry),
    "",
    "\\begin{document}",
    "\\sansmath",
    "\\begin{tikzpicture}[x=1cm, y=1cm, every node/.style={text=ink}]",
    "\\useasboundingbox (0,0) rectangle (16,9);",
    "\\fill[paper] (0,0) rectangle (16,9);",
    "",
    "% --- header -------------------------------------------------------------",
    "\\node[anchor=base west, font=\\ottotitle] at (0.6,8.30)",
    paste0("  {", title, "};"),
    "\\fill[kowhai] (0.62,8.02) rectangle (2.10,8.07);",
    "\\node[anchor=base west, font=\\ottosub, text=muted] at (0.6,7.55)",
    paste0("  {", subtitle, "};"),
    "",
    "% legend",
    "\\begin{scope}[shift={(9.75,7.64)}]",
    "  \\draw[natpath, ink] (0,0) -- (0.45,0);",
    "  \\node[natdot=ink] at (0.45,0) {};",
    paste0("  \\node[legendtext] at (0.56,0) {observed ", unit, "};"),
    "  \\draw[assignpath, ink] (3.00,0) -- (3.45,0);",
    "  \\node[assigndot=ink] at (3.45,0) {};",
    paste0("  \\node[legendtext] at (3.57,0) {shifted ", unit, "};"),
    "\\end{scope}",
    "",
    axis,
    target_trial_panel(1, "\\leftx", arms[[1]], values, observed, times, notes, geometry),
    target_trial_panel(2, "\\rightx", arms[[2]], values, observed, times, notes, geometry),
    "% --- footer ---------------------------------------------------------------",
    "\\node[anchor=base west, font=\\ottosmall, text=muted] at (0.6,0.16)",
    paste0("  {", footer, "};"),
    "",
    "\\end{tikzpicture}",
    "\\end{document}"
  )
}

# write the source and its style file, and optionally build the pdf and png
target_trial_write <- function(tikz, file, build) {
  directory <- dirname(file)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  writeLines(tikz, file)
  style <- system.file("tikz", "margot-target-trial.sty", package = "margot")
  style_copy <- file.path(directory, "margot-target-trial.sty")
  file.copy(style, style_copy, overwrite = TRUE)
  files <- c(file, style_copy)
  if (!isTRUE(build)) {
    return(files)
  }
  pdflatex <- Sys.which("pdflatex")
  if (!nzchar(pdflatex)) {
    cli::cli_abort("{.arg build} needs {.code pdflatex} on the search path.",
      class = "margot_error_target_trial_build", call = NULL
    )
  }
  stem <- tools::file_path_sans_ext(basename(file))
  old_directory <- setwd(directory)
  on.exit(setwd(old_directory), add = TRUE)
  log <- suppressWarnings(system2(
    pdflatex, c("-interaction=nonstopmode", "-halt-on-error", basename(file)),
    stdout = TRUE, stderr = TRUE
  ))
  pdf <- paste0(stem, ".pdf")
  if (!identical(attr(log, "status") %||% 0L, 0L) || !file.exists(pdf)) {
    cli::cli_abort(c("pdflatex could not build {.file {file}}.", utils::tail(log, 12)),
      class = "margot_error_target_trial_build", call = NULL
    )
  }
  unlink(paste0(stem, c(".aux", ".log")))
  files <- c(files, file.path(directory, pdf))
  pdftoppm <- Sys.which("pdftoppm")
  if (nzchar(pdftoppm)) {
    system2(pdftoppm, c("-png", "-r", "300", "-singlefile", pdf, stem))
    files <- c(files, file.path(directory, paste0(stem, ".png")))
  }
  files
}
