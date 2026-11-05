# domain tables Bonferroni-corrected over their own outcomes differ only in their
# interval level: twelve outcomes give 99.58% intervals, six give 99.17%
ci_level_fixture <- function(n) {
  z <- stats::qnorm(0.975)
  estimates <- c(0.20, 0.09, 0.03, -0.12, 0.01, 0, 0.05, -0.02, 0.06, 0.07, -0.04, 0.15)
  standard_errors <- c(0.03, 0.02, 0.01, 0.03, 0.02, 0.015, 0.012, 0.02, 0.012, 0.02, 0.012, 0.05)
  fixture <- data.frame(
    outcome = paste0("outcome_", seq_len(12)),
    ATE = estimates,
    standard_error = standard_errors
  )[seq_len(n), ]
  fixture[["2.5 %"]] <- fixture$ATE - z * fixture$standard_error
  fixture[["97.5 %"]] <- fixture$ATE + z * fixture$standard_error
  fixture$E_Value <- NA_real_
  fixture$E_Val_bound <- NA_real_
  fixture
}

ci_level_scale_info <- function(n) {
  data.frame(outcome = paste0("outcome_", seq_len(n)), transformation = "identity",
    center = 4, scale = 1.25, orientation = 1, unit = "scale points", unit_multiplier = 1)
}

ci_level_table <- function(n, ..., threshold = 1.1) {
  suppressMessages(margot_plot(ci_level_fixture(n), adjust = "bonferroni", alpha = 0.05,
    order = "magnitude_desc", scale_info = ci_level_scale_info(n), e_val_bound_threshold = threshold,
    include_coefficients = FALSE, options = list(use_title_case = FALSE), ...))$transformed_table
}

# split a markdown table into a header vector and a matrix of cells
parse_markdown <- function(md) {
  lines <- md[grepl("^\\|", md)]
  split <- function(line) trimws(strsplit(sub("\\|$", "", sub("^\\|", "", line)), "|", fixed = TRUE)[[1]])
  header <- split(lines[1])
  cells <- do.call(rbind, lapply(lines[-(1:2)], split))
  colnames(cells) <- header
  list(header = header, cells = cells)
}

test_that("domains corrected over different numbers of outcomes bind with a per-row level", {
  twelve <- ci_level_table(12)
  six <- ci_level_table(6)
  expect_true("99.58% CI lower" %in% names(twelve))
  expect_true("99.17% CI lower" %in% names(six))

  aligned <- margot:::margot_align_ci_levels(list(twelve, six))
  for (table in aligned) {
    expect_true(all(c("CI lower", "CI upper", "CI lower (scale points)", "CI upper (scale points)",
      "CI relative to 0", "CI level") %in% names(table)))
    expect_false(any(grepl("^[0-9.]+% CI", names(table))))
    # the level sits after the last interval limit
    limits <- which(grepl("^CI (lower|upper)", names(table)))
    expect_identical(which(names(table) == "CI level"), max(limits) + 1L)
  }
  expect_identical(unique(aligned[[1]][["CI level"]]), "99.58%")
  expect_identical(unique(aligned[[2]][["CI level"]]), "99.17%")
  # limits and decisions are unchanged
  expect_identical(aligned[[1]][["CI lower"]], twelve[["99.58% CI lower"]])
  expect_identical(aligned[[2]][["CI upper (scale points)"]], six[["99.17% CI upper (scale points)"]])
  expect_identical(aligned[[2]][["CI relative to 0"]], six[["99.17% CI relative to 0"]])

  parsed <- parse_markdown(margot_bind_tables(list(twelve = twelve, six = six), output_format = "markdown"))
  expect_identical(nrow(parsed$cells), 18L)
  expect_identical(sum(parsed$header == "CI level"), 1L)
  expect_false(any(parsed$cells %in% c("", "NA")))
  levels <- gsub("\\*", "", parsed$cells[, "CI level"])
  expect_identical(levels, rep(c("99.58%", "99.17%"), c(12, 6)))
  expect_identical(gsub("\\*", "", parsed$cells[, "Domain"]), rep(c("twelve", "six"), c(12, 6)))
})

test_that("tables that share one level are left untouched", {
  twelve <- ci_level_table(12)
  expect_identical(margot:::margot_align_ci_levels(list(twelve, twelve)), list(twelve, twelve))
  parsed <- parse_markdown(margot_bind_tables(list(a = twelve, b = twelve), output_format = "markdown"))
  expect_true("99.58% CI lower" %in% parsed$header)
  expect_false("CI level" %in% parsed$header)
})

test_that("sorting and highlighting keep each row's level", {
  twelve <- ci_level_table(12)
  six <- ci_level_table(6)
  parsed <- parse_markdown(margot_bind_tables(list(twelve = twelve, six = six),
    sort_E_val_bound = "desc", e_val_bound_threshold = 1.1, output_format = "markdown"))
  cells <- gsub("\\*", "", parsed$cells)
  bounds <- as.numeric(cells[, "E-value bound"])
  expect_identical(cells[, "Domain"], rep(c("twelve", "six"), c(12, 6)))
  expect_true(all(diff(bounds[1:12]) <= 0))
  expect_true(all(diff(bounds[13:18]) <= 0))
  expect_identical(cells[, "CI level"], rep(c("99.58%", "99.17%"), c(12, 6)))
  # each row's level and limits travel together through the sort
  six_rows <- 13:18
  expected_lower <- six[["99.17% CI lower"]][match(cells[six_rows, "Outcome"], six$Outcome)]
  expect_equal(as.numeric(cells[six_rows, "CI lower"]), expected_lower, tolerance = 1e-12)
  # rows whose bound exceeds the threshold are emphasised
  emphasised <- grepl("^\\*\\*", parsed$cells[, "Outcome"])
  expect_identical(emphasised, bounds > 1.1)
})

test_that("HTML and LaTeX outputs show the level once per row", {
  twelve <- ci_level_table(12)
  six <- ci_level_table(6)
  for (format in c("html", "latex")) {
    rendered <- as.character(margot_bind_tables(list(twelve = twelve, six = six), output_format = format))
    expect_identical(lengths(regmatches(rendered, gregexpr("CI level", rendered, fixed = TRUE))), 1L, info = format)
    level_pattern <- if (format == "latex") "99.58\\\\?%" else "99.58%"
    expect_identical(lengths(regmatches(rendered, gregexpr(level_pattern, rendered))), 12L, info = format)
    expect_match(rendered, "six", fixed = TRUE)
  }
})

test_that("mixed layouts and renamed headers are still refused", {
  twelve <- ci_level_table(12)
  six_legacy <- ci_level_table(6, table_columns = "legacy")
  expect_error(margot_bind_tables(list(report = twelve, legacy = six_legacy)),
    "mix the report and legacy layouts")
  renamed <- ci_level_table(6, rename_ate = "Standardised contrast")
  expect_error(margot_bind_tables(list(twelve = twelve, renamed = renamed)), "do not share the same columns")
  expect_error(margot_bind_tables(list(twelve = twelve, renamed = renamed)), "'Standardised contrast'", fixed = TRUE)
})

test_that("legacy tables with different single levels bind on their confidence_level", {
  twelve <- ci_level_table(12, table_columns = "legacy")
  six <- ci_level_table(6, table_columns = "legacy")
  expect_true(all(c("0.21 %", "99.79 %") %in% names(twelve)))
  expect_true(all(c("0.42 %", "99.58 %") %in% names(six)))
  aligned <- margot:::margot_align_ci_levels(list(twelve, six))
  expect_true(all(c("CI lower", "CI upper", "confidence_level") %in% names(aligned[[2]])))
  expect_false("CI level" %in% names(aligned[[2]]))
  expect_identical(aligned[[2]][["CI lower"]], six[["0.42 %"]])

  parsed <- parse_markdown(margot_bind_tables(list(twelve = twelve, six = six), output_format = "markdown"))
  expect_false(any(parsed$cells %in% c("", "NA")))
  expect_identical(nrow(parsed$cells), 18L)
})

test_that("a table whose coverage already varies by row joins the others", {
  varying_input <- ci_level_fixture(2)
  varying_input$confidence_level <- c(0.9, 0.99)
  # adjust = "none" keeps supplied E-values, so supply them
  varying_input$E_Value <- c(1.8, 1.5)
  varying_input$E_Val_bound <- c(1.3, 1.1)
  varying <- suppressMessages(margot_plot(varying_input, adjust = "none", scale_info = ci_level_scale_info(2),
    e_val_bound_threshold = 1.1, include_coefficients = FALSE,
    options = list(use_title_case = FALSE)))$transformed_table
  expect_true(all(c("CI lower", "CI level") %in% names(varying)))
  six <- ci_level_table(6)
  aligned <- margot:::margot_align_ci_levels(list(six, varying))
  expect_identical(aligned[[2]][["CI level"]], c("90%", "99%"))
  parsed <- parse_markdown(margot_bind_tables(list(six = six, varying = varying), output_format = "markdown"))
  expect_false(any(parsed$cells %in% c("", "NA")))
  expect_setequal(unique(gsub("\\*", "", parsed$cells[, "CI level"])), c("99.17%", "90%", "99%"))
})

test_that("margot_combine_results binds domains with different levels", {
  twelve <- ci_level_table(12)
  six <- ci_level_table(6)
  rendered <- as.character(margot:::margot_combine_results(list(twelve = twelve, six = six), format = "html"))
  expect_identical(lengths(regmatches(rendered, gregexpr("CI level", rendered, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(rendered, gregexpr("99.17%", rendered, fixed = TRUE))), 6L)
  expect_false(grepl(">NA<", rendered, fixed = TRUE))
})

test_that("only complete generated interval headers lose their level", {
  twelve <- ci_level_table(12, rename_ate = "99.58% CI lower estimate")
  six <- ci_level_table(6, rename_ate = "99.17% CI lower estimate")
  for (bind in list(
    function(x) margot_bind_tables(x),
    function(x) margot:::margot_combine_results(x, format = "html")
  )) {
    expect_error(bind(list(twelve = twelve, six = six)), "'99.58% CI lower estimate'", fixed = TRUE)
  }
  # the generated headers of these tables would still align on their own
  aligned <- margot:::margot_align_ci_levels(list(ci_level_table(12), ci_level_table(6)))
  expect_true("CI lower" %in% names(aligned[[1]]))
})

test_that("margot_combine_results refuses tables that do not share their columns", {
  twelve <- ci_level_table(12)
  combine <- function(x) margot:::margot_combine_results(x, format = "html")
  expect_error(combine(list(a = twelve, b = ci_level_table(6, rename_ate = "Contrast"))), "do not share the same columns")
  expect_error(combine(list(a = twelve, b = ci_level_table(6, threshold = 1.2))), "'E-value bound > 1.2'", fixed = TRUE)
  expect_error(combine(list(a = twelve, b = ci_level_table(6, table_columns = "legacy"))), "mix the report and legacy layouts")
})

test_that("renamed limits that leave only the decision header levelled are refused cleanly", {
  rename_limits <- function(level) {
    stats::setNames(
      as.list(paste0(level, c(" CI lower", " CI upper", " CI lower (scale points)", " CI upper (scale points)"))),
      c("Lower", "Upper", "Lower (scale points)", "Upper (scale points)")
    )
  }
  twelve <- ci_level_table(12, rename_cols = TRUE, col_renames = rename_limits("99.58%"))
  six <- ci_level_table(6, rename_cols = TRUE, col_renames = rename_limits("99.17%"))
  expect_true(all(c("Lower", "99.58% CI relative to 0") %in% names(twelve)))
  warnings <- character()
  result <- withCallingHandlers(
    tryCatch(margot_bind_tables(list(twelve = twelve, six = six)), error = function(e) conditionMessage(e)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(result, "do not share the same columns")
  expect_length(warnings, 0L)
})

test_that("a renamed estimate that would clash with a level-free limit is refused", {
  twelve <- ci_level_table(12, rename_ate = "CI lower")
  six <- ci_level_table(6, rename_ate = "CI lower")
  expect_error(margot_bind_tables(list(twelve = twelve, six = six)), "share the header 'CI lower'")
})

test_that("margot_combine_results labels each domain's own rows", {
  twelve <- ci_level_table(12)
  six <- ci_level_table(6)
  rendered <- as.character(margot:::margot_combine_results(list(twelve = twelve, six = six), format = "html"))
  position <- function(pattern) regexpr(pattern, rendered, fixed = TRUE)[[1]]
  last_position <- function(pattern) max(gregexpr(pattern, rendered, fixed = TRUE)[[1]])
  expect_lt(position(">twelve<"), position("99.58%"))
  expect_gt(position(">six<"), last_position("99.58%"))
  expect_lt(position(">six<"), position("99.17%"))
})

test_that("margot_combine_results keeps legacy coverage unrounded", {
  twelve <- ci_level_table(12, table_columns = "legacy")
  six <- ci_level_table(6, table_columns = "legacy")
  rendered <- as.character(margot:::margot_combine_results(list(twelve = twelve, six = six), format = "html"))
  expect_match(rendered, "0.9958333", fixed = TRUE)
  expect_match(rendered, "0.9916667", fixed = TRUE)
  expect_false(grepl(">1.00<", rendered, fixed = TRUE))
})
