# synthetic twelve-outcome fixture with edge cases, as in the reporting panel's harness:
# an interval that excludes zero with a small E-value bound, a bound near 1.1,
# negative effects, a zero estimate and intervals that cross zero
make_decision_fixture <- function() {
  z <- stats::qnorm(0.975)
  ev <- function(d) {
    r <- exp(0.91 * abs(d))
    r + sqrt(r * (r - 1))
  }
  d_at_1.1 <- stats::uniroot(function(d) ev(d) - 1.1, c(0, 1))$root
  zb <- stats::qnorm(1 - 0.05 / (2 * 12))
  fixture <- data.frame(
    outcome = c("trust", "warmth", "edge_small", "anxiety", "null_cross", "exact_zero",
      "threshold_edge", "small_negative_cross", "moderate", "clear_positive",
      "negative_small_bound", "wide_positive"),
    ATE = c(0.20, 0.09, 0.03, -0.12, 0.01, 0, d_at_1.1 + zb * 0.012, -0.02, 0.05, 0.07, -0.04, 0.15),
    standard_error = c(0.03, 0.02, 0.01, 0.03, 0.02, 0.015, 0.012, 0.02, 0.012, 0.02, 0.012, 0.05)
  )
  fixture[["2.5 %"]] <- fixture$ATE - z * fixture$standard_error
  fixture[["97.5 %"]] <- fixture$ATE + z * fixture$standard_error
  fixture$E_Value <- NA_real_
  fixture$E_Val_bound <- NA_real_
  fixture
}

decision_labels <- c(trust = "Trust in science", warmth = "Warmth to refugees",
  edge_small = "Interval excludes zero, small bound", anxiety = "Anxiety",
  null_cross = "Interval crosses zero", exact_zero = "Zero estimate",
  threshold_edge = "E-value bound near threshold", small_negative_cross = "Small negative, crosses zero",
  moderate = "Moderate positive", clear_positive = "Clear positive",
  negative_small_bound = "Negative, small bound", wide_positive = "Wide positive")

decision_scale_info <- function(fixture) {
  data.frame(outcome = fixture$outcome, transformation = "identity",
    center = 4, scale = 1.25, orientation = 1, unit = "scale points", unit_multiplier = 1)
}

decision_plot <- function(...) {
  fixture <- make_decision_fixture()
  suppressMessages(margot_plot(fixture, adjust = "bonferroni", alpha = 0.05,
    order = "magnitude_desc", label_mapping = decision_labels,
    scale_info = decision_scale_info(fixture), e_val_bound_threshold = 1.1,
    save_output = FALSE, ...))
}

# the plot's top-to-bottom order: ggplot draws the last factor level at the top
plot_top_to_bottom <- function(plot) rev(levels(plot$data$outcome))

test_that("categories follow the corrected interval alone by default", {
  out <- decision_plot()
  data <- out$plot$data
  category <- stats::setNames(as.character(data$Estimate), data$original_var_name)

  # the corrected interval decides the category
  expect_identical(levels(data$Estimate), c("positive", "includes null", "negative"))
  expect_true(all(category[data$original_var_name[data$`2.5 %` > 0]] == "positive"))
  expect_true(all(category[data$original_var_name[data$`97.5 %` < 0]] == "negative"))
  expect_identical(
    sort(names(category)[category == "includes null"]),
    sort(c("null_cross", "exact_zero", "small_negative_cross"))
  )

  # an interval that excludes zero keeps its category when its bound is small
  expect_lt(data$E_Val_bound[data$original_var_name == "edge_small"], 1.1)
  expect_identical(category[["edge_small"]], "positive")
  expect_identical(category[["negative_small_bound"]], "negative")
  expect_identical(category[["wide_positive"]], "positive")

  # neutral legend wording; no "not reliable" in the default outputs
  built <- ggplot2::ggplot_build(out$plot)
  legend_labels <- built$plot$scales$get_scales("colour")$get_labels()
  expect_identical(legend_labels, c("Interval above 0", "Interval includes 0", "Interval below 0"))
  expect_false(any(grepl("not reliable", c(legend_labels, out$interpretation, unlist(out$transformed_table)))))
})

test_that("the E-value flag is a separate table column and never changes the decision", {
  out <- decision_plot()
  table <- out$transformed_table

  decision <- "99.58% CI relative to 0"
  expect_identical(names(table)[1], "Outcome")
  expect_true(all(c(decision, "E-value", "E-value bound", "E-value bound > 1.1") %in% names(table)))
  expect_identical(table[[decision]][table$Outcome == "Interval excludes zero, small bound"], "above 0")
  expect_identical(table[["E-value bound > 1.1"]][table$Outcome == "Interval excludes zero, small bound"], "no")
  expect_identical(
    table[["E-value bound > 1.1"]],
    ifelse(table[["E-value bound"]] > 1.1, "yes", "no")
  )
  expect_setequal(unique(table[[decision]]), c("above 0", "includes 0", "below 0"))
  expect_false(anyDuplicated(names(table)) > 0)
})

test_that("the E-value flag uses a strict comparison by default", {
  input <- tibble::tibble(
    outcome = c("equal", "above"),
    ATE = c(0.3, 0.4),
    `2.5 %` = c(0.1, 0.2),
    `97.5 %` = c(0.5, 0.6),
    E_Value = c(1.5, 1.6),
    E_Val_bound = c(1.1, 1.1 + 1e-9)
  )
  row <- function(out, outcome) match(outcome, out$transformed_table$Outcome)
  args <- list(input, type = "RD", e_val_bound_threshold = 1.1, include_coefficients = FALSE,
    options = list(use_title_case = FALSE))
  strict <- suppressMessages(do.call(margot_plot, args))
  inclusive <- suppressMessages(do.call(margot_plot, c(args, list(e_val_bound_comparison = ">="))))

  expect_identical(strict$transformed_table[row(strict, c("equal", "above")), "E-value bound > 1.1"], c("no", "yes"))
  expect_identical(inclusive$transformed_table[row(inclusive, c("equal", "above")), "E-value bound >= 1.1"], c("yes", "yes"))
  expect_match(strict$interpretation, "E-value bound = 1.100 (does not exceed 1.1)", fixed = TRUE)
  # a bound that only rounds to the threshold is printed with enough decimals to agree with its flag
  expect_match(strict$interpretation, "E-value bound = 1.100000001 (exceeds 1.1)", fixed = TRUE)
  # emphasis in the table follows the same comparison
  bold <- suppressMessages(do.call(margot_plot, c(args, list(bold_rows = TRUE))))
  expect_setequal(bold$transformed_table$Outcome, c("equal", "**above**"))
  # the category is the same under either comparison
  expect_identical(as.character(strict$plot$data$Estimate), c("positive", "positive"))
  expect_identical(as.character(inclusive$plot$data$Estimate), c("positive", "positive"))
})

test_that("corrected E-value bounds use the exact conversion of the limit nearest the null", {
  fixture <- make_decision_fixture()
  corrected <- margot_correct_combined_table(fixture, adjust = "bonferroni", alpha = 0.05, scale = "RD")
  rounded <- margot_correct_combined_table(fixture, adjust = "bonferroni", alpha = 0.05, scale = "RD",
    e_val_bound_conversion = "rounded")
  z_star <- stats::qnorm(1 - 0.05 / (2 * 12))

  # exact: RR = exp(0.91 |d|) at the corrected limit nearest zero, that is exp(0.91 (|est| - z* SE))
  nearest <- ifelse(fixture$ATE > 0, corrected$`2.5 %`, corrected$`97.5 %`)
  excludes <- corrected$`2.5 %` > 0 | corrected$`97.5 %` < 0
  rr <- exp(0.91 * abs(nearest))
  expected <- ifelse(excludes, rr + sqrt(rr * (rr - 1)), 1)
  expect_equal(corrected$E_Val_bound, expected, tolerance = 1e-14)
  rr_se <- exp(0.91 * (abs(fixture$ATE[excludes]) - z_star * fixture$standard_error[excludes]))
  expect_equal(corrected$E_Val_bound[excludes], rr_se + sqrt(rr_se * (rr_se - 1)), tolerance = 1e-12)

  # the threshold edge case sits just above 1.1 under the exact conversion
  edge <- fixture$outcome == "threshold_edge"
  expect_equal(corrected$E_Val_bound[edge], 1.1, tolerance = 1e-4)
  expect_gt(corrected$E_Val_bound[edge], 1.1)

  # rounded: the earlier 1.78 x SE* with SE* rescaled from the corrected interval
  se_star <- (rounded$`97.5 %` - rounded$ATE) / stats::qnorm(0.975)
  expected_rounded <- vapply(seq_len(nrow(fixture)), function(i) {
    margot:::.margot_evalues_ols(fixture$ATE[i], se = se_star[i], sd = 1)[["E_Val_bound"]]
  }, numeric(1))
  expect_equal(rounded$E_Val_bound, expected_rounded, tolerance = 1e-14)
  expect_true(all(rounded$E_Val_bound[excludes] > corrected$E_Val_bound[excludes]))

  # point E-values and intervals do not depend on the conversion
  expect_identical(corrected$E_Value, rounded$E_Value)
  expect_identical(corrected$`2.5 %`, rounded$`2.5 %`)

  # margot_plot uses the exact conversion and passes the legacy option through
  expect_equal(decision_plot()$plot$data$E_Val_bound,
    corrected$E_Val_bound[match(decision_plot()$plot$data$original_var_name, fixture$outcome)],
    tolerance = 1e-14)
})

test_that("the interpretation lists every interval that excludes zero in plot order with its flag", {
  out <- decision_plot()
  lines <- strsplit(out$interpretation, "\n", fixed = TRUE)[[1]]
  bullets <- lines[startsWith(lines, "- ")]
  listed <- sub(":.*$", "", substring(bullets, 3))

  table <- out$transformed_table
  excludes <- table$Outcome[table[["99.58% CI relative to 0"]] != "includes 0"]
  expect_identical(listed, excludes)
  expect_identical(listed, plot_top_to_bottom(out$plot)[plot_top_to_bottom(out$plot) %in% excludes])
  expect_length(listed, 9L)

  # each bullet names the model-scale quantity with the table's header
  expect_true(all(grepl(": ATE (standardised) = ", bullets, fixed = TRUE)))
  edge_line <- bullets[listed == "Interval excludes zero, small bound"]
  expect_match(edge_line, "E-value bound = 1.036 (does not exceed 1.1)", fixed = TRUE)
  expect_match(bullets[listed == "E-value bound near threshold"], "E-value bound = 1.1001 (exceeds 1.1)", fixed = TRUE)
  expect_match(out$interpretation, "They are listed in plot order, from top to bottom.", fixed = TRUE)
  expect_match(out$interpretation, "beyond the measured covariates", fixed = TRUE)
  expect_match(out$interpretation, "the approximation RR = exp(0.91 |d|)", fixed = TRUE)
  expect_match(out$interpretation, "The note in brackets says whether the bound exceeds 1.1;", fixed = TRUE)
  expect_match(out$interpretation, "Bonferroni correction", fixed = TRUE)
  expect_match(out$interpretation, "99.58% confidence intervals", fixed = TRUE)
  expect_match(out$interpretation, "12 comparisons", fixed = TRUE)
  expect_match(out$interpretation, "Of the 12 average treatment effects, 9 have adjusted confidence intervals that exclude 0", fixed = TRUE)
  expect_match(out$interpretation, "the other 3 outcomes include 0: Small negative, crosses zero; Interval crosses zero; Zero estimate.", fixed = TRUE)
})

test_that("each ordering runs from the top of the plot and the table and prose follow it", {
  fixture <- make_decision_fixture()
  labels <- decision_labels[fixture$outcome]
  magnitude <- abs(fixture$ATE)
  custom <- rev(sort(unname(labels)))
  expected_top <- list(
    magnitude_desc = labels[[which.max(magnitude)]],
    magnitude_asc = labels[[which.min(magnitude)]],
    alphabetical = sort(unname(labels))[[1]],
    custom = custom[[1]]
  )

  for (ordering in names(expected_top)) {
    args <- list(fixture, adjust = "bonferroni", order = ordering, label_mapping = decision_labels,
      e_val_bound_threshold = 1.1, include_coefficients = FALSE)
    if (ordering == "custom") args$custom_order <- custom
    out <- suppressMessages(do.call(margot_plot, args))
    top_to_bottom <- plot_top_to_bottom(out$plot)
    bullets <- strsplit(out$interpretation, "\n", fixed = TRUE)[[1]]
    listed <- sub(":.*$", "", substring(bullets[startsWith(bullets, "- ")], 3))

    expect_identical(top_to_bottom[[1]], expected_top[[ordering]], info = ordering)
    expect_identical(out$transformed_table$Outcome, top_to_bottom, info = ordering)
    expect_identical(listed, top_to_bottom[top_to_bottom %in% listed], info = ordering)
    if (ordering == "custom") expect_identical(top_to_bottom, custom)
  }

  # evaluebound_desc puts the largest corrected bound at the top
  out <- suppressMessages(margot_plot(fixture, adjust = "bonferroni", order = "evaluebound_desc",
    label_mapping = decision_labels, include_coefficients = FALSE))
  bounds <- out$transformed_table[["E-value bound"]]
  expect_identical(bounds, sort(bounds, decreasing = TRUE))
})

test_that("supplied colours keyed by the earlier level name still apply", {
  out <- decision_plot(options = list(colors = c(positive = "red", "not reliable" = "grey40", negative = "blue")))
  scale <- ggplot2::ggplot_build(out$plot)$plot$scales$get_scales("colour")
  expect_identical(unname(scale$map(c("positive", "includes null", "negative"))), c("red", "grey40", "blue"))
})

test_that("risk-ratio plots describe intervals relative to one", {
  input <- tibble::tibble(
    outcome = c("higher", "lower", "unclear"),
    `E[Y(1)]/E[Y(0)]` = c(1.5, 0.7, 1.05),
    `2.5 %` = c(1.2, 0.5, 0.9),
    `97.5 %` = c(1.9, 0.95, 1.2),
    E_Value = c(2.37, 2.21, 1.27),
    E_Val_bound = c(1.69, 1.29, 1)
  )
  out <- suppressMessages(margot_plot(input, type = "RR", include_coefficients = FALSE,
    options = list(use_title_case = FALSE)))
  table <- out$transformed_table
  expect_identical(table[match(c("higher", "lower", "unclear"), table$Outcome), "95% CI relative to 1"],
    c("above 1", "below 1", "includes 1"))
  expect_match(out$interpretation, "exclude 1", fixed = TRUE)
  expect_false(grepl("exp(0.91", out$interpretation, fixed = TRUE))
  expect_true("ATE (risk ratio)" %in% names(table))
})

test_that("legacy options reproduce the categories, E-values, table and prose of margot 1.3.2", {
  # reference values produced by margot main (1.3.2.9000, commit 1421ea5) on this fixture;
  # the display label "E-value bound at threshold" was later renamed "near threshold"
  main_levels <- c("Trust in science", "Wide positive", "Anxiety", "Warmth to refugees",
    "Clear positive", "Moderate positive", "E-value bound near threshold", "Negative, small bound",
    "Interval excludes zero, small bound", "Small negative, crosses zero", "Interval crosses zero",
    "Zero estimate")
  main_keys <- c("trust", "wide_positive", "anxiety", "warmth", "clear_positive", "moderate",
    "threshold_edge", "negative_small_bound", "edge_small", "small_negative_cross", "null_cross",
    "exact_zero")
  main_categories <- c("positive", "not reliable", "negative", "positive", "positive", "positive",
    "positive", "not reliable", "not reliable", "not reliable", "not reliable", "not reliable")
  main_bounds <- c(1.4581319112183322, 1.0867423483044134, 1.2122477368163227, 1.2070133773524114,
    1.1206386493516125, 1.1351362814987047, 1.1004634988238682, 1.0773971335315329,
    1.0370664471990478, 1, 1, 1)
  main_evalues <- c(1.6889607176012267, 1.5557000719702674, 1.474132108000181, 1.3897013827031621,
    1.3305341275860969, 1.2672725501062652, 1.245379478409641, 1.233144099793509,
    1.196323690812481, 1.1551290599487309, 1.1051889070468919, 1)
  main_table_names <- c("ATE", "0.21 %", "99.79 %", "confidence_level", "E_Value", "E_Val_bound",
    "reported_estimate", "reported_lower", "reported_upper", "reporting_quantity", "reporting_unit")
  main_interpretation <- paste0(
    "Confidence intervals and E-values were adjusted for 12 comparisons using Bonferroni correction ",
    "(Bonferroni-adjusted 99.58% confidence intervals). \n\nThe following estimates of average ",
    "treatment effects meet the specified reporting threshold (E‑value lower bound >= 1.1):\n\n",
    "- E-value bound near threshold: 0.044 (99.58% CI: 0.009 to 0.078); on the original scale, mean difference = 0.054 scale points (99.58% CI: 0.011 to 0.097). E-value bound = 1.10\n",
    "- Moderate positive: 0.050 (99.58% CI: 0.016 to 0.084); on the original scale, mean difference = 0.062 scale points (99.58% CI: 0.020 to 0.105). E-value bound = 1.14\n",
    "- Clear positive: 0.070 (99.58% CI: 0.013 to 0.127); on the original scale, mean difference = 0.088 scale points (99.58% CI: 0.016 to 0.159). E-value bound = 1.12\n",
    "- Warmth to refugees: 0.090 (99.58% CI: 0.033 to 0.147); on the original scale, mean difference = 0.112 scale points (99.58% CI: 0.041 to 0.184). E-value bound = 1.21\n",
    "- Anxiety: -0.120 (99.58% CI: -0.206 to -0.034); on the original scale, mean difference = -0.150 scale points (99.58% CI: -0.257 to -0.043). E-value bound = 1.21\n",
    "- Trust in science: 0.200 (99.58% CI: 0.114 to 0.286); on the original scale, mean difference = 0.250 scale points (99.58% CI: 0.143 to 0.357). E-value bound = 1.46"
  )

  legacy <- decision_plot(
    category_rule = "interval_and_evalue",
    e_val_bound_comparison = ">=",
    e_val_bound_conversion = "rounded",
    order_direction = "bottom_to_top",
    table_columns = "legacy"
  )
  data <- legacy$plot$data

  expect_identical(levels(data$outcome), main_levels)
  expect_identical(data$original_var_name, main_keys)
  expect_identical(levels(data$Estimate), c("positive", "not reliable", "negative"))
  expect_identical(as.character(data$Estimate), main_categories)
  expect_equal(data$E_Val_bound, main_bounds, tolerance = 1e-14)
  expect_equal(data$E_Value, main_evalues, tolerance = 1e-14)

  table <- legacy$transformed_table
  expect_identical(names(table), main_table_names)
  expect_identical(rownames(table), rev(main_levels))
  expect_equal(table$E_Val_bound, rev(main_bounds), tolerance = 1e-14)
  expect_equal(table$E_Value, rev(main_evalues), tolerance = 1e-14)
  expect_identical(legacy$interpretation, main_interpretation)
  expect_identical(legacy$plot$labels$colour %||% "Estimate", "Estimate")

  # the two decision arguments alone reproduce the earlier categories on this fixture
  partial <- decision_plot(category_rule = "interval_and_evalue", e_val_bound_comparison = ">=")
  partial_category <- stats::setNames(as.character(partial$plot$data$Estimate), partial$plot$data$original_var_name)
  expect_identical(unname(partial_category[main_keys]), main_categories)
})

test_that("holm and BH stop under the interval rule because their intervals cannot carry it", {
  fixture <- make_decision_fixture()
  for (method in c("holm", "BH")) {
    expect_error(
      suppressMessages(margot_plot(fixture, adjust = method, label_mapping = decision_labels)),
      "cannot be combined with adjust",
      info = method
    )
    expect_error(
      margot:::margot_interpret_marginal(fixture, adjust = method),
      "cannot be combined with adjust",
      info = method
    )
  }

  # margot's holm intervals exclude zero even for an outcome with raw p = 0.62,
  # which is why an interval decision is refused
  holm <- margot_correct_combined_table(fixture, adjust = "holm", scale = "RD")
  null_cross <- fixture$outcome == "null_cross"
  expect_gt(2 * stats::pnorm(-abs(fixture$ATE / fixture$standard_error))[null_cross], 0.5)
  expect_false(holm$`2.5 %`[null_cross] < 0 && holm$`97.5 %`[null_cross] > 0)

  # under bonferroni the same non-significant outcome is classed as including the null
  bonferroni <- decision_plot()
  expect_identical(
    as.character(bonferroni$plot$data$Estimate[bonferroni$plot$data$original_var_name == "null_cross"]),
    "includes null"
  )

  # the message names the shared limitation of the legacy rule
  expect_error(
    suppressMessages(margot_plot(fixture, adjust = "holm", label_mapping = decision_labels)),
    "carries the same limitation"
  )

  # the legacy rule still reproduces earlier holm output, with one warning per call
  warnings <- character()
  legacy <- withCallingHandlers(
    suppressMessages(margot_plot(fixture, adjust = "holm", label_mapping = decision_labels,
      category_rule = "interval_and_evalue", e_val_bound_threshold = 1.1)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(legacy$plot, "ggplot")
  expect_length(grep("exclude the null by construction", warnings, fixed = TRUE), 1L)
  expect_warning(
    margot:::margot_interpret_marginal(fixture, adjust = "BH", category_rule = "interval_and_evalue"),
    "exclude the null by construction"
  )
})

test_that("report-table headers never collide and a clash stops", {
  fixture <- make_decision_fixture()[1:3, ]
  unitless <- decision_scale_info(fixture)
  unitless$unit <- ""
  out <- suppressMessages(margot_plot(fixture, adjust = "none", scale_info = unitless,
    rename_ate = "Mean difference", include_coefficients = FALSE))
  table <- out$transformed_table
  expect_false(anyDuplicated(names(table)) > 0)
  expect_true(all(c("Mean difference", "Mean difference (original scale)",
    "95% CI lower (original scale)") %in% names(table)))
  # the model-scale estimate stays inside its own interval
  expect_true(all(table[["Mean difference"]] >= table[["95% CI lower"]] &
    table[["Mean difference"]] <= table[["95% CI upper"]]))
  expect_equal(table[["Mean difference (original scale)"]], table[["Mean difference"]] * 1.25)

  expect_error(
    suppressMessages(margot_plot(fixture, adjust = "none", rename_ate = "E-value", include_coefficients = FALSE)),
    "share the header 'E-value'"
  )
  expect_error(
    suppressMessages(margot_plot(fixture, adjust = "none", include_coefficients = FALSE,
      rename_cols = TRUE, col_renames = list("E-value" = "E-value bound"))),
    "share the header"
  )
})

test_that("bounds print with three decimals and enough digits to agree with the flag", {
  fmt <- margot:::format_margot_evalue_bound
  expect_identical(fmt(1.4577, 1.1, ">"), "1.458")
  expect_identical(fmt(1.1, 1.1, ">"), "1.100")
  expect_identical(fmt(1.10005126520, 1.1, ">"), "1.1001")
  expect_identical(fmt(1.1 + .Machine$double.eps, 1.1, ">"), "1.1000000000000003")
  expect_identical(fmt(1.1 - .Machine$double.eps, 1.1, ">"), "1.0999999999999999")
  expect_identical(fmt(1.1 - .Machine$double.eps, 1.1, ">="), "1.0999999999999999")
  # the printed value always agrees with the flag
  for (x in c(1.0995, 1.1 - 1e-12, 1.1, 1.1 + 1e-12, 1.1004)) {
    printed <- as.numeric(fmt(x, 1.1, ">"))
    expect_identical(printed > 1.1, x > 1.1, info = x)
  }
})

test_that("the exact conversion can leave a bound below the threshold that the rounded one passes", {
  rr_at <- function(e_value) e_value^2 / (2 * e_value - 1)
  d_threshold <- log(rr_at(1.1)) / 0.91
  z_star <- stats::qnorm(1 - 0.05 / 4)
  se <- 0.012
  # place the adjusted lower limit just below the exact threshold
  estimate <- d_threshold - 2e-5 + z_star * se
  input <- data.frame(outcome = c("between_conversions", "clear"), ATE = c(estimate, 0.3))
  input[["2.5 %"]] <- input$ATE - stats::qnorm(0.975) * c(se, 0.05)
  input[["97.5 %"]] <- input$ATE + stats::qnorm(0.975) * c(se, 0.05)
  input$E_Value <- NA_real_
  input$E_Val_bound <- NA_real_

  args <- list(input, adjust = "bonferroni", e_val_bound_threshold = 1.1, include_coefficients = FALSE,
    options = list(use_title_case = FALSE, remove_underscores = FALSE))
  exact <- suppressMessages(do.call(margot_plot, args))
  rounded <- suppressMessages(do.call(margot_plot, c(args, list(e_val_bound_conversion = "rounded"))))
  pick <- function(out) out$transformed_table[out$transformed_table$Outcome == "between_conversions", ]

  expect_lt(pick(exact)[["E-value bound"]], 1.1)
  expect_gt(pick(rounded)[["E-value bound"]], 1.1)
  expect_identical(pick(exact)[["E-value bound > 1.1"]], "no")
  expect_identical(pick(rounded)[["E-value bound > 1.1"]], "yes")
  # the decision is the same under either conversion
  expect_identical(pick(exact)[["97.5% CI relative to 0"]], "above 0")
  expect_identical(pick(rounded)[["97.5% CI relative to 0"]], "above 0")
})

test_that("magnitude orderings rank ratios by their distance from one on the log scale", {
  input <- tibble::tibble(
    outcome = c("harm_big", "harm_small", "protective"),
    `E[Y(1)]/E[Y(0)]` = c(2, 1.1, 0.4),
    `2.5 %` = c(1.5, 1.02, 0.3),
    `97.5 %` = c(2.6, 1.2, 0.55),
    E_Value = c(3.41, 1.43, 4.4),
    E_Val_bound = c(2.37, 1.16, 3.1)
  )
  desc <- suppressMessages(margot_plot(input, type = "RR", order = "magnitude_desc",
    include_coefficients = FALSE, options = list(use_title_case = FALSE, remove_underscores = FALSE)))
  asc <- suppressMessages(margot_plot(input, type = "RR", order = "magnitude_asc",
    include_coefficients = FALSE, options = list(use_title_case = FALSE, remove_underscores = FALSE)))
  expect_identical(desc$transformed_table$Outcome, c("protective", "harm_big", "harm_small"))
  expect_identical(plot_top_to_bottom(desc$plot), c("protective", "harm_big", "harm_small"))
  expect_identical(asc$transformed_table$Outcome, c("harm_small", "harm_big", "protective"))
})

test_that("standalone prose says it follows the requested order rather than a plot", {
  input <- tibble::tibble(
    outcome = c("a", "b"),
    ATE = c(0.3, -0.2),
    `2.5 %` = c(0.1, -0.35),
    `97.5 %` = c(0.5, -0.05),
    E_Value = c(1.9, 1.7),
    E_Val_bound = c(1.4, 1.2)
  )
  standalone <- margot:::margot_interpret_marginal(input, order = "alphabetical", e_val_bound_threshold = 1.1)
  expect_match(standalone$interpretation, "listed in the requested order", fixed = TRUE)
  expect_false(grepl("plot order", standalone$interpretation, fixed = TRUE))
  expect_match(standalone$interpretation, "- a: ATE = 0.300", fixed = TRUE)
})

test_that("the figure repeats the category in the point shape and keeps coefficients out of the legend", {
  out <- decision_plot()
  # the coefficients form a column just outside the panel, headed "Estimate"
  layers <- out$plot$layers
  is_text <- vapply(layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))
  maps_label <- vapply(layers, function(layer) !is.null(layer$mapping$label), logical(1))
  built <- ggplot2::ggplot_build(out$plot)
  coefficients <- built$data[[which(is_text & maps_label)]]
  top_to_bottom <- plot_top_to_bottom(out$plot)
  labels <- stats::setNames(trimws(coefficients$label), levels(out$plot$data$outcome)[coefficients$y])
  expect_identical(labels[["Trust in science"]], "0.20")
  expect_identical(labels[["Anxiety"]], "-0.12")
  expect_true(all(is.infinite(coefficients$x) & coefficients$x > 0))
  expect_identical(trimws(layers[[which(is_text & !maps_label)]]$aes_params$label), "Estimate")
  expect_identical(layers[[which(is_text & maps_label)]]$aes_params$colour, "black")
  expect_identical(out$plot$coordinates$clip, "off")

  scales <- built$plot$scales
  shape_map <- scales$get_scales("shape")$map(c("positive", "includes null", "negative"))
  expect_equal(unname(shape_map), c(24, 21, 25))
  expect_identical(out$plot$labels$x, "Standardised effect (difference scale)")

  # colouring coefficients by category draws them inside the panel, out of the legend
  inside <- decision_plot(options = list(coefficient_colour = NULL))
  layers <- inside$plot$layers
  is_text <- vapply(layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))
  maps_label <- vapply(layers, function(layer) !is.null(layer$mapping$label), logical(1))
  expect_false(layers[[which(is_text & maps_label)]]$show.legend)
  # the coefficient column is named above its top row
  header_layer <- layers[[which(is_text & !maps_label)]]
  expect_identical(header_layer$aes_params$label, "Estimate")
  expect_equal(header_layer$data$y, 12 + 0.55)

  plain <- decision_plot(options = list(point_shapes = NULL, coefficient_colour = NULL, coefficient_header = NULL))
  expect_null(ggplot2::ggplot_build(plain$plot)$plot$scales$get_scales("shape"))
  expect_equal(sum(vapply(plain$plot$layers, function(layer) inherits(layer$geom, "GeomText"), logical(1))), 1)
})

test_that("margot_bind_tables sorts and highlights either table layout", {
  out <- decision_plot()
  legacy <- decision_plot(table_columns = "legacy")
  bound_order <- out$transformed_table$Outcome[order(-out$transformed_table[["E-value bound"]])]

  for (table in list(out$transformed_table, legacy$transformed_table)) {
    md <- margot_bind_tables(table, sort_E_val_bound = "desc", e_val_bound_threshold = 1.1)
    rows <- md[grepl("^\\|", md)][-(1:2)]
    first_cells <- trimws(gsub("\\*", "", vapply(strsplit(rows, "|", fixed = TRUE), `[`, character(1), 2)))
    expect_identical(first_cells, bound_order)
    # rows whose bound exceeds the threshold are emphasised
    bold_rows <- grepl("**", rows, fixed = TRUE)
    expect_identical(bold_rows, sort(out$transformed_table[["E-value bound"]], decreasing = TRUE) > 1.1)
  }

  # domain lists bind report tables too
  domains <- margot_bind_tables(list(first = out$transformed_table, second = out$transformed_table),
    sort_E_val_bound = "asc")
  expect_true(any(grepl("Trust in science", domains, fixed = TRUE)))

  combined <- margot:::margot_combine_results(list(first = out$transformed_table), format = "html")
  expect_match(as.character(combined), "Trust in science", fixed = TRUE)
  expect_match(as.character(combined), "Variable", fixed = TRUE)
})

test_that("input columns cannot stand in for the computed ordering", {
  base <- tibble::tibble(
    outcome = c("small", "large"),
    ATE = c(0.1, 0.2),
    `2.5 %` = c(0.05, 0.1),
    `97.5 %` = c(0.15, 0.3),
    E_Value = c(1.4, 1.6),
    E_Val_bound = c(1.2, 1.3)
  )
  # columns named like local values used inside group_tab()
  masked <- base
  masked$magnitude <- c(3, 2)
  masked$custom_order <- c("x", "y")
  masked$type <- c("RR", "RR")
  masked$effect_col <- c("E_Value", "E_Value")
  run <- function(data, ordering, ...) {
    out <- suppressMessages(margot_plot(data, order = ordering, include_coefficients = FALSE,
      options = list(use_title_case = FALSE), ...))
    out$transformed_table$Outcome
  }
  expect_identical(run(masked, "magnitude_desc"), c("large", "small"))
  expect_identical(run(masked, "magnitude_asc"), c("small", "large"))
  expect_identical(run(masked, "custom", custom_order = c("small", "large")), c("small", "large"))
  expect_identical(run(masked, "magnitude_desc"), run(base, "magnitude_desc"))

  annotated <- margot:::group_tab(masked, type = "RD", order = "magnitude_desc")
  expect_identical(as.character(annotated$Estimate), c("positive", "positive"))
  expect_match(annotated$estimate_lab[1], "^0.2 \\(0.1-0.3\\)")
})

test_that("custom estimate headers reach the prose", {
  renamed <- decision_plot(rename_ate = "Standardised contrast")
  expect_true("Standardised contrast" %in% names(renamed$transformed_table))
  expect_match(renamed$interpretation, "- Trust in science: Standardised contrast = 0.200", fixed = TRUE)

  remapped <- decision_plot(rename_cols = TRUE, col_renames = list("Effect (SD)" = "ATE (standardised)"))
  expect_true("Effect (SD)" %in% names(remapped$transformed_table))
  expect_match(remapped$interpretation, "- Trust in science: Effect (SD) = 0.200", fixed = TRUE)
})

test_that("renaming the Outcome column keeps bolding on the renamed column", {
  out <- decision_plot(rename_cols = TRUE, col_renames = list(Endpoint = "Outcome"), bold_rows = TRUE)
  table <- out$transformed_table
  expect_false("Outcome" %in% names(table))
  expect_identical(names(table)[1], "Endpoint")
  flagged <- table[["E-value bound > 1.1"]] == "yes"
  expect_true(all(grepl("^\\*\\*.*\\*\\*$", table$Endpoint[flagged])))
  expect_false(any(grepl("**", table$Endpoint[!flagged], fixed = TRUE)))
})

test_that("ratio prose uses fixed decimals for each scale", {
  input <- tibble::tibble(
    outcome = c("protective", "harmful"),
    `E[Y(1)]/E[Y(0)]` = c(0.5, 1.6),
    `2.5 %` = c(0.4, 1.3),
    `97.5 %` = c(0.62, 1.95),
    E_Value = c(3.41, 2.58),
    E_Val_bound = c(2.61, 1.92)
  )
  out <- suppressMessages(margot_plot(input, type = "RR", include_coefficients = FALSE,
    options = list(use_title_case = FALSE)))
  expect_match(out$interpretation, "ATE (risk ratio) = 0.50 (95% CI: 0.40 to 0.62)", fixed = TRUE)
  expect_match(out$interpretation, "ATE (risk ratio) = 1.60 (95% CI: 1.30 to 1.95)", fixed = TRUE)
  expect_match(out$interpretation, "E-value bound = 2.610 (exceeds 1.2)", fixed = TRUE)

  # original units share one choice of decimals within a bullet, with no negative zero
  expect_identical(margot:::format_margot_fixed(-0.0001, 3), "0.000")
  expect_identical(margot:::margot_unit_digits(c(0.95, 0.4, 1.5)), 2L)
  expect_identical(margot:::format_margot_fixed(12345.678, 0), "12,346")
})

test_that("margot_bind_tables refuses mixed layouts and sorts within domains", {
  out <- decision_plot()
  legacy <- decision_plot(table_columns = "legacy")
  expect_error(
    margot_bind_tables(list(report = out$transformed_table, legacy = legacy$transformed_table),
      sort_E_val_bound = "desc"),
    "mix the report and legacy layouts"
  )

  first <- out$transformed_table[1:6, ]
  second <- out$transformed_table[7:12, ]
  md <- margot_bind_tables(list(first = first, second = second), sort_E_val_bound = "desc",
    output_format = "markdown")
  rows <- md[grepl("^\\|", md)][-(1:2)]
  cells <- strsplit(rows, "|", fixed = TRUE)
  domains <- trimws(gsub("\\*", "", vapply(cells, `[`, character(1), 2)))
  outcomes <- trimws(gsub("\\*", "", vapply(cells, `[`, character(1), 3)))
  # domains stay contiguous and in their supplied order
  expect_identical(domains, rep(c("first", "second"), each = 6))
  expect_identical(outcomes[1:6], first$Outcome[order(-first[["E-value bound"]])])
  expect_identical(outcomes[7:12], second$Outcome[order(-second[["E-value bound"]])])

  html <- margot_bind_tables(list(first = first, second = second), sort_E_val_bound = "asc",
    output_format = "html")
  expect_match(as.character(html), "first", fixed = TRUE)
})

test_that("outcomes without an interval are reported as having no decision", {
  input <- tibble::tibble(
    outcome = c("a", "b", "c"),
    ATE = c(0.05, -0.02, 0.1),
    `2.5 %` = c(-0.05, -0.1, NA),
    `97.5 %` = c(0.15, 0.06, NA),
    E_Value = c(1.2, 1.1, 1.4),
    E_Val_bound = c(1, 1, NA)
  )
  run <- function(data) {
    suppressMessages(margot_plot(data, include_coefficients = FALSE, options = list(use_title_case = FALSE)))
  }
  crossing <- run(input)
  expect_match(crossing$interpretation,
    "The confidence intervals of both average treatment effects with a confidence interval include 0.", fixed = TRUE)
  expect_match(crossing$interpretation,
    "No confidence interval was available for c, so no interval decision was made for it.", fixed = TRUE)
  table <- crossing$transformed_table
  expect_identical(table[["95% CI relative to 0"]][table$Outcome == "c"], "not available")
  # the outcome stays in the table but is not drawn
  expect_setequal(as.character(crossing$plot$data$outcome), c("a", "b"))
  expect_setequal(table$Outcome, c("a", "b", "c"))

  mixed <- input
  mixed[1, c("2.5 %", "97.5 %")] <- list(0.01, 0.09)
  out <- run(mixed)
  expect_match(out$interpretation,
    "Of the 2 average treatment effects with a confidence interval, 1 has a confidence interval that excludes 0.", fixed = TRUE)
  expect_match(out$interpretation, "No confidence interval was available for c", fixed = TRUE)
  expect_match(out$interpretation, "The confidence interval of the other outcome includes 0: b.", fixed = TRUE)
  expect_match(out$interpretation, "- a: ATE (standardised) = 0.050", fixed = TRUE)

  # two outcomes without a missing interval read "both", not "all 2"
  both <- run(input[1:2, ])
  expect_match(both$interpretation, "The confidence intervals of both average treatment effects include 0.", fixed = TRUE)
  expect_false(grepl("all 2", both$interpretation, fixed = TRUE))
})

test_that("prose without adjustment says only that this call applied none", {
  out <- decision_plot()
  fixture <- make_decision_fixture()
  corrected <- margot_correct_combined_table(fixture, adjust = "bonferroni", scale = "RD")
  again <- suppressMessages(margot_plot(corrected, adjust = "none", label_mapping = decision_labels,
    include_coefficients = FALSE))
  expect_match(again$interpretation,
    "Intervals are 99.58% confidence intervals; this call applied no further adjustment for multiple comparisons.",
    fixed = TRUE)
  expect_false(grepl("with no adjustment", again$interpretation, fixed = TRUE))
})

test_that("printed limits keep their side of the null and stay distinct", {
  ratio <- tibble::tibble(
    outcome = "near_one",
    `E[Y(1)]/E[Y(0)]` = 0.992,
    `2.5 %` = 0.988,
    `97.5 %` = 0.996,
    E_Value = 1.1,
    E_Val_bound = 1.08
  )
  out <- suppressMessages(margot_plot(ratio, type = "RR", include_coefficients = FALSE,
    options = list(use_title_case = FALSE, remove_underscores = FALSE)))
  expect_match(out$interpretation, "ATE (risk ratio) = 0.992 (95% CI: 0.988 to 0.996)", fixed = TRUE)
  expect_false(grepl("1.00)", out$interpretation, fixed = TRUE))

  difference <- tibble::tibble(
    outcome = "near_zero",
    ATE = 0.0006,
    `2.5 %` = 0.0002,
    `97.5 %` = 0.001,
    E_Value = 1.01,
    E_Val_bound = 1.002
  )
  out <- suppressMessages(margot_plot(difference, include_coefficients = FALSE,
    options = list(use_title_case = FALSE, remove_underscores = FALSE)))
  expect_match(out$interpretation, "ATE (standardised) = 0.0006 (95% CI: 0.0002 to 0.0010)", fixed = TRUE)

  digits <- margot:::margot_triple_digits
  expect_identical(digits(c(0.5, 0.4, 0.62), 1, 2L), 2L)
  expect_identical(digits(c(0.992, 0.988, 0.996), 1, 2L), 3L)
  expect_identical(digits(c(-0.12, -0.206, -0.034), 0, 3L), 3L)
})

test_that("chained column renames give the prose the final header", {
  out <- decision_plot(rename_cols = TRUE,
    col_renames = list(Intermediate = "ATE (standardised)", Final = "Intermediate"))
  expect_true("Final" %in% names(out$transformed_table))
  expect_false("Intermediate" %in% names(out$transformed_table))
  expect_match(out$interpretation, "- Trust in science: Final = 0.200", fixed = TRUE)
  expect_false(grepl("Intermediate", out$interpretation, fixed = TRUE))
})

test_that("margot_bind_tables reads the layout from the schema, not the bound spelling", {
  report <- decision_plot()$transformed_table
  legacy_renamed <- decision_plot(table_columns = "legacy", rename_cols = TRUE,
    col_renames = list("E-value bound" = "E_Val_bound"))$transformed_table
  expect_true("E-value bound" %in% names(legacy_renamed))
  expect_error(
    margot_bind_tables(list(report = report, legacy = legacy_renamed), sort_E_val_bound = "desc"),
    "mix the report and legacy layouts"
  )

  # two report tables whose bound columns are spelled differently bind as one layout
  respelled <- report
  names(respelled)[names(respelled) == "E-value bound"] <- "E-Value bound"
  md <- margot_bind_tables(list(first = report[1:6, ], second = respelled[7:12, ]),
    sort_E_val_bound = "desc", output_format = "markdown")
  header <- md[grepl("^\\|", md)][1]
  expect_false(grepl("E-Value bound", header, fixed = TRUE))
  rows <- md[grepl("^\\|", md)][-(1:2)]
  expect_length(rows, 12L)
  # every row has its bound, so none is left blank
  cells <- strsplit(rows, "|", fixed = TRUE)
  bound_index <- which(trimws(strsplit(header, "|", fixed = TRUE)[[1]]) == "E-value bound")
  bounds <- trimws(gsub("\\*", "", vapply(cells, `[`, character(1), bound_index)))
  expect_false(any(bounds %in% c("", "NA")))
})

test_that("ratio coefficients sit clear of the intervals", {
  input <- tibble::tibble(
    outcome = c("protective", "harmful", "small"),
    `E[Y(1)]/E[Y(0)]` = c(0.5, 1.6, 1.05),
    `2.5 %` = c(0.4, 1.3, 0.95),
    `97.5 %` = c(0.62, 1.95, 1.16),
    E_Value = c(3.41, 2.58, 1.28),
    E_Val_bound = c(2.61, 1.92, 1)
  )
  # by default the coefficients sit outside the right edge of the panel, so no
  # interval runs under them
  out <- suppressMessages(margot_plot(input, type = "RR", options = list(use_title_case = FALSE)))
  text_index <- which(vapply(out$plot$layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
  outside <- ggplot2::ggplot_build(out$plot)$data[[text_index]]
  expect_true(all(is.infinite(outside$x) & outside$x > 0))
  expect_lte(max(input$`97.5 %`), out$plot$coordinates$limits$x[2])

  # inside the panel, the label column is moved clear of the intervals
  inside <- suppressMessages(margot_plot(input, type = "RR",
    options = list(use_title_case = FALSE, coefficient_colour = NULL)))
  layers <- inside$plot$layers
  text_index <- which(vapply(layers, function(layer) {
    inherits(layer$geom, "GeomText") && !is.null(layer$mapping$label)
  }, logical(1)))
  label_x <- unique(ggplot2::ggplot_build(inside$plot)$data[[text_index]]$x)
  limits <- inside$plot$coordinates$limits$x
  # the label column, about 15% of the panel, ends before the smallest lower limit
  expect_lte(label_x + 0.15 * diff(limits), min(input$`2.5 %`))
  expect_equal(label_x, limits[1])

  # caller-set positions are kept
  kept <- suppressMessages(margot_plot(input, type = "RR",
    options = list(use_title_case = FALSE, x_lim_lo = 0.3, x_lim_hi = 2, x_offset = 0.35)))
  expect_equal(kept$plot$coordinates$limits$x, c(0.3, 2))
})

test_that("one missing limit gives no decision even when the other sits at the null", {
  cases <- list(
    list(type = "RD", lower = 0, upper = NA, estimate = 0.1, null = "0"),
    list(type = "RD", lower = NA, upper = 0, estimate = -0.1, null = "0"),
    list(type = "RR", lower = 1, upper = NA, estimate = 1.2, null = "1"),
    list(type = "RR", lower = NA, upper = 1, estimate = 0.8, null = "1")
  )
  for (case in cases) {
    info <- paste(case$type, case$lower, case$upper)
    category <- margot:::margot_interval_category(case$lower, case$upper, as.numeric(case$null))
    expect_true(is.na(category), info = info)

    effect_name <- if (case$type == "RR") "E[Y(1)]/E[Y(0)]" else "ATE"
    input <- data.frame(outcome = c("partial", "crossing"), check.names = FALSE)
    input[[effect_name]] <- c(case$estimate, if (case$type == "RR") 1.05 else 0.05)
    input[["2.5 %"]] <- c(case$lower, if (case$type == "RR") 0.9 else -0.05)
    input[["97.5 %"]] <- c(case$upper, if (case$type == "RR") 1.2 else 0.15)
    input$E_Value <- c(NA, 1.2)
    input$E_Val_bound <- c(NA, 1)
    out <- suppressMessages(margot_plot(input, type = case$type, include_coefficients = FALSE,
      options = list(use_title_case = FALSE)))
    table <- out$transformed_table
    decision <- table[[paste0("95% CI relative to ", case$null)]]
    expect_identical(decision[table$Outcome == "partial"], "not available", info = info)
    expect_identical(as.character(out$plot$data$outcome), "crossing", info = info)
    expect_match(out$interpretation, "No confidence interval was available for partial", fixed = TRUE, info = info)
    expect_match(out$interpretation, "Only one estimate has a confidence interval, and it includes", fixed = TRUE, info = info)
  }
})

test_that("tiny intervals fall back to significant digits that keep the decision visible", {
  fmt <- margot:::margot_format_triple
  difference <- fmt(c(2e-11, 1e-11, 3e-11), 0, 3L)
  expect_identical(difference, c("2e-11", "1e-11", "3e-11"))
  ratio <- fmt(1 + c(2e-11, 1e-11, 3e-11), 1, 2L)
  expect_true(all(as.numeric(ratio) > 1))
  expect_identical(length(unique(ratio)), 3L)
  below <- fmt(1 - c(2e-11, 3e-11, 1e-11), 1, 2L)
  expect_true(all(as.numeric(below) < 1))
  # ordinary values keep fixed decimals
  expect_identical(fmt(c(0.2, 0.114, 0.286), 0, 3L), c("0.200", "0.114", "0.286"))

  input <- tibble::tibble(
    outcome = "tiny",
    ATE = 2e-11,
    `2.5 %` = 1e-11,
    `97.5 %` = 3e-11,
    E_Value = 1,
    E_Val_bound = 1
  )
  out <- suppressMessages(margot_plot(input, include_coefficients = FALSE,
    options = list(use_title_case = FALSE)))
  expect_match(out$interpretation, "ATE (standardised) = 2e-11 (95% CI: 1e-11 to 3e-11)", fixed = TRUE)
  expect_false(grepl("0.0000000000", out$interpretation, fixed = TRUE))
})

test_that("report-table row names repeat the outcome labels", {
  out <- decision_plot()
  table <- out$transformed_table
  expect_identical(rownames(table), table$Outcome)

  bold <- decision_plot(bold_rows = TRUE)
  expect_identical(rownames(bold$transformed_table), bold$transformed_table$Outcome)
  expect_true(any(grepl("^\\*\\*", rownames(bold$transformed_table))))

  renamed <- decision_plot(rename_cols = TRUE, col_renames = list(Endpoint = "Outcome"))
  expect_identical(rownames(renamed$transformed_table), renamed$transformed_table$Endpoint)

  # margot renders each label once
  html <- as.character(margot_bind_tables(table, output_format = "html"))
  expect_identical(lengths(regmatches(html, gregexpr("Trust in science", html, fixed = TRUE))), 1L)
  html_list <- as.character(margot_bind_tables(list(first = table[1:6, ], second = table[7:12, ]),
    output_format = "html"))
  expect_identical(lengths(regmatches(html_list, gregexpr("Trust in science", html_list, fixed = TRUE))), 1L)
  md <- margot_bind_tables(table, output_format = "markdown")
  expect_identical(sum(grepl("Trust in science", md, fixed = TRUE)), 1L)
  combined <- as.character(margot:::margot_combine_results(list(first = table), format = "html"))
  expect_identical(lengths(regmatches(combined, gregexpr("Trust in science", combined, fixed = TRUE))), 1L)
})

test_that("several outcomes without an interval are joined in plain English", {
  expect_identical(margot:::margot_join_names("a"), "a")
  expect_identical(margot:::margot_join_names(c("a", "b")), "a and b")
  expect_identical(margot:::margot_join_names(c("a", "b", "c")), "a, b and c")

  input <- tibble::tibble(
    outcome = c("a", "b", "c", "d"),
    ATE = c(0.05, 0.1, 0.2, -0.1),
    `2.5 %` = c(-0.05, NA, NA, NA),
    `97.5 %` = c(0.15, NA, NA, NA),
    E_Value = c(1.2, NA, NA, NA),
    E_Val_bound = c(1, NA, NA, NA)
  )
  out <- suppressMessages(margot_plot(input, include_coefficients = FALSE, order = "alphabetical",
    options = list(use_title_case = FALSE)))
  expect_match(out$interpretation,
    "No confidence interval was available for b, c and d, so no interval decision was made for them.", fixed = TRUE)
})

test_that("the displayed threshold keeps every stated comparison true", {
  input <- tibble::tibble(
    outcome = c("close", "clear"),
    ATE = c(0.3, 0.4),
    `2.5 %` = c(0.1, 0.2),
    `97.5 %` = c(0.5, 0.6),
    E_Value = c(1.5, 1.6),
    E_Val_bound = c(1.100000005, 1.4)
  )
  run <- function(threshold) {
    suppressMessages(margot_plot(input, e_val_bound_threshold = threshold, include_coefficients = FALSE,
      options = list(use_title_case = FALSE)))
  }
  precise <- run(1.10000001)
  table <- precise$transformed_table
  expect_true("E-value bound > 1.10000001" %in% names(table))
  expect_identical(table[["E-value bound > 1.10000001"]][table$Outcome == "close"], "no")
  expect_match(precise$interpretation, "(does not exceed 1.10000001)", fixed = TRUE)
  expect_match(precise$interpretation, "the bound exceeds 1.10000001;", fixed = TRUE)
  expect_false(grepl("exceed 1.1)", precise$interpretation, fixed = TRUE))

  # the usual epsilon adjustment still reads as 1.1
  usual <- run(1.1 + .Machine$double.eps)
  expect_true("E-value bound > 1.1" %in% names(usual$transformed_table))
  expect_match(usual$interpretation, "(exceeds 1.1)", fixed = TRUE)

  fmt <- margot:::format_margot_threshold
  expect_identical(fmt(1.1, c(1.4, 1.036)), "1.1")
  expect_identical(fmt(1.10000001, 1.100000005), "1.10000001")
  expect_identical(fmt(1.1 + .Machine$double.eps, c(1.458, 1.1000513)), "1.1")
})

test_that("legacy columns renamed by col_renames reach the prose", {
  renamed <- decision_plot(table_columns = "legacy", rename_cols = TRUE, col_renames = list(Contrast = "ATE"))
  expect_true("Contrast" %in% names(renamed$transformed_table))
  expect_match(renamed$interpretation, "- Trust in science: Contrast = 0.200", fixed = TRUE)

  plain <- decision_plot(table_columns = "legacy")
  expect_match(plain$interpretation, "- Trust in science: ATE (standardised) = 0.200", fixed = TRUE)
})

test_that("renderers keep a renamed outcome column and print each label once", {
  table <- decision_plot(rename_cols = TRUE, col_renames = list(Endpoint = "Outcome"))$transformed_table
  expect_identical(attr(table, "margot_outcome_column"), "Endpoint")
  count <- function(text) lengths(regmatches(text, gregexpr("Trust in science", text, fixed = TRUE)))

  md <- margot_bind_tables(table, output_format = "markdown")
  expect_identical(sum(grepl("Trust in science", md, fixed = TRUE)), 1L)
  expect_false(any(grepl("| Outcome", md, fixed = TRUE)))
  expect_identical(count(as.character(margot_bind_tables(table, output_format = "html"))), 1L)
  expect_identical(count(as.character(margot_bind_tables(list(first = table, second = table),
    output_format = "html"))), 2L)
  expect_identical(count(as.character(margot:::margot_combine_results(list(first = table), format = "html"))), 1L)
})

test_that("a comma decimal mark neither warns nor changes the printed numbers", {
  withr::local_options(OutDec = ",", warn = 2)
  out <- decision_plot()
  expect_match(out$interpretation, "family-wise alpha = 0.05", fixed = TRUE)
  expect_match(out$interpretation, "ATE (standardised) = 0.200 (99.58% CI: 0.114 to 0.286)", fixed = TRUE)
  expect_match(out$interpretation, "E-value bound = 1.1001 (exceeds 1.1)", fixed = TRUE)
  expect_true("E-value bound > 1.1" %in% names(out$transformed_table))

  precise <- tibble::tibble(
    outcome = c("close", "tiny"),
    ATE = c(0.3, 2e-11),
    `2.5 %` = c(0.1, 1e-11),
    `97.5 %` = c(0.5, 3e-11),
    E_Value = c(1.5, 1),
    E_Val_bound = c(1.100000005, 1)
  )
  out <- suppressMessages(margot_plot(precise, e_val_bound_threshold = 1.10000001,
    include_coefficients = FALSE, options = list(use_title_case = FALSE)))
  expect_match(out$interpretation, "(does not exceed 1.10000001)", fixed = TRUE)
  expect_match(out$interpretation, "2e-11 (95% CI: 1e-11 to 3e-11)", fixed = TRUE)
  expect_identical(margot:::format_margot_fixed(1234.5, 1), "1,234.5")
})

test_that("domain tables with differently named outcome columns bind into one column", {
  renamed <- decision_plot(rename_cols = TRUE, col_renames = list(Endpoint = "Outcome"))$transformed_table
  plain <- decision_plot()$transformed_table
  md <- margot_bind_tables(list(first = renamed, second = plain), output_format = "markdown")
  header <- trimws(strsplit(md[grepl("^\\|", md)][1], "|", fixed = TRUE)[[1]])
  expect_identical(header[2:3], c("Domain", "Endpoint"))
  expect_false("Outcome" %in% header)
  rows <- md[grepl("^\\|", md)][-(1:2)]
  labels <- trimws(gsub("\\*", "", vapply(strsplit(rows, "|", fixed = TRUE), `[`, character(1), 3)))
  expect_identical(labels, c(renamed$Endpoint, plain$Outcome))

  # a table that already has a column with the first table's header cannot be renamed into it
  clash <- plain
  clash$Endpoint <- "x"
  expect_error(margot_bind_tables(list(first = renamed, second = clash)), "Cannot use 'Endpoint'")
})

test_that("group_tab orders a grouped table as a whole and keeps its grouping", {
  input <- tibble::tibble(
    outcome = c("a", "b", "c", "d"),
    family = c("x", "x", "y", "y"),
    ATE = c(0.1, 0.4, 0.3, 0.2),
    `2.5 %` = c(0.05, 0.3, 0.2, 0.1),
    `97.5 %` = c(0.15, 0.5, 0.4, 0.3),
    E_Value = c(1.3, 1.9, 1.7, 1.5),
    E_Val_bound = c(1.1, 1.6, 1.4, 1.2)
  )
  grouped <- dplyr::group_by(input, family)
  desc <- margot:::group_tab(grouped, type = "RD", order = "magnitude_desc")
  asc <- margot:::group_tab(grouped, type = "RD", order = "magnitude_asc")
  custom <- margot:::group_tab(grouped, type = "RD", order = "custom", custom_order = c("c", "a", "d", "b"))
  expect_identical(desc$ATE, c(0.4, 0.3, 0.2, 0.1))
  expect_identical(asc$ATE, c(0.1, 0.2, 0.3, 0.4))
  expect_identical(custom$outcome, c("c", "a", "d", "b"))
  expect_identical(dplyr::group_vars(desc), "family")
  expect_identical(desc$ATE, margot:::group_tab(input, type = "RD", order = "magnitude_desc")$ATE)
})

test_that("a legacy table with renamed columns is still recognised as legacy", {
  report <- decision_plot()$transformed_table
  legacy <- decision_plot(table_columns = "legacy", rename_cols = TRUE,
    col_renames = list(Coverage = "confidence_level", Lower = "2.5 %", Upper = "97.5 %",
      "E-value bound" = "E_Val_bound"))$transformed_table
  # the legacy layout renames before it writes its quantile headers
  expect_true(all(c("Coverage", "Lower", "Upper") %in% names(legacy)))
  expect_false(any(c("confidence_level", "0.21 %", "99.79 %") %in% names(legacy)))
  expect_identical(attr(legacy, "margot_table_layout"), "legacy")
  expect_identical(attr(report, "margot_table_layout"), "report")
  expect_error(
    margot_bind_tables(list(report = report, legacy = legacy)),
    "mix the report and legacy layouts"
  )
})

test_that("the binding guard compares columns, so copies without attributes behave", {
  report <- decision_plot()$transformed_table
  legacy <- decision_plot(table_columns = "legacy")$transformed_table
  copy <- legacy[, names(legacy), drop = FALSE]
  expect_null(attr(copy, "margot_table_layout"))

  # a legacy table and its attribute-free copy share their columns and bind
  md <- margot_bind_tables(list(original = legacy, copy = copy), sort_E_val_bound = "desc")
  rows <- md[grepl("^\\|", md)][-(1:2)]
  expect_length(rows, 24L)
  expect_false(any(grepl("|NA|", gsub(" ", "", rows), fixed = TRUE)))

  # an intact report table and the copy do not, and the error names the columns
  expect_error(margot_bind_tables(list(report = report, copy = copy)), "do not share the same columns")
  expect_error(margot_bind_tables(list(report = report, copy = copy)), "'0.21 %'", fixed = TRUE)

  # tables corrected for different family sizes bind with a per-row level, but
  # a table without the original-scale columns still differs and is refused
  fixture <- make_decision_fixture()[1:6, ]
  smaller <- suppressMessages(margot_plot(fixture, adjust = "bonferroni",
    label_mapping = decision_labels, scale_info = decision_scale_info(fixture),
    e_val_bound_threshold = 1.1, include_coefficients = FALSE))$transformed_table
  bound <- margot_bind_tables(list(all = report, some = smaller), output_format = "markdown")
  expect_true(any(grepl("CI level", bound, fixed = TRUE)))
  no_units <- suppressMessages(margot_plot(fixture, adjust = "bonferroni",
    label_mapping = decision_labels, e_val_bound_threshold = 1.1, include_coefficients = FALSE))$transformed_table
  expect_error(margot_bind_tables(list(all = report, some = no_units)), "do not share the same columns")
})

test_that("group_tab restores the drop policy of a grouped table", {
  input <- tibble::tibble(
    outcome = c("a", "b"),
    family = factor(c("x", "x"), levels = c("x", "y")),
    ATE = c(0.1, 0.4),
    `2.5 %` = c(0.05, 0.3),
    `97.5 %` = c(0.15, 0.5),
    E_Value = c(1.3, 1.9),
    E_Val_bound = c(1.1, 1.6)
  )
  kept <- dplyr::group_by(input, family, .drop = FALSE)
  out <- margot:::group_tab(kept, type = "RD", order = "magnitude_desc")
  expect_false(dplyr::group_by_drop_default(out))
  expect_identical(dplyr::n_groups(out), dplyr::n_groups(kept))
  dropped <- margot:::group_tab(dplyr::group_by(input, family), type = "RD", order = "magnitude_desc")
  expect_true(dplyr::group_by_drop_default(dropped))
})

test_that("a table with both bound spellings keeps both and nothing is overwritten", {
  report <- decision_plot()$transformed_table
  both <- report
  both$E_Val_bound <- both[["E-value bound"]] + 1
  expect_error(margot_bind_tables(list(first = report, second = both)), "'E_Val_bound'", fixed = TRUE)
  # binding the double-spelled table with itself keeps both columns intact
  md <- margot_bind_tables(list(first = both, second = both), output_format = "markdown")
  header <- trimws(strsplit(md[grepl("^\\|", md)][1], "|", fixed = TRUE)[[1]])
  expect_identical(sum(header == "E-value bound"), 1L)
  expect_identical(sum(header %in% c("E_Val_bound", "E-Value bound")), 1L)

  # a single outcome skips the requested correction instead of stopping
  single <- suppressMessages(margot_plot(make_decision_fixture()[1, ], adjust = "holm",
    include_coefficients = FALSE))
  expect_match(single$interpretation, "Intervals are 95% confidence intervals.", fixed = TRUE)
})
