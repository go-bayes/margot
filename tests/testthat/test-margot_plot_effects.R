# tests for margot_lmtp_effects(), margot_plot_effects() and margot_save_effects_plot().
# the margot_lmtp() fixtures are genuine fits on simulated data with glm learners.

effects_fixture <- local({
  cache <- list()
  function(scale = c("additive", "rr")) {
    scale <- match.arg(scale)
    if (!is.null(cache[[scale]])) return(cache[[scale]])
    seed <- if (scale == "additive") 20261106L else 20261107L
    set.seed(seed)
    n <- if (scale == "additive") 600L else 800L
    l1 <- stats::rnorm(n)
    l2 <- stats::rbinom(n, 1, 0.4)
    a <- pmin(7, pmax(1, round(3 + 0.6 * l1 + 0.5 * l2 + stats::rnorm(n), 1)))
    dat <- data.frame(l1 = l1, l2 = l2, a = a)
    if (scale == "additive") {
      dat$y_warm <- 0.25 * a + 0.4 * l1 + stats::rnorm(n)
      dat$y_trust <- -0.15 * a + 0.3 * l2 + stats::rnorm(n)
      dat$y_flat <- 0.3 * l1 + stats::rnorm(n)
      outcomes <- c("y_warm", "y_trust", "y_flat")
    } else {
      dat$y_smoke <- stats::rbinom(n, 1, stats::plogis(-1 + 0.3 * a + 0.4 * l1))
      dat$y_volunteer <- stats::rbinom(n, 1, stats::plogis(-0.5 - 0.2 * a + 0.3 * l2))
      outcomes <- c("y_smoke", "y_volunteer")
    }
    lower <- function(data, trt) pmax(1, data[[trt]] - 1)
    fit <- suppressMessages(margot_lmtp(
      data = dat, outcome_vars = outcomes, trt = "a", shift_functions = list(lower = lower),
      lmtp_model_type = lmtp::lmtp_tmle, contrast_type = "null", contrast_scale = scale,
      lmtp_defaults = list(baseline = c("l1", "l2"),
                           outcome_type = if (scale == "additive") "continuous" else "binomial",
                           folds = 2, learners_trt = "SL.glm", learners_outcome = "SL.glm"),
      n_cores = 1, save_output = FALSE, progress = "none", seed = seed
    ))
    out <- list(combined_tables = fit$combined_tables, contrasts = fit$contrasts)
    cache[[scale]] <<- out
    out
  }
})

contrast_values <- function(fx) {
  tab <- fx$combined_tables$combined_outcomes_lower_vs_null
  do.call(rbind, lapply(rownames(tab), function(o) fx$contrasts[[o]][[paste0(o, "_lower_vs_null")]]$estimates))
}

synthetic_effects <- function(estimate, half_width, conf_level = 0.95, type = "difference", ...) {
  data.frame(outcome = paste0("y", seq_along(estimate)), estimate = estimate,
             lower = estimate - half_width, upper = estimate + half_width,
             conf_level = conf_level, type = type, ..., stringsAsFactors = FALSE)
}

test_that("margot_lmtp() output gives margot's 95% intervals and lmtp's regime means", {
  skip_on_cran()
  fx <- effects_fixture("additive")
  tab <- fx$combined_tables$combined_outcomes_lower_vs_null
  vals <- contrast_values(fx)
  plain <- margot_lmtp_effects(fx, outcomes = c("y_warm", "y_trust", "y_flat"))
  expect_equal(plain$lower, tab[["2.5 %"]], tolerance = 1e-10)
  expect_equal(plain$upper, vals$conf.high, tolerance = 1e-10)
  expect_equal(plain$mean_reference, vals$ref)
  expect_equal(plain$mean_intervention, vals$shift)
  expect_equal(unique(plain$conf_level), 0.95)
  # differences on the original scale carry no e-values.
  expect_null(plain$evalue)
})

test_that("bonferroni intervals use lmtp's standard errors and the stated family size", {
  skip_on_cran()
  fx <- effects_fixture("additive")
  vals <- contrast_values(fx)
  bon <- margot_lmtp_effects(fx, adjust = "bonferroni", family_size = 3)
  z <- stats::qnorm(1 - 0.05 / 6)
  expect_equal(bon$lower, vals$estimate - z * vals$std.error, tolerance = 1e-10)
  expect_equal(unique(bon$conf_level), 1 - 0.05 / 3)
  expect_identical(.margot_effects_ci_label(bon$conf_level), "98.33% CI")
  expect_error(margot_lmtp_effects(fx, adjust = "bonferroni"), "family_size` is required")
  # a bare table with symmetric wald intervals gives the same result.
  tab <- fx$combined_tables$combined_outcomes_lower_vs_null
  bare <- margot_lmtp_effects(tab, contrast = "lower_vs_null", adjust = "bonferroni", family_size = 3)
  expect_equal(bare$lower, bon$lower, tolerance = 1e-8)
})

test_that("alpha is honoured without adjustment", {
  skip_on_cran()
  fx <- effects_fixture("additive")
  vals <- contrast_values(fx)
  a10 <- margot_lmtp_effects(fx, alpha = 0.10)
  expect_equal(a10$upper, vals$estimate + stats::qnorm(0.95) * vals$std.error, tolerance = 1e-10)
  expect_identical(.margot_effects_ci_label(a10$conf_level), "90% CI")
})

test_that("inconsistent intervals and missing outcomes are refused", {
  skip_on_cran()
  fx <- effects_fixture("additive")
  tab <- fx$combined_tables$combined_outcomes_lower_vs_null
  skew <- tab
  skew[1L, "97.5 %"] <- skew[1L, "97.5 %"] + 0.1
  expect_error(margot_lmtp_effects(skew, contrast = "x", adjust = "bonferroni", family_size = 3),
               "not symmetric Wald")
  shifted <- fx
  shifted$combined_tables$combined_outcomes_lower_vs_null[1L, c("2.5 %", "97.5 %")] <-
    shifted$combined_tables$combined_outcomes_lower_vs_null[1L, c("2.5 %", "97.5 %")] + 0.05
  expect_error(margot_lmtp_effects(shifted, adjust = "bonferroni", family_size = 3), "not g\\(estimate\\)")
  expect_error(margot_lmtp_effects(fx, outcomes = c("y_warm", "y_trust", "y_flat", "y_missing")),
               "lacks outcomes: y_missing")
  expect_error(margot_lmtp_effects(fx, labels = c(y_warm = "Warmth")), "labels missing for: y_trust, y_flat")
})

test_that("risk ratios use log-scale standard errors and give e-values", {
  skip_on_cran()
  rx <- effects_fixture("rr")
  rtab <- rx$combined_tables$combined_outcomes_lower_vs_null
  rvals <- contrast_values(rx)
  plain <- margot_lmtp_effects(rx, outcomes = c("y_smoke", "y_volunteer"))
  expect_true(all(plain$type == "ratio"))
  expect_equal(plain$lower, rvals$conf.low, tolerance = 1e-10)
  bon <- margot_lmtp_effects(rx, adjust = "bonferroni", family_size = 2)
  z <- stats::qnorm(1 - 0.05 / 4)
  expect_equal(bon$upper, exp(log(rvals$estimate) + z * rvals$std.error), tolerance = 1e-10)
  ev <- .margot_effects_evalue(bon$estimate, bon$lower, bon$upper, "ratio")
  expect_equal(bon$evalue_bound, ev$evalue_bound)
  skew <- rx
  skew$combined_tables$combined_outcomes_lower_vs_null[1L, "97.5 %"] <- rtab[1L, "97.5 %"] * 1.05
  expect_error(margot_lmtp_effects(skew, adjust = "bonferroni", family_size = 2), "not g\\(estimate\\)")
  expect_error(margot_lmtp_effects(rx, scale_info = data.frame(outcome = c("y_smoke", "y_volunteer"),
                                                               scale = c(2, 3))),
               "differences only")
})

test_that("scale_info converts to original units with margot's preparation constants", {
  skip_on_cran()
  fx <- effects_fixture("additive")
  vals <- contrast_values(fx)
  info <- data.frame(outcome = c("y_warm", "y_trust", "y_flat"), center = c(4, 5, 3), scale = c(1.2, 1.1, 1.3))
  scaled <- margot_lmtp_effects(fx, standardised = TRUE, scale_info = info)
  expect_equal(scaled$mean_reference, c(4, 5, 3) + c(1.2, 1.1, 1.3) * vals$ref)
  expect_equal(scaled$raw_estimate, scaled$estimate * c(1.2, 1.1, 1.3))
  expect_false(anyNA(scaled$evalue))
  # without a centre the means of standardised outcomes are dropped, not printed in SD units.
  expect_true(all(is.na(margot_lmtp_effects(fx, standardised = TRUE)$mean_reference)))
  no_center <- margot_lmtp_effects(fx, standardised = TRUE, scale_info = info[c("outcome", "scale")])
  expect_true(all(is.na(no_center$mean_reference)))
  # reversed orientation keeps the original-unit interval ordered.
  flipped <- margot_lmtp_effects(fx, standardised = TRUE, scale_info = transform(info, orientation = -1))
  expect_equal(flipped$raw_estimate, -scaled$raw_estimate)
  expect_true(all(flipped$raw_lower <= flipped$raw_estimate & flipped$raw_estimate <= flipped$raw_upper))
  expect_error(margot_lmtp_effects(fx, scale_info = transform(info, transformation = "log1p")),
               "identity transformations")
})

test_that("supplied standard errors rebuild the registered intervals", {
  est <- c(0.08, 0.02, -0.01)
  native_se <- c(0.020, 0.015, 0.018)
  registered_se <- c(0.021, 0.0152, 0.0179)
  tab <- data.frame(`E[Y(1)]-E[Y(0)]` = est, `2.5 %` = est - 1.96 * native_se,
                    `97.5 %` = est + 1.96 * native_se, check.names = FALSE, row.names = c("a", "b", "c"))
  tab[["2.5 %"]] <- est - stats::qnorm(0.975) * native_se
  tab[["97.5 %"]] <- est + stats::qnorm(0.975) * native_se
  supplied <- margot_lmtp_effects(tab, contrast = "k", adjust = "bonferroni", family_size = 12,
    se_source = "supplied", standardised = TRUE,
    standard_errors = data.frame(outcome = c("a", "b", "c"), contrast = "k", standard_error = registered_se))
  z <- stats::qnorm(1 - 0.05 / 24)
  expect_equal(supplied$lower, est - z * registered_se, tolerance = 1e-12)
  expect_equal(supplied$upper, est + z * registered_se, tolerance = 1e-12)
  expect_identical(.margot_effects_ci_label(supplied$conf_level), "99.58% CI")
  expect_true(all(supplied$se_source == "supplied"))
  # interval e-values follow the exact conversion RR = exp(0.91 d) at the limit nearest 0.
  rr <- exp(0.91 * supplied$lower[1L])
  expect_equal(supplied$evalue_bound[1L], rr + sqrt(rr * (rr - 1)))
  expect_equal(supplied$evalue_bound[2:3], c(1, 1))
  expect_error(margot_lmtp_effects(tab, contrast = "k", se_source = "supplied",
    standard_errors = data.frame(outcome = "a", contrast = "k", standard_error = 0.02)),
    "lacks outcomes for k: b, c")
})

test_that("decimal points align exactly, kerning pairs included", {
  family <- .margot_effects_font()
  g1 <- .margot_effects_glyphs(c("0.011]", "0.062]", "\u22120.025]"), 10, 1:3, 6.5, family)
  dots <- g1$x[g1$label == "."]
  expect_length(dots, 3L)
  expect_lt(max(dots) - min(dots), 1e-9)
})

test_that("decimals keep excluding intervals off the null, with no negative zero", {
  expect_identical(.margot_effects_digits(c(0.0006, -0.05), c(0.09, 0.02), 0), 3L)
  expect_identical(.margot_effects_digits(c(0.02, -0.05), c(0.09, 0.02), 0), 2L)
  expect_identical(.margot_effects_num(-0.0001, 3), "0.000")
  expect_identical(.margot_effects_num(-0.25, 2), "\u22120.25")
  expect_identical(.margot_effects_ci_label(1 - 0.05 / 40), "99.875% CI")
  expect_identical(.margot_effects_ci_label(0.95), "95% CI")
})

test_that("axis breaks bracket every interval and the null on both scales", {
  cases <- list(
    synthetic_effects(c(0.12, -0.03, 0.05), 0.05),
    synthetic_effects(c(1.4, 0.8), c(0.2, 0.1), type = "ratio"),
    synthetic_effects(c(0.12, 0.03), 0.04, raw_estimate = c(0.15, 0.04), raw_lower = c(0.1, -0.01),
                      raw_upper = c(0.2, 0.09))
  )
  for (i in seq_along(cases)) {
    fig <- margot_plot_effects(cases[[i]], xlab = "x", plot = if (i == 3L) "raw" else "estimate", width_mm = 89)
    ax <- attr(fig, "axis")
    expect_lte(min(ax$breaks), ax$data_range[1L])
    expect_gte(max(ax$breaks), ax$data_range[2L])
    expect_lt(ax$limits[1L], min(ax$breaks))
    expect_gt(ax$limits[2L], max(ax$breaks))
  }
})

test_that("tick labels stay at least 2.5 mm apart across widths and scales", {
  tables <- list(
    synthetic_effects(c(0.09, 0.05, 0.03, 0.01, -0.01), c(0.04, 0.05, 0.03, 0.03, 0.04), conf_level = 1 - 0.05 / 12),
    synthetic_effects(c(1.4, 0.8), c(0.2, 0.1), type = "ratio"),
    synthetic_effects(c(0.0004, 0.0001), c(0.0001, 0.0002))
  )
  tables[[1L]] <- transform(tables[[1L]], raw_estimate = estimate * 1.2, raw_lower = lower * 1.2,
                            raw_upper = upper * 1.2, mean_reference = 4, mean_intervention = 4 + estimate,
                            evalue = 1.2, evalue_bound = 1)
  gaps <- numeric()
  for (w in c(89, 120, 165, 183)) for (k in seq_along(tables)) {
    cols <- if (w >= 165 && k == 1L) c("plotted", "raw", "means", "evalue") else character()
    ax <- attr(margot_plot_effects(tables[[k]], xlab = "x", columns = cols, width_mm = w), "axis")
    gaps <- c(gaps, ax$label_gap_mm)
  }
  expect_gte(min(gaps), 2.5)
  tiny <- attr(margot_plot_effects(tables[[3L]], xlab = "x", width_mm = 89), "axis")$breaks
  expect_false(anyDuplicated(.margot_effects_axis_labels(tiny, "difference")) > 0)
})

test_that("ratio ranges render with positive, round breaks that include 1", {
  family <- .margot_effects_font()
  for (r in list(c(0.85, 1.2), c(0.91, 1.12), c(0.95, 1.05), c(0.80, 1.19), c(0.98, 1.02), c(0.56, 2.2))) {
    for (w in c(30, 60)) {
      b <- .margot_effects_breaks(r, "ratio", panel_mm = w, family = family)
      expect_true(length(b) >= 2L && all(b > 0) && 1 %in% b && min(b) <= r[1L] && max(b) >= r[2L])
    }
    d <- data.frame(outcome = "a", estimate = sqrt(prod(r)), lower = r[1L], upper = r[2L], conf_level = 0.95,
                    type = "ratio")
    expect_s3_class(margot_plot_effects(d, xlab = "x", width_mm = 89), "patchwork")
  }
  for (r in list(c(0.2, 0.95), c(0.4, 0.97), c(0.1, 0.5))) {
    b <- .margot_effects_breaks(r, "ratio", panel_mm = 60, family = family)
    expect_true(all(abs(signif(b, 2) - b) < 1e-12 * b) && min(b) <= r[1L] && max(b) >= r[2L])
  }
  near <- data.frame(outcome = "a", estimate = 1.0003, lower = 0.999, upper = 1.001, conf_level = 0.95, type = "ratio")
  nax <- attr(margot_plot_effects(near, xlab = "x", width_mm = 89), "axis")
  expect_lte(diff(range(nax$breaks)), 0.01)
  expect_true(1 %in% nax$breaks)
  wide <- data.frame(outcome = c("a", "b"), estimate = c(10, 0.9), lower = c(1.0001, 0.5), upper = c(1000, 1.6),
                     conf_level = 0.95, type = "ratio")
  wax <- attr(margot_plot_effects(wide, xlab = "x", width_mm = 89), "axis")
  expect_true(wax$limits[1L] <= 0.5 && wax$limits[2L] >= 1000 && wax$label_gap_mm >= 2.5)
})

test_that("crowded tick labels warn", {
  expect_warning(.margot_effects_breaks(c(-0.0001, 0.0005), panel_mm = 8, family = .margot_effects_font()),
                 "widen the figure")
})

test_that("layout refuses inputs it cannot draw faithfully", {
  d <- synthetic_effects(c(0.12, -0.03, 0.05), 0.05)
  two <- rbind(transform(d, contrast = "a"), transform(d[-1L, ], contrast = "b"))
  expect_error(margot_plot_effects(two, xlab = "x"), "share one outcome set")
  expect_error(margot_plot_effects(d, xlab = "x", columns = "means"), "needs mean_reference, mean_intervention")
  expect_error(margot_plot_effects(d, xlab = "x", columns = "evalue"), "needs evalue, evalue_bound")
  expect_error(margot_plot_effects(d, xlab = "x", plot = "raw"), "plot = \"raw\" needs")
  expect_error(margot_plot_effects(d, xlab = "x", columns = "other"), "unknown column")
  bad <- d
  bad$estimate[1L] <- 1
  expect_error(margot_plot_effects(bad, xlab = "x"), "within its interval")
  expect_error(margot_plot_effects(transform(d, type = "ratio"), xlab = "x"), "positive interval limits")
})

test_that("two contrasts share one axis and keep labels apart across the gutter", {
  d <- synthetic_effects(c(0.12, -0.03, 0.05), 0.05)
  fig <- margot_plot_effects(rbind(transform(d, contrast = "a"), transform(d, contrast = "b")), xlab = "x",
                             columns = "estimate", contrast_titles = c(a = "Gain", b = "Loss"), width_mm = 183)
  expect_gte(attr(fig, "axis")$label_gap_mm, 2.5)
  expect_length(attr(fig, "layout_mm"), 4L)
})

test_that("rows follow group titles, then estimates within group", {
  d <- synthetic_effects(c(0.01, 0.09, 0.05, -0.02), 0.03, label = c("w", "x", "y", "z"),
                         group = c("b", "a", "b", "a"))
  fig <- margot_plot_effects(d, xlab = "x", group_titles = c(b = "Group B", a = "Group A"), width_mm = 89)
  expect_s3_class(fig, "patchwork")
  expect_named(attr(fig, "font"), c("family", "file", "md5"))
  size <- attr(fig, "size_mm")
  expect_equal(size[["width"]], 89)
  expect_gt(size[["height"]], 10)
})

test_that("figures save at their print size", {
  skip_on_cran()
  skip_if_not(capabilities("cairo"))
  skip_if_not_installed("ragg")
  d <- synthetic_effects(c(0.12, -0.03, 0.05), 0.05)
  fig <- margot_plot_effects(d, xlab = "Difference, SD units", columns = "estimate", width_mm = 120)
  stub <- file.path(withr::local_tempdir(), "effects")
  paths <- margot_save_effects_plot(fig, stub)
  expect_true(all(file.exists(paths)))
  # png width from the ihdr chunk (bytes 17-20, big-endian).
  con <- file(paths[2L], "rb")
  header <- readBin(con, "raw", 24L)
  close(con)
  width_px <- sum(as.integer(header[17:20]) * 256^(3:0))
  expect_lte(abs(width_px - 120 / 25.4 * 600), 1)
  expect_error(margot_save_effects_plot(ggplot2::ggplot(), stub), "must come from margot_plot_effects")
})

test_that("group titles are optional and may be partial", {
  d <- synthetic_effects(c(0.01, 0.09, 0.05, -0.02), 0.03, group = c("b", "a", "b", "a"))
  expect_s3_class(margot_plot_effects(d, xlab = "x", width_mm = 89), "patchwork")
  expect_s3_class(margot_plot_effects(d, xlab = "x", group_titles = c(b = "Group B"), width_mm = 89), "patchwork")
})

test_that("original-unit columns are validated before layout", {
  d <- synthetic_effects(c(0.12, 0.03), 0.04)
  expect_error(margot_plot_effects(transform(d, raw_estimate = c(10, 20), raw_lower = c(-1, -2), raw_upper = c(1, 2)),
                                   xlab = "x", plot = "raw"), "lie within their intervals")
  expect_error(margot_plot_effects(transform(d, raw_estimate = c(0.1, 0.2)), xlab = "x"),
               "need all of raw_estimate, raw_lower and raw_upper")
})

test_that("supplied standard errors must be unique per outcome and contrast", {
  tab <- data.frame(`E[Y(1)]-E[Y(0)]` = c(0.1, 0.2), `2.5 %` = c(0.1, 0.2) - stats::qnorm(0.975) * 0.02,
                    `97.5 %` = c(0.1, 0.2) + stats::qnorm(0.975) * 0.02, check.names = FALSE, row.names = c("a", "b"))
  se <- data.frame(outcome = c("a", "a", "b"), contrast = "k", standard_error = c(0.02, 0.03, 0.02))
  expect_error(margot_lmtp_effects(tab, contrast = "k", se_source = "supplied", standard_errors = se),
               "repeats outcome and contrast: a / k")
})

test_that("the bonferroni family cannot be smaller than the outcomes reported", {
  tab <- data.frame(`E[Y(1)]-E[Y(0)]` = c(0.1, 0.2), `2.5 %` = c(0.1, 0.2) - stats::qnorm(0.975) * 0.02,
                    `97.5 %` = c(0.1, 0.2) + stats::qnorm(0.975) * 0.02, check.names = FALSE, row.names = c("a", "b"))
  expect_error(margot_lmtp_effects(tab, contrast = "k", adjust = "bonferroni", family_size = 1),
               "smaller than the 2 outcomes")
  larger <- margot_lmtp_effects(tab, contrast = "k", adjust = "bonferroni", family_size = 12)
  expect_equal(unique(larger$conf_level), 1 - 0.05 / 12)
  expect_equal(unique(larger$family_size), 12L)
})

test_that("odds-ratio contrasts are refused", {
  skip_on_cran()
  rx <- effects_fixture("rr")
  odds <- rx
  for (o in names(odds$contrasts)) {
    for (k in names(odds$contrasts[[o]])) odds$contrasts[[o]][[k]]$type <- "or"
  }
  expect_error(margot_lmtp_effects(odds), "is an odds ratio")
})

test_that("point fill follows the interval that is drawn", {
  d <- synthetic_effects(0.2, 0.1, raw_estimate = 0.01, raw_lower = -0.05, raw_upper = 0.07)
  raw_fig <- margot_plot_effects(d, xlab = "x", plot = "raw", width_mm = 89)
  expect_false(attr(raw_fig, "plotted")$excludes_null)
  expect_equal(attr(raw_fig, "plotted")$lower, -0.05)
  sd_fig <- margot_plot_effects(d, xlab = "x", width_mm = 89)
  expect_true(attr(sd_fig, "plotted")$excludes_null)
})

test_that("documented examples parse and run", {
  for (rd in c("margot_lmtp_effects", "margot_plot_effects")) {
    path <- testthat::test_path("..", "..", "man", paste0(rd, ".Rd"))
    skip_if_not(file.exists(path), "source man pages not available")
    ex <- tempfile(fileext = ".R")
    tools::Rd2ex(path, ex)
    expect_no_error(source(ex, local = new.env()))
  }
  # rendered help keeps every paragraph (an escaped percent sign once cut one short).
  text <- paste(utils::capture.output(tools::Rd2txt(testthat::test_path("..", "..", "man", "margot_lmtp_effects.Rd"))),
                collapse = " ")
  expect_match(text, "odds ratios")
  expect_match(text, "\\(2.5 %\\)|2.5 %")
})
