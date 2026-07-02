# tests for the registered trim-ladder positivity gate

make_fit <- function(dr) list(density_ratios = dr)

test_that("gate passes tame ratios at the first rung", {
  set.seed(2026)
  tame <- matrix(stats::runif(1500, 0.8, 1.25), ncol = 3)
  gate <- margot_lmtp_positivity_gate(make_fit(tame), verbose = FALSE)

  expect_named(gate, c("by_rung", "selection", "criteria"))
  expect_equal(nrow(gate$by_rung), 3L)
  expect_true(all(c("min_ess_frac", "trim_mass_share", "support_status",
                    "ess_ok", "mass_ok", "band_ok", "pass")
                  %in% names(gate$by_rung)))
  expect_equal(gate$selection$selected_rung, 0.99)
  expect_match(gate$selection$verdict, "\\.trim = 0\\.99")
  expect_true(all(gate$by_rung$pass))
  expect_identical(unique(gate$by_rung$support_status), "Adequate")
})

test_that("gate fails pathological ratios and applies the contingency verdict", {
  set.seed(2026)
  n <- 600
  heavy <- matrix(stats::rlnorm(n * 3, meanlog = 0, sdlog = 2.5), ncol = 3)
  gate <- margot_lmtp_positivity_gate(make_fit(heavy), verbose = FALSE)

  expect_true(is.na(gate$selection$selected_rung))
  expect_match(gate$selection$verdict, "contingency")
  expect_false(any(gate$by_rung$pass))
})

test_that("criteria move in opposite directions as rungs descend", {
  set.seed(42)
  mixed <- matrix(stats::rlnorm(900, meanlog = 0, sdlog = 1.2), ncol = 3)
  gate <- margot_lmtp_positivity_gate(make_fit(mixed), verbose = FALSE)

  by_rung <- gate$by_rung[order(-gate$by_rung$rung), ]
  # descending the ladder homogenises weights: ESS fraction never decreases
  expect_true(all(diff(by_rung$min_ess_frac) >= -1e-12))
  # descending the ladder caps harder: trimmed mass share never decreases
  expect_true(all(diff(by_rung$trim_mass_share) >= -1e-12))
})

test_that("weight diagnostic reports trim_mass_share within [0, 1]", {
  set.seed(7)
  dr <- matrix(stats::rlnorm(600, meanlog = 0, sdlog = 1), ncol = 2)
  diag <- margot_lmtp_weight_diag_from_fit(make_fit(dr), trim_right = 0.98)

  expect_true("trim_mass_share" %in% names(diag$wave_table))
  share <- diag$wave_table$trim_mass_share
  expect_true(all(share >= 0 & share <= 1, na.rm = TRUE))
  expect_true(any(share > 0))
})

test_that("gate handles censoring zeros without error", {
  set.seed(11)
  dr <- matrix(stats::runif(900, 0.7, 1.4), ncol = 3)
  # once censored, future lost: zero out a block of later-wave ratios
  dr[1:60, 2:3] <- 0
  gate <- margot_lmtp_positivity_gate(make_fit(dr), verbose = FALSE)

  expect_equal(nrow(gate$selection), 1L)
  expect_true(is.finite(gate$by_rung$min_ess_frac[1]))
})
