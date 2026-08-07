# the fixture below reproduces the failure geometry recorded in the arc
# simulation validation (test/grf-blp-policy-cv-validation): under a
# constant-effect null, replicate 6 gained +0.014 in held-out value while
# depth-one root stability was 0.43 and depth-two root stability was 0.92, so
# the relative stability guard passed on a negative loss and the gain guard did
# all the remaining work.

# purpose: build a two-depth value summary for one model. inputs: the held-out
# gains at depths one and two. output: a value_summary data frame with the
# columns the depth rule reads.
make_value_summary <- function(gain_d1, gain_d2, model = "model_y") {
  data.frame(
    model = model,
    outcome = sub("^model_", "", model),
    outcome_label = sub("^model_", "", model),
    depth = c(1L, 2L),
    gain_vs_control_mean = c(gain_d1, gain_d2),
    stringsAsFactors = FALSE
  )
}

# purpose: build a root-split summary carrying one selection frequency per
# depth. inputs: the depth-one and depth-two root-split frequencies; NA drops
# the corresponding row so the stability comparison becomes incomputable.
# output: a split_summary data frame.
make_split_summary <- function(root_d1, root_d2, model = "model_y") {
  rows <- data.frame(
    model = model,
    depth = c(1L, 2L),
    node_id = c(1L, 1L),
    variable = c("x1", "x1"),
    selection_frequency = c(root_d1, root_d2),
    stringsAsFactors = FALSE
  )
  rows[!is.na(rows$selection_frequency), , drop = FALSE]
}

test_that("the absolute floor blocks depth two when the depth-one root is unstable", {
  # gain guard passes (+0.0141 > 0.01) and the relative stability guard passes
  # trivially (loss = 0.43 - 0.92 = -0.49), yet the depth-one root agrees in
  # only 43% of fits, so no reliable first split was found.
  selection <- .policy_cv_select_depths(
    value_summary = make_value_summary(0.34, 0.3541),
    split_summary = make_split_summary(0.43, 0.92),
    min_gain_for_depth_switch = 0.01,
    max_stability_loss_for_depth_switch = 0.05,
    min_root_stability_for_depth_switch = 0.5
  )

  expect_equal(nrow(selection), 1L)
  expect_equal(selection$selected_depth, 1L)
  expect_true(selection$stability_ok)
  expect_false(selection$root_stability_floor_ok)
  expect_equal(selection$reason, "depth one root split is not stable enough to license depth two")
  expect_gt(selection$depth2_minus_depth1, 0.01)
})

test_that("the floor leaves a stable depth-one root alone", {
  # same gain and same relative-stability geometry, but the depth-one root now
  # clears the floor, so the pre-existing guards decide as before.
  selection <- .policy_cv_select_depths(
    value_summary = make_value_summary(0.34, 0.3541),
    split_summary = make_split_summary(0.72, 0.92),
    min_gain_for_depth_switch = 0.01,
    max_stability_loss_for_depth_switch = 0.05,
    min_root_stability_for_depth_switch = 0.5
  )

  expect_equal(selection$selected_depth, 2L)
  expect_true(selection$root_stability_floor_ok)
  expect_equal(selection$reason, "depth two clears held-out value and stability thresholds")
})

test_that("min_root_stability_for_depth_switch = 0 recovers the old behaviour", {
  # the same fixture that the default now blocks must select depth two once the
  # floor is switched off, which is the opt-out for studies registered under
  # the pre-1.1.015 rule.
  selection <- .policy_cv_select_depths(
    value_summary = make_value_summary(0.34, 0.3541),
    split_summary = make_split_summary(0.43, 0.92),
    min_gain_for_depth_switch = 0.01,
    max_stability_loss_for_depth_switch = 0.05,
    min_root_stability_for_depth_switch = 0
  )

  expect_equal(selection$selected_depth, 2L)
  expect_true(selection$root_stability_floor_ok)
  expect_equal(selection$reason, "depth two clears held-out value and stability thresholds")
  expect_equal(selection$min_root_stability_for_depth_switch, 0)
})

test_that("an incomputable stability loss fails closed with a warning", {
  # the depth-two root-split row is missing, so the loss is NA. Before 1.1.015
  # that passed the guard; it must now refuse depth two and say so.
  expect_warning(
    selection <- .policy_cv_select_depths(
      value_summary = make_value_summary(0.34, 0.3541),
      split_summary = make_split_summary(0.72, NA_real_),
      min_gain_for_depth_switch = 0.01,
      max_stability_loss_for_depth_switch = 0.05,
      min_root_stability_for_depth_switch = 0.5
    ),
    "stability could not be computed"
  )

  expect_equal(selection$selected_depth, 1L)
  expect_false(selection$stability_ok)
  expect_equal(selection$reason, "root-split stability could not be computed, so depth two was refused")
})

test_that("an incomputable stability loss still fails closed with the floor disabled", {
  # disabling the floor is an opt-out from the new absolute guard only; the
  # fail-closed handling of a missing stability comparison is not opt-outable.
  expect_warning(
    selection <- .policy_cv_select_depths(
      value_summary = make_value_summary(0.34, 0.3541),
      split_summary = make_split_summary(NA_real_, NA_real_),
      min_gain_for_depth_switch = 0.01,
      max_stability_loss_for_depth_switch = 0.05,
      min_root_stability_for_depth_switch = 0
    ),
    "stability could not be computed"
  )

  expect_equal(selection$selected_depth, 1L)
  expect_false(selection$stability_ok)
})

test_that("the floor value travels into the returned settings", {
  old_options <- options(margot.policy_tree.min_node_size = 5L)
  on.exit(options(old_options), add = TRUE)
  set.seed(4242)
  n <- 120
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 1, -0.4) + ifelse(x$x2 > 1, 0.15, 0)
  )
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        top_vars = c("x1", "x2")
      )
    ),
    covariates = x,
    weights = rep(1, n)
  )

  out <- margot_policy_tree_cv(
    object,
    model_names = "y",
    depths = c(1, 2),
    num_folds = 3,
    n_repeats = 2,
    min_root_stability_for_depth_switch = 0.6,
    tree_method = "policytree",
    seed = 99,
    verbose = FALSE
  )

  expect_equal(out$metadata$min_root_stability_for_depth_switch, 0.6)
  expect_true("root_stability_floor_ok" %in% names(out$depth_selection))
  expect_equal(out$depth_selection$min_root_stability_for_depth_switch, rep(0.6, nrow(out$depth_selection)))

  default_out <- margot_policy_tree_cv(
    object,
    model_names = "y",
    depths = c(1, 2),
    num_folds = 3,
    n_repeats = 2,
    tree_method = "policytree",
    seed = 99,
    verbose = FALSE
  )
  expect_equal(default_out$metadata$min_root_stability_for_depth_switch, 0.5)

  expect_error(
    margot_policy_tree_cv(
      object,
      model_names = "y",
      depths = 1,
      num_folds = 3,
      n_repeats = 1,
      min_root_stability_for_depth_switch = 1.5,
      tree_method = "policytree",
      verbose = FALSE
    ),
    "min_root_stability_for_depth_switch"
  )
})
