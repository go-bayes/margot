test_that("binary action scores agree with hand-calculated residual corrections", {
  scores <- margot_policy_action_scores(c(2, 3), c(0, 1), c(1, 2),
    c(.25, .75), c(.4, -.8))
  expect_equal(scores[, "control"], c(.9 + 1.1 / .75, 2.6))
  expect_equal(scores[, "treated"], c(1.3, 1.8 + 1.2 / .75))
  shifted <- margot_policy_action_scores(c(2, 3) + 10, c(0, 1),
    c(1, 2) + 10, c(.25, .75), c(.4, -.8))
  expect_equal(shifted, scores + 10)
})

test_that("binary action scores reproduce policytree scores for a simulated forest", {
  skip_if_not_installed("grf")
  skip_if_not_installed("policytree")
  set.seed(491)
  x <- matrix(rnorm(600), 200, 3)
  w <- rep(0:1, 100)
  y <- x[, 1] + w * (.5 + x[, 2]) + rnorm(200)
  forest <- grf::causal_forest(x, y, w, W.hat = rep(.5, 200),
    num.trees = 100, num.threads = 1, seed = 493)
  ours <- margot_policy_action_scores(y, w, as.numeric(forest$Y.hat),
    as.numeric(forest$W.hat), as.numeric(predict(forest)$predictions))
  expect_equal(unname(ours), unname(policytree::double_robust_scores(forest)), tolerance = 1e-10)
})

test_that("action score boundaries fail without silent recycling or clipping", {
  expect_error(margot_policy_action_scores(1:2, c(0, 1), 1, c(.5, .5), c(0, 0)), "outcome_mean")
  expect_error(margot_policy_action_scores(1:2, c(0, 2), 1:2, c(.5, .5), c(0, 0)), "zero and one")
  expect_error(margot_policy_action_scores(1:2, c(0, 1), 1:2, c(0, .5), c(0, 0)), "strictly")
  expect_error(margot_policy_action_scores(1:2, c(0, 1), 1:2, c(.5, .5), c(NA, 0)), "treatment_effect")
})
