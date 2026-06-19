test_that("bootstrap stability uses the full bootstrap sample", {
  idx <- get_stability_indices(
    not_missing = 1:20,
    vary_type = "bootstrap",
    train_prop = 0.5,
    sample_seed = 10,
    split_seed = 20
  )

  expect_length(idx$train_idx, 20)
  expect_length(idx$test_idx, 0)
})

test_that("depth selector requires material gain and small stability loss", {
  object <- list(
    results = list(
      model_a = list(stability_metrics = list(consensus_strength = list(depth_1 = 0.80, depth_2 = 0.80))),
      model_b = list(stability_metrics = list(consensus_strength = list(depth_1 = 0.80, depth_2 = 0.73))),
      model_c = list(stability_metrics = list(consensus_strength = list(depth_1 = 0.80, depth_2 = 0.76)))
    )
  )

  testthat::local_mocked_bindings(
    margot_policy_summary_report = function(...) {
      args <- list(...)
      if (!is.null(args$depths_by_model)) {
        stop("skip combined summary in selector test")
      }
      depth <- args$depth
      estimates <- if (depth == 1L) {
        c(model_a = 0.100, model_b = 0.100, model_c = 0.100)
      } else {
        c(model_a = 0.109, model_b = 0.120, model_c = 0.120)
      }
      coherent <- data.frame(
        model = names(estimates),
        contrast = "policy - control_all",
        depth = depth,
        estimate = unname(estimates),
        ci_lo = unname(estimates) - 0.01,
        ci_hi = unname(estimates) + 0.01
      )
      recs <- stats::setNames(lapply(estimates, function(x) {
        list(
          decision = "caution",
          selected_label = NA_character_,
          full = list(pv = x, lo = x - 0.01, hi = x + 0.01)
        )
      }), names(estimates))
      list(
        coherent_policy_values = coherent,
        recommendations_by_model = recs,
        split_table_compact = list()
      )
    }
  )

  out <- margot_policy_summary_compare_depths(
    object,
    verbose = FALSE,
    min_gain_for_depth_switch = 0.01,
    max_stability_loss_for_depth_switch = 0.05
  )

  expect_equal(unname(out$depth_map["model_a"]), 1L)
  expect_equal(unname(out$depth_map["model_b"]), 1L)
  expect_equal(unname(out$depth_map["model_c"]), 2L)
  expect_equal(out$depth_summary_df$stability_loss_depth2[out$depth_summary_df$model == "model_b"], 0.07)
})

test_that("margot_qini refuses descriptive in-sample regeneration by default", {
  models <- list(
    results = list(model_y = list()),
    computation_params = list(use_train_test_split = FALSE)
  )

  expect_error(
    margot_qini(models, verbose = FALSE),
    "in-sample QINI"
  )
})

test_that("margot_policy_split_diagnostic evaluates repeated held-out splits", {
  set.seed(42)
  n <- 60
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  gamma <- cbind(
    control = rep(0, n),
    treated = ifelse(x$x1 > 0, 1, -0.5)
  )
  object <- list(
    results = list(
      model_y = list(
        dr_scores = gamma,
        top_vars = c("x1", "x2")
      )
    ),
    covariates = x,
    not_missing = seq_len(n)
  )

  out <- margot_policy_split_diagnostic(
    object,
    depths = 1,
    n_splits = 3,
    train_proportion = 0.5,
    tree_method = "policytree",
    verbose = FALSE
  )

  expect_s3_class(out, "margot_policy_split_diagnostic")
  expect_equal(nrow(out), 3L)
  expect_true(all(out$n_train == 30L))
  expect_true(all(out$n_eval == 30L))
  expect_true(is.data.frame(attr(out, "summary")))
})
