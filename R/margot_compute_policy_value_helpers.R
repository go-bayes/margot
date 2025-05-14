#' Adjust policy-value p-values for multiplicity
#'
#' @param tbl Data frame from [margot_collect_policy_values()].
#' @param method Character; correction method as in [stats::p.adjust()]. Default: "bonferroni".
#' @param alpha Numeric; significance threshold after adjustment. Default: 0.05.
#'
#' @return A tibble: the input `tbl` with two new columns:
#' \describe{
#'   \item{p_adj}{Adjusted p-value.}
#'   \item{pass}{Logical; `TRUE` if `p_adj < alpha`.}
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom stats p.adjust
#' @keywords internal
margot_adjust_policy_p <- function(tbl,
                                   method = "bonferroni",
                                   alpha  = 0.05) {
  dplyr::mutate(tbl,
                p_adj = stats::p.adjust(p_value, method = method),
                pass  = p_adj < alpha)
}



#' Add or refresh a policy-value test for one model
#'
#' Thin wrapper around `margot_add_policy_values()` that evaluates a single
#' tree depth (default: 2). It preserves the original function’s seed and
#' bootstrap settings but uses friendlier defaults for interactive workflows.
#'
#' @param model List; one outcome-specific element of `cf_out$results`.
#' @param depth Integer; policy-tree depth to evaluate. Default: 2.
#' @param R Integer; number of bootstrap replicates. Default: 999.
#' @param seed Integer; RNG seed. Default: 2025.
#'
#' @return Invisibly returns the modified `model` list.
#' @keywords internal
margot_add_policy_p <- function(model,
                                depth = 2L,
                                R     = 999L,
                                seed  = 2025L) {
  margot_add_policy_values(model,
                           depths = depth,
                           R      = R,
                           seed   = seed)
}


#' Bootstrap test-set policy value
#'
#' Non-parametric bootstrap of the doubly-robust gain for a stored `policy_tree`.
#' Computes the difference between the mean gain under the policy and the
#' average treatment effect in the evaluation fold.
#'
#' @param model List; one outcome entry from a `margot` result.
#' @param depth Integer; depth of the stored `policy_tree` (1 or 2). Default: 2.
#' @param R Integer ≥ 199; number of bootstrap replicates. Default: 499.
#' @param seed Integer or `NULL`; RNG seed for reproducibility.
#'
#' @return Object of class `policy_value_test` with components:
#' \describe{
#'   \item{estimate}{Numeric; bootstrap estimate of policy gain minus ATE.}
#'   \item{std.err}{Numeric; standard error of bootstrap replicates.}
#'   \item{p.value}{Numeric; two-sided p-value.}
#'   \item{n_eval}{Integer; number of evaluation observations.}
#' }
#'
#' @importFrom stats complete.cases pnorm sd
#' @keywords internal
margot_compute_policy_value <- function(model,
                                        depth = 2L,
                                        R     = 499L,
                                        seed  = NULL) {
  if (!is.null(seed)) set.seed(seed)
  tag <- paste0("policy_tree_depth_", depth)
  pol <- model[[tag]]
  if (is.null(pol)) stop("no ", tag, " slot present – run `margot_policy()` first")

  dr   <- model$dr_scores
  full <- model$plot_data$X_test_full
  X    <- full[, pol$columns, drop = FALSE]
  keep <- complete.cases(X)
  X    <- X[keep, , drop = FALSE]
  dr   <- dr[keep, , drop = FALSE]
  n    <- nrow(X)
  stopifnot(n > 0L)

  pick <- function(a, mat) {
    if (min(a) == 0L) a <- a + 1L
    mat[cbind(seq_along(a), a)]
  }

  a_hat   <- policytree::predict(pol, X)
  ate_hat <- mean(dr[, 2] - dr[, 1])
  pv_hat  <- mean(pick(a_hat, dr)) - ate_hat

  reps <- replicate(R, {
    idx  <- sample.int(n, n, TRUE)
    a_bs <- policytree::predict(pol, X[idx, , drop = FALSE])
    mean(pick(a_bs, dr[idx, , drop = FALSE])) - ate_hat
  })

  se <- sd(reps)
  p  <- 2 * pnorm(-abs(pv_hat / se))

  structure(
    list(
      estimate = pv_hat,
      std.err  = se,
      p.value  = p,
      n_eval   = n
    ),
    class = "policy_value_test"
  )
}


#' Add bootstrap policy-value tests for multiple depths
#'
#' @param model  list. One element of `margot$results`.
#' @param depths integer vector. Depths to evaluate (default `c(1, 2)`).
#' @param R,seed Passed to [margot_compute_policy_value()].
#'
#' @return The modified `model` (invisibly).
#'
#' @keywords internal
margot_add_policy_values <- function(model,
                                     depths = c(1L, 2L),
                                     R      = 499L,
                                     seed   = 42L) {
  for (d in depths) {
    model[[paste0("policy_value_depth_", d)]] <-
      margot_compute_policy_value(model, depth = d, R = R, seed = seed)
  }
  invisible(model)
}


# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#  3.  Collect and adjust p-values across outcomes + depths
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#' Collect policy-value summaries
#'
#' @param cf_out list. **Normalised** margot result with `policy_value_depth_*`
#'   slots present.
#' @param depths integer vector. Depths to include.
#'
#' @return A `tibble` with one row per outcome × depth.
#'
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @keywords internal
margot_collect_policy_values <- function(cf_out, depths = c(1L, 2L)) {
  purrr::map_dfr(cf_out$outcome_vars, function(out) {
    m <- cf_out$results[[paste0("model_", out)]]
    purrr::map_dfr(depths, function(d) {
      tag <- paste0("policy_value_depth_", d)
      pv  <- m[[tag]]
      if (is.null(pv)) return(NULL)
      tibble::tibble(outcome = out,
                     depth   = d,
                     estimate = pv$estimate,
                     std_err  = pv$std.err,
                     p_value  = pv$p.value)
    })
  })
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#  4. vectorised wrapper for a set of outcomes --------------------------------
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

#' attach policy-value tests to a subset of outcomes
#'
#' this is the pared-back replacement for the earlier, more complex batch
#' helper. it simply iterates over the outcomes you pass in `keep`, calls
#' `margot_add_policy_p()` on each model, and invisibly returns the updated
#' `cf_out` so you can keep piping.
#'
#' @param cf_out list. full **margot** result.
#' @param keep   character. outcome names to update (without the `model_` prefix).
#' @param depth  integer. policy-tree depth. default 2.
#' @param R,seed bootstrap settings for the underlying test (defaults 999 / 2025).
#'
#' @return the modified `cf_out` (invisibly).
#' @export
#' @importFrom purrr map
margot_add_policy_batch <- function(cf_out,
                                    keep,
                                    depth = 2L,
                                    R     = 999L,
                                    seed  = 2025L) {

  idx <- paste0("model_", keep)

  cf_out$results[idx] <- purrr::map(cf_out$results[idx],
                                    margot_add_policy_p,
                                    depth = depth,
                                    R     = R,
                                    seed  = seed)

  invisible(cf_out)
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#  4. quick reporting helper --------------------------------------------------
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

#' summarise policy tests (add if absent, adjust, return tidy table)
#'
#' this convenience wrapper makes a complete, end-to-end policy report in one
#' line. it:
#' 1. optionally runs the bootstrap test for the selected outcomes (via
#'    `margot_add_policy_batch()`),
#' 2. pulls the depth‑specific results with `margot_policy_summary()`, and
#' 3. returns the tidy summary.
#'
#' nothing is printed; you decide what to do with the tibble (view, knit, etc.).
#'
#' @param cf_out  **margot** result list.
#' @param keep    character. which outcomes to include; `NULL` keeps all.
#' @param depth   integer. policy-tree depth. default 2.
#' @param adjust  character. multiplicity adjustment method for *p*-values.
#'                default "bonferroni".
#' @param alpha   numeric. significance level for the pass/fail flag. default 0.05.
#' @param R,seed  bootstrap settings if tests need to be (re)run.
#'
#' @return tibble. one row per outcome with adjusted *p*-values and pass flag.
#' @export
margot_report_policy <- function(cf_out,
                                 keep   = NULL,
                                 depth  = 2L,
                                 adjust = "bonferroni",
                                 alpha  = 0.05,
                                 R      = 999L,
                                 seed   = 2025L) {
  if (!is.null(keep)) {
    cf_out <- margot_add_policy_batch(cf_out,
                                      keep  = keep,
                                      depth = depth,
                                      R     = R,
                                      seed  = seed)
  }

  margot_policy_summary(cf_out,
                        depths = depth,
                        adjust = adjust,
                        alpha  = alpha)
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#  4. full outcome summary (ATE, RATE, policy p) -----------------------------
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

#' summarise ate, rate and policy tests across all outcomes
#'
#' pared‑back copy of the original (pre‑refactor) helper that pulls key
#' diagnostics – average treatment effect, two rate metrics (AUTOC & QINI) and
#' the depth‑specific policy *p*‑value – for every outcome in the object.
#' returns a tibble ready for downstream plotting or reporting.
#'
#' @param cf_out margot result list.
#' @param target character. keep both rate metrics (`"both"`, default) or drop
#'        one: `"AUTOC"` or `"QINI"`.
#' @param adjust character. multiplicity adjustment method (see
#'        [stats::p.adjust()]). default "bonferroni".
#' @param alpha  numeric. confidence level for rate intervals (default 0.05).
#' @return tibble. one row per outcome.
#' @export
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select starts_with
margot_summarise_all <- function(cf_out,
                                 target = c("AUTOC", "QINI", "both"),
                                 adjust = c("bonferroni", "holm", "BH", "none"),
                                 alpha  = 0.05) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  target <- match.arg(target)
  adjust <- match.arg(adjust)
  z      <- stats::qnorm(1 - alpha / 2)

  res <- purrr::map_dfr(cf_out$outcome_vars, function(outcome) {
    m <- cf_out$results[[paste0("model_", outcome)]]

    ate_est <- unname(m$ate["estimate"])
    ate_se  <- unname(m$ate["std.err"])

    rr_a <- m$rate_result
    rr_q <- m$rate_qini

    a_est <- rr_a$estimate; a_se <- rr_a$std.err
    a_lo  <- a_est - z * a_se; a_hi <- a_est + z * a_se

    q_est <- rr_q$estimate; q_se <- rr_q$std.err
    q_lo  <- q_est - z * q_se; q_hi <- q_est + z * q_se

    pv_tag  <- m$policy_value_depth_2 %||% m$policy_value
    policy_p <- if (!is.null(pv_tag)) pv_tag$p.value else NA_real_

    tibble::tibble(outcome,
                   ate_est, ate_se,
                   rate_autoc     = a_est,
                   rate_autoc_se  = a_se,
                   rate_autoc_lo  = a_lo,
                   rate_autoc_hi  = a_hi,
                   rate_qini      = q_est,
                   rate_qini_se   = q_se,
                   rate_qini_lo   = q_lo,
                   rate_qini_hi   = q_hi,
                   policy_p)
  })

  res <- dplyr::mutate(res,
                       p_adj = stats::p.adjust(policy_p, method = adjust),
                       pass  = p_adj < alpha)

  if (target == "AUTOC") {
    res <- dplyr::select(res, -dplyr::starts_with("rate_qini"))
  } else if (target == "QINI") {
    res <- dplyr::select(res, -dplyr::starts_with("rate_autoc"))
  }

  res
}


#' Screen outcomes by ATE and/or rate significance with multiplicity control
#'
#' Returns the names of outcome variables whose policy test passes the adjusted threshold,
#' applying rules based on ATE and rate metrics with optional multiplicity adjustment.
#'
#' @param model_results List; a margot result containing `$results` with each element
#'   having `ate`, `rate_result`, and `rate_qini` entries.
#' @param rule Character; one of:
#'   * "ate_or_rate" – keep if either ATE or the selected rate test is significant;
#'   * "ate"          – keep only if the ATE is significant;
#'   * "rate"         – keep only if the selected rate test is significant.
#' @param target Character; which rate metric defines “rate significant?”:
#'   * "AUTOC"  – test only AUTOC rate;
#'   * "QINI"   – test only QINI rate;
#'   * "either" – significant if either AUTOC or QINI passes;
#'   * "both"   – significant only if both AUTOC and QINI pass.
#' @param alpha Numeric; two-sided significance level (default 0.05).
#' @param adjust Character; multiplicity adjustment method for p-values:
#'   "none", "bonferroni", "holm", "BH"/"fdr", or "BY". Default "none".
#'
#' @return Character vector of outcome names to keep (without the `model_` prefix).
#'
#' @details
#' If `adjust != "none"`, p-values for ATE, AUTOC rate, and QINI rate
#' are each adjusted across outcomes using `stats::p.adjust()`. Significance flags are
#' determined by comparing adjusted p-values to `alpha`. Outcomes are then filtered
#' according to `rule` and `target` as described above.
#'
#' @export
#' @importFrom stats pnorm p.adjust
margot_screen_models <- function(model_results,
                                 rule   = c("ate_or_rate", "ate", "rate"),
                                 target = c("AUTOC", "QINI", "either", "both"),
                                 alpha  = 0.05,
                                 adjust = c("none", "bonferroni", "holm", "BH", "fdr", "BY")) {
  stopifnot(is.list(model_results), "results" %in% names(model_results))
  rule   <- match.arg(rule)
  target <- toupper(match.arg(target))
  adjust <- match.arg(adjust)

  outcomes      <- names(model_results$results)
  n_outcomes    <- length(outcomes)
  ate_p_raw     <- rep(NA_real_, n_outcomes)
  rate_a_p_raw  <- rep(NA_real_, n_outcomes)
  rate_q_p_raw  <- rep(NA_real_, n_outcomes)

  for (i in seq_along(outcomes)) {
    res <- model_results$results[[outcomes[i]]]
    if (is.null(res$ate) || is.null(res$rate_result) || is.null(res$rate_qini)) next

    ate_est <- unname(res$ate["estimate"])
    ate_se  <- unname(res$ate["std.err"])
    ate_p_raw[i] <- 2 * stats::pnorm(-abs(ate_est / ate_se))

    rate_a_p_raw[i] <- 2 * stats::pnorm(-abs(res$rate_result$estimate / res$rate_result$std.err))
    rate_q_p_raw[i] <- 2 * stats::pnorm(-abs(res$rate_qini$estimate   / res$rate_qini$std.err))
  }

  if (adjust != "none") {
    ate_p_adj    <- stats::p.adjust(ate_p_raw,    method = adjust)
    rate_a_p_adj <- stats::p.adjust(rate_a_p_raw, method = adjust)
    rate_q_p_adj <- stats::p.adjust(rate_q_p_raw, method = adjust)
  } else {
    ate_p_adj    <- ate_p_raw
    rate_a_p_adj <- rate_a_p_raw
    rate_q_p_adj <- rate_q_p_raw
  }

  ate_sig <- ate_p_adj < alpha
  sig_a   <- rate_a_p_adj < alpha
  sig_q   <- rate_q_p_adj < alpha

  rate_sig <- switch(target,
                     AUTOC  = sig_a,
                     QINI   = sig_q,
                     EITHER = sig_a | sig_q,
                     BOTH   = sig_a & sig_q)

  keep_flag <- switch(rule,
                      ate_or_rate = ate_sig | rate_sig,
                      ate         = ate_sig,
                      rate        = rate_sig)

  sub("^model_", "", outcomes[keep_flag & !is.na(keep_flag)])
}



#' Attach policy-value tests to a batch of models
#'
#' Runs `margot_compute_policy_value()` at specified depths for each outcome,
#' optionally in parallel via **future**/**furrr**.
#'
#' @param cf_out   list; a margot result containing `$results` and `$outcome_vars`
#' @param outcomes character vector or NULL; outcome names (without `model_` prefix). NULL means all.
#' @param depths   integer vector; tree depths to evaluate. default c(1L, 2L)
#' @param R        integer ≥ 199; bootstrap replicates. default 499L
#' @param seed     integer; RNG seed for reproducibility. default 42L
#' @param parallel logical; whether to use parallel processing. default FALSE
#'
#' @return invisibly returns modified `cf_out` with added policy-value tests
#' @keywords internal
#' @importFrom purrr map
#' @importFrom future plan multisession
#' @importFrom furrr future_map furrr_options
margot_add_policy_values_batch <- function(cf_out,
                                           outcomes = NULL,
                                           depths   = c(1L, 2L),
                                           R        = 499L,
                                           seed     = 42L,
                                           parallel = FALSE) {
  stopifnot(is.list(cf_out), "results" %in% names(cf_out))
  if (is.null(outcomes)) {
    outcomes <- cf_out$outcome_vars
  }
  model_keys <- paste0("model_", outcomes)

  # choose mapper based on parallel flag
  if (parallel && requireNamespace("future", quietly = TRUE) && requireNamespace("furrr", quietly = TRUE)) {
    ncores <- parallel::detectCores(logical = FALSE)
    future::plan(future::multisession, workers = max(1, ncores - 1))
    mapper <- furrr::future_map
    mapper_opts <- furrr::furrr_options(seed = seed)
    map_args <- list(.options = mapper_opts)
  } else {
    mapper <- purrr::map
    map_args <- list()
  }

  # helper to add only missing depths
  add_missing <- function(model) {
    missing_depths <- depths[vapply(depths, function(d) {
      is.null(model[[paste0("policy_value_depth_", d)]])
    }, logical(1))]

    if (length(missing_depths) > 0L) {
      model <- margot_add_policy_values(
        model,
        depths = missing_depths,
        R      = R,
        seed   = seed
      )
    }
    model
  }

  # apply to each selected model
  cf_out$results[model_keys] <- do.call(
    mapper,
    c(list(cf_out$results[model_keys], add_missing), map_args)
  )

  invisible(cf_out)
}


#' Collect and adjust policy-value tests, focusing on desired depths
#'
#' Combines [margot_collect_policy_values()] and [margot_adjust_policy_p()] into a single summary step.
#'
#' @param cf_out margot result list (after running batch helper).
#' @param depths integer vector. Depths to include. Default 2.
#' @param adjust character. Multiplicity adjustment method. Default "bonferroni";
#'   other options include "holm" or any supported by [stats::p.adjust()].
#' @param alpha numeric. Significance threshold after adjustment. Default 0.05.
#'
#' @return Adjusted summary `tibble` filtered to requested depths.
#' @export
margot_policy_summary <- function(cf_out,
                                  depths = 2L,
                                  adjust = "bonferroni",
                                  alpha  = 0.05) {
  cf_out |>
    margot_collect_policy_values(depths = depths) |>
    margot_adjust_policy_p(method = adjust, alpha = alpha)
}
