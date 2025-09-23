#' Policy Tree Summary Report (text + markdown table)
#'
#' Generates a concise textual summary and a markdown table for policy tree results
#' (typically from `margot_policy_tree_stability()`). The report focuses on:
#' - Policy value vs control-all and vs treat-all (95% CIs)
#' - Average uplift among treated (95% CI) and coverage treated (%)
#' - Optional stability highlights (consensus strength and top split variable)
#' - Optional split breakdown: per-branch selective uplifts and contributions
#'
#' Method (estimation and identities):
#' - For each model and depth, we evaluate the consensus policy on the held-out test set
#'   used to build `plot_data` (policytree test indices).
#' - Let μ0(x), μ1(x) be conditional means E[Y(0)|X=x], E[Y(1)|X=x] estimated by
#'   policytree/GRF. We use `double_robust_scores()` stored in `dr_scores` to obtain
#'   per-unit estimates for μ0 and μ1 on the test rows, and we predict policy actions a(x)
#'   from the consensus policy tree. We ensure all quantities are aligned to the same test
#'   rows by mapping via rownames and a single complete-case mask over the policy columns.
#' - Policy value contrasts are computed as sample averages (plugin DR estimators):
#'     PV vs control-all  = mean( 1{a(x)=Treat} × (μ1(x) − μ0(x)) )
#'     PV vs treat-all    = mean( 1{a(x)=Control} × (μ0(x) − μ1(x)) )
#' - Treated-only metrics link to these via the identity:
#'     Coverage = mean(1{a(x)=Treat}), Uplift_Treated = mean(μ1 − μ0 | a(x)=Treat)
#'     PV vs control-all = Coverage × Uplift_Treated
#' - Split contributions report the subgroup decomposition of these means over the first split
#'   (or leaves), and sum exactly to the overall policy values.
#'
#' Standard errors:
#' - `se_method = "bootstrap"` (default): bootstrap resampling of the test rows and
#'   re-prediction of the policy actions to obtain SEs and CIs.
#' - `se_method = "plugin"`: influence-function plug-in SEs via the sample variance of the
#'   per-unit contributions (e.g., TreatMask × (μ1−μ0) for PV vs control-all); i.e.,
#'   se = sd(contribution)/sqrt(n). This mimics the asymptotic SE used by policytree
#'   for value estimators based on DR pseudo-outcomes when raw (Y, W) are not required.
#'
#' Automated recommendations (optional):
#' - When `auto_recommend = TRUE`, the reporter identifies a dominant favorable split (by PV(control-all)
#'   contribution share) and evaluates a restricted policy that treats only inside that split (keeps
#'   the learned actions there; forces control elsewhere). It then compares full vs restricted policies
#'   and emits a recommendation per model: deploy_full, deploy_restricted, caution, or do_not_deploy.
#'
#' @param object A `margot_stability_policy_tree` object.
#' @param model_names Optional character vector of outcome names (with or without
#'   `model_` prefix). Default: all models in `object`.
#' @param depth Integer; policy tree depth to summarize (default 2).
#' @param depths_by_model Optional named vector/list mapping models (with or without `model_`
#'   prefix) to depth 1 or 2; overrides `depth` on a per-model basis so mixed-depth reports can
#'   be generated.
#' @param R Integer ≥ 199; bootstrap replicates for reporter CIs (default 499).
#' @param seed Integer or NULL; RNG seed (default 42).
#' @param label_mapping Optional named list for display labels.
#' @param digits Integer; rounding for numeric displays (default 3).
#' @param include_stability Logical; include stability highlights per model (default TRUE).
#' @param include_explanation Logical; include policy value explanation block (default TRUE).
#' @param table_type Character; one of "full" (default) or "treated_only" for condensed table.
#' @param include_split_breakdown Character; one of "branch" (default), "leaf", or "none".
#'   Adds a compact per-split summary per model with action-conditional treated-only uplift(s)
#'   and contributions to policy value vs control-all and vs treat-all. Coherent by construction:
#'   we only report uplift among treated in treated subgroups and uplift among controlled in
#'   controlled subgroups.
#' @param se_method Character; one of "bootstrap" (default) or "plugin" for influence-function
#'   plug-in SEs based on per-unit DR contributions.
#' @param source_models Optional original model container (e.g., from `margot_causal_forest()`)
#'   if you wish to run external audits; not required for estimation.
#' @param audit_with_policytree Logical; if TRUE and `source_models` are available, performs a
#'   side-by-side audit using the original GRF predictions on the same test rows: (i) tau_hat-based
#'   policy values PVc = mean(1{a=Treat}×tau_hat), PVT = mean(1{a=Control}×(-tau_hat)); and
#'   (ii) conditional-means-based values using `conditional_means` if present. This helps validate
#'   that DR-based values agree with GRF-based alternatives on the same evaluation slice.
#' @param auto_recommend Logical; if TRUE, compute and report restricted-policy recommendations
#'   derived from dominant favorable split(s). Default FALSE.
#' @param dominance_threshold Numeric in (0,1); minimum share of |PV(control-all)| contribution to
#'   consider a split dominant (default 0.6).
#' @param strict_branch Logical; if TRUE, require positive branch-level treated-only uplift with
#'   CI lower bound > 0 to consider a branch favorable (default FALSE).
#' @param restricted_scope Character; one of "leaf" (default) or "branch"; defines the granularity
#'   used to construct the restricted policy (treat inside selected branch/leaf; control elsewhere).
#' @param split_compact Logical; if TRUE (default), builds compact split tables from
#'   `split_breakdown` with reduced columns.
#' @param contrib_scale Character; one of "rel_abs" (default, signed share of |PV|),
#'   "rel_signed" (share of signed PV), or "abs" (raw contribution values).
#' @param show_pvt Logical; include PV vs treat-all contribution column in compact tables (default FALSE).
#' @param show_ci_compact Logical; include CIs in compact tables (default TRUE).
#' @param min_action_pct Numeric (default 5); below this percent treated or controlled in a split,
#'   suppress the corresponding action-specific uplift (report as "—").
#' @param split_drop_zero Logical (default TRUE); drop split rows whose PV contributions are numerically
#'   zero for both baselines in the compact tables.
#' @param split_top_only Logical (default FALSE); when TRUE, retain only the top-contributing split per
#'   model in the compact tables (helpful for depth-1 trees where complementary branches are redundant).
#'
#' @return A list with:
#'   - `text`: Combined summary text (character scalar)
#'   - `report`: Narrative policy summary formatted for scientific reporting
#'   - `interpretation`: Concise sentence-level summary listing wins, caution, and uncertain models
#'   - `table_md`: Markdown table (character scalar) when `render_markdown = TRUE`
#'   - `table_df`: Data frame of policy value summary rows (title-case columns)
#'   - `split_breakdown`: Named list mapping model id -> data frame of split-level metrics
#'   - `split_table_compact`: Named list mapping model id -> compact split table (data frame)
#'   - `split_table_compact_df`: Combined compact split table with `Outcome` column
#'   - `split_table_compact_md`: Markdown rendering of combined compact table
#'   - `method_overview`: Character string describing the estimation approach and identities
#'   - `method_by_model`: Named list of per-model method blurbs (including n_eval, coverage, SE method)
#'   - `policy_value_audit`: Optional named list of audit results when `audit_with_policytree = TRUE`
#'   - `recommendations_by_model`: Optional per-model recommendation objects when `auto_recommend = TRUE`
#'   - `recommendations_text`: Optional combined markdown text for recommendations
#'   - `recommended_model_ids`: Character vector of model identifiers flagged for deployment
#'   - `recommended_model_names`: Character vector of human-readable labels for the recommended models
#'   - `practical_takeaways_text`: Optional high-level bullet summary of deployment guidance
#'   - `wins_model_ids`: Character vector of model identifiers where policy vs control-all is strictly positive (CI > 0)
#'   - `wins_model_names`: Character vector of human-readable labels matching `wins_model_ids`
#'   - `neutral_model_ids`: Character vector of model identifiers with inconclusive policy vs control-all evidence (CI crosses 0)
#'   - `neutral_model_names`: Character vector of human-readable labels matching `neutral_model_ids`
#'   - `caution_model_ids`: Character vector of model identifiers where policy vs control-all is negative (CI < 0)
#'   - `caution_model_names`: Character vector of human-readable labels matching `caution_model_ids`
#'   - `group_table`: Named list of grouped brief tables (data frames)
#'   - `group_table_df`: Combined grouped table with a `Group` column
#'   - `model_depths`: Named integer vector giving the depth used for each model in the report
#'   - `depth_map`: Convenience alias for `model_depths` (for passing to `depths_by_model`)
#'   - `depth1_model_ids`: Character vector of model identifiers summarised at depth 1
#'   - `depth1_model_names`: Character vector of human-readable labels for depth-1 models
#'   - `depth2_model_ids`: Character vector of model identifiers summarised at depth 2
#'   - `depth2_model_names`: Character vector of human-readable labels for depth-2 models
#'   - `recommended_depth1_model_ids`: Recommended-for-deployment models evaluated at depth 1
#'   - `recommended_depth1_model_names`: Human-readable labels for recommended depth-1 models
#'   - `recommended_depth2_model_ids`: Recommended-for-deployment models evaluated at depth 2
#'   - `recommended_depth2_model_names`: Human-readable labels for recommended depth-2 models
#'
#' @export
margot_policy_summary_report <- function(object,
                                         model_names = NULL,
                                         depth = 2L,
                                         depths_by_model = NULL,
                                         R = 499L,
                                         seed = 42L,
                                         label_mapping = NULL,
                                         digits = 3,
                                         include_stability = TRUE,
                                         include_explanation = TRUE,
                                         table_type = c("full", "treated_only"),
                                         original_df = NULL,
                                         verbose = TRUE,
                                         report_df = NULL,
                                         order_by = c("pv_control_all", "uplift_treated", "none"),
                                         decreasing = TRUE,
                                         filter_significant = FALSE,
                                         alpha = 0.05,
                                         group_by_sign = TRUE,
                                         compact = FALSE,
                                         render_markdown = TRUE,
                                         include_split_breakdown = c("branch", "leaf", "none"),
                                         split_compact = TRUE,
                                         contrib_scale = c("rel_abs", "rel_signed", "abs"),
                                         show_pvt = FALSE,
                                         show_ci_compact = TRUE,
                                         min_action_pct = 5,
                                         split_drop_zero = TRUE,
                                         split_top_only = FALSE,
                                         se_method = c("bootstrap", "plugin"),
                                         source_models = NULL,
                                         audit_with_policytree = FALSE,
                                         auto_recommend = FALSE,
                                         dominance_threshold = 0.6,
                                         strict_branch = FALSE,
                                         restricted_scope = c("leaf", "branch")) {
  stopifnot(inherits(object, "margot_stability_policy_tree"))
  table_type <- match.arg(table_type)
  order_by <- match.arg(order_by)
  include_split_breakdown <- match.arg(include_split_breakdown)
  restricted_scope <- match.arg(restricted_scope)
  contrib_scale <- match.arg(contrib_scale)
  se_method <- match.arg(se_method)

  # determine model set
  if (is.null(model_names)) {
    model_names <- names(object$results)
  } else {
    model_names <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
  }

  model_depths <- setNames(rep(as.integer(depth), length(model_names)), model_names)
  if (!is.null(depths_by_model)) {
    if (is.null(names(depths_by_model))) {
      stop("depths_by_model must be a named vector or list")
    }
    provided_names <- ifelse(grepl("^model_", names(depths_by_model)), names(depths_by_model), paste0("model_", names(depths_by_model)))
    missing_models <- setdiff(provided_names, model_names)
    if (length(missing_models)) {
      stop("depths_by_model references unknown models: ", paste(missing_models, collapse = ", "))
    }
    model_depths[provided_names] <- as.integer(depths_by_model)
  }
  if (!all(model_depths %in% c(1L, 2L))) {
    stop("depth values must be 1 or 2")
  }
  unique_depths <- sort(unique(model_depths))

  depth1_model_ids <- names(model_depths[model_depths == 1L])
  depth2_model_ids <- names(model_depths[model_depths == 2L])
  depth1_model_names <- if (length(depth1_model_ids)) vapply(depth1_model_ids, function(mn) .apply_label_stability(gsub("^model_", "", mn), label_mapping), character(1)) else character()
  depth2_model_names <- if (length(depth2_model_ids)) vapply(depth2_model_ids, function(mn) .apply_label_stability(gsub("^model_", "", mn), label_mapping), character(1)) else character()


  # coherent policy value reporter (ensures PVs, treated-only, and split metrics align)
  .compute_coherent_policy_report <- function(object, model_names, model_depths, R, seed, alpha, label_mapping, verbose, se_method) {
    normalize_actions <- function(actions) {
      if (is.null(actions)) return(actions)
      actions <- as.integer(actions)
      if (!length(actions)) return(actions)
      if (all(is.na(actions))) return(actions)
      if (min(actions, na.rm = TRUE) == 0L) actions <- actions + 1L
      actions
    }
    if (!is.null(seed)) set.seed(seed)
    z <- stats::qnorm(1 - alpha / 2)
    out <- list()
    if (isTRUE(verbose)) {
      depth_info <- paste(sort(unique(model_depths)), collapse = ", ")
      cli::cli_alert_info("Evaluating consensus policies ({length(model_names)} model{?s}; depth(s) = {depth_info}; se={se_method})")
      pb <- cli::cli_progress_bar(total = length(model_names), format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}")
    }
    for (mn in model_names) {
      res <- object$results[[mn]]
      d <- model_depths[[mn]]
      tag <- paste0("policy_tree_depth_", d)
      pol <- res[[tag]]
      pd <- res$plot_data
      dr <- res$dr_scores
      if (is.null(dr)) dr <- res$dr_scores_flipped
      if (is.null(pol) || is.null(pd) || is.null(dr)) next
      full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
      if (is.null(full)) next
      if (!all(pol$columns %in% colnames(full))) next
      # build evaluation matrices consistently and align predictions via keep mask
      X_full <- as.data.frame(full[, pol$columns, drop = FALSE])
      # Map DR scores to the exact test rows using rownames if available
      test_idx <- pd$test_indices
      if (is.null(test_idx)) {
        test_idx <- suppressWarnings(as.integer(rownames(full)))
      }
      drm <- as.matrix(dr)
      if (!is.null(test_idx) && length(test_idx) == nrow(full) && all(!is.na(test_idx)) && max(test_idx) <= nrow(drm)) {
        drm_test <- drm[test_idx, , drop = FALSE]
      } else {
        # fallback: assume first nrow(full) rows correspond to test set
        take <- seq_len(min(nrow(full), nrow(drm)))
        drm_test <- drm[take, , drop = FALSE]
        if (length(take) < nrow(X_full)) {
          X_full <- X_full[take, , drop = FALSE]
        }
      }
      keep <- stats::complete.cases(X_full)
      if (!any(keep)) next
      Xk <- as.data.frame(X_full[keep, , drop = FALSE])
      drk <- as.matrix(drm_test[keep, , drop = FALSE])
      n <- nrow(Xk)
      if (!n || n < 2) next
      # predictions and effects
      # predict on full then subset to ensure alignment with keep
      a_hat_full <- tryCatch(predict(pol, X_full), error = function(e) NULL)
      if (is.null(a_hat_full)) next
      if (is.matrix(a_hat_full)) a_hat_full <- a_hat_full[, 1]
      a_hat <- normalize_actions(a_hat_full[keep])
      eff <- as.numeric(drk[, 2] - drk[, 1])
      if (length(a_hat) != length(eff)) next
      # PV vs control-all and vs treat-all using same slice + predictions
      pv_ctrl <- mean((a_hat == 2L) * eff)
      pv_treat <- mean((a_hat == 1L) * (-eff))
      # standard errors and CIs depending on se_method
      if (identical(se_method, "plugin")) {
        s_ctrl <- as.numeric((a_hat == 2L) * eff)
        s_treat <- as.numeric((a_hat == 1L) * (-eff))
        se_ctrl <- stats::sd(s_ctrl) / sqrt(n)
        se_treat <- stats::sd(s_treat) / sqrt(n)
        lo_ctrl <- pv_ctrl - z * se_ctrl
        hi_ctrl <- pv_ctrl + z * se_ctrl
        lo_treat <- pv_treat - z * se_treat
        hi_treat <- pv_treat + z * se_treat
      } else {
        reps_ctrl <- replicate(R, {
          idx <- sample.int(n, n, TRUE)
          # re-predict on bootstrap sample slice
          ah <- tryCatch(predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
          if (is.null(ah)) return(NA_real_)
          if (is.matrix(ah)) ah <- ah[, 1]
          ah <- normalize_actions(ah)
          ef <- as.numeric(drk[idx, 2] - drk[idx, 1])
          mean((ah == 2L) * ef)
        })
        reps_ctrl <- reps_ctrl[!is.na(reps_ctrl)]
        reps_treat <- replicate(R, {
          idx <- sample.int(n, n, TRUE)
          ah <- tryCatch(predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
          if (is.null(ah)) return(NA_real_)
          if (is.matrix(ah)) ah <- ah[, 1]
          ah <- normalize_actions(ah)
          ef <- as.numeric(drk[idx, 2] - drk[idx, 1])
          mean((ah == 1L) * (-ef))
        })
        reps_treat <- reps_treat[!is.na(reps_treat)]
        se_ctrl <- stats::sd(reps_ctrl)
        se_treat <- stats::sd(reps_treat)
        lo_ctrl <- pv_ctrl - z * se_ctrl
        hi_ctrl <- pv_ctrl + z * se_ctrl
        lo_treat <- pv_treat - z * se_treat
        hi_treat <- pv_treat + z * se_treat
      }
      # treated-only uplift and coverage
      treat_mask <- a_hat == 2L
      coverage <- mean(treat_mask)
      uplift_t <- if (any(treat_mask)) mean(eff[treat_mask]) else NA_real_
      if (identical(se_method, "plugin")) {
        if (any(treat_mask)) {
          se_uplift <- stats::sd(eff[treat_mask]) / sqrt(sum(treat_mask))
          lo_u <- uplift_t - z * se_uplift
          hi_u <- uplift_t + z * se_uplift
        } else {
          se_uplift <- NA_real_
          lo_u <- NA_real_
          hi_u <- NA_real_
        }
      } else {
        reps_uplift <- replicate(R, {
          idx <- sample.int(n, n, TRUE)
          ah <- predict(pol, Xk[idx, , drop = FALSE])
          if (is.matrix(ah)) ah <- ah[, 1]
          ah <- normalize_actions(ah)
          ef <- drk[idx, 2] - drk[idx, 1]
          tm <- ah == 2L
          if (any(tm)) mean(ef[tm]) else NA_real_
        })
        reps_uplift <- reps_uplift[!is.na(reps_uplift)]
        if (length(reps_uplift) > 1) {
          se_uplift <- stats::sd(reps_uplift)
          lo_u <- uplift_t - z * se_uplift
          hi_u <- uplift_t + z * se_uplift
        } else {
          se_uplift <- NA_real_
          lo_u <- NA_real_
          hi_u <- NA_real_
        }
      }
      out_name <- gsub("^model_", "", mn)
      out_label <- .apply_label_stability(out_name, label_mapping)
      # two rows, one per contrast, carrying treated-only metrics for convenience
      out[[length(out) + 1L]] <- data.frame(
        model = mn,
        outcome = out_name,
        outcome_label = out_label,
        depth = d,
        contrast = "policy - control_all",
        estimate = pv_ctrl,
        std_err = se_ctrl,
        ci_lo = lo_ctrl,
        ci_hi = hi_ctrl,
        avg_uplift_treated = uplift_t,
        avg_uplift_treated_se = se_uplift,
        avg_uplift_treated_ci_lo = lo_u,
        avg_uplift_treated_ci_hi = hi_u,
        coverage_treated = coverage,
        n_eval = n,
        stringsAsFactors = FALSE
      )
      out[[length(out) + 1L]] <- data.frame(
        model = mn,
        outcome = out_name,
        outcome_label = out_label,
        depth = d,
        contrast = "policy - treat_all",
        estimate = pv_treat,
        std_err = se_treat,
        ci_lo = lo_treat,
        ci_hi = hi_treat,
        avg_uplift_treated = uplift_t,
        avg_uplift_treated_se = se_uplift,
        avg_uplift_treated_ci_lo = lo_u,
        avg_uplift_treated_ci_hi = hi_u,
        coverage_treated = coverage,
        n_eval = n,
        stringsAsFactors = FALSE
      )
      if (isTRUE(verbose)) cli::cli_progress_update()
    }
    if (isTRUE(verbose)) cli::cli_progress_done()
    if (!length(out)) return(data.frame())
    do.call(rbind, out)
  }

  depth_display <- paste(unique_depths, collapse = ", ")
  if (verbose) cli::cli_alert_info("Computing policy values (R = {R}, depth(s) = {depth_display}, se = {se_method})")
  rep <- .compute_coherent_policy_report(object, model_names, model_depths, R, seed, alpha, label_mapping, verbose, se_method)
  if (!nrow(rep)) {
    cli::cli_alert_warning("No evaluable models for coherent PV computation; falling back to reporter")
    rep <- margot_report_consensus_policy_value(
      object,
      model_names = model_names,
      depths = unique_depths,
      R = R,
      seed = seed,
      include_treated_only = TRUE,
      label_mapping = label_mapping,
      verbose = verbose
    )
  }

  rep_list <- lapply(model_names, function(mn) {
    d <- model_depths[[mn]]
    rep[rep$model == mn & rep$depth == d, , drop = FALSE]
  })
  rep_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, rep_list)
  rep <- if (length(rep_list)) do.call(rbind, rep_list) else NULL
  if (is.null(rep) || !nrow(rep)) {
    stop("No policy value rows available for the requested models/depths")
  }

  # build table df depending on type
  if (table_type == "treated_only") {
    tbl_df <- margot_table_treated_only(rep, label_mapping = label_mapping, digits = digits)
  } else {
    tbl_df <- margot_table_consensus_policy_value(object, report_df = rep, label_mapping = label_mapping)
  }
  # updated column names for readability
  col_map <- c(
    outcome = "Outcome",
    depth = "Depth",
    contrast = "Contrast",
    estimate = "Policy Value (95% CI)",
    std_err = "Std. Error",
    ci = "Policy Value (95% CI)",
    n_eval = "Evaluation N",
    avg_uplift_treated = "Effect in Treated (95% CI)",
    avg_uplift_treated_ci = "Effect in Treated (95% CI)",
    coverage_treated_pct = "Coverage (%)"
  )
  for (nm in names(tbl_df)) {
    if (nm %in% names(col_map)) {
      names(tbl_df)[names(tbl_df) == nm] <- col_map[[nm]]
    }
  }

  # markdown table (optional)
  if (isTRUE(render_markdown)) {
    if (verbose) cli::cli_alert_info("Rendering summary table ({table_type})")
    table_md <- tryCatch({
      knitr::kable(tbl_df, format = "markdown")
    }, error = function(e) {
      # fallback simple table
      paste(capture.output(print(tbl_df)), collapse = "\n")
    })
  } else {
    table_md <- ""
  }

  # small helper for policy value explanation
  policy_expl <- paste0(
    "Policy value vs control-all: mean benefit when treating only those recommended; ",
    "policy value vs treat-all: mean benefit when withholding treatment only where the policy recommends control. ",
    "Avg uplift among treated: average (DR_treat − DR_control) across units the policy recommends to treat."
  )

  # build per-model text
  lines <- character()
  if (length(unique_depths) > 1) {
    depth_lines <- paste(vapply(model_names, function(mn) {
      lab <- .apply_label_stability(gsub("^model_", "", mn), label_mapping)
      paste0(lab, " → ", model_depths[[mn]])
    }, character(1)), collapse = "; ")
    lines <- c(lines, paste0("Depth assignments: ", depth_lines))
  }
  if (verbose) {
    cli::cli_alert_info("Summarising {length(model_names)} model{?s}")
  }

  # build per-model summary frame aligned with table columns
  # extract control-all and treat-all rows per model
  ctrl_rows <- rep[rep$contrast == "policy - control_all", , drop = FALSE]
  treat_rows <- rep[rep$contrast == "policy - treat_all", , drop = FALSE]
  # merge by model
  merge_by <- c("model", "outcome", "outcome_label", "depth")
  mm <- merge(ctrl_rows, treat_rows, by = merge_by, all = TRUE, suffixes = c("_ctrl", "_treat"))
  # pick a donor row for uplift/coverage (identical across contrasts in our reporter)
  mm$uplift <- mm$avg_uplift_treated_ctrl
  mm$uplift_lo <- mm$avg_uplift_treated_ci_lo_ctrl
  mm$uplift_hi <- mm$avg_uplift_treated_ci_hi_ctrl
  mm$coverage <- mm$coverage_treated_ctrl
  # fallbacks in case only treat rows exist
  na_idx <- is.na(mm$uplift) & !is.na(mm$avg_uplift_treated_treat)
  mm$uplift[na_idx] <- mm$avg_uplift_treated_treat[na_idx]
  mm$uplift_lo[na_idx] <- mm$avg_uplift_treated_ci_lo_treat[na_idx]
  mm$uplift_hi[na_idx] <- mm$avg_uplift_treated_ci_hi_treat[na_idx]
  mm$coverage[na_idx] <- mm$coverage_treated_treat[na_idx]

  # ordering and filtering
  if (filter_significant) {
    # keep models with pv_control_all CI excluding 0
    mm <- mm[!(mm$ci_lo_ctrl <= 0 & mm$ci_hi_ctrl >= 0), , drop = FALSE]
  }
  if (order_by != "none") {
    if (order_by == "pv_control_all") {
      ord <- order(mm$estimate_ctrl, decreasing = decreasing, na.last = TRUE)
    } else { # uplift_treated
      ord <- order(mm$uplift, decreasing = decreasing, na.last = TRUE)
    }
    mm <- mm[ord, , drop = FALSE]
  }

  # convert to original scale (add small labels)
  # precompute transform info map for speed (if requested)
  trinfo <- list()
  if (!is.null(original_df)) {
    for (mn in model_names) {
      trinfo[[mn]] <- tryCatch(get_outcome_transformation_info(mn, original_df), error = function(e) NULL)
    }
  }

  convert_original <- function(eff, model_name) {
    if (is.null(original_df) || is.na(eff)) return("")
    info <- trinfo[[model_name]]
    if (is.null(info)) return("")
    if (isTRUE(info$has_z) && !isTRUE(info$has_log)) {
      val <- eff * info$orig_sd
      return(paste0(" (original scale: ", round(val, digits), ")"))
    }
    if (isTRUE(info$has_z) && isTRUE(info$has_log)) {
      dlog <- eff * info$log_sd
      pct <- (exp(dlog) - 1) * 100
      return(paste0(" (original scale: ", round(pct, 1), "%)"))
    }
    ""
  }

  fmt_ci <- function(lo, hi) paste0("[", round(lo, digits), ", ", round(hi, digits), "]")

  # classify models by sign of pv_control_all (always available for downstream uses)
  wins <- mm[mm$ci_lo_ctrl > 0, , drop = FALSE]
  neutral <- mm[mm$ci_lo_ctrl <= 0 & mm$ci_hi_ctrl >= 0, , drop = FALSE]
  harm <- mm[mm$ci_hi_ctrl < 0, , drop = FALSE]

  # Optionally group by sign for text/table rendering
  if (isTRUE(group_by_sign)) {
    groups <- list(
      "Wins (pv vs control-all > 0, CI>0)" = wins,
      "Neutral (pv vs control-all ~ 0)" = neutral,
      "Caution (pv vs control-all < 0, CI<0)" = harm
    )
  } else {
    groups <- list("Summary" = mm)
  }

  # helper: compute first-split branch/leaf breakdown for a model
  # returns a data.frame with one row per branch/leaf containing coherent, action-conditional metrics.
  .compute_split_breakdown <- function(mn, mode = c("branch", "leaf"), model_depths) {
    mode <- match.arg(mode)
    local_mode <- mode
    res <- object$results[[mn]]
    d <- model_depths[[mn]]
    tag <- paste0("policy_tree_depth_", d)
    pol <- res[[tag]]
    pd <- res$plot_data
    dr <- res$dr_scores
    if (is.null(dr)) dr <- res$dr_scores_flipped
    if (is.null(pol) || is.null(pd) || is.null(dr)) return(NULL)
    full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
    if (is.null(full)) return(NULL)
    # align evaluation matrix to policy columns and complete cases
    if (!all(pol$columns %in% colnames(full))) return(NULL)
    X_full <- as.data.frame(full[, pol$columns, drop = FALSE])
    # Align DR scores to test rows via rownames if available
    test_idx <- suppressWarnings(as.integer(rownames(full)))
    drm <- as.matrix(dr)
    if (!is.null(test_idx) && all(!is.na(test_idx)) && length(test_idx) == nrow(full) && max(test_idx) <= nrow(drm)) {
      drm_test <- drm[test_idx, , drop = FALSE]
    } else {
      drm_test <- drm[seq_len(nrow(full)), , drop = FALSE]
    }
    keep <- stats::complete.cases(X_full)
    if (!any(keep)) return(NULL)
    Xk <- as.data.frame(X_full[keep, , drop = FALSE])
    drk <- drm_test[keep, , drop = FALSE]
    N <- nrow(Xk)
    # predict policy actions once, align via keep
    a_hat_full <- tryCatch(predict(pol, X_full), error = function(e) NULL)
    if (is.null(a_hat_full)) return(NULL)
    if (is.matrix(a_hat_full)) a_hat_full <- a_hat_full[, 1]
    a_hat <- as.integer(a_hat_full[keep])
    if (is.null(a_hat) || length(a_hat) != N) return(NULL)
    # effects under treat vs control
    eff <- as.numeric(drk[, 2] - drk[, 1])
    # identify grouping by split(s)
    nodes <- tryCatch(pol$nodes, error = function(e) NULL)
    if (is.null(nodes) || length(nodes) < 1) return(NULL)
    # first split
    var1 <- pol$columns[nodes[[1]]$split_variable]
    thr1 <- nodes[[1]]$split_value
    # build branch labels
    v1lab <- .apply_label_stability(var1, label_mapping)
    # branch membership
    left <- Xk[, var1] <= thr1
    right <- !left
    group_id <- if (local_mode == "branch") {
      # 1 = left, 2 = right
      out <- integer(N)
      out[left] <- 1L; out[right] <- 2L
      out
    } else {
      # leaf mode (depth 2 supported). If not available, fall back to branch mode
      if (length(nodes) < 3 || d < 2L) {
        out <- integer(N); out[left] <- 1L; out[right] <- 2L
        local_mode <- "branch"
        out
      } else {
        var2 <- pol$columns[nodes[[2]]$split_variable]
        thr2 <- nodes[[2]]$split_value
        var3 <- pol$columns[nodes[[3]]$split_variable]
        thr3 <- nodes[[3]]$split_value
        lvals <- Xk[left, var2]
        rvals <- Xk[right, var3]
        out <- integer(N)
        # leaf1: left & var2 <= thr2
        if (any(left)) {
          out[left] <- ifelse(lvals <= thr2, 1L, 2L)
        }
        # leaf3/4: right & var3 ... map to 3/4
        if (any(right)) {
          out[right] <- ifelse(rvals <= thr3, 3L, 4L)
        }
        out
      }
    }

    # build group labels
    mk_label <- function(gid) {
      if (local_mode == "branch") {
        if (gid == 1L) {
          paste0("baseline ", v1lab, " ≤ ", round(thr1, digits))
        } else {
          paste0("baseline ", v1lab, " > ", round(thr1, digits))
        }
      } else {
        # leaf mode (needs var2/var3); if missing, fallback already occurred
        var2 <- pol$columns[nodes[[2]]$split_variable]
        thr2 <- nodes[[2]]$split_value
        var3 <- pol$columns[nodes[[3]]$split_variable]
        thr3 <- nodes[[3]]$split_value
        v2lab <- .apply_label_stability(var2, label_mapping)
        v3lab <- .apply_label_stability(var3, label_mapping)
        if (gid == 1L) paste0("baseline ", v1lab, " ≤ ", round(thr1, digits),
                              " & baseline ", v2lab, " ≤ ", round(thr2, digits))
        else if (gid == 2L) paste0("baseline ", v1lab, " ≤ ", round(thr1, digits),
                                   " & baseline ", v2lab, " > ", round(thr2, digits))
        else if (gid == 3L) paste0("baseline ", v1lab, " > ", round(thr1, digits),
                                   " & baseline ", v3lab, " ≤ ", round(thr3, digits))
        else paste0("baseline ", v1lab, " > ", round(thr1, digits),
                    " & baseline ", v3lab, " > ", round(thr3, digits))
      }
    }

    groups <- sort(unique(group_id))
    # bootstrap helper
    set.seed(seed)
    z <- stats::qnorm(1 - alpha / 2)
    # Precompute bootstrap indices once
    if (isTRUE(R > 1L)) {
      bs_fun <- function(fun) stats::sd(replicate(R, fun(sample.int(N, N, TRUE))), na.rm = TRUE)
    } else {
      bs_fun <- function(fun) NA_real_
    }

    out_rows <- list()
    for (g in groups) {
      gid <- which(group_id == g)
      n_g <- length(gid)
      if (!n_g) next
      share <- n_g / N
      # action coverage within group
      a_g <- a_hat[gid]
      treat_within <- mean(a_g == 2L)
      ctrl_within <- mean(a_g == 1L)
      # treated-only uplift in this group (if any treated)
      treat_mask <- a_g == 2L
      uplift_treat <- if (any(treat_mask)) mean(eff[gid][treat_mask]) else NA_real_
      # avoid-harm uplift among controlled (for treat-all contrast)
      ctrl_mask <- a_g == 1L
      uplift_ctrl <- if (any(ctrl_mask)) mean(-eff[gid][ctrl_mask]) else NA_real_
      # contributions to policy values (sum over units in group of action-conditional effect, divided by N)
      contrib_ctrl_all <- sum((a_g == 2L) * eff[gid]) / N
      contrib_treat_all <- sum((a_g == 1L) * (-eff[gid])) / N

      # bootstrap SEs and CIs
      # define closures over current group for replicate
      se_u_treat <- if (any(treat_mask) && R > 1L) bs_fun(function(idx) {
        mask <- (group_id[idx] == g) & (a_hat[idx] == 2L)
        if (any(mask)) mean(eff[idx][mask]) else NA_real_
      }) else NA_real_
      se_u_ctrl <- if (any(ctrl_mask) && R > 1L) bs_fun(function(idx) {
        mask <- (group_id[idx] == g) & (a_hat[idx] == 1L)
        if (any(mask)) mean((-eff)[idx][mask]) else NA_real_
      }) else NA_real_
      se_c_ctrl_all <- if (R > 1L) bs_fun(function(idx) {
        sum((group_id[idx] == g & a_hat[idx] == 2L) * eff[idx]) / N
      }) else NA_real_
      se_c_treat_all <- if (R > 1L) bs_fun(function(idx) {
        sum((group_id[idx] == g & a_hat[idx] == 1L) * ((-eff)[idx])) / N
      }) else NA_real_

      lo_u_treat <- if (!is.na(se_u_treat)) uplift_treat - z * se_u_treat else NA_real_
      hi_u_treat <- if (!is.na(se_u_treat)) uplift_treat + z * se_u_treat else NA_real_
      lo_u_ctrl <- if (!is.na(se_u_ctrl)) uplift_ctrl - z * se_u_ctrl else NA_real_
      hi_u_ctrl <- if (!is.na(se_u_ctrl)) uplift_ctrl + z * se_u_ctrl else NA_real_
      lo_c_ctrl_all <- if (!is.na(se_c_ctrl_all)) contrib_ctrl_all - z * se_c_ctrl_all else NA_real_
      hi_c_ctrl_all <- if (!is.na(se_c_ctrl_all)) contrib_ctrl_all + z * se_c_ctrl_all else NA_real_
      lo_c_treat_all <- if (!is.na(se_c_treat_all)) contrib_treat_all - z * se_c_treat_all else NA_real_
      hi_c_treat_all <- if (!is.na(se_c_treat_all)) contrib_treat_all + z * se_c_treat_all else NA_real_

      out_rows[[length(out_rows) + 1L]] <- data.frame(
        group = if (local_mode == "branch") if (g == 1L) "Left" else "Right" else paste0("Leaf ", g),
        label = mk_label(g),
        n_group = n_g,
        share = share,
        treated_within = treat_within,
        control_within = ctrl_within,
        uplift_treated = uplift_treat,
        uplift_treated_lo = lo_u_treat,
        uplift_treated_hi = hi_u_treat,
        uplift_control = uplift_ctrl,
        uplift_control_lo = lo_u_ctrl,
        uplift_control_hi = hi_u_ctrl,
        contrib_pv_control_all = contrib_ctrl_all,
        contrib_pv_control_all_lo = lo_c_ctrl_all,
        contrib_pv_control_all_hi = hi_c_ctrl_all,
        contrib_pv_treat_all = contrib_treat_all,
        contrib_pv_treat_all_lo = lo_c_treat_all,
        contrib_pv_treat_all_hi = hi_c_treat_all,
        stringsAsFactors = FALSE
      )
    }
    if (!length(out_rows)) return(NULL)
    do.call(rbind, out_rows)
  }

  # prepare split breakdowns if requested
  split_breakdown <- list()
  if (include_split_breakdown != "none") {
    if (verbose) {
      cli::cli_alert_info("Computing {include_split_breakdown} split breakdowns ({length(unique(mm$model))} model{?s})")
      pb_sb <- cli::cli_progress_bar(total = length(unique(mm$model)), format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total}")
    }
    for (mn in unique(mm$model)) {
      split_breakdown[[mn]] <- tryCatch(
        .compute_split_breakdown(mn, mode = include_split_breakdown, model_depths = model_depths),
        error = function(e) NULL
      )
      if (isTRUE(verbose)) cli::cli_progress_update(id = pb_sb)
    }
    if (isTRUE(verbose)) cli::cli_progress_done(id = pb_sb)
  }

  # Consistency audit: PV(control-all) should equal coverage * uplift_treated and sum of split contributions
  coherence_audit <- NULL
  policy_value_audit <- NULL
  if (length(split_breakdown)) {
    rows <- list()
    for (mn in unique(mm$model)) {
      sb <- split_breakdown[[mn]]
      if (is.null(sb) || !nrow(sb)) next
      # pull PVs and treated-only from rep (coherent report)
      d <- model_depths[[mn]]
      sub_ctrl <- rep[rep$model == mn & rep$contrast == "policy - control_all" & rep$depth == d, , drop = FALSE]
      sub_treat <- rep[rep$model == mn & rep$contrast == "policy - treat_all" & rep$depth == d, , drop = FALSE]
      if (!nrow(sub_ctrl)) next
      pv_ctrl_rep <- sub_ctrl$estimate[1]
      pv_treat_rep <- if (nrow(sub_treat)) sub_treat$estimate[1] else NA_real_
      cov_t <- sub_ctrl$coverage_treated[1]
      upl_t <- sub_ctrl$avg_uplift_treated[1]
      pv_ctrl_cov_uplift <- cov_t * upl_t
      pv_ctrl_splits <- sum(sb$contrib_pv_control_all, na.rm = TRUE)
      pv_treat_splits <- sum(sb$contrib_pv_treat_all, na.rm = TRUE)
      d1 <- pv_ctrl_rep - pv_ctrl_cov_uplift
      d2 <- pv_ctrl_rep - pv_ctrl_splits
      d3 <- if (!is.na(pv_treat_rep)) pv_treat_rep - pv_treat_splits else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        model = mn,
        pv_ctrl_rep = pv_ctrl_rep,
        pv_ctrl_from_cov_uplift = pv_ctrl_cov_uplift,
        pv_ctrl_from_splits = pv_ctrl_splits,
        delta_ctrl_cov_uplift = d1,
        delta_ctrl_splits = d2,
        pv_treat_rep = pv_treat_rep,
        pv_treat_from_splits = pv_treat_splits,
        delta_treat_splits = d3,
        stringsAsFactors = FALSE
      )
      # warn if mismatch
      tol <- 1e-6
      if (!is.na(d1) && abs(d1) > tol) cli::cli_alert_warning("Coherence check (treated-only) mismatch for {.var {mn}}: Δ = {signif(d1, 3)}")
      if (!is.na(d2) && abs(d2) > tol) cli::cli_alert_warning("Coherence check (splits sum) mismatch for {.var {mn}}: Δ = {signif(d2, 3)}")
      if (!is.na(d3) && abs(d3) > tol) cli::cli_alert_warning("Coherence check (treat-all splits) mismatch for {.var {mn}}: Δ = {signif(d3, 3)}")
    }
    if (length(rows)) coherence_audit <- do.call(rbind, rows)
  }

  # Optional audit using source_models (tau_hat and conditional_means on same test rows)
  if (isTRUE(audit_with_policytree) && !is.null(source_models)) {
    audits <- list()
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Auditing policy values with GRF outputs ({length(unique(mm$model))} model{?s})")
      pb_ad <- cli::cli_progress_bar(total = length(unique(mm$model)), format = "{cli::pb_bar} {cli::pb_percent} {cli::pb_current}/{cli::pb_total}")
    }
    for (mn in unique(mm$model)) {
      d <- model_depths[[mn]]
      pol <- object$results[[mn]][[paste0("policy_tree_depth_", d)]]
      if (is.null(pol)) next
      # test frame and alignment
      pd <- object$results[[mn]]$plot_data
      if (is.null(pd)) next
      full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
      if (is.null(full)) next
      X_full <- as.data.frame(full[, pol$columns, drop = FALSE])
      test_idx <- suppressWarnings(as.integer(rownames(full)))
      # predict actions and keep mask
      a_hat_full <- tryCatch(predict(pol, X_full), error = function(e) NULL)
      if (is.null(a_hat_full)) next
      if (is.matrix(a_hat_full)) a_hat_full <- a_hat_full[, 1]
      keep <- stats::complete.cases(X_full)
      if (!any(keep)) next
      a_hat <- as.integer(a_hat_full[keep])
      # tau_hat-based audit
      tau_all <- tryCatch(source_models$results[[mn]]$tau_hat, error = function(e) NULL)
      tau_ok <- !is.null(tau_all)
      if (tau_ok) {
        if (!is.null(test_idx) && all(!is.na(test_idx)) && max(test_idx) <= length(tau_all)) {
          tau_test <- tau_all[test_idx]
        } else {
          tau_test <- tau_all[seq_len(nrow(full))]
        }
        tau_test <- as.numeric(tau_test[keep])
        pv_ctrl_tau <- mean((a_hat == 2L) * tau_test)
        pv_treat_tau <- mean((a_hat == 1L) * (-tau_test))
      } else {
        pv_ctrl_tau <- pv_treat_tau <- NA_real_
      }
      # conditional means-based audit
      cm_all <- tryCatch(source_models$results[[mn]]$conditional_means, error = function(e) NULL)
      if (!is.null(cm_all) && nrow(cm_all) >= nrow(full)) {
        if (!is.null(test_idx) && all(!is.na(test_idx)) && max(test_idx) <= nrow(cm_all)) {
          cm_test <- cm_all[test_idx, , drop = FALSE]
        } else {
          cm_test <- cm_all[seq_len(nrow(full)), , drop = FALSE]
        }
        cm_test <- cm_test[keep, , drop = FALSE]
        delta <- as.numeric(cm_test[, 2] - cm_test[, 1])
        pv_ctrl_cm <- mean((a_hat == 2L) * delta)
        pv_treat_cm <- mean((a_hat == 1L) * (-delta))
      } else {
        pv_ctrl_cm <- pv_treat_cm <- NA_real_
      }
      # our coherent PV (control/treat rows)
      d <- model_depths[[mn]]
      r_ctrl <- rep[rep$model == mn & rep$contrast == "policy - control_all" & rep$depth == d, , drop = FALSE]
      r_trt  <- rep[rep$model == mn & rep$contrast == "policy - treat_all" & rep$depth == d, , drop = FALSE]
      audits[[mn]] <- list(
        pv_ctrl_dr = if (nrow(r_ctrl)) r_ctrl$estimate[1] else NA_real_,
        pv_treat_dr = if (nrow(r_trt)) r_trt$estimate[1] else NA_real_,
        pv_ctrl_tau = pv_ctrl_tau,
        pv_treat_tau = pv_treat_tau,
        pv_ctrl_cm = pv_ctrl_cm,
        pv_treat_cm = pv_treat_cm
      )
      if (isTRUE(verbose)) cli::cli_progress_update(id = pb_ad)
    }
    if (length(audits)) policy_value_audit <- audits
    if (isTRUE(verbose)) cli::cli_progress_done(id = pb_ad)
  }

  # Automated recommendations: restricted policy per model treating only in dominant favorable split
  recommendations_by_model <- list()
  recommendations_text <- ""
  practical_takeaways_text <- ""
  recommended_model_ids <- character()
  recommended_model_names <- character()
  auto_decision_counts <- NULL
  decision_line <- NULL
  dominance_block <- NULL
  uncertainty_line <- NULL
  risk_line <- NULL
  if (isTRUE(auto_recommend) && length(split_breakdown)) {
    if (isTRUE(verbose)) cli::cli_alert_info("Computing restricted-policy recommendations ({length(unique(mm$model))} model{?s})")

    .eval_restricted <- function(mn, keep_groups, scope) {
      res <- object$results[[mn]]
      d <- model_depths[[mn]]
      tag <- paste0("policy_tree_depth_", d)
      pol <- res[[tag]]; pd <- res$plot_data; dr <- res$dr_scores
      if (is.null(dr)) dr <- res$dr_scores_flipped
      if (is.null(pol) || is.null(pd) || is.null(dr)) return(NULL)
      full <- if (!is.null(pd$X_test_full)) pd$X_test_full else pd$X_test
      if (is.null(full)) return(NULL)
      X_full <- as.data.frame(full[, pol$columns, drop = FALSE])
      test_idx <- suppressWarnings(as.integer(rownames(full)))
      drm <- as.matrix(dr)
      if (!is.null(test_idx) && all(!is.na(test_idx)) && length(test_idx) == nrow(full) && max(test_idx) <= nrow(drm)) {
        drm_test <- drm[test_idx, , drop = FALSE]
      } else {
        drm_test <- drm[seq_len(nrow(full)), , drop = FALSE]
      }
      keep <- stats::complete.cases(X_full)
      if (!any(keep)) return(NULL)
      Xk <- as.data.frame(X_full[keep, , drop = FALSE])
      drk <- as.matrix(drm_test[keep, , drop = FALSE])
      N <- nrow(Xk)
      a_full <- tryCatch(predict(pol, X_full), error = function(e) NULL)
      if (is.null(a_full)) return(NULL)
      if (is.matrix(a_full)) a_full <- a_full[, 1]
      a_hat <- as.integer(a_full[keep])
      eff <- as.numeric(drk[, 2] - drk[, 1])
      nodes <- pol$nodes
      var1 <- pol$columns[nodes[[1]]$split_variable]
      thr1 <- nodes[[1]]$split_value
      left <- Xk[, var1] <= thr1
      right <- !left
      if (identical(scope, "branch")) {
        gid <- integer(N); gid[left] <- 1L; gid[right] <- 2L
      } else {
      if (length(nodes) < 3 || d < 2L) {
          gid <- integer(N); gid[left] <- 1L; gid[right] <- 2L
        } else {
          var2 <- pol$columns[nodes[[2]]$split_variable]; thr2 <- nodes[[2]]$split_value
          var3 <- pol$columns[nodes[[3]]$split_variable]; thr3 <- nodes[[3]]$split_value
          gid <- integer(N)
          gid[left] <- ifelse(Xk[left, var2] <= thr2, 1L, 2L)
          gid[right] <- ifelse(Xk[right, var3] <= thr3, 3L, 4L)
        }
      }
      sel <- gid %in% keep_groups
      a_var <- a_hat
      a_var[!sel] <- 1L
      pv_ctrl <- mean((a_var == 2L) * eff)
      pv_treat <- mean((a_var == 1L) * (-eff))
      cov <- mean(a_var == 2L)
      upl <- if (any(a_var == 2L)) mean(eff[a_var == 2L]) else NA_real_
      z <- stats::qnorm(1 - alpha / 2)
      if (identical(se_method, "plugin")) {
        s_c <- (a_var == 2L) * eff; s_t <- (a_var == 1L) * (-eff)
        se_c <- stats::sd(s_c) / sqrt(N); se_t <- stats::sd(s_t) / sqrt(N)
        se_u <- if (any(a_var == 2L)) stats::sd(eff[a_var == 2L]) / sqrt(sum(a_var == 2L)) else NA_real_
        list(
          pv_ctrl = pv_ctrl, se_ctrl = se_c, lo_ctrl = pv_ctrl - z * se_c, hi_ctrl = pv_ctrl + z * se_c,
          pv_treat = pv_treat, se_treat = se_t, lo_treat = pv_treat - z * se_t, hi_treat = pv_treat + z * se_t,
          coverage = cov, uplift = upl, lo_u = if (is.na(se_u)) NA_real_ else upl - z * se_u,
          hi_u = if (is.na(se_u)) NA_real_ else upl + z * se_u, n_eval = N
        )
      } else {
        reps_c <- replicate(R, {
          idx <- sample.int(N, N, TRUE)
          # recompute group id and actions on bootstrap slice
          ah <- tryCatch(predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
          if (is.null(ah)) return(NA_real_)
          if (is.matrix(ah)) ah <- ah[, 1]
          # group ids for bootstrap slice
          if (identical(scope, "branch")) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          } else if (length(nodes) >= 3 && d >= 2L) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            v2 <- pol$columns[nodes[[2]]$split_variable]; t2 <- nodes[[2]]$split_value
            v3 <- pol$columns[nodes[[3]]$split_variable]; t3 <- nodes[[3]]$split_value
            gid_b <- integer(N)
            gid_b[lft] <- ifelse(Xk[idx, v2, drop = FALSE][lft, 1] <= t2, 1L, 2L)
            gid_b[!lft] <- ifelse(Xk[idx, v3, drop = FALSE][!lft, 1] <= t3, 3L, 4L)
          } else {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          }
          ah <- as.integer(ah)
          ah[!(gid_b %in% keep_groups)] <- 1L
          ef <- as.numeric(drk[idx, 2] - drk[idx, 1])
          mean((ah == 2L) * ef)
        })
        reps_c <- reps_c[!is.na(reps_c)]
        se_c <- stats::sd(reps_c)
        reps_t <- replicate(R, {
          idx <- sample.int(N, N, TRUE)
          ah <- tryCatch(predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
          if (is.null(ah)) return(NA_real_)
          if (is.matrix(ah)) ah <- ah[, 1]
          if (identical(scope, "branch")) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          } else if (length(nodes) >= 3 && d >= 2L) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            v2 <- pol$columns[nodes[[2]]$split_variable]; t2 <- nodes[[2]]$split_value
            v3 <- pol$columns[nodes[[3]]$split_variable]; t3 <- nodes[[3]]$split_value
            gid_b <- integer(N)
            gid_b[lft] <- ifelse(Xk[idx, v2, drop = FALSE][lft, 1] <= t2, 1L, 2L)
            gid_b[!lft] <- ifelse(Xk[idx, v3, drop = FALSE][!lft, 1] <= t3, 3L, 4L)
          } else {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          }
          ah <- as.integer(ah)
          ah[!(gid_b %in% keep_groups)] <- 1L
          ef <- as.numeric(drk[idx, 2] - drk[idx, 1])
          mean((ah == 1L) * (-ef))
        })
        reps_t <- reps_t[!is.na(reps_t)]
        se_t <- stats::sd(reps_t)
        # treated-only uplift via bootstrap
        reps_u <- replicate(R, {
          idx <- sample.int(N, N, TRUE)
          ah <- tryCatch(predict(pol, Xk[idx, , drop = FALSE]), error = function(e) NULL)
          if (is.null(ah)) return(NA_real_)
          if (is.matrix(ah)) ah <- ah[, 1]
          if (identical(scope, "branch")) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          } else if (length(nodes) >= 3 && d >= 2L) {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            v2 <- pol$columns[nodes[[2]]$split_variable]; t2 <- nodes[[2]]$split_value
            v3 <- pol$columns[nodes[[3]]$split_variable]; t3 <- nodes[[3]]$split_value
            gid_b <- integer(N)
            gid_b[lft] <- ifelse(Xk[idx, v2, drop = FALSE][lft, 1] <= t2, 1L, 2L)
            gid_b[!lft] <- ifelse(Xk[idx, v3, drop = FALSE][!lft, 1] <= t3, 3L, 4L)
          } else {
            lft <- Xk[idx, var1, drop = FALSE][, 1] <= thr1
            gid_b <- integer(N); gid_b[lft] <- 1L; gid_b[!lft] <- 2L
          }
          ah <- as.integer(ah)
          ah[!(gid_b %in% keep_groups)] <- 1L
          tm <- ah == 2L
          if (any(tm)) mean((drk[idx, 2] - drk[idx, 1])[tm]) else NA_real_
        })
        reps_u <- reps_u[!is.na(reps_u)]
        se_u <- if (length(reps_u) > 1) stats::sd(reps_u) else NA_real_
        list(
          pv_ctrl = pv_ctrl, se_ctrl = se_c, lo_ctrl = pv_ctrl - z * se_c, hi_ctrl = pv_ctrl + z * se_c,
          pv_treat = pv_treat, se_treat = se_t, lo_treat = pv_treat - z * se_t, hi_treat = pv_treat + z * se_t,
          coverage = cov, uplift = upl,
          lo_u = if (is.na(se_u)) NA_real_ else upl - z * se_u,
          hi_u = if (is.na(se_u)) NA_real_ else upl + z * se_u,
          n_eval = N
        )
      }
    }

    for (mn in unique(mm$model)) {
      sb <- split_breakdown[[mn]]
      if (is.null(sb) || !nrow(sb)) next
      contrib <- sb$contrib_pv_control_all
      abs_sum <- sum(abs(contrib), na.rm = TRUE)
      if (abs_sum == 0 || all(is.na(contrib))) next
      shares <- sign(contrib) * abs(contrib) / abs_sum * 100
      best <- which.max(contrib)
      favorable <- length(best) == 1 && !is.na(contrib[best]) && contrib[best] > 0
      if (isTRUE(strict_branch) && favorable) {
        if (!is.null(sb$uplift_treated_lo) && !is.null(sb$uplift_treated_hi)) {
          if (is.na(sb$uplift_treated_lo[best]) || sb$uplift_treated_lo[best] <= 0) favorable <- FALSE
        } else {
          favorable <- FALSE
        }
      }
      sel_groups <- if (favorable) best else integer(0)
      res_restr <- if (length(sel_groups)) .eval_restricted(mn, sel_groups, restricted_scope) else NULL
      # full policy metrics
      d <- model_depths[[mn]]
      full_ctrl <- rep[rep$model == mn & rep$contrast == "policy - control_all" & rep$depth == d, , drop = FALSE]
      full_pv <- if (nrow(full_ctrl)) full_ctrl$estimate[1] else NA_real_
      full_lo <- if (nrow(full_ctrl)) full_ctrl$ci_lo[1] else NA_real_
      full_hi <- if (nrow(full_ctrl)) full_ctrl$ci_hi[1] else NA_real_
      decision <- "caution"
      reason <- ""
      if (!is.null(res_restr)) {
        if (res_restr$pv_ctrl > max(0, full_pv) && res_restr$lo_ctrl > 0) {
          decision <- "deploy_restricted"
          reason <- "restricted PV(control-all) > 0 with 95% CI > 0 and ≥ full"
        } else if (full_lo > 0) {
          decision <- "deploy_full"
          reason <- "full PV(control-all) 95% CI > 0"
        } else if (res_restr$pv_ctrl > 0 && res_restr$pv_ctrl > full_pv) {
          decision <- "caution"
          reason <- "restricted PV improved but CI crosses 0"
        } else if (full_pv <= 0 && res_restr$pv_ctrl <= 0) {
          decision <- "do_not_deploy"
          reason <- "both full and restricted PV ≤ 0"
        }
      } else {
        if (full_lo > 0) {
          decision <- "deploy_full"
          reason <- "full PV(control-all) 95% CI > 0"
        } else if (!favorable || (length(best) && abs(shares[best]) * 0.01 < dominance_threshold)) {
          decision <- "do_not_deploy"
          reason <- "no favorable dominant split"
        }
      }
      # build text
      lab <- sb$label[best]
      # Build nuanced per-model summary using split stats
      fmt_ci2 <- function(lo, hi) if (is.na(lo) || is.na(hi)) "" else sprintf(" [%.3f, %.3f]", lo, hi)
      # order branches by contribution
      ord <- order(contrib, decreasing = TRUE)
      dom <- ord[1]; oth <- ord[2]
      mk_branch_line <- function(idx) {
        if (is.na(idx) || idx < 1 || idx > nrow(sb)) return("")
        sprintf("%s — Share %.1f%%, Treated %.1f%%, Uplift_T %.3f%s, PVc %.1f%%",
                sb$label[idx], 100*sb$share[idx], 100*sb$treated_within[idx],
                sb$uplift_treated[idx], fmt_ci2(sb$uplift_treated_lo[idx], sb$uplift_treated_hi[idx]), shares[idx])
      }
      dom_line <- mk_branch_line(dom)
      oth_line <- mk_branch_line(oth)
      # Restricted policy detail if available
      restr_line <- if (!is.null(res_restr)) sprintf("Restricted PV=%.3f%s, Coverage=%.1f%% (Δ vs full = %.3f)",
                          res_restr$pv_ctrl, fmt_ci2(res_restr$lo_ctrl, res_restr$hi_ctrl), 100*res_restr$coverage, res_restr$pv_ctrl - full_pv) else ""
      head_line <- switch(decision,
        deploy_restricted = sprintf("- %s: Recommend restricted policy (treat only in %s). PV=%.3f%s, Coverage=%.1f%%. Full PV=%.3f%s.",
                                    gsub("^model_", "", mn), lab, res_restr$pv_ctrl, fmt_ci2(res_restr$lo_ctrl, res_restr$hi_ctrl), 100*res_restr$coverage,
                                    full_pv, fmt_ci2(full_lo, full_hi)),
        deploy_full = sprintf("- %s: Recommend deploying full policy. PV=%.3f%s.",
                               gsub("^model_", "", mn), full_pv, fmt_ci2(full_lo, full_hi)),
        do_not_deploy = sprintf("- %s: Do not deploy (no favorable dominant split / PV ≤ 0). Full PV=%.3f%s.",
                                 gsub("^model_", "", mn), full_pv, fmt_ci2(full_lo, full_hi)),
        sprintf("- %s: Caution. Restricted policy improves PV but uncertain.", gsub("^model_", "", mn))
      )
      rec_line <- paste(
        head_line,
        if (nzchar(dom_line)) paste0("  • Dominant: ", dom_line) else "",
        if (nzchar(oth_line)) paste0("  • Other: ", oth_line) else "",
        if (nzchar(restr_line)) paste0("  • ", restr_line) else "",
        sep = "\n"
      )
      recommendations_text <- paste(recommendations_text, rec_line, sep = if (nzchar(recommendations_text)) "\n" else "")
      recommendations_by_model[[mn]] <- list(
        decision = decision,
        reason = reason,
        selected_label = if (length(best)) lab else NA_character_,
        dominance_pct = if (length(best) && abs_sum > 0) round(100 * abs(contrib[best]) / abs_sum, 1) else NA_real_,
        full = list(pv = full_pv, lo = full_lo, hi = full_hi),
        restricted = res_restr
      )
    }
  }

  # Build compact split tables (per-model and combined) if requested
  split_table_compact <- list()
  split_table_compact_df <- NULL
  split_table_compact_md <- ""
  split_table_compact_md_combined <- ""
  if (isTRUE(split_compact) && length(split_breakdown)) {
    fmt_eff_ci <- function(est, lo, hi, digits, show_ci) {
      if (is.na(est)) return("—")
      val <- round(est, digits)
      if (isTRUE(show_ci) && !is.na(lo) && !is.na(hi)) {
        paste0(val, " ", fmt_ci(lo, hi))
      } else {
        as.character(val)
      }
    }
    # Compact a single model table
    compact_one <- function(sb) {
      if (is.null(sb) || !nrow(sb)) return(NULL)
      # denominator for contributions
      pv_ctrl <- sb$contrib_pv_control_all
      pv_treat <- sb$contrib_pv_treat_all
      denom_rel_abs <- sum(abs(pv_ctrl), na.rm = TRUE)
      denom_rel_signed <- sum(pv_ctrl, na.rm = TRUE)
      # thresholds
      thr <- min_action_pct / 100
      idx <- seq_len(nrow(sb))
      pv_c_val <- pv_ctrl
      pv_t_val <- pv_treat
      if (isTRUE(split_drop_zero) && nrow(sb)) {
        keep <- !(abs(ifelse(is.na(pv_c_val), 0, pv_c_val)) < 1e-10 &
                    abs(ifelse(is.na(pv_t_val), 0, pv_t_val)) < 1e-10)
        if (any(keep)) {
          idx <- idx[keep]
        }
      }
      sb <- sb[idx, , drop = FALSE]
      if (!nrow(sb)) return(NULL)
      pv_c_val <- pv_ctrl[idx]
      pv_t_val <- pv_treat[idx]
      treated_pct <- 100 * sb$treated_within
      control_pct <- 100 * sb$control_within
      # compute PVc column depending on scale
      if (contrib_scale == "rel_abs") {
        pv_c_val <- if (denom_rel_abs > 0) 100 * sign(pv_ctrl) * abs(pv_ctrl) / denom_rel_abs else rep(NA_real_, length(pv_ctrl))
        pv_t_val <- if (denom_rel_abs > 0) 100 * sign(pv_treat) * abs(pv_treat) / denom_rel_abs else rep(NA_real_, length(pv_treat))
        pv_c_val <- pv_c_val[idx]
        pv_t_val <- pv_t_val[idx]
        pv_c_label <- "PVc %"
        pv_t_label <- "PVT %"
      } else if (contrib_scale == "rel_signed") {
        pv_c_val <- if (abs(denom_rel_signed) > 0) 100 * pv_ctrl / denom_rel_signed else rep(NA_real_, length(pv_ctrl))
        pv_t_val <- if (abs(denom_rel_signed) > 0) 100 * pv_treat / denom_rel_signed else rep(NA_real_, length(pv_treat))
        pv_c_val <- pv_c_val[idx]
        pv_t_val <- pv_t_val[idx]
        pv_c_label <- "PVc %"
        pv_t_label <- "PVT %"
      } else {
        # already subset above
        pv_c_label <- "PVc"
        pv_t_label <- "PVT"
      }
      # Construct data frame
      out <- data.frame(
        Split = gsub("^baseline ", "", sb$label),
        `Share %` = round(100 * sb$share, 1),
        `Treated %` = round(treated_pct, 1),
        check.names = FALSE
      )
      # Uplift treated and avoided harm for control
      ut_show <- sb$treated_within >= thr & !is.na(sb$uplift_treated)
      uc_show <- sb$control_within >= thr & !is.na(sb$uplift_control)
      ut_col <- vapply(seq_len(nrow(sb)), function(i) {
        if (!ut_show[i]) return("—")
        fmt_eff_ci(sb$uplift_treated[i], sb$uplift_treated_lo[i], sb$uplift_treated_hi[i], digits, show_ci_compact)
      }, character(1))
      uc_col <- vapply(seq_len(nrow(sb)), function(i) {
        if (!uc_show[i]) return("—")
        fmt_eff_ci(sb$uplift_control[i], sb$uplift_control_lo[i], sb$uplift_control_hi[i], digits, show_ci_compact)
      }, character(1))
      out$`Uplift_T [CI]` <- ut_col
      out$`AvoidedHarm_C [CI]` <- uc_col
      out[[pv_c_label]] <- ifelse(is.na(pv_c_val), NA, round(pv_c_val, 1))
      if (isTRUE(show_pvt)) {
        out[[pv_t_label]] <- ifelse(is.na(pv_t_val), NA, round(pv_t_val, 1))
      }
      if (isTRUE(split_top_only) && nrow(out) > 1) {
        metric <- abs(ifelse(is.na(out[[pv_c_label]]), 0, out[[pv_c_label]]))
        top <- which.max(metric)
        out <- out[top, , drop = FALSE]
      }
      # order by absolute PVc (or absolute value) descending for visibility
      ord <- order(abs(ifelse(is.na(pv_c_val), 0, pv_c_val)), decreasing = TRUE)
      out <- out[ord, , drop = FALSE]
      rownames(out) <- NULL
      out
    }

    # Build per-model compact tables
    for (mn in names(split_breakdown)) {
      sb <- split_breakdown[[mn]]
      ct <- compact_one(sb)
      if (!is.null(ct)) split_table_compact[[mn]] <- ct
    }
    # Combined DF with Outcome column
    if (length(split_table_compact)) {
      dfs <- list()
      for (mn in names(split_table_compact)) {
        lab <- tryCatch({
          out_name <- gsub("^model_", "", mn)
          .apply_label_stability(out_name, label_mapping)
        }, error = function(e) gsub("^model_", "", mn))
        df <- split_table_compact[[mn]]
        df$Outcome <- lab
        # move Outcome to first col
        df <- df[, c("Outcome", setdiff(colnames(df), "Outcome")), drop = FALSE]
        dfs[[length(dfs) + 1L]] <- df
      }
      split_table_compact_df <- do.call(rbind, dfs)
      rownames(split_table_compact_df) <- NULL
      # Markdown rendering (group by outcome)
      parts <- list()
      for (mn in names(split_table_compact)) {
        lab <- tryCatch({
          out_name <- gsub("^model_", "", mn)
          .apply_label_stability(out_name, label_mapping)
        }, error = function(e) gsub("^model_", "", mn))
        tbl <- split_table_compact[[mn]]
        md <- tryCatch(knitr::kable(tbl, format = "markdown"), error = function(e) paste(capture.output(print(tbl)), collapse = "\n"))
        parts[[length(parts) + 1L]] <- paste0("#### By Split (Compact) — ", lab, "\n\n", md, "\n")
      }
      split_table_compact_md <- paste(unlist(parts), collapse = "\n")
      # Build a single combined markdown table as well
      split_table_compact_md_combined <- tryCatch({
        knitr::kable(split_table_compact_df, format = "markdown")
      }, error = function(e) {
        paste(capture.output(print(split_table_compact_df)), collapse = "\n")
      })
    } else {
      split_table_compact_df <- data.frame()
      split_table_compact_md <- ""
      split_table_compact_md_combined <- ""
    }
  }

  # helper to extract readable names for model lists
  resolve_outcome_labels <- function(df) {
    if (!nrow(df)) return(character())
    vapply(seq_len(nrow(df)), function(idx) {
      outcome <- df$outcome[idx]
      fallback <- df$outcome_label[idx]
      if (!is.null(label_mapping)) {
        .apply_label_stability(outcome, label_mapping)
      } else {
        fallback %||% outcome
      }
    }, character(1))
  }

  format_report_entry <- function(df_row) {
    if (!nrow(df_row)) return("")
    label <- resolve_outcome_labels(df_row)[1]
    pieces <- character()
    est_ctrl <- df_row[["estimate_ctrl"]]
    if (!is.na(est_ctrl)) {
      pieces <- c(pieces, paste0(
        "policy vs control-all ",
        round(est_ctrl, digits), " ",
        fmt_ci(df_row[["ci_lo_ctrl"]], df_row[["ci_hi_ctrl"]]),
        convert_original(est_ctrl, df_row[["model"]])
      ))
    }
    est_treat <- df_row[["estimate_treat"]]
    if (!is.na(est_treat)) {
      pieces <- c(pieces, paste0(
        "policy vs treat-all ",
        round(est_treat, digits), " ",
        fmt_ci(df_row[["ci_lo_treat"]], df_row[["ci_hi_treat"]]),
        convert_original(est_treat, df_row[["model"]])
      ))
    }
    uplift_val <- df_row[["uplift"]]
    if (!is.na(uplift_val)) {
      uplift_piece <- paste0(
        "avg uplift (treated) ",
        round(uplift_val, digits)
      )
      lo <- df_row[["uplift_lo"]]
      hi <- df_row[["uplift_hi"]]
      if (!is.na(lo) && !is.na(hi)) {
        uplift_piece <- paste0(uplift_piece, " ", fmt_ci(lo, hi))
      }
      uplift_piece <- paste0(uplift_piece, convert_original(uplift_val, df_row[["model"]]))
      pieces <- c(pieces, uplift_piece)
    }
    cov_val <- df_row[["coverage"]]
    if (!is.na(cov_val)) {
      pieces <- c(pieces, paste0("coverage ", round(100 * cov_val, 1), "%"))
    }
    paste0(label, " (", paste(pieces, collapse = "; "), ")")
  }

  wins_model_ids <- wins$model
  wins_model_names <- resolve_outcome_labels(wins)
  neutral_model_ids <- neutral$model
  neutral_model_names <- resolve_outcome_labels(neutral)
  caution_model_ids <- harm$model
  caution_model_names <- resolve_outcome_labels(harm)

  format_name_list <- function(x) {
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (!length(x)) return("none")
    if (length(x) == 1) return(x)
    if (length(x) == 2) return(paste(x, collapse = " and "))
    paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
  }

  wins_sentence <- if (length(wins_model_names)) {
    paste0(
      "Policy value gains vs control-all (wins): ",
      format_name_list(wins_model_names),
      "."
    )
  } else {
    "No models delivered a policy value gain vs control-all (wins)."
  }

  neutral_sentence <- if (length(neutral_model_names)) {
    paste0(
      "Policy value confidence intervals crossed zero for: ",
      format_name_list(neutral_model_names),
      "."
    )
  } else {
    "No models had policy value confidence intervals that crossed zero."
  }

  caution_sentence <- if (length(caution_model_names)) {
    paste0(
      "Policy deployment would underperform control-all for: ",
      format_name_list(caution_model_names),
      "."
    )
  } else {
    "No models showed policy value estimates entirely below zero (caution)."
  }

  interpretation_text <- paste(wins_sentence, neutral_sentence, caution_sentence)

  depth_pref_sentence <- if (length(unique_depths) == 1) {
    if (unique_depths == 1L) {
      "All evaluated models used depth-1 policies."
    } else {
      "All evaluated models used depth-2 policies."
    }
  } else {
    depth1_list <- format_name_list(depth1_model_names)
    depth2_list <- format_name_list(depth2_model_names)
    paste0(
      "Depth preference: 1-level policies performed best for ",
      depth1_list,
      "; 2-level policies performed best for ",
      depth2_list,
      "."
    )
  }

  interpretation_text <- paste(interpretation_text, depth_pref_sentence)

  build_group_block <- function(header, df, empty_msg) {
    heading <- paste0("### ", header)
    if (!nrow(df)) {
      return(paste(c(heading, "", empty_msg, ""), collapse = "\n"))
    }
    entries <- vapply(
      seq_len(nrow(df)),
      function(idx) paste0("- ", format_report_entry(df[idx, , drop = FALSE])),
      character(1)
    )
    paste(c(heading, "", entries, ""), collapse = "\n")
  }

  report_sections <- list()
  if (isTRUE(include_explanation)) {
    report_sections <- c(report_sections, list(policy_expl, ""))
  }

  depth_sentence <- if (length(unique_depths) == 1) {
    paste0(
      "Consensus policy evaluation at depth ", unique_depths,
      " compared targeted deployment against control-all and treat-all baselines."
    )
  } else {
    paste0(
      "Consensus policy evaluation (mixed depths per model) compared targeted deployment against control-all and treat-all baselines."
    )
  }

  report_sections <- c(report_sections, list(
    depth_sentence,
    "",
    build_group_block(
      "Wins (policy value vs control-all 95% CI entirely > 0)",
      wins,
      "No wins: no models delivered a statistically significant policy value gain vs control-all (95% CI > 0)."
    ),
    build_group_block(
      "Neutral (policy value vs control-all CI spans zero)",
      neutral,
      "No neutral models: every evaluable policy was classified as win or caution."
    ),
    build_group_block(
      "Caution (policy value vs control-all 95% CI entirely < 0)",
      harm,
      "No caution models: no policy value estimates fell entirely below zero."
    )
  ))

  report_text <- paste(unlist(report_sections), collapse = "\n")

  # build grouped text
  for (gname in names(groups)) {
    gdf <- groups[[gname]]
    if (!nrow(gdf)) next
    lines <- c(lines, paste0("\n#### ", gname, "\n"))
    for (ii in seq_len(nrow(gdf))) {
      row <- gdf[ii, ]
      mn <- row$model
      out <- row$outcome
      out_label <- if (!is.null(label_mapping)) .apply_label_stability(out, label_mapping) else row$outcome_label %||% out

      pv_ctrl_text <- paste0(round(row$estimate_ctrl, digits), " ", fmt_ci(row$ci_lo_ctrl, row$ci_hi_ctrl), convert_original(row$estimate_ctrl, mn))
      pv_treat_text <- if (!is.na(row$estimate_treat)) paste0(round(row$estimate_treat, digits), " ", fmt_ci(row$ci_lo_treat, row$ci_hi_treat), convert_original(row$estimate_treat, mn)) else "NA"
      uplift_text <- if (!is.na(row$uplift)) paste0(round(row$uplift, digits), if (!is.na(row$uplift_lo) & !is.na(row$uplift_hi)) paste0(" ", fmt_ci(row$uplift_lo, row$uplift_hi)) else "", convert_original(row$uplift, mn)) else "NA"
      coverage_text <- if (!is.na(row$coverage)) paste0(round(100 * row$coverage, 1), "%") else "NA"

      # optional stability highlight
      stab_text <- ""
      if (isTRUE(include_stability)) {
        sm <- object$results[[mn]]$stability_metrics
        sel_d <- model_depths[[mn]]
        if (!is.null(sm)) {
          strength <- suppressWarnings(tryCatch(
            if (sel_d == 2L && !is.null(sm$consensus_strength$depth_2)) {
              sm$consensus_strength$depth_2
            } else {
              sm$consensus_strength$depth_1
            }, error = function(e) NA_real_))
          vf <- suppressWarnings(tryCatch(sm$var_inclusion_freq, error = function(e) NULL))
          topv_label <- NULL
          topf <- NA_real_
          if (!is.null(vf)) {
            if (sel_d == 1L && "depth_1_freq" %in% names(vf)) {
              ord <- order(vf$depth_1_freq, decreasing = TRUE)
              topv <- vf$variable[ord][1]
              topf <- vf$depth_1_freq[ord][1]
              topv_label <- .apply_label_stability(topv, label_mapping)
            } else if (sel_d == 2L && all(c("depth_2_node1_freq", "depth_2_node2_freq") %in% names(vf))) {
              freq <- apply(cbind(vf$depth_2_node1_freq, vf$depth_2_node2_freq), 1, function(x) {
                if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
              })
              ord <- order(freq, decreasing = TRUE)
              topv <- vf$variable[ord][1]
              topf <- freq[ord][1]
              topv_label <- .apply_label_stability(topv, label_mapping)
            }
          }
          if (!is.na(strength)) {
            stab_text <- paste0("; stability: depth-", sel_d, " consensus ", round(strength, 2))
            if (!is.null(topv_label) && !is.na(topf)) {
              stab_text <- paste0(stab_text, ", top split = ", topv_label, " (", round(100 * topf, 1), "%)")
            }
          } else if (!is.null(topv_label) && !is.na(topf)) {
            stab_text <- paste0("; dominant split = ", topv_label, " (", round(100 * topf, 1), "%)")
          }
        }
      }

      depth_note <- if (length(unique_depths) > 1 || model_depths[[mn]] != depth) paste0(" [depth ", model_depths[[mn]], "]") else ""

      if (isTRUE(compact)) {
        # compact line: focus on control-all PV + treated-only uplift + coverage
        lines <- c(lines, paste0(
          "- ", out_label, depth_note, ": ",
          "PV(control-all) = ", pv_ctrl_text, "; ",
          "Uplift(treated) = ", uplift_text, "; ",
          "Coverage = ", coverage_text
        ))
      } else {
        lines <- c(lines, paste0(
          "- ", out_label, depth_note, ": ",
          "policy vs control-all = ", pv_ctrl_text, "; ",
          "policy vs treat-all = ", pv_treat_text, "; ",
          "avg uplift (treated) = ", uplift_text, ", coverage = ", coverage_text, stab_text
        ))
        # Optional: append a compact first-split breakdown that remains coherent
        if (include_split_breakdown != "none") {
          sb <- split_breakdown[[mn]]
          if (!is.null(sb) && nrow(sb) > 0) {
            # dominance check for PV vs control-all
            total_ctrl_all <- sum(sb$contrib_pv_control_all, na.rm = TRUE)
            dom_msg <- ""
            if (!is.na(total_ctrl_all) && abs(total_ctrl_all) > 0) {
              shares <- abs(sb$contrib_pv_control_all) / sum(abs(sb$contrib_pv_control_all), na.rm = TRUE)
              if (length(shares) && max(shares, na.rm = TRUE) >= 0.6) {
                top_idx <- which.max(shares)
                dom_lab <- sb$label[top_idx]
                dom_msg <- paste0("; dominance: ", round(100 * shares[top_idx], 1), "% of PV(control-all) from ", dom_lab)
              }
            }
            # build compact lines per group
            for (jj in seq_len(nrow(sb))) {
              row_sb <- sb[jj, ]
              t_pct <- if (!is.na(row_sb$treated_within)) paste0(round(100 * row_sb$treated_within, 1), "% treated") else ""
              c_pct <- if (!is.na(row_sb$control_within)) paste0(round(100 * row_sb$control_within, 1), "% control") else ""
              parts <- c()
              parts <- c(parts, paste0(row_sb$group, ": ", row_sb$label))
              if (nzchar(t_pct) || nzchar(c_pct)) parts <- c(parts, paste0("mix ", paste(Filter(nzchar, c(t_pct, c_pct)), collapse = ", ")))
              if (!is.na(row_sb$uplift_treated)) {
                ut <- paste0(round(row_sb$uplift_treated, digits))
                if (!is.na(row_sb$uplift_treated_lo) && !is.na(row_sb$uplift_treated_hi)) ut <- paste0(ut, " ", fmt_ci(row_sb$uplift_treated_lo, row_sb$uplift_treated_hi))
                parts <- c(parts, paste0("uplift(treated) ", ut))
              }
              if (!is.na(row_sb$uplift_control)) {
                uc <- paste0(round(row_sb$uplift_control, digits))
                if (!is.na(row_sb$uplift_control_lo) && !is.na(row_sb$uplift_control_hi)) uc <- paste0(uc, " ", fmt_ci(row_sb$uplift_control_lo, row_sb$uplift_control_hi))
                parts <- c(parts, paste0("uplift(control) ", uc))
              }
              parts <- c(parts, paste0("PV(ctrl-all) contrib ", round(row_sb$contrib_pv_control_all, digits)))
              parts <- c(parts, paste0("PV(treat-all) contrib ", round(row_sb$contrib_pv_treat_all, digits)))
              lines <- c(lines, paste0("  • By split: ", paste(parts, collapse = "; ")))
            }
            if (nzchar(dom_msg)) {
              lines <- c(lines, paste0("  • ", dom_msg))
            }
          }
        }
      }
    }
  }

  # Method text (overview and per-model)
  method_overview <- paste0(
    "We evaluate the consensus policy on the held-out test set used for the policy tree evaluation. ",
    "For each unit, we obtain conditional means (μ0, μ1) via double-robust scores and predict the policy action a(x). ",
    "Policy value contrasts are computed as sample averages of per-unit contributions: ",
    "PV(control-all) = mean(1{a=Treat} × (μ1−μ0)), PV(treat-all) = mean(1{a=Control} × (μ0−μ1)). ",
    "Coverage = mean(1{a=Treat}) and uplift among treated = mean(μ1−μ0 | a=Treat), satisfying ",
    "PV(control-all) = Coverage × Uplift_Treated. Split contributions decompose these means over the first split (or leaves) and sum exactly to the overall values. ",
    sprintf("Standard errors use %s (R=%d for bootstrap).", se_method, ifelse(se_method == "bootstrap", R, NA_integer_))
  )

  make_method_blurb <- function(row_ctrl, sb) {
    if (is.null(row_ctrl) || !nrow(row_ctrl)) return("")
    n_eval <- row_ctrl$n_eval[1]
    cov <- row_ctrl$coverage_treated[1]
    upl <- row_ctrl$avg_uplift_treated[1]
    pv <- row_ctrl$estimate[1]
    se <- row_ctrl$std_err[1]
    ci <- paste0("[", round(row_ctrl$ci_lo[1], digits), ", ", round(row_ctrl$ci_hi[1], digits), "]")
    contr_ok <- if (!is.null(sb) && nrow(sb)) {
      s <- sum(sb$contrib_pv_control_all, na.rm = TRUE)
      paste0("Split contributions sum to ", round(s, digits), ". ")
    } else ""
    paste0(
      sprintf("Evaluation N=%d. Coverage=%.1f%%. Uplift_Treated=%s. PV(control-all)=%s %s. ",
              n_eval, 100*cov, round(upl, digits), round(pv, digits), ci),
      contr_ok,
      sprintf("SE method=%s.", se_method)
    )
  }

  method_by_model <- list()
  for (mn in unique(mm$model)) {
    row_ctrl <- rep[rep$model == mn & rep$contrast == "policy - control_all" & rep$depth == model_depths[[mn]], , drop = FALSE]
    method_by_model[[mn]] <- make_method_blurb(row_ctrl, split_breakdown[[mn]])
  }

  # Practical takeaways block (summarize recommendations and dominance)
  if (isTRUE(auto_recommend) && length(recommendations_by_model)) {
    decs <- vapply(recommendations_by_model, function(x) x$decision, character(1))
    n_full <- sum(decs == "deploy_full", na.rm = TRUE)
    n_restr <- sum(decs == "deploy_restricted", na.rm = TRUE)
    n_caut <- sum(decs == "caution", na.rm = TRUE)
    n_no <- sum(decs == "do_not_deploy", na.rm = TRUE)
    auto_decision_counts <- c(full = n_full, restricted = n_restr, caution = n_caut, do_not_deploy = n_no)
    decision_line <- sprintf("• Decisions — Full: %d, Restricted: %d, Caution: %d, Do not deploy: %d.", n_full, n_restr, n_caut, n_no)

    dom_ok <- vapply(
      recommendations_by_model,
      function(x) !is.null(x$dominance_pct) && !is.na(x$dominance_pct) && x$dominance_pct >= dominance_threshold * 100,
      logical(1)
    )
    dom_lines <- character()
    if (any(dom_ok)) {
      for (nm in names(recommendations_by_model)[dom_ok]) {
        r <- recommendations_by_model[[nm]]
        dom_lines <- c(dom_lines, sprintf("%s: %s (%.1f%% of PVc)", gsub("^model_", "", nm), r$selected_label, r$dominance_pct))
      }
    }
    dominance_block <- if (length(dom_lines)) {
      paste0("• Dominant positive split(s):\n  - ", paste(dom_lines, collapse = "\n  - "))
    } else {
      "• No strong single dominant positive split across models."
    }
    uncertainty_line <- "• Uncertainty: Most branch/leaf uplift CIs cross zero; treat as directional. Depth-1 policies may stabilize at a cost of nuance."
    risk_line <- "• Harm flags: If dominant contribution is negative or full PV ≤ 0, avoid deployment or restrict to favorable branches and re-evaluate."
  }

  if (length(recommendations_by_model)) {
    recommended_model_ids <- names(Filter(function(x) {
      is.list(x) && !is.null(x$decision) && x$decision %in% c("deploy_full", "deploy_restricted")
    }, recommendations_by_model))
    if (length(recommended_model_ids)) {
      recommended_model_names <- vapply(recommended_model_ids, function(mn) {
        out_name <- gsub("^model_", "", mn)
        .apply_label_stability(out_name, label_mapping)
      }, character(1))
    }
  }

  recommended_depth1_model_ids <- if (length(recommended_model_ids)) recommended_model_ids[model_depths[recommended_model_ids] == 1L] else character()
  recommended_depth2_model_ids <- if (length(recommended_model_ids)) recommended_model_ids[model_depths[recommended_model_ids] == 2L] else character()
  recommended_depth1_model_names <- if (length(recommended_depth1_model_ids)) vapply(recommended_depth1_model_ids, function(mn) .apply_label_stability(gsub("^model_", "", mn), label_mapping), character(1)) else character()
  recommended_depth2_model_names <- if (length(recommended_depth2_model_ids)) vapply(recommended_depth2_model_ids, function(mn) .apply_label_stability(gsub("^model_", "", mn), label_mapping), character(1)) else character()

  depth_summary_line <- paste0(
    "• Depth allocation — depth 1: ", length(depth1_model_ids), " model(s); depth 2: ", length(depth2_model_ids), " model(s)."
  )
  depth_pref_line <- paste0(
    "• Depth preference — 1-level: ", format_name_list(depth1_model_names),
    "; 2-level: ", format_name_list(depth2_model_names), "."
  )

  practical_lines <- character()
  if (!is.null(auto_decision_counts)) {
    if (!is.null(decision_line)) practical_lines <- c(practical_lines, decision_line)
    if (!is.null(dominance_block)) practical_lines <- c(practical_lines, dominance_block)
    if (!is.null(uncertainty_line)) practical_lines <- c(practical_lines, uncertainty_line)
    if (!is.null(risk_line)) practical_lines <- c(practical_lines, risk_line)
    if (length(recommended_model_ids)) {
      deployment_details <- paste(
        sprintf(
          "%s (depth %s)",
          recommended_model_names,
          model_depths[recommended_model_ids]
        ),
        collapse = ", "
      )
      practical_lines <- c(practical_lines, paste0("• Recommended for deployment: ", deployment_details, "."))
    } else {
      practical_lines <- c(practical_lines, "• Recommended for deployment: none.")
    }
    if (length(recommended_depth1_model_names)) {
      practical_lines <- c(
        practical_lines,
        paste0("• Recommended depth-1 models: ", paste(recommended_depth1_model_names, collapse = ", "), ".")
      )
    }
    if (length(recommended_depth2_model_names)) {
      practical_lines <- c(
        practical_lines,
        paste0("• Recommended depth-2 models: ", paste(recommended_depth2_model_names, collapse = ", "), ".")
      )
    }
  }
  practical_lines <- c(practical_lines, depth_summary_line, depth_pref_line)
  practical_takeaways_text <- paste(practical_lines[nzchar(practical_lines)], collapse = "\n")

  if (length(unique_depths) == 1) {
    header <- paste0("### Policy Summary (depth ", unique_depths, ")\n\n")
  } else {
    header <- "### Policy Summary (mixed depths)\n\n"
  }
  expl <- if (isTRUE(include_explanation)) paste0("\n\n", policy_expl, "\n\n") else "\n"
  text <- paste0(header, paste(lines, collapse = "\n"), expl)
  if (isTRUE(auto_recommend) && nzchar(recommendations_text)) {
    text <- paste0(text, "\n### Recommendations\n\n", recommendations_text, "\n")
  }
  if (nzchar(practical_takeaways_text)) {
    text <- paste0(text, "\n### Practical Takeaways\n\n", practical_takeaways_text, "\n")
  }

  # build grouped brief tables matching text sections (as data frames, not markdown)
  make_brief_df <- function(df) {
    if (!nrow(df)) return(NULL)
    lab <- if (!is.null(label_mapping)) vapply(df$outcome, function(x) .apply_label_stability(x, label_mapping), character(1)) else (df$outcome_label %||% df$outcome)
    pv_ctrl <- paste0(round(df$estimate_ctrl, digits), " ", fmt_ci(df$ci_lo_ctrl, df$ci_hi_ctrl))
    uplift <- if (!is.na(df$uplift[1])) {
      u_str <- round(df$uplift, digits)
      if (!all(is.na(df$uplift_lo)) && !all(is.na(df$uplift_hi))) {
        u_str <- paste0(u_str, " ", fmt_ci(df$uplift_lo, df$uplift_hi))
      }
      u_str
    } else rep("NA", nrow(df))
    cov <- paste0(round(100 * df$coverage, 1), "%")
    df_out <- data.frame(
      Outcome = lab,
      `Effect Size (95% CI)` = pv_ctrl,
      `Effect in Treated (95% CI)` = uplift,
      `Coverage (%)` = cov,
      check.names = FALSE
    )
    df_out
  }

  group_tables <- list()
  for (gname in names(groups)) {
    gdf <- groups[[gname]]
    if (!nrow(gdf)) next
    group_tables[[gname]] <- make_brief_df(gdf)
  }

  # also provide a single combined data frame with a group column
  group_table_df <- NULL
  if (length(group_tables)) {
    dfs <- lapply(names(group_tables), function(gn) {
      df <- group_tables[[gn]]
      if (is.null(df) || !nrow(df)) return(NULL)
      df$Group <- gn
      df
    })
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs)) {
      group_table_df <- do.call(rbind, dfs)
      group_table_df <- group_table_df[, c("Group", setdiff(names(group_table_df), "Group")), drop = FALSE]
      rownames(group_table_df) <- NULL
    } else {
      group_table_df <- data.frame()
    }
  } else {
    group_table_df <- data.frame()
  }

  list(
    text = text,
    report = report_text,
    interpretation = interpretation_text,
    table_md = table_md,
    table_df = tbl_df,
    split_breakdown = split_breakdown,
    split_table_compact = split_table_compact,
    split_table_compact_df = split_table_compact_df,
    split_table_compact_md = split_table_compact_md,
    split_table_compact_md_combined = split_table_compact_md_combined,
    coherence_audit = coherence_audit,
    method_overview = method_overview,
    method_by_model = method_by_model,
    policy_value_audit = policy_value_audit,
    recommendations_by_model = recommendations_by_model,
    recommendations_text = recommendations_text,
    recommended_model_ids = recommended_model_ids,
    recommended_model_names = recommended_model_names,
    recommended_depth1_model_ids = recommended_depth1_model_ids,
    recommended_depth1_model_names = recommended_depth1_model_names,
    recommended_depth2_model_ids = recommended_depth2_model_ids,
    recommended_depth2_model_names = recommended_depth2_model_names,
    practical_takeaways_text = practical_takeaways_text,
    wins_model_ids = wins_model_ids,
    wins_model_names = wins_model_names,
    neutral_model_ids = neutral_model_ids,
    neutral_model_names = neutral_model_names,
    caution_model_ids = caution_model_ids,
    caution_model_names = caution_model_names,
    group_table = group_tables,
    group_table_df = group_table_df,
    model_depths = model_depths,
    depth_map = model_depths,
    depth1_model_ids = depth1_model_ids,
    depth1_model_names = depth1_model_names,
    depth2_model_ids = depth2_model_ids,
    depth2_model_names = depth2_model_names,
    coherent_policy_values = rep
  )
}
