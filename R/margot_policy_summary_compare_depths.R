#' Compare Depth-1 vs Depth-2 Policy Summaries and Pick Best per Outcome
#'
#' Runs [margot_policy_summary_report()] twice (depth 1 and depth 2) with consistent
#' evaluation settings, then produces a side-by-side comparison of policy values vs
#' control-all and selects the preferred depth per outcome. By default, uses
#' se_method = "plugin" and turns on `auto_recommend` to obtain numeric PV and CIs.
#'
#' @param object A `margot_stability_policy_tree` object with consensus trees for
#'   depth 1 and/or depth 2 (run with `depth = "both"` in `margot_policy_tree_stability()`).
#' @param model_names Optional character vector of outcomes (with or without `model_`).
#' @param original_df Optional original-scale data for scale annotations (passed through).
#' @param se_method Character; "plugin" (default) or "bootstrap"; passed to reporter.
#' @param R Integer; bootstrap replicates when `se_method = "bootstrap"` (default 499).
#' @param seed Integer; RNG seed (default 42).
#' @param label_mapping Optional named list mapping outcome names to display labels.
#' @param include_split_breakdown_1 Character; split view for depth 1 (default "branch").
#' @param include_split_breakdown_2 Character; split view for depth 2 (default "leaf").
#' @param auto_recommend Logical; turn on automated restricted-policy recommendations (default TRUE).
#' @param dominance_threshold Numeric; see [margot_policy_summary_report()].
#' @param strict_branch Logical; see [margot_policy_summary_report()].
#' @param restricted_scope_1 Character; restricted scope for depth 1 (default "branch").
#' @param restricted_scope_2 Character; restricted scope for depth 2 (default "leaf").
#' @param split_compact Logical; pass-through to [margot_policy_summary_report()] controlling
#'   whether compact split tables are generated (default TRUE).
#' @param min_action_pct Numeric; minimum treated/control share (%) required to keep
#'   action-specific uplifts in compact tables (default 5). Lowering this can restore
#'   split rows that would otherwise be suppressed.
#' @param split_drop_zero Logical; drop rows with zero contributions from compact tables (default TRUE).
#' @param split_top_only Logical; keep only the top-contributing split per model when building compact
#'   tables (default FALSE).
#' @param verbose Logical; print progress (default TRUE).
#'
#' @return A list with:
#'   - `depth_map`: named integer vector indicating the preferred depth (1 or 2) per model.
#'   - `depth_summary_df`: tidy per-model summary (preferred depth, policy values, gain vs alternative, decision).
#'   - `depth_table_df` / `depth_table_md`: formatted comparison of depth-1 vs depth-2 policy values.
#'   - `depth_takeaways_text`: concise prose describing which outcomes favour depth-1 vs depth-2.
#'   - `summary_text`: markdown narrative combining depth takeaways with key recommendations.
#'   - `recommendations_df`: deployment recommendations with policy value details by depth.
#'
#' @export
margot_policy_summary_compare_depths <- function(object,
                                                 model_names = NULL,
                                                 original_df = NULL,
                                                 se_method = c("plugin", "bootstrap"),
                                                 R = 499L,
                                                 seed = 42L,
                                                 label_mapping = NULL,
                                                 include_split_breakdown_1 = "branch",
                                                 include_split_breakdown_2 = "leaf",
                                                 auto_recommend = TRUE,
                                                 dominance_threshold = 0.6,
                                                 strict_branch = FALSE,
                                                 restricted_scope_1 = "branch",
                                                 restricted_scope_2 = "leaf",
                                                 split_compact = TRUE,
                                                 min_action_pct = 5,
                                                 split_drop_zero = TRUE,
                                                 split_top_only = FALSE,
                                                 verbose = TRUE) {
  se_method <- match.arg(se_method)

  if (isTRUE(verbose)) cli::cli_alert_info("Comparing depth-1 and depth-2 policy summaries")

  available_models <- names(object$results)
  available_clean <- gsub("^model_", "", available_models)
  name_lookup <- setNames(available_models, available_models)
  name_lookup <- c(name_lookup, setNames(available_models, available_clean))
  name_lookup <- name_lookup[!duplicated(names(name_lookup))]

  resolve_model_ids <- function(x) {
    if (is.null(x) || !length(x)) return(character())
    vals <- as.character(x)
    if (anyNA(vals)) stop("model names cannot contain NA values")
    unknown <- setdiff(unique(vals), names(name_lookup))
    if (length(unknown)) {
      stop("Unknown model name(s): ", paste(unknown, collapse = ", "))
    }
    unname(name_lookup[vals])
  }

  inferred_depths_external <- NULL
  if (!is.null(model_names) && length(model_names) && !is.null(names(model_names))) {
    candidate <- names(model_names)
    depth_vals <- as.character(model_names)
    if (!anyNA(candidate) && !anyNA(depth_vals) &&
        all(candidate %in% names(name_lookup)) &&
        all(depth_vals %in% c("1", "2"))) {
      inferred_depths_external <- setNames(as.integer(depth_vals), resolve_model_ids(candidate))
      model_names <- resolve_model_ids(candidate)
    } else {
      model_names <- resolve_model_ids(model_names)
    }
  } else {
    model_names <- resolve_model_ids(model_names)
  }
  if (!length(model_names)) model_names <- available_models

  d1 <- tryCatch(
    margot_policy_summary_report(
      object,
      model_names = model_names,
      depth = 1L,
      original_df = original_df,
      se_method = se_method,
      R = R,
      seed = seed,
      label_mapping = label_mapping,
      include_split_breakdown = include_split_breakdown_1,
      auto_recommend = auto_recommend,
      dominance_threshold = dominance_threshold,
      strict_branch = strict_branch,
      restricted_scope = restricted_scope_1,
      split_compact = split_compact,
      min_action_pct = min_action_pct,
      split_drop_zero = split_drop_zero,
      split_top_only = split_top_only,
      verbose = verbose
    ), error = function(e) NULL)

  d2 <- tryCatch(
    margot_policy_summary_report(
      object,
      model_names = model_names,
      depth = 2L,
      original_df = original_df,
      se_method = se_method,
      R = R,
      seed = seed,
      label_mapping = label_mapping,
      include_split_breakdown = include_split_breakdown_2,
      auto_recommend = auto_recommend,
      dominance_threshold = dominance_threshold,
      strict_branch = strict_branch,
      restricted_scope = restricted_scope_2,
      split_compact = split_compact,
      min_action_pct = min_action_pct,
      split_drop_zero = split_drop_zero,
      split_top_only = split_top_only,
      verbose = verbose
    ), error = function(e) NULL)

  # pull PVc (full policy) and CI for each depth from coherence_audit (preferred),
  # else from recommendations_by_model; else empty
  get_pv <- function(rep_obj, depth_sel) {
    if (is.null(rep_obj)) return(list(models = character(), pv = NULL, label = character()))
    if (!is.null(rep_obj$coherent_policy_values) && nrow(rep_obj$coherent_policy_values) > 0) {
      cpv <- rep_obj$coherent_policy_values
      sub <- cpv[cpv$contrast == "policy - control_all" & cpv$depth == depth_sel, , drop = FALSE]
      if (nrow(sub) > 0) {
        models <- sub$model
        pv_mat <- cbind(pv = sub$estimate, lo = sub$ci_lo, hi = sub$ci_hi)
        rownames(pv_mat) <- models
        return(list(models = models, pv = pv_mat, label = models))
      }
    }
    if (!is.null(rep_obj$recommendations_by_model) && length(rep_obj$recommendations_by_model)) {
      models <- names(rep_obj$recommendations_by_model)
      pv_mat <- do.call(rbind, lapply(rep_obj$recommendations_by_model, function(x) c(pv = x$full$pv, lo = x$full$lo, hi = x$full$hi)))
      rownames(pv_mat) <- models
      return(list(models = models, pv = pv_mat, label = models))
    }
    list(models = character(), pv = NULL, label = character())
  }

  # Fallback: if per-depth reporter failed (d1/d2 NULL), use raw policy value reporter
  get_pv_from_rep <- function(object, depth_sel) {
    rep <- tryCatch(margot_report_consensus_policy_value(object, depths = depth_sel, R = R, seed = seed, include_treated_only = TRUE, label_mapping = label_mapping, verbose = FALSE), error = function(e) NULL)
    if (is.null(rep) || !nrow(rep)) return(list(models = character(), pv = NULL))
    sub <- rep[rep$contrast == "policy - control_all" & rep$depth == depth_sel, , drop = FALSE]
    pv_mat <- cbind(pv = sub$estimate, lo = sub$ci_lo, hi = sub$ci_hi)
    rownames(pv_mat) <- sub$model
    list(models = sub$model, pv = pv_mat)
  }

  pv1 <- if (!is.null(d1)) get_pv(d1, 1L) else get_pv_from_rep(object, 1L)
  pv2 <- if (!is.null(d2)) get_pv(d2, 2L) else get_pv_from_rep(object, 2L)
  all_models <- sort(unique(c(pv1$models, pv2$models)))

  # Build comparison table
  rows <- list()
  sel <- character()
  for (mn in all_models) {
    lab <- tryCatch({
      out <- gsub("^model_", "", mn)
      .apply_label_stability(out, label_mapping)
    }, error = function(e) gsub("^model_", "", mn))
    v1 <- if (mn %in% pv1$models) pv1$pv[mn, "pv"] else NA_real_
    l1 <- if (mn %in% pv1$models) pv1$pv[mn, "lo"] else NA_real_
    h1 <- if (mn %in% pv1$models) pv1$pv[mn, "hi"] else NA_real_
    v2 <- if (mn %in% pv2$models) pv2$pv[mn, "pv"] else NA_real_
    l2 <- if (mn %in% pv2$models) pv2$pv[mn, "lo"] else NA_real_
    h2 <- if (mn %in% pv2$models) pv2$pv[mn, "hi"] else NA_real_
    # choose best by PV; if tie, by higher lower-CI
    pick <- NA_character_
    if (!is.na(v1) || !is.na(v2)) {
      s1 <- ifelse(is.na(v1), -Inf, v1)
      s2 <- ifelse(is.na(v2), -Inf, v2)
      if (s1 > s2) pick <- "1" else if (s2 > s1) pick <- "2" else {
        # tie-break by CI lower bound
        lb1 <- ifelse(is.na(l1), -Inf, l1)
        lb2 <- ifelse(is.na(l2), -Inf, l2)
        pick <- if (lb1 >= lb2) "1" else "2"
      }
    }
    sel[mn] <- pick
    rows[[length(rows) + 1L]] <- data.frame(
      Model = mn,
      Outcome = lab,
      Depth1_PV = round(v1, 3), Depth1_CI = if (!is.na(l1) && !is.na(h1)) sprintf("[%.3f, %.3f]", l1, h1) else "",
      Depth2_PV = round(v2, 3), Depth2_CI = if (!is.na(l2) && !is.na(h2)) sprintf("[%.3f, %.3f]", l2, h2) else "",
      Selected_Depth = pick,
      stringsAsFactors = FALSE
    )
  }
  compare_df <- do.call(rbind, rows)
  # order by selected depth PV descending
  ord <- order(ifelse(compare_df$Selected_Depth == "1", compare_df$Depth1_PV, compare_df$Depth2_PV), decreasing = TRUE, na.last = TRUE)
  compare_df <- compare_df[ord, , drop = FALSE]
  rownames(compare_df) <- NULL

  compare_df$Depth1_PV_value <- if (length(pv1$models)) {
    vapply(compare_df$Model, function(mn) if (mn %in% pv1$models) pv1$pv[mn, "pv"] else NA_real_, numeric(1))
  } else rep(NA_real_, nrow(compare_df))
  compare_df$Depth2_PV_value <- if (length(pv2$models)) {
    vapply(compare_df$Model, function(mn) if (mn %in% pv2$models) pv2$pv[mn, "pv"] else NA_real_, numeric(1))
  } else rep(NA_real_, nrow(compare_df))

  depth_summary_df <- within(compare_df, {
    depth_selected <- suppressWarnings(as.integer(Selected_Depth))
    depth_selected <- ifelse(is.na(depth_selected), NA_integer_, depth_selected)
    depth_label <- ifelse(depth_selected == 1L, "depth 1", ifelse(depth_selected == 2L, "depth 2", NA_character_))
    pv_depth1 <- Depth1_PV_value
    pv_depth2 <- Depth2_PV_value
    pv_selected <- ifelse(depth_selected == 1L, pv_depth1, pv_depth2)
    pv_alternative <- ifelse(depth_selected == 1L, pv_depth2, pv_depth1)
    pv_gain <- pv_selected - pv_alternative
  })
  depth_summary_df <- depth_summary_df[, c("Model", "Outcome", "depth_selected", "depth_label", "pv_depth1", "pv_depth2", "pv_selected", "pv_alternative", "pv_gain")]
  names(depth_summary_df)[1:2] <- c("model", "outcome_label")
  depth_summary_df$outcome <- gsub("^model_", "", depth_summary_df$model)
  depth_summary_df <- depth_summary_df[, c("model", "outcome", "outcome_label", "depth_selected", "depth_label", "pv_depth1", "pv_depth2", "pv_selected", "pv_alternative", "pv_gain")]

  compare_md <- if (nrow(compare_df)) {
    tryCatch(knitr::kable(compare_df[, c("Outcome", "Depth1_PV", "Depth1_CI", "Depth2_PV", "Depth2_CI", "Selected_Depth")], format = "markdown"),
             error = function(e) paste(capture.output(print(compare_df)), collapse = "\n"))
  } else ""

  # Build best-of-depths per-model summary using selected depth recommendations
  best_by_model <- list()
  best_lines <- character()
  for (mn in all_models) {
    pick <- sel[mn]
    if (is.na(pick) || (!pick %in% c("1", "2"))) next
    rep_sel <- if (pick == "1") d1 else d2
    recs <- if (!is.null(rep_sel)) rep_sel$recommendations_by_model else NULL
    lab <- tryCatch({
      out <- gsub("^model_", "", mn)
      .apply_label_stability(out, label_mapping)
    }, error = function(e) gsub("^model_", "", mn))
    if (!is.null(recs) && !is.null(recs[[mn]])) {
      r <- recs[[mn]]
      best_by_model[[mn]] <- r
      # one-liner summary
      decision <- r$decision
      pv <- r$full$pv; lo <- r$full$lo; hi <- r$full$hi
      sel_label <- r$selected_label
      line <- switch(decision,
        deploy_restricted = sprintf("- %s (depth %s): Recommend restricted policy (split: %s). PV=%.3f [%.3f, %.3f]",
                                    lab, pick, ifelse(is.na(sel_label), "—", sel_label), pv, lo, hi),
        deploy_full = sprintf("- %s (depth %s): Recommend deploying full policy. PV=%.3f [%.3f, %.3f]",
                              lab, pick, pv, lo, hi),
        do_not_deploy = sprintf("- %s (depth %s): Do not deploy. PV=%.3f [%.3f, %.3f]",
                                lab, pick, pv, lo, hi),
        sprintf("- %s (depth %s): Caution. PV=%.3f [%.3f, %.3f]",
                lab, pick, pv, lo, hi)
      )
      best_lines <- c(best_lines, line)
    } else {
      # fallback to coherence audit if recommendations missing
      ca <- if (!is.null(rep_sel)) rep_sel$coherence_audit else NULL
      if (!is.null(ca)) {
        i <- which(ca$model == mn)
        if (length(i)) {
          pv <- ca$pv_ctrl_rep[i]; lo <- NA_real_; hi <- NA_real_
          best_by_model[[mn]] <- list(decision = "caution", reason = "no recommendations available",
                                      selected_label = NA_character_, full = list(pv = pv, lo = lo, hi = hi))
          best_lines <- c(best_lines, sprintf("- %s (depth %s): PV=%.3f", lab, pick, pv))
        }
      } else {
        # last fallback to raw reporter values
        val <- if (pick == "1" && mn %in% pv1$models) pv1$pv[mn, , drop = FALSE] else if (pick == "2" && mn %in% pv2$models) pv2$pv[mn, , drop = FALSE] else NULL
        if (!is.null(val)) {
          pv <- val[1, "pv"]; lo <- val[1, "lo"]; hi <- val[1, "hi"]
          best_by_model[[mn]] <- list(decision = "caution", reason = "raw reporter fallback",
                                      selected_label = NA_character_, full = list(pv = pv, lo = lo, hi = hi))
          best_lines <- c(best_lines, sprintf("- %s (depth %s): PV=%.3f%s", lab, pick, pv, if (!is.na(lo) && !is.na(hi)) sprintf(" [%.3f, %.3f]", lo, hi) else ""))
        }
      }
    }
  }
  best_text <- if (length(best_lines)) paste(c("### Best-of-Depths Summary", "", best_lines, ""), collapse = "\n") else ""

  best_summary <- NULL
  depth_mapping <- sel[!is.na(sel)]
  if (!is.null(inferred_depths_external) && length(inferred_depths_external)) {
    depth_mapping[names(inferred_depths_external)] <- as.character(inferred_depths_external)
  }
  if (length(depth_mapping)) {
    depth_mapping_int <- as.integer(depth_mapping)
    names(depth_mapping_int) <- names(depth_mapping)
    best_summary <- tryCatch(
      margot_policy_summary_report(
        object,
        model_names = names(depth_mapping_int),
        depths_by_model = depth_mapping_int,
        depth = 2L,
        original_df = original_df,
        se_method = se_method,
        R = R,
        seed = seed,
        label_mapping = label_mapping,
        include_split_breakdown = "branch",
        split_compact = split_compact,
        min_action_pct = min_action_pct,
        split_drop_zero = split_drop_zero,
        split_top_only = split_top_only,
        auto_recommend = auto_recommend,
        dominance_threshold = dominance_threshold,
        strict_branch = strict_branch,
        verbose = verbose
      ),
      error = function(e) {
        if (isTRUE(verbose)) {
          cli::cli_alert_warning("Failed to build combined depth summary: {e$message}")
        }
        NULL
      }
    )
    if (!is.null(best_summary)) {
      best_text <- best_summary$report
      best_by_model <- best_summary$recommendations_by_model
      best_split_df <- best_summary$split_table_compact_df
      best_split_md <- best_summary$split_table_compact_md
    }
  }

  # Build best-of-depths combined split table (selected depths only)
  if (is.null(best_summary)) {
    best_split_df <- data.frame()
    best_split_md <- ""
    if (length(best_by_model)) {
      rows <- list()
      for (mn in names(sel)) {
        pick <- sel[mn]
        if (is.na(pick) || (!pick %in% c("1","2"))) next
        rep_sel <- if (pick == "1") d1 else d2
        sbt <- rep_sel$split_table_compact[[mn]]
        if (!is.null(sbt) && nrow(sbt)) {
          out_lab <- tryCatch({
            out <- gsub("^model_", "", mn)
            .apply_label_stability(out, label_mapping)
          }, error = function(e) gsub("^model_", "", mn))
          sbt$Outcome <- out_lab
          sbt$Depth <- pick
          sbt <- sbt[, c("Outcome", "Depth", setdiff(colnames(sbt), c("Outcome","Depth"))), drop = FALSE]
          rows[[length(rows) + 1L]] <- sbt
        }
      }
      if (length(rows)) {
        best_split_df <- do.call(rbind, rows)
        rownames(best_split_df) <- NULL
        best_split_md <- tryCatch(knitr::kable(best_split_df, format = "markdown"), error = function(e) paste(capture.output(print(best_split_df)), collapse = "\n"))
      }
    }
    if (!nzchar(best_split_md)) {
      best_split_md <- sprintf("No compact split tables available after filtering (min_action_pct = %s).",
                               format(min_action_pct, trim = TRUE))
    }
  } else {
    best_split_df <- best_summary$split_table_compact_df
    if (is.null(best_split_df)) best_split_df <- data.frame()
    best_split_md <- best_summary$split_table_compact_md
    if (is.null(best_split_md)) best_split_md <- ""
    if (!nzchar(best_split_md)) {
      best_split_md <- sprintf("No compact split tables available after filtering (min_action_pct = %s).",
                               format(min_action_pct, trim = TRUE))
    }
  }

  depth_map_vec <- if (!is.null(best_summary)) {
    if (!is.null(best_summary$depth_map)) best_summary$depth_map else best_summary$model_depths
  } else {
    dm <- sel
    dm[!is.na(dm)] <- as.integer(dm[!is.na(dm)])
    dm <- as.integer(dm)
    names(dm) <- names(sel)
    dm
  }
  depth_map_vec <- depth_map_vec[!is.na(depth_map_vec)]
  if (!is.null(inferred_depths_external) && length(inferred_depths_external)) {
    depth_map_vec[names(inferred_depths_external)] <- inferred_depths_external
  }

  if (nrow(depth_summary_df)) {
    depth_summary_df$depth_selected <- ifelse(is.na(depth_summary_df$depth_selected) & depth_summary_df$model %in% names(depth_map_vec),
                                              depth_map_vec[depth_summary_df$model], depth_summary_df$depth_selected)
    depth_summary_df$depth_label <- ifelse(depth_summary_df$depth_selected == 1L, "depth 1",
                                           ifelse(depth_summary_df$depth_selected == 2L, "depth 2", depth_summary_df$depth_label))
    depth_summary_df$pv_selected <- ifelse(depth_summary_df$depth_selected == 1L, depth_summary_df$pv_depth1,
                                           ifelse(depth_summary_df$depth_selected == 2L, depth_summary_df$pv_depth2, depth_summary_df$pv_selected))
    depth_summary_df$pv_alternative <- ifelse(depth_summary_df$depth_selected == 1L, depth_summary_df$pv_depth2,
                                              ifelse(depth_summary_df$depth_selected == 2L, depth_summary_df$pv_depth1, depth_summary_df$pv_alternative))
    depth_summary_df$pv_gain <- depth_summary_df$pv_selected - depth_summary_df$pv_alternative
  }

  depth_map_vec <- depth_map_vec[order(match(names(depth_map_vec), depth_summary_df$model))]
  depth_map_vec <- depth_map_vec[!is.na(depth_map_vec)]
  depth_map_vec <- depth_map_vec[!duplicated(names(depth_map_vec))]

  depth_summary_df$decision <- NA_character_
  if (length(best_by_model)) {
    decisions_lookup <- vapply(best_by_model, function(x) x$decision %||% NA_character_, character(1))
    depth_summary_df$decision <- decisions_lookup[depth_summary_df$model]
  }
  depth_summary_df$decision[is.na(depth_summary_df$decision)] <- "not_evaluated"

  depth_table_df <- depth_summary_df
  depth_table_df$pv_depth1 <- round(depth_table_df$pv_depth1, 3)
  depth_table_df$pv_depth2 <- round(depth_table_df$pv_depth2, 3)
  depth_table_df$pv_selected <- round(depth_table_df$pv_selected, 3)
  depth_table_df$pv_alternative <- round(depth_table_df$pv_alternative, 3)
  depth_table_df$pv_gain <- round(depth_table_df$pv_gain, 3)
  depth_table_df$Preferred_Depth <- ifelse(is.na(depth_table_df$depth_selected), "unknown", paste0("depth ", depth_table_df$depth_selected))
  depth_table_df$Delta_vs_alt <- depth_table_df$pv_gain
  depth_table_df$Decision <- depth_table_df$decision
  depth_table_df <- depth_table_df[, c("outcome_label", "pv_depth1", "pv_depth2", "Preferred_Depth", "Delta_vs_alt", "Decision")]
  names(depth_table_df)[1] <- "Outcome"
  depth_table_md <- if (nrow(depth_table_df)) {
    tryCatch(knitr::kable(depth_table_df, format = "markdown"), error = function(e) paste(capture.output(print(depth_table_df)), collapse = "\n"))
  } else ""

  summarise_depth_list <- function(df, depth_label) {
    if (!nrow(df)) return(sprintf("No outcomes preferred depth %s.", depth_label))
    items <- vapply(seq_len(nrow(df)), function(i) {
      lbl <- df$outcome_label[i]
      gain <- df$pv_gain[i]
      if (is.na(gain)) lbl else paste0(lbl, " (Δ = ", round(gain, 3), ")")
    }, character(1))
    paste(items, collapse = "; ")
  }

  depth1_pref <- depth_summary_df[depth_summary_df$depth_selected == 1L & !is.na(depth_summary_df$depth_selected), , drop = FALSE]
  depth2_pref <- depth_summary_df[depth_summary_df$depth_selected == 2L & !is.na(depth_summary_df$depth_selected), , drop = FALSE]
  depth_takeaways_lines <- c(
    sprintf("Depth-1 preferred (%d): %s", nrow(depth1_pref), summarise_depth_list(depth1_pref, "1")),
    sprintf("Depth-2 preferred (%d): %s", nrow(depth2_pref), summarise_depth_list(depth2_pref, "2"))
  )
  depth_takeaways_lines <- depth_takeaways_lines[nchar(depth_takeaways_lines) > 0]
  depth_takeaways_text <- paste(depth_takeaways_lines, collapse = "\n")

  if (nzchar(depth_takeaways_text)) {
    depth_block <- paste(c("### Depth Selection", "", depth_takeaways_text, ""), collapse = "\n")
    if (nzchar(best_text)) {
      best_text <- paste0(depth_block, best_text)
    } else {
      best_text <- depth_block
    }
  }

  recommendations_df <- depth_summary_df[, c("model", "outcome", "outcome_label", "depth_selected", "depth_label", "pv_depth1", "pv_depth2", "pv_selected", "pv_alternative", "pv_gain", "decision")]

  list(
    depth_map = depth_map_vec,
    depth_summary_df = depth_summary_df,
    depth_table_df = depth_table_df,
    depth_table_md = depth_table_md,
    depth_takeaways_text = depth_takeaways_text,
    summary_text = best_text,
    recommendations_df = recommendations_df
  )
}
