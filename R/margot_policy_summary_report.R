#' Policy Tree Summary Report (text + markdown table)
#'
#' Generates a concise textual summary and a markdown table for policy tree results
#' (typically from `margot_policy_tree_stability()`). The report focuses on:
#' - Policy value vs control-all and vs treat-all (95% CIs)
#' - Average uplift among treated (95% CI) and coverage treated (%)
#' - Optional stability highlights (consensus strength and top split variable)
#'
#' @param object A `margot_stability_policy_tree` object.
#' @param model_names Optional character vector of outcome names (with or without
#'   `model_` prefix). Default: all models in `object`.
#' @param depth Integer; policy tree depth to summarize (default 2).
#' @param R Integer ≥ 199; bootstrap replicates for reporter CIs (default 499).
#' @param seed Integer or NULL; RNG seed (default 42).
#' @param label_mapping Optional named list for display labels.
#' @param digits Integer; rounding for numeric displays (default 3).
#' @param include_stability Logical; include stability highlights per model (default TRUE).
#' @param include_explanation Logical; include policy value explanation block (default TRUE).
#' @param table_type Character; one of "full" (default) or "treated_only" for condensed table.
#'
#' @return A list with:
#'   - `text`: Combined summary text (character scalar)
#'   - `report`: Narrative policy summary formatted for scientific reporting
#'   - `interpretation`: Concise sentence-level summary listing wins, caution, and uncertain models
#'   - `table_md`: Markdown table (character scalar) when `render_markdown = TRUE`
#'   - `table_df`: Data frame of policy value summary rows (title-case columns)
#'   - `wins_model_ids`: Character vector of model identifiers where policy vs control-all is strictly positive (CI > 0)
#'   - `wins_model_names`: Character vector of human-readable labels matching `wins_model_ids`
#'   - `neutral_model_ids`: Character vector of model identifiers with inconclusive policy vs control-all evidence (CI crosses 0)
#'   - `neutral_model_names`: Character vector of human-readable labels matching `neutral_model_ids`
#'   - `caution_model_ids`: Character vector of model identifiers where policy vs control-all is negative (CI < 0)
#'   - `caution_model_names`: Character vector of human-readable labels matching `caution_model_ids`
#'   - `group_table`: Named list of grouped brief tables (data frames)
#'   - `group_table_df`: Combined grouped table with a `Group` column
#'
#' @export
margot_policy_summary_report <- function(object,
                                         model_names = NULL,
                                         depth = 2L,
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
                                         render_markdown = TRUE) {
  stopifnot(inherits(object, "margot_stability_policy_tree"))
  table_type <- match.arg(table_type)
  order_by <- match.arg(order_by)

  # determine model set
  if (is.null(model_names)) {
    model_names <- names(object$results)
  } else {
    model_names <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
  }

  # reporter with treated-only metrics
  if (is.null(report_df)) {
    if (verbose) cli::cli_alert_info("Computing policy values (R = {R}, depth = {depth})")
    rep <- margot_report_consensus_policy_value(
      object,
      model_names = model_names,
      depths = depth,
      R = R,
      seed = seed,
      include_treated_only = TRUE,
      label_mapping = label_mapping,
      verbose = verbose
    )
  } else {
    if (verbose) cli::cli_alert_info("Using precomputed policy value report (depth = {depth})")
    rep <- report_df
    # optional filter to provided models
    if (!is.null(model_names)) {
      keep <- ifelse(grepl("^model_", model_names), model_names, paste0("model_", model_names))
      rep <- rep[rep$model %in% keep & rep$depth %in% depth, , drop = FALSE]
    } else {
      rep <- rep[rep$depth %in% depth, , drop = FALSE]
    }
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

  lines <- character()
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

  report_sections <- c(report_sections, list(
    paste0(
      "Consensus policy evaluation at depth ", depth,
      " compared targeted deployment against control-all and treat-all baselines."
    ),
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
        if (!is.null(sm)) {
          cs1 <- suppressWarnings(tryCatch(sm$consensus_strength$depth_1, error = function(e) NA_real_))
          vf <- suppressWarnings(tryCatch(sm$var_inclusion_freq, error = function(e) NULL))
          if (!is.null(vf) && "depth_1_freq" %in% names(vf)) {
            ord <- order(vf$depth_1_freq, decreasing = TRUE)
            topv <- vf$variable[ord][1]
            topf <- vf$depth_1_freq[ord][1]
            topv_label <- .apply_label_stability(topv, label_mapping)
            stab_text <- paste0(
              "; stability: depth-1 consensus ", round(cs1, 2), ", top split = ", topv_label, " (", round(100 * topf, 1), "%)"
            )
          } else if (!is.na(cs1)) {
            stab_text <- paste0("; stability: depth-1 consensus ", round(cs1, 2))
          }
        }
      }

      if (isTRUE(compact)) {
        # compact line: focus on control-all PV + treated-only uplift + coverage
        lines <- c(lines, paste0(
          "- ", out_label, ": ",
          "PV(control-all) = ", pv_ctrl_text, "; ",
          "Uplift(treated) = ", uplift_text, "; ",
          "Coverage = ", coverage_text
        ))
      } else {
        lines <- c(lines, paste0(
          "- ", out_label, ": ",
          "policy vs control-all = ", pv_ctrl_text, "; ",
          "policy vs treat-all = ", pv_treat_text, "; ",
          "avg uplift (treated) = ", uplift_text, ", coverage = ", coverage_text, stab_text
        ))
      }
    }
  }

  header <- paste0("### Policy Summary (depth ", depth, ")\n\n")
  expl <- if (isTRUE(include_explanation)) paste0("\n\n", policy_expl, "\n\n") else "\n"
  text <- paste0(header, paste(lines, collapse = "\n"), expl)

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
    wins_model_ids = wins_model_ids,
    wins_model_names = wins_model_names,
    neutral_model_ids = neutral_model_ids,
    neutral_model_names = neutral_model_names,
    caution_model_ids = caution_model_ids,
    caution_model_names = caution_model_names,
    group_table = group_tables,
    group_table_df = group_table_df
  )
}
