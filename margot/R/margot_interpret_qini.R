#' Interpret Qini Results
#'
#' Interprets Qini results for binary and multi-arm treatments, automatically
#' detecting treatment type from input data.
#'
#' @param multi_batch List from margot_batch_policy() with diff_gain_summaries
#' @param label_mapping Named list mapping model names to readable labels
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param decimal_places Decimal places for rounding (default: 2)
#' @param model_names Character vector of models to process (optional)
#' @return List with summary_table, qini_explanation, reliable_model_names, reliable_model_ids
#' @export
margot_interpret_qini <- function(
    multi_batch,
    label_mapping   = NULL,
    alpha           = 0.05,
    decimal_places  = 2,
    model_names     = NULL
) {
  cli::cli_alert_info("starting Qini interpretation")

  ##----------------------------------------------------------------##
  ##  treat single-model objects as a length-1 list               ##
  ##----------------------------------------------------------------##
  if ("diff_gain_summaries" %in% names(multi_batch)) {
    multi_batch <- list(model_result = multi_batch)
  }

  if (!is.null(model_names)) {
    multi_batch <- multi_batch[intersect(names(multi_batch), model_names)]
    if (length(multi_batch) == 0)
      cli::cli_abort("none of the requested model_names found in multi_batch")
  }
  if (length(multi_batch) == 0)
    cli::cli_abort("multi_batch is empty")

  ##----------------------------------------------------------------##
  ##  binary vs multi-arm switch                                  ##
  ##----------------------------------------------------------------##
  first_spend <- names(multi_batch[[1]]$diff_gain_summaries)[1]
  first_obj   <- multi_batch[[1]]$diff_gain_summaries[[first_spend]]
  is_binary   <- !is.list(first_obj) || !"all_arms" %in% names(first_obj)

  if (is_binary) {
    res <- margot_interpret_qini_binary(
      multi_batch, label_mapping, alpha, decimal_places
    )
  } else {
    cli::cli_abort("multi-arm Qini interpretation not yet implemented")
  }

  ##----------------------------------------------------------------##
  ##  assemble markdown explanation                                ##
  ##----------------------------------------------------------------##
  # build each per-model block once
  model_expls <- vapply(names(res$explanations), function(nm) {
    paste0("**", nm, "**\n", res$explanations[[nm]])
  }, character(1))

  # preamble only once, then join
  full_expl <- paste0(
    res$qini_explanation,
    "\n\n",
    paste(model_expls, collapse = "\n\n")
  )

  list(
    summary_table         = res$summary_table,
    qini_explanation      = full_expl,
    reliable_model_names  = res$reliable_model_names,
    reliable_model_ids    = res$reliable_model_ids
  )
}


#’ ---------------------------------------------------------------------------
#’ Binary-treatment implementation (helper)
#’ ---------------------------------------------------------------------------
#’ @keywords internal
margot_interpret_qini_binary <- function(
    multi_batch,
    label_mapping   = NULL,
    alpha           = 0.05,
    decimal_places  = 2
) {
  cli::cli_alert_info("processing binary treatment model…")

  # the single-preamble text
  qini_explanation <- create_qini_explanation_binary()

  ##-------------------  helpers in local scope  ----------------------##
  extract_estimates <- function(dg) {
    est <- as.numeric(sub("^\\s*([+-]?[0-9.]+).*", "\\1", dg$diff_gain))
    se  <- as.numeric(sub(".*SE:?\\s*([0-9.]+)\\).*", "\\1", dg$diff_gain))
    if (anyNA(c(est, se))) return(c(estimate=NA, ci_lower=NA, ci_upper=NA))
    z  <- stats::qnorm(1 - alpha/2)
    c(estimate=est, ci_lower=est - z*se, ci_upper=est + z*se)
  }

  fmt_est_ci <- function(v) {
    if (anyNA(v)) return("NA [NA, NA]")
    txt <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places,
                          "f, %.", decimal_places, "f]"),
                   v["estimate"], v["ci_lower"], v["ci_upper"])
    if (v["ci_lower"] > 0)      paste0("**", txt, "**")
    else if (v["ci_upper"] < 0) paste0("*",  txt, "*")
    else                         txt
  }

  make_sentence <- function(dg, spend) {
    v <- extract_estimates(dg)
    if (anyNA(v)) {
      sprintf("At %s%% spend: No data available.", spend*100)
    } else if (v["ci_lower"] * v["ci_upper"] > 0) {
      if (v["estimate"] > 0) {
        sprintf("At %s%% spend: CATE prioritisation is beneficial (diff: %.2f [95%% CI %.2f, %.2f]).",
                spend*100, v["estimate"], v["ci_lower"], v["ci_upper"])
      } else {
        sprintf("At %s%% spend: CATE prioritisation worsens outcomes compared to ATE.",
                spend*100)
      }
    } else {
      sprintf("At %s%% spend: No reliable benefits from CATE prioritisation.",
              spend*100)
    }
  }

  label_fn <- function(x) {
    if (exists("transform_var_name", mode="function")) {
      transform_var_name(x, label_mapping, TRUE, TRUE, TRUE, TRUE)
    } else {
      tools::toTitleCase(gsub("_", " ", sub("^model_|_z$", "", x)))
    }
  }

  ##-------------------  main loop  -----------------------------------##
  rows     <- list()
  explains <- list()
  keep_ids <- character(0)
  keep_lbl <- character(0)

  for (nm in names(multi_batch)) {
    dg_list <- multi_batch[[nm]]$diff_gain_summaries
    spend_vals <- as.numeric(sub("^spend_", "", names(dg_list)))
    idx20 <- which(abs(spend_vals - 0.2) < 1e-4)
    idx50 <- which(abs(spend_vals - 0.5) < 1e-4)

    est20 <- if (length(idx20)) extract_estimates(dg_list[[idx20]]) else c(NA,NA,NA)
    est50 <- if (length(idx50)) extract_estimates(dg_list[[idx50]]) else c(NA,NA,NA)

    rows[[nm]] <- tibble::tibble(
      Model       = label_fn(nm),
      `Spend 20%` = fmt_est_ci(est20),
      `Spend 50%` = fmt_est_ci(est50)
    )

    sent20 <- if (length(idx20)) make_sentence(dg_list[[idx20]], 0.2) else "At 20% spend: No data available."
    sent50 <- if (length(idx50)) make_sentence(dg_list[[idx50]], 0.5) else "At 50% spend: No data available."

    if (grepl("No reliable benefits", sent20) &&
        grepl("No reliable benefits", sent50)) {
      explains[[label_fn(nm)]] <-
        "No benefits for priority investments as measured by the Qini curve at the twenty or fifty percent spend levels."
    } else {
      txt <- paste(sent20, sent50)
      # strip any accidental duplicate preamble at start of these sentences:
      txt <- sub("^We computed[^.]*\\.\\s*", "", txt)
      explains[[label_fn(nm)]] <- txt
    }

    # keepers = any CI lower > 0
    if ((!is.na(est20["ci_lower"]) && est20["ci_lower"] > 0) ||
        (!is.na(est50["ci_lower"]) && est50["ci_lower"] > 0)) {
      keep_ids <- c(keep_ids, nm)
      keep_lbl <- c(keep_lbl, label_fn(nm))
    }
  }

  list(
    summary_table         = dplyr::bind_rows(rows),
    explanations          = explains,
    qini_explanation      = qini_explanation,
    reliable_model_names  = keep_lbl,
    reliable_model_ids    = keep_ids
  )
}


#’ @keywords internal
create_qini_explanation_binary <- function() {
  "We computed cumulative gains from prioritising individuals by CATE at 20% and 50% spend levels, comparing against a no-prioritisation baseline."
}
