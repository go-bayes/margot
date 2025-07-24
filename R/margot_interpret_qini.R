#' Interpret Qini Results
#'
#' Interprets Qini results for binary and multi-arm treatments, automatically
#' detecting treatment type from input data.
#'
#' @details
#' The function automatically detects available spend levels in the data. If requested
#' spend levels are not available, it will use the closest available levels and warn
#' the user. To see what spend levels are available, check the names of
#' `multi_batch[[1]]$diff_gain_summaries`.
#'
#' This function accepts output from either:
#' \itemize{
#'   \item margot_policy() - which includes policy trees and Qini results
#'   \item margot_qini() - which focuses solely on Qini curves and gains
#' }
#'
#' @param multi_batch List from margot_policy() or margot_qini() with diff_gain_summaries
#' @param label_mapping Named list mapping model names to readable labels
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param decimal_places Decimal places for rounding (default: 2)
#' @param model_names Character vector of models to process (optional)
#' @param spend_levels Numeric vector of spend levels to analyze (default: c(0.2, 0.5)).
#'   If requested levels don't exist in the data, the function will use available levels instead.
#' @param include_intro Logical whether to include explanatory text about CATE and Qini curves (default: TRUE)
#' @return List with summary_table, qini_explanation, concise_summary, reliable_model_names, reliable_model_ids
#' @export
margot_interpret_qini <- function(
    multi_batch,
    label_mapping   = NULL,
    alpha           = 0.05,
    decimal_places  = 2,
    model_names     = NULL,
    spend_levels    = c(0.2, 0.5),
    include_intro   = TRUE
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
      multi_batch, label_mapping, alpha, decimal_places, spend_levels, include_intro
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
    concise_summary       = res$concise_summary,
    reliable_model_names  = res$reliable_model_names,
    reliable_model_ids    = res$reliable_model_ids,
    harmful_model_ids     = res$harmful_model_ids,
    harmful_model_names   = res$harmful_model_names,
    no_effect_model_ids   = res$no_effect_model_ids,
    no_effect_model_names = res$no_effect_model_names
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
    decimal_places  = 2,
    spend_levels    = c(0.2, 0.5),
    include_intro   = TRUE
) {
  cli::cli_alert_info("processing binary treatment model…")

  # check available spend levels in the data
  available_spend_levels <- numeric()
  for (nm in names(multi_batch)) {
    if (!is.null(multi_batch[[nm]]$diff_gain_summaries)) {
      spend_names <- names(multi_batch[[nm]]$diff_gain_summaries)
      spend_vals <- as.numeric(sub("^spend_", "", spend_names))
      available_spend_levels <- unique(c(available_spend_levels, spend_vals))
    }
  }

  if (length(available_spend_levels) > 0) {
    available_spend_levels <- sort(available_spend_levels)
    cli::cli_alert_info("Available spend levels in data: {paste0(available_spend_levels * 100, '%', collapse = ', ')}")

    # check if requested spend levels exist
    missing_levels <- setdiff(spend_levels, available_spend_levels)
    if (length(missing_levels) > 0) {
      cli::cli_alert_warning(
        "Requested spend levels not found in data: {paste0(missing_levels * 100, '%', collapse = ', ')}. Using available levels instead."
      )
      # use intersection of requested and available
      spend_levels <- intersect(spend_levels, available_spend_levels)
      if (length(spend_levels) == 0) {
        # if no intersection, use all available
        spend_levels <- available_spend_levels
      }
    }
  }

  # the single-preamble text
  qini_explanation <- create_qini_explanation_binary(spend_levels, include_intro)

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

  # for concise summary
  beneficial_models <- character(0)
  harmful_models <- character(0)
  no_effect_models <- character(0)
  
  # track ids as well as names
  harmful_ids <- character(0)
  no_effect_ids <- character(0)

  for (nm in names(multi_batch)) {
    dg_list <- multi_batch[[nm]]$diff_gain_summaries
    spend_vals <- as.numeric(sub("^spend_", "", names(dg_list)))

    # find indices for requested spend levels
    spend_indices <- list()
    estimates <- list()
    sentences <- list()

    for (spend in spend_levels) {
      idx <- which(abs(spend_vals - spend) < 1e-4)
      if (length(idx)) {
        spend_indices[[as.character(spend)]] <- idx
        estimates[[as.character(spend)]] <- extract_estimates(dg_list[[idx]])
        sentences[[as.character(spend)]] <- make_sentence(dg_list[[idx]], spend)
      } else {
        estimates[[as.character(spend)]] <- c(NA, NA, NA)
        sentences[[as.character(spend)]] <- sprintf("At %s%% spend: No data available.", spend * 100)
      }
    }

    # create row for summary table
    row_data <- list(Model = label_fn(nm))
    for (spend in spend_levels) {
      col_name <- sprintf("Spend %s%%", spend * 100)
      row_data[[col_name]] <- fmt_est_ci(estimates[[as.character(spend)]])
    }
    rows[[nm]] <- tibble::tibble(!!!row_data)

    # create explanation text
    all_sentences <- unlist(sentences)
    no_benefits_count <- sum(grepl("No reliable benefits", all_sentences))
    beneficial_count <- sum(grepl("beneficial", all_sentences))
    worsens_count <- sum(grepl("worsens", all_sentences))

    if (no_benefits_count == length(spend_levels)) {
      spend_text <- paste(paste0(spend_levels * 100, "%"), collapse = " or ")
      explains[[label_fn(nm)]] <- paste0(
        "No benefits for priority investments as measured by the Qini curve at the ",
        spend_text, " spend levels."
      )
      no_effect_models <- c(no_effect_models, label_fn(nm))
      no_effect_ids <- c(no_effect_ids, nm)
    } else {
      txt <- paste(all_sentences, collapse = " ")
      txt <- sub("^We computed[^.]*\\.\\s*", "", txt)
      
      
      explains[[label_fn(nm)]] <- txt

      # categorize for concise summary
      if (beneficial_count > worsens_count) {
        beneficial_models <- c(beneficial_models, label_fn(nm))
      } else if (worsens_count > beneficial_count) {
        harmful_models <- c(harmful_models, label_fn(nm))
        harmful_ids <- c(harmful_ids, nm)
      } else {
        no_effect_models <- c(no_effect_models, label_fn(nm))
        no_effect_ids <- c(no_effect_ids, nm)
      }
    }

    # keepers = any CI lower > 0 across all spend levels
    has_positive_effect <- FALSE
    for (est in estimates) {
      if (!is.na(est["ci_lower"]) && est["ci_lower"] > 0) {
        has_positive_effect <- TRUE
        break
      }
    }

    if (has_positive_effect) {
      keep_ids <- c(keep_ids, nm)
      keep_lbl <- c(keep_lbl, label_fn(nm))
    }
  }

  # create concise summary
  concise_summary <- create_concise_summary(beneficial_models, harmful_models, no_effect_models, spend_levels)

  list(
    summary_table         = dplyr::bind_rows(rows),
    explanations          = explains,
    qini_explanation      = qini_explanation,
    concise_summary       = concise_summary,
    reliable_model_names  = keep_lbl,
    reliable_model_ids    = keep_ids,
    harmful_model_ids     = harmful_ids,
    harmful_model_names   = harmful_models,
    no_effect_model_ids   = no_effect_ids,
    no_effect_model_names = no_effect_models
  )
}


#' @keywords internal
create_concise_summary <- function(beneficial_models, harmful_models, no_effect_models, spend_levels) {
  spend_text <- paste(paste0(spend_levels * 100, "%"), collapse = " and ")

  parts <- character(0)

  if (length(beneficial_models) > 0) {
    if (length(beneficial_models) == 1) {
      parts <- c(parts, paste0("CATE-based targeting improves outcomes for ", beneficial_models, "."))
    } else if (length(beneficial_models) == 2) {
      parts <- c(parts, paste0("CATE-based targeting improves outcomes for ",
                               paste(beneficial_models, collapse = " and "), "."))
    } else {
      last_model <- beneficial_models[length(beneficial_models)]
      other_models <- beneficial_models[-length(beneficial_models)]
      parts <- c(parts, paste0("CATE-based targeting improves outcomes for ",
                               paste(other_models, collapse = ", "), ", and ", last_model, "."))
    }
  }

  if (length(harmful_models) > 0) {
    if (length(harmful_models) == 1) {
      parts <- c(parts, paste0("CATE-based targeting worsens outcomes for ", harmful_models, "."))
    } else if (length(harmful_models) == 2) {
      parts <- c(parts, paste0("CATE-based targeting worsens outcomes for ",
                               paste(harmful_models, collapse = " and "), "."))
    } else {
      last_model <- harmful_models[length(harmful_models)]
      other_models <- harmful_models[-length(harmful_models)]
      parts <- c(parts, paste0("CATE-based targeting worsens outcomes for ",
                               paste(other_models, collapse = ", "), ", and ", last_model, "."))
    }
  }

  if (length(no_effect_models) > 0) {
    if (length(no_effect_models) == 1) {
      parts <- c(parts, paste0("No reliable benefit from CATE-based targeting for ", no_effect_models, "."))
    } else if (length(no_effect_models) == 2) {
      parts <- c(parts, paste0("No reliable benefit from CATE-based targeting for ",
                               paste(no_effect_models, collapse = " and "), "."))
    } else {
      last_model <- no_effect_models[length(no_effect_models)]
      other_models <- no_effect_models[-length(no_effect_models)]
      parts <- c(parts, paste0("No reliable benefit from CATE-based targeting for ",
                               paste(other_models, collapse = ", "), ", and ", last_model, "."))
    }
  }

  if (length(parts) == 0) {
    return(paste0("No models analyzed at the ", spend_text, " spend levels."))
  }

  summary <- paste(parts, collapse = " ")
  paste0(summary, " (Based on Qini curve analysis at ", spend_text, " spend levels.)")
}


#' @keywords internal
create_qini_explanation_binary <- function(spend_levels = c(0.2, 0.5), include_intro = TRUE) {
  # format spend levels as percentages
  spend_text <- paste(paste0(spend_levels * 100, "%"), collapse = " and ")

  base_text <- if (include_intro) {
    paste0(
      "The QINI curve compares targeted treatment allocation (based on individual treatment effects) versus uniform allocation (based on average treatment effect). ",
      "Small differences in the expected values of the treatment after the entire population is treated are expected due to out of sample cross-validation (all estimates are tested on data the model has not seen). ",
      "We computed cumulative gains from prioritising individuals by CATE at ", spend_text, " spend levels."
    )
  } else {
    paste0("We computed cumulative gains from prioritising individuals by CATE at ", spend_text, " spend levels.")
  }
  
  # add note about potential paradox at high spend levels
  if (any(spend_levels >= 0.9)) {
    base_text <- paste0(
      base_text,
      " Note: CATE-based targeting may show benefits even when the average treatment effect is unreliable, as heterogeneous effects can produce valuable targeting opportunities despite an uncertain population average."
    )
  }
  
  base_text
}
