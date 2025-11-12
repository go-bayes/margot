#' Summarize IPSI probabilities from transition tables
#'
#' Consumes the output of `margot_transition_table()` (or a compatible list of
#' transition matrices) and computes initiation probabilities for each wave pair
#' under incremental propensity score interventions (IPSIs). Internally it calls
#' `margot_compute_ipsi_probability()` for every matrix, returning a tidy data
#' frame that includes the natural initiation rate, counterfactual probabilities,
#' fold increases, and the raw counts used in each estimate.
#'
#' @param transitions Either the object returned by `margot_transition_table()`
#'   or a list of transition matrices (data frames or `knitr_kable`s containing
#'   `"From / To"`, `"State 1"`, and `"Total"` columns). When an object from
#'   `margot_transition_table()` is supplied, the stored wave labels are used in
#'   the summary.
#' @param deltas Numeric vector of IPSI scaling factors (greater than 1). Passed
#'   to `margot_compute_ipsi_probability()`; defaults to the standard set
#'   `c(2, 5, 10)`.
#' @param pretty Logical; if `TRUE`, the table component is formatted for
#'   `knitr::kable()` / `kbl()` while numeric values remain attached under
#'   `attr(table, "raw")`.
#' @param digits_prob Integer digits for percentage columns when `pretty = TRUE`.
#' @param digits_fold Integer digits for fold-increase columns when `pretty = TRUE`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`table`}{Data frame with one row per wave pair × delta containing
#'       the natural and counterfactual initiation probabilities, fold increases,
#'       and raw counts (string-formatted when `pretty = TRUE`).}
#'     \item{`report`}{Character vector of NZ-English sentences, with LaTeX math
#'       markup (e.g., `$\\to$`, `$\\delta$`, `$p'$`), summarising each wave pair.}
#'   }
#'
#' @examples
#' transitions <- margot_transition_table(
#'   data.frame(
#'     id = rep(1:3, each = 3),
#'     wave = rep(c(2018, 2019, 2022), 3),
#'     religion = c(0, 0, 1,
#'                  0, 1, 1,
#'                  1, 1, 1),
#'     observed = 1
#'   ),
#'   state_var = "religion",
#'   id_var = "id",
#'   wave_var = "wave",
#'   observed_var = "observed",
#'   observed_val = 1,
#'   waves = c(2018, 2019, 2022)
#' )
#'
#' margot_transition_ipsi_summary(transitions)
#'
#' @export
margot_transition_ipsi_summary <- function(transitions,
                                           deltas = c(2, 5, 10),
                                           pretty = FALSE,
                                           digits_prob = 1,
                                           digits_fold = 1) {
  stopifnot(length(deltas) > 0, all(is.finite(deltas)), all(deltas > 1))
  digits_prob <- max(0L, as.integer(digits_prob))
  digits_fold <- max(0L, as.integer(digits_fold))

  is_margot_obj <- inherits(transitions, "margot_transitions")
  tables_list <- NULL
  waves_list <- NULL

  extract_from_kable <- function(x) {
    if (inherits(x, "knitr_kable")) {
      data_attr <- attr(x, "table_data")
      if (!is.null(data_attr)) return(data_attr)
      stop("knitr_kable inputs must include a 'table_data' attribute.")
    }
    x
  }

  if (is_margot_obj) {
    if (!is.null(transitions$get_table_data)) {
      tables_list <- transitions$get_table_data(which = NULL, drop = FALSE)
    } else if (!is.null(transitions$tables_data)) {
      tables_list <- transitions$tables_data
    } else {
      tables_list <- lapply(transitions$tables, extract_from_kable)
    }
    waves_list <- transitions$waves
  } else if (is.list(transitions)) {
    tables_list <- lapply(transitions, extract_from_kable)
  } else {
    stop("`transitions` must be a margot_transition_table() result or a list of tables.")
  }

  if (!length(tables_list)) {
    stop("No transition tables supplied.")
  }

  tbl_names <- names(tables_list)
  if (is.null(tbl_names) || any(!nchar(tbl_names))) {
    tbl_names <- paste0("table_", seq_along(tables_list))
  }

  rows <- vector("list", length(tables_list))
  for (i in seq_along(tables_list)) {
    mat <- tables_list[[i]]
    if (!is.data.frame(mat)) {
      stop("Each transition matrix must be a data frame or knitr_kable with table_data.")
    }
    probs <- margot_compute_ipsi_probability(mat, deltas = deltas)
    counts <- attr(probs, "counts")
    waves <- if (!is.null(waves_list) && length(waves_list) >= i) waves_list[[i]] else c(NA, NA)
    rows[[i]] <- cbind(
      table_index = i,
      table_name = tbl_names[[i]],
      wave_from = waves[1],
      wave_to = waves[2],
      initiations = counts$initiations,
      non_attenders = counts$non_attenders,
      probs,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "tables") <- tables_list

  format_pct_sentence <- function(x) {
    if (is.na(x)) return("NA")
    val <- formatC(100 * x, format = "f", digits = digits_prob, drop0trailing = FALSE)
    paste0(val, "\\%")
  }
  format_fold_sentence <- function(x) {
    if (is.na(x)) return("NA")
    val <- formatC(x, format = "f", digits = digits_fold, drop0trailing = FALSE)
    paste0(val, "-fold increase")
  }
  format_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  format_num <- function(x) format(x, trim = TRUE, scientific = FALSE)

  wave_label_report <- ifelse(
    is.na(out$wave_from) | is.na(out$wave_to),
    paste("Table", out$table_name),
    paste0("Wave ", out$wave_from, " $\\to$ ", out$wave_to)
  )
  idx_split <- split(seq_len(nrow(out)), wave_label_report)
  report <- unlist(lapply(names(idx_split), function(label) {
    idx <- idx_split[[label]]
    df <- out[idx, , drop = FALSE]
    counts <- df[1, ]
    header <- sprintf(
      "%s: The natural initiation rate was approximately %s (%s / %s non-attenders).",
      label,
      format_pct_sentence(counts$natural_p),
      format_count(counts$initiations),
      format_count(counts$non_attenders)
    )
    formula_sentence <- "Counterfactual probabilities follow $p' = 1 - (1 - p)/\\delta$ for this transition."
    delta_lines <- vapply(
      seq_len(nrow(df)),
      function(j) sprintf(
        "For $\\delta = %s$ (so $1/\\delta = %s$), the counterfactual initiation probability is about %s, representing a %s.",
        df$delta[j],
        format_num(df$delta_inverse[j]),
        format_pct_sentence(df$counterfactual_p[j]),
        format_fold_sentence(df$fold_increase[j])
      ),
      character(1)
    )
    c(header, formula_sentence, delta_lines, "")
  }), use.names = FALSE)
  if (length(report) && report[length(report)] == "") report <- report[-length(report)]

  wave_pair <- ifelse(
    is.na(out$wave_from) | is.na(out$wave_to),
    out$table_name,
    paste(out$wave_from, "->", out$wave_to)
  )

  table_out <- out
  if (pretty) {
    percent_fmt <- function(x) ifelse(
      is.na(x),
      NA_character_,
      sprintf(paste0("%.", digits_prob, "f%%"), 100 * x)
    )
    fold_fmt <- function(x) ifelse(
      is.na(x),
      NA_character_,
      sprintf(paste0("%.", digits_fold, "f×"), x)
    )
    table_pretty <- data.frame(
      `Wave pair` = wave_pair,
      Delta = out$delta,
      `1/Delta` = out$delta_inverse,
      `Initiations / at-risk` = sprintf("%s / %s", out$initiations, out$non_attenders),
      `Natural p` = percent_fmt(out$natural_p),
      `Counterfactual p` = percent_fmt(out$counterfactual_p),
      `Fold increase` = fold_fmt(out$fold_increase),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    attr(table_pretty, "raw") <- out
    attr(table_pretty, "tables") <- tables_list
    table_out <- table_pretty
  }

  list(
    table = table_out,
    report = report
  )
}
