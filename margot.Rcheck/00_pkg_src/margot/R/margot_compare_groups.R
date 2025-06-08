#' Compare subgroups from a causal forest model.
#'
#' @description
#' Compare two sets of treatment-effect estimates and report their differences.
#'
#' @param group         data frame with treatment effects and confidence intervals for primary group
#' @param subgroup      data frame with treatment effects and confidence intervals for contrast group
#' @param type          Character; one of `"RD"` or `"RR"` (default: both allowed)
#' @param label_mapping Optional named vector for nicer row labels
#' @param decimal_places Number of digits for CI formatting (default: 3)
#' @param group1_name   Character; name to use for primary group (default: "Group 1")
#' @param group2_name   Character; name to use for contrast group (default: "Group 2")
#' @return A list with:
#'   - `results`: a data frame with columns `Outcomes` and `Group Differences`
#'   - `interpretation`: a character string summarising reliable differences
#' @export
#' @importFrom purrr pmap_dfr map2_chr
#' @importFrom tibble tibble
#' @importFrom vctrs vec_recycle
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr mutate relocate select filter
margot_compare_groups <- function(group,
                                  subgroup,
                                  type = c("RD", "RR"),
                                  label_mapping = NULL,
                                  decimal_places = 3,
                                  group1_name = "Group 1",
                                  group2_name = "Group 2") {
  type <- match.arg(type)
  stopifnot(is.data.frame(group), is.data.frame(subgroup), nrow(group) == nrow(subgroup))

  # determine which columns to use
  est_col <- if (type == "RD") "E[Y(1)]-E[Y(0)]" else "E[Y(1)]/E[Y(0)]"
  needed  <- c(est_col, "2.5 %", "97.5 %")
  if (any(!needed %in% names(group)) || any(!needed %in% names(subgroup))) {
    stop("data frames must contain columns: ", paste(needed, collapse = ", "))
  }

  # helper to recover se from wald CI
  wald_se <- function(lo, hi) (hi - lo) / (2 * 1.96)
  bold_ci <- function(est, lo, hi, dp, null) {
    txt <- sprintf(paste0("%.", dp, "f [%.", dp, "f, %.", dp, "f]"), est, lo, hi)
    if (lo > null | hi < null) paste0("**", txt, "**") else txt
  }
  null_val <- ifelse(type == "RD", 0, 1)

  # compute difference or ratio for each outcome
  out_full <- purrr::pmap_dfr(list(
    est_A = group[[est_col]],
    est_B = subgroup[[est_col]],
    lo_A  = group$`2.5 %`,
    hi_A  = group$`97.5 %`,
    lo_B  = subgroup$`2.5 %`,
    hi_B  = subgroup$`97.5 %`
  ), function(est_A, est_B, lo_A, hi_A, lo_B, hi_B) {
    se_A <- wald_se(lo_A, hi_A)
    se_B <- wald_se(lo_B, hi_B)
    if (type == "RD") {
      stat <- est_B - est_A
      se_d <- sqrt(se_A^2 + se_B^2)
      lo   <- stat - 1.96 * se_d
      hi   <- stat + 1.96 * se_d
    } else {
      stat <- est_B / est_A
      se_ln <- sqrt((se_B / est_B)^2 + (se_A / est_A)^2)
      lo    <- exp(log(stat) - 1.96 * se_ln)
      hi    <- exp(log(stat) + 1.96 * se_ln)
    }
    tibble::tibble(
      delta    = stat,
      lo       = lo,
      hi       = hi,
      result   = bold_ci(stat, lo, hi, decimal_places, null_val),
      reliable = (lo > null_val) | (hi < null_val)
    )
  })

  # apply label mapping if provided
  rn <- rownames(group)
  if (!is.null(label_mapping)) {
    mapped <- label_mapping[rn]
    mapped[is.na(mapped)] <- rn[is.na(mapped)]
    rn <- vctrs::vec_recycle(mapped, size = length(rn))
  }
  out_full <- out_full %>%
    dplyr::mutate(outcome = rn) %>%
    dplyr::relocate(outcome)

  # prepare results table
  results_tbl <- out_full %>%
    dplyr::select(outcome, result) %>%
    dplyr::rename(Outcomes = outcome, `Group Differences` = result)

  # summarise interpretation
  reliable_rows <- out_full %>% dplyr::filter(reliable)
  if (nrow(reliable_rows) == nrow(out_full)) {
    interp <- glue::glue(
      "We found reliable treatment-effect differences comparing {group2_name} to {group1_name} for all outcomes."
    )
  } else if (nrow(reliable_rows) == 0) {
    interp <- glue::glue(
      "We did not find reliable treatment-effect differences comparing {group2_name} to {group1_name} for any outcome."
    )
  } else {
    entries <- purrr::map2_chr(
      reliable_rows$outcome,
      seq_len(nrow(reliable_rows)),
      function(name, idx) {
        row <- reliable_rows[idx, ]
        sprintf(
          "%s (%s vs %s): $\\delta$ = %.*f [%.*f, %.*f]",
          name, group2_name, group1_name,
          decimal_places, row$delta,
          decimal_places, row$lo,
          decimal_places, row$hi
        )
      }
    )
    entries_collapsed <- glue::glue_collapse(entries, sep = ", ", last = " and ")
    interp <- glue::glue(
      "We found reliable treatment-effect differences comparing {group2_name} to {group1_name} for {entries_collapsed}. ",
      "We did not find reliable differences for all other outcomes."
    )
  }

  list(results = results_tbl, interpretation = interp)
}

