#' Interpret Marginal Causal Effect Estimates
#'
#' @md
#' @description
#' This function takes the output of `group_tab()` (with RD or RR estimates
#' plus E‑values) and produces a compact, per‑outcome markdown report. It only
#' reports those estimates with **Evidence** or **Strong evidence**, unless
#' `interpret_all_E_gt1 = TRUE`. Outcomes appear in the same order as you passed
#' via `order` (e.g. `"magnitude_desc"`, `"evaluebound_desc"`).
#'
#' @param df A data frame from `group_tab()`, including `outcome`, an effect column,
#'   `2.5 %`, `97.5 %`, `E_Value`, `E_Val_bound`, and optionally `unit`.
#' @param type One of `"RD"` or `"RR"`. Default: `"RD"`.
#' @param order One of
#'   `"alphabetical"`, `"magnitude_desc"`, `"magnitude_asc"`,
#'   `"evaluebound_desc"`, `"evaluebound_asc"`, `"custom"`, or `"default"`.
#'   (`"default"` → `"magnitude_desc"`; deprecated.)
#' @param original_df Optional raw data frame for back‑transforming to original scale.
#' @param interpret_all_E_gt1 Logical; if `TRUE`, include _all_ estimates with
#'   `E_Value > 1` & `E_Val_bound > 1`. Default: `FALSE`.
#'
#' @return A list with the following element:
#' \describe{
#'   \item{interpretation}{A markdown string with per‐outcome headings and
#'     sentences, plus a closing note about any remaining estimates.}
#' }
#'
#' @importFrom dplyr filter mutate case_when rowwise ungroup
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence
#' @importFrom rlang .data
#' @export
margot_interpret_marginal <- function(
    df,
    type = c("RD", "RR"),
    order = c(
      "alphabetical", "magnitude_desc", "magnitude_asc",
      "evaluebound_desc", "evaluebound_asc", "custom", "default"
    ),
    original_df = NULL,
    interpret_all_E_gt1 = FALSE
) {
  # validate arguments
  type  <- match.arg(type)
  order <- match.arg(order)
  if (order == "default") {
    warning("'default' is deprecated; using 'magnitude_desc' instead.")
    order <- "magnitude_desc"
  }

  message(glue::glue("starting interpretation with order = '{order}'..."))

  # apply sorting
  df <- group_tab(df, type = type, order = order)

  # ensure unit column
  if (!"unit" %in% names(df)) df$unit <- ""

  # back-transform if requested
  if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)

  # identify effect column
  effect_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
    "E[Y(1)]-E[Y(0)]"
  } else {
    "E[Y(1)]/E[Y(0)]"
  }
  has_orig <- paste0(effect_col, "_original") %in% names(df)
  null_val <- ifelse(type == "RR", 1, 0)

  # filter by evidence
  if (interpret_all_E_gt1) {
    df_f <- df %>%
      filter(E_Value > 1, E_Val_bound > 1) %>%
      mutate(evidence_strength = "Evidence")
  } else {
    df_f <- df %>%
      mutate(
        evidence_strength = case_when(
          (`2.5 %` > null_val & E_Val_bound > 2) |
            (`97.5 %` < null_val & E_Val_bound > 2) ~ "Strong evidence",
          (`2.5 %` > null_val & E_Val_bound > 1.1) |
            (`97.5 %` < null_val & E_Val_bound > 1.1) ~ "Evidence",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(evidence_strength))
  }

  # if nothing to report
  if (nrow(df_f) == 0) {
    message("no estimates meet evidence criteria.")
    return(list(
      interpretation = "No reliable causal evidence detected."
    ))
  }

  # build per-outcome text
  interp <- df_f %>%
    rowwise() %>%
    mutate(
      lab = glue::glue(
        "{round(.data[[effect_col]],3)} (",
        round(`2.5 %`,3), ", ", round(`97.5 %`,3), ")"
      ),
      lab_orig = if (has_orig) glue::glue(
        "{round(.data[[paste0(effect_col,'_original')]],3)} {unit} (",
        round(`2.5 %_original`,3), ", ", round(`97.5 %_original`,3), ")"
      ) else NA_character_,
      text = glue::glue(
        "#### {outcome}\n\n",
        "The effect ({type}) is {lab}. ",
        "{if (has_orig) paste0('On original scale, ', lab_orig, '. ')}",
        "E-value lower bound is {E_Val_bound}, indicating {tolower(evidence_strength)}."
      ),
      text = str_to_sentence(text)
    ) %>%
    ungroup()

  # reverse text order so narrative matches plot ordering # NEW
  if (grepl("_(asc|desc)$", order)) {
    interp <- interp[nrow(interp):1, ]
  }

  # combine
  body <- paste(interp$text, collapse = "\n\n")

  rem <- nrow(df) - nrow(df_f)
  if (rem > 0) {
    concl <- if (interpret_all_E_gt1) {
      "All other estimates presented unreliable evidence."
    } else {
      "All other estimates presented weak or unreliable evidence."
    }
    body <- paste(body, concl, sep = "\n\n")
  }

  message("interpretation complete 👍")
  list(interpretation = body)
}
# margot_interpret_marginal <- function(df,
#                                       type = c("RD", "RR"),
#                                       order = "alphabetical",
#                                       original_df = NULL,
#                                       interpret_all_E_gt1 = FALSE) {
#   type <- match.arg(type)
#
#   if (order == "default") {
#     warning("'default' order is deprecated. please use 'magnitude' instead.")
#     order <- "magnitude"
#   }
#
#   if (!order %in% c("alphabetical", "magnitude")) {
#     stop("invalid 'order' parameter. choose from 'alphabetical' or 'magnitude'.")
#   }
#
#   message("starting compact interpretation of causal effect estimates...")
#
#   df <- group_tab(df, type = type, order = order)
#
#   # fix after grouping or at the start of margot_interpret_marginal:
#   if (!"unit" %in% names(df)) {
#     df$unit <- rep("", nrow(df))
#   }
#
#   if (!is.null(original_df)) {
#     df <- back_transform_estimates(df, original_df)
#   }
#
#   effect_size_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else if ("E[Y(1)]/E[Y(0)]" %in% names(df)) {
#     "E[Y(1)]/E[Y(0)]"
#   } else {
#     stop("data must contain either 'E[Y(1)]-E[Y(0)]' or 'E[Y(1)]/E[Y(0)]' column.")
#   }
#
#   has_original_scale <- paste0(effect_size_col, "_original") %in% names(df)
#   null_value <- ifelse(type == "RR", 1, 0)
#
#   # when interpreting all estimates with E_Value > 1, require the lower bound to be > 1.
#   if (interpret_all_E_gt1) {
#     df_filtered <- df %>%
#       dplyr::filter(E_Value > 1, E_Val_bound > 1) %>%
#       dplyr::mutate(evidence_strength = "Evidence")
#   } else {
#     df_filtered <- df %>%
#       dplyr::mutate(
#         evidence_strength = dplyr::case_when(
#           (`2.5 %` > null_value & E_Val_bound > 2) | (`97.5 %` < null_value & E_Val_bound > 2) ~ "Strong evidence",
#           (`2.5 %` > null_value & E_Val_bound > 1.1) | (`97.5 %` < null_value & E_Val_bound > 1.1) ~ "Evidence",
#           TRUE ~ NA_character_
#         )
#       ) %>%
#       dplyr::filter(!is.na(evidence_strength))
#   }
#
#   total_outcomes <- nrow(df)
#   reported_outcomes <- nrow(df_filtered)
#   remaining_outcomes <- total_outcomes - reported_outcomes
#
#   if (nrow(df_filtered) == 0) {
#     message("no effect estimates with evidence or strong evidence for causality were found.")
#     return(list(interpretation = "No reliable causal evidence detected for the reported outcomes."))
#   }
#
#   interpretation <- df_filtered %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       unit = ifelse(!is.na(unit) & unit != "", unit, ""),
#       estimate_lab = paste0(
#         round(!!rlang::sym(effect_size_col), 3), " (",
#         round(`2.5 %`, 3), ", ",
#         round(`97.5 %`, 3), ")"
#       ),
#       estimate_lab_original = if (has_original_scale) {
#         if (unit != "") {
#           paste0(
#             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " ", unit, " (",
#             round(`2.5 %_original`, 3), " to ",
#             round(`97.5 %_original`, 3), " ", unit, ")"
#           )
#         } else {
#           paste0(
#             round(!!rlang::sym(paste0(effect_size_col, "_original")), 3), " (",
#             round(`2.5 %_original`, 3), ", ",
#             round(`97.5 %_original`, 3), ")"
#           )
#         }
#       } else {
#         NA_character_
#       },
#       outcome_interpretation = glue::glue(
#         "#### {outcome}\n\n",
#         "The effect estimate ({type}) is {estimate_lab}. ",
#         "{if (!is.na(estimate_lab_original)) paste0('On the original scale, the estimated effect is ', estimate_lab_original, '. ') else ''}",
#         "E-value lower bound is {E_Val_bound}, indicating {tolower(evidence_strength)} for causality."
#       ),
#       outcome_interpretation = stringr::str_to_sentence(outcome_interpretation)
#     ) %>%
#     dplyr::ungroup()
#
#   interpretation_text <- paste(interpretation$outcome_interpretation, collapse = "\n\n")
#
#   if (remaining_outcomes > 0) {
#     final_paragraph <- if (interpret_all_E_gt1) {
#       "\n\nAll other effect estimates presented unreliable evidence for causality."
#     } else {
#       "\n\nAll other effect estimates presented either weak or unreliable evidence for causality."
#     }
#     final_paragraph <- stringr::str_to_sentence(final_paragraph)
#     interpretation_text <- paste(interpretation_text, final_paragraph, sep = "\n\n")
#   }
#
#   message("compact interpretation completed successfully 👍")
#
#   return(list(
#     interpretation = interpretation_text
#   ))
# }
