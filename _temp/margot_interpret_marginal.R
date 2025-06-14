#' Interpret Marginal Causal Effect Estimates
#'
#' @md
#' @description
#' this function takes the output of `group_tab()` (with rd or rr estimates and e‑values)
#' and returns a markdown bulleted list of outcomes that exhibited causal evidence.
#' it notes that outcomes are presented in the same order as they appear in the plot.
#'
#' @param df a data frame from `group_tab()`, including `outcome`, an effect column,
#'   `2.5 %`, `97.5 %`, `E_Value`, `E_Val_bound`, and optionally `unit`.
#' @param type one of `"RD"` or `"RR"`. default: `"RD"`.
#' @param order one of
#'   `"alphabetical"`, `"magnitude_desc"`, `"magnitude_asc"`,
#'   `"evaluebound_desc"`, `"evaluebound_asc"`, `"custom"`, or `"default"`.
#'   (`"default"` → `"magnitude_desc"`; deprecated.)
#' @param original_df optional raw data frame for back‑transforming to original scale.
#' @param interpret_all_E_gt1 logical; if `TRUE`, include _all_ estimates with
#'   `E_Value > 1` & `E_Val_bound > 1`. default: `FALSE`.
#'
#' @return a list with one element:
#' \describe{
#'   \item{interpretation}{a markdown string with a bulleted list of outcomes.}
#' }
#'
#' @importFrom dplyr filter mutate case_when rowwise ungroup pull
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

  # sort and back-transform as needed
  df <- group_tab(df, type = type, order = order)
  if (!"unit" %in% names(df)) df$unit <- ""
  if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)

  # determine effect column
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
      mutate(evidence_strength = "causal evidence")
  } else {
    df_f <- df %>%
      mutate(evidence_strength = case_when(
        (`2.5 %` > null_val & E_Val_bound > 2) |
          (`97.5 %` < null_val & E_Val_bound > 2) ~ "strong causal evidence",
        (`2.5 %` > null_val & E_Val_bound > 1.1) |
          (`97.5 %` < null_val & E_Val_bound > 1.1) ~ "causal evidence",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(evidence_strength))
  }

  # handle no evidence
  if (nrow(df_f) == 0) {
    message("no estimates meet evidence criteria.")
    return(list(interpretation = "No reliable causal evidence was detected for any outcome."))
  }

  # # reverse so bullets match plot
  # if (grepl("_(asc|desc)$", order)) df_f <- df_f[nrow(df_f):1, ]
  #
  # choose intro based on evidence types
  # strengths <- unique(df_f$evidence_strength)
  # if (all(strengths == "strong causal evidence")) {
  #   intro <- "The following outcomes exhibited strong causal evidence (presented in the same order as they appear in the results table):\n"
  # } else {
  #   intro <- "The following outcomes exhibited causal evidence (presented in the same order as they appear in the plot):\n"
  # }

  strengths <- unique(df_f$evidence_strength)
  if (all(strengths == "strong causal evidence")) {
    intro <- "The following outcomes exhibited strong causal evidence:\n"
  } else {
    intro <- "The following outcomes exhibited causal evidence:\n"
  }
  # build bullets without trailing evidence phrase
  bullets <- df_f %>%
    rowwise() %>%
    mutate(
      lab = glue::glue(
        "{round(.data[[effect_col]], 3)} (",
        round(`2.5 %`, 3), ", ", round(`97.5 %`, 3), ")"
      ),
      lab_orig = if (has_orig) glue::glue(
        "{round(.data[[paste0(effect_col, '_original')]], 3)} {unit} (",
        round(`2.5 %_original`, 3), ", ", round(`97.5 %_original`, 3), ")"
      ) else NA_character_,
      text = glue::glue(
        "- {outcome}: {type} = {lab}",
        "{if (has_orig) glue('; original scale: {lab_orig}')}",
        "; E-value lower bound = {E_Val_bound}"
      )
    ) %>%
    ungroup() %>%
    pull(text)

  # concluding line
  rem <- nrow(df) - nrow(df_f)
  conclusion <- if (rem > 0) {
    "Other estimates were either weak or unreliable."
  } else {
    ""
  }

  # assemble output
  parts <- c(intro, bullets)
  if (conclusion != "") parts <- c(parts, "", conclusion)
  interpretation <- paste(parts, collapse = "\n")

  message("interpretation complete 👍")
  list(interpretation = interpretation)
}

# margot_interpret_marginal <- function(
#     df,
#     type = c("RD", "RR"),
#     order = c(
#       "alphabetical", "magnitude_desc", "magnitude_asc",
#       "evaluebound_desc", "evaluebound_asc", "custom", "default"
#     ),
#     original_df = NULL,
#     interpret_all_E_gt1 = FALSE
# ) {
#   # validate arguments
#   type  <- match.arg(type)
#   order <- match.arg(order)
#   if (order == "default") {
#     warning("'default' is deprecated; using 'magnitude_desc' instead.")
#     order <- "magnitude_desc"
#   }
#   message(glue::glue("starting interpretation with order = '{order}'..."))
#
#   # sort and back-transform as needed
#   df <- group_tab(df, type = type, order = order)
#   if (!"unit" %in% names(df)) df$unit <- ""
#   if (!is.null(original_df)) df <- back_transform_estimates(df, original_df)
#
#   # determine effect column
#   effect_col <- if ("E[Y(1)]-E[Y(0)]" %in% names(df)) {
#     "E[Y(1)]-E[Y(0)]"
#   } else {
#     "E[Y(1)]/E[Y(0)]"
#   }
#   has_orig <- paste0(effect_col, "_original") %in% names(df)
#   null_val <- ifelse(type == "RR", 1, 0)
#
#   # filter by evidence
#   if (interpret_all_E_gt1) {
#     df_f <- df %>%
#       filter(E_Value > 1, E_Val_bound > 1) %>%
#       mutate(evidence_strength = "causal evidence")
#   } else {
#     df_f <- df %>%
#       mutate(evidence_strength = case_when(
#         (`2.5 %` > null_val & E_Val_bound > 2) |
#           (`97.5 %` < null_val & E_Val_bound > 2) ~ "strong causal evidence",
#         (`2.5 %` > null_val & E_Val_bound > 1.1) |
#           (`97.5 %` < null_val & E_Val_bound > 1.1) ~ "causal evidence",
#         TRUE ~ NA_character_
#       )) %>%
#       filter(!is.na(evidence_strength))
#   }
#
#   # handle no evidence
#   if (nrow(df_f) == 0) {
#     message("no estimates meet evidence criteria.")
#     return(list(interpretation = "No reliable causal evidence was detected for any outcome."))
#   }
#
#   # reverse so bullets match plot
#   if (grepl("_(asc|desc)$", order)) df_f <- df_f[nrow(df_f):1, ]
#
#   # choose intro based on evidence types
#   # strengths <- unique(df_f$evidence_strength)
#   # if (all(strengths == "strong causal evidence")) {
#   #   intro <- "The following outcomes exhibited strong causal evidence (presented in the same order as they appear in the results table):\n"
#   # } else {
#   #   intro <- "The following outcomes exhibited causal evidence (presented in the same order as they appear in the plot):\n"
#   # }
#
#   strengths <- unique(df_f$evidence_strength)
#     if (all(strengths == "strong causal evidence")) {
#       intro <- "The following outcomes exhibited strong causal evidence:\n"
#       } else {
#         intro <- "The following outcomes exhibited causal evidence:\n"
#         }
#   # build bullets without trailing evidence phrase
#   bullets <- df_f %>%
#     rowwise() %>%
#     mutate(
#       lab = glue::glue(
#         "{round(.data[[effect_col]], 3)} (",
#         round(`2.5 %`, 3), ", ", round(`97.5 %`, 3), ")"
#       ),
#       lab_orig = if (has_orig) glue::glue(
#         "{round(.data[[paste0(effect_col, '_original')]], 3)} {unit} (",
#         round(`2.5 %_original`, 3), ", ", round(`97.5 %_original`, 3), ")"
#       ) else NA_character_,
#       text = glue::glue(
#         "- {outcome}: {type} = {lab}",
#         "{if (has_orig) glue('; original scale: {lab_orig}')}",
#         "; E-value lower bound = {E_Val_bound}"
#       )
#     ) %>%
#     ungroup() %>%
#     pull(text)
#
#   # concluding line
#   rem <- nrow(df) - nrow(df_f)
#   conclusion <- if (rem > 0) {
#     "Other estimates were either weak or unreliable."
#   } else {
#     ""
#   }
#
#   # assemble output
#   parts <- c(intro, bullets)
#   if (conclusion != "") parts <- c(parts, "", conclusion)
#   interpretation <- paste(parts, collapse = "\n")
#
#   message("interpretation complete 👍")
#   list(interpretation = interpretation)
# }
