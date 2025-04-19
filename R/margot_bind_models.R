#' Combine multiple batched model outputs
#'
#' @description
#' This function combines either “causal forest” or “lmtp” batched model outputs
#' into a single object, provided they share compatible structures.
#'
#' @param ... One or more batched model output objects.
#' @param quiet Logical; if TRUE, suppresses CLI feedback messages. Default is FALSE.
#'
#' @return A single combined model output object.
#'
#' @details
#' - If all inputs have the four elements `models`, `contrasts`,
#'   `individual_tables` and `combined_tables`, it assumes an **lmtp**‐style object
#'   and flattens each list, row‐binding the `combined_tables`.
#' - Otherwise it falls back on the original “causal forest” structure
#'   with elements `results`, `combined_table`, `outcome_vars`,
#'   `not_missing` and `full_models`.
#' - It checks that any `not_missing` vectors match exactly across inputs.
#'
#' @examples
#' # for lmtp outputs
#' combined_lmtp <- margot_bind_models(
#'   health_lmtp_output,
#'   psych_lmtp_output
#' )
#'
#' # for causal forest outputs
#' combined_cf <- margot_bind_models(
#'   models_binary_social,
#'   models_binary_psych
#' )
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom purrr map_lgl flatten map
#' @importFrom dplyr bind_rows
#' @export
margot_bind_models <- function(..., quiet = FALSE) {
  models <- list(...)

  if (length(models) < 1) {
    cli::cli_alert_danger("no models provided!")
    return(NULL)
  }
  if (length(models) == 1) {
    cli::cli_alert_warning("only one model provided, returning as is.")
    return(models[[1]])
  }

  # detect lmtp‐style structure
  lmtp_names <- c("models", "contrasts", "individual_tables", "combined_tables")
  is_lmtp <- purrr::map_lgl(models, ~ setequal(names(.x), lmtp_names))

  if (all(is_lmtp)) {
    if (!quiet) cli::cli_alert_info("combining {length(models)} lmtp outputs...")
    combined <- list(
      models = purrr::flatten(purrr::map(models, "models")),
      contrasts = purrr::flatten(purrr::map(models, "contrasts")),
      individual_tables = purrr::flatten(purrr::map(models, "individual_tables"))
    )
    # bind each combined_tables data.frame
    dfs <- purrr::map(models, ~ .x$combined_tables[[1]])
    combined$combined_tables <- list(
      combined_outcomes_religious_vs_secular = dplyr::bind_rows(dfs)
    )
    if (!quiet) cli::cli_alert_success("successfully combined {length(models)} lmtp outputs!")
    return(combined)
  }

  # otherwise, assume causal forest structure
  if (!quiet) cli::cli_alert_info("checking {length(models)} causal forest models...")
  expected_cf <- c("results", "combined_table", "outcome_vars", "not_missing", "full_models")
  ok_structure <- purrr::map_lgl(models, ~ all(names(.x) %in% expected_cf) &&
                                   length(names(.x)) == length(expected_cf))
  if (!all(ok_structure)) {
    cli::cli_alert_danger("not all models have the expected causal forest structure!")
    return(NULL)
  }

  # ensure all not_missing vectors match exactly
  nm <- purrr::map(models, "not_missing")
  if (!all(purrr::map_lgl(nm[-1], ~ identical(.x, nm[[1]])))) {
    cli::cli_alert_danger("the 'not_missing' vectors differ across models!")
    return(NULL)
  }
  if (!quiet) cli::cli_alert_success("all causal forest models are compatible!")

  # combine
  if (!quiet) cli::cli_alert_info("combining causal forest models...")
  combined <- list(
    results       = unlist(purrr::map(models, "results"),       recursive = FALSE),
    combined_table= dplyr::bind_rows(purrr::map(models, "combined_table")),
    outcome_vars  = unlist(purrr::map(models, "outcome_vars")),
    not_missing   = models[[1]]$not_missing,
    full_models   = unlist(purrr::map(models, "full_models"),   recursive = FALSE)
  )

  if (!quiet) {
    cli::cli_alert_success("successfully combined {length(models)} causal forest models!")
    cli::cli_alert_info("combined object has {length(combined$results)} individual models.")
  }
  combined
}
