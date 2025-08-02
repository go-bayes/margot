#' Combine multiple batched model outputs (with covariates & metadata)
#'
#' @description
#' This function combines either "causal forest" or "lmtp" batched model outputs
#' into a single object, provided they share compatible structures.  It now also
#' preserves `covariates`, `data`, `weights`, and any flip or rescue metadata
#' when binding causal forest outputs.
#'
#' @param ... One or more batched model output objects.
#' @param quiet Logical; if TRUE, suppresses CLI feedback messages. Default is FALSE.
#'
#' @return A single combined model output object.
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom purrr map_lgl flatten map map_chr
#' @importFrom dplyr bind_rows
#' @export
margot_bind_models <- function(..., quiet = FALSE) {
  models <- list(...)
  n_models <- length(models)

  if (n_models < 1) {
    cli::cli_alert_danger("no models provided!")
    return(NULL)
  }
  if (n_models == 1) {
    cli::cli_alert_warning("only one model provided, returning as is.")
    return(models[[1]])
  }

  # detect lmtp-style structure
  lmtp_names <- c("models", "contrasts", "individual_tables", "combined_tables")
  is_lmtp <- purrr::map_lgl(models, ~ setequal(names(.x), lmtp_names))

  if (all(is_lmtp)) {
    if (!quiet) cli::cli_alert_info("combining {n_models} lmtp outputs...")
    combined <- list(
      models            = purrr::flatten(purrr::map(models, "models")),
      contrasts         = purrr::flatten(purrr::map(models, "contrasts")),
      individual_tables = purrr::flatten(purrr::map(models, "individual_tables"))
    )
    tbl_names <- unique(unlist(purrr::map(models, ~ names(.x$combined_tables))))
    combined$combined_tables <- purrr::set_names(
      purrr::map(tbl_names, function(tbl) {
        dplyr::bind_rows(purrr::map(models, ~ .x$combined_tables[[tbl]]))
      }),
      tbl_names
    )
    if (!quiet) cli::cli_alert_success("successfully combined {n_models} lmtp outputs!")
    return(combined)
  }

  # assume causal forest structure
  if (!quiet) cli::cli_alert_info("checking {n_models} causal forest models...")
  cf_fields <- c("results", "combined_table", "outcome_vars", "not_missing", "full_models")
  ok_cf <- purrr::map_lgl(models, ~ all(cf_fields %in% names(.x)))
  if (!all(ok_cf)) {
    cli::cli_alert_danger("not all models have the expected causal forest structure!")
    return(NULL)
  }

  # check not_missing match
  nm_list <- purrr::map(models, "not_missing")
  if (!all(purrr::map_lgl(nm_list[-1], ~ identical(.x, nm_list[[1]])))) {
    cli::cli_alert_danger("the 'not_missing' vectors differ across models!")
    return(NULL)
  }
  if (!quiet) cli::cli_alert_success("all causal forest models are compatible!")

  if (!quiet) cli::cli_alert_info("combining causal forest models...")
  combined <- list(
    results        = unlist(purrr::map(models, "results"), recursive = FALSE),
    combined_table = dplyr::bind_rows(purrr::map(models, "combined_table")),
    outcome_vars   = unlist(purrr::map(models, "outcome_vars")),
    not_missing    = models[[1]]$not_missing,
    full_models    = unlist(purrr::map(models, "full_models"), recursive = FALSE)
  )

  # preserve covariates/data if present and identical
  if (all(purrr::map_lgl(models, ~ !is.null(.x$covariates)))) {
    covs <- purrr::map(models, "covariates")
    if (all(purrr::map_lgl(covs[-1], ~ identical(.x, covs[[1]])))) {
      combined$covariates <- covs[[1]]
      if (!quiet) cli::cli_alert_info("covariates preserved in combined object")
    }
  }
  if (all(purrr::map_lgl(models, ~ !is.null(.x$data)))) {
    datas <- purrr::map(models, "data")
    if (all(purrr::map_lgl(datas[-1], ~ identical(.x, datas[[1]])))) {
      combined$data <- datas[[1]]
      if (!quiet) cli::cli_alert_info("data preserved in combined object")
    }
  }
  # carry over flip/rescue metadata if any
  flips <- purrr::map(models, ~ .x$flip_outcomes)
  flips <- unique(unlist(flips))
  if (length(flips)) combined$flip_outcomes <- flips

  if (!quiet) {
    cli::cli_alert_success("successfully combined {n_models} causal forest models!")
    cli::cli_alert_info("combined object has {length(combined$results)} individual models.")
  }
  combined
}


# margot_bind_models <- function(..., quiet = FALSE) {
#   models <- list(...)
#
#   if (length(models) < 1) {
#     cli::cli_alert_danger("no models provided!")
#     return(NULL)
#   }
#   if (length(models) == 1) {
#     cli::cli_alert_warning("only one model provided, returning as is.")
#     return(models[[1]])
#   }
#
#   # detect lmtp‐style structure
#   lmtp_names <- c("models", "contrasts", "individual_tables", "combined_tables")
#   is_lmtp <- purrr::map_lgl(models, ~ setequal(names(.x), lmtp_names))
#
#   if (all(is_lmtp)) {
#     if (!quiet) cli::cli_alert_info("combining {length(models)} lmtp outputs...")
#
#     # flatten the simple list components
#     combined <- list(
#       models            = purrr::flatten(purrr::map(models, "models")),
#       contrasts         = purrr::flatten(purrr::map(models, "contrasts")),
#       individual_tables = purrr::flatten(purrr::map(models, "individual_tables"))
#     )
#
#     # --- new dynamic handling of combined_tables ---
#     # 1. collect every table‐name that appears in any model
#     tbl_names <- unique(unlist(purrr::map(models, ~ names(.x$combined_tables))))
#     # 2. for each name, bind_rows across all models
#     combined$combined_tables <- purrr::set_names(
#       purrr::map(tbl_names, function(tbl) {
#         dplyr::bind_rows(purrr::map(models, ~ .x$combined_tables[[tbl]]))
#       }),
#       tbl_names
#     )
#     # -------------------------------------------------
#
#     if (!quiet) cli::cli_alert_success("successfully combined {length(models)} lmtp outputs!")
#     return(combined)
#   }
#
#   # otherwise, assume causal forest structure
#   if (!quiet) cli::cli_alert_info("checking {length(models)} causal forest models...")
#   expected_cf <- c("results", "combined_table", "outcome_vars", "not_missing", "full_models")
#   ok_structure <- purrr::map_lgl(models, ~ all(names(.x) %in% expected_cf) &&
#                                    length(names(.x)) == length(expected_cf))
#   if (!all(ok_structure)) {
#     cli::cli_alert_danger("not all models have the expected causal forest structure!")
#     return(NULL)
#   }
#
#   # ensure all not_missing vectors match exactly
#   nm <- purrr::map(models, "not_missing")
#   if (!all(purrr::map_lgl(nm[-1], ~ identical(.x, nm[[1]])))) {
#     cli::cli_alert_danger("the 'not_missing' vectors differ across models!")
#     return(NULL)
#   }
#   if (!quiet) cli::cli_alert_success("all causal forest models are compatible!")
#
#   # combine causal forest models
#   if (!quiet) cli::cli_alert_info("combining causal forest models...")
#   combined <- list(
#     results        = unlist(purrr::map(models, "results"),       recursive = FALSE),
#     combined_table = dplyr::bind_rows(purrr::map(models, "combined_table")),
#     outcome_vars   = unlist(purrr::map(models, "outcome_vars")),
#     not_missing    = models[[1]]$not_missing,
#     full_models    = unlist(purrr::map(models, "full_models"),   recursive = FALSE)
#   )
#
#   if (!quiet) {
#     cli::cli_alert_success("successfully combined {length(models)} causal forest models!")
#     cli::cli_alert_info("combined object has {length(combined$results)} individual models.")
#   }
#   combined
# }
