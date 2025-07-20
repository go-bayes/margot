#' Get display labels for multiple variable names
#'
#' Helper to map a vector of variable names to human-readable labels.
#' Falls back on `transform_var_name()` when no explicit mapping is found.
#'
#' @param vars Character vector of variable names to convert.
#' @param label_map Named list mapping variable names to labels (e.g., `label_mapping_all`).
#' @return Character vector of display labels, in the same order as `vars`.
#' @details If an entry of `vars` is not present in `label_map`, this function
#' calls  `transform_var_name()` to auto-generate a label based on naming conventions.
#' @importFrom purrr map_chr
#' @export
margot_get_labels <- function(vars, label_map) {
  purrr::map_chr(vars, ~ transform_var_name(.x, label_map))
}

# illustration
# flip_outcomes <- c(
#   "t2_hlth_sleep_hours_z", "t2_log_hours_exercise_z",
#   "t2_short_form_health_z",  "t2_bodysat_z"
# )
# flip_outcome_labels <- margot_get_labels(flip_outcomes, label_mapping_all)
# print(flip_outcome_labels)
