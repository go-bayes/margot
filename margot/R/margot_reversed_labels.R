#' Update label map by marking reversed outcomes
#'
#' Helper to tag reversed outcomes by appending "(reversed)" to labels.
#'
#' @param label_map named list mapping variable names to human-readable labels
#' @param reversed  character vector of variable names that have been flipped
#' @return           named list of labels, with "(reversed)" appended to specified entries
#' @export
#' @examples
#' label_mapping_all <- list(
#'   t2_log_hours_exercise_z        = "Hours of Exercise (log)",
#'   t2_hlth_fatigue_z              = "Fatigue",
#'   t2_kessler_latent_anxiety_z    = "Anxiety",
#'   t2_kessler_latent_depression_z = "Depression",
#'   t2_rumination_z                = "Rumination",
#'   t2_foregiveness_z              = "Forgiveness",
#'   t2_perfectionism_z             = "Perfectionism",
#'   t2_self_esteem_z               = "Self Esteem",
#'   t2_gratitude_z                 = "Gratitude",
#'   t2_lifesat_z                   = "Life Satisfaction",
#'   t2_meaning_purpose_z           = "Meaning: Purpose",
#'   t2_meaning_sense_z             = "Meaning: Sense",
#'   t2_pwi_z                       = "Personal Well-being Index",
#'   t2_belong_z                    = "Social Belonging",
#'   t2_neighbourhood_community_z   = "Neighbourhood Community",
#'   t2_support_z                   = "Social Support"
#' )
#'
#' flip_outcomes <- c(
#'   "t2_kessler_latent_anxiety_z",
#'   "t2_kessler_latent_depression_z",
#'   "t2_rumination_z"
#' )
#'
#' # update mapping
#' label_mapping_all <- mark_reversed_labels(label_mapping_all, flip_outcomes)
#'
#' print(label_mapping_all)
#' #> $t2_log_hours_exercise_z
#' #> [1] "Hours of Exercise (log)"
#' #> ...
#' #> $t2_kessler_latent_anxiety_z
#' #> [1] "Anxiety (reversed)"
#' #> ...
margot_reversed_labels <- function(label_map, reversed) {
  stopifnot(is.list(label_map), is.character(reversed))
  res <- lapply(names(label_map), function(var) {
    lbl <- label_map[[var]]
    if (var %in% reversed) {
      paste0(lbl, " (reversed)")
    } else {
      lbl
    }
  })
  names(res) <- names(label_map)
  res
}
