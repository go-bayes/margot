#' Update label map by marking reversed outcomes
#'
#' Helper to tag reversed outcomes by appending "(reversed)" to labels.
#' This function now handles the new "_r" suffix convention for flipped models.
#'
#' @param label_map named list mapping variable names to human-readable labels
#' @param reversed  character vector of variable names that have been flipped
#' @param use_r_suffix logical; if TRUE (default), creates new entries with "_r" suffix 
#'        for flipped outcomes. If FALSE, uses the old behavior of modifying in-place.
#' @param remove_original logical; if TRUE and use_r_suffix is TRUE, removes the original
#'        (non-flipped) entries from the label map. Default is FALSE.
#' @return named list of labels, with "(reversed)" appended to specified entries
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
#' # new behavior (default): creates entries with _r suffix
#' label_mapping_new <- margot_reversed_labels(label_mapping_all, flip_outcomes)
#' print(label_mapping_new[c("t2_kessler_latent_anxiety_z", "t2_kessler_latent_anxiety_z_r")])
#' #> $t2_kessler_latent_anxiety_z
#' #> [1] "Anxiety"
#' #> $t2_kessler_latent_anxiety_z_r
#' #> [1] "Anxiety (reversed)"
#'
#' # old behavior: modifies in place
#' label_mapping_old <- margot_reversed_labels(label_mapping_all, flip_outcomes, use_r_suffix = FALSE)
#' print(label_mapping_old[["t2_kessler_latent_anxiety_z"]])
#' #> [1] "Anxiety (reversed)"
#' 
#' # remove original entries
#' label_mapping_clean <- margot_reversed_labels(label_mapping_all, flip_outcomes, remove_original = TRUE)
#' # original anxiety entry removed, only _r version remains
margot_reversed_labels <- function(label_map, reversed, use_r_suffix = TRUE, remove_original = FALSE) {
  stopifnot(is.list(label_map), is.character(reversed))
  
  if (!use_r_suffix) {
    # old behavior: modify labels in-place
    res <- lapply(names(label_map), function(var) {
      lbl <- label_map[[var]]
      if (var %in% reversed) {
        paste0(lbl, " (reversed)")
      } else {
        lbl
      }
    })
    names(res) <- names(label_map)
    return(res)
  }
  
  # new behavior: create entries with "_r" suffix
  res <- label_map
  
  # process each reversed outcome
  for (var in reversed) {
    # handle both with and without "model_" prefix
    var_base <- gsub("^model_", "", var)
    
    # find the original label - check various possibilities
    original_label <- NULL
    if (!is.null(label_map[[var]])) {
      original_label <- label_map[[var]]
    } else if (!is.null(label_map[[var_base]])) {
      original_label <- label_map[[var_base]]
    } else if (!is.null(label_map[[paste0("model_", var)]])) {
      original_label <- label_map[[paste0("model_", var)]]
    }
    
    if (!is.null(original_label)) {
      # add new entry with _r suffix
      new_key <- paste0(var_base, "_r")
      res[[new_key]] <- paste0(original_label, " (reversed)")
      
      # also add with model_ prefix if that's the pattern
      if (any(grepl("^model_", names(label_map)))) {
        res[[paste0("model_", new_key)]] <- paste0(original_label, " (reversed)")
      }
    }
  }
  
  # optionally remove original entries
  if (remove_original) {
    vars_to_remove <- c(reversed, gsub("^model_", "", reversed), paste0("model_", reversed))
    res <- res[!names(res) %in% vars_to_remove]
  }
  
  res
}
