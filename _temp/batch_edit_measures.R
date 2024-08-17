#' Batch Edit Measures
#'
#' This function performs a batch edit operation on a list of measures. It can edit
#' both character fields and list fields within each measure.
#'
#' @param measures_data A list of measures, where each measure is itself a list.
#' @param field The name of the field to edit in each measure.
#' @param old_value The value to be replaced.
#' @param new_value The value to replace the old value with.
#'
#' @return A modified list of measures with the specified edits applied.
#'
#' @details
#' The function iterates through all measures in the input list. For each measure,
#' if the specified field exists, it checks if the field is a character or a list.
#' For character fields, it replaces the old value with the new value if there's a match.
#' For list fields, it iterates through the list and replaces any matching values.
#'
#' @examples
#' \dontrun{
#' # Example: Change all references from "[@nzavs2009]" to "[@nzavs2021]"
#' measures_data <- list(
#'   measure1 = list(name = "Measure 1", reference = "[@nzavs2009]"),
#'   measure2 = list(name = "Measure 2", reference = "[@nzavs2009]"),
#'   measure3 = list(name = "Measure 3", reference = "Other reference")
#' )
#'
#' updated_measures <- batch_edit_measures(measures_data, "reference", "[@nzavs2009]", "[@nzavs2021]")
#' # This would change the reference in measure1 and measure2, but not measure3
#' }
#'
#' @export
batch_edit_measures <- function(measures_data, field, old_value, new_value) {
  edited_count <- 0

  for (measure_name in names(measures_data)) {
    measure <- measures_data[[measure_name]]
    if (field %in% names(measure)) {
      if (is.character(measure[[field]])) {
        if (measure[[field]] == old_value) {
          measures_data[[measure_name]][[field]] <- new_value
          edited_count <- edited_count + 1
        }
      } else if (is.list(measure[[field]])) {
        for (i in seq_along(measure[[field]])) {
          if (measure[[field]][[i]] == old_value) {
            measures_data[[measure_name]][[field]][[i]] <- new_value
            edited_count <- edited_count + 1
          }
        }
      }
    }
  }

  cat("Edited", edited_count, "entries.\n")
  return(measures_data)
}
