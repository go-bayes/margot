#' Helper function to map requested model names to actual model names (handling flipped outcomes)
#' @importFrom cli cli_alert_info
#' 
#' @description
#' When outcomes are flipped in margot_causal_forest, they get an "_r" suffix.
#' This function helps map requested model names to their actual names in the results.
#' 
#' @param requested_names Character vector of requested model names
#' @param available_names Character vector of available model names in results
#' @param verbose Logical whether to print messages
#' 
#' @return List with:
#'   - mapped_names: character vector of matched model names
#'   - missing_names: character vector of requested names that couldn't be matched
#'   - mapping: named vector showing the mapping (names are requested, values are actual)
#' 
#' @keywords internal
#' @noRd
.map_model_names_with_flips <- function(requested_names, available_names, verbose = TRUE) {
  # ensure model names have the "model_" prefix
  requested_prefixed <- ifelse(
    grepl("^model_", requested_names),
    requested_names,
    paste0("model_", requested_names)
  )
  
  # initialize mapping
  mapping <- setNames(character(length(requested_prefixed)), requested_prefixed)
  
  # try to find each requested model
  for (i in seq_along(requested_prefixed)) {
    req_name <- requested_prefixed[i]
    
    # first try exact match
    if (req_name %in% available_names) {
      mapping[req_name] <- req_name
    } else {
      # try with _r suffix (flipped outcome)
      flipped_name <- paste0(req_name, "_r")
      if (flipped_name %in% available_names) {
        mapping[req_name] <- flipped_name
        if (verbose) {
          cli::cli_alert_info("Mapped {req_name} to flipped outcome {flipped_name}")
        }
      } else {
        # try removing _r suffix if it's already there
        if (grepl("_r$", req_name)) {
          base_name <- sub("_r$", "", req_name)
          if (base_name %in% available_names) {
            mapping[req_name] <- base_name
            if (verbose) {
              cli::cli_alert_info("Mapped {req_name} to base outcome {base_name}")
            }
          }
        }
      }
    }
  }
  
  # identify which were found and which are missing
  found_mask <- mapping != ""
  mapped_names <- mapping[found_mask]
  missing_names <- names(mapping)[!found_mask]
  
  # also check for the original unprefixed names in missing
  if (length(missing_names) > 0) {
    missing_original <- gsub("^model_", "", missing_names)
  } else {
    missing_original <- character(0)
  }
  
  return(list(
    mapped_names = unname(mapped_names),
    missing_names = missing_names,
    missing_original = missing_original,
    mapping = mapping[found_mask]
  ))
}

#' Check if any outcomes in a model result are flipped
#' 
#' @param models Output from margot_causal_forest
#' @return Logical vector indicating which models are flipped
#' @keywords internal
#' @noRd
.detect_flipped_models <- function(models) {
  model_names <- names(models$results)
  grepl("_r$", model_names)
}