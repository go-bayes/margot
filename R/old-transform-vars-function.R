
#' transform_var_name <- function(var_name, label_mapping = NULL,
#'                                remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
#'                                use_title_case = TRUE, remove_underscores = TRUE) {
#'   display_name <- var_name
#'
#'   # Remove 'model_' prefix if present (for model names)
#'   if (startsWith(display_name, "model_")) {
#'     display_name <- sub("^model_", "", display_name)
#'   }
#'
#'   # apply label mapping first, if exists
#'   if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
#'     mapped_label <- label_mapping[[display_name]]
#'     cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
#'     return(mapped_label)
#'   }
#'
#'   # else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
#'   if (startsWith(display_name, "t0_")) {
#'     t2_var <- sub("^t0_", "t2_", display_name)
#'     if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
#'       mapped_label <- label_mapping[[t2_var]]
#'       cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
#'       return(mapped_label)
#'     }
#'   }
#'
#'   # else, apply transformations
#'   if (remove_tx_prefix) {
#'     display_name <- sub("^t[0-9]+_", "", display_name)
#'   }
#'   if (remove_z_suffix) {
#'     display_name <- sub("_z$", "", display_name)
#'   }
#'   if (remove_underscores) {
#'     display_name <- gsub("_", " ", display_name)
#'   }
#'
#'   if (use_title_case) {
#'     display_name <- tools::toTitleCase(display_name)
#'     # Replace "Nz" with "NZ"
#'     display_name <- gsub("Nz", "NZ", display_name)
#'   }
#'
#'   # Notify if transformed
#'   if (display_name != var_name) {
#'     cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
#'   }
#'
#'   return(display_name)
#' }

