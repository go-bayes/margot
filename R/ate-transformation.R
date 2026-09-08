# resolve saved outcome scales and preserve the scientific reporting quantity

# find the retained source contrast column in a marginal reporting table
#' @keywords internal
margot_ate_effect_column <- function(data) {
  candidates <- c("ATE", "ATT", "ATC", "ATO", "E[Y|A]", "E[Y(1)]-E[Y(0)]", "E[Y(1)]/E[Y(0)]")
  found <- intersect(candidates, names(data))
  if (!length(found)) stop("No supported effect column was supplied.", call. = FALSE)
  found[[1L]]
}

# infer a legacy scale only from a declared suffix and a matching source column
#' @keywords internal
margot_ate_legacy_scale <- function(outcome, original_df) {
  key <- sub("^model_", "", outcome)
  orientation <- if (grepl("_r$", key)) -1 else 1
  key <- sub("_r$", "", key)
  standardised <- grepl("_z$", key)
  source <- sub("_z$", "", key)
  logged <- grepl("(^|_)log_", source)
  candidates <- unique(c(source, sub("^t[0-9]+_", "", source)))
  found <- intersect(candidates, names(original_df))
  if (!length(found)) {
    stop("No matching unstandardised source column for outcome '", outcome,
         "'; supply scale_info with the saved preparation constants.", call. = FALSE)
  }
  values <- original_df[[found[[1L]]]]
  if (!is.numeric(values)) stop("Scale source must be numeric: ", found[[1L]], call. = FALSE)
  if (length(found) > 1L && !all(vapply(found[-1L], function(name) {
    isTRUE(all.equal(as.numeric(original_df[[name]]), as.numeric(values)))
  }, logical(1)))) stop("Ambiguous source columns for outcome '", outcome, "'.", call. = FALSE)
  saved_center <- attr(values, if (logged) "log_mean" else "scaled:center", exact = TRUE)
  saved_scale <- attr(values, if (logged) "log_sd" else "scaled:scale", exact = TRUE)
  center <- if (standardised) {
    if (!is.null(saved_center)) as.numeric(saved_center) else mean(values, na.rm = TRUE)
  } else 0
  scale <- if (standardised) {
    if (!is.null(saved_scale)) as.numeric(saved_scale) else stats::sd(values, na.rm = TRUE)
  } else 1
  hours <- grepl("(^|_)hours_", source)
  data.frame(outcome = outcome, transformation = if (logged) "log1p" else "identity",
             center = center, scale = scale, orientation = orientation,
             unit = if (hours) "minutes" else "", unit_multiplier = if (hours) 60 else 1,
             source_column = found[[1L]], stringsAsFactors = FALSE)
}

# validate one explicit transformation row for every source outcome key
#' @keywords internal
margot_ate_scale_info <- function(outcomes, scale_info = NULL, original_df = NULL) {
  if (is.null(scale_info)) {
    if (!is.null(original_df) && length(outcomes)) {
      warning("Inferring legacy transformations from outcome names and original_df; scales may be recomputed. Supply scale_info with saved preparation constants for reproducible reporting. Legacy log names mean log1p.", call. = FALSE)
      scale_info <- do.call(rbind, lapply(unique(outcomes), margot_ate_legacy_scale, original_df = original_df))
    } else {
      scale_info <- data.frame(outcome = unique(outcomes), stringsAsFactors = FALSE)
    }
  }
  if (!is.data.frame(scale_info) || !"outcome" %in% names(scale_info)) {
    stop("scale_info must be a data frame keyed by outcome.", call. = FALSE)
  }
  scale_info$outcome <- as.character(scale_info$outcome)
  if (anyNA(scale_info$outcome) || any(!nzchar(scale_info$outcome)) || anyDuplicated(scale_info$outcome)) {
    stop("scale_info requires unique, non-missing outcome keys.", call. = FALSE)
  }
  absent <- setdiff(outcomes, scale_info$outcome)
  if (length(absent)) stop("scale_info is missing outcomes: ", paste(absent, collapse = ", "), call. = FALSE)
  defaults <- list(transformation = "identity", center = 0, scale = 1, orientation = 1,
                   unit = "", unit_multiplier = 1)
  for (name in names(defaults)) if (!name %in% names(scale_info)) scale_info[[name]] <- rep(defaults[[name]], nrow(scale_info))
  scale_info <- scale_info[match(outcomes, scale_info$outcome), , drop = FALSE]
  rownames(scale_info) <- NULL
  scale_info$transformation <- as.character(scale_info$transformation)
  scale_info$unit <- as.character(scale_info$unit)
  if (anyNA(scale_info$transformation) || any(!scale_info$transformation %in% c("identity", "log", "log1p"))) {
    stop("transformation must be identity, log, or log1p.", call. = FALSE)
  }
  for (name in c("center", "scale", "orientation", "unit_multiplier")) {
    if (!is.numeric(scale_info[[name]]) || any(!is.finite(scale_info[[name]]))) {
      stop("scale_info ", name, " must contain finite numeric values.", call. = FALSE)
    }
  }
  if (any(scale_info$scale <= 0) || any(scale_info$unit_multiplier <= 0)) stop("scale and unit_multiplier must be positive.", call. = FALSE)
  if (any(!scale_info$orientation %in% c(-1, 1))) stop("orientation must be -1 or 1.", call. = FALSE)
  if (anyNA(scale_info$unit)) stop("unit must not be missing.", call. = FALSE)
  scale_info
}

# map supplied contrasts and interval endpoints without changing source estimates
#' @keywords internal
margot_prepare_ate_reporting <- function(results_df, original_df = NULL, scale_info = NULL, type = "RD") {
  type <- match.arg(type, c("RD", "RR"))
  effect <- margot_ate_effect_column(results_df)
  if (effect == "E[Y(1)]/E[Y(0)]") type <- "RR"
  if (effect == "E[Y(1)]-E[Y(0)]" && type != "RD") stop("Difference column conflicts with type = 'RR'.", call. = FALSE)
  if (!all(c("2.5 %", "97.5 %") %in% names(results_df))) stop("Supplied confidence interval endpoints are required.", call. = FALSE)
  if (!"outcome" %in% names(results_df)) results_df$outcome <- rownames(results_df)
  if (!"original_var_name" %in% names(results_df)) results_df$original_var_name <- as.character(results_df$outcome)
  keys <- as.character(results_df$original_var_name)
  if (anyNA(keys) || any(!nzchar(keys))) stop("Outcome keys must not be missing.", call. = FALSE)
  metadata <- margot_ate_scale_info(keys, scale_info, original_df)
  estimate <- results_df[[effect]]
  lower <- results_df[["2.5 %"]]
  upper <- results_df[["97.5 %"]]
  if (!all(vapply(list(estimate, lower, upper), is.numeric, logical(1)))) stop("Estimates and interval endpoints must be numeric.", call. = FALSE)
  if (any(lower > upper, na.rm = TRUE)) stop("Confidence interval endpoints are not ordered.", call. = FALSE)
  nonlinear <- metadata$transformation != "identity"
  if (type == "RR") {
    if (any(nonlinear | metadata$center != 0 | metadata$orientation != 1)) {
      stop("Original-scale ratio reporting requires an identity transformation, zero centre, and unchanged orientation; a centred or nonlinear outcome ratio is insufficient.", call. = FALSE)
    }
    if (any(c(estimate, lower, upper) < 0, na.rm = TRUE)) stop("Risk ratios and interval endpoints must be non-negative.", call. = FALSE)
    reported <- estimate
    reported_lower <- lower
    reported_upper <- upper
    quantity <- rep("risk_ratio", length(keys))
    units <- rep("ratio", length(keys))
  } else {
    multiplier <- metadata$orientation * metadata$scale
    reported <- multiplier * estimate
    endpoints_a <- multiplier * lower
    endpoints_b <- multiplier * upper
    reported_lower <- pmin(endpoints_a, endpoints_b)
    reported_upper <- pmax(endpoints_a, endpoints_b)
    affine <- !nonlinear
    reported[affine] <- reported[affine] * metadata$unit_multiplier[affine]
    reported_lower[affine] <- reported_lower[affine] * metadata$unit_multiplier[affine]
    reported_upper[affine] <- reported_upper[affine] * metadata$unit_multiplier[affine]
    reported[nonlinear] <- exp(reported[nonlinear])
    reported_lower[nonlinear] <- exp(reported_lower[nonlinear])
    reported_upper[nonlinear] <- exp(reported_upper[nonlinear])
    quantity <- ifelse(metadata$transformation == "log", "geometric_mean_ratio",
                       ifelse(metadata$transformation == "log1p", "shifted_geometric_mean_ratio", "mean_difference"))
    units <- metadata$unit
    units[metadata$transformation == "log"] <- "ratio of geometric means of Y"
    units[metadata$transformation == "log1p"] <- "ratio of geometric means of Y + 1"
  }
  results_df$reported_estimate <- reported
  results_df$reported_lower <- reported_lower
  results_df$reported_upper <- reported_upper
  results_df$reporting_quantity <- quantity
  results_df$reporting_unit <- units
  compatible <- !nonlinear
  results_df[[paste0(effect, "_original")]] <- ifelse(compatible, reported, NA_real_)
  results_df[["2.5 %_original"]] <- ifelse(compatible, reported_lower, NA_real_)
  results_df[["97.5 %_original"]] <- ifelse(compatible, reported_upper, NA_real_)
  results_df$unit <- ifelse(compatible, units, NA_character_)
  attr(results_df, "report_scale_info") <- metadata
  results_df
}


# refuse unsupported legacy policy conversions while leaving model-scale reports available
#' @keywords internal
margot_assert_policy_reporting_scale <- function(model_name, original_df) {
  if (is.null(original_df)) return(invisible(NULL))
  nonlinear <- grepl("(^|_)log_", model_name)
  reversed <- grepl("_r$", model_name)
  if (any(nonlinear | reversed)) {
    stop("Original-scale policy reporting does not support nonlinear or reversed outcomes. Use original_df = NULL for model-scale reporting; an arithmetic original-unit policy contrast cannot be recovered by this legacy conversion.", call. = FALSE)
  }
  invisible(NULL)
}
