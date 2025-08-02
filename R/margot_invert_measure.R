#' Invert Measure Values for Reverse Scoring
#'
#' @description
#' Inverts measure values using either z-score negation or ordinal scale inversion.
#' This function is used internally by margot_causal_forest when flip_outcomes
#' is specified.
#'
#' @param x Numeric vector of values to invert
#' @param method Character string specifying inversion method:
#'   - "zscore": Simple negation for already standardized data (default)
#'   - "ordinal": Invert on ordinal scale using bounds
#' @param scale_bounds Numeric vector of length 2 specifying [min, max] bounds
#'   for ordinal scale inversion. Required when method = "ordinal".
#'   If NULL and method = "ordinal", bounds are inferred from data range.
#'
#' @return Numeric vector of inverted values
#'
#' @details
#' Z-score method:
#' Simply negates the values: x_flipped = -x
#' This assumes the input data is already standardized (z-scores).
#' For raw data that needs standardization, consider using scale() first.
#'
#' Ordinal method:
#' Uses the formula: x_flipped = (max + min) - x
#' This preserves distances between values while reversing their order.
#'
#' @examples
#' \dontrun{
#' # z-score inversion (for already standardized data)
#' z_scores <- scale(c(1, 2, 3, 4, 5))[, 1] # standardize first
#' margot_invert_measure(z_scores, method = "zscore")
#'
#' # ordinal scale inversion with known bounds
#' likert <- c(1, 2, 3, 4, 5, 3, 2)
#' margot_invert_measure(likert, method = "ordinal", scale_bounds = c(1, 5))
#'
#' # ordinal scale inversion with inferred bounds
#' margot_invert_measure(likert, method = "ordinal")
#' }
#'
#' @export
margot_invert_measure <- function(x,
                                  method = c("zscore", "ordinal"),
                                  scale_bounds = NULL) {
  # validate inputs
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  method <- match.arg(method)

  # handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x) # all NA, return as-is
  }

  # perform inversion based on method
  if (method == "zscore") {
    # simple negation for z-scores
    # assumes data is already standardized
    x_flipped <- -x_clean

    # check if data appears to be standardized
    # (mean near 0, sd near 1)
    mean_x <- mean(x_clean)
    sd_x <- sd(x_clean)

    if (abs(mean_x) > 0.1 || abs(sd_x - 1) > 0.1) {
      cli::cli_alert_warning("Data may not be standardized (mean = {round(mean_x, 3)}, sd = {round(sd_x, 3)}). Consider standardizing first.")
    }
  } else if (method == "ordinal") {
    # ordinal scale inversion
    if (is.null(scale_bounds)) {
      # infer bounds from data
      scale_bounds <- c(min(x_clean), max(x_clean))
      cli::cli_alert_info("Inferring scale bounds from data: [{scale_bounds[1]}, {scale_bounds[2]}]")
    }

    # validate bounds
    if (length(scale_bounds) != 2) {
      stop("scale_bounds must be a vector of length 2: c(min, max)")
    }

    if (scale_bounds[1] >= scale_bounds[2]) {
      stop("scale_bounds[1] must be less than scale_bounds[2]")
    }

    # check if values are within bounds
    if (any(x_clean < scale_bounds[1] | x_clean > scale_bounds[2])) {
      cli::cli_alert_warning("Some values are outside specified bounds")
    }

    # apply ordinal inversion formula
    x_flipped <- (scale_bounds[2] + scale_bounds[1]) - x_clean
  }

  # reconstruct full vector with NA values
  result <- numeric(length(x))
  result[na_idx] <- NA
  result[!na_idx] <- x_flipped

  return(result)
}

#' Validate Flip Outcomes Specification
#'
#' @description
#' Internal function to validate flip_outcomes specification and ensure
#' all required information is provided.
#'
#' @param flip_outcomes Character vector or list specifying outcomes to flip
#' @param outcome_vars Character vector of all outcome variables
#' @param flip_method Character string specifying default flip method
#' @param flip_scale_bounds Named list or single vector of scale bounds
#'
#' @return List with validated flip specifications
#'
#' @keywords internal
#' @noRd
.validate_flip_spec <- function(flip_outcomes,
                                outcome_vars,
                                flip_method = "zscore",
                                flip_scale_bounds = NULL) {
  # handle different input formats
  if (is.character(flip_outcomes)) {
    # simple character vector - use defaults for all
    flip_spec <- list()
    for (outcome in flip_outcomes) {
      if (!(outcome %in% outcome_vars)) {
        cli::cli_alert_warning("Outcome '{outcome}' not found in outcome_vars, skipping")
        next
      }

      flip_spec[[outcome]] <- list(
        method = flip_method,
        scale_bounds = if (is.list(flip_scale_bounds)) {
          flip_scale_bounds[[outcome]]
        } else {
          flip_scale_bounds
        }
      )
    }
  } else if (is.list(flip_outcomes)) {
    # list format - validate each entry
    flip_spec <- list()
    for (outcome in names(flip_outcomes)) {
      if (!(outcome %in% outcome_vars)) {
        cli::cli_alert_warning("Outcome '{outcome}' not found in outcome_vars, skipping")
        next
      }

      spec <- flip_outcomes[[outcome]]

      # handle different spec formats
      if (is.logical(spec) && spec) {
        # TRUE means use defaults
        flip_spec[[outcome]] <- list(
          method = flip_method,
          scale_bounds = if (is.list(flip_scale_bounds)) {
            flip_scale_bounds[[outcome]]
          } else {
            flip_scale_bounds
          }
        )
      } else if (is.list(spec)) {
        # detailed specification
        flip_spec[[outcome]] <- list(
          method = spec$method %||% flip_method,
          scale_bounds = spec$scale_bounds %||%
            (if (is.list(flip_scale_bounds)) flip_scale_bounds[[outcome]] else flip_scale_bounds)
        )
      }
    }
  } else {
    stop("flip_outcomes must be a character vector or list")
  }

  return(flip_spec)
}
